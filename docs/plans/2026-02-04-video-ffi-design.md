# Video FFI Integration Design

## Overview

Integrate video playback into the render thread architecture, routing video commands through the command queue instead of direct FFI calls.

**Goals:**
- Move VideoCache ownership to render thread
- Implement video command handlers
- Update FFI to use threaded path
- Maintain zero-copy DMA-BUF video frame import

## Architecture

### Current Problem

Video FFI calls bypass the render thread command queue:
```
Emacs → FFI → WgpuRenderer directly (wrong thread!)
```

### Solution

Route through command queue:
```
Emacs → FFI → RenderCommand → Render thread → VideoCache
```

### RenderApp Fields

```rust
#[cfg(feature = "video")]
video_cache: Option<VideoCache>,
```

### Command Handlers

```rust
RenderCommand::VideoCreate { id, path } => {
    if let Some(ref mut cache) = self.video_cache {
        cache.load_file(&path);
    }
}

RenderCommand::VideoPlay { id } => {
    if let Some(ref mut cache) = self.video_cache {
        cache.play(id);
    }
}

RenderCommand::VideoPause { id } => {
    if let Some(ref mut cache) = self.video_cache {
        cache.pause(id);
    }
}

RenderCommand::VideoDestroy { id } => {
    if let Some(ref mut cache) = self.video_cache {
        cache.remove(id);
    }
}
```

### FFI Pattern

```rust
pub unsafe extern "C" fn neomacs_display_load_video(...) -> u32 {
    // Threaded path
    if let Some(ref state) = THREADED_STATE {
        let id = VIDEO_ID_COUNTER.fetch_add(1, ...);
        let cmd = RenderCommand::VideoCreate { id, path };
        state.emacs_comms.cmd_tx.try_send(cmd);
        return id;
    }
    // Fallback to direct call
}
```

### Frame Processing

Call `video_cache.process_pending_videos(&device, &queue)` each frame in render loop to import decoded frames to GPU textures.

## Files Changed

| File | Change |
|------|--------|
| `render_thread.rs` | Add video_cache, implement handlers, process frames |
| `ffi.rs` | Add VIDEO_ID_COUNTER, update FFI for threaded path |

## Testing

1. Build with `cargo build --features video`
2. Load video, verify playback
3. Play/pause controls work
4. Video renders inline
5. CPU idle when paused
