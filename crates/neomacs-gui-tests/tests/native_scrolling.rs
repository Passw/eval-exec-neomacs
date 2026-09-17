//! Native precise scroll diagnostic on an isolated headless Wayland compositor.
//! This exercises the shared PixelDelta path, not AppKit's event generation.
#![cfg(target_os = "linux")]

use serde_json::Value;
use std::{
    fs,
    os::{fd::AsRawFd, unix::fs::PermissionsExt},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};
#[path = "native_scrolling/wayland.rs"]
mod wayland;

struct OwnedChild(Child);
impl Drop for OwnedChild {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn tool(name: &str) -> String {
    std::env::var(format!("NEOMACS_GUI_{}", name.to_uppercase())).unwrap_or_else(|_| name.into())
}

fn state(path: &Path, after: u64) -> Value {
    let deadline = Instant::now() + Duration::from_secs(8);
    loop {
        if let Ok(bytes) = fs::read(path)
            && let Ok(value) = serde_json::from_slice::<Value>(&bytes)
            && value["sample"]
                .as_u64()
                .is_some_and(|sample| sample > after)
        {
            return value;
        }
        assert!(Instant::now() < deadline, "no fresh state: {path:?}");
        thread::sleep(Duration::from_millis(20));
    }
}

#[test]
// Prerequisites: fresh release Neomacs/runtime and sway on PATH.
fn precise_native_scroll_advances_without_snapback() {
    run_native_scroll(ScrollKind::Precise);
}

#[test]
fn native_wheel_scroll_advances_without_snapback() {
    run_native_scroll(ScrollKind::Wheel);
}

#[derive(Clone, Copy, Debug)]
enum ScrollKind {
    Precise,
    Wheel,
}

fn run_native_scroll(kind: ScrollKind) {
    let root = PathBuf::from(env!("CARGO_WORKSPACE_DIR"));
    let artifact_root = root.join("target/neomacs-gui-tests");
    fs::create_dir_all(&artifact_root).unwrap();
    let artifacts = artifact_root.join(format!("native-scrolling-{kind:?}-{}", std::process::id()));
    fs::create_dir(&artifacts).unwrap();
    fs::set_permissions(&artifacts, fs::Permissions::from_mode(0o700)).unwrap();
    // Short path to the same workspace directory, below the Unix socket limit.
    let runtime_dir = fs::File::open(&artifacts).unwrap();
    let runtime = format!(
        "/proc/{}/fd/{}",
        std::process::id(),
        runtime_dir.as_raw_fd()
    );
    let config = artifacts.join("sway.conf");
    fs::write(
        &config,
        r#"
output * resolution 1000x700
xwayland disable
seat seat0 fallback true
default_border none
focus_follows_mouse yes
"#,
    )
    .unwrap();
    let log = fs::File::create(artifacts.join("sway.log")).unwrap();
    let mut compositor = OwnedChild(
        Command::new(tool("sway"))
            .args(["--unsupported-gpu", "--config"])
            .arg(&config)
            .env("XDG_RUNTIME_DIR", &runtime)
            .env("WLR_BACKENDS", "headless")
            .env("WLR_RENDERER", "pixman")
            .env("WLR_LIBINPUT_NO_DEVICES", "1")
            .env_remove("WAYLAND_DISPLAY")
            .env_remove("DISPLAY")
            .stdout(Stdio::from(log.try_clone().unwrap()))
            .stderr(Stdio::from(log))
            .spawn()
            .expect("start sway"),
    );
    let deadline = Instant::now() + Duration::from_secs(8);
    let socket = loop {
        if let Some(path) = fs::read_dir(&artifacts)
            .unwrap()
            .filter_map(Result::ok)
            .map(|entry| entry.path())
            .find(|p| {
                let n = p.file_name().unwrap().to_string_lossy();
                n.starts_with("wayland-") && !n.ends_with(".lock")
            })
        {
            break path.file_name().unwrap().to_string_lossy().into_owned();
        }
        assert!(
            compositor.0.try_wait().unwrap().is_none(),
            "sway exited: {artifacts:?}"
        );
        assert!(
            Instant::now() < deadline,
            "sway socket absent: {artifacts:?}"
        );
        thread::sleep(Duration::from_millis(25));
    };
    let state_path = artifacts.join("state.json");
    let pixels_path = artifacts.join("surface.png");
    let binary = std::env::var_os("NEOMACS_GUI_TEST_BINARY")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("target/release/neomacs"));
    let mut editor = OwnedChild(
        Command::new(binary)
            .args(["-Q", "-l"])
            .arg(root.join("crates/neomacs-gui-tests/fixtures/native-scrolling.el"))
            .env("XDG_RUNTIME_DIR", &runtime)
            .env("WAYLAND_DISPLAY", &socket)
            .env("WINIT_UNIX_BACKEND", "wayland")
            .env_remove("DISPLAY")
            .env("NEOMACS_GUI_STATE_JSON", &state_path)
            .env("NEOMACS_DEBUG_SURFACE_READBACK", "10000")
            .env("NEOMACS_DEBUG_SURFACE_READBACK_PNG", &pixels_path)
            .env("WAYLAND_DEBUG", "1")
            .env(
                "RUST_LOG",
                "warn,neomacs=debug,neomacs_display_runtime=debug",
            )
            .env("NEOMACS_LOG_FILE", artifacts.join("neomacs.log"))
            .stdout(fs::File::create(artifacts.join("stdout")).unwrap())
            .stderr(fs::File::create(artifacts.join("stderr")).unwrap())
            .spawn()
            .unwrap(),
    );
    let mut trackpad = wayland::Trackpad::connect(&PathBuf::from(&runtime).join(&socket));
    let initial = state(&state_path, 2);
    trackpad.move_to_body();
    thread::sleep(Duration::from_millis(200));
    let mut previous = state(&state_path, initial["sample"].as_u64().unwrap());
    image::open(&pixels_path)
        .expect("initial GUI readback")
        .save(artifacts.join("before.png"))
        .unwrap();
    let mut trace = vec![previous.clone()];
    for step in 0..12 {
        match kind {
            ScrollKind::Precise => trackpad.scroll(4.0),
            ScrollKind::Wheel => trackpad.wheel(),
        }
        thread::sleep(Duration::from_millis(500));
        let current = state(&state_path, previous["sample"].as_u64().unwrap());
        trace.push(current.clone());
        fs::write(
            artifacts.join("trace.json"),
            serde_json::to_vec_pretty(&trace).unwrap(),
        )
        .unwrap();
        if let Ok(png) = image::open(&pixels_path) {
            png.save(artifacts.join(format!("step-{step}.png")))
                .unwrap();
        }
        eprintln!("step={step} before={previous} after={current}; artifacts={artifacts:?}");
        assert!(editor.0.try_wait().unwrap().is_none(), "editor exited");
        let native_log = fs::read_to_string(artifacts.join("neomacs.log")).unwrap();
        let expected_event = match kind {
            ScrollKind::Precise => "PixelScroll {",
            ScrollKind::Wheel => "MouseScroll {",
        };
        assert!(
            native_log.contains(expected_event),
            "native input did not reach the VM bridge as {expected_event}: {artifacts:?}"
        );
        let position = |s: &Value| (s["start"].as_i64().unwrap(), s["vscroll"].as_i64().unwrap());
        assert!(
            position(&current) > position(&previous),
            "{kind:?} scrolling must advance without snapping back: before={previous}, after={current}; artifacts={artifacts:?}"
        );
        previous = current;
    }
}
