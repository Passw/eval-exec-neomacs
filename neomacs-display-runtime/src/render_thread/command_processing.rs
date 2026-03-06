use super::*;

impl RenderApp {
    /// Process pending commands from Emacs
    pub(super) fn process_commands(&mut self) -> bool {
        let mut should_exit = false;

        while let Ok(cmd) = self.comms.cmd_rx.try_recv() {
            match cmd {
                RenderCommand::Shutdown => {
                    tracing::info!("Render thread received shutdown command");
                    should_exit = true;
                }
                RenderCommand::ScrollBlit { .. } => {
                    // No-op: scroll blitting is no longer needed with full-frame rendering.
                    // The entire frame is rebuilt from authoritative layout output each time.
                    tracing::debug!("ScrollBlit ignored (full-frame rendering mode)");
                }
                RenderCommand::ImageLoadFile {
                    id,
                    path,
                    max_width,
                    max_height,
                    fg_color,
                    bg_color,
                } => {
                    tracing::info!(
                        "Loading image {}: {} (max {}x{})",
                        id,
                        path,
                        max_width,
                        max_height
                    );
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.load_image_file_with_id(
                            id, &path, max_width, max_height, fg_color, bg_color,
                        );
                        // Get dimensions and notify Emacs
                        if let Some((w, h)) = renderer.get_image_size(id) {
                            // Store in shared map for main thread to read
                            if let Ok(mut dims) = self.image_dimensions.lock() {
                                dims.insert(id, (w, h));
                            }
                            // Send event to Emacs so it can trigger redisplay
                            self.comms.send_input(InputEvent::ImageDimensionsReady {
                                id,
                                width: w,
                                height: h,
                            });
                            tracing::debug!(
                                "Sent ImageDimensionsReady for image {}: {}x{}",
                                id,
                                w,
                                h
                            );
                        }
                    } else {
                        tracing::warn!("Renderer not initialized, cannot load image {}", id);
                    }
                }
                RenderCommand::ImageLoadData {
                    id,
                    data,
                    max_width,
                    max_height,
                    fg_color,
                    bg_color,
                } => {
                    tracing::info!(
                        "Loading image data {}: {} bytes (max {}x{})",
                        id,
                        data.len(),
                        max_width,
                        max_height
                    );
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.load_image_data_with_id(
                            id, &data, max_width, max_height, fg_color, bg_color,
                        );
                        // Get dimensions and notify Emacs
                        if let Some((w, h)) = renderer.get_image_size(id) {
                            if let Ok(mut dims) = self.image_dimensions.lock() {
                                dims.insert(id, (w, h));
                            }
                            self.comms.send_input(InputEvent::ImageDimensionsReady {
                                id,
                                width: w,
                                height: h,
                            });
                            tracing::debug!(
                                "Sent ImageDimensionsReady for image data {}: {}x{}",
                                id,
                                w,
                                h
                            );
                        }
                    } else {
                        tracing::warn!("Renderer not initialized, cannot load image data {}", id);
                    }
                }
                RenderCommand::ImageLoadArgb32 {
                    id,
                    data,
                    width,
                    height,
                    stride,
                } => {
                    tracing::debug!(
                        "Loading ARGB32 image {}: {}x{} stride={}",
                        id,
                        width,
                        height,
                        stride
                    );
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.load_image_argb32_with_id(id, &data, width, height, stride);
                        if let Some((w, h)) = renderer.get_image_size(id) {
                            if let Ok(mut dims) = self.image_dimensions.lock() {
                                dims.insert(id, (w, h));
                            }
                        }
                    }
                }
                RenderCommand::ImageLoadRgb24 {
                    id,
                    data,
                    width,
                    height,
                    stride,
                } => {
                    tracing::debug!(
                        "Loading RGB24 image {}: {}x{} stride={}",
                        id,
                        width,
                        height,
                        stride
                    );
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.load_image_rgb24_with_id(id, &data, width, height, stride);
                        if let Some((w, h)) = renderer.get_image_size(id) {
                            if let Ok(mut dims) = self.image_dimensions.lock() {
                                dims.insert(id, (w, h));
                            }
                        }
                    }
                }
                RenderCommand::ImageFree { id } => {
                    tracing::debug!("Freeing image {}", id);
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.free_image(id);
                    }
                }
                RenderCommand::WebKitCreate { id, width, height } => {
                    tracing::info!("Creating WebKit view: id={}, {}x{}", id, width, height);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(ref backend) = self.wpe_backend {
                        if let Some(platform_display) = backend.platform_display() {
                            match WpeWebView::new(id, platform_display, width, height) {
                                Ok(view) => {
                                    self.webkit_views.insert(id, view);
                                    tracing::info!("WebKit view {} created successfully", id);
                                }
                                Err(e) => {
                                    tracing::error!("Failed to create WebKit view {}: {:?}", id, e)
                                }
                            }
                        } else {
                            tracing::error!("WPE platform display not available");
                        }
                    } else {
                        tracing::warn!("WPE backend not initialized, cannot create WebKit view");
                    }
                }
                RenderCommand::WebKitLoadUri { id, url } => {
                    tracing::info!("Loading URL in WebKit view {}: {}", id, url);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get_mut(&id) {
                        if let Err(e) = view.load_uri(&url) {
                            tracing::error!("Failed to load URL in view {}: {:?}", id, e);
                        }
                    } else {
                        tracing::warn!("WebKit view {} not found", id);
                    }
                }
                RenderCommand::WebKitResize { id, width, height } => {
                    tracing::debug!("Resizing WebKit view {}: {}x{}", id, width, height);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get_mut(&id) {
                        view.resize(width, height);
                    }
                }
                RenderCommand::WebKitDestroy { id } => {
                    tracing::info!("Destroying WebKit view {}", id);
                    #[cfg(feature = "wpe-webkit")]
                    {
                        self.webkit_views.remove(&id);
                        // Clean up the renderer's webkit cache
                        if let Some(ref mut renderer) = self.renderer {
                            renderer.remove_webkit_view(id);
                        }
                    }
                }
                RenderCommand::WebKitClick { id, x, y, button } => {
                    tracing::debug!(
                        "WebKit click view {} at ({}, {}), button {}",
                        id,
                        x,
                        y,
                        button
                    );
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get(&id) {
                        view.click(x, y, button);
                    }
                }
                RenderCommand::WebKitPointerEvent {
                    id,
                    event_type,
                    x,
                    y,
                    button,
                    state,
                    modifiers,
                } => {
                    tracing::trace!(
                        "WebKit pointer event view {} type {} at ({}, {})",
                        id,
                        event_type,
                        x,
                        y
                    );
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get(&id) {
                        view.send_pointer_event(event_type, x, y, button, state, modifiers);
                    }
                }
                RenderCommand::WebKitScroll {
                    id,
                    x,
                    y,
                    delta_x,
                    delta_y,
                } => {
                    tracing::debug!(
                        "WebKit scroll view {} at ({}, {}), delta ({}, {})",
                        id,
                        x,
                        y,
                        delta_x,
                        delta_y
                    );
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get(&id) {
                        view.scroll(x, y, delta_x, delta_y);
                    }
                }
                RenderCommand::WebKitKeyEvent {
                    id,
                    keyval,
                    keycode,
                    pressed,
                    modifiers,
                } => {
                    tracing::debug!(
                        "WebKit key event view {} keyval {} pressed {}",
                        id,
                        keyval,
                        pressed
                    );
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get(&id) {
                        view.send_keyboard_event(keyval, keycode, pressed, modifiers);
                    }
                }
                RenderCommand::WebKitGoBack { id } => {
                    tracing::info!("WebKit go back view {}", id);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get_mut(&id) {
                        let _ = view.go_back();
                    }
                }
                RenderCommand::WebKitGoForward { id } => {
                    tracing::info!("WebKit go forward view {}", id);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get_mut(&id) {
                        let _ = view.go_forward();
                    }
                }
                RenderCommand::WebKitReload { id } => {
                    tracing::info!("WebKit reload view {}", id);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get_mut(&id) {
                        let _ = view.reload();
                    }
                }
                RenderCommand::WebKitExecuteJavaScript { id, script } => {
                    tracing::debug!("WebKit execute JS view {}", id);
                    #[cfg(feature = "wpe-webkit")]
                    if let Some(view) = self.webkit_views.get(&id) {
                        let _ = view.execute_javascript(&script);
                    }
                }
                RenderCommand::WebKitSetFloating {
                    id,
                    x,
                    y,
                    width,
                    height,
                } => {
                    tracing::info!(
                        "WebKit set floating: id={} at ({},{}) {}x{}",
                        id,
                        x,
                        y,
                        width,
                        height
                    );
                    #[cfg(feature = "wpe-webkit")]
                    {
                        self.floating_webkits.retain(|w| w.webkit_id != id);
                        self.floating_webkits
                            .push(crate::core::scene::FloatingWebKit {
                                webkit_id: id,
                                x,
                                y,
                                width,
                                height,
                            });
                        self.frame_dirty = true;
                    }
                }
                RenderCommand::WebKitRemoveFloating { id } => {
                    tracing::info!("WebKit remove floating: id={}", id);
                    #[cfg(feature = "wpe-webkit")]
                    {
                        self.floating_webkits.retain(|w| w.webkit_id != id);
                        self.frame_dirty = true;
                    }
                }
                RenderCommand::VideoCreate { id, path } => {
                    tracing::info!("Loading video {}: {}", id, path);
                    #[cfg(feature = "video")]
                    if let Some(ref mut renderer) = self.renderer {
                        let video_id = renderer.load_video_file(&path);
                        tracing::info!(
                            "Video loaded with id {} (requested id was {})",
                            video_id,
                            id
                        );
                    }
                }
                RenderCommand::VideoPlay { id } => {
                    tracing::debug!("Playing video {}", id);
                    #[cfg(feature = "video")]
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.video_play(id);
                    }
                }
                RenderCommand::VideoPause { id } => {
                    tracing::debug!("Pausing video {}", id);
                    #[cfg(feature = "video")]
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.video_pause(id);
                    }
                }
                RenderCommand::VideoDestroy { id } => {
                    tracing::info!("Destroying video {}", id);
                    #[cfg(feature = "video")]
                    if let Some(ref mut renderer) = self.renderer {
                        renderer.video_stop(id);
                    }
                }
                RenderCommand::SetMouseCursor { cursor_type } => {
                    if let Some(ref window) = self.window {
                        if cursor_type == 0 {
                            // Hidden/invisible cursor
                            window.set_cursor_visible(false);
                        } else {
                            use winit::window::CursorIcon;
                            window.set_cursor_visible(true);
                            let icon = match cursor_type {
                                2 => CursorIcon::Text,    // I-beam
                                3 => CursorIcon::Pointer, // Hand/pointer
                                4 => CursorIcon::Crosshair,
                                5 => CursorIcon::EwResize, // Horizontal resize
                                6 => CursorIcon::NsResize, // Vertical resize
                                7 => CursorIcon::Wait,     // Hourglass
                                8 => CursorIcon::NwseResize, // NW-SE (top-left/bottom-right)
                                9 => CursorIcon::NeswResize, // NE-SW (top-right/bottom-left)
                                10 => CursorIcon::NeswResize,
                                11 => CursorIcon::NwseResize,
                                _ => CursorIcon::Default, // Arrow
                            };
                            window.set_cursor(icon);
                        }
                    }
                }
                RenderCommand::WarpMouse { x, y } => {
                    if let Some(ref window) = self.window {
                        use winit::dpi::PhysicalPosition;
                        let pos = PhysicalPosition::new(x as f64, y as f64);
                        let _ = window.set_cursor_position(pos);
                    }
                }
                RenderCommand::SetWindowTitle { title } => {
                    self.chrome.title = title.clone();
                    if let Some(ref window) = self.window {
                        window.set_title(&title);
                    }
                    if !self.chrome.decorations_enabled {
                        self.frame_dirty = true;
                    }
                }
                RenderCommand::SetWindowFullscreen { mode } => {
                    if let Some(ref window) = self.window {
                        use winit::window::Fullscreen;
                        match mode {
                            3 => {
                                // FULLSCREEN_BOTH: borderless fullscreen
                                window.set_fullscreen(Some(Fullscreen::Borderless(None)));
                                self.chrome.is_fullscreen = true;
                            }
                            4 => {
                                // FULLSCREEN_MAXIMIZED
                                window.set_maximized(true);
                                self.chrome.is_fullscreen = false;
                            }
                            _ => {
                                // FULLSCREEN_NONE or partial: exit fullscreen
                                window.set_fullscreen(None);
                                window.set_maximized(false);
                                self.chrome.is_fullscreen = false;
                            }
                        }
                        self.frame_dirty = true;
                    }
                }
                RenderCommand::SetWindowMinimized { minimized } => {
                    if let Some(ref window) = self.window {
                        window.set_minimized(minimized);
                    }
                }
                RenderCommand::SetWindowPosition { x, y } => {
                    if let Some(ref window) = self.window {
                        window.set_outer_position(winit::dpi::PhysicalPosition::new(x, y));
                    }
                }
                RenderCommand::SetWindowSize { width, height } => {
                    if let Some(ref window) = self.window {
                        // Emacs sends logical pixel dimensions
                        let size = winit::dpi::LogicalSize::new(width, height);
                        let _ = window.request_inner_size(size);
                    }
                }
                RenderCommand::SetWindowDecorated { decorated } => {
                    self.chrome.decorations_enabled = decorated;
                    if let Some(ref window) = self.window {
                        window.set_decorations(decorated);
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::SetCursorBlink {
                    enabled,
                    interval_ms,
                } => {
                    tracing::debug!(
                        "Cursor blink: enabled={}, interval={}ms",
                        enabled,
                        interval_ms
                    );
                    self.cursor.blink_enabled = enabled;
                    self.cursor.blink_interval =
                        std::time::Duration::from_millis(interval_ms as u64);
                    if !enabled {
                        self.cursor.blink_on = true;
                        self.frame_dirty = true;
                    }
                }
                RenderCommand::SetCursorAnimation { enabled, speed } => {
                    tracing::debug!("Cursor animation: enabled={}, speed={}", enabled, speed);
                    self.cursor.anim_enabled = enabled;
                    self.cursor.anim_speed = speed;
                    if !enabled {
                        self.cursor.animating = false;
                    }
                }
                RenderCommand::SetAnimationConfig {
                    cursor_enabled,
                    cursor_speed,
                    cursor_style,
                    cursor_duration_ms,
                    transition_policy,
                    trail_size,
                } => {
                    tracing::debug!(
                        "Animation config: cursor={}/{}/style={:?}/{}ms/trail={}, crossfade={}/{}ms/effect={:?}/easing={:?}, scroll={}/{}ms/effect={:?}/easing={:?}",
                        cursor_enabled,
                        cursor_speed,
                        cursor_style,
                        cursor_duration_ms,
                        trail_size,
                        transition_policy.crossfade_enabled,
                        transition_policy.crossfade_duration_ms,
                        transition_policy.crossfade_effect,
                        transition_policy.crossfade_easing,
                        transition_policy.scroll_enabled,
                        transition_policy.scroll_duration_ms,
                        transition_policy.scroll_effect,
                        transition_policy.scroll_easing
                    );
                    self.cursor.anim_enabled = cursor_enabled;
                    self.cursor.anim_speed = cursor_speed;
                    self.cursor.anim_style = cursor_style;
                    self.cursor.anim_duration = cursor_duration_ms as f32 / 1000.0;
                    self.cursor.trail_size = trail_size.clamp(0.0, 1.0);
                    self.transitions.policy = transition_policy;
                    if !cursor_enabled {
                        self.cursor.animating = false;
                    }
                    if !transition_policy.crossfade_enabled {
                        self.transitions.crossfades.clear();
                    }
                    if !transition_policy.scroll_enabled {
                        self.transitions.scroll_slides.clear();
                    }
                }
                #[cfg(feature = "neo-term")]
                RenderCommand::TerminalCreate {
                    id,
                    cols,
                    rows,
                    mode,
                    shell,
                } => {
                    let term_mode = match mode {
                        1 => crate::terminal::TerminalMode::Inline,
                        2 => crate::terminal::TerminalMode::Floating,
                        _ => crate::terminal::TerminalMode::Window,
                    };
                    match crate::terminal::TerminalView::new(
                        id,
                        cols,
                        rows,
                        term_mode,
                        shell.as_deref(),
                    ) {
                        Ok(view) => {
                            // Register term Arc in shared map for cross-thread access
                            if let Ok(mut shared) = self.shared_terminals.lock() {
                                shared.insert(id, view.term.clone());
                            }
                            self.terminal_manager.terminals.insert(id, view);
                            tracing::info!(
                                "Terminal {} created ({}x{}, {:?})",
                                id,
                                cols,
                                rows,
                                term_mode
                            );
                        }
                        Err(e) => {
                            tracing::error!("Failed to create terminal {}: {}", id, e);
                        }
                    }
                }
                #[cfg(feature = "neo-term")]
                RenderCommand::TerminalWrite { id, data } => {
                    if let Some(view) = self.terminal_manager.get_mut(id) {
                        if let Err(e) = view.write(&data) {
                            tracing::warn!("Terminal {} write error: {}", id, e);
                        }
                    }
                }
                #[cfg(feature = "neo-term")]
                RenderCommand::TerminalResize { id, cols, rows } => {
                    if let Some(view) = self.terminal_manager.get_mut(id) {
                        view.resize(cols, rows);
                    }
                }
                #[cfg(feature = "neo-term")]
                RenderCommand::TerminalDestroy { id } => {
                    if let Ok(mut shared) = self.shared_terminals.lock() {
                        shared.remove(&id);
                    }
                    self.terminal_manager.destroy(id);
                    tracing::info!("Terminal {} destroyed", id);
                }
                #[cfg(feature = "neo-term")]
                RenderCommand::TerminalSetFloat { id, x, y, opacity } => {
                    if let Some(view) = self.terminal_manager.get_mut(id) {
                        view.float_x = x;
                        view.float_y = y;
                        view.float_opacity = opacity;
                    }
                }
                RenderCommand::ShowPopupMenu {
                    x,
                    y,
                    items,
                    title,
                    fg,
                    bg,
                } => {
                    tracing::info!("ShowPopupMenu at ({}, {}) with {} items", x, y, items.len());
                    let (fs, lh, cw) = self
                        .glyph_atlas
                        .as_ref()
                        .map(|a| {
                            (
                                a.default_font_size(),
                                a.default_line_height(),
                                a.default_char_width(),
                            )
                        })
                        .unwrap_or((13.0, 17.0, 13.0 * 0.6));
                    let mut menu = PopupMenuState::new(x, y, items, title, fs, lh, cw);
                    menu.face_fg = fg;
                    menu.face_bg = bg;
                    self.popup_menu = Some(menu);
                    self.frame_dirty = true;
                }
                RenderCommand::HidePopupMenu => {
                    tracing::info!("HidePopupMenu");
                    self.popup_menu = None;
                    self.menu_bar_active = None;
                    self.frame_dirty = true;
                }
                RenderCommand::ShowTooltip {
                    x,
                    y,
                    text,
                    fg_r,
                    fg_g,
                    fg_b,
                    bg_r,
                    bg_g,
                    bg_b,
                } => {
                    tracing::debug!("ShowTooltip at ({}, {})", x, y);
                    let (fs, lh, cw) = self
                        .glyph_atlas
                        .as_ref()
                        .map(|a| {
                            (
                                a.default_font_size(),
                                a.default_line_height(),
                                a.default_char_width(),
                            )
                        })
                        .unwrap_or((13.0, 17.0, 13.0 * 0.6));
                    self.tooltip = Some(TooltipState::new(
                        x,
                        y,
                        &text,
                        (fg_r, fg_g, fg_b),
                        (bg_r, bg_g, bg_b),
                        self.width as f32 / self.scale_factor as f32,
                        self.height as f32 / self.scale_factor as f32,
                        fs,
                        lh,
                        cw,
                    ));
                    self.frame_dirty = true;
                }
                RenderCommand::HideTooltip => {
                    tracing::debug!("HideTooltip");
                    self.tooltip = None;
                    self.frame_dirty = true;
                }
                RenderCommand::VisualBell => {
                    self.visual_bell_start = Some(std::time::Instant::now());
                    // Trigger cursor error pulse if enabled
                    if self.effects.cursor_error_pulse.enabled {
                        if let Some(renderer) = self.renderer.as_mut() {
                            renderer.trigger_cursor_error_pulse(std::time::Instant::now());
                        }
                    }
                    // Trigger edge snap indicator if enabled
                    if self.effects.edge_snap.enabled {
                        if let Some(ref frame) = self.current_frame {
                            // Find selected window and check boundary
                            for info in &frame.window_infos {
                                if info.selected && !info.is_minibuffer {
                                    let at_top = info.window_start <= 1;
                                    let at_bottom = info.window_end >= info.buffer_size;
                                    if at_top || at_bottom {
                                        if let Some(renderer) = self.renderer.as_mut() {
                                            renderer.trigger_edge_snap(
                                                info.bounds,
                                                info.mode_line_height,
                                                at_top,
                                                at_bottom,
                                                std::time::Instant::now(),
                                            );
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::RequestAttention { urgent } => {
                    if let Some(ref window) = self.window {
                        let attention = if urgent {
                            Some(winit::window::UserAttentionType::Critical)
                        } else {
                            Some(winit::window::UserAttentionType::Informational)
                        };
                        window.request_user_attention(attention);
                    }
                }
                RenderCommand::UpdateEffect(updater) => {
                    (updater.0)(&mut self.effects);
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.effects = self.effects.clone();
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::SetScrollIndicators { enabled } => {
                    self.scroll_indicators_enabled = enabled;
                    self.frame_dirty = true;
                }
                RenderCommand::SetTitlebarHeight { height } => {
                    self.chrome.titlebar_height = height;
                    self.frame_dirty = true;
                }
                RenderCommand::SetShowFps { enabled } => {
                    self.fps.enabled = enabled;
                    self.frame_dirty = true;
                }
                RenderCommand::SetCornerRadius { radius } => {
                    self.chrome.corner_radius = radius;
                    self.frame_dirty = true;
                }
                RenderCommand::SetExtraSpacing {
                    line_spacing,
                    letter_spacing,
                } => {
                    self.extra_line_spacing = line_spacing;
                    self.extra_letter_spacing = letter_spacing;
                    self.frame_dirty = true;
                }
                RenderCommand::SetIndentGuideRainbow { enabled, colors } => {
                    // Convert sRGB colors to linear for GPU rendering
                    let linear_colors: Vec<(f32, f32, f32, f32)> = colors
                        .iter()
                        .map(|(r, g, b, a)| {
                            let c = crate::core::types::Color::new(*r, *g, *b, *a).srgb_to_linear();
                            (c.r, c.g, c.b, c.a)
                        })
                        .collect();
                    self.effects.indent_guides.rainbow_enabled = enabled;
                    self.effects.indent_guides.rainbow_colors = linear_colors.clone();
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.set_indent_guide_rainbow(enabled, linear_colors);
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::SetCursorSizeTransition {
                    enabled,
                    duration_ms,
                } => {
                    self.cursor.size_transition_enabled = enabled;
                    self.cursor.size_transition_duration = duration_ms as f32 / 1000.0;
                    if !enabled {
                        self.cursor.size_animating = false;
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::SetLigaturesEnabled { enabled } => {
                    tracing::info!("Ligatures enabled: {}", enabled);
                    // Ligatures are handled by the layout engine (Emacs thread),
                    // not the render thread. The flag is stored on
                    // NeomacsDisplay/LayoutEngine via a separate static.
                    // This command is a no-op on the render thread but we log it.
                }
                RenderCommand::RemoveChildFrame { frame_id } => {
                    tracing::info!("Removing child frame 0x{:x}", frame_id);
                    self.child_frames.remove_frame(frame_id);
                    self.frame_dirty = true;
                }
                RenderCommand::SetChildFrameStyle {
                    corner_radius,
                    shadow_enabled,
                    shadow_layers,
                    shadow_offset,
                    shadow_opacity,
                } => {
                    self.child_frame_corner_radius = corner_radius;
                    self.child_frame_shadow_enabled = shadow_enabled;
                    self.child_frame_shadow_layers = shadow_layers;
                    self.child_frame_shadow_offset = shadow_offset;
                    self.child_frame_shadow_opacity = shadow_opacity;
                    self.frame_dirty = true;
                }
                RenderCommand::SetToolBar {
                    items,
                    height,
                    fg_r,
                    fg_g,
                    fg_b,
                    bg_r,
                    bg_g,
                    bg_b,
                } => {
                    // Load icon textures for any new icons
                    for item in &items {
                        if !item.is_separator
                            && !item.icon_name.is_empty()
                            && !self.toolbar_icon_textures.contains_key(&item.icon_name)
                        {
                            if let Some(svg_data) =
                                crate::backend::wgpu::toolbar_icons::get_icon_svg(&item.icon_name)
                            {
                                if let Some(renderer) = self.renderer.as_mut() {
                                    let icon_size = self.toolbar_icon_size;
                                    let id = renderer
                                        .load_image_data(svg_data, icon_size, icon_size, 0, 0);
                                    self.toolbar_icon_textures
                                        .insert(item.icon_name.clone(), id);
                                    tracing::debug!(
                                        "Loaded toolbar icon '{}' as image_id={}",
                                        item.icon_name,
                                        id
                                    );
                                }
                            }
                        }
                    }
                    self.toolbar_items = items;
                    self.toolbar_height = height;
                    self.toolbar_fg = (fg_r, fg_g, fg_b);
                    self.toolbar_bg = (bg_r, bg_g, bg_b);
                    self.frame_dirty = true;
                }
                RenderCommand::SetToolBarConfig { icon_size, padding } => {
                    self.toolbar_icon_size = icon_size;
                    self.toolbar_padding = padding;
                    // Clear cached textures so they reload at new size
                    for (_name, id) in self.toolbar_icon_textures.drain() {
                        if let Some(renderer) = self.renderer.as_mut() {
                            renderer.free_image(id);
                        }
                    }
                    self.frame_dirty = true;
                }
                RenderCommand::SetMenuBar {
                    items,
                    height,
                    fg_r,
                    fg_g,
                    fg_b,
                    bg_r,
                    bg_g,
                    bg_b,
                } => {
                    tracing::debug!(
                        "SetMenuBar: {} items, height={}, fg=({:.3},{:.3},{:.3}), bg=({:.3},{:.3},{:.3})",
                        items.len(),
                        height,
                        fg_r,
                        fg_g,
                        fg_b,
                        bg_r,
                        bg_g,
                        bg_b
                    );
                    self.menu_bar_items = items;
                    self.menu_bar_height = height;
                    self.menu_bar_fg = (fg_r, fg_g, fg_b);
                    self.menu_bar_bg = (bg_r, bg_g, bg_b);
                    self.frame_dirty = true;
                }
                RenderCommand::CreateWindow {
                    emacs_frame_id,
                    width,
                    height,
                    title,
                } => {
                    tracing::info!(
                        "CreateWindow request: frame_id=0x{:x} {}x{} \"{}\"",
                        emacs_frame_id,
                        width,
                        height,
                        title
                    );
                    self.multi_windows
                        .request_create(emacs_frame_id, width, height, title);
                    // Actual creation happens in about_to_wait() with ActiveEventLoop
                }
                RenderCommand::DestroyWindow { emacs_frame_id } => {
                    tracing::info!("DestroyWindow request: frame_id=0x{:x}", emacs_frame_id);
                    self.multi_windows.request_destroy(emacs_frame_id);
                }
            }
        }

        should_exit
    }

    /// Get latest frame from Emacs (non-blocking)
    pub(super) fn poll_frame(&mut self) {
        // Get the newest frame, discarding older ones
        // Route child frames to the child frame manager, root frames to current_frame
        // Secondary windows route to multi_windows manager
        self.child_frames.tick();
        while let Ok(frame) = self.comms.frame_rx.try_recv() {
            // Check if this frame belongs to a secondary window
            let frame_id = frame.frame_id;
            let parent_id = frame.parent_id;

            // Try routing to secondary windows first (by frame_id)
            if frame_id != 0 && parent_id == 0 && self.multi_windows.windows.contains_key(&frame_id)
            {
                self.multi_windows.route_frame(frame);
                continue;
            }
            // Try routing child frames to secondary windows (by parent_id)
            if parent_id != 0 && self.multi_windows.windows.contains_key(&parent_id) {
                self.multi_windows.route_frame(frame);
                continue;
            }

            if parent_id != 0 {
                // Child frame: store in primary window's manager
                self.child_frames.update_frame(frame);
            } else {
                // Root frame: update primary window's current_frame
                self.current_frame = Some(frame);
                // Reset blink to visible when new frame arrives (cursor just moved/redrawn)
                self.cursor.reset_blink();
            }
            self.frame_dirty = true;
        }
        // Child frame lifetime is managed by explicit RemoveChildFrame commands
        // from C code (frame deletion, visibility change, unparenting).
        // No staleness pruning — child frames persist until explicitly removed.

        // Extract active cursor target for animation
        // Scan root frame first, then child frames (only one active cursor exists)
        {
            let mut active_cursor: Option<CursorTarget> = None;

            if let Some(ref frame) = self.current_frame {
                active_cursor = frame.glyphs.iter().find_map(|g| match g {
                    FrameGlyph::Cursor {
                        window_id,
                        x,
                        y,
                        width,
                        height,
                        style,
                        color,
                    } if !style.is_hollow() => Some(CursorTarget {
                        window_id: *window_id,
                        x: *x,
                        y: *y,
                        width: *width,
                        height: *height,
                        style: *style,
                        color: *color,
                        frame_id: 0,
                    }),
                    _ => None,
                });
            }

            // If no active cursor in root frame, check child frames
            if active_cursor.is_none() {
                for (_, entry) in &self.child_frames.frames {
                    if let Some(ct) = entry.frame.glyphs.iter().find_map(|g| match g {
                        FrameGlyph::Cursor {
                            window_id,
                            x,
                            y,
                            width,
                            height,
                            style,
                            color,
                        } if !style.is_hollow() => Some(CursorTarget {
                            window_id: *window_id,
                            x: *x,
                            y: *y,
                            width: *width,
                            height: *height,
                            style: *style,
                            color: *color,
                            frame_id: entry.frame_id,
                        }),
                        _ => None,
                    }) {
                        active_cursor = Some(ct);
                        break;
                    }
                }
            }

            if let Some(new_target) = active_cursor {
                let had_target = self.cursor.target.is_some();
                let target_moved = self.cursor.target.as_ref().map_or(true, |old| {
                    (old.x - new_target.x).abs() > 0.5
                        || (old.y - new_target.y).abs() > 0.5
                        || (old.width - new_target.width).abs() > 0.5
                        || (old.height - new_target.height).abs() > 0.5
                });

                if !had_target || !self.cursor.anim_enabled {
                    // First appearance or animation disabled: snap
                    self.cursor.current_x = new_target.x;
                    self.cursor.current_y = new_target.y;
                    self.cursor.current_w = new_target.width;
                    self.cursor.current_h = new_target.height;
                    self.cursor.animating = false;
                    // Snap corner springs to target corners
                    let corners = CursorState::target_corners(&new_target);
                    for i in 0..4 {
                        self.cursor.corner_springs[i].x = corners[i].0;
                        self.cursor.corner_springs[i].y = corners[i].1;
                        self.cursor.corner_springs[i].vx = 0.0;
                        self.cursor.corner_springs[i].vy = 0.0;
                        self.cursor.corner_springs[i].target_x = corners[i].0;
                        self.cursor.corner_springs[i].target_y = corners[i].1;
                    }
                    self.cursor.prev_target_cx = new_target.x + new_target.width / 2.0;
                    self.cursor.prev_target_cy = new_target.y + new_target.height / 2.0;
                } else if target_moved {
                    let now = std::time::Instant::now();
                    self.cursor.animating = true;
                    self.cursor.last_anim_time = now;
                    // Capture start position for easing/linear/spring styles
                    self.cursor.start_x = self.cursor.current_x;
                    self.cursor.start_y = self.cursor.current_y;
                    self.cursor.start_w = self.cursor.current_w;
                    self.cursor.start_h = self.cursor.current_h;
                    self.cursor.anim_start_time = now;
                    // For spring: reset velocities
                    self.cursor.velocity_x = 0.0;
                    self.cursor.velocity_y = 0.0;
                    self.cursor.velocity_w = 0.0;
                    self.cursor.velocity_h = 0.0;

                    // Set up 4-corner springs for trail effect (spring style only)
                    if self.cursor.anim_style == CursorAnimStyle::CriticallyDampedSpring {
                        let new_corners = CursorState::target_corners(&new_target);
                        let new_cx = new_target.x + new_target.width / 2.0;
                        let new_cy = new_target.y + new_target.height / 2.0;
                        let old_cx = self.cursor.prev_target_cx;
                        let old_cy = self.cursor.prev_target_cy;

                        // Travel direction (normalized)
                        let dx = new_cx - old_cx;
                        let dy = new_cy - old_cy;
                        let len = (dx * dx + dy * dy).sqrt();
                        let (dir_x, dir_y) = if len > 0.001 {
                            (dx / len, dy / len)
                        } else {
                            (1.0, 0.0)
                        };

                        // Corner direction vectors from center: TL(-1,-1), TR(1,-1), BR(1,1), BL(-1,1)
                        let corner_dirs: [(f32, f32); 4] =
                            [(-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0)];

                        // Compute dot products and rank corners
                        let mut dots: [(f32, usize); 4] = corner_dirs
                            .iter()
                            .enumerate()
                            .map(|(i, (cx, cy))| (cx * dir_x + cy * dir_y, i))
                            .collect::<Vec<_>>()
                            .try_into()
                            .unwrap();
                        dots.sort_by(|a, b| a.0.total_cmp(&b.0));
                        // dots[0] = most trailing (lowest dot), dots[3] = most leading (highest dot)

                        let base_dur = self.cursor.anim_duration; // seconds
                        for (rank, &(_dot, corner_idx)) in dots.iter().enumerate() {
                            let factor = 1.0 - self.cursor.trail_size * (rank as f32 / 3.0);
                            let duration_i = (base_dur * factor).max(0.01);
                            let omega_i = 4.0 / duration_i;

                            self.cursor.corner_springs[corner_idx].target_x =
                                new_corners[corner_idx].0;
                            self.cursor.corner_springs[corner_idx].target_y =
                                new_corners[corner_idx].1;
                            self.cursor.corner_springs[corner_idx].omega = omega_i;
                            // Don't reset velocity — preserve momentum from in-flight animation
                        }

                        self.cursor.prev_target_cx = new_cx;
                        self.cursor.prev_target_cy = new_cy;
                    }
                }

                // Spawn typing ripple when cursor moves (if enabled)
                if target_moved && had_target && self.effects.typing_ripple.enabled {
                    if let Some(renderer) = self.renderer.as_mut() {
                        let cx = new_target.x + new_target.width / 2.0;
                        let cy = new_target.y + new_target.height / 2.0;
                        renderer.spawn_ripple(cx, cy);
                    }
                }

                // Record cursor trail fade position when cursor moves
                if target_moved && had_target && self.effects.cursor_trail_fade.enabled {
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.record_cursor_trail(
                            self.cursor.current_x,
                            self.cursor.current_y,
                            self.cursor.current_w,
                            self.cursor.current_h,
                        );
                    }
                }

                // Update IME cursor area so candidate window follows text cursor.
                self.update_ime_cursor_area_if_needed(&new_target);

                // Detect cursor size change for smooth size transition
                if self.cursor.size_transition_enabled {
                    let dw = (new_target.width - self.cursor.size_target_w).abs();
                    let dh = (new_target.height - self.cursor.size_target_h).abs();
                    if dw > 2.0 || dh > 2.0 {
                        self.cursor.size_animating = true;
                        self.cursor.size_start_w = self.cursor.current_w;
                        self.cursor.size_start_h = self.cursor.current_h;
                        self.cursor.size_anim_start = std::time::Instant::now();
                    }
                    self.cursor.size_target_w = new_target.width;
                    self.cursor.size_target_h = new_target.height;
                }

                self.cursor.target = Some(new_target);
            }
        }
    }
}
