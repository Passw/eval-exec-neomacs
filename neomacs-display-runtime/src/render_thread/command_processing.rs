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
}
