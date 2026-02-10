//! Window transition state (crossfade and scroll animations).

use std::collections::HashMap;
use crate::core::types::Rect;
#[allow(unused_imports)]
use crate::core::frame_glyphs::FrameGlyph;
use super::RenderApp;

/// State for an active crossfade transition
pub(super) struct CrossfadeTransition {
    pub(super) started: std::time::Instant,
    pub(super) duration: std::time::Duration,
    pub(super) bounds: Rect,
    pub(super) effect: crate::core::scroll_animation::ScrollEffect,
    pub(super) easing: crate::core::scroll_animation::ScrollEasing,
    pub(super) old_texture: wgpu::Texture,
    pub(super) old_view: wgpu::TextureView,
    pub(super) old_bind_group: wgpu::BindGroup,
}

/// State for an active scroll slide transition
pub(super) struct ScrollTransition {
    pub(super) started: std::time::Instant,
    pub(super) duration: std::time::Duration,
    pub(super) bounds: Rect,
    pub(super) direction: i32, // +1 = scroll down (content up), -1 = scroll up
    pub(super) effect: crate::core::scroll_animation::ScrollEffect,
    pub(super) easing: crate::core::scroll_animation::ScrollEasing,
    pub(super) old_texture: wgpu::Texture,
    pub(super) old_view: wgpu::TextureView,
    pub(super) old_bind_group: wgpu::BindGroup,
}

/// Window transition state (crossfade and scroll animations).
///
/// Groups configuration, double-buffer textures, and active transition maps.
pub(super) struct TransitionState {
    // Configuration
    pub(super) crossfade_enabled: bool,
    pub(super) crossfade_duration: std::time::Duration,
    pub(super) crossfade_effect: crate::core::scroll_animation::ScrollEffect,
    pub(super) crossfade_easing: crate::core::scroll_animation::ScrollEasing,
    pub(super) scroll_enabled: bool,
    pub(super) scroll_duration: std::time::Duration,
    pub(super) scroll_effect: crate::core::scroll_animation::ScrollEffect,
    pub(super) scroll_easing: crate::core::scroll_animation::ScrollEasing,

    // Double-buffer offscreen textures
    pub(super) offscreen_a: Option<(wgpu::Texture, wgpu::TextureView, wgpu::BindGroup)>,
    pub(super) offscreen_b: Option<(wgpu::Texture, wgpu::TextureView, wgpu::BindGroup)>,
    pub(super) current_is_a: bool,

    // Active transitions
    pub(super) crossfades: HashMap<i64, CrossfadeTransition>,
    pub(super) scroll_slides: HashMap<i64, ScrollTransition>,

    // Per-window metadata from previous frame (for transition detection)
    pub(super) prev_window_infos: HashMap<i64, crate::core::frame_glyphs::WindowInfo>,
}

impl Default for TransitionState {
    fn default() -> Self {
        Self {
            crossfade_enabled: true,
            crossfade_duration: std::time::Duration::from_millis(200),
            crossfade_effect: crate::core::scroll_animation::ScrollEffect::Crossfade,
            crossfade_easing: crate::core::scroll_animation::ScrollEasing::EaseOutQuad,
            scroll_enabled: true,
            scroll_duration: std::time::Duration::from_millis(150),
            scroll_effect: crate::core::scroll_animation::ScrollEffect::default(),
            scroll_easing: crate::core::scroll_animation::ScrollEasing::default(),
            offscreen_a: None,
            offscreen_b: None,
            current_is_a: true,
            crossfades: HashMap::new(),
            scroll_slides: HashMap::new(),
            prev_window_infos: HashMap::new(),
        }
    }
}

impl TransitionState {
    /// Check if any transitions are currently active
    pub(super) fn has_active(&self) -> bool {
        !self.crossfades.is_empty() || !self.scroll_slides.is_empty()
    }
}

impl RenderApp {
    /// Ensure offscreen textures exist (lazily created)
    pub(super) fn ensure_offscreen_textures(&mut self) {
        if self.transitions.offscreen_a.is_some() && self.transitions.offscreen_b.is_some() {
            return;
        }
        let renderer = match self.renderer.as_ref() {
            Some(r) => r,
            None => return,
        };
        let w = self.width;
        let h = self.height;

        if self.transitions.offscreen_a.is_none() {
            let (tex, view) = renderer.create_offscreen_texture(w, h);
            let bg = renderer.create_texture_bind_group(&view);
            self.transitions.offscreen_a = Some((tex, view, bg));
        }
        if self.transitions.offscreen_b.is_none() {
            let (tex, view) = renderer.create_offscreen_texture(w, h);
            let bg = renderer.create_texture_bind_group(&view);
            self.transitions.offscreen_b = Some((tex, view, bg));
        }
    }

    /// Get the "current" offscreen texture view and bind group
    pub(super) fn current_offscreen_view_and_bg(&self) -> Option<(&wgpu::TextureView, &wgpu::BindGroup)> {
        let (_, ref view, ref bg) = if self.transitions.current_is_a {
            self.transitions.offscreen_a.as_ref()?
        } else {
            self.transitions.offscreen_b.as_ref()?
        };
        Some((view, bg))
    }

    /// Get the "previous" offscreen texture, view, and bind group
    pub(super) fn previous_offscreen(&self) -> Option<(&wgpu::Texture, &wgpu::TextureView, &wgpu::BindGroup)> {
        let (ref tex, ref view, ref bg) = if self.transitions.current_is_a {
            self.transitions.offscreen_b.as_ref()?
        } else {
            self.transitions.offscreen_a.as_ref()?
        };
        Some((tex, view, bg))
    }

    /// Snapshot the previous offscreen texture into a new dedicated texture
    pub(super) fn snapshot_prev_texture(&self) -> Option<(wgpu::Texture, wgpu::TextureView, wgpu::BindGroup)> {
        let renderer = self.renderer.as_ref()?;
        let (prev_tex, _, _) = self.previous_offscreen()?;

        let (snap, snap_view) = renderer.create_offscreen_texture(self.width, self.height);

        // GPU copy
        let mut encoder = renderer.device().create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Snapshot Copy Encoder"),
        });
        encoder.copy_texture_to_texture(
            wgpu::ImageCopyTexture {
                texture: prev_tex,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::ImageCopyTexture {
                texture: &snap,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
        );
        renderer.queue().submit(std::iter::once(encoder.finish()));

        let snap_bg = renderer.create_texture_bind_group(&snap_view);
        Some((snap, snap_view, snap_bg))
    }

    /// Detect transitions by comparing current and previous window infos
    pub(super) fn detect_transitions(&mut self) {
        let frame = match self.current_frame.as_ref() {
            Some(f) => f,
            None => return,
        };

        let now = std::time::Instant::now();

        for info in &frame.window_infos {
            if let Some(prev) = self.transitions.prev_window_infos.get(&info.window_id) {
                if prev.buffer_id != 0 && info.buffer_id != 0 {
                    if prev.buffer_id != info.buffer_id {
                        // Text fade-in on buffer switch
                        if self.effects.text_fade_in.enabled && !info.is_minibuffer {
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.trigger_text_fade_in(info.window_id, info.bounds, now);
                            }
                        }
                        // Buffer switch → crossfade
                        // Suppress for minibuffer (small windows change buffers on every keystroke)
                        if self.transitions.crossfade_enabled && info.bounds.height >= 50.0 {
                            // Cancel existing transition for this window
                            self.transitions.crossfades.remove(&info.window_id);
                            self.transitions.scroll_slides.remove(&info.window_id);

                            if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                                log::debug!("Starting crossfade for window {} (buffer changed, effect={:?})", info.window_id, self.transitions.crossfade_effect);
                                self.transitions.crossfades.insert(info.window_id, CrossfadeTransition {
                                    started: now,
                                    duration: self.transitions.crossfade_duration,
                                    bounds: info.bounds,
                                    effect: self.transitions.crossfade_effect,
                                    easing: self.transitions.crossfade_easing,
                                    old_texture: tex,
                                    old_view: view,
                                    old_bind_group: bg,
                                });
                            }
                        }
                    } else if info.is_minibuffer
                        && (prev.bounds.height - info.bounds.height).abs() > 2.0
                    {
                        // Minibuffer height change → crossfade
                        if self.transitions.crossfade_enabled {
                            self.transitions.crossfades.remove(&info.window_id);
                            self.transitions.scroll_slides.remove(&info.window_id);

                            if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                                log::debug!("Starting minibuffer crossfade (height {} → {})",
                                    prev.bounds.height, info.bounds.height);
                                self.transitions.crossfades.insert(info.window_id, CrossfadeTransition {
                                    started: now,
                                    duration: std::time::Duration::from_millis(150),
                                    bounds: info.bounds,
                                    effect: self.transitions.crossfade_effect,
                                    easing: self.transitions.crossfade_easing,
                                    old_texture: tex,
                                    old_view: view,
                                    old_bind_group: bg,
                                });
                            }
                        }
                    } else if prev.window_start != info.window_start {
                        // Text fade-in on scroll
                        if self.effects.text_fade_in.enabled && !info.is_minibuffer {
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.trigger_text_fade_in(info.window_id, info.bounds, now);
                            }
                        }
                        // Scroll line spacing animation (accordion effect)
                        if self.effects.scroll_line_spacing.enabled {
                            let dir = if info.window_start > prev.window_start { 1 } else { -1 };
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.trigger_scroll_line_spacing(info.window_id, info.bounds, dir, now);
                            }
                        }
                        // Scroll momentum indicator
                        if self.effects.scroll_momentum.enabled && !info.is_minibuffer {
                            let dir = if info.window_start > prev.window_start { 1 } else { -1 };
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.trigger_scroll_momentum(info.window_id, info.bounds, dir, now);
                            }
                        }
                        // Scroll velocity fade overlay
                        if self.effects.scroll_velocity_fade.enabled && !info.is_minibuffer {
                            let delta = (info.window_start - prev.window_start).unsigned_abs() as f32;
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.trigger_scroll_velocity_fade(info.window_id, info.bounds, delta, now);
                            }
                        }
                        // Scroll → slide (content area only, excluding mode-line)
                        let content_height = info.bounds.height - info.mode_line_height;
                        if self.transitions.scroll_enabled && content_height >= 50.0 {
                            // Cancel existing transition for this window
                            self.transitions.crossfades.remove(&info.window_id);
                            self.transitions.scroll_slides.remove(&info.window_id);

                            let dir = if info.window_start > prev.window_start { 1 } else { -1 };

                            // Use content-only bounds (exclude mode-line at bottom)
                            let content_bounds = Rect::new(
                                info.bounds.x, info.bounds.y,
                                info.bounds.width, content_height,
                            );

                            if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                                log::debug!("Starting scroll slide for window {} (dir={}, effect={:?}, content_h={})",
                                    info.window_id, dir, self.transitions.scroll_effect, content_height);
                                self.transitions.scroll_slides.insert(info.window_id, ScrollTransition {
                                    started: now,
                                    duration: self.transitions.scroll_duration,
                                    bounds: content_bounds,
                                    direction: dir,
                                    effect: self.transitions.scroll_effect,
                                    easing: self.transitions.scroll_easing,
                                    old_texture: tex,
                                    old_view: view,
                                    old_bind_group: bg,
                                });
                            }
                        }
                    } else if (prev.char_height - info.char_height).abs() > 1.0 {
                        // Font size changed (text-scale-adjust) → crossfade
                        if self.transitions.crossfade_enabled {
                            self.transitions.crossfades.remove(&info.window_id);
                            self.transitions.scroll_slides.remove(&info.window_id);

                            if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                                log::debug!("Starting font-size crossfade for window {} (char_height {} → {})",
                                    info.window_id, prev.char_height, info.char_height);
                                self.transitions.crossfades.insert(info.window_id, CrossfadeTransition {
                                    started: now,
                                    duration: std::time::Duration::from_millis(200),
                                    bounds: info.bounds,
                                    effect: self.transitions.crossfade_effect,
                                    easing: self.transitions.crossfade_easing,
                                    old_texture: tex,
                                    old_view: view,
                                    old_bind_group: bg,
                                });
                            }
                        }
                    } else if self.effects.line_animation.enabled
                        && prev.buffer_size != info.buffer_size
                        && !info.is_minibuffer
                    {
                        // Buffer size changed with same window_start → line insertion/deletion
                        // Find cursor Y from frame glyphs as the edit point
                        let mut cursor_y: Option<f32> = None;
                        for g in &frame.glyphs {
                            if let crate::core::frame_glyphs::FrameGlyph::Cursor { x, y, style, .. } = g {
                                // Check cursor is within this window
                                if *x >= info.bounds.x && *x < info.bounds.x + info.bounds.width
                                    && *y >= info.bounds.y && *y < info.bounds.y + info.bounds.height
                                    && *style != 3
                                {
                                    cursor_y = Some(*y);
                                    break;
                                }
                            }
                        }
                        if let Some(edit_y) = cursor_y {
                            let ch = info.char_height;
                            let delta = info.buffer_size - prev.buffer_size;
                            // Positive delta = insertion (lines move down), negative = deletion (lines move up)
                            let offset = if delta > 0 { -ch } else { ch };
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.start_line_animation(
                                    info.bounds,
                                    edit_y + ch, // animate rows below cursor
                                    offset,
                                    self.effects.line_animation.duration_ms,
                                );
                            }
                        }
                    } else if (prev.bounds.width - info.bounds.width).abs() > 2.0
                        || (prev.bounds.height - info.bounds.height).abs() > 2.0
                    {
                        // Window resized (balance-windows, divider drag) → crossfade
                        if self.transitions.crossfade_enabled && !info.is_minibuffer {
                            self.transitions.crossfades.remove(&info.window_id);
                            self.transitions.scroll_slides.remove(&info.window_id);

                            // Use full-frame crossfade (window_id 0) since
                            // all windows resize together during balance
                            let full_bounds = Rect::new(0.0, 0.0, frame.width, frame.height);
                            if !self.transitions.crossfades.contains_key(&0) {
                                if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                                    log::debug!("Starting window-resize crossfade (bounds changed)");
                                    self.transitions.crossfades.insert(0, CrossfadeTransition {
                                        started: now,
                                        duration: std::time::Duration::from_millis(150),
                                        bounds: full_bounds,
                                        effect: self.transitions.crossfade_effect,
                                        easing: self.transitions.crossfade_easing,
                                        old_texture: tex,
                                        old_view: view,
                                        old_bind_group: bg,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        // Detect window split/delete (window count or IDs changed)
        if self.transitions.crossfade_enabled && !self.transitions.prev_window_infos.is_empty() {
            let curr_ids: std::collections::HashSet<i64> = frame.window_infos.iter()
                .filter(|i| !i.is_minibuffer)
                .map(|i| i.window_id)
                .collect();
            let prev_non_mini: std::collections::HashSet<i64> = self.transitions.prev_window_infos.iter()
                .filter(|(_, v)| !v.is_minibuffer)
                .map(|(k, _)| *k)
                .collect();

            if prev_non_mini != curr_ids && prev_non_mini.len() > 0 && curr_ids.len() > 0 {
                // Window layout changed — full-frame crossfade
                // Use a synthetic window_id (0) for the full-frame transition
                let full_bounds = Rect::new(0.0, 0.0, frame.width, frame.height);
                if let Some((tex, view, bg)) = self.snapshot_prev_texture() {
                    log::debug!("Starting window split/delete crossfade ({} → {} windows)",
                        prev_non_mini.len(), curr_ids.len());
                    self.transitions.crossfades.insert(0, CrossfadeTransition {
                        started: now,
                        duration: std::time::Duration::from_millis(200),
                        bounds: full_bounds,
                        effect: self.transitions.crossfade_effect,
                        easing: self.transitions.crossfade_easing,
                        old_texture: tex,
                        old_view: view,
                        old_bind_group: bg,
                    });
                }
            }
        }

        // Detect window switch (selected window changed) → highlight fade
        if self.effects.window_switch_fade.enabled {
            let mut new_selected: Option<(i64, Rect)> = None;
            for info in &frame.window_infos {
                if info.selected && !info.is_minibuffer {
                    new_selected = Some((info.window_id, info.bounds));
                    break;
                }
            }
            if let Some((wid, bounds)) = new_selected {
                if self.prev_selected_window_id != 0 && wid != self.prev_selected_window_id {
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.start_window_fade(wid, bounds);
                        self.frame_dirty = true;
                    }
                }
                self.prev_selected_window_id = wid;
            }
        }

        // Detect theme change (background color changed significantly)
        if self.effects.theme_transition.enabled {
            let bg = &frame.background;
            let new_bg = (bg.r, bg.g, bg.b, bg.a);
            if let Some(old_bg) = self.prev_background {
                let dr = (new_bg.0 - old_bg.0).abs();
                let dg = (new_bg.1 - old_bg.1).abs();
                let db = (new_bg.2 - old_bg.2).abs();
                // Threshold: any channel changed by more than ~2% means theme switch
                if dr > 0.02 || dg > 0.02 || db > 0.02 {
                    let full_bounds = Rect::new(0.0, 0.0, frame.width, frame.height);
                    if !self.transitions.crossfades.contains_key(&-1) {
                        if let Some((tex, view, bg_group)) = self.snapshot_prev_texture() {
                            log::debug!("Starting theme transition crossfade (bg changed)");
                            self.transitions.crossfades.insert(-1, CrossfadeTransition {
                                started: now,
                                duration: self.effects.theme_transition.duration,
                                bounds: full_bounds,
                                effect: self.transitions.crossfade_effect,
                                easing: self.transitions.crossfade_easing,
                                old_texture: tex,
                                old_view: view,
                                old_bind_group: bg_group,
                            });
                        }
                    }
                }
            }
            self.prev_background = Some(new_bg);
        }

        // Update prev_window_infos from current frame
        self.transitions.prev_window_infos.clear();
        for info in &frame.window_infos {
            self.transitions.prev_window_infos.insert(info.window_id, info.clone());
        }
    }

    /// Render active transitions on top of the surface
    pub(super) fn render_transitions(&mut self, surface_view: &wgpu::TextureView) {
        let now = std::time::Instant::now();
        let renderer = match self.renderer.as_ref() {
            Some(r) => r,
            None => return,
        };

        // Get current offscreen bind group for "new" texture
        let current_bg = match self.current_offscreen_view_and_bg() {
            Some((_, bg)) => bg as *const wgpu::BindGroup,
            None => return,
        };

        // Render crossfades (using per-transition effect/easing)
        let mut completed_crossfades = Vec::new();
        for (&wid, transition) in &self.transitions.crossfades {
            let elapsed = now.duration_since(transition.started);
            let raw_t = (elapsed.as_secs_f32() / transition.duration.as_secs_f32()).min(1.0);
            let elapsed_secs = elapsed.as_secs_f32();

            // SAFETY: current_bg is valid for the duration of this function
            renderer.render_scroll_effect(
                surface_view,
                &transition.old_bind_group,
                unsafe { &*current_bg },
                raw_t,
                elapsed_secs,
                1, // direction: forward
                &transition.bounds,
                transition.effect,
                transition.easing,
                self.width,
                self.height,
            );

            if raw_t >= 1.0 {
                completed_crossfades.push(wid);
            }
        }
        for wid in completed_crossfades {
            self.transitions.crossfades.remove(&wid);
        }

        // Render scroll slides
        let mut completed_scrolls = Vec::new();
        for (&wid, transition) in &self.transitions.scroll_slides {
            let elapsed = now.duration_since(transition.started);
            let raw_t = (elapsed.as_secs_f32() / transition.duration.as_secs_f32()).min(1.0);
            let elapsed_secs = elapsed.as_secs_f32();

            renderer.render_scroll_effect(
                surface_view,
                &transition.old_bind_group,
                unsafe { &*current_bg },
                raw_t,
                elapsed_secs,
                transition.direction,
                &transition.bounds,
                transition.effect,
                transition.easing,
                self.width,
                self.height,
            );

            if raw_t >= 1.0 {
                completed_scrolls.push(wid);
            }
        }
        for wid in completed_scrolls {
            self.transitions.scroll_slides.remove(&wid);
        }
    }
}
