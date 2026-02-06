//! Animation configuration system.
//!
//! Provides user-configurable animation settings that can be controlled
//! from Emacs Lisp via `setq` or `customize`.

use std::time::Duration;
use crate::core::cursor_animation::CursorAnimationMode;
use crate::core::buffer_transition::BufferTransitionEffect;
use crate::core::scroll_animation::{ScrollEffect, ScrollEasing};

/// Master animation configuration
#[derive(Debug, Clone)]
pub struct AnimationConfig {
    /// Master switch - disable all animations
    pub enabled: bool,
    
    /// Cursor animation settings
    pub cursor: CursorAnimationConfig,
    
    /// Buffer transition settings
    pub buffer_transition: BufferTransitionConfig,
    
    /// Scroll animation settings
    pub scroll: ScrollAnimationConfig,
}

impl Default for AnimationConfig {
    fn default() -> Self {
        Self {
            enabled: false, // Disabled by default - user opts in
            cursor: CursorAnimationConfig::default(),
            buffer_transition: BufferTransitionConfig::default(),
            scroll: ScrollAnimationConfig::default(),
        }
    }
}

impl AnimationConfig {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Enable all animations with sensible defaults
    pub fn enable_all(&mut self) {
        self.enabled = true;
        self.cursor.enabled = true;
        self.buffer_transition.enabled = true;
        self.scroll.enabled = true;
    }
    
    /// Disable all animations
    pub fn disable_all(&mut self) {
        self.enabled = false;
    }
    
    /// Check if cursor animation should run
    pub fn cursor_animation_active(&self) -> bool {
        self.enabled && self.cursor.enabled
    }
    
    /// Check if buffer transition should run
    pub fn buffer_transition_active(&self) -> bool {
        self.enabled && self.buffer_transition.enabled
    }
    
    /// Check if scroll animation should run
    pub fn scroll_animation_active(&self) -> bool {
        self.enabled && self.scroll.enabled
    }
}

/// Cursor animation configuration
#[derive(Debug, Clone)]
pub struct CursorAnimationConfig {
    /// Enable cursor animation
    pub enabled: bool,
    
    /// Animation mode/style
    pub mode: CursorAnimationMode,
    
    /// Animation speed (higher = faster, 1-100)
    pub speed: f32,
    
    /// Enable cursor glow effect
    pub glow: bool,
    
    /// Glow intensity (0.0 - 1.0)
    pub glow_intensity: f32,
    
    /// Particle count for particle effects
    pub particle_count: u32,
    
    /// Particle trail length
    pub trail_length: u32,
}

impl Default for CursorAnimationConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            mode: CursorAnimationMode::Smooth, // Just smooth movement by default
            speed: 15.0,
            glow: false,
            glow_intensity: 0.3,
            particle_count: 15,
            trail_length: 40,
        }
    }
}

/// Buffer transition configuration
#[derive(Debug, Clone)]
pub struct BufferTransitionConfig {
    /// Enable buffer switch animations
    pub enabled: bool,
    
    /// Transition effect type
    pub effect: BufferTransitionEffect,
    
    /// Transition duration in milliseconds
    pub duration_ms: u32,
    
    /// Auto-detect buffer switches (vs explicit trigger)
    pub auto_detect: bool,
}

impl Default for BufferTransitionConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            effect: BufferTransitionEffect::Crossfade,
            duration_ms: 200,
            auto_detect: true,
        }
    }
}

impl BufferTransitionConfig {
    pub fn duration(&self) -> Duration {
        Duration::from_millis(self.duration_ms as u64)
    }
}

/// Scroll animation configuration
#[derive(Debug, Clone)]
pub struct ScrollAnimationConfig {
    /// Enable smooth scrolling
    pub enabled: bool,

    /// Scroll animation duration in milliseconds
    pub duration_ms: u32,

    /// Lines to scroll before animation kicks in (1 = always animate)
    pub threshold_lines: u32,

    /// Visual effect for scroll transitions
    pub effect: ScrollEffect,

    /// Easing/physics model for scroll timing
    pub easing: ScrollEasing,
}

impl Default for ScrollAnimationConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            duration_ms: 150,
            threshold_lines: 1,
            effect: ScrollEffect::default(),
            easing: ScrollEasing::default(),
        }
    }
}

/// Builder for animation config from string options
impl AnimationConfig {
    /// Set option by name (for Lisp integration)
    /// Returns true if option was recognized
    pub fn set_option(&mut self, name: &str, value: &str) -> bool {
        match name {
            // Master switch
            "animation" | "animations" => {
                self.enabled = parse_bool(value);
                true
            }
            
            // Cursor options
            "cursor-animation" => {
                self.cursor.enabled = parse_bool(value);
                true
            }
            "cursor-animation-mode" | "cursor-animation-style" => {
                self.cursor.mode = CursorAnimationMode::from_str(value);
                true
            }
            "cursor-animation-speed" => {
                if let Ok(v) = value.parse::<f32>() {
                    self.cursor.speed = v.clamp(1.0, 100.0);
                }
                true
            }
            "cursor-glow" => {
                self.cursor.glow = parse_bool(value);
                true
            }
            "cursor-glow-intensity" => {
                if let Ok(v) = value.parse::<f32>() {
                    self.cursor.glow_intensity = v.clamp(0.0, 1.0);
                }
                true
            }
            "cursor-particle-count" => {
                if let Ok(v) = value.parse::<u32>() {
                    self.cursor.particle_count = v.clamp(1, 100);
                }
                true
            }
            
            // Buffer transition options
            "buffer-transition" | "buffer-switch-animation" => {
                self.buffer_transition.enabled = parse_bool(value);
                true
            }
            "buffer-transition-effect" | "buffer-transition-style" => {
                self.buffer_transition.effect = BufferTransitionEffect::from_str(value);
                true
            }
            "buffer-transition-duration" => {
                if let Ok(v) = value.parse::<u32>() {
                    self.buffer_transition.duration_ms = v.clamp(50, 1000);
                }
                true
            }
            
            // Scroll options
            "scroll-animation" | "smooth-scroll" => {
                self.scroll.enabled = parse_bool(value);
                true
            }
            "scroll-animation-duration" => {
                if let Ok(v) = value.parse::<u32>() {
                    self.scroll.duration_ms = v.clamp(50, 500);
                }
                true
            }
            "scroll-effect" | "scroll-animation-effect" | "scroll-style" => {
                self.scroll.effect = ScrollEffect::from_str(value);
                true
            }
            "scroll-easing" | "scroll-animation-easing" => {
                self.scroll.easing = ScrollEasing::from_str(value);
                true
            }

            _ => false,
        }
    }
    
    /// Get option value as string (for Lisp integration)
    pub fn get_option(&self, name: &str) -> Option<String> {
        match name {
            "animation" | "animations" => Some(bool_str(self.enabled)),
            "cursor-animation" => Some(bool_str(self.cursor.enabled)),
            "cursor-animation-mode" => Some(format!("{:?}", self.cursor.mode).to_lowercase()),
            "cursor-animation-speed" => Some(self.cursor.speed.to_string()),
            "cursor-glow" => Some(bool_str(self.cursor.glow)),
            "buffer-transition" => Some(bool_str(self.buffer_transition.enabled)),
            "buffer-transition-effect" => Some(format!("{:?}", self.buffer_transition.effect).to_lowercase()),
            "buffer-transition-duration" => Some(self.buffer_transition.duration_ms.to_string()),
            "scroll-animation" => Some(bool_str(self.scroll.enabled)),
            "scroll-effect" => Some(self.scroll.effect.as_str().to_string()),
            "scroll-easing" => Some(self.scroll.easing.as_str().to_string()),
            _ => None,
        }
    }
}

fn parse_bool(s: &str) -> bool {
    matches!(s.to_lowercase().as_str(), "t" | "true" | "1" | "yes" | "on")
}

fn bool_str(b: bool) -> String {
    if b { "t".to_string() } else { "nil".to_string() }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_default_disabled() {
        let config = AnimationConfig::default();
        assert!(!config.enabled);
    }
    
    #[test]
    fn test_enable_all() {
        let mut config = AnimationConfig::default();
        config.enable_all();
        assert!(config.enabled);
        assert!(config.cursor.enabled);
        assert!(config.buffer_transition.enabled);
    }
    
    #[test]
    fn test_set_option() {
        let mut config = AnimationConfig::default();
        
        assert!(config.set_option("animation", "t"));
        assert!(config.enabled);
        
        assert!(config.set_option("cursor-animation-mode", "railgun"));
        assert_eq!(config.cursor.mode, CursorAnimationMode::Railgun);
        
        assert!(config.set_option("buffer-transition-effect", "page-curl"));
        assert_eq!(config.buffer_transition.effect, BufferTransitionEffect::PageCurl);
    }
}
