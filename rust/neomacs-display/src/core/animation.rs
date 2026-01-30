//! Animation system for smooth scrolling, cursor blink, etc.

use std::time::{Duration, Instant};

/// Easing functions for animations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Easing {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
}

impl Easing {
    /// Apply easing function to a value t in [0, 1]
    pub fn apply(&self, t: f32) -> f32 {
        let t = t.clamp(0.0, 1.0);
        match self {
            Easing::Linear => t,
            Easing::EaseIn => t * t,
            Easing::EaseOut => 1.0 - (1.0 - t) * (1.0 - t),
            Easing::EaseInOut => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    1.0 - (-2.0 * t + 2.0).powi(2) / 2.0
                }
            }
        }
    }
}

/// A single animation
#[derive(Debug, Clone)]
pub struct Animation {
    /// Start value
    pub from: f32,

    /// End value
    pub to: f32,

    /// Duration
    pub duration: Duration,

    /// Start time
    pub start_time: Instant,

    /// Easing function
    pub easing: Easing,

    /// Is this animation complete?
    pub completed: bool,
}

impl Animation {
    /// Create a new animation
    pub fn new(from: f32, to: f32, duration: Duration, easing: Easing) -> Self {
        Self {
            from,
            to,
            duration,
            start_time: Instant::now(),
            easing,
            completed: false,
        }
    }

    /// Get current value at time `now`
    pub fn value_at(&mut self, now: Instant) -> f32 {
        let elapsed = now.duration_since(self.start_time);
        
        if elapsed >= self.duration {
            self.completed = true;
            return self.to;
        }

        let t = elapsed.as_secs_f32() / self.duration.as_secs_f32();
        let eased_t = self.easing.apply(t);
        
        self.from + (self.to - self.from) * eased_t
    }

    /// Get current value (using current time)
    pub fn current_value(&mut self) -> f32 {
        self.value_at(Instant::now())
    }

    /// Check if animation is complete
    pub fn is_complete(&self) -> bool {
        self.completed
    }
}

/// Animation manager handles all active animations
#[derive(Debug)]
pub struct AnimationManager {
    /// Scroll animations by window ID
    scroll_animations: Vec<(i32, Animation)>,

    /// Cursor blink state
    cursor_blink_on: bool,
    last_cursor_toggle: Instant,
    cursor_blink_interval: Duration,

    /// Frame time tracking
    last_frame_time: Option<Instant>,
}

impl Default for AnimationManager {
    fn default() -> Self {
        Self::new()
    }
}

impl AnimationManager {
    pub fn new() -> Self {
        Self {
            scroll_animations: Vec::new(),
            cursor_blink_on: true,
            last_cursor_toggle: Instant::now(),
            cursor_blink_interval: Duration::from_millis(530),
            last_frame_time: None,
        }
    }

    /// Start a smooth scroll animation for a window
    pub fn animate_scroll(&mut self, window_id: i32, from: f32, to: f32) {
        // Remove any existing scroll animation for this window
        self.scroll_animations.retain(|(id, _)| *id != window_id);

        let animation = Animation::new(
            from,
            to,
            Duration::from_millis(150),
            Easing::EaseOut,
        );

        self.scroll_animations.push((window_id, animation));
    }

    /// Get current scroll offset for a window (returns None if no animation)
    pub fn get_scroll_offset(&mut self, window_id: i32) -> Option<f32> {
        let now = Instant::now();
        
        for (id, anim) in &mut self.scroll_animations {
            if *id == window_id {
                return Some(anim.value_at(now));
            }
        }
        
        None
    }

    /// Update all animations, returns true if any animation is active
    pub fn tick(&mut self) -> bool {
        let now = Instant::now();
        self.last_frame_time = Some(now);

        // Update cursor blink
        if now.duration_since(self.last_cursor_toggle) >= self.cursor_blink_interval {
            self.cursor_blink_on = !self.cursor_blink_on;
            self.last_cursor_toggle = now;
        }

        // Remove completed scroll animations
        self.scroll_animations.retain(|(_, anim)| !anim.is_complete());

        // Return true if there are active animations
        !self.scroll_animations.is_empty()
    }

    /// Get cursor visibility (for blinking)
    pub fn cursor_visible(&self) -> bool {
        self.cursor_blink_on
    }

    /// Reset cursor blink (call when cursor moves)
    pub fn reset_cursor_blink(&mut self) {
        self.cursor_blink_on = true;
        self.last_cursor_toggle = Instant::now();
    }

    /// Set cursor blink interval
    pub fn set_cursor_blink_interval(&mut self, interval: Duration) {
        self.cursor_blink_interval = interval;
    }

    /// Check if any animations are running
    pub fn has_active_animations(&self) -> bool {
        !self.scroll_animations.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread::sleep;

    #[test]
    fn test_easing() {
        assert_eq!(Easing::Linear.apply(0.5), 0.5);
        assert!(Easing::EaseIn.apply(0.5) < 0.5);
        assert!(Easing::EaseOut.apply(0.5) > 0.5);
    }

    #[test]
    fn test_animation() {
        let mut anim = Animation::new(0.0, 100.0, Duration::from_millis(100), Easing::Linear);
        
        // At start
        let v1 = anim.current_value();
        assert!(v1 < 50.0);
        
        // Wait and check progress
        sleep(Duration::from_millis(50));
        let v2 = anim.current_value();
        assert!(v2 > v1);
        
        // Wait until complete
        sleep(Duration::from_millis(60));
        let v3 = anim.current_value();
        assert_eq!(v3, 100.0);
        assert!(anim.is_complete());
    }
}
