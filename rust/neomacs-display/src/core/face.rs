//! Face (text styling) types.

use crate::core::types::Color;
use bitflags::bitflags;

bitflags! {
    /// Face attributes flags
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FaceAttributes: u32 {
        const BOLD = 1 << 0;
        const ITALIC = 1 << 1;
        const UNDERLINE = 1 << 2;
        const OVERLINE = 1 << 3;
        const STRIKE_THROUGH = 1 << 4;
        const INVERSE = 1 << 5;
        const BOX = 1 << 6;
    }
}

/// Underline style
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnderlineStyle {
    #[default]
    None,
    Line,
    Wave,
    Double,
    Dotted,
    Dashed,
}

/// Box type for face
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BoxType {
    #[default]
    None,
    Line,
    Raised3D,
    Sunken3D,
}

/// A face defines text styling (colors, font, decorations)
#[repr(C)]
#[derive(Debug, Clone)]
pub struct Face {
    /// Face ID
    pub id: u32,

    /// Foreground color
    pub foreground: Color,

    /// Background color
    pub background: Color,

    /// Underline color (if different from foreground)
    pub underline_color: Option<Color>,

    /// Overline color
    pub overline_color: Option<Color>,

    /// Strike-through color
    pub strike_through_color: Option<Color>,

    /// Box color
    pub box_color: Option<Color>,

    /// Font family name
    pub font_family: String,

    /// Font size in points (1/72 inch)
    pub font_size: f32,

    /// Font weight (400 = normal, 700 = bold)
    pub font_weight: u16,

    /// Attribute flags
    pub attributes: FaceAttributes,

    /// Underline style
    pub underline_style: UnderlineStyle,

    /// Box type
    pub box_type: BoxType,

    /// Box line width
    pub box_line_width: i32,
}

impl Default for Face {
    fn default() -> Self {
        Self {
            id: 0,
            foreground: Color::WHITE,
            background: Color::BLACK,
            underline_color: None,
            overline_color: None,
            strike_through_color: None,
            box_color: None,
            font_family: "monospace".to_string(),
            font_size: 12.0,
            font_weight: 400,
            attributes: FaceAttributes::empty(),
            underline_style: UnderlineStyle::None,
            box_type: BoxType::None,
            box_line_width: 0,
        }
    }
}

impl Face {
    /// Create a new face with default values
    pub fn new(id: u32) -> Self {
        Self {
            id,
            ..Default::default()
        }
    }

    /// Check if face is bold
    pub fn is_bold(&self) -> bool {
        self.attributes.contains(FaceAttributes::BOLD) || self.font_weight >= 700
    }

    /// Check if face is italic
    pub fn is_italic(&self) -> bool {
        self.attributes.contains(FaceAttributes::ITALIC)
    }

    /// Check if face has underline
    pub fn has_underline(&self) -> bool {
        self.underline_style != UnderlineStyle::None
    }

    /// Get the underline color (foreground if not explicitly set)
    pub fn get_underline_color(&self) -> Color {
        self.underline_color.unwrap_or(self.foreground)
    }

    /// Create a Pango font description string
    pub fn to_pango_font_description(&self) -> String {
        let mut desc = self.font_family.clone();
        
        if self.is_italic() {
            desc.push_str(" Italic");
        }
        
        if self.is_bold() {
            desc.push_str(" Bold");
        }
        
        desc.push_str(&format!(" {}", self.font_size as i32));
        desc
    }
}

/// Face cache for efficient lookup
#[derive(Debug, Default)]
pub struct FaceCache {
    faces: Vec<Face>,
}

impl FaceCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get face by ID
    pub fn get(&self, id: u32) -> Option<&Face> {
        self.faces.iter().find(|f| f.id == id)
    }

    /// Add or update a face
    pub fn insert(&mut self, face: Face) {
        if let Some(existing) = self.faces.iter_mut().find(|f| f.id == face.id) {
            *existing = face;
        } else {
            self.faces.push(face);
        }
    }

    /// Get default face (ID 0)
    pub fn default_face(&self) -> Option<&Face> {
        self.get(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_face_creation() {
        let face = Face::new(1);
        assert_eq!(face.id, 1);
        assert!(!face.is_bold());
    }

    #[test]
    fn test_pango_font_desc() {
        let mut face = Face::new(0);
        face.font_family = "DejaVu Sans Mono".to_string();
        face.font_size = 14.0;
        face.attributes = FaceAttributes::BOLD | FaceAttributes::ITALIC;
        
        let desc = face.to_pango_font_description();
        assert!(desc.contains("DejaVu Sans Mono"));
        assert!(desc.contains("Bold"));
        assert!(desc.contains("Italic"));
        assert!(desc.contains("14"));
    }
}
