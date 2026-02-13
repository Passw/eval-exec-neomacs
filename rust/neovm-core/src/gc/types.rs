//! GC object tags and header layout.

/// Object type tags for GC-managed objects.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum GcTag {
    /// Cons cell (car + cdr = 2 slots).
    Cons = 0,
    /// Heap-allocated string (length + UTF-8 bytes).
    String = 1,
    /// Vector (length + elements).
    Vector = 2,
    /// Lambda/closure (params + body + env).
    Lambda = 3,
    /// Hash table.
    HashTable = 4,
    /// Interned symbol.
    Symbol = 5,
    /// Forwarding pointer (used during GC copy).
    Forwarded = 255,
}

impl GcTag {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            0 => Some(GcTag::Cons),
            1 => Some(GcTag::String),
            2 => Some(GcTag::Vector),
            3 => Some(GcTag::Lambda),
            4 => Some(GcTag::HashTable),
            5 => Some(GcTag::Symbol),
            255 => Some(GcTag::Forwarded),
            _ => None,
        }
    }
}

/// Object header stored at the beginning of each GC-managed object.
///
/// Layout: 8 bytes total.
/// - tag: 1 byte (GcTag)
/// - flags: 1 byte (bit 0 = marked, bit 1 = pinned)
/// - _pad: 2 bytes
/// - size: 4 bytes (total object size in bytes including header)
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct ObjHeader {
    pub tag: u8,
    pub flags: u8,
    pub _pad: u16,
    /// Total object size in bytes (including this header).
    pub size: u32,
}

impl ObjHeader {
    pub const SIZE: usize = std::mem::size_of::<ObjHeader>();

    pub fn new(tag: GcTag, size: u32) -> Self {
        Self {
            tag: tag as u8,
            flags: 0,
            _pad: 0,
            size,
        }
    }

    pub fn gc_tag(&self) -> Option<GcTag> {
        GcTag::from_u8(self.tag)
    }

    pub fn is_forwarded(&self) -> bool {
        self.tag == GcTag::Forwarded as u8
    }

    pub fn is_marked(&self) -> bool {
        self.flags & 1 != 0
    }

    pub fn set_marked(&mut self, marked: bool) {
        if marked {
            self.flags |= 1;
        } else {
            self.flags &= !1;
        }
    }

    pub fn is_pinned(&self) -> bool {
        self.flags & 2 != 0
    }
}

/// Forwarding pointer — replaces the header+payload during copying GC.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct ForwardingPtr {
    pub header: ObjHeader,
    /// Pointer to the new location in to_space.
    pub new_addr: usize,
}

impl ForwardingPtr {
    pub fn new(new_addr: usize, original_size: u32) -> Self {
        Self {
            header: ObjHeader::new(GcTag::Forwarded, original_size),
            new_addr,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_size() {
        assert_eq!(ObjHeader::SIZE, 8);
    }

    #[test]
    fn header_flags() {
        let mut h = ObjHeader::new(GcTag::Cons, 24);
        assert!(!h.is_marked());
        h.set_marked(true);
        assert!(h.is_marked());
        h.set_marked(false);
        assert!(!h.is_marked());
    }

    #[test]
    fn tag_round_trip() {
        for tag in [
            GcTag::Cons,
            GcTag::String,
            GcTag::Vector,
            GcTag::Lambda,
            GcTag::Symbol,
        ] {
            assert_eq!(GcTag::from_u8(tag as u8), Some(tag));
        }
    }
}
