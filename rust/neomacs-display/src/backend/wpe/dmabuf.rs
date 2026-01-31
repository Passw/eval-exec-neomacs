//! DMA-BUF texture conversion.
//!
//! Converts EGLImages from WPE WebKit to GdkTextures using DMA-BUF
//! for zero-copy GPU-to-GPU transfer.

use std::ffi::CStr;
use std::ptr;

use gdk4::prelude::*;
use gdk4::Texture;

use crate::core::error::{DisplayError, DisplayResult};
use super::sys::egl;

/// DMA-BUF texture exporter using EGL MESA extensions.
pub struct DmaBufExporter {
    /// EGL display
    egl_display: egl::EGLDisplay,
    /// Function pointer for eglExportDMABUFImageMESA
    export_dmabuf_image: Option<egl::PFNEGLEXPORTDMABUFIMAGEMESAPROC>,
    /// Function pointer for eglExportDMABUFImageQueryMESA  
    export_dmabuf_query: Option<egl::PFNEGLEXPORTDMABUFIMAGEQUERYMESAPROC>,
    /// Whether DMA-BUF export is supported
    supported: bool,
}

impl DmaBufExporter {
    /// Create a new DMA-BUF exporter for the given EGL display.
    pub fn new(egl_display: *mut libc::c_void) -> Self {
        let egl_display = egl_display as egl::EGLDisplay;
        
        if egl_display.is_null() {
            log::warn!("DmaBufExporter: NULL EGL display");
            return Self {
                egl_display,
                export_dmabuf_image: None,
                export_dmabuf_query: None,
                supported: false,
            };
        }

        // Check for EGL_MESA_image_dma_buf_export extension
        let extensions = unsafe {
            let ext_ptr = egl::eglQueryString(egl_display, egl::EGL_EXTENSIONS as i32);
            if ext_ptr.is_null() {
                String::new()
            } else {
                CStr::from_ptr(ext_ptr).to_string_lossy().into_owned()
            }
        };

        let has_export_ext = extensions.contains("EGL_MESA_image_dma_buf_export");
        
        if !has_export_ext {
            log::info!("DmaBufExporter: EGL_MESA_image_dma_buf_export not supported");
            return Self {
                egl_display,
                export_dmabuf_image: None,
                export_dmabuf_query: None,
                supported: false,
            };
        }

        // Get function pointers
        let export_dmabuf_image = unsafe {
            let name = b"eglExportDMABUFImageMESA\0";
            let proc = egl::eglGetProcAddress(name.as_ptr() as *const i8);
            if proc.is_none() {
                None
            } else {
                Some(std::mem::transmute::<_, egl::PFNEGLEXPORTDMABUFIMAGEMESAPROC>(proc.unwrap()))
            }
        };

        let export_dmabuf_query = unsafe {
            let name = b"eglExportDMABUFImageQueryMESA\0";
            let proc = egl::eglGetProcAddress(name.as_ptr() as *const i8);
            if proc.is_none() {
                None
            } else {
                Some(std::mem::transmute::<_, egl::PFNEGLEXPORTDMABUFIMAGEQUERYMESAPROC>(proc.unwrap()))
            }
        };

        let supported = export_dmabuf_image.is_some() && export_dmabuf_query.is_some();
        
        if supported {
            log::info!("DmaBufExporter: DMA-BUF export initialized successfully");
        } else {
            log::warn!("DmaBufExporter: Failed to get extension function pointers");
        }

        Self {
            egl_display,
            export_dmabuf_image,
            export_dmabuf_query,
            supported,
        }
    }

    /// Check if DMA-BUF export is supported.
    pub fn is_supported(&self) -> bool {
        self.supported
    }

    /// Convert an EGLImage to a GdkTexture using DMA-BUF.
    ///
    /// Returns a GdkDmabufTexture that can be used for GPU-accelerated compositing.
    pub fn egl_image_to_texture(
        &self,
        egl_image: *mut libc::c_void,
        width: u32,
        height: u32,
        gdk_display: &gdk4::Display,
    ) -> DisplayResult<Texture> {
        if !self.supported {
            return Err(DisplayError::WebKit("DMA-BUF export not supported".into()));
        }

        if egl_image.is_null() {
            return Err(DisplayError::WebKit("NULL EGLImage".into()));
        }

        let export_query = self.export_dmabuf_query.unwrap();
        let export_image = self.export_dmabuf_image.unwrap();

        unsafe {
            // Query the format and number of planes
            let mut fourcc: libc::c_int = 0;
            let mut num_planes: libc::c_int = 0;
            let mut modifier: u64 = 0;

            let query_result = export_query(
                self.egl_display,
                egl_image as egl::EGLImageKHR,
                &mut fourcc,
                &mut num_planes,
                &mut modifier,
            );

            if query_result == 0 {
                return Err(DisplayError::WebKit("eglExportDMABUFImageQueryMESA failed".into()));
            }

            if num_planes < 1 || num_planes > 4 {
                return Err(DisplayError::WebKit(format!("Invalid plane count: {}", num_planes)));
            }

            // Export the DMA-BUF file descriptors
            let mut fds: [libc::c_int; 4] = [-1; 4];
            let mut strides: [i32; 4] = [0; 4];
            let mut offsets: [i32; 4] = [0; 4];

            let export_result = export_image(
                self.egl_display,
                egl_image as egl::EGLImageKHR,
                fds.as_mut_ptr(),
                strides.as_mut_ptr(),
                offsets.as_mut_ptr(),
            );

            if export_result == 0 {
                return Err(DisplayError::WebKit("eglExportDMABUFImageMESA failed".into()));
            }

            // Create GdkDmabufTextureBuilder
            let builder = gdk4::DmabufTextureBuilder::new();
            builder.set_display(gdk_display);
            builder.set_width(width);
            builder.set_height(height);
            builder.set_fourcc(fourcc as u32);
            builder.set_modifier(modifier);
            builder.set_n_planes(num_planes as u32);

            // Set plane info
            for i in 0..num_planes as usize {
                if fds[i] >= 0 {
                    builder.set_fd(i as u32, fds[i]);
                    builder.set_stride(i as u32, strides[i] as u32);
                    builder.set_offset(i as u32, offsets[i] as u32);
                }
            }

            // Build the texture
            match builder.build() {
                Ok(texture) => {
                    log::trace!(
                        "DMA-BUF texture created: {}x{}, fourcc={:08x}, planes={}, modifier={:016x}",
                        width, height, fourcc, num_planes, modifier
                    );
                    Ok(texture.upcast::<Texture>())
                }
                Err(e) => {
                    // Close file descriptors on error
                    for i in 0..num_planes as usize {
                        if fds[i] >= 0 {
                            libc::close(fds[i]);
                        }
                    }
                    Err(DisplayError::WebKit(format!("Failed to build DmabufTexture: {}", e)))
                }
            }
        }
    }
}

impl Default for DmaBufExporter {
    fn default() -> Self {
        // Create with current EGL display
        unsafe {
            let display = egl::eglGetCurrentDisplay();
            Self::new(display as *mut libc::c_void)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dmabuf_exporter_without_display() {
        let exporter = DmaBufExporter::new(ptr::null_mut());
        assert!(!exporter.is_supported());
    }
}
