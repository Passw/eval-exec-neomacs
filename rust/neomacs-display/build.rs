//! Build script for neomacs-display
//!
//! Generates C header files for FFI using cbindgen.

use std::env;
use std::path::PathBuf;

fn main() {
    // Only generate headers if cbindgen is available
    if let Ok(_) = which::which("cbindgen") {
        let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let output_file = PathBuf::from(&crate_dir)
            .join("include")
            .join("neomacs_display.h");

        // Ensure include directory exists
        std::fs::create_dir_all(PathBuf::from(&crate_dir).join("include")).ok();

        // Generate header
        let config = cbindgen::Config::from_file("cbindgen.toml")
            .unwrap_or_default();

        cbindgen::Builder::new()
            .with_crate(&crate_dir)
            .with_config(config)
            .generate()
            .map(|bindings| bindings.write_to_file(&output_file))
            .ok();

        println!("cargo:rerun-if-changed=src/ffi.rs");
        println!("cargo:rerun-if-changed=cbindgen.toml");
    }
}
