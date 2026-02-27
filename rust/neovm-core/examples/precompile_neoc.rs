use neovm_core::emacs_core::load::precompile_source_file;
use std::path::PathBuf;

fn main() {
    let paths: Vec<PathBuf> = std::env::args().skip(1).map(PathBuf::from).collect();
    if paths.is_empty() {
        eprintln!("usage: precompile_neoc <source.el> [source2.el ...]");
        std::process::exit(2);
    }

    let mut had_errors = false;
    for path in &paths {
        match precompile_source_file(path) {
            Ok(cache_path) => {
                println!("{} -> {}", path.display(), cache_path.display());
            }
            Err(err) => {
                had_errors = true;
                eprintln!("failed to precompile {}: {:?}", path.display(), err);
            }
        }
    }

    if had_errors {
        std::process::exit(1);
    }
}
