//! `infra` — materialize and inspect shared editor-config fixtures.

use std::process::exit;

fn main() {
    let mut args = std::env::args_os().skip(1);
    match args.next().as_deref().and_then(|arg| arg.to_str()) {
        Some("materialize") => match args.next().as_deref().and_then(|arg| arg.to_str()) {
            Some("doom") => {
                let source = match args.next() {
                    Some(path) => neomacs_infra::DoomSource::Operator(path.into()),
                    None => neomacs_infra::DoomSource::resolve()
                        .unwrap_or(neomacs_infra::DoomSource::Pinned),
                };
                match neomacs_infra::DoomEnvironment::materialize(source) {
                    Ok(environment) => {
                        println!("doom fixture ready at {}", environment.tree().display());
                    }
                    Err(error) => {
                        eprintln!("error: {error}");
                        exit(1);
                    }
                }
            }
            other => {
                eprintln!("usage: infra materialize doom");
                if let Some(other) = other {
                    eprintln!("unknown environment: {other}");
                }
                exit(2);
            }
        },
        Some("status") => match neomacs_infra::doom_status() {
            Ok(status) => println!("doom: {status}"),
            Err(status) => println!("doom: {status}"),
        },
        _ => {
            eprintln!("usage: infra <materialize doom|status>");
            exit(2);
        }
    }
}
