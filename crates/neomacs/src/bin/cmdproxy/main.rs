//! GNU-compatible shell adapter for Neomacs on Windows.
//!
//! Emacs Lisp invokes `shell-file-name` with the Unix-shaped `-c COMMAND`
//! interface. Windows command processors use `/c COMMAND`; this private helper
//! owns that translation and nothing else. Keeping it as a separate process
//! matches GNU Emacs's `nt/cmdproxy.c`, preserves user `COMSPEC` selection,
//! and leaves Neomacs's general subprocess layer free of shell-specific rules.

use std::ffi::{OsStr, OsString};
#[cfg(windows)]
use std::process::Command;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Invocation {
    Interactive {
        pass_through: Vec<OsString>,
    },
    Command {
        command: OsString,
        pass_through: Vec<OsString>,
    },
}

fn is_switch(argument: &OsStr, letter: char) -> bool {
    let text = argument.to_string_lossy();
    let mut chars = text.chars();
    matches!(chars.next(), Some('-' | '/'))
        && chars
            .next()
            .is_some_and(|value| value.eq_ignore_ascii_case(&letter))
        && chars.next().is_none()
}

fn starts_with_switch_marker(argument: &OsStr) -> bool {
    argument.to_string_lossy().starts_with(['-', '/'])
}

fn is_environment_size_switch(argument: &OsStr) -> bool {
    let text = argument.to_string_lossy();
    let bytes = text.as_bytes();
    bytes.len() >= 3
        && matches!(bytes[0], b'-' | b'/')
        && bytes[1].eq_ignore_ascii_case(&b'e')
        && bytes[2] == b':'
}

fn parse_invocation(arguments: impl IntoIterator<Item = OsString>) -> Result<Invocation, String> {
    let arguments = arguments.into_iter().collect::<Vec<_>>();
    let mut pass_through = Vec::new();
    let mut command = None;
    let mut index = 0;

    while index < arguments.len() {
        let argument = &arguments[index];
        if !starts_with_switch_marker(argument) {
            break;
        }
        if is_switch(argument, 'c') {
            index += 1;
            let Some(value) = arguments.get(index) else {
                return Err(format!(
                    "cmdproxy: expecting a command after {}",
                    argument.to_string_lossy()
                ));
            };
            command = Some(value.clone());
        } else if is_switch(argument, 'i') {
            // Interactive is already the no-`-c` default. GNU ignores `-i`
            // after `-c`; the closed Invocation enum makes the same result
            // unambiguous without retaining a second Boolean.
        } else if is_environment_size_switch(argument) {
            // `-e:N` controlled command.com's fixed environment block. Modern
            // cmd.exe has no such limit, so accepting and dropping it is the
            // compatible behavior.
        } else {
            pass_through.push(argument.clone());
        }
        index += 1;
    }

    Ok(match command {
        Some(command) => Invocation::Command {
            command,
            pass_through,
        },
        None => Invocation::Interactive { pass_through },
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct CommandProcessorPlan {
    program: OsString,
    pass_through: Vec<OsString>,
    command: Option<OsString>,
}

fn plan_command_processor(
    invocation: Invocation,
    comspec: Option<OsString>,
) -> Result<CommandProcessorPlan, String> {
    let program = comspec
        .filter(|value| !value.is_empty())
        .ok_or_else(|| "cmdproxy: COMSPEC is not set".to_owned())?;
    let (pass_through, command) = match invocation {
        Invocation::Interactive { pass_through } => (pass_through, None),
        Invocation::Command {
            command,
            pass_through,
        } => (pass_through, Some(command)),
    };
    Ok(CommandProcessorPlan {
        program,
        pass_through,
        command,
    })
}

#[cfg(windows)]
fn run(plan: CommandProcessorPlan) -> Result<i32, String> {
    use std::os::windows::process::CommandExt;

    let mut process = Command::new(&plan.program);
    process.args(&plan.pass_through);
    if let Some(command) = plan.command {
        process.arg("/c");
        // cmd.exe does not use CommandLineToArgvW rules for the command tail.
        // GNU cmdproxy likewise encloses the complete tail in one pair of
        // quotes and leaves any interior quotes untouched.
        let mut quoted = OsString::from("\"");
        quoted.push(command);
        quoted.push("\"");
        process.raw_arg(quoted);
    }
    let status = process
        .status()
        .map_err(|error| format!("cmdproxy: could not run {:?}: {error}", plan.program))?;
    Ok(status.code().unwrap_or(1))
}

#[cfg(not(windows))]
fn run(_plan: CommandProcessorPlan) -> Result<i32, String> {
    Err("cmdproxy is only supported on Windows".to_owned())
}

fn main() {
    let result = parse_invocation(std::env::args_os().skip(1))
        .and_then(|invocation| plan_command_processor(invocation, std::env::var_os("COMSPEC")))
        .and_then(run);
    match result {
        Ok(code) => std::process::exit(code),
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(2);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(values: &[&str]) -> Vec<OsString> {
        values.iter().map(OsString::from).collect()
    }

    #[test]
    fn unix_c_switch_becomes_a_typed_command_invocation() {
        assert_eq!(
            parse_invocation(args(&["-c", "whoami"])),
            Ok(Invocation::Command {
                command: OsString::from("whoami"),
                pass_through: Vec::new(),
            })
        );
    }

    #[test]
    fn windows_c_switch_preserves_the_complete_shell_command() {
        assert_eq!(
            parse_invocation(args(&["/c", "echo one | findstr one"])),
            Ok(Invocation::Command {
                command: OsString::from("echo one | findstr one"),
                pass_through: Vec::new(),
            })
        );
    }

    #[test]
    fn no_command_is_an_interactive_invocation_with_shell_switches() {
        assert_eq!(
            parse_invocation(args(&["/q"])),
            Ok(Invocation::Interactive {
                pass_through: args(&["/q"]),
            })
        );
    }

    #[test]
    fn command_dot_com_environment_size_is_accepted_but_other_e_switches_pass_through() {
        assert_eq!(
            parse_invocation(args(&["-e:4096", "-example"])),
            Ok(Invocation::Interactive {
                pass_through: args(&["-example"]),
            })
        );
    }

    #[test]
    fn c_without_a_command_is_rejected_before_launch() {
        assert_eq!(
            parse_invocation(args(&["-c"])),
            Err("cmdproxy: expecting a command after -c".to_owned())
        );
    }

    #[test]
    fn command_processor_plan_requires_comspec() {
        let invocation = parse_invocation(args(&["-c", "whoami"])).unwrap();
        assert_eq!(
            plan_command_processor(invocation, None),
            Err("cmdproxy: COMSPEC is not set".to_owned())
        );
    }

    #[test]
    fn command_processor_plan_keeps_comspec_and_command_separate() {
        let invocation = parse_invocation(args(&["-c", "echo %USERNAME%"])).unwrap();
        assert_eq!(
            plan_command_processor(invocation, Some(OsString::from(r"C:\Windows\cmd.exe"))),
            Ok(CommandProcessorPlan {
                program: OsString::from(r"C:\Windows\cmd.exe"),
                pass_through: Vec::new(),
                command: Some(OsString::from("echo %USERNAME%")),
            })
        );
    }
}
