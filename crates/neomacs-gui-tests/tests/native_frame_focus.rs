//! Issue #395: native focus must determine where real keyboard input lands.
//!
//! Run explicitly until the bug is fixed:
//! `cargo nextest run -p neomacs-gui-tests --test native_frame_focus --run-ignored only`
//! `NEOMACS_GUI_TEST_BINARY` can also point to GNU Emacs for the same X11 scenario.

#![cfg(target_os = "linux")]

use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    thread,
    time::{Duration, Instant},
};

use neomacs_gui_tests::{
    DisplayHarness, GuiBackend, GuiRunOptions, GuiScenario, GuiTestPlan, ProcessGuiCommandRunner,
};
use serde_json::Value;

#[test]
#[ignore = "known bug #395: focus routes input to the old frame; requires Xvfb and xdotool"]
fn native_x11_focus_routes_typing_to_the_focused_frame() {
    let root = PathBuf::from(env!("CARGO_WORKSPACE_DIR"));
    let artifacts = root.join(format!(
        "target/neomacs-gui-tests/native-frame-focus-{}",
        std::process::id()
    ));
    fs::create_dir_all(artifacts.parent().unwrap()).unwrap();
    fs::create_dir(&artifacts).expect("fresh focus test artifacts");
    let session = DisplayHarness::Xvfb
        .start_session(&artifacts)
        .expect("isolated headless X11 display");
    xdotool(session.env(), &["getdisplaygeometry"]).expect("X11 client connection");
    let binary = std::env::var_os("NEOMACS_GUI_TEST_BINARY")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("target/release/neomacs"));
    let mut plan = GuiTestPlan::new(
        GuiBackend::LinuxX11,
        &root,
        &artifacts,
        GuiScenario::new(
            "native-frame-focus",
            root.join("crates/neomacs-gui-tests/fixtures/native-frame-focus.el"),
        ),
    )
    .with_program(binary)
    .with_env("NEOMACS_GUI_FOCUS_CONTROL", artifacts.display().to_string());
    for (key, value) in session.env() {
        plan = plan.with_env(key, value);
    }

    let (run, observation) = thread::scope(|scope| {
        let run = scope.spawn(|| {
            plan.run_with(
                &mut ProcessGuiCommandRunner,
                GuiRunOptions::with_timeout(Duration::from_secs(45)),
            )
        });
        let observation = exercise_native_input(&artifacts, session.env(), || run.is_finished());
        // Let the fixture capture its final display even when the behavioral
        // assertion fails. The process runner also enforces a hard deadline.
        fs::write(artifacts.join("stop"), "stop").expect("stop focus fixture");
        (run.join().expect("GUI runner thread"), observation)
    });
    let run = run.expect("focus test GUI artifacts");
    assert!(
        observation.is_ok(),
        "{observation:?}; artifacts: {}",
        artifacts.display()
    );
    assert!(!run.timed_out, "{run:#?}");
    assert_eq!(run.exit_code, Some(0), "{run:#?}");
}

fn exercise_native_input(
    artifacts: &Path,
    env: &[(String, String)],
    finished: impl Fn() -> bool,
) -> Result<(), String> {
    let ready = wait_for_state(artifacts, "ready", 0, &finished)?;
    let pid = ready["pid"]
        .as_u64()
        .ok_or("fixture did not report its PID")?
        .to_string();
    let primary = wait_for_window(env, &pid, None, &finished)?;
    focus_window(env, &primary)?;
    type_text(env, "p")?;
    expect_buffers(artifacts, "initial", "p", "", &finished)?;

    // Use XTEST, not `key --window`: winit ignores XSendEvent-style keys.
    xdotool(env, &["key", "--clearmodifiers", "ctrl+x", "5", "2"])?;
    let secondary = wait_for_window(env, &pid, Some(&primary), &finished)?;
    // Xvfb has no window manager to activate a new window for us. Set and
    // verify actual X input focus without selecting any Lisp frame.
    focus_window(env, &secondary)?;
    type_text(env, "n")?;
    expect_buffers(artifacts, "new-frame", "p", "n", &finished)?;

    focus_window(env, &primary)?;
    type_text(env, "aa")?;
    expect_buffers(artifacts, "focus-primary", "paa", "n", &finished)?;

    focus_window(env, &secondary)?;
    type_text(env, "bb")?;
    expect_buffers(artifacts, "focus-secondary", "paa", "nbb", &finished)
}

fn expect_buffers(
    artifacts: &Path,
    stage: &str,
    primary: &str,
    secondary: &str,
    finished: &impl Fn() -> bool,
) -> Result<(), String> {
    let state = wait_for_state(artifacts, stage, primary.len() + secondary.len(), finished)?;
    fs::write(
        artifacts.join(format!("{stage}.json")),
        serde_json::to_string_pretty(&state).unwrap(),
    )
    .map_err(|error| error.to_string())?;
    if state["primary"] != primary || state["secondary"] != secondary {
        return Err(format!(
            "{stage}: native keys went to the wrong buffer; expected primary={primary:?}, \
             secondary={secondary:?}; observed {state}"
        ));
    }
    Ok(())
}

fn wait_for_state(
    artifacts: &Path,
    stage: &str,
    text_bytes: usize,
    finished: &impl Fn() -> bool,
) -> Result<Value, String> {
    let deadline = Instant::now() + Duration::from_secs(12);
    let mut last = Value::Null;
    loop {
        if let Ok(bytes) = fs::read(artifacts.join("state.json"))
            && let Ok(state) = serde_json::from_slice::<Value>(&bytes)
        {
            let received = state["primary"].as_str().map(str::len).unwrap_or(0)
                + state["secondary"].as_str().map(str::len).unwrap_or(0);
            if received >= text_bytes {
                return Ok(state);
            }
            last = state;
        }
        if finished() || Instant::now() >= deadline {
            return Err(format!(
                "{stage}: timed out waiting for input; last state {last}"
            ));
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn wait_for_window(
    env: &[(String, String)],
    pid: &str,
    exclude: Option<&str>,
    finished: &impl Fn() -> bool,
) -> Result<String, String> {
    let deadline = Instant::now() + Duration::from_secs(12);
    loop {
        if let Ok(windows) = xdotool(env, &["search", "--onlyvisible", "--pid", pid])
            && let Some(window) = windows.lines().find(|window| Some(*window) != exclude)
        {
            return Ok(window.to_owned());
        }
        if finished() || Instant::now() >= deadline {
            return Err(format!(
                "native window for PID {pid} excluding {exclude:?} did not appear"
            ));
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn focus_window(env: &[(String, String)], window: &str) -> Result<(), String> {
    xdotool(env, &["windowfocus", window])?;
    let focused = xdotool(env, &["getwindowfocus"])?;
    if focused.trim() != window {
        return Err(format!("X11 focused {}, expected {window}", focused.trim()));
    }
    eprintln!("Verified native X11 keyboard focus: window {window}");
    Ok(())
}

fn type_text(env: &[(String, String)], text: &str) -> Result<(), String> {
    xdotool(env, &["type", "--clearmodifiers", "--delay", "20", text]).map(|_| ())
}

fn xdotool(env: &[(String, String)], args: &[&str]) -> Result<String, String> {
    let output = Command::new("xdotool")
        .args(args)
        .envs(env.iter().map(|(key, value)| (key, value)))
        .output()
        .map_err(|error| format!("xdotool {args:?}: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "xdotool {args:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}
