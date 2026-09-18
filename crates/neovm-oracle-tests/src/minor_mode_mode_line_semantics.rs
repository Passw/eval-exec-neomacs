//! Oracle parity tests for the mode line's minor-mode segment.
//!
//! GNU builds that segment in `lisp/bindings.el` `mode-line--minor-modes`,
//! which has FOUR branches selected by `mode-line-collapse-minor-modes`:
//! nil (default) shows `minor-mode-alist` verbatim, `(not MODES)` hides
//! MODES, a plain list shows only those MODES, and `t` collapses everything
//! behind a single indicator.
//!
//! Why this is worth pinning: the segment is `minor-mode-alist` order
//! verbatim in the default branch, so a wrong order there is visible as a
//! reordered mode line, and NOTHING in this suite covered this function --
//! `add/minor_mode_semantics.rs` covers `add-minor-mode` (the inserter), and
//! `run/mode_hooks_semantics.rs` covers mode hooks, but the constructor that
//! turns the alist into a mode line had no oracle test at all.
//!
//! The second group pins the ORDERING PRIMITIVES that feed it, because
//! "which mode appears first" is decided by them: `add-hook` and
//! `add-to-list` both PREPEND by default, and `define-minor-mode` registers
//! its `:lighter` in `minor-mode-alist` at DEFINITION time (when the file
//! loads), not at enable time.  A divergence in any of these would reorder
//! every mode line.

use crate::common::assert_oracle_parity_expect;
use crate::common::return_if_neovm_enable_oracle_proptest_not_set;

/// The default branch: `mode-line--minor-modes` returns the alist verbatim,
/// so display order IS alist order.
#[test]
fn oracle_mode_line_minor_modes_follows_alist_order_by_default() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(let ((mode-line-collapse-minor-modes nil)
      (minor-mode-alist '((mm-a " A") (mm-b " B") (mm-c " C"))))
  (let ((construct (mode-line--minor-modes)))
    (list (delq nil
                (mapcar (lambda (x) (and (memq x '(mm-a mm-b mm-c)) x))
                        (flatten-tree construct))))))
"#;

    let expect = expect_test::expect![[r#""OK ((mm-a mm-b mm-c))""#]];
    assert_oracle_parity_expect(form, expect);
}

/// The `(not MODES)` branch: the listed modes are hidden and the rest kept.
#[test]
fn oracle_mode_line_minor_modes_not_branch_hides_listed_modes() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(let ((mode-line-collapse-minor-modes '(not (mm-b)))
      (minor-mode-alist '((mm-a " A") (mm-b " B") (mm-c " C"))))
  (let ((construct (mode-line--minor-modes)))
    (list (delq nil
                (mapcar (lambda (x) (and (memq x '(mm-a mm-b mm-c)) x))
                        (flatten-tree construct))))))
"#;

    let expect = expect_test::expect![[r#""OK (nil)""#]];
    assert_oracle_parity_expect(form, expect);
}

/// The plain-list branch: only the listed modes are shown.
#[test]
fn oracle_mode_line_minor_modes_list_branch_shows_only_listed_modes() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(let ((mode-line-collapse-minor-modes '(mm-b))
      (minor-mode-alist '((mm-a " A") (mm-b " B") (mm-c " C"))))
  (let ((construct (mode-line--minor-modes)))
    (list (delq nil
                (mapcar (lambda (x) (and (memq x '(mm-a mm-b mm-c)) x))
                        (flatten-tree construct))))))
"#;

    let expect = expect_test::expect![[r#""OK ((mm-a mm-c))""#]];
    assert_oracle_parity_expect(form, expect);
}

/// `t` collapses every lighter behind one indicator, so no mode shows.
#[test]
fn oracle_mode_line_minor_modes_t_branch_collapses_every_lighter() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(let ((mode-line-collapse-minor-modes t)
      (minor-mode-alist '((mm-a " A") (mm-b " B") (mm-c " C"))))
  (let ((construct (mode-line--minor-modes)))
    (list (delq nil
                (mapcar (lambda (x) (and (memq x '(mm-a mm-b mm-c)) x))
                        (flatten-tree construct))))))
"#;

    let expect = expect_test::expect![[r#""OK (nil)""#]];
    assert_oracle_parity_expect(form, expect);
}

/// `add-hook` prepends by default; APPEND non-nil appends; a DEPTH orders by
/// depth.  The mode line's ordering inherits this.
#[test]
fn oracle_add_hook_prepends_by_default_and_obeys_append_and_depth() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(progn
  (defvar mmo-hook nil)
  (defun mmo-a () nil)
  (defun mmo-b () nil)
  (list
   (progn (setq mmo-hook nil)
          (add-hook 'mmo-hook #'mmo-a)
          (add-hook 'mmo-hook #'mmo-b)
          mmo-hook)
   (progn (setq mmo-hook nil)
          (add-hook 'mmo-hook #'mmo-a nil t)
          (add-hook 'mmo-hook #'mmo-b nil t)
          mmo-hook)
   (progn (setq mmo-hook nil)
          (add-hook 'mmo-hook #'mmo-a 90)
          (add-hook 'mmo-hook #'mmo-b 10)
          mmo-hook)))
"#;

    let expect = expect_test::expect![[r#""OK ((mmo-b mmo-a) (mmo-b mmo-a t) (mmo-b mmo-a))""#]];
    assert_oracle_parity_expect(form, expect);
}

/// `add-to-list` prepends by default and appends when APPEND is non-nil --
/// the primitive `define-minor-mode` and Doom both use to build the alist.
///
/// The variables must be DYNAMIC (`defvar`), not `let`-bound: `add-to-list`
/// resolves LIST-VAR as a symbol value, so a lexical binding raises
/// `void-variable`.  A `let`-bound version of this form captures that error
/// instead of the ordering it means to test, which is how this test was
/// wrong the first time -- see `add/to_list_semantics.rs`, which pins the
/// `void-variable` behaviour deliberately.
#[test]
fn oracle_add_to_list_prepends_by_default_and_appends_when_asked() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(progn
  (defvar mmo-default-order nil)
  (defvar mmo-append-order nil)
  (setq mmo-default-order nil)
  (setq mmo-append-order nil)
  (add-to-list 'mmo-default-order 'x)
  (add-to-list 'mmo-default-order 'y)
  (add-to-list 'mmo-append-order 'x t)
  (add-to-list 'mmo-append-order 'y t)
  (list mmo-default-order mmo-append-order))
"#;

    let expect = expect_test::expect![[r#""OK ((y x) (x y))""#]];
    assert_oracle_parity_expect(form, expect);
}

/// `define-minor-mode`'s `:lighter` registers in `minor-mode-alist` at
/// DEFINITION time, so the alist order is the LOAD order -- which is why a
/// load-order difference is visible as a reordered mode line.
#[test]
fn oracle_define_minor_mode_registers_lighter_at_definition_time() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"
(let ((minor-mode-alist nil))
  (define-minor-mode mmo-mode-a "a" :lighter " AA")
  (define-minor-mode mmo-mode-b "b" :lighter " BB")
  (list (mapcar #'car minor-mode-alist)
        (progn (mmo-mode-a 1)
               (list mmo-mode-a
                     (mapcar #'car minor-mode-alist)))))
"#;

    let expect =
        expect_test::expect![[r#""OK ((mmo-mode-b mmo-mode-a) (t (mmo-mode-b mmo-mode-a)))""#]];
    assert_oracle_parity_expect(form, expect);
}
