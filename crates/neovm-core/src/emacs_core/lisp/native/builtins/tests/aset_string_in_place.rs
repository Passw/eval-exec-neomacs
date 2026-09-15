//! `aset` on a string overwrites one byte in place, without the heap write
//! barrier and without recounting characters. What Lisp observes must not
//! change: contents, length, byte length, multibyteness, text properties,
//! the `copy-sequence` it was made from, and the errors — with and without
//! GC stress (the barrier is gone, so a collection between writes is the
//! case to watch).

use crate::emacs_core::{Context, format_eval_result};

#[test]
fn aset_on_strings_is_observably_unchanged() {
    crate::test_utils::init_test_tracing();
    let cases: &[(&str, &str)] = &[
        // Multibyte ASCII string (a literal is multibyte only if non-ASCII;
        // `aéc` is), unibyte strings, a propertized string, and the source
        // of a copy.
        (
            "(let ((s (copy-sequence \"aéc\"))) (aset s 2 ?x) (list s (length s) (string-bytes s) (multibyte-string-p s)))",
            "(\"aéx\" 3 4 t)",
        ),
        (
            "(let ((s (copy-sequence \"abc\"))) (aset s 1 ?z) (list s (length s) (string-bytes s) (multibyte-string-p s)))",
            "(\"azc\" 3 3 nil)",
        ),
        (
            "(let ((s (string-to-unibyte \"abc\"))) (aset s 1 200) (list s (length s) (string-bytes s)))",
            "(\"a\\310c\" 3 3)",
        ),
        (
            "(let* ((o \"hello\") (s (copy-sequence o))) (aset s 0 ?J) (list o s))",
            "(\"hello\" \"Jello\")",
        ),
        (
            "(let ((s (propertize (copy-sequence \"abcd\") 'face 'bold))) (aset s 2 ?Z) (list s (get-text-property 2 'face s)))",
            "(#(\"abZd\" 0 4 (face bold)) bold)",
        ),
        (
            "(let ((s (make-string 1000 ?a)) (i 0)) (while (< i 1000) (aset s i (+ ?a (% i 26))) (setq i (1+ i))) (list (substring s 0 30) (length s) (string-bytes s)))",
            "(\"abcdefghijklmnopqrstuvwxyzabcd\" 1000 1000)",
        ),
        (
            "(condition-case err (let ((s (copy-sequence \"aéc\"))) (aset s 1 ?x)) (error err))",
            "(error \"Attempt to replace non-ASCII char in multibyte string\")",
        ),
        (
            "(condition-case err (let ((s (copy-sequence \"abc\"))) (aset s 3 ?x)) (error err))",
            "(args-out-of-range \"abc\" 3)",
        ),
    ];
    for gc_stress in [false, true] {
        let mut ev = Context::new();
        ev.gc_stress = gc_stress;
        for (form, want) in cases {
            let got = format_eval_result(&ev.eval_str(form));
            assert_eq!(got, format!("OK {want}"), "{form} (gc_stress {gc_stress})");
        }
    }
}
