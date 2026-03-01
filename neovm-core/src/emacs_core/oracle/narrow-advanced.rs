//! Oracle parity tests for advanced narrowing patterns:
//! `narrow-to-region`, `widen`, `save-restriction`, nested narrowing,
//! and buffer operations under narrowed contexts.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// narrow-to-region with point-min/point-max
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_narrow_point_min_max() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(with-temp-buffer
                    (insert "0123456789")
                    (let ((full-min (point-min))
                          (full-max (point-max)))
                      (narrow-to-region 4 8)
                      (let ((narrow-min (point-min))
                            (narrow-max (point-max))
                            (narrow-str (buffer-string)))
                        (widen)
                        (list full-min full-max
                              narrow-min narrow-max
                              narrow-str
                              (buffer-string)))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// save-restriction nesting
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_narrow_save_restriction() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(with-temp-buffer
                    (insert "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (let ((results nil))
                      (save-restriction
                        (narrow-to-region 1 11)
                        (setq results
                              (cons (buffer-string) results))
                        ;; Nested save-restriction
                        (save-restriction
                          (narrow-to-region 3 7)
                          (setq results
                                (cons (buffer-string) results)))
                        ;; After inner restore
                        (setq results
                              (cons (buffer-string) results)))
                      ;; After outer restore
                      (setq results
                            (cons (buffer-string) results))
                      (nreverse results)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Operations under narrowing
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_narrow_search_confined() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Searches are confined to narrowed region
    let form = r#"(with-temp-buffer
                    (insert "aaa XXX bbb XXX ccc XXX ddd")
                    (save-restriction
                      (narrow-to-region 9 19)
                      ;; Only see "bbb XXX cc"
                      (goto-char (point-min))
                      (let ((found (search-forward "XXX" nil t)))
                        (list (buffer-string)
                              found
                              (point)
                              ;; Search for something outside narrow
                              (progn
                                (goto-char (point-min))
                                (search-forward "aaa" nil t))))))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_narrow_insert_in_region() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Insert within narrowed region
    let form = r#"(with-temp-buffer
                    (insert "ABCDEFGH")
                    (save-restriction
                      (narrow-to-region 3 7)
                      (goto-char (point-min))
                      (insert "***")
                      (list (buffer-string)  ;; narrowed view
                            (point-min)
                            (point-max)))
                    ;; After widen: insert affected the full buffer
                    (buffer-string))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: process sections with narrowing
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_narrow_process_sections() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Process each line in a buffer by narrowing to it
    let form = r#"(with-temp-buffer
                    (insert "hello world\nfoo bar baz\ntest\n")
                    (goto-char (point-min))
                    (let ((results nil))
                      (while (not (eobp))
                        (let ((bol (line-beginning-position))
                              (eol (line-end-position)))
                          (save-restriction
                            (narrow-to-region bol eol)
                            (let ((words 0))
                              (goto-char (point-min))
                              (while (re-search-forward "\\w+" nil t)
                                (setq words (1+ words)))
                              (setq results
                                    (cons (list (buffer-string) words)
                                          results)))))
                        (forward-line 1))
                      (nreverse results)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: accumulate from narrowed regions
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_narrow_accumulate_sections() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Extract data from delimited sections
    let form = r#"(with-temp-buffer
                    (insert "--- Section A ---\ndata-a1\ndata-a2\n")
                    (insert "--- Section B ---\ndata-b1\n")
                    (insert "--- Section C ---\ndata-c1\ndata-c2\ndata-c3\n")
                    (goto-char (point-min))
                    (let ((sections nil))
                      (while (re-search-forward
                              "^--- \\(.*\\) ---$" nil t)
                        (let ((title (match-string 1))
                              (start (1+ (line-end-position))))
                          ;; Find end of section
                          (let ((end (if (re-search-forward
                                         "^--- " nil t)
                                        (progn (goto-char
                                                (match-beginning 0))
                                               (line-beginning-position))
                                      (point-max))))
                            (save-restriction
                              (narrow-to-region start end)
                              (goto-char (point-min))
                              (let ((lines nil))
                                (while (not (eobp))
                                  (let ((line (buffer-substring
                                               (line-beginning-position)
                                               (line-end-position))))
                                    (unless (string= line "")
                                      (setq lines (cons line lines))))
                                  (forward-line 1))
                                (setq sections
                                      (cons (list title (nreverse lines))
                                            sections)))))))
                      (nreverse sections)))"#;
    assert_oracle_parity(form);
}
