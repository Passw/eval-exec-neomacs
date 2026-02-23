;;; oracle_eval.el --- GNU Emacs oracle evaluator for NeoVM compatibility -*- lexical-binding: nil; -*-

(let ((forms-file (getenv "NEOVM_FORMS_FILE")))
  (unless (and forms-file (file-readable-p forms-file))
    (princ "ERR (:invalid-input \"NEOVM_FORMS_FILE missing or unreadable\")")
    (terpri)
    (kill-emacs 2))

  (with-temp-buffer
    (insert-file-contents forms-file)
    (goto-char (point-min))

    (defun escape-case-string (string)
      "Escape newline, carriage return, and tab in STRING for vm-compat output."
      (let ((escaped string))
        (setq escaped (replace-regexp-in-string "\n" "\\\\n" escaped nil t))
        (setq escaped (replace-regexp-in-string "\r" "\\\\r" escaped nil t))
        (setq escaped (replace-regexp-in-string "\t" "\\\\t" escaped nil t))
        escaped))

    (let ((index 0)
          (case-prefix "__NEOVM_CASE__\t")
          form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (setq index (1+ index))
            (let* ((rendered-form (prin1-to-string form))
                  (status
                   (condition-case err
                       (let ((value (eval form nil)))
                         (concat "OK " (prin1-to-string value)))
                      (error
                       (concat "ERR " (prin1-to-string (list (car err) (cdr err))))))))
              (princ case-prefix)
              (princ (number-to-string index))
              (princ "\t")
              (princ (escape-case-string rendered-form))
              (princ "\t")
              (princ (escape-case-string status)))
            (terpri))
        (end-of-file nil)))))
