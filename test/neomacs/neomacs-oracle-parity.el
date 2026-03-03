;;; neomacs-oracle-parity.el --- Visual parity oracle scenarios -*- lexical-binding: t -*-

;; Usage:
;;   NEOMACS_ORACLE_SCENARIO=hello NEOMACS_ORACLE_LABEL=neomacs \
;;     ./src/emacs -Q -l test/neomacs/neomacs-oracle-parity.el

;;; Code:

(defconst neomacs-oracle-scenario
  (or (getenv "NEOMACS_ORACLE_SCENARIO") "hello"))

(defconst neomacs-oracle-label
  (or (getenv "NEOMACS_ORACLE_LABEL") "neomacs"))

(defun neomacs-oracle--set-font ()
  "Set a deterministic default font when available."
  (let ((candidates '("Noto Sans Mono-16"
                      "DejaVu Sans Mono-16"
                      "Monospace-16")))
    (catch 'done
      (dolist (font candidates)
        (condition-case nil
            (progn
              (set-frame-font font t t)
              (throw 'done t))
          (error nil))))))

(defun neomacs-oracle--setup-common ()
  "Normalize frame state for screenshot comparison."
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (setq cursor-type 'box)
  (set-frame-size (selected-frame) 132 42)
  (neomacs-oracle--set-font)
  (setq frame-title-format
        (format "neomacs-oracle-%s-%s"
                neomacs-oracle-scenario
                neomacs-oracle-label)))

(defun neomacs-oracle--scenario-hello ()
  "Open view-hello-file for LANGUAGE/HELLO column alignment checks."
  (view-hello-file)
  (goto-char (point-min))
  (recenter 0))

(defun neomacs-oracle--scenario-cjk (index)
  "Create CJK cursor geometry scenario with cursor INDEX (1 or 2)."
  (let ((buf (get-buffer-create "*Neomacs Oracle CJK*")))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "a好好b\n")
    (insert "a好好b\n")
    (insert "\nCursor oracle: compare first/second 好 begin/end.\n")
    (goto-char (point-min))
    (search-forward "a")
    (forward-char (if (> index 1) 1 0))
    (setq buffer-read-only t)))

(defun neomacs-oracle--scenario-weights ()
  "Create Noto/Hack bold-vs-extra-bold scenario."
  (let ((buf (get-buffer-create "*Neomacs Oracle Weights*")))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Noto Sans Mono + Hack weight parity\n\n")
    (insert "Noto h=1.6 w=bold:       ")
    (let ((s (point)))
      (insert "a好好b  ABCXYZ 0123456789  -> <= >=")
      (put-text-property
       s (point) 'face '(:family "Noto Sans Mono" :height 1.6 :weight bold)))
    (insert "\n")
    (insert "Noto h=1.6 w=extra-bold: ")
    (let ((s (point)))
      (insert "a好好b  ABCXYZ 0123456789  -> <= >=")
      (put-text-property
       s (point) 'face '(:family "Noto Sans Mono" :height 1.6 :weight extra-bold)))
    (insert "\n\n")
    (insert "Hack h=1.6 w=bold:       ")
    (let ((s (point)))
      (insert "a好好b  ABCXYZ 0123456789  -> <= >=")
      (put-text-property
       s (point) 'face '(:family "Hack" :height 1.6 :weight bold)))
    (insert "\n")
    (insert "Hack h=1.6 w=extra-bold: ")
    (let ((s (point)))
      (insert "a好好b  ABCXYZ 0123456789  -> <= >=")
      (put-text-property
       s (point) 'face '(:family "Hack" :height 1.6 :weight extra-bold)))
    (insert "\n")
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun neomacs-oracle--run ()
  "Dispatch parity scenario from environment."
  (neomacs-oracle--setup-common)
  (pcase neomacs-oracle-scenario
    ("hello" (neomacs-oracle--scenario-hello))
    ("weights" (neomacs-oracle--scenario-weights))
    ("cjk1" (neomacs-oracle--scenario-cjk 1))
    ("cjk2" (neomacs-oracle--scenario-cjk 2))
    (_ (neomacs-oracle--scenario-weights)))
  (redisplay t)
  ;; Safety timeout for automation harness.
  (run-at-time 180 nil (lambda () (kill-emacs 0))))

(add-hook 'emacs-startup-hook #'neomacs-oracle--run)

;;; neomacs-oracle-parity.el ends here
