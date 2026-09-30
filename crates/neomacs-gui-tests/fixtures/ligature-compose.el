;;; ligature-compose.el --- Issue #447 GUI verification -*- lexical-binding: t -*-
;; Renders "->" with a composition-function-table rule (font-shape-gstring)
;; and writes the frame snapshot + surface readback for pixel comparison.
(run-at-time
 1 nil
 (lambda ()
   (switch-to-buffer (get-buffer-create "*ligature*"))
   (setq-local mode-line-format nil cursor-type nil)
   (set-face-attribute 'default nil :font
                       (font-spec :family "DejaVu Sans Mono" :size 30))
   (setq auto-composition-mode t
         auto-composition-function 'auto-compose-chars
         composition-function-table (make-char-table nil))
   (aset composition-function-table ?-
         '([("\\(?:->\\)" 0 font-shape-gstring)]))
   (let ((enable (getenv "NEOMACS_LIGATURE_ENABLED")))
     (if (equal enable "0")
         (setq composition-function-table (make-char-table nil))))
   (insert (propertize "a -> b" 'face '(:foreground "black" :background "white")))
   (goto-char (point-min))
   (local-set-key
    (kbd "C-c t")
    (lambda ()
      (interactive)
      (redisplay t)
      (neomacs--write-frame-snapshot (getenv "NEOMACS_GUI_FRAME_SNAPSHOT_JSON") nil 'json)
      (run-at-time 1 nil
       (lambda ()
         (copy-file (getenv "NEOMACS_DEBUG_SURFACE_READBACK_PNG")
                    (concat (getenv "NEOMACS_GUI_FRAME_SNAPSHOT_JSON") ".png") t)
         (kill-emacs 0)))))
   (with-temp-file (getenv "NEOMACS_SELECTION_READY") (insert "ready"))))
