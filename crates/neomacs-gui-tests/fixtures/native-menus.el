;;; native-menus.el --- Observe real mouse-driven menus -*- lexical-binding: t -*-

;; A deterministic native submenu, independent of installed packages and the
;; default File menu's row ordering. Only the harness supplies mouse input.
(require 'json)
(require 'cl-lib)
(let ((menu (make-sparse-keymap "File"))
      (child (make-sparse-keymap "Submenu")))
  (define-key child [noop] '(menu-item "No action" ignore))
  (define-key menu [child] (list 'menu-item "Submenu" child))
  (define-key global-map [menu-bar file] (list 'menu-item "File" menu)))
(defvar neomacs-menu-directory (getenv "NEOMACS_GUI_MENU_CONTROL"))
(defvar neomacs-menu-sample 0)
(menu-bar-mode 1)
(modify-frame-parameters nil '((title . "NEOMACS-MENU-REPRO")))

(defun neomacs-menu-observe ()
  (let ((state `((pid . ,(emacs-pid))
                 (sample . ,(cl-incf neomacs-menu-sample))
                 (char-width . ,(frame-char-width))
                 (char-height . ,(frame-char-height)))))
    (with-temp-file (expand-file-name "state.pending" neomacs-menu-directory)
      (insert (json-encode state)))
    (rename-file (expand-file-name "state.pending" neomacs-menu-directory)
                 (expand-file-name "state.json" neomacs-menu-directory) t))
  (when (file-exists-p (expand-file-name "stop" neomacs-menu-directory))
    (kill-emacs 0)))

(run-at-time 0.2 0.05 #'neomacs-menu-observe)
