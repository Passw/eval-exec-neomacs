;;; neomacs-webkit.el --- WebKit browser support for Neomacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Neomacs Contributors
;; Keywords: web, browser, webkit

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides WebKit browser embedding support for Neomacs.
;; It uses WPE WebKit for headless rendering with GPU acceleration.
;;
;; Basic usage:
;;   (neomacs-webkit-browse "https://example.com")
;;
;; API functions:
;;   `neomacs-webkit-browse' - Open URL in a new browser view
;;   `neomacs-webkit-back' - Go back in history
;;   `neomacs-webkit-forward' - Go forward in history
;;   `neomacs-webkit-reload' - Reload current page
;;   `neomacs-webkit-eval-js' - Execute JavaScript

;;; Code:

(defgroup neomacs-webkit nil
  "WebKit browser embedding for Neomacs."
  :group 'web
  :prefix "neomacs-webkit-")

(defcustom neomacs-webkit-default-width 800
  "Default width for WebKit views."
  :type 'integer
  :group 'neomacs-webkit)

(defcustom neomacs-webkit-default-height 600
  "Default height for WebKit views."
  :type 'integer
  :group 'neomacs-webkit)

(defvar neomacs-webkit--views (make-hash-table :test 'eq)
  "Hash table mapping view IDs to their metadata.")

(defvar neomacs-webkit--initialized nil
  "Whether the WebKit subsystem has been initialized.")

(defun neomacs-webkit--ensure-initialized ()
  "Ensure the WebKit subsystem is initialized."
  (unless neomacs-webkit--initialized
    (when (neomacs-webkit-init)
      (setq neomacs-webkit--initialized t))))

(defun neomacs-webkit-browse (url &optional width height)
  "Open URL in a new WebKit browser view.
Optional WIDTH and HEIGHT specify view dimensions.
Returns the view ID on success, nil on failure."
  (interactive "sURL: ")
  (neomacs-webkit--ensure-initialized)
  (let* ((w (or width neomacs-webkit-default-width))
         (h (or height neomacs-webkit-default-height))
         (view-id (neomacs-webkit-create w h)))
    (when view-id
      (neomacs-webkit-load-uri view-id url)
      (puthash view-id `(:url ,url :width ,w :height ,h) neomacs-webkit--views)
      (message "WebKit view %d: %s" view-id url))
    view-id))

(defun neomacs-webkit-back (view-id)
  "Go back in history for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-go-back view-id))

(defun neomacs-webkit-forward (view-id)
  "Go forward in history for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-go-forward view-id))

(defun neomacs-webkit-eval-js (view-id script)
  "Execute JavaScript SCRIPT in VIEW-ID."
  (interactive "nView ID: \nsJavaScript: ")
  (neomacs-webkit-execute-js view-id script))

(defun neomacs-webkit-close (view-id)
  "Close WebKit view VIEW-ID."
  (interactive "nView ID: ")
  (when (neomacs-webkit-destroy view-id)
    (remhash view-id neomacs-webkit--views)
    (message "WebKit view %d closed" view-id)))

(defun neomacs-webkit-close-all ()
  "Close all WebKit views."
  (interactive)
  (maphash (lambda (id _info)
             (neomacs-webkit-destroy id))
           neomacs-webkit--views)
  (clrhash neomacs-webkit--views)
  (message "All WebKit views closed"))

(defun neomacs-webkit-show-floating (view-id x y &optional width height)
  "Show VIEW-ID as floating overlay at position (X, Y).
Optional WIDTH and HEIGHT override stored dimensions."
  (interactive "nView ID: \nnX position: \nnY position: ")
  (let* ((info (gethash view-id neomacs-webkit--views))
         (w (or width (plist-get info :width) neomacs-webkit-default-width))
         (h (or height (plist-get info :height) neomacs-webkit-default-height)))
    (neomacs-webkit-floating view-id x y w h)))

(defun neomacs-webkit-hide-floating (view-id)
  "Hide floating overlay for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-floating-clear view-id))

;;; Browser buffer mode

(defvar-local neomacs-webkit-buffer-view-id nil
  "The WebKit view ID associated with this buffer.")

(defvar neomacs-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'neomacs-webkit-mode-reload)
    (define-key map "r" #'neomacs-webkit-mode-reload)
    (define-key map "B" #'neomacs-webkit-mode-back)
    (define-key map "F" #'neomacs-webkit-mode-forward)
    (define-key map "q" #'neomacs-webkit-mode-quit)
    (define-key map "o" #'neomacs-webkit-mode-open)
    map)
  "Keymap for `neomacs-webkit-mode'.")

(define-derived-mode neomacs-webkit-mode special-mode "WebKit"
  "Major mode for browsing the web with WebKit.
\\{neomacs-webkit-mode-map}"
  (setq buffer-read-only t))

(defun neomacs-webkit-mode-reload ()
  "Reload the current page."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-reload neomacs-webkit-buffer-view-id)
    (message "Reloading...")))

(defun neomacs-webkit-mode-back ()
  "Go back in history."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-go-back neomacs-webkit-buffer-view-id)))

(defun neomacs-webkit-mode-forward ()
  "Go forward in history."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-go-forward neomacs-webkit-buffer-view-id)))

(defun neomacs-webkit-mode-quit ()
  "Close the WebKit browser buffer."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-close neomacs-webkit-buffer-view-id)
    (setq neomacs-webkit-buffer-view-id nil))
  (kill-buffer))

(defun neomacs-webkit-mode-open (url)
  "Open URL in the current WebKit view."
  (interactive "sURL: ")
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-load-uri neomacs-webkit-buffer-view-id url)
    (message "Loading: %s" url)))

;;;###autoload
(defun neomacs-webkit-open-url (url)
  "Open URL in a new WebKit browser buffer."
  (interactive "sURL: ")
  (neomacs-webkit--ensure-initialized)
  (let* ((buffer (generate-new-buffer "*WebKit*"))
         (view-id (neomacs-webkit-browse url)))
    (when view-id
      (with-current-buffer buffer
        (neomacs-webkit-mode)
        (setq neomacs-webkit-buffer-view-id view-id)
        (rename-buffer (format "*WebKit: %s*" url) t))
      (pop-to-buffer buffer)
      (message "WebKit browser opened: %s" url))
    buffer))

(provide 'neomacs-webkit)

;;; neomacs-webkit.el ends here
