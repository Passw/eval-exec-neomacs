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
  "Default width for WebKit views.
If set to 0 or nil, auto-calculate from window width."
  :type '(choice integer (const nil))
  :group 'neomacs-webkit)

(defcustom neomacs-webkit-default-height 600
  "Default height for WebKit views.
If set to 0 or nil, auto-calculate from width using aspect ratio."
  :type '(choice integer (const nil))
  :group 'neomacs-webkit)

(defcustom neomacs-webkit-default-aspect-ratio (/ 16.0 9.0)
  "Default aspect ratio (width/height) for auto-calculated heights."
  :type 'float
  :group 'neomacs-webkit)

(defcustom neomacs-webkit-auto-resize t
  "If non-nil, automatically resize webkit views when window size changes."
  :type 'boolean
  :group 'neomacs-webkit)

(defvar neomacs-webkit--views (make-hash-table :test 'eq)
  "Hash table mapping view IDs to their metadata.")

(defvar-local neomacs-webkit--buffer-views nil
  "List of (POSITION . VIEW-ID) for webkit views in this buffer.
Used for responsive auto-resize on window size changes.")

(defvar neomacs-webkit--initialized nil
  "Whether the WebKit subsystem has been initialized.")

(defcustom neomacs-webkit-new-window-action 'new-buffer
  "Action to take when WebKit requests a new window.
Possible values:
  `new-buffer' - Open URL in a new webkit buffer (default)
  `same-buffer' - Load URL in the current webkit view
  `external' - Open URL in external browser
  `ignore' - Ignore the request
  `ask' - Ask user what to do"
  :type '(choice (const :tag "Open in new buffer" new-buffer)
                 (const :tag "Load in same view" same-buffer)
                 (const :tag "Open in external browser" external)
                 (const :tag "Ignore" ignore)
                 (const :tag "Ask user" ask))
  :group 'neomacs-webkit)

(defun neomacs-webkit--handle-new-window (view-id url frame-name)
  "Handle a new window request from WebKit VIEW-ID.
URL is the requested URL, FRAME-NAME is the target attribute (e.g., \"_blank\")."
  (message "WebKit new window request: view=%d url=%s frame=%s"
           view-id url frame-name)
  (pcase neomacs-webkit-new-window-action
    ('new-buffer
     ;; Open URL in a new webkit buffer
     (let ((new-view-id (neomacs-webkit-browse url)))
       (when new-view-id
         (message "Opened %s in new webkit view %d" url new-view-id)))
     t)
    ('same-buffer
     ;; Load URL in the same view
     (neomacs-webkit-load-uri view-id url)
     (message "Loading %s in view %d" url view-id)
     t)
    ('external
     ;; Open in external browser
     (browse-url url)
     (message "Opened %s in external browser" url)
     t)
    ('ignore
     ;; Ignore the request
     (message "Ignored new window request for %s" url)
     nil)
    ('ask
     ;; Ask user what to do
     (let ((choice (completing-read
                    (format "New window request for %s: " url)
                    '("Open in new buffer" "Load in same view"
                      "Open in external browser" "Ignore")
                    nil t)))
       (pcase choice
         ("Open in new buffer"
          (neomacs-webkit-browse url)
          t)
         ("Load in same view"
          (neomacs-webkit-load-uri view-id url)
          t)
         ("Open in external browser"
          (browse-url url)
          t)
         (_
          nil))))
    (_
     ;; Default: open in new buffer
     (neomacs-webkit-browse url)
     t)))

(defun neomacs-webkit--ensure-initialized ()
  "Ensure the WebKit subsystem is initialized."
  (unless neomacs-webkit--initialized
    (when (neomacs-webkit-init)
      ;; Set up the new window callback
      (neomacs-webkit-set-new-window-function #'neomacs-webkit--handle-new-window)
      (setq neomacs-webkit--initialized t))))

(defun neomacs-webkit--calculate-dimensions (width height)
  "Calculate actual dimensions from WIDTH and HEIGHT.
If WIDTH is 0 or nil, use window body width minus margin.
If HEIGHT is 0 or nil, calculate from width using aspect ratio.
Returns (WIDTH . HEIGHT) cons cell."
  (let* ((margin 16)
         (w (if (and width (> width 0))
                width
              (- (window-body-width nil t) margin)))
         (h (if (and height (> height 0))
                height
              (round (/ w neomacs-webkit-default-aspect-ratio)))))
    (cons w h)))

(defun neomacs-webkit-browse (url &optional width height)
  "Open URL in a new WebKit browser view.
Optional WIDTH and HEIGHT specify view dimensions.
If WIDTH is 0 or nil, auto-calculate from window width.
If HEIGHT is 0 or nil, auto-calculate using aspect ratio.
Returns the view ID on success, nil on failure."
  (interactive "sURL: ")
  (neomacs-webkit--ensure-initialized)
  (let* ((dims (neomacs-webkit--calculate-dimensions
                (or width neomacs-webkit-default-width)
                (or height neomacs-webkit-default-height)))
         (w (car dims))
         (h (cdr dims))
         (view-id (neomacs-webkit-create w h)))
    (when view-id
      (neomacs-webkit-load-uri view-id url)
      (puthash view-id `(:url ,url :width ,w :height ,h) neomacs-webkit--views)
      (message "WebKit view %d: %s (%dx%d)" view-id url w h))
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

(defun neomacs-webkit-resize-view (view-id width height)
  "Resize WebKit view VIEW-ID to WIDTH x HEIGHT pixels.
Returns t on success, nil on failure."
  (interactive "nView ID: \nnWidth: \nnHeight: ")
  (when (neomacs-webkit-resize view-id width height)
    ;; Update stored dimensions
    (let ((info (gethash view-id neomacs-webkit--views)))
      (when info
        (plist-put info :width width)
        (plist-put info :height height)))
    t))

(defun neomacs-webkit--register-view (pos view-id)
  "Register webkit VIEW-ID at buffer position POS for auto-resize tracking."
  (unless neomacs-webkit--buffer-views
    (setq neomacs-webkit--buffer-views nil))
  (push (cons pos view-id) neomacs-webkit--buffer-views))

(defun neomacs-webkit--update-display-spec (pos view-id new-width new-height)
  "Update the display spec at POS for VIEW-ID with new dimensions."
  (save-excursion
    (goto-char pos)
    (let ((spec (get-text-property pos 'display)))
      (when (and spec (eq (car spec) 'webkit))
        ;; Update the spec with new dimensions
        (let ((new-spec (list 'webkit
                              :id view-id
                              :width new-width
                              :height new-height)))
          (put-text-property pos (1+ pos) 'display new-spec))))))

(defun neomacs-webkit--auto-resize-buffer (window)
  "Resize all webkit views in WINDOW's buffer to fit the new window width."
  (when neomacs-webkit-auto-resize
    (with-current-buffer (window-buffer window)
      (when neomacs-webkit--buffer-views
        (let* ((margin 16)
               (new-width (- (window-body-width window t) margin))
               (new-height (round (/ new-width neomacs-webkit-default-aspect-ratio))))
          (dolist (entry neomacs-webkit--buffer-views)
            (let ((pos (car entry))
                  (view-id (cdr entry)))
              ;; Resize the webkit view
              (when (neomacs-webkit-resize view-id new-width new-height)
                ;; Update the display spec in the buffer
                (neomacs-webkit--update-display-spec pos view-id new-width new-height)
                ;; Update stored metadata
                (let ((info (gethash view-id neomacs-webkit--views)))
                  (when info
                    (plist-put info :width new-width)
                    (plist-put info :height new-height)))))))))))

(defun neomacs-webkit--window-size-change-hook (frame)
  "Hook function for `window-size-change-functions'.
Resizes webkit views in all windows of FRAME."
  (dolist (window (window-list frame))
    (neomacs-webkit--auto-resize-buffer window)))

(defun neomacs-webkit-enable-auto-resize ()
  "Enable automatic webkit view resizing on window size changes."
  (interactive)
  (add-hook 'window-size-change-functions #'neomacs-webkit--window-size-change-hook)
  (setq neomacs-webkit-auto-resize t)
  (message "WebKit auto-resize enabled"))

(defun neomacs-webkit-disable-auto-resize ()
  "Disable automatic webkit view resizing on window size changes."
  (interactive)
  (remove-hook 'window-size-change-functions #'neomacs-webkit--window-size-change-hook)
  (setq neomacs-webkit-auto-resize nil)
  (message "WebKit auto-resize disabled"))

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
    (define-key map "i" #'neomacs-webkit-mode-toggle-input)  ; Enter input mode
    (define-key map [mouse-1] #'neomacs-webkit-mode-mouse-click)
    (define-key map [wheel-up] #'neomacs-webkit-mode-scroll-up)
    (define-key map [wheel-down] #'neomacs-webkit-mode-scroll-down)
    map)
  "Keymap for `neomacs-webkit-mode'.
Press 'i' to enter input mode for typing in web forms.")

(defvar neomacs-webkit-mode-line-format
  '(:eval (neomacs-webkit--mode-line-string))
  "Mode line format for WebKit buffers.")

(defun neomacs-webkit--mode-line-string ()
  "Generate mode line string for WebKit buffer."
  (if neomacs-webkit-buffer-view-id
      (let ((title (neomacs-webkit-get-title neomacs-webkit-buffer-view-id))
            (progress (neomacs-webkit-get-progress neomacs-webkit-buffer-view-id))
            (loading (neomacs-webkit-loading-p neomacs-webkit-buffer-view-id)))
        (format " [%s%s]"
                (or title "WebKit")
                (if loading
                    (format " %.0f%%" (* 100 (or progress 0)))
                  "")))
    " [WebKit]"))

(define-derived-mode neomacs-webkit-mode special-mode "WebKit"
  "Major mode for browsing the web with WebKit.
\\{neomacs-webkit-mode-map}"
  (setq buffer-read-only t)
  (setq mode-line-format
        (list "%e" mode-line-front-space
              neomacs-webkit-mode-line-format
              " " mode-line-buffer-identification
              mode-line-end-spaces)))

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
    (neomacs-webkit-floating-clear neomacs-webkit-buffer-view-id)
    (neomacs-webkit-close neomacs-webkit-buffer-view-id)
    (setq neomacs-webkit-buffer-view-id nil))
  (kill-buffer))

(defun neomacs-webkit-mode-open (url)
  "Open URL in the current WebKit view."
  (interactive "sURL: ")
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-load-uri neomacs-webkit-buffer-view-id url)
    (message "Loading: %s" url)))

(defvar-local neomacs-webkit--floating-position nil
  "Position and size of floating WebKit view (x y width height).")

(defun neomacs-webkit-mode-mouse-click (event)
  "Handle mouse click EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (click-x (car pos))
           (click-y (cdr pos))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           ;; Translate to WebKit view coordinates
           (view-x (- click-x float-x))
           (view-y (- click-y float-y)))
      (neomacs-webkit-click neomacs-webkit-buffer-view-id view-x view-y 1))))

(defun neomacs-webkit-mode-scroll-up (event)
  "Handle scroll up EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           (view-x (- (car pos) float-x))
           (view-y (- (cdr pos) float-y)))
      (neomacs-webkit-send-scroll neomacs-webkit-buffer-view-id view-x view-y 0 -50))))

(defun neomacs-webkit-mode-scroll-down (event)
  "Handle scroll down EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           (view-x (- (car pos) float-x))
           (view-y (- (cdr pos) float-y)))
      (neomacs-webkit-send-scroll neomacs-webkit-buffer-view-id view-x view-y 0 50))))

;; Keyboard input support for text entry in web forms

(defvar neomacs-webkit--input-mode nil
  "Non-nil when input mode is active (for typing in web forms).")

(defun neomacs-webkit-mode-toggle-input ()
  "Toggle input mode for typing in web forms.
When enabled, most keys are forwarded to the WebKit view."
  (interactive)
  (setq neomacs-webkit--input-mode (not neomacs-webkit--input-mode))
  (if neomacs-webkit--input-mode
      (progn
        (message "WebKit input mode ON - type to enter text, C-c C-c to exit")
        (neomacs-webkit--enable-input-mode))
    (message "WebKit input mode OFF")
    (neomacs-webkit--disable-input-mode)))

(defun neomacs-webkit--enable-input-mode ()
  "Enable input mode keybindings."
  ;; Use a minor mode map that captures most keys
  (setq-local neomacs-webkit--saved-keymap (current-local-map))
  (use-local-map neomacs-webkit-input-mode-map))

(defun neomacs-webkit--disable-input-mode ()
  "Disable input mode keybindings."
  (when neomacs-webkit--saved-keymap
    (use-local-map neomacs-webkit--saved-keymap)
    (setq-local neomacs-webkit--saved-keymap nil)))

(defvar neomacs-webkit--saved-keymap nil
  "Saved keymap before entering input mode.")

(defun neomacs-webkit--char-to-keysym (char)
  "Convert CHAR to XKB keysym."
  (cond
   ;; Letters
   ((and (>= char ?a) (<= char ?z)) char)
   ((and (>= char ?A) (<= char ?Z)) char)
   ;; Numbers
   ((and (>= char ?0) (<= char ?9)) char)
   ;; Space
   ((= char ?\s) #x0020)
   ;; Common punctuation
   ((= char ?.) #x002e)
   ((= char ?,) #x002c)
   ((= char ?!) #x0021)
   ((= char ??) #x003f)
   ((= char ?:) #x003a)
   ((= char ?\;) #x003b)
   ((= char ?') #x0027)
   ((= char ?\") #x0022)
   ((= char ?-) #x002d)
   ((= char ?_) #x005f)
   ((= char ?=) #x003d)
   ((= char ?+) #x002b)
   ((= char ?/) #x002f)
   ((= char ?\\) #x005c)
   ((= char ?@) #x0040)
   ((= char ?#) #x0023)
   ((= char ?$) #x0024)
   ((= char ?%) #x0025)
   ((= char ?^) #x005e)
   ((= char ?&) #x0026)
   ((= char ?*) #x002a)
   ((= char ?\() #x0028)
   ((= char ?\)) #x0029)
   ((= char ?\[) #x005b)
   ((= char ?\]) #x005d)
   ((= char ?{) #x007b)
   ((= char ?}) #x007d)
   ((= char ?<) #x003c)
   ((= char ?>) #x003e)
   ((= char ?|) #x007c)
   ((= char ?`) #x0060)
   ((= char ?~) #x007e)
   ;; Default: use char code as keysym
   (t char)))

(defun neomacs-webkit-input-self-insert ()
  "Forward current key to WebKit as text input."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (let* ((char last-command-event)
           (keysym (neomacs-webkit--char-to-keysym char))
           (modifiers 0))
      ;; Send key down then key up
      (neomacs-webkit-send-key neomacs-webkit-buffer-view-id keysym 0 t modifiers)
      (neomacs-webkit-send-key neomacs-webkit-buffer-view-id keysym 0 nil modifiers))))

(defun neomacs-webkit-input-return ()
  "Send Return/Enter to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff0d 36 t 0)  ; XK_Return
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff0d 36 nil 0)))

(defun neomacs-webkit-input-backspace ()
  "Send Backspace to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff08 22 t 0)  ; XK_BackSpace
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff08 22 nil 0)))

(defun neomacs-webkit-input-tab ()
  "Send Tab to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff09 23 t 0)  ; XK_Tab
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff09 23 nil 0)))

(defun neomacs-webkit-input-escape ()
  "Send Escape to WebKit and exit input mode."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff1b 9 t 0)  ; XK_Escape
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff1b 9 nil 0))
  (neomacs-webkit-mode-toggle-input))

(defun neomacs-webkit-input-arrow-up ()
  "Send Up arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff52 111 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff52 111 nil 0)))

(defun neomacs-webkit-input-arrow-down ()
  "Send Down arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff54 116 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff54 116 nil 0)))

(defun neomacs-webkit-input-arrow-left ()
  "Send Left arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff51 113 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff51 113 nil 0)))

(defun neomacs-webkit-input-arrow-right ()
  "Send Right arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff53 114 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff53 114 nil 0)))

(defvar neomacs-webkit-input-mode-map
  (let ((map (make-keymap)))
    ;; Self-insert for printable characters
    (dotimes (i 128)
      (when (and (>= i 32) (/= i 127))  ; printable ASCII
        (define-key map (vector i) #'neomacs-webkit-input-self-insert)))
    ;; Special keys
    (define-key map (kbd "RET") #'neomacs-webkit-input-return)
    (define-key map (kbd "DEL") #'neomacs-webkit-input-backspace)
    (define-key map (kbd "<backspace>") #'neomacs-webkit-input-backspace)
    (define-key map (kbd "TAB") #'neomacs-webkit-input-tab)
    (define-key map (kbd "<up>") #'neomacs-webkit-input-arrow-up)
    (define-key map (kbd "<down>") #'neomacs-webkit-input-arrow-down)
    (define-key map (kbd "<left>") #'neomacs-webkit-input-arrow-left)
    (define-key map (kbd "<right>") #'neomacs-webkit-input-arrow-right)
    ;; Exit input mode
    (define-key map (kbd "ESC") #'neomacs-webkit-input-escape)
    (define-key map (kbd "C-c C-c") #'neomacs-webkit-mode-toggle-input)
    ;; Keep mouse bindings
    (define-key map [mouse-1] #'neomacs-webkit-mode-mouse-click)
    (define-key map [wheel-up] #'neomacs-webkit-mode-scroll-up)
    (define-key map [wheel-down] #'neomacs-webkit-mode-scroll-down)
    map)
  "Keymap for WebKit input mode (typing in web forms).")

(defun neomacs-webkit-mode-show-fullscreen ()
  "Show WebKit view as fullscreen floating overlay."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (let* ((frame (selected-frame))
           (width (frame-pixel-width frame))
           (height (frame-pixel-height frame)))
      (setq neomacs-webkit--floating-position (list 0 0 width height))
      (neomacs-webkit-floating neomacs-webkit-buffer-view-id 0 0 width height))))

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

;;; Cursor-aware inline WebKit input mode
;;
;; Keyboard input only reaches an inline WebKit view when:
;; 1. The cursor is on the WebKit character (has `neomacs-webkit-id' text prop)
;; 2. Input mode is explicitly toggled ON for that view
;;
;; Moving the cursor off the WebKit character auto-deactivates input mode.
;; Clicking on a WebKit view (C sets `neomacs-webkit-clicked-view-id') also
;; enters input mode automatically.

(defvar neomacs-webkit-debug nil
  "When non-nil, log WebKit input routing events to *Messages*.")

(defun neomacs-webkit--log (fmt &rest args)
  "Log FMT with ARGS if `neomacs-webkit-debug' is non-nil."
  (when neomacs-webkit-debug
    (apply #'message (concat "[webkit-input] " fmt) args)))

(defvar-local neomacs-webkit--input-view-id nil
  "View ID of the inline WebKit view currently accepting keyboard input, or nil.")

;;; Hover keymap — active when cursor is on a WebKit character but input is OFF

(defvar neomacs-webkit-hover-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'neomacs-webkit-enter-input-mode)
    (define-key map (kbd "RET") #'neomacs-webkit-enter-input-mode)
    map)
  "Keymap active when cursor is on a WebKit character but input mode is off.
Press `i' or RET to enter input mode for this view.")

;;; neomacs-webkit-insert — Lisp wrapper that adds text properties

(defun neomacs-webkit-insert (url width height &optional load-p)
  "Insert inline WebKit view at point.  Returns view-id or nil.
Calls the C function `neomacs-insert-webkit' to create the view,
then inserts a propertized character with `neomacs-webkit-id' and
`keymap' text properties for cursor-aware input routing.

URL, WIDTH, HEIGHT, and LOAD-P are passed to `neomacs-insert-webkit'."
  (let ((spec (neomacs-insert-webkit url width height load-p)))
    (when spec
      (let ((view-id (plist-get (cdr spec) :id)))
        (insert (propertize " "
                  'display spec
                  'neomacs-webkit-id view-id
                  'keymap neomacs-webkit-hover-keymap))
        view-id))))

;;; Interaction mode keymap — active when input mode is ON

(defvar neomacs-webkit-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Self-insert for printable ASCII
    (dotimes (i 128)
      (when (and (>= i 32) (/= i 127))
        (define-key map (vector i) #'neomacs-webkit-interaction-self-insert)))
    ;; Special keys
    (define-key map (kbd "RET") #'neomacs-webkit-interaction-return)
    (define-key map (kbd "DEL") #'neomacs-webkit-interaction-backspace)
    (define-key map (kbd "<backspace>") #'neomacs-webkit-interaction-backspace)
    (define-key map (kbd "TAB") #'neomacs-webkit-interaction-tab)
    (define-key map (kbd "<up>") #'neomacs-webkit-interaction-arrow-up)
    (define-key map (kbd "<down>") #'neomacs-webkit-interaction-arrow-down)
    (define-key map (kbd "<left>") #'neomacs-webkit-interaction-arrow-left)
    (define-key map (kbd "<right>") #'neomacs-webkit-interaction-arrow-right)
    ;; Exit
    (define-key map (kbd "C-g") #'neomacs-webkit-interaction-quit)
    (define-key map (kbd "ESC") #'neomacs-webkit-interaction-quit)
    (define-key map (kbd "C-c C-c") #'neomacs-webkit-interaction-quit)
    map)
  "Keymap active when typing into an inline WebKit view.")

;;; Minor mode

(define-minor-mode neomacs-webkit-interaction-mode
  "Minor mode for keyboard input to inline WebKit views.
Activated when entering input mode on a WebKit character.
Press ESC, C-g, or C-c C-c to exit."
  :lighter " WebKit-Input"
  (if neomacs-webkit-interaction-mode
      (progn
        (neomacs-webkit--log "input ON for view %s" neomacs-webkit--input-view-id)
        (message "WebKit input active (view %s) — ESC to exit"
                 neomacs-webkit--input-view-id))
    (neomacs-webkit--log "input OFF (was view %s)" neomacs-webkit--input-view-id)
    (setq neomacs-webkit--input-view-id nil)
    (message "WebKit input off")))

;;; Input activation / deactivation

(defun neomacs-webkit--activate-input (view-id)
  "Activate input mode for VIEW-ID."
  (setq neomacs-webkit--input-view-id view-id)
  (neomacs-webkit-interaction-mode 1))

(defun neomacs-webkit--deactivate-input ()
  "Deactivate input mode."
  (neomacs-webkit-interaction-mode -1))

(defun neomacs-webkit-enter-input-mode ()
  "Enter input mode for the WebKit view at point.
Reads the `neomacs-webkit-id' text property to determine which view."
  (interactive)
  (let ((view-id (get-text-property (point) 'neomacs-webkit-id)))
    (if view-id
        (neomacs-webkit--activate-input view-id)
      (neomacs-webkit--log "enter-input-mode: no webkit-id at point")
      (message "No WebKit view at point"))))

;;; Key forwarding commands

(defun neomacs-webkit-interaction-self-insert ()
  "Forward character to the focused inline WebKit view."
  (interactive)
  (when neomacs-webkit--input-view-id
    (let* ((char last-command-event)
           (keysym (neomacs-webkit--char-to-keysym char)))
      (neomacs-webkit-send-key neomacs-webkit--input-view-id keysym 0 t 0)
      (neomacs-webkit-send-key neomacs-webkit--input-view-id keysym 0 nil 0))))

(defun neomacs-webkit-interaction--send (keysym keycode)
  "Send KEYSYM with KEYCODE to focused webkit view."
  (when neomacs-webkit--input-view-id
    (neomacs-webkit-send-key neomacs-webkit--input-view-id keysym keycode t 0)
    (neomacs-webkit-send-key neomacs-webkit--input-view-id keysym keycode nil 0)))

(defun neomacs-webkit-interaction-return ()
  "Send Return to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff0d 36))

(defun neomacs-webkit-interaction-backspace ()
  "Send Backspace to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff08 22))

(defun neomacs-webkit-interaction-tab ()
  "Send Tab to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff09 23))

(defun neomacs-webkit-interaction-arrow-up ()
  "Send Up to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff52 111))

(defun neomacs-webkit-interaction-arrow-down ()
  "Send Down to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff54 116))

(defun neomacs-webkit-interaction-arrow-left ()
  "Send Left to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff51 113))

(defun neomacs-webkit-interaction-arrow-right ()
  "Send Right to WebKit." (interactive)
  (neomacs-webkit-interaction--send #xff53 114))

(defun neomacs-webkit-interaction-quit ()
  "Exit WebKit interaction mode."
  (interactive)
  (neomacs-webkit--deactivate-input))

;;; Post-command hook — click-to-activate and auto-deactivate

(defun neomacs-webkit--post-command-check ()
  "Post-command hook for WebKit input routing.
Handles two concerns:
1. Click-to-activate: when C sets `neomacs-webkit-clicked-view-id',
   auto-enter input mode if point is on that view.
2. Auto-deactivate: if input mode is active but cursor has moved
   off the view, deactivate."
  ;; 1. Click-to-activate
  (when (and (boundp 'neomacs-webkit-clicked-view-id)
             neomacs-webkit-clicked-view-id
             (integerp neomacs-webkit-clicked-view-id)
             (> neomacs-webkit-clicked-view-id 0))
    (let ((clicked-id neomacs-webkit-clicked-view-id))
      (setq neomacs-webkit-clicked-view-id nil)
      (neomacs-webkit--log "click detected: view %d" clicked-id)
      ;; Only activate if point is on that view's character
      (when (eq (get-text-property (point) 'neomacs-webkit-id) clicked-id)
        (neomacs-webkit--activate-input clicked-id))))
  ;; 2. Auto-deactivate: cursor left the input-mode view
  (when neomacs-webkit--input-view-id
    (unless (eq (get-text-property (point) 'neomacs-webkit-id)
                neomacs-webkit--input-view-id)
      (neomacs-webkit--log "cursor left view %d, deactivating"
                           neomacs-webkit--input-view-id)
      (neomacs-webkit--deactivate-input))))

(add-hook 'post-command-hook #'neomacs-webkit--post-command-check)

(provide 'neomacs-webkit)

;;; neomacs-webkit.el ends here
