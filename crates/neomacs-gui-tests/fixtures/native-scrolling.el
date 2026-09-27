;;; native-scrolling.el --- Observe native scroll gestures -*- lexical-binding: t; -*-
(require 'json)
(load (expand-file-name "../../neomacs-perf/fixtures/scrolling-content.el"
                        (file-name-directory load-file-name)) nil nil t)
(defvar neomacs-scroll-rich (getenv "NEOMACS_GUI_SCROLL_RICH"))
(defvar neomacs-scroll-content-metadata nil)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(defvar neomacs-scroll-sample 0)
(defvar neomacs-scroll-window nil)
(defvar neomacs-scroll-pixels 0)
(defvar neomacs-scroll-wheels 0)
(defvar neomacs-scroll-pages 0)
(defvar neomacs-scroll-lines
  (string-to-number (or (getenv "NEOMACS_GUI_SCROLL_LINES") "400")))
(dolist (command '(scroll-up-command scroll-down-command
                   pixel-scroll-interpolate-down pixel-scroll-interpolate-up))
  (advice-add command :after
              (lambda (&rest _args)
                (setq neomacs-scroll-pages (1+ neomacs-scroll-pages)))))
(advice-add 'mwheel-scroll :after
            (lambda (&rest _args)
              (setq neomacs-scroll-wheels (1+ neomacs-scroll-wheels))))
(advice-add 'pixel-scroll-precision :after
            (lambda (event)
              (when (consp (nth 4 event))
                (setq neomacs-scroll-pixels
                      (+ neomacs-scroll-pixels (abs (cdr (nth 4 event))))))))
(defun neomacs-scroll-observe ()
  (setq neomacs-scroll-sample (1+ neomacs-scroll-sample))
  (let ((state `((sample . ,neomacs-scroll-sample)
                 (content . ,neomacs-scroll-content-metadata)
                 (buffer-size . ,(with-current-buffer (window-buffer neomacs-scroll-window)
                                   (buffer-size)))
                 (processed-pixels . ,neomacs-scroll-pixels)
                 (processed-wheels . ,neomacs-scroll-wheels)
                 (processed-pages . ,neomacs-scroll-pages)
                 (start . ,(window-start neomacs-scroll-window))
                 (vscroll . ,(window-vscroll neomacs-scroll-window t))
                 (point . ,(window-point neomacs-scroll-window))
                 (selected . ,(eq neomacs-scroll-window (selected-window)))
                 (selected-start . ,(window-start)))))
    (with-temp-file (getenv "NEOMACS_GUI_STATE_JSON")
      (insert (json-encode state))))
  (run-at-time 0.05 nil #'neomacs-scroll-observe))
(run-at-time
 1 nil
 (lambda ()
   (switch-to-buffer (get-buffer-create "*native-scrolling*"))
   (delete-other-windows)
   (erase-buffer)
   (if neomacs-scroll-rich
       (progn
         (neomacs-scroll-content-insert neomacs-scroll-lines)
         (setq neomacs-scroll-content-metadata neomacs-scroll-content-summary))
     ;; Repeat fixed-width lines so buffer size changes without changing row geometry.
     (let ((block (mapconcat
                   (lambda (i) (format "Line %03d -- native scrolling diagnostic\n" i))
                   (number-sequence 0 999) "")))
       (dotimes (_ (/ neomacs-scroll-lines 1000))
         (insert block))
       (dotimes (i (% neomacs-scroll-lines 1000))
         (insert (format "Line %03d -- native scrolling diagnostic\n" i)))))
   (goto-char (point-min))
   (when (> neomacs-scroll-lines 400)
     (forward-line (/ neomacs-scroll-lines 2))
     (set-window-start (selected-window) (point) t))
   (setq neomacs-scroll-window (selected-window))
   (when (getenv "NEOMACS_GUI_SCROLL_OTHER_WINDOW")
     (select-window (split-window-right)))
   (neomacs-scroll-observe)))
(run-at-time 180 nil (lambda () (kill-emacs 2)))
