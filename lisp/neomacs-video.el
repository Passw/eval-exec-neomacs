;;; neomacs-video.el --- Video playback support for Neomacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Neomacs Contributors
;; Keywords: multimedia, video

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

;; This package provides video playback support for Neomacs using GStreamer.
;; 
;; Basic usage:
;;   (neomacs-video-play-file "/path/to/video.mp4")
;;
;; API functions:
;;   `neomacs-video-load' - Load a video from URI, returns video ID
;;   `neomacs-video-play' - Start playback
;;   `neomacs-video-pause' - Pause playback
;;   `neomacs-video-stop' - Stop playback
;;   `neomacs-video-insert' - Insert video display at point

;;; Code:

(defvar neomacs-video--players (make-hash-table :test 'eq)
  "Hash table mapping video IDs to their metadata.")

(defun neomacs-video-play-file (file)
  "Play video FILE in a new player.
Returns the video ID on success, nil on failure."
  (interactive "fVideo file: ")
  (let* ((uri (if (string-match-p "^[a-z]+://" file)
                  file
                (concat "file://" (expand-file-name file))))
         (video-id (neomacs-video-load uri)))
    (when video-id
      (puthash video-id `(:uri ,uri :state playing) neomacs-video--players)
      (neomacs-video-play video-id)
      (message "Playing video %d: %s" video-id file))
    video-id))

(defun neomacs-video-toggle-pause (video-id)
  "Toggle pause state of VIDEO-ID."
  (interactive "nVideo ID: ")
  (let* ((info (gethash video-id neomacs-video--players))
         (state (plist-get info :state)))
    (if (eq state 'playing)
        (progn
          (neomacs-video-pause video-id)
          (puthash video-id (plist-put info :state 'paused) neomacs-video--players)
          (message "Video %d paused" video-id))
      (neomacs-video-play video-id)
      (puthash video-id (plist-put info :state 'playing) neomacs-video--players)
      (message "Video %d playing" video-id))))

(defun neomacs-video-stop-all ()
  "Stop all playing videos."
  (interactive)
  (maphash (lambda (id _info)
             (neomacs-video-stop id))
           neomacs-video--players)
  (clrhash neomacs-video--players)
  (message "All videos stopped"))

(defun neomacs-video-insert (file &optional width height)
  "Insert video FILE at point with optional WIDTH and HEIGHT.
WIDTH and HEIGHT default to 640x360 if not specified.
Returns the video ID on success.  Point is left after the video."
  (interactive "fVideo file: ")
  (let* ((uri (if (string-match-p "^[a-z]+://" file)
                  file
                (concat "file://" (expand-file-name file))))
         (video-id (neomacs-video-load uri))
         (w (or width 640))
         (h (or height 360)))
    (when video-id
      (puthash video-id `(:uri ,uri :state stopped :width ,w :height ,h)
               neomacs-video--players)
      ;; Insert a placeholder with video display property
      ;; The display engine will render this as a VIDEO_GLYPH
      (let ((start (point)))
        (insert " ")
        (put-text-property start (point) 'display
                           `(video :id ,video-id :width ,w :height ,h))
        (put-text-property start (point) 'neomacs-video-id video-id))
      ;; Point is now AFTER the video, so subsequent inserts go after it
      (message "Inserted video %d" video-id)
      video-id)))

(defun neomacs-video-at-point ()
  "Get the video ID at point, or nil if none."
  (get-text-property (point) 'neomacs-video-id))

(defun neomacs-video-play-at-point ()
  "Start playing the video at point."
  (interactive)
  (let ((video-id (neomacs-video-at-point)))
    (if video-id
        (progn
          (neomacs-video-play video-id)
          (message "Playing video %d" video-id))
      (message "No video at point"))))

(defun neomacs-video-pause-at-point ()
  "Pause the video at point."
  (interactive)
  (let ((video-id (neomacs-video-at-point)))
    (if video-id
        (progn
          (neomacs-video-pause video-id)
          (message "Paused video %d" video-id))
      (message "No video at point"))))

(defun neomacs-video-show-floating (file &optional x y width height)
  "Show video FILE as a floating layer at X, Y with WIDTH, HEIGHT.
Defaults: X=50, Y=50, WIDTH=640, HEIGHT=360.
Returns the video ID on success."
  (interactive "fVideo file: ")
  (let* ((uri (if (string-match-p "^[a-z]+://" file)
                  file
                (concat "file://" (expand-file-name file))))
         (video-id (neomacs-video-load uri))
         (vx (or x 50))
         (vy (or y 50))
         (vw (or width 640))
         (vh (or height 360)))
    (when video-id
      (puthash video-id `(:uri ,uri :state playing :x ,vx :y ,vy :width ,vw :height ,vh)
               neomacs-video--players)
      (neomacs-video-floating video-id vx vy vw vh)
      (neomacs-video-play video-id)
      (message "Playing video %d as floating at (%d,%d) %dx%d" video-id vx vy vw vh))
    video-id))

(defun neomacs-video-hide-floating (video-id)
  "Hide the floating video layer for VIDEO-ID and stop playback."
  (interactive "nVideo ID: ")
  (neomacs-video-floating-clear video-id)
  (neomacs-video-stop video-id)
  (remhash video-id neomacs-video--players)
  (message "Video %d floating layer hidden" video-id))

;;; Loop Control

(defvar neomacs-video--loop-timers (make-hash-table :test 'eq)
  "Hash table mapping video IDs to their loop update timers.")

(defun neomacs-video-loop (video-id &optional loop-count)
  "Enable loop playback for VIDEO-ID.
LOOP-COUNT can be:
  nil or t - infinite loop
  0 - no looping (disable loop)
  positive integer - loop that many times

Returns t on success."
  (interactive "nVideo ID: \nP")
  (let ((count (cond
                ((null loop-count) -1)  ; Default: infinite
                ((eq loop-count t) -1)  ; t means infinite
                ((integerp loop-count) loop-count)
                (t -1))))               ; Fallback: infinite
    (when (neomacs-video-set-loop video-id count)
      ;; Update metadata
      (let ((info (gethash video-id neomacs-video--players)))
        (when info
          (puthash video-id (plist-put info :loop count) neomacs-video--players)))
      ;; Set up or cancel the update timer for EOS detection
      (let ((existing-timer (gethash video-id neomacs-video--loop-timers)))
        (when existing-timer
          (cancel-timer existing-timer)
          (remhash video-id neomacs-video--loop-timers)))
      (if (= count 0)
          (message "Video %d loop: disabled" video-id)
        ;; Start timer to call update periodically for EOS detection
        (let ((timer (run-with-timer 0.2 0.2 
                       (lambda ()
                         (when (gethash video-id neomacs-video--players)
                           (neomacs-video-update video-id))))))
          (puthash video-id timer neomacs-video--loop-timers))
        (message "Video %d loop: %s" video-id 
                 (if (< count 0) "infinite" (format "%d times" count))))
      t)))

(defun neomacs-video-loop-infinite (video-id)
  "Enable infinite loop for VIDEO-ID."
  (interactive "nVideo ID: ")
  (neomacs-video-loop video-id t))

(defun neomacs-video-loop-disable (video-id)
  "Disable loop for VIDEO-ID."
  (interactive "nVideo ID: ")
  (neomacs-video-loop video-id 0))

(defun neomacs-video-show-floating-loop (file &optional x y width height)
  "Show video FILE as floating and loop infinitely.
Like `neomacs-video-show-floating' but with automatic looping."
  (interactive "fVideo file: ")
  (let ((video-id (neomacs-video-show-floating file x y width height)))
    (when video-id
      (neomacs-video-loop video-id t))
    video-id))

(provide 'neomacs-video)
;;; neomacs-video.el ends here
