;;; neomacs-image.el --- Image display support for Neomacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Neomacs Contributors
;; Keywords: multimedia, images

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

;; This package provides GPU-accelerated image display support for Neomacs.
;; 
;; Basic usage:
;;   (neomacs-image-show-file "/path/to/image.png")
;;
;; API functions:
;;   `neomacs-image-load' - Load an image from file, returns image ID
;;   `neomacs-image-size' - Get image dimensions as (width . height)
;;   `neomacs-image-free' - Free an image from cache
;;   `neomacs-image-floating' - Display image as floating layer
;;   `neomacs-image-floating-clear' - Remove floating image layer

;;; Code:

(defvar neomacs-image--cache (make-hash-table :test 'eq)
  "Hash table mapping image IDs to their metadata.")

(defun neomacs-image-show-file (file &optional x y width height)
  "Display image FILE as a floating layer at X, Y with optional WIDTH, HEIGHT.
If WIDTH/HEIGHT are nil, uses the image's natural size.
X and Y default to 50 if not specified.
Returns the image ID on success, nil on failure."
  (interactive "fImage file: ")
  (let* ((path (expand-file-name file))
         (image-id (neomacs-image-load path)))
    (when image-id
      (let* ((size (neomacs-image-size image-id))
             (img-width (or width (car size)))
             (img-height (or height (cdr size)))
             (img-x (or x 50))
             (img-y (or y 50)))
        (puthash image-id `(:path ,path :width ,img-width :height ,img-height
                            :x ,img-x :y ,img-y)
                 neomacs-image--cache)
        (neomacs-image-floating image-id img-x img-y img-width img-height)
        (message "Showing image %d at (%d,%d) %dx%d" 
                 image-id img-x img-y img-width img-height)))
    image-id))

(defun neomacs-image-hide (image-id)
  "Hide the floating image layer for IMAGE-ID and free it from cache."
  (interactive "nImage ID: ")
  (neomacs-image-floating-clear image-id)
  (neomacs-image-free image-id)
  (remhash image-id neomacs-image--cache)
  (message "Image %d hidden and freed" image-id))

(defun neomacs-image-show-scaled (file scale &optional x y)
  "Display image FILE scaled by SCALE factor at X, Y.
SCALE is a float where 1.0 = original size, 0.5 = half, 2.0 = double.
Returns the image ID on success."
  (interactive "fImage file: \nnScale factor: ")
  (let* ((path (expand-file-name file))
         (image-id (neomacs-image-load path)))
    (when image-id
      (let* ((size (neomacs-image-size image-id))
             (img-width (round (* (car size) scale)))
             (img-height (round (* (cdr size) scale)))
             (img-x (or x 50))
             (img-y (or y 50)))
        (puthash image-id `(:path ,path :width ,img-width :height ,img-height
                            :x ,img-x :y ,img-y :scale ,scale)
                 neomacs-image--cache)
        (neomacs-image-floating image-id img-x img-y img-width img-height)
        (message "Showing image %d (%.1fx) at (%d,%d) %dx%d" 
                 image-id scale img-x img-y img-width img-height)))
    image-id))

(defun neomacs-image-info (image-id)
  "Get information about IMAGE-ID from the cache."
  (gethash image-id neomacs-image--cache))

(defun neomacs-image-list ()
  "List all loaded images."
  (interactive)
  (let ((images '()))
    (maphash (lambda (id info)
               (push (cons id info) images))
             neomacs-image--cache)
    (if images
        (with-help-window "*Neomacs Images*"
          (princ "Loaded Images:\n\n")
          (dolist (img (sort images (lambda (a b) (< (car a) (car b)))))
            (let ((id (car img))
                  (info (cdr img)))
              (princ (format "  ID %d: %s\n" id (plist-get info :path)))
              (princ (format "         Size: %dx%d at (%d,%d)\n"
                             (plist-get info :width)
                             (plist-get info :height)
                             (plist-get info :x)
                             (plist-get info :y))))))
      (message "No images loaded"))))

(defun neomacs-image-free-all ()
  "Free all loaded images."
  (interactive)
  (maphash (lambda (id _info)
             (neomacs-image-floating-clear id)
             (neomacs-image-free id))
           neomacs-image--cache)
  (clrhash neomacs-image--cache)
  (message "All images freed"))

(defun neomacs-image-move (image-id x y)
  "Move IMAGE-ID to new position X, Y while keeping its size."
  (interactive "nImage ID: \nnNew X: \nnNew Y: ")
  (let ((info (gethash image-id neomacs-image--cache)))
    (when info
      (let ((width (plist-get info :width))
            (height (plist-get info :height)))
        (neomacs-image-floating image-id x y width height)
        (puthash image-id (plist-put (plist-put info :x x) :y y)
                 neomacs-image--cache)
        (message "Moved image %d to (%d,%d)" image-id x y)))))

(defun neomacs-image-resize (image-id width height)
  "Resize IMAGE-ID to WIDTH, HEIGHT while keeping its position."
  (interactive "nImage ID: \nnNew width: \nnNew height: ")
  (let ((info (gethash image-id neomacs-image--cache)))
    (when info
      (let ((x (plist-get info :x))
            (y (plist-get info :y)))
        (neomacs-image-floating image-id x y width height)
        (puthash image-id (plist-put (plist-put info :width width) :height height)
                 neomacs-image--cache)
        (message "Resized image %d to %dx%d" image-id width height)))))

(provide 'neomacs-image)
;;; neomacs-image.el ends here
