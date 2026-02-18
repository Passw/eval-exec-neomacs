;;; neomacs-rounded-box-test.el --- Test rounded corner box rendering -*- lexical-binding: t -*-

;; Test the :corner-radius box attribute for neomacs GPU renderer.
;; Usage: ./src/emacs -Q -l test/neomacs/neomacs-rounded-box-test.el

;;; Commentary:

;; This test visually compares:
;;   1. Normal text (no box)
;;   2. Standard sharp box (:box with no corner-radius)
;;   3. Rounded corner box (:box with :corner-radius)
;;   4. Various corner radii and line widths

;;; Code:

;; --- Normal text (no box) ---
(defface neomacs-rbox-normal
  '((t :foreground "white" :background "gray25"))
  "Normal text - no box.")

;; --- Standard sharp boxes ---
(defface neomacs-rbox-sharp-thin
  '((t :foreground "white" :background "dark blue"
       :box (:line-width 1 :color "cyan")))
  "Sharp box, 1px cyan.")

(defface neomacs-rbox-sharp-medium
  '((t :foreground "white" :background "dark blue"
       :box (:line-width 2 :color "cyan")))
  "Sharp box, 2px cyan.")

(defface neomacs-rbox-sharp-thick
  '((t :foreground "white" :background "dark blue"
       :box (:line-width 3 :color "cyan")))
  "Sharp box, 3px cyan.")

;; --- Rounded boxes with various radii ---
(defface neomacs-rbox-round-r4
  '((t :foreground "white" :background "dark green"
       :box (:line-width 2 :color "orange" :corner-radius 4)))
  "Rounded box, radius=4.")

(defface neomacs-rbox-round-r8
  '((t :foreground "white" :background "dark green"
       :box (:line-width 2 :color "orange" :corner-radius 8)))
  "Rounded box, radius=8.")

(defface neomacs-rbox-round-r12
  '((t :foreground "white" :background "dark green"
       :box (:line-width 2 :color "orange" :corner-radius 12)))
  "Rounded box, radius=12.")

(defface neomacs-rbox-round-r16
  '((t :foreground "white" :background "dark green"
       :box (:line-width 2 :color "orange" :corner-radius 16)))
  "Rounded box, radius=16 (large).")

;; --- Rounded boxes with various line widths ---
(defface neomacs-rbox-round-w1
  '((t :foreground "white" :background "purple4"
       :box (:line-width 1 :color "yellow" :corner-radius 8)))
  "Rounded box, width=1, radius=8.")

(defface neomacs-rbox-round-w3
  '((t :foreground "white" :background "purple4"
       :box (:line-width 3 :color "yellow" :corner-radius 8)))
  "Rounded box, width=3, radius=8.")

(defface neomacs-rbox-round-w5
  '((t :foreground "white" :background "purple4"
       :box (:line-width 5 :color "yellow" :corner-radius 8)))
  "Rounded box, width=5, radius=8.")

;; --- Rounded boxes with different colors ---
(defface neomacs-rbox-red
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "red" :corner-radius 8)))
  "Red rounded box.")

(defface neomacs-rbox-green
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "lime green" :corner-radius 8)))
  "Green rounded box.")

(defface neomacs-rbox-blue
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "dodger blue" :corner-radius 8)))
  "Blue rounded box.")

(defface neomacs-rbox-gold
  '((t :foreground "black" :background "gold"
       :box (:line-width 3 :color "dark orange" :corner-radius 10)))
  "Gold tag style.")

;; --- Adjacent non-rounded (sharp) boxes with different colors ---
(defface neomacs-rbox-adj-sharp-red
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "red")))
  "Sharp red box for adjacency test.")

(defface neomacs-rbox-adj-sharp-green
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "lime green")))
  "Sharp green box for adjacency test.")

(defface neomacs-rbox-adj-sharp-blue
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "dodger blue")))
  "Sharp blue box for adjacency test.")

(defface neomacs-rbox-adj-sharp-gold
  '((t :foreground "black" :background "gold"
       :box (:line-width 2 :color "dark orange")))
  "Sharp gold box for adjacency test.")

;; --- Fancy border styles ---
(defface neomacs-rbox-style-solid
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "dodger blue" :corner-radius 10
             :border-style solid :border-speed 100)))
  "Fancy: solid.")

(defface neomacs-rbox-style-rainbow
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "white" :corner-radius 12
             :border-style rainbow :border-speed 100)))
  "Fancy: rainbow.")

(defface neomacs-rbox-style-animated-rainbow
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "white" :corner-radius 12
             :border-style animated-rainbow :border-speed 100)))
  "Fancy: animated rainbow.")

(defface neomacs-rbox-style-gradient
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "deep pink" :corner-radius 10
             :border-style gradient :border-speed 100 :color2 "cyan")))
  "Fancy: gradient.")

(defface neomacs-rbox-style-glow
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "lime green" :corner-radius 10
             :border-style glow :border-speed 100)))
  "Fancy: pulsing glow.")

(defface neomacs-rbox-style-neon
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "magenta" :corner-radius 10
             :border-style neon :border-speed 100 :color2 "cyan")))
  "Fancy: neon double-stroke.")

(defface neomacs-rbox-style-dashed
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "gold" :corner-radius 10
             :border-style dashed :border-speed 100)))
  "Fancy: dashed.")

(defface neomacs-rbox-style-comet
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "orange red" :corner-radius 12
             :border-style comet :border-speed 100)))
  "Fancy: comet trail.")

(defface neomacs-rbox-style-iridescent
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "steel blue" :corner-radius 10
             :border-style iridescent :border-speed 100)))
  "Fancy: iridescent.")

(defface neomacs-rbox-style-fire
  '((t :foreground "white" :background "gray20"
       :box (:line-width 3 :color "dark red" :corner-radius 10
             :border-style fire :border-speed 100)))
  "Fancy: fire / plasma.")

(defface neomacs-rbox-style-heartbeat
  '((t :foreground "white" :background "gray20"
       :box (:line-width 2 :color "red" :corner-radius 10
             :border-style heartbeat :border-speed 100)))
  "Fancy: heartbeat.")

;; --- Comparison: sharp vs rounded side by side ---
(defface neomacs-rbox-compare-sharp
  '((t :foreground "white" :background "#404060"
       :box (:line-width 2 :color "#8888ff")))
  "Sharp comparison box.")

(defface neomacs-rbox-compare-round
  '((t :foreground "white" :background "#404060"
       :box (:line-width 2 :color "#8888ff" :corner-radius 8)))
  "Rounded comparison box.")

;; ============================================================================
;; Build the test buffer
;; ============================================================================

(defun neomacs-rbox--insert-section (title)
  "Insert a section TITLE header."
  (insert "\n")
  (let ((start (point)))
    (insert (format "=== %s ===\n" title))
    (put-text-property start (point) 'face '(:weight bold :height 1.2 :foreground "gold"))))

(defun neomacs-rbox--insert-sample (label face-name)
  "Insert a labeled sample with FACE-NAME applied to sample text."
  (insert (format "  %-40s " label))
  (let ((start (point)))
    (insert " Sample Text ABCdef 123 ")
    (put-text-property start (point) 'face face-name))
  (insert "\n"))

(defun neomacs-rbox--insert-inline (label face-name)
  "Insert inline sample showing face in context."
  (insert (format "  %-40s before " label))
  (let ((start (point)))
    (insert "BOXED")
    (put-text-property start (point) 'face face-name))
  (insert " after\n"))

(defun neomacs-rounded-box-test ()
  "Create rounded corner box test buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Rounded Box Test*")))
    (switch-to-buffer buf)
    (erase-buffer)

    ;; Header
    (let ((start (point)))
      (insert "NEOMACS ROUNDED CORNER BOX TEST\n")
      (put-text-property start (point) 'face '(:weight bold :height 1.8 :foreground "cyan")))
    (insert (format "Emacs version: %s | Window system: %s\n" emacs-version window-system))
    (insert (make-string 78 ?-) "\n")

    ;; === NO BOX (baseline) ===
    (neomacs-rbox--insert-section "BASELINE: NO BOX")
    (neomacs-rbox--insert-sample "Normal text (no box):" 'neomacs-rbox-normal)

    ;; === SHARP BOX (standard Emacs) ===
    (neomacs-rbox--insert-section "SHARP BOX (standard, no corner-radius)")
    (neomacs-rbox--insert-sample "Sharp 1px cyan:" 'neomacs-rbox-sharp-thin)
    (neomacs-rbox--insert-sample "Sharp 2px cyan:" 'neomacs-rbox-sharp-medium)
    (neomacs-rbox--insert-sample "Sharp 3px cyan:" 'neomacs-rbox-sharp-thick)

    ;; === ROUNDED BOX: varying radius ===
    (neomacs-rbox--insert-section "ROUNDED BOX: VARYING CORNER RADIUS")
    (neomacs-rbox--insert-sample "Rounded radius=4:" 'neomacs-rbox-round-r4)
    (neomacs-rbox--insert-sample "Rounded radius=8:" 'neomacs-rbox-round-r8)
    (neomacs-rbox--insert-sample "Rounded radius=12:" 'neomacs-rbox-round-r12)
    (neomacs-rbox--insert-sample "Rounded radius=16:" 'neomacs-rbox-round-r16)

    ;; === ROUNDED BOX: varying line width ===
    (neomacs-rbox--insert-section "ROUNDED BOX: VARYING LINE WIDTH")
    (neomacs-rbox--insert-sample "Width=1 radius=8:" 'neomacs-rbox-round-w1)
    (neomacs-rbox--insert-sample "Width=3 radius=8:" 'neomacs-rbox-round-w3)
    (neomacs-rbox--insert-sample "Width=5 radius=8:" 'neomacs-rbox-round-w5)

    ;; === ROUNDED BOX: colors ===
    (neomacs-rbox--insert-section "ROUNDED BOX: DIFFERENT COLORS")
    (neomacs-rbox--insert-sample "Red border:" 'neomacs-rbox-red)
    (neomacs-rbox--insert-sample "Green border:" 'neomacs-rbox-green)
    (neomacs-rbox--insert-sample "Blue border:" 'neomacs-rbox-blue)
    (neomacs-rbox--insert-sample "Gold tag style:" 'neomacs-rbox-gold)

    ;; === SIDE BY SIDE COMPARISON ===
    (neomacs-rbox--insert-section "SIDE BY SIDE: SHARP vs ROUNDED")
    (insert "  ")
    (let ((s (point)))
      (insert " SHARP ")
      (put-text-property s (point) 'face 'neomacs-rbox-compare-sharp))
    (insert "  ")
    (let ((s (point)))
      (insert " ROUNDED ")
      (put-text-property s (point) 'face 'neomacs-rbox-compare-round))
    (insert "  ")
    (let ((s (point)))
      (insert " SHARP ")
      (put-text-property s (point) 'face 'neomacs-rbox-compare-sharp))
    (insert "  ")
    (let ((s (point)))
      (insert " ROUNDED ")
      (put-text-property s (point) 'face 'neomacs-rbox-compare-round))
    (insert "\n")

    ;; === INLINE CONTEXT ===
    (neomacs-rbox--insert-section "INLINE: BOX IN CONTEXT")
    (neomacs-rbox--insert-inline "Sharp inline:" 'neomacs-rbox-sharp-medium)
    (neomacs-rbox--insert-inline "Rounded inline:" 'neomacs-rbox-round-r8)
    (neomacs-rbox--insert-inline "Gold tag inline:" 'neomacs-rbox-gold)

    ;; === ADJACENT ROUNDED BOXES ===
    (neomacs-rbox--insert-section "ADJACENT ROUNDED BOXES")
    (insert "  ")
    (let ((s (point)))
      (insert " Red ")
      (put-text-property s (point) 'face 'neomacs-rbox-red))
    (let ((s (point)))
      (insert " Green ")
      (put-text-property s (point) 'face 'neomacs-rbox-green))
    (let ((s (point)))
      (insert " Blue ")
      (put-text-property s (point) 'face 'neomacs-rbox-blue))
    (let ((s (point)))
      (insert " Gold ")
      (put-text-property s (point) 'face 'neomacs-rbox-gold))
    (insert "\n")

    ;; === ADJACENT NON-ROUNDED (SHARP) BOXES ===
    (neomacs-rbox--insert-section "ADJACENT SHARP BOXES (no corner-radius)")
    (insert "  ")
    (let ((s (point)))
      (insert " Red ")
      (put-text-property s (point) 'face 'neomacs-rbox-adj-sharp-red))
    (let ((s (point)))
      (insert " Green ")
      (put-text-property s (point) 'face 'neomacs-rbox-adj-sharp-green))
    (let ((s (point)))
      (insert " Blue ")
      (put-text-property s (point) 'face 'neomacs-rbox-adj-sharp-blue))
    (let ((s (point)))
      (insert " Gold ")
      (put-text-property s (point) 'face 'neomacs-rbox-adj-sharp-gold))
    (insert "\n")

    ;; === FANCY BORDER STYLES ===
    (neomacs-rbox--insert-section "FANCY BORDER STYLES (:border-style)")
    (neomacs-rbox--insert-sample "solid:" 'neomacs-rbox-style-solid)
    (neomacs-rbox--insert-sample "rainbow:" 'neomacs-rbox-style-rainbow)
    (neomacs-rbox--insert-sample "animated-rainbow:" 'neomacs-rbox-style-animated-rainbow)
    (neomacs-rbox--insert-sample "gradient (pink→cyan):" 'neomacs-rbox-style-gradient)
    (neomacs-rbox--insert-sample "glow:" 'neomacs-rbox-style-glow)
    (neomacs-rbox--insert-sample "neon (magenta+cyan):" 'neomacs-rbox-style-neon)
    (neomacs-rbox--insert-sample "dashed:" 'neomacs-rbox-style-dashed)
    (neomacs-rbox--insert-sample "comet:" 'neomacs-rbox-style-comet)
    (neomacs-rbox--insert-sample "iridescent:" 'neomacs-rbox-style-iridescent)
    (neomacs-rbox--insert-sample "fire:" 'neomacs-rbox-style-fire)
    (neomacs-rbox--insert-sample "heartbeat:" 'neomacs-rbox-style-heartbeat)

    ;; === FANCY STYLES INLINE ===
    (neomacs-rbox--insert-section "FANCY STYLES: INLINE CONTEXT")
    (neomacs-rbox--insert-inline "animated-rainbow inline:" 'neomacs-rbox-style-animated-rainbow)
    (neomacs-rbox--insert-inline "gradient inline:" 'neomacs-rbox-style-gradient)
    (neomacs-rbox--insert-inline "neon inline:" 'neomacs-rbox-style-neon)
    (neomacs-rbox--insert-inline "comet inline:" 'neomacs-rbox-style-comet)
    (neomacs-rbox--insert-inline "heartbeat inline:" 'neomacs-rbox-style-heartbeat)

    ;; === ADJACENT FANCY BOXES ===
    (neomacs-rbox--insert-section "ADJACENT FANCY BOXES")
    (insert "  ")
    (let ((s (point)))
      (insert " Rainbow ")
      (put-text-property s (point) 'face 'neomacs-rbox-style-rainbow))
    (let ((s (point)))
      (insert " Gradient ")
      (put-text-property s (point) 'face 'neomacs-rbox-style-gradient))
    (let ((s (point)))
      (insert " Neon ")
      (put-text-property s (point) 'face 'neomacs-rbox-style-neon))
    (let ((s (point)))
      (insert " Fire ")
      (put-text-property s (point) 'face 'neomacs-rbox-style-fire))
    (let ((s (point)))
      (insert " Heartbeat ")
      (put-text-property s (point) 'face 'neomacs-rbox-style-heartbeat))
    (insert "\n")

    ;; === EXPECTED BEHAVIOR ===
    (neomacs-rbox--insert-section "EXPECTED BEHAVIOR")
    (insert "
  - Normal text: plain colored background, no border
  - Sharp box: rectangular border with 90-degree corners
  - Rounded box: border with smooth rounded corners
  - Background fill should also be rounded (not sharp rect)
  - Border should render on top of text (visible ring)
  - Consecutive boxed chars should merge into single box span
  - Adjacent boxes with different faces should be separate
  - Corner radius is clamped to 45%% of box dimensions
")

    (goto-char (point-min))
    (setq buffer-read-only t)
    (message "Rounded box test loaded! Scroll through to check rendering.")))

;; Run automatically
(neomacs-rounded-box-test)

;;; neomacs-rounded-box-test.el ends here
