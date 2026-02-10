;;; neo-term-test.el --- Manual test for neo-term GPU terminal -*- lexical-binding: t -*-

;; Usage: ./src/emacs -Q -l test/neomacs/neo-term-test.el
;;
;; This test verifies that the neo-term terminal emulator:
;;   1. Creates terminals via the FFI
;;   2. Renders terminal content as GPU glyphs
;;   3. Supports Window, Inline, and Floating modes
;;
;; If the FFI functions are not available (non-neomacs build), the test
;; shows a placeholder message.

;;; Code:

(require 'cl-lib)

(defun neo-term-test--ffi-available-p ()
  "Check if neo-term FFI functions are available."
  (fboundp 'neomacs-terminal-create))

(defun neo-term-test--insert-section (title)
  "Insert a section header."
  (insert (propertize (format "\n=== %s ===\n\n" title)
                      'face '(:weight bold :height 1.3))))

(defun neo-term-test--run ()
  "Run the neo-term test suite."
  (let ((buf (get-buffer-create "*neo-term-test*")))
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)

      (insert (propertize "Neo-Term GPU Terminal Test\n"
                          'face '(:weight bold :height 1.5 :underline t)))
      (insert (format "Date: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

      ;; Check FFI availability
      (neo-term-test--insert-section "FFI Status")

      (if (neo-term-test--ffi-available-p)
          (progn
            (insert (propertize "PASS" 'face '(:foreground "green" :weight bold)))
            (insert " neo-term FFI functions are available\n")

            ;; Test terminal creation
            (neo-term-test--insert-section "Terminal Creation")

            ;; Test 1: Window mode terminal
            (insert "Test 1: Creating Window mode terminal (80x24)... ")
            (condition-case err
                (let ((id (neomacs-terminal-create 80 24 0 nil)))
                  (if (and id (> id 0))
                      (progn
                        (insert (propertize (format "OK (id=%d)\n" id)
                                            'face '(:foreground "green")))
                        ;; Write some test data
                        (insert "  Writing test data... ")
                        (neomacs-terminal-write
                         id "echo 'Hello from neo-term!'\r")
                        (insert (propertize "OK\n" 'face '(:foreground "green")))

                        ;; Resize
                        (insert "  Resizing to 120x40... ")
                        (neomacs-terminal-resize id 120 40)
                        (insert (propertize "OK\n" 'face '(:foreground "green")))

                        ;; Destroy
                        (insert "  Destroying... ")
                        (neomacs-terminal-destroy id)
                        (insert (propertize "OK\n" 'face '(:foreground "green"))))
                    (insert (propertize "FAIL (id=0)\n"
                                        'face '(:foreground "red")))))
              (error
               (insert (propertize (format "ERROR: %s\n"
                                           (error-message-string err))
                                   'face '(:foreground "red")))))

            ;; Test 2: Inline mode terminal
            (insert "\nTest 2: Creating Inline mode terminal (60x10)... ")
            (condition-case err
                (let ((id (neomacs-terminal-create 60 10 1 nil)))
                  (if (and id (> id 0))
                      (progn
                        (insert (propertize (format "OK (id=%d)\n" id)
                                            'face '(:foreground "green")))
                        (neomacs-terminal-destroy id)
                        (insert "  Destroyed.\n"))
                    (insert (propertize "FAIL (id=0)\n"
                                        'face '(:foreground "red")))))
              (error
               (insert (propertize (format "ERROR: %s\n"
                                           (error-message-string err))
                                   'face '(:foreground "red")))))

            ;; Test 3: Floating mode terminal
            (insert "\nTest 3: Creating Floating mode terminal (80x24)... ")
            (condition-case err
                (let ((id (neomacs-terminal-create 80 24 2 nil)))
                  (if (and id (> id 0))
                      (progn
                        (insert (propertize (format "OK (id=%d)\n" id)
                                            'face '(:foreground "green")))
                        ;; Set float position
                        (insert "  Setting float position (100, 100, opacity=0.9)... ")
                        (neomacs-terminal-set-float id 100.0 100.0 0.9)
                        (insert (propertize "OK\n" 'face '(:foreground "green")))
                        (neomacs-terminal-destroy id)
                        (insert "  Destroyed.\n"))
                    (insert (propertize "FAIL (id=0)\n"
                                        'face '(:foreground "red")))))
              (error
               (insert (propertize (format "ERROR: %s\n"
                                           (error-message-string err))
                                   'face '(:foreground "red")))))

            ;; Summary
            (neo-term-test--insert-section "Summary")
            (insert "All FFI entry points tested.\n")
            (insert "For visual verification, use:\n")
            (insert "  M-x neo-term       -- open a window-mode terminal\n")
            (insert "  M-x neo-term-floating -- open a floating terminal\n"))

        ;; FFI not available
        (insert (propertize "SKIP" 'face '(:foreground "yellow" :weight bold)))
        (insert " neo-term FFI functions not available\n")
        (insert "\nThis is expected for non-neomacs builds.\n")
        (insert "The FFI is provided by the neomacs-display Rust library\n")
        (insert "with the 'neo-term' feature enabled.\n")
        (insert "\nTo test, build neomacs with:\n")
        (insert "  cargo build --manifest-path rust/neomacs-display/Cargo.toml\n")
        (insert "  make -C src emacs\n"))

      (neo-term-test--insert-section "Architecture")
      (insert "neo-term uses a two-thread architecture:\n\n")
      (insert "  Emacs (C)                     Render thread (Rust/wgpu)\n")
      (insert "  ─────────                     ──────────────────────────\n")
      (insert "  terminal_create() ──cmd──►    TerminalManager::create()\n")
      (insert "  terminal_write()  ──cmd──►    TerminalView::write()\n")
      (insert "  terminal_resize() ──cmd──►    TerminalView::resize()\n")
      (insert "  terminal_destroy()──cmd──►    TerminalManager::destroy()\n")
      (insert "                                PTY reader thread\n")
      (insert "                                  └─► ansi::Processor\n")
      (insert "                                  └─► Term::advance()\n")
      (insert "                                  └─► wakeup → render\n")
      (insert "\n  Terminal cells are expanded into FrameGlyph::Char\n")
      (insert "  and rendered through the existing rect + glyph pipeline.\n")

      (goto-char (point-min)))
    (special-mode)))

;; Run the test
(neo-term-test--run)

;;; neo-term-test.el ends here
