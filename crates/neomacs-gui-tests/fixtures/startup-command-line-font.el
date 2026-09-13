;;; startup-command-line-font.el --- Explicit font at first native allocation -*- lexical-binding: t -*-

(run-at-time
 0.2 nil
 (lambda ()
   (condition-case err
       (progn
         (unless (and (equal (face-attribute 'default :family) "DejaVu Sans Mono")
                      (= (frame-char-width) 13) (= (frame-char-height) 25)
                      (= (frame-width) 80))
           (error "Command-line font geometry is incoherent: family=%S cell=%sx%s columns=%s parameters=%S"
                  (face-attribute 'default :family) (frame-char-width)
                  (frame-char-height) (frame-width) (frame-parameters)))
         (kill-emacs 0))
     (error (message "Command-line startup font: %S" err) (kill-emacs 1)))))
