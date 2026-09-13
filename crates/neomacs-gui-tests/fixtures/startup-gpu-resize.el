;;; startup-gpu-resize.el --- Native allocation after blocked GPU discovery -*- lexical-binding: t -*-

;; The harness resizes our native X window while driver discovery is blocked.
;; The process deadline is owned by the harness; passing requires Lisp to see
;; that actual native allocation after the GPU becomes usable.
(run-at-time
 0.05 0.05
 (lambda ()
   (when (and (= (frame-native-width) 901) (= (frame-native-height) 603))
     (princ "GPU-RESIZE-PASS\n" 'external-debugging-output)
     (kill-emacs 0))))
