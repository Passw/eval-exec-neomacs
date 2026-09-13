;;; startup-gpu-resize.el --- Native allocation after blocked GPU discovery -*- lexical-binding: t -*-

;; The harness resizes our native X window while driver discovery is blocked.
;; The process deadline is owned by the harness; passing requires Lisp to see
;; that actual native allocation after the GPU becomes usable.
(defvar startup-gpu-primary (selected-frame))
(defvar startup-gpu-child
  (make-frame `((parent-frame . ,startup-gpu-primary) (visibility . nil)
                (width . 40) (height . 10))))
(defvar startup-gpu-child-size
  (list (frame-native-width startup-gpu-child) (frame-native-height startup-gpu-child)))
(select-frame startup-gpu-child)
(with-temp-file (getenv "NEOMACS_GUI_GPU_READY_FILE") (insert "ready\n"))
(run-at-time
 0.05 0.05
 (lambda ()
   (unless (equal startup-gpu-child-size
                  (list (frame-native-width startup-gpu-child)
                        (frame-native-height startup-gpu-child)))
     (princ (format "GPU resize reached selected child: primary=%S child=%S expected-child=%S\n"
                    (list (frame-native-width startup-gpu-primary) (frame-native-height startup-gpu-primary))
                    (list (frame-native-width startup-gpu-child) (frame-native-height startup-gpu-child))
                    startup-gpu-child-size)
            'external-debugging-output)
     (kill-emacs 1))
   (when (and (= (frame-native-width startup-gpu-primary) 901)
              (= (frame-native-height startup-gpu-primary) 603)
              (equal startup-gpu-child-size
                     (list (frame-native-width startup-gpu-child)
                           (frame-native-height startup-gpu-child))))
     (princ "GPU-RESIZE-PASS\n" 'external-debugging-output)
     (kill-emacs 0))))
