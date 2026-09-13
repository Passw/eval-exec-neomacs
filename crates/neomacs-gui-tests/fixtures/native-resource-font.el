;;; native-resource-font.el --- Native resource at first allocation -*- lexical-binding: t -*-

(run-at-time
 0.2 nil
 (lambda ()
   (condition-case err
       (let* ((family (getenv "NEOMACS_GUI_RESOURCE_FAMILY"))
              (requested (getenv "NEOMACS_GUI_RESOURCE_FONT"))
              (control (open-font (font-spec :name requested)))
              (actual (face-attribute 'default :font)))
         (unless (equal (x-get-resource "font" "Font") requested)
           (error "Native resource lookup did not return %S" requested))
         (unless (and (equal (face-attribute 'default :family) family)
                      (= (frame-width) 80)
                      (fontp actual) (fontp control)
                      (numberp (font-get actual :size))
                      (= (font-get actual :size) (font-get control :size)))
           (error "Resource startup mismatch: family=%S wanted=%S columns=%S"
                  (face-attribute 'default :family) family (frame-width)))
         (with-temp-file (getenv "NEOMACS_GUI_STATE_JSON")
           (insert (json-serialize
                    `((native_width . ,(frame-native-width))
                      (native_height . ,(frame-native-height))
                      (columns . ,(frame-width)) (family . ,family)))))
         (kill-emacs 0))
     (error (message "Resource startup font: %S" err) (kill-emacs 1)))))
