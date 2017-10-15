;;;;
;;;; The Parametric system definition.
;;;; Alex Striff.
;;;;

(defsystem "parametric"
  :description "Parametric: A parametric curve exploration tool."
  :version "0.0.1"
  :author "Alex Striff"
  :licence "MIT License"
  :depends-on ("alexandria")
  :components ((:file "src/packages")
               (:file "src/param" :depends-on ("src/packages"))
               (:file "src/svg" :depends-on ("src/packages"))))

