(asdf:defsystem :icfpc-2017
  :serial t
  :depends-on (:cl-json
               :usocket
               :cl-dot
               :alexandria
               :cl-containers)
  :components ((:file :icfpc-2017)))
