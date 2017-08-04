(asdf:defsystem :icfpc-2017
  :serial t
  :depends-on (:anaphora
               :serapeum
               ;; :rutils
               :cl-json
               :usocket)
  :components ((:file :icfpc-2017)))

;; (asdf:defsystem icfpc-2015-test
;;   :serial t
;;   :depends-on (:icfpc-2015
;;                :prove)
;;   :components ((:file :t))
;;   ;; :defsystem-depends-on (:prove)
;;   ;; :perform (test-op :after (op c)
;;   ;;                   (funcall (intern #. (string :run-test-system) :cl-test-more)
;;   ;;                            c))
;;   )
