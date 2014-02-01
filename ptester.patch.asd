;;;; ptester.patch.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :ptester.patch
  :serial t
  :depends-on (:fiveam
               :macroexpand-dammit
               :ptester)
  :components ((:file "package")
               (:file "ptester.patch")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :ptester.patch))))
  (load-system :ptester.patch)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) :ptester.patch)))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
