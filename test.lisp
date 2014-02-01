;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package #:ptester)


(5am:def-suite :ptester.patch)
(5am:in-suite :ptester.patch)

(defvar *test-output*
  (make-two-way-stream (make-concatenated-stream)
                       (make-broadcast-stream)))

(5am:test :ptester
  (let ((*error-output* *test-output*))
    (5am:is-false (macrolet ((with-error-protect (form)
                               (let ((*error-protect-tests* T))
                                 (macroexpand-dammit:macroexpand-dammit form))))
                    (with-error-protect (test 1 (error "foo")))))
    (5am:is-true (test-error (error "foo")))
    (5am:is-false (test-no-error (error "foo")))
    (5am:is-false (test-error (car '(10))))
    (5am:is-true (test-warning (warn "foo")))
    (5am:is-false (test-no-warning (warn "foo")))
    (5am:is-false (test-warning (car '(10))))
    (5am:is-true (test-no-warning (car '(10))))
    (5am:is-true (test-error (error "foo: ~a" 10)))
    (5am:is-true (test-error (error "foo: ~a" 10)
                             :format-control "foo: ~a"))
    (5am:is-true (test-error (error "foo: ~a" 10) 
                             :format-control "foo: ~a"
                             :format-arguments '(10)))
    (5am:is-false (test-error (error "foo: ~a" 10) :format-control "foo:  ~a"))
    (5am:is-false (test-error (error "foo: ~a" 10) 
                              :format-control "foo: ~a"
                              :format-arguments '(11)))
    (5am:is-true (test-error (error "foo: ~a" 10)
                             :condition-type 'condition
                             :include-subtypes t))
    (5am:is-false (test-error (error "foo: ~a" 10)
                              :condition-type 'ptester.patch:simple-break
                              :include-subtypes t))
    (5am:is-true (test-error (break "foo: ~a" 10) :catch-breaks t
                             :condition-type 'ptester.patch:simple-break
                             :include-subtypes t)) ))


;;; *EOF*
