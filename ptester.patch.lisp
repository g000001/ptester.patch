;;;; ptester.patch.lisp -*- Mode: Lisp;-*- 

(in-package #:ptester)

;;; "ptester.patch" goes here. Hacks and glory await!

#-allegro
(defmacro test-error (form &key announce
                                catch-breaks
                                (fail-info nil fail-info-given)
                                (known-failure nil known-failure-given)
                                (condition-type ''simple-error)
                                (include-subtypes nil include-subtypes-given)
                                (format-control nil format-control-given)
                                (format-arguments nil format-arguments-given))
  "Test that `form' signals an error. The order of evaluation of the
arguments is keywords first, then test form.

If `announce' is non-nil, then cause the error message to be printed.

The `catch-breaks' is non-nil then consider a call to common-lisp:break an
`error'.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures.

If `condition-type' is non-nil, it should be a symbol naming a condition
type, which is used to check against the signalled condition type.  The
test will fail if they do not match.

`include-subtypes', used with `condition-type', can be used to match a
condition to an entire subclass of the condition type hierarchy.

`format-control' and `format-arguments' can be used to check the error
message itself."
  (let ((g-announce (gensym))
        (g-catch-breaks (gensym))
        (g-fail-info (gensym))
        (g-known-failure (gensym))
        (g-condition-type (gensym))
        (g-include-subtypes (gensym))
        (g-format-control (gensym))
        (g-format-arguments (gensym))
        (g-c (gensym)))
    `(let* ((,g-announce ,announce)
            (,g-catch-breaks ,catch-breaks)
            ,@(when fail-info-given `((,g-fail-info ,fail-info)))
            ,@(when known-failure-given `((,g-known-failure ,known-failure)))
            (,g-condition-type ,condition-type)
            ,@(when include-subtypes-given
                `((,g-include-subtypes ,include-subtypes)))
            ,@(when format-control-given
                `((,g-format-control ,format-control)))
            ,@(when format-arguments-given
                `((,g-format-arguments ,format-arguments)))
            (,g-c (test-values-errorset ,form ,g-announce ,g-catch-breaks)))
       (test-check
        :predicate #'eq
        :expected-result t
        :test-results
        (test-values (and (conditionp ,g-c)
                          ,@(if* include-subtypes-given
                               then `((if* ,g-include-subtypes
                                         then (typep ,g-c ,g-condition-type)
                                         else (eq (class-of ,g-c)
                                                  (find-class
                                                   ,g-condition-type))))
                               else `((eq (class-of ,g-c)
                                          (find-class ,g-condition-type))))
                          ,@(when format-control-given
                              `((or
                                 (null ,g-format-control)
                                 (string= 
                                  ,g-format-control
                                  (simple-condition-format-control ,g-c)))))
                          ,@(when format-arguments-given
                              `((or
                                 (null ,g-format-arguments)
                                 (equal
                                  ,g-format-arguments
                                  (simple-condition-format-arguments ,g-c))))))
                     t)
        :test-form ',form
        ,@(when fail-info-given `(:fail-info ,g-fail-info))
        ,@(when known-failure-given `(:known-failure ,g-known-failure))
        :condition-type ,g-condition-type
        :condition ,g-c
        ,@(when include-subtypes-given
            `(:include-subtypes ,g-include-subtypes))
        ,@(when format-control-given
            `(:format-control ,g-format-control))
        ,@(when format-arguments-given
            `(:format-arguments ,g-format-arguments))))))


#-allegro
(defun test-check (&key (predicate #'eql)
                        expected-result test-results test-form
                        multiple-values fail-info known-failure
                        wanted-message got-message condition-type condition
                        include-subtypes format-control format-arguments
                   &aux fail predicate-failed got wanted)
  ;; for debugging large/complex test sets:
  (when *announce-test*
    (format t "Just did test ~s~%" test-form)
    (force-output))

  ;; this is an internal function
  (flet ((check (expected-result result)
           (let* ((results
                   (multiple-value-list
                    (errorset (funcall predicate expected-result result))))
                  (failed (null (car results))))
             (if failed
                 (progn
                   (setq predicate-failed t)
                   nil)
                 (cadr results)))))
    (when (conditionp test-results)
      (setq condition test-results)
      (setq test-results nil))
    (when (null (car test-results))
      (setq fail t))
    (if* (and (not fail) (not multiple-values))
       then ;; should be a single result
            ;; expected-result is the single result wanted
            (when (not (and (cdr test-results)
                            (check expected-result (cadr test-results))))
              (setq fail t))
            (when (and (not fail) (cddr test-results))
              (setq fail 'single-got-multiple))
       else ;; multiple results wanted
            ;; expected-result is a list of results, each of which
            ;; should be checked against the corresponding test-results
            ;; using the predicate
            (do ((got (cdr test-results) (cdr got))
                 (want expected-result (cdr want)))
                ((or (null got) (null want))
                 (when (not (and (null want) (null got)))
                   (setq fail t)))
              (when (not (check (car got) (car want)))
                (return (setq fail t)))))
    (if* fail
       then (when (not known-failure)
              (format *error-output*
                      "~& * * * UNEXPECTED TEST FAILURE * * *~%")
              (incf *test-unexpected-failures*))
            (format *error-output* "~&Test failed: ~@[known failure: ~*~]~s~%"
                    known-failure test-form)
            (if* (eq 'single-got-multiple fail)
               then (format
                     *error-output*
                     "~
Reason: additional value were returned from test form.~%")
             elseif predicate-failed
               then (format *error-output* "Reason: predicate error.~%")
             elseif (null (car test-results))
               then (format *error-output* "~
Reason: an error~@[ (of type `~s')~] was detected.~%"
                            (when condition (class-of condition)))
             elseif condition
               then (if* (not (conditionp condition))
                       then (format *error-output* "~
Reason: expected but did not detect an error of type `~s'.~%"
                                    condition-type)
                     elseif (null condition-type)
                       then (format *error-output* "~
Reason: detected an unexpected error of type `~s':
        ~a.~%"
                                    (class-of condition)
                                    condition)
                     elseif (not (if* include-subtypes
                                    then (typep condition condition-type)
                                    else (eq (class-of condition)
                                             (find-class condition-type))))
                       then (format *error-output* "~
Reason: detected an incorrect condition type.~%")
                            (format *error-output*
                                    "  wanted: ~s~%" condition-type)
                            (format *error-output*
                                    "     got: ~s~%" (class-of condition))
                     elseif (and format-control
                                 (not (string=
                                       (setq got format-control)
                                       (setq wanted
                                         (simple-condition-format-control
                                          condition)))))
                       then ;; format control doesn't match
                            (format *error-output* "~
Reason: the format-control was incorrect.~%")
                            (format *error-output* "  wanted: ~s~%" wanted)
                            (format *error-output* "     got: ~s~%" got)
                     elseif (and format-arguments
                                 (not (equal
                                       (setq got format-arguments)
                                       (setq wanted
                                         (simple-condition-format-arguments
                                          condition)))))
                       then (format *error-output* "~
Reason: the format-arguments were incorrect.~%")
                            (format *error-output* "  wanted: ~s~%" wanted)
                            (format *error-output* "     got: ~s~%" got)
                       else ;; what else????
                            (error "internal-error"))
               else (let ((*print-length* 50)
                          (*print-level* 10))
                      (if* wanted-message
                         then (format *error-output*
                                      "  wanted: ~a~%" wanted-message)
                         else (if* (not multiple-values)
                                 then (format *error-output*
                                              "  wanted: ~s~%"
                                              expected-result)
                                 else (format
                                       *error-output*
                                       "  wanted values: ~{~s~^, ~}~%"
                                       expected-result)))
                      (if* got-message
                         then (format *error-output*
                                      "     got: ~a~%" got-message)
                         else (if* (not multiple-values)
                                 then (format *error-output* "     got: ~s~%"
                                       (second test-results))
                                 else (format
                                       *error-output*
                                       "     got values: ~{~s~^, ~}~%"
                                       (cdr test-results))))))
            (when fail-info
              (format *error-output* "Additional info: ~a~%" fail-info))
            (incf *test-errors*)
            (when *break-on-test-failures*
              (break "~a is non-nil." '*break-on-test-failures*))
       else (when known-failure
              (format *error-output*
                      "~&Expected test failure for ~s did not occur.~%"
                      test-form)
              (when fail-info
                (format *error-output* "Additional info: ~a~%" fail-info))
              (setq fail t))
            (incf *test-successes*))
    (not fail)))


#-allegro
(progn
  (define-condition break-condition (condition) ())
  (define-condition ptester.patch:simple-break (break-condition simple-condition) 
    ((old-cond :reader old-cond :initarg :old-cond)
     (old-hook :reader old-hook :initarg :old-hook))))


#-allegro
(defun call-with-simple-break (fn)
  #+sbcl (let ((sb-ext:*invoke-debugger-hook* 
                (lambda (cond hook) 
                  (signal 'ptester.patch:simple-break
                          :old-cond cond
                          :old-hook hook))))
           (funcall fn)))


#-allegro
(defmacro test-values-errorset (form &optional announce catch-breaks)
  ;; internal macro
  (let ((g-announce (gensym))
        (g-catch-breaks (gensym)))
    `(let* ((,g-announce ,announce)
            (,g-catch-breaks ,catch-breaks))
       (handler-case (cons t (multiple-value-list 
                              (call-with-simple-break 
                               (lambda () ,form))))
         (condition (condition)
           (if* (and (null ,g-catch-breaks)
                     (typep condition 'ptester.patch:simple-break))
                then (let ((old-cond (old-cond condition)))
                       (apply #'break 
                              (simple-condition-format-control old-cond)
                              (simple-condition-format-arguments old-cond))) 
                elseif ,g-announce
                then (format *error-output* "~&Condition type: ~a~%"
                             (class-of condition))
                (format *error-output* "~&Message: ~a~%" condition))
           condition)))))


;;; *EOF*
