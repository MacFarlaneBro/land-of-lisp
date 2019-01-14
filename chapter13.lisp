(error "foo")

;; Create a new condition named foo
(define-condition foo () ()
  ;; print a customer error message to report the error
  (:report (lambda (condition stream)
	     (princ "Stop Fooing around, numbskull!" stream))))

(error 'foo)

(defun bad-function ()
  (error 'foo))

;; handle any exceptions which may emerge from bad-function
(handler-case (bad-function)
  ;; As our error is handled here we catch the exception and continue running
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))


;; unwind-protect is essentially a 'finally' operator, it guarantees to execute the 2nd-nth statements regardless of errors in the primary statement
(unwind-protect (/1 0)
  (princ "I ned to say 'flubyduby' no matter what"))
;; Many of the 'with-' macros included in common lisp include an implicit 'unwind-protect' under the hood
