;; Create a new, unnamed function and set it equal to *foo*
(defparameter *foo* (lambda ()
		      5))

;; Call the function referenced by *foo*
(funcall *FOO*)

;; Here the assigment of 5 to x is persisted through the use of a
;; closure, as long as the parameter *foo* references the closure
;; its internal state (referencing x) will persist and x will not
;; be garbage collected.
(defparameter *foo* (let ((x 5))
		      (lambda ()
			x)))

;; Create a local var 'line-number' with a value of 0
(let ((line-number 0))
  ;; Define a function 'my-print' which takes 1 argument
  ;; doing this inside a let creates a closure behind the scenes.
  (defun my-print (x)
    ;; Print the current line number
    (print line-number)
    ;; Print the argument
    (print x)
    ;; Increment the line number
    (incf line-number)
    ;; return nil
    nil))

(my-print "this")
(my-print "is")
(my-print "a")
(my-print "test")
