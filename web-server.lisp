(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code
	  ;; Try and parse an integer from the result
	  (parse-integer
	   ;; Convert the list of c1 and c2 to a string
	   (coerce (list c1 c2) 'string)
	   :radix 16
	   :junk-allowed t)))
    ;; If successful, get the matching character
    (if code
	(code-char code)
	;; Otherwise return the supplied default
	default)))

(defun decode-param (s)
  ;; Define function f
  (labels ((f (lst)
	     ;; When the argument is a list
	     (when list
	       ;; switch on the first element
	       (case (car lst)
		 ;; If it's a %
		 (#\%
		  ;; Create a cell consisting of
		  (cons
		   ;; The http char of the second element and third element
		   (http-char (cadr lst) (caddr lst))
		   ;; recurse on the remainder of the list
		   (f (cdddr lst))))
		 ;; If it's a +
		 (#\+
		  ;; Create a cell consisting of
		  (cons
		   ;; A space
		   #\space
		   ;; and then whatever the result of the remainder of the list is
		   (f (cdr lst))))
		 ;; Otherwise hold on to the current first element as is, the call f again on the remainder of the list
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    ;; Convert the string to a list and pass it to the function, then reconvert the result to a string when it's returned.
    (coerce (f (coerce s 'list)) 'string)))
