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
	     (when lst
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

(decode-param  "foo%3F")
(decode-param "foo+bar")

(defun parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1
	   ;; These two cons calls essentially form the alist
	   (cons (cons
		  ;; Convert string to symbol
		  (intern
		   (string-upcase
		    ;; Get the key part of the param
		    (subseq s 0 i1)))
		  ;; Get the value part of the param, decoding any special characters
		  (decode-param (subseq s (1+ i1) i2)))
		 ;; Keep evaluating the params until they're all done
		 (and i2 (parse-params (subseq s (1+ i2))))))
	  ;; If we find an empty string, we've reached the end of the string of params
	  ((equal s "") nil)
	  (t s))))

(parse-params "name=bob&age=25&gender=male")

(defun parse-url (s)
  (let* ((url
	   ;; Get the substring of
	   (subseq s
		   ;; 2 after the position of the first space
		   (+ 2 (position #\space s))
		   ;; To the last space
		   (position #\space s :from-end t)))
	 ;; Get the postition of the end of the the url and beginnning of the params
	 (x (position #\? url)))
    ;; If the params exist
    (if x
	;; Extract the url and cons it to the params
	(cons (subseq url 0 x)
	      ;; Parase the url params
	      (parse-params (subseq url (1+ x))))
	;; Otherwise just return the url as is
	(cons url '()))))

(parse-url "GET /lolcats.html HTTP/1.1")

(parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1")

(defun get-header (stream)
  ;; Read s from the current line of the stream
  (let* ((s (read-line stream))
	 ;; if there's content left add the result to h
	 (h
	   ;; Set i to the position of the colon in the string
	   (let ((i (position #\: s)))
	     ;; If there is a colon in the string
	     (when i
	       ;; cons together
	       (cons
		;; The symbolised string before the colon
		(intern (string-upcase (subseq s 0 i)))
		;; The string after the colon
		(subseq s (+ i 2)))))))
    ;; While there are header lines left keep recursing
    (when h
      (cons h (get-header stream)))))
