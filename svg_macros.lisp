(defun print-tag (name alst closingp)
  ;; Print the opening tag
  (princ #\<)
  ;; If it's a closing tag print the requisite slash
  (when closingp
    (princ #\/))
  ;; print the tag name
  (princ (string-downcase name))
  ;; For each attribute 
  (mapc (lambda (att)
	  ;; print the name and value of the attribute separated by =
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst)
  ;; Print the closing tag
  (princ #\>))

(print-tag 'mytag '((color . blue) (height . 9)) nil)

(defmacro tag (name atts &body body)
  ;; Execute the commands in order
  `(progn
     ;; Print the start tag using the given name
     (print-tag ',name
		;; create a list of attribute cons cells from the provided list of attributes
		(list ,@(mapcar (lambda (x)
				  `(cons ',(car x) ,(cdr x)))
				;; split the attributes into pairs using the previously defined macro
				(pairs atts)))
		;; Mark the tag as an opening tag
		nil)
     ;; execute the contents
     ,@body
     ;; print the end tag
     (print-tag ',name nil t)))

(macroexpand '(tag mytag (color 'blue height (+ 4 5))))


(tag html ()
  (tag body ()
    (princ "Hello World!")))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(defmacro svg (&body body)
  ;; Create the outer svg tag with the xml namespace set to the svg spec
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   ;; and the link sent to the xlink (enables hyperlinks)
		   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ;; Then enter the body
     ,@body))


(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))
