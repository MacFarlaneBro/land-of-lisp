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
  ;; For each column
  (mapcar (lambda (x)
	    ;; get the value of x + amt, bounded by 0 and 255
	    (min 255 (max 0 (+ x amt))))
	  ;; The arguments for mapcar
	  col))

(defun svg-style (color)
  ;; add the color values to the fill field.
  (format nil
	  "~{fill:rgb(~a,~a~,a);stroke:rgb(~a~a~a)~}"
	  (append color
		  (brightness color -100))))

(defun circle (center radius color)
  ;; Generate a circle tag
  (tag circle
       ;; set the center of the circle using x and y 
       (cx (car center)
	   cy (cdr center)
	   ;; Specify the radius of the circle
	   r radius
	   ;; specify the style
	   style (svg-style color))))

(svg (circle '(50 . 50) 50 '(255 0 0))
     (circle '(100 . 100) 50 '(0 0 255)))

(defun polygon (points color)
  (tag polygon (points (format nil
			       "~{~a,~a~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))
