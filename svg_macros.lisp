;; Define a new macro called let1 which takes three arguments, var, val and body
(defmacro let1  (var val
		;; This is essentially a varargs definition
		&body body)
  ;; Transform those arguments into the lisp code defined below
  `(let ((,var ,val))
     ;; The body argument is put in place using the splicing comma (,@)
     ,@body))

(let ((foo (+ 2 3)))
  (* foo foo))

(let1
    ;; var
    foo
    ;; val
    (+ 2 3)
  ;; body
  (* foo foo))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(defun my-length (lst)
  (labels ((f (lst acc)
	     (split lst
		    ;; Here we use the tail variable defined in the macro, though not present elsewhere in the defined function, macros that automatically generate variables in this way are called anaphoric macros.
		    (f tail (1+ acc))
		    acc)))
    (f lst 0)))

;; Updated version of split macro
(defmacro split (val yes no)
  `(let1 x ,val
     (if x
	 (let ((head (car x))
	       (tail (cdr x)))
	   ,yes
	   ,no))))

;; In order to avoid the previous issue, there is a CL function whose sole purpose is to generate silly variable names that noone would ever personally use
(gensym)

;; Final split macro version
(defmacro split (val yes no)
  ;; First define a varible 'g' using the no-clash gensym name
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
	   (let ((head (car ,g))
		 (tail (cdr ,g)))
	     ,yes)
	   ,no))))

(macroexpand '(split '(2 3)
	       (+ x head)
	       nil))

;; Recursive Macros

(defun pairs (lst)
  ;; Define function take a list & accumulator
  (labels ((f (lst acc)
	     ;; Split the list 
	     (split lst
		    ;; If there are items in the tail end
		    (if tail
			;; Call the function again with the remainder of the list
			(f (cdr tail)
			   ;; cons the cons of head and tail to remainder of accumulator
			   (cons (cons head
				       ;; cons first element of tail to head
				       (car tail))
				 acc))
			;; If tail is empty, reverse the accumulator and return it
			(reverse acc))
		    ;; return the reverse of the accumulator
		    (reverse acc))))
    ;; Start the function with the full list & empty accumulator
    (f lst nil)))

(pairs '(a b c d e f ))

(defmacro recurse (vars &body body)
  ;; Create a variable 'p' by splitting the 'vars' into pairs using the pairs function
  (let1 p (pairs vars)
    ;; Define the local function 'self'
    `(labels ((self
		  ;; Pass the first element (the name) of each item of p into body
		  ,(mapcar #'car p)
		,@body))
       ;; Call the self function to start the recursion, using the second element (value) of each
       ;; item in p
       (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse
      ;; Variable list 
      (lst				;name of list
       lst				;value of var
       acc				;name of accumulator
       0				;value of accumulator
       )
    (split lst
	   (f tail (1+ acc))
	   acc)))

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

(defun pairs (lst)
  ;; Define function take a list & accumulator
  (labels ((f (lst acc)
	     ;; Split the list 
	     (split lst
		    ;; If there are items in the tail end
		    (if tail
			;; Call the function again with the remainder of the list
			(f (cdr tail)
			   ;; cons the cons of head and tail to remainder of accumulator
			   (cons (cons head
				       ;; cons first element of tail to head
				       (car tail))
				 acc))
			;; If tail is empty, reverse the accumulator and return it
			(reverse acc))
		    ;; return the reverse of the accumulator
		    (reverse acc))))
    ;; Start the function with the full list & empty accumulator
    (f lst nil)))


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
  ;; If the length isnt zero
  (unless (zerop length)
    ;; Cons value with the result of
    (cons value
	  ;; Recursively call the function with either
	  (random-walk (if (zerop (random 2))
			   ;; one less than the current value
			   (1- value)
			   ;; or one more tthan the current value
			   (1+ value))
		       ;; reduce the length before recursing
		       (1- length)))))

(random-walk 100 10)

(with-open-file (*standard-output* "random_walk.svg"
				   :direction :output
				   :if-exists :supersede)
  ;; Open the given file and overwrite it if it exists
  (svg (loop repeat 10
	     ;; For 10 iterations
	     do (polygon
		 ;; Create a polygon
		 (append '((0 . 200))
			 ;; For the length of the graph
			 (loop for x
			       ;; Generate graph ticks from 0-400 for 100 ticks
			       for y in (random-walk 100 400)
			       ;; Return a cons plot of the x and y values
			       collect (cons x y))
			 '((400 . 200)))
		 ;; Get three random numbers between 1 and 256 
		 (loop repeat 3
		       collect (random 256))))))
