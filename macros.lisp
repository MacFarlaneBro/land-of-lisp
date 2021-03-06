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

;; The macroexpand function can be used to show the code generated by a macro
(macroexpand '(let1 foo (+2 3)
	       (* foo foo)))

;; Standard, tail call optimised list length function
(defun my-length (lst)
  (labels ((f (lst acc)
	     (if lst
		 (f (cdr lst) (1+ acc))
		 acc)))
    f lst 0))

(defmacro split (val yes no)
  ;; If the list has elements
  `(if ,val
       ;; split it into a head and tail
       (let ((head (car ,val))
	     (tail (cdr ,val)))
	 ;; return yes
	 ,yes) 
       ;; otherwise return no
       ,no))

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

;; This fails as the macro redefines the value of the variable x, this is called variable capture
(let1 x 100 (split '(2 3)
		   (+ x head)
		   nil))
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
