(defun add (a b)
  (princ "I am adding now")
  (+ a b))

(defmacro lazy (&body body)
  ;; Define 2 variables called forced & value
  (let ((forced (gensym))
	(value (gensym)))
    ;; Begin generating the code for teh body
    `(let ((,forced nil)
	   (,value nil))
       ;; This lambda function will be returned as a result of executing the
       ;; macro.
       (lambda ()
	 ;; If this value has not yet been forced
	 (unless ,forced
	   ;; Set the value of the execution of body to value
	   (setf ,value (progn ,@body))
	   ;; Set forced to true
	   (setf ,forced t))
	 ;; return value
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

;; The following functions force the lazy value before calling the function as normal
(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *foo* (lazy-cons 4 7))

(lazy-car *foo*)
(lazy-cdr *foo*)

(defparameter *integers*
  ;; Generate a local function f
  (labels ((f (n)
	     ;; when forced, recursively cons through another iteration of the function
	     (lazy-cons n (f (1+ n)))))
    ;; Finally call the function to start it off
    (f 1)))

;; As there is no exit condition for the above code it will just to continue to count up integers indefinitely.

;; Required to terminate a list
(defun lazy-nil ()
  (lazy nil))

;; Required to check for list termination
(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  ;; If there are still elements of the list
  (lazy (when list
	  ;; combine a lazy current element of the list
	  (cons (car lst)
		;; With the lazy remainder of the list
		(make-lazy (cdr lst))))))

(defun take (n lst)
  ;; Unless the list has terminated or n is zero
  (unless (or (zerop n) (lazy-null lst))
    ;; Get a cons of the first element of the list and take the remainder recursively
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(take 10 *integers*)

;; Lazily call a function on each element of a list
(defun lazy-mapcar (fun lst)
  ;; If the list is not yet finished
  (lazy (unless (lazy-null lst)
	  ;; call the function on the top item of the list
	  (cons (funcall fun (lazy-car lst))
		;; Recurse on the remainder of the list
		(lazy-mapcar fun (lazy-cdr lst))))))

;; Lazily call a function on each element of a list and return a list of the results
(defun lazy-mapcan (fun lst)
  ;; Define temp function f
  (labels ((f (lst-cur)
	     ;; If this element is the last element
	     (if (lazy-null lst-cur)
		 ;; The force the recursing fo the outer function on the remainder of the list
		 ;; thus returning the whole thing
		 (force (lazy-mapcan fun (lazy-cdr lst)))
		 ;; Otherwise cons the first element with the result of the lazy eval of the remainder
		 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    ;; Finally start the recursion using the newly defined function.
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  ;; Unless the list if empty
  (unless (lazy-null lst)
    ;; assign the first element of lst to x
    (let ((x (lazy-car lst)))
      ;; If calling the supplied function with x as its arg is true
      (if (funcall fun x)
	  ;; return x
	  x
	  ;; Otherwise recall the function on the remainder of the list
	  (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  ;; If n is zero
  (if (zerop n)
      ;; Return the value currently at the top of the list
      (lazy-car lst)
      ;; Otherwise decrement n and recurse
      (lazy-nth (1- n) (lazy-cdr lst))))
