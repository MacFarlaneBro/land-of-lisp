;; describing places
(defparameter *nodes* '((living-room (you are in the living room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant weldong torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; paths, or edges in math-terms
(defparameter *edges* '((living-room
			 (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))


;; describing a path between places
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describing multiple paths between places
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; Fetch the value of 'living room' from the alist '*edges*'
(assoc 'living-room *edges*)

;; Get the second element of the top list
(cdr (assoc 'living-room *edges*))

;; For each of the objects in the list returned by cdr, apply the function mapcar
(append (mapcar #'describe-path (cdr (assoc 'living-room *edges*))))

;; Append each output of mapcar together into a single list
(apply #'append
       ;; For each of the objects in the list returned by cdr, apply the function mapcar
       (mapcar #'describe-path
	       ;; Get the second element of the top list
	       (cdr
		;; Fetch the value of 'living room' from the table '*edges*'
		(assoc 'living-room *edges*))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  ;; Define a local function called 'at-loc-p' taking a single argument 'obj'
  (labels ((at-loc-p (obj)
	     ;; Check if the element is equal to loc
	     (eq
	      ;; Get the second element of the first list
	      (cadr
	       ;; fetch the object matching symbol 'obj from 'obj-locs'
	       (assoc obj obj-locs))
	      loc)))
    ;; Remove the symbols from the list where the 'at-loc-p' function evaluates to false
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  ;; Define a local function called 'describe-obj' taking a single argument 'obj'
  (labels ((describe-obj (obj)
	     ;; Use quasiquoting to interpolate the symbol 'obj' into another list of symbols
	     `(you see a ,obj on the floor.)))
    ;; Append each output of mapcar together into a single list
    (apply #'append
	   ;; Apply the function 'describe-obj' to each of the values returned by objects-at
	   (mapcar #'describe-obj
		   ;; Get the objects present at the given location 'loc'
		   (objects-at loc objs obj-loc)))))

(describe-objects 'living-room *objects* *object-locations*)

;; This variable will track the players current location and is initialised to living-room
(defparameter *location* 'living-room)

;; The presence of the global variables *location*, *nodes*, *edges*, *objects* and *object-locations* mean that the 'look' function is not function as it relies on external state
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(look)

(defun walk (direction)
  ;; define a local variable 'next' to hold the location to be walked to
  (let ((next
	  ;; locate the selected direction from the 'edges' associated with 'location' by searching for a matching key as defined below
	  (find direction
		;; Remove the name of the location, leaving only a list of edges
		(cdr
		 ;; Fetch the edges associated with the current location from the 'edges' alist
		 (assoc *location* *edges*))
		;; define the key used by the 'find' function
		:key #'cadr)))
    ;; If the argument supplied to 'direction' was found by the find function above
    (if next
	;; Use progn to execute multiple expressions 
	(progn
	  ;; Set the location to the value provided
	  (setf *location* (car next))
	  ;; Execute the 'look' function at the new location.
	  (look))
	;; Otherwise print 'you cannot go that way'
	'(you cannot go that way))))

*location*

(walk 'west)

;; Keyword parameters
;; special parameters passed at the end of a function call allowing access to built in features
;; Syntax - ':keyword-name keyword-value'

(defun pickup (object)
  ;; If the object argument matches an object at the current location
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 ;; Prepend the given value to the supplied list
	 (push
	  ;; create a new list consisting of the object and 'body' the new location
	  (list object 'body)
	  ;; The list to add the above created list to
	  *object-locations*)
	 ;; Print a message informing the player that they are now carrying the object
	 `(you are now carrying the ,object))
	;; if the object does not match an object at the given location, slap the players hand
	(t '(you cannot get that.))))

;; N.B. Although the push command always adds a new value to the alist, when extracting items with assoc, we always receive the first matching item i.e. the most recently added. So aside from eternally growing larger in size, functionally this works the same as assigning a new value to a dictionary item.

(walk 'east)

(pickup 'whiskey)
