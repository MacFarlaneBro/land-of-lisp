(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  ;; Initialise variable pos
  (let ((pos
	  ;; Create a new cons cell containing a random height and width for the plant
	  (cons (+ left (random width)) (+ top (random height)))))
    ;; put the position of the plant in the plants hash table
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  ;; Create a plant in the jungle
  (apply #'random-plant *jungle*)
  ;; Create a plant anywhere in the map
  (random-plant 0 0 *width* *height*))

(defstruct animal x y energy dir genes)

(defparameter *animals*
  ;; ash shifts the bits of arg1 to the left by arg2 times, thus the function below effectively halves the width value
  (list (make-animal :x    (ash *width*  -1)
		     :y    (ash *height* -1)
		     :energy 1000
		     :dir  0
		     ;; Generate a list of 8 genes with a value between 0 and 10
		     :genes (loop repeat 8
				  collecting (1+ (random 10))))))

(defun move (animal)
  ;; Capture the animals direction and coordinates in local variables
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    ;; Change the animals x coordinate
    (setf (animal-x animal)
	  ;; Get the remainder of the calculation and the width of the map
	  ;; this allows wrapping of the map
	  (mod
	   ;; Add the current x value
	   (+ x
	      ;; If direction is greater than 1 and less than 5 move right one space
	      (cond ((and (>= dir 2) (< dir 5)) 1)
		    ;; If the direction is 1 or 5 don't move on the x axis
		    ((or (= dir 1) (= dir 5)) 0)
		    ;; Otherwise move left one space
		    (t -1))
	      ;; Add to width
	      *width*)
	   *width*))
    ;; Change the animals y coordinate
    (setf (animal-y animal) (mod (+ y
				    
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0))
				    *height*)
				 *height*))
    ;; Decrease the animals energy
    (decf (animal-energy animal))))

(defun turn (animal)
  ;; Set x to a random value between 0 and the sum of all the animals genes
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;; Define function angle
    (labels ((angle (genes x)
	       ;; Set xnu to x - the value of the first gene on the current list
	       (let ((xnu (- x (car genes))))
		 ;; If xnu is less than 0 return 0
		 (if (< xnu 0)
		     0
		     ;; Otherwise recursively call angle on all remaining list items and add 1 to the result
		     (1+ (angle (cdr genes) xnu))))))
      ;; Set the animals direction
      (setf (animal-dir animal)
	    ;; calculate the direction from the current direction and the angle function defined above
	    ;; mod 8 to keep it 0-7
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
		 8)))))
