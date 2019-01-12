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

(defun eat (animal)
  ;; a cons consisting of the x and y locations of the animal
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    ;; Use that cons to get the energy of the plant at the animals location
    (when (gethash pos *plants*)
      ;; Increase the animals energy by the plants energy
      (incf (animal-energy animal) *plant-energy*)
      ;; Remove the plant from the plants hash table
      (remhash pos *plants*))))

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  ;; Assign animal energy to local var e
  (let ((e (animal-energy animal)))
    ;; If e is greater than or equal to reproduction energy
    (when (>= e *reproduction-energy*)
      ;; reduce the animals energy by half
      (setf (animal-energy animal) (ash e -1))
      ;; copy out the animal
      (let ((animal-nu (copy-structure animal))
	    ;; copy the animals genes
	    (genes     (copy-list (animal-genes animal)))
	    ;; assign a random number between 1 and 8 to mutation
	    (mutation (random 8)))
	;; Get the gene index referenced by 'mutation and set it
	(setf (nth mutation genes)
	      ;; to either one or it's current value + random (0-3) + -1
	      (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))


(defun update-world ()
  ;; Remove all the animals with no energy left
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0))
			     *animals*))
  ;; Make all the animals turn, move, eat and reproduce
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  ;; Add some plants
  (add-plants))

(defun draw-world ()
  ;; For each y index tick
  (loop for y
	  below *height*
	;; Start a new line
	do (progn (fresh-line)
		  ;; Print a pipe at the beginning of the line
		  (princ "|")
		  ;; For each x index tick
		  (loop for x
			  below *width*
			;; If there's an animal at this locatino print the letter m
			do (princ (cond ((some (lambda (animal)
						 (and (= (animal-x animal) x)
						      (= (animal-y animal) y)))
					       *animals*)
					 #\M)
					;; If their's a plant, print an asterisk
					((gethash (cons x y) *plants*) #\*)
					;; Otherwise print a space
					(t #\space))))
		  ;; Print a pipe at the end of the line
		  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    ;; Unless teh user entered quit
    (cond ((equal str "quit") ())
	  ;; Run the simulation for x number of times
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
			   below x
			 do (update-world)
			    ;; print a dot every thousand days
			 if (zerop (mod i 1000))
			   do (princ #\.))
		   (update-world))
	       ;; Rerun the function
	       (evolution))))))
