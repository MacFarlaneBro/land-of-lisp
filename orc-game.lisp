(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  ;; Unless the player or all monsters are dead
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    ;; 
    (dotimes
	;; Assign the value to k
	(k
	 ;; Add one
	 (1+
	  ;; Remove any remainder
	  (truncate
	   ;; divide 15 by the previous value
	   (/
	    ;; Get max of player-agility and zero
	    (max 0 *player-agility*) 15))))
      ;; Show the monsters then attack them
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    ;; pirnt a fresh line
    (fresh-line)
    ;; loop over collection
    (map
     ;; return a list
     'list
     ;; loop function
     (lambda (m)
       ;; If monster isn't dead, have it attack
       (or (monster-dead m) (monster-attack m)))
     ;; loop collection
     *monsters*)
    ;; repeat the game loop
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  ;; Switch based on the player entered character
  (case (read)
    ;; If a stab then choose the monster 
    (s (monster-hit (pick-monster)
		    ;; Do damage equal to 2 + a random amount of player strength
		    (+ 2 (randval (ash *player-strength* -1)))))
    ;; If double swing calculate the strength first as rounded strength / 6
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 ;; Chose a monster to hit
	 (monster-hit (pick-monster) x)
	 ;; If the monsters aren't all dead
	 (unless (monsters-dead)
	   ;; Hit another monster
	   (monster-hit (pick-monster) x))))
    ;; Otherwise randomly hit monsters a random number of times
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

(defun randval (n)
  ;; then add one
  (1+
   ;; then choose a random value from 0 to the chosen number
   (random
      ;; Chose a random number between 1 and n 
    (max 1 n))))

(defun random-monster ()
  ;; Get a random monster number
  (let ((m (aref *monsters* (random (length *monsters*)))))
    ;; If the monster isn't dead return it, otherwise recurse.
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  ;; Read the players input
  (let ((x (read)))
    ;; If the input is not a number or a valid monster number
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	;; return the element of the monsters array at index x - 1
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))

(defun init-monsters ()
  ;; Set the returned array to the monsters global
  (setf *monsters*
	;; Loop over argument
	(map
	 ;; Return result as vector type
	 'vector
	 ;; Loop function
	 (lambda (x)
	   ;; Call the function returned by the nth function call below
	   (funcall
	    ;; return a value from first argument, using second argument as an index
	    (nth
	     ;; First argument
	     ;; Get a random index from monster-builders
	     (random (length *monster-builders*))
	     ;; Second argument
	     *monster-builders*)))
	 ;; Create an array with length 'monster-num'
	 (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  ;; Apply the function monster dead to all the monsters, return true if every monster is dead
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    ;; Create a list of each argumnt in monsters
    (map 'list
	 ;; loop function
	 (lambda (m)
	   (fresh-line)
	   (princ "  ")
	   ;; print and Increment x
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval 10)))

(make-monster)

(defmethod monster-hit (m x)
  ;; Decrement the health of monster m by value x
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
	     (princ (type-of m))
	     (princ "! "))
      (progn (princ "You hit the ")
	     (princ (type-of m))
	     (princ ", knocking off ")
	     (princ x)
	     (princ " health points! "))))

;; Get the type of the argument
(type-of 'foo)

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

;; Define an orc structure
(defstruct (orc
	    ;; Inherit from monster
	    (:include monster))
  (club-level (randval 8)))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))

(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off ")
	     (princ x)
	     (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of it's heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
	   (decf *player-agility* 2))
	  ((= x *player-strength* )
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength* 2)))))

