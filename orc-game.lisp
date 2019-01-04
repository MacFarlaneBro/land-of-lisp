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
  <= *player-health* 0)

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
