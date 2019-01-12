(defun robots ()
  ;; Naming the loop main allows use of 'return-from' for early exit
  (loop named main
	;; Define the direction keys
	with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
			    (d .   1) (z .  63) (x .  64) (c . 65))
	;; Define the starting position
	for pos = 544
	;; Print the instructions to the screen
	  then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
		      (force-output)
		      ;; Read the players entry
		      (let* ((c (read))
			     ;; get the associated direction or nil if there is no match
			     (d (assoc c directions)))
			;; If there was a direction get the associated value from teh directions alist
			(cond (d (+ pos (cdr d)))
			      ;; Otherwise if the player selected teleport, choose a random location
			      ((eq 't c) (random 1024))
			      ;; If the player selected leave the exit the game
			      ((eq 'l c) (return-from main 'bye))
			      ;; Otherwise choose the current position
			      (t pos))))
	;; Create a random location for each fo the 10 robots
	for monsters = (loop repeat 10
			     collect (random 1024))
	;; for each robot location
	  then (loop for mpos in monsters
		     ;; If their is more than one robot
		     collect (if (> (count mpos monsters) 1)
				 ;; return it
				 mpos
				 ;; Otherwise
				 (cdar
				  ;; Sort locations based on distance to the player
				  (sort (loop for (k . d) in directions
					      for new-mpos = (+ mpos d)
					      ;; calculate manhattan distance to the player
					      collect (cons (+ (abs (- (mod new-mpos 64)
								       (mod pos 64)))
							       (abs (- (ash new-mpos -6)
								       (ash pos -6))))
							    new-mpos))
					;; The sort predicate
					'<
					;; The sort key
					:key #'car))))
	
	when (loop for mpos in monsters
		   ;; When there are no monsters left the playher has won
		   always (> (count mpos monsters) 1))
	  return 'player-wins
	;; Otherwise
	do (format t
		   ;; print the map
		   "~%|~{~<|~%|~,65:;~A~>~}|"
		   (loop for p
			   below 1024
			 ;; If the current square has a monster
			 collect (cond ((member p monsters)
					;; If it also has the player, game over, the player loses
					(cond ((= p pos) (return-from main 'player-loses))
					      ;; If the square has more than one monster, signify an explosion
					      ((> (count p monsters) 1) #\#)
					      ;; Otherwise print a representation of the monsters location
					      (t #\A)))
				       ;; If the current square has the player, print the player representation
				       ((= p pos)
					#\@)
				       ;; Otherwise just print a space
				       (t
					#\ ))))))
