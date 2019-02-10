(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;; Clean, functional code

(defun board-array (lst)
  ;; Create a new array containing the contents of lst
  (make-array *board-hexnum* :initial-contents lst))

(defun game-tree (board player spare-dice first-move)
  ;; put the player, board and passing move in a list and return it
  (list player
	board
	(add-passing-move board
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  ;; If it's the players first move
  (if first-move
      ;; Return moves
      moves
      ;; otherwise cons togher the moves
      (cons
       ;; and a list starting with nil
       (list nil
	     ;; Then containing the remaining game tree
	     (game-tree
	      ;; The current state of teh board
	      (add-new-dice board player (1- space-dice))
	      ;; The player
	      (mod (1- player) *num-players*)
	      ;; The spare dice
	      0
	      ;; whether it's the first move (it is, because first-move it true)
	      t))
       moves)))

(defun attacking-moves (board cur-player spare-dice)
  ;; Define the new functions 'player' taking the argument 'pos' and 'dice' taking the remainder of pos from array board.
  (labels ((player (pos)
	     ;; Get the first element from index 'pos' of array 'board'
	     (car (aref board pos)))
	   ;; get the remainder of index pos from array board
	   (dice (pos)
	     (cadr (aref board pos))))
    ;; apply the supplied function to each element in the sequence, concatenating the results of each operation together
    (mapcan (lambda (src)
	      ;; If the current player in the sequence is the current player
	      (when (eq (player src) cur-player)
		;; For each
		(mapcan (lambda (dst)
			  ;; If The current player is not the destination player
			  (when (and (not (eq (player dst) cur-player))
				     ;; and the the number of dice at the current location is greater than the number of dice at the destination location
				     (> (dice src) (dice dst)))
			    (list
			     (list
			      ;; Create a list of the source and the destination
			      (list src dst)
			      ;; call the 'game-tree' function using the 'board-attack' move
			      (game-tree (board-attack board cur-player src dst (dice src))
					 ;; The current player
					 cur-player
					 ;; Add the number of space dice to the number of dice at the attacked destination
					 (+ spare-dice (dice dst))
					 ;; Assert that this isn't the first turn
					 nil)))))
			;; Apply the above function to all neighbors of the current src location
			(neighbors src))))
	    ;; Apply the above function for every hex on the board.
	    (loop for n below *board-hexnum*
	       collect n))))

(defun neighbors (pos)
  ;; Define up as the users current position - the board size
  (let ((up (- pos *board-size*))
	;; Define down as the users current position plus the board size
	(down (+ pos *board-size*)))
    ;; For each item in 
    (loop for p in (append
		    ;; Create a list of the up and down variables and append it to
		    (list up down)
		    ;; if the position is not equal to board size
		    (unless (zerop (mod pos *board-size*))
		      ;; move position to up -1 and position - 1
		      (list (1- up) (1- pos)))
		    ;; if the board size is one more than the current position
		    (unless (zerop (mod (1+ pos) *board-size*))
				   ;; move position to up +1 and position + 1
				   (list (1+ pos) (1+ down))))
		    ;; When the current iteration element is not negative 
		    when (and (>= p 0)
			      ;; and is less than the board size
			      (< p *board-hexnum*))
		    ;; add p to the current list being generated
		    collect p)))

(neighbors 2)
;; Dirty, imperative code

(defun gen-board ()
  ;; Call the board array function
  (board-array
   ;; For each n in board-hexnum
   (loop for n below *board-hexnum*
	 ;; Create a list of
	 collect
	 ;; Create a list consisting of a player number
	 (list (random *num-players*)
	       ;; And 1 + a number between zero and the max number of dice per hexagon
	       (1+ (random *max-dice*))))))

(gen-board)

(defun player-letter (n)
  (code-char (+ 97 n)))

(player-letter 1)

(defun draw-board (board)
  ;; For each number below board size
  (loop for y below *board-size*
	;; Execute the following sequence
	do (progn
	     ;; print a fresh line
	     (fresh-line)
	     ;; while board size - y is not zero 
	     (loop repeat (- *board-size* y)
		   ;; print a space
		   do (princ "  "))
	     ;; while x is below board size
	     (loop for x below *board-size*
		   ;; set hex equal to the value of the hex at teh x and y coordinates provided
		   for hex = (aref board (+ x (* *board-size* y)))
		   ;; print the letter of the player occupying the current hex
		   do (format t "~a-~a " (player-letter (first hex))
			      ;; print the number of dice on the hex
			      (second hex))))))

(draw-board (gen-board))
