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
