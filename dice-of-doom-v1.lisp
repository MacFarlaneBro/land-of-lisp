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

;;Attacking

(defun board-attack (board player src dst dice)
  "Determine what happens if the hexagon src attacks the hexagon dst"
  ;; Keep track of the current position while looping across the board
  (board-array (loop for pos
		     ;; Keep track of the hexagon at the current position
		     for hex across board
		     ;; If the current hexagon is the source hexagon then place a single die there
		     collect (cond ((eq pos src) (list player 1))
				   ;; If the current hexagon is the destination then place the remaining dice there subtracting the one left behind
				   ((eq pos dst) (list player (1- dice)))
				   ;; Otherwise just leave the hexagon the way it is
				   (t hex)))))

(board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)

(defun add-new-dice (board player spare-dice)
  "At the end of each round the player receives reinforcements"
  ;; Define temporary function f which takes a list and a number as arguments
  (labels ((f (lst n)
	     ;; If the list is nil returnnil
	     (cond ((null lst) nil)
		   ;;  If the number is zero return the list
		   ((zerop n) lst)
		   ;; Otherwise define current player
		   (t (let ((cur-player (caar lst))
			    ;; And the current dice
			    (cur-dice (cadar lst)))
			;; If the current player is this turns player and the current number of dice is less than the maximum number of dice
			(if (and (eq cur-player player) (< cur-dice *max-dice*))
			    ;; Add a die to the current player
			    (cons (list cur-player (1+ cur-dice))
				  ;; Call the function on the remaineder of the list
				  (f (cdr list)
				     ;; and one minus the number
				     (1- n)))
			    ;; Otherwise
			    ;; Combine the beginning of the list
			    (cons (car lst)
				  ;; With the result of calling the function on the remainder of the lsit and the current numner
				  (f (cdr lst) n))))))))
    ;; Call the locally defined function shown above on the board (after converting it to a list from an array and the number of space
    (board-array (f (coerce board 'list) spare-dice))))

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
