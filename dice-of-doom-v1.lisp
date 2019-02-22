(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 3)
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
	      (add-new-dice board player (1- spare-dice))
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
				  (f (cdr lst)
				     ;; and one minus the number
				     (1- n)))
			    ;; Otherwise
			    ;; Combine the beginning of the list
			    (cons (car lst)
				  ;; With the result of calling the function on the remainder of the lsit and the current numner
				  (f (cdr lst) n))))))))
    ;; Call the locally defined function shown above on the board (after converting it to a list from an array and the number of space
    (board-array (f (coerce board 'list) spare-dice))))

(defun winners (board)
  ;; Define tally as the car of all the hexes on the board
  ;; Use across loop construct to directly traverse array
  (let* ((tally (loop for hex across board
		      collect (car hex)))
	 ;; Define totals as The number of hexes each player occupies
	 (totals (mapcar (lambda (player)
			   ;; Create an alist of players and the number of occurences of player in tally
			   (cons player (count player tally)))
			 ;; Remove any duplicates from tally before passing them into the above lambda
			 (remove-duplicates tally)))
	 ;; Define best as the player with the highest cdr value in the above totals
	 (best (apply #'max (mapcar #'cdr totals))))
    ;; For each element in totals pass in the player (car)
    (mapcar #'car
	    ;; remove it if their score is not equal to the best calculated score
	    (remove-if (lambda (x)
			 (not (eq (cdr x) best)))
		       ;; Pass in each of the elements in totals
		       totals))))

(defun announce-winner (board)
  (fresh-line)
  ;; Set w to the result of the winners function
  (let ((w (winners board)))
    ;; If there is more than one winner
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w))))))

(defun rate-position (tree player)
  ;; Assign moves to all possible remaining moves
  (let ((moves (caddr tree)))
    ;; While moves is not nil
    (if moves
	;; If the player on this node of the tree is the curreent player then get the
	(apply (if (eq (car tree) player)
		   ;; highest rated move
		   #'max
		   ;; Otherwise it's the other player so get the lowest rated move
		   #'min)
	       ;; a list of all the possible moves and their quality ratings
	       (get-ratings tree player))
	;; If moves is nil then whoever is at the end of the tree is thee winner
	(let ((w (winners (cadr tree))))
	  ;; If the current player is in the winners list
	  (if (member player w)
	      ;; Return 1 divided by the number of winners in the list
	      (/ 1 (length w))
	      ;; Otherwise return 0
	      0)))))

(defun get-ratings (tree player)
  ;; For each move in the tree
  (mapcar (lambda (move)
	    ;; get the rating for this player
	    (rate-position (cadr move) player))
	  (caddr tree)))

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

(defun play-vs-human (tree)
  ;; Print some info about the current state of the tree
  (print-info tree)
  ;; If there are moves remaining
  (if (caddr tree)
      ;; Recurse the function on the current tree
      (play-vs-human (handle-human tree))
      ;; Announce the winner 
      (announce-winner (cadr tree))))

(defun print-info (tree)
  ;; Print a new line
  (fresh-line)
  ;; Print the name of the current player 
  (format t "current player = ~a" (player-letter (car tree)))
  ;; Draw the current board
  (draw-board (cadr tree)))

(defun handle-human (tree)
  ;; Print a new line
  (fresh-line)
  (princ "choose your move: ")
  ;; Assign the moves available to the variable moves
  (let ((moves (caddr tree)))
    ;; For each of the available moves
    (loop for move in moves
	  ;; For each element in the list
	  for n from 1
	  ;; Assign the current move to 'action'
	  do (let ((action (car move)))
	       (fresh-line)
	       ;; Print the move number
	       (format t "~a. " n)
	       ;; If an action exists
	       (if action
		   ;; Print the action available
		   (format t "~a. -> ~a" (car action) (cadr action))
		   ;; Otherwise end the turn
		   (princ "end turn"))))
    (fresh-line)
    ;; get the remainder of the first item 
    (cadr
     ;; Return the element chosen by the player
     (nth
      ;; Deduct 1 first as we've increment all the choices by 1 to make them more human readable.
      (1- (read)) moves))))

(defun handle-computer (tree)
  ;; Assign the result of calling get ratings for the computer
  (let ((ratings (get-ratings tree (car tree))))
    ;; For the resulting tree get the remainder of the first item
    (cadr
     ;; Get the item at the max rated position
     (nth (position (apply #'max ratings) ratings)
	  ;; get the item
	  (caddr tree)))))

(defun play-vs-computer (tree)
  ;; Print the tree
  (print-info tree)
  (cond
    ;; If the remaining tree is null announce the winner
    ((null (caddr tree)) (announce-winner (cadr tree)))
    ;; If it's player 0's go then execute the human go subroutine
    ((zerop (car tree)) (play-vs-computer (handle-human tree)))
    ;; If it's the computers go then execute the computers subroutine
    (t (play-vs-computer (handle-computer tree)))))

(defun add-new-dice (board player spare-dice)
  ;; Create a new function 'f' taking a list, number and accumulator as arguments
  (labels ((f (lst n acc)
	     ;; If n is zero
	     (cond ((zerop n)
		    ;; Append the reversed value of the accumulator to the list
		    (append (reverse acc) lst))
		   ;; If the list is null
		   ((null lst)
		    ;; Return the reversed value of the accumulator
		    (reverse acc))
		   ;; Otherwise
		   (t
		    ;; Assign a current player variable
		    (let ((cur-player (caar lst))
			  ;; Assign a current dice variable
			  (cur-dice (cadar lst)))
		      ;; If both the current player is the player calling the function
		      (if (and (eq cur-player player)
			       ;; And the current number of dice is less than the max
			       ;; number of dice.
			       (< cur-dice *max-dice*))
			  ;; Call the locally defined function
			  (f
			   ;; with the remainder of the list
			   (cdr lst)
			   ;; with 1 less than the supplied n
			   (1- n)
			   ;; and with the combination
			   (cons
			    ;; Of the current player and one + the current dice 
			    (list cur-player (1+ cur-dice))
			    ;; And the current value of the accumulator
			    acc))
			  ;; Otherwise call the function
			  (f
			   ;; with the remainder of the list
			   (cdr lst)
			   ;; The current value of n
			   n
			   ;; And the first value of the list combined with the accumulator
			   (cons (car lst) acc))))))))
    ;; Pass the result of the function to the board array setting fucntion
    (board-array
     ;; Call the function initially
     (f
      ;; with the board coerced to a list 
      (coerce board 'list)
      ;; All of the spare dicee
      spare-dice
      ;; And an empty list for the accumulator
      ()))))
