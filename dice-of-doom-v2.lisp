(load "dice-of-doom-v1.lisp")
(load "lazy_loading.lisp")

(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun add-passing-move (board player spare-dice first-move moves)
  ;; If this is the first move
  (if first-move
      ;; return it
      move
      ;; Otherwise cons together
      (lazy-cons (list nil
		       ;; The remainder of the game tree
		       (game-tree (add-new-dice board player
						(1- spare-dice))
				  (mod (1+ player) *num-players*)
				  0
				  t))
		 moves)))


(defun attacking-moves (board cur-player spare-dice)
  ;; Create a 'player function' which gets the player at the current position
  (labels ((player (pos)
	     (car (aref board pos)))
	   ;; Create a dice function which gets the number of dice at the current position
	   (dice (pos)
	     (cadr (aref board pos))))
    ;; Lazy generate a list
    (lazy-mapcan
     ;; src is the current board hex
     (lambda (src)
       ;; If the current player is the same as the player of the current hex
       (if (eq (player src) cur-player)
	   ;; Generate another list (lazily)
	   (lazy-mapcan
	    ;; dst is the neighbor of the 'src' hex
	    (lambda (dst)
	      
	      (if (and
		   ;; If the neighbor square isn't owned by the current player
		   (not (eq (player dst)
			    cur-player))
		   ;; And has fewer dice than the current square
		   (> (dice src) (dice dst)))
		  ;; Make a lazy list
		  (make-lazy
		   (list (list (list src dst)
			       ;; Generate the game tree showing the attacking move for the current neighbor
			       (game-tree (board-attack board
							cur-player
							src
							dst
							(dice src))
					  cur-player
					  (+ spare-dice (dice dst))
					  nil))))
		  ;; Otherwise return nil
		  (lazy-nil)))
	    ;; Get all of the neighbors of the current hex to feed this mapcan
	    (make-lazy (neighbors src)))
	   (lazy-nil)))
     ;; Get the id of each board hex to feat the lazy mapcan
     (make-lazy (loop for n below *board-hexnum*
		      collect n)))
    ))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  ;; set moves to available set of moves on the tree
  (let ((moves (caddr tree)))
    ;; Define function 'print moves'
    (labels ((print-moves (moves n)
	       ;; Unless there are no moves
	       (unless (lazy-null moves)
		 ;; set 'move' to the first item ove moves
		 (let* ((move (lazy-car moves))
			;; set action the to the action element of move
			(action (car move)))
		   (fresh-line)
		   ;; Print the current move number
		   (format t "~a. " n)
		   ;; If action is non-nil
		   (if action
		       ;; Print the move
		       (format t "~a -> ~a" (car action) (cadr action))
		       ;; Otherwise print 'end-turn'
		       (princ "end turn")))
		 ;; Recursively print all the moves
		 (print-moves (lazy-cdr moves) (1+ n)))))
      ;; Start the move printing funtion
      (print-moves moves 1))
    (fresh-line)
    ;; Apply the move chosen by the human
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  ;; Print the current tree info
  (print-info tree)
  ;; ther are moves left in the tree
  (if (not (lazy-null (caddr tree)))
      ;; play the next round
      (play-vs-human (handle-human tree))
      ;; otherwise announc the winner
      (announce-winner (cadr tree))))

(defun limit-tree-depth (tree depth)
  ;; Create a list 
  (list
   ;; Get the first element of the current tree
   (car tree)
   ;; And the next element
   (cadr tree)
   ;; If the depth checker is now zero
   (if (zerop depth)
       ;; return nil
       (lazy-nil)
       ;; Otherwise for each remaining item
       (lazy-mapcar (lambda (move)
		      ;; Get the first element
		      (list (car move)
			    ;; Recurse with the remainer of the tree and one less the depth
			    (limit-tree-depth (cadr move) (1- depth))))
		    ;; In the rest of the tree
		    (caddr tree)))))


(defparameter *ai-level* 4)
(defun handle-computer (tree)
  ;; Set 'ratings' to calling get ratings on the result of limiting the tree depth to the ai-level
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
			      ;; and the first element of tree
			      (car tree))))
    ;; Get the first element of the remainder of
    (cadr
     ;; Get nth item
     (lazy-nth
      ;; Get the index of the highest rated move
      (position (apply #'max ratings) ratings)
      ;; From the available moves
      (caddr tree)))))

(defun play-vs-computer (tree)
  ;; Print the tree
  (print-info tree)
  ;; If the remainder of the tree is null, announce the inner
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
	;; If the first item is zero, it's the humans turn
	((zerop (car tree)) (play-vs-computer (handle-human tree)))
	;; Otherwise it's the computers turn
	(t (play-vs-computer (handle-computer tree)))))

(defun score-board (board player)
  ;; For each hex in the board
  (loop for hex across board
	;; For each possible move
	for pos from 0
	;; Add 
	sum
	;; If the the current hex is the players
	(if (eq (car hex) player)
	    ;; If the current hex is threatened
	    (if (threatened pos board)
		;; return 1 
		1
		;; Otherwiser return 2
		2)
	    ;; If the hex is not owned by the current player, return -1
	    -1)))

(defun threatened (pos board)
  ;; set hex to the current hex
  (let* ((hex (aref board pos))
	 ;; get the current hex player
	 (player (car hex))
	 ;; get the current hex dice
	 (dice (cadr hex)))
    ;; For each neighbour of the current hex
    (loop for n in (neighbors pos)
	  ;; Get the neighbouring hex
	  do (let* ((nhex (aref board n))
		    ;; player
		    (nplayer (car nhex))
		    ;; and dice
		    (ndice (cadr nhex)))
	       ;; When the hex is owned by another player and has more dice
	       (when (and (not (eq player nplayer)) (> ndice dice))
		 ;; return true
		 (return t))))))

(defun get-ratings (tree player)
  ;; Fully evalate lazy list
  (take-all (lazy-mapcar (lambda (move)
			   ;; Get the rating for every move
			   (rate-postition (cadr move) player))
			 (caddr tree))))

(defun rate-position (tree player)
  ;; Get available moves
  (let ((moves (caddr tree)))
    ;; If there are moves remaining
    (if (not (lazy-null moves))
	;; If the current player owns the current first element
	(apply (if (eq (car tree) player)
		   ;; return max function
		   #'max
		   ;; Otherwise return min function
		   #'min)
	       ;; get the ratings for each move
	       (get-ratings tree player))
	;; otherwise score the available moves
	(score-board (cadr tree) player))))


(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  ;; Create function f which takes 2 args, moves and  lower limit
  (labels ((f (moves lower-limit)
	     ;; unless moves is empty
	     (unless (lazy-null moves)
	       ;; set 'x' to the result of calling rate position on the first element in moves
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  ;; passing in the player, upper & lower limit
					  player
					  upper-limit
					  lower-limit)))
		 ;; If x is greater than the upper limit
		 (if (>= x upper-limit)
		     ;; return a list of only x
		     (list x)
		     ;; Otherwise return the cons of x and the result of recursing the function
		     (cons x (f
			      ;; passing in the remainder of moves
			      (lazy-cdr moves)
			      ;; the max of either x or the lower-limit as lower-limit
			      (max x lower-limit))))))))
    ;; Start the recursing function
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lowerlimit)
  (labels ((f (moves upper-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (<= x lower-limit)
		     (list x)
		     (cons x (f lazy-cdr moves) (min x upper-limit)))))))
    (f (caddr tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  ;; Get the current moves possible 
  (let ((moves (caddr tree)))
    ;; If there are moves remaining
    (if (not (lazy-null moves))
	;; If it's the current players hex
	(if (eq (car tree) player)
	    ;; Get the max rating
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit))
	    ;; Get the min rating
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit)))
	;; print the score board
	(score-board (cadr tree) player))))

(defun handle-computer (tree)
  ;; Set the ratings
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
				     (car tree)
				     most-positive-fixnum
				     most-negative-fixnum)))
    ;; Execute the best current move
    (cadr
     (lazy-nth
      ;; Get the max rated position
      (position (apply #'max ratings) ratings)
      (caddr tree)))))

(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(play-vs-computer (game-tree (gen-board) 0 0 t))
