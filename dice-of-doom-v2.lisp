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

(play-vs-human (game-tree (gen-board) 0 0 t))
