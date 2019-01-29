(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;; Clean, functional code

(defun board-array (lst)
  ;; Create a new array containing the contents of lst
  (make-array *board-hexnum* :initial-contents lst))

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
