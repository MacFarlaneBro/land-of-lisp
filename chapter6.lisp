(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string 
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  ;; If the value of 'lst' is not nil
  (when lst
    ;; Assign the first item from the list to 'item'
    (let ((item (car lst))
	  ;; assign the remainder of the list to 'rest'
	  (rest (cdr lst)))
      
      (cond
	;; If the character in 'item' is a space character
	((eql item #\space)
	 ;; rejoin item in place -
	 (cons item
	       ;; after re-calling tweak-text on the remainder of the list using the same values as called in the current instance
	       (tweak-text rest caps lit)))
	;; If the character in 'item' is a sentence ender
	((member item '(#\! #\? #\.))
	 ;; rejoin item in place -
	 (cons item
	       ;; after re-calling tweak-text on the remainder but this time passing the 'caps' boolean as true.
	       (tweak-text rest t lit)))
	;; If the item is a quotation mark
	((eql item #\")
	 ;; call tweak-text on the remainder but invert boolean 'lit'
	 ;; This means 'lit' will remain active until another quotation mark is detected
	 (tweak-text rest caps (not lit)))
	;; If 'lit' is active 
	(lit
	 ;; Add the next item
	 (cons item
	       ;; but disable manual capitalisation handling
	       (tweak-text rest nil lit)))
	;; If 'caps' is active
	(caps
	 ;; Add the next item after first converting it to upper-case
	 (cons (char-upcase item)
	       ;; Then make sure the next letter is not capitalised
	       (tweak-text rest nil lit)))
	;; default case
	(t
	 ;; make sure the item is lowercase
	 (cons (char-downcase item)
	       ;; falsify the caps and lit variables.
	       (tweak-text rest nil nil)))))))
