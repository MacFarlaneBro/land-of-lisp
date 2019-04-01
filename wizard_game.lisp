(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
    (member object (cdr (inventory))))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))

;; Return true if the player currently holds the supplied object
(defun have (object)
  (member object (inventory)))

(defparameter *chain-welded* nil)

;; Combine two objects
(defun weld (subject object)
  ;; If all of the given conditions are met
  (if (and (eq *location* 'attic)
	   (eq subject 'chain)
	   (eq object 'bucket)
	   (have 'chain)
	   (have 'bucket)
	   (not *chain-welded*))
      ;; Make 'chain welded true'
      (progn (setf *chain-welded* t)
	     '(the chain is now securely welded to the bucket.))
      ;; Otherwise say you cant
      '(you cannot weld like that.)))

(pushnew 'weld *allowed-commands*)

(setf *bucket-filled* nil)

(defun dunk (subject object)
  ;; If all conditions are met, dunk the bucket
  (if (and (eq *location* 'garden)
	   (eq subject 'bucket)
	   (eq object 'well)
	   (have 'bucket)
	   *chain-welded*)
      (progn (setf *bucket-filled* 't)
	     '(the bucket is now full of water))
      '(you cannot dunk like that.)))

(pushnew 'dunk *allowed-commands*)

(let ((subj (gensym))
      (obj (gensym)))
  (defmacro game-action (command subj obj place &body body)
    ;; create a new function with the 'command' name
    `(progn (defun ,command (subject object)
	      ;; Check the location
	      (if (and (eq *location* ',place)
		       ;; Check the subject
		       (eq subject ',subj)
		       ;; Check the object
		       (eq object ',obj)
		       ;; Check they have the required object
		       (have ',subj))
		  ;; insert action to take when requirements are met
		  ,@body
		  ;; If false, action has failed, print message
		  '(i cant ,command like that.)))
	    ;; Add the command to the list of allowed commands
	    (pushnew ',command *allowed-commands*))))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (progn (setf *chain-welded* t)
	 '(the chain is now securely welded to the bucket.)))

(setf *bucket-filled* nil)

(game-action dunk bucket well garden
  (if (and *chain-welded*
	   (have 'bucket))
      (progn (setf *bucket-filled* 't)
	     '(the bucket is now full of water))
      '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
	((have 'frog) '(the wizard awakens and sees that you stole his frog.
			he is so upset he banishes you to the
			netherworlds- you lose! the end.))
	(t '(the wizard awakens from his slumber and greets you warmly.
	     he hands you the magic low-carb donut- you win! the end.))))
