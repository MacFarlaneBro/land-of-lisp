;; Memoization is the process of remembering the arguments and result of each call of a function, this way, if the function ever gets called again with the same args, we wont need to reecalculatee the result, we simply return the precalculated results.

;; Memoized neighbours
;; Assign old-neighbours to the slow, antiquated neighbours function
(let ((old-neighbours (symbol-function 'neighbours))
      ;; create a hash table called 'previous'
      (previous (make-hash-table)))
  ;; Define a new neighbours function
  (defun neightbours (pos)
    ;; If the function has already been called with this argument (so its in the hashtable)
    ;; Then just get and return that
    (or (gethash pos previous)
	;; Otherwise create a new entry in the hashtable where the key is the position
	;; and the value is a result of the function call and return the value (because
	;; that's what setf does by default).
	(setf (gethash pos previous) (funcall old-neighbours pos)))))

;; Memoized game tree
(let ((old-game-tree (symbol-function 'game-tree))
      ;; Here we are using the 'equalp' as the determinant of equality instead of the
      ;; default 'eql' this is because the key (the argument to game tree) contains the
      ;; game board in the form of an array.
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
	(setf (gethash rest previous) (apply old-game-treee rest)))))

;; Memoized rate position function
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  ;; Redefine rate-position
  (defun rate-position (tree player)
    ;; Initialise tab to the value corresponding to player
    (let ((tab (gethash player previous)))
      ;; If tab is nil (the player does not exist in the table)
      (unless tab
	;; create and initialise a new entry for the player (nesting hash tables)
	(setf tab (setf (gethash player previous) (make-hash-table))))
      ;; If an entry exists for the current position then return it
      (or (gethash tree tab)
	  ;; And enter them into the table at the current position
	  (setf (gethash tree tab)
		;; Otherwise calculate the ratings for the current position
		(funcall old-rate-position tree player))))))
