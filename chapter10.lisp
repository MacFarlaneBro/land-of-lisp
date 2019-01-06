;; The loop macro
(loop for i
	below 5
      sum i)

;; => 10

;; Loop tricks

;; Counting from a starting point to an ending point
(loop for i
      from 5
	to 10
      sum i)
;; => 45

;; Iterating through a list
(loop for i
	in '(100 20 3)
      sum i)
;; => 123

;; Doing stuff in a loop
(loop for i
	below 5
      do (print i))
;; => 0 1 2 3 4

;; Doing stuff under certain conditions
(loop for i
	below 10
      when (oddp i)
	sum i)
;; => 25
;; N.B. This may be the first thing I've evaluated which seems to be interpreted completely differently by the emacs lisp interpreter

;; Breaking out of a loop early
(loop for i
      from 0
      do (print i)
      when (= i 5)
	return 'falafel)

;; => 0 1 2 3 4 FALAFEL

;; Collecting a list of values
(loop for i
	in '(2 3 4 5 6)
      collect (* i i))
;; => (4 9 16 25 36)

;; Using multiple for clauses
(loop for x below 10
      for y below 10
      collect (+ x y))
