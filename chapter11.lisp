(format
 ;; Destination of the formatted string, t for print to console, nil to return the string or stream to output to stream
 t
 ;; The message to be printed
 "Add onion rings for only ~$ dollars more!"
 ;; The value to interpolate
 1.5)

;; All control sequences used by format begin with the tilde (~) character

(prin1 "foo")

(princ "foo")

;; Include "" delimiters
(format t "I am printing ~s in the middle of this sentence." "foo")

;; Exclude "" delimiters
(format t "I am printing ~a in the middle of this sentence." "foo")

;; Add padding spaces on the right by using a numerical value
(format t "I am printing ~10a within ten spaces of room." "foo")

;; Add padding spaces on the left by using a numerical value + @
(format t "I am printing ~10@a within ten spaces of room." "foo")

;; Add padding spaces in groups of 3, until 10 or more spaces is reached
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")

;; Pass only the third formatting argument, leaving the first two blank
(format t "I am printing ~,,4a in the middle of this sentence." "foo")

;; Pad the string with the specified value
(format t "The word ~,,4,'!a feels very important" "foo")

;; Here we combine the above formatting with the previous @ command to place the !'s before the interpolated foo
(format t "The word ~,,4,'!@a feels very important" "foo")

;; Display a number in hexadecimal format
(format t "The number 1000 in hexadecimal is ~x" 1000)

;; Display a number in binary format
(format t "The number 1000 in binary is ~b" 1000)

;; Display a number in decimal format
(format t "The number 1000 in decimal is ~d" 1000)

;; Display numbers correctly formatted
(format t "Numbers with commas in them are ~:d times better." 1000000)

;; Add padding
(format t "I am printing ~10d within ten spaces of room." 1000000)

;; Add padding of x's
(format t "I am printing ~10,'xd within ten spaces of room." 1000000)

;; round to four characters
(format t "PI can be estimated as ~4f" 3.14159)

;; Round to 4 decimal points
(format t "PI can be estimated as ~,4f" 3.14159)

;; Pi is actually included in common lisp as standard
(format t "PI can be estimated as ~,4f" pi)

;; Scale the interpolated argument by factors of 10
(format t "Percentages are ~,,2f percent better then fractions" 0.77)

;; Format currencies correcly
(format t "I wish I had ~$ pounds in my bank account." 1000000.2)

(progn (princ 22)
       ;; Finish the current line and start a new one 
       (terpri)
       (princ 33))

(progn (princ 22)
       ;; Add a new line unless we're on a totally empty line
       (fresh-line)
       (princ 33))

(progn (princ 22)
       ;; Add a new line unless we're on a totally empty line
       (fresh-line)
       ;; This has no effect!
       (fresh-line)
       (princ 33))

;; % is the format equivalent of terpri
(progn (format t "this is on one line ~%")
       (format t "~% this is on another line"))

;; & is the format equivalent of fresh-line
(progn (format t "this is on one line ~&")
       (format t "~& this is on another line"))

;; A numerical argument can be appended to dictate the number of newlines added
(format t "this will print ~5%on two lines spread far apart")

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(random-animal)

;; Do 10 times
(loop repeat 10
      ;; Print a random animal at the 5th, 15th and 25th column
      do (format t "~5t~a ~15t~a ~25t~a~%" 
		 (random-animal)
		 (random-animal)
		 (random-animal)))

(loop repeat 10
      ;; Space the animals equally apart on a single line
      do (format t "~30<~a~;~a~;~a~>~%"
		 (random-animal)
		 (random-animal)
		 (random-animal)))
;; ~30< - start justifying (30 spaces wide)
;; ~a - insert value
;; ~; - start new item to justify
;; ~> - stop justiying
;; ~% - insert newline

;; Create a single, neatly centered column
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

(loop repeat 10
      ;; use :@ to justify multiple values on a line
      do (format t "~30:@<~a~;~a~;~a~>~%"
		 (random-animal)
		 (random-animal)
		 (random-animal))) 

(loop repeat 10
      ;; Print three columns, each of which is justified in independent 10 char segments
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
		 (random-animal)
		 (random-animal)
		 (random-animal)))


(defparameter *animals* (loop repeat 10 collect (random-animal)))

;; Use ~{ and ~} to create loops through lists within strings
(format t "~{I see a ~a! ~}" *animals*)

;; A single iteration can grab more than one element from the loop at a time
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

;; Create justified a table of the integers from 0 to 99
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
