
;; some test data to test this with
(defparameter *wizard-nodes* '((living-room (you are in the living room.
                                a wizard is snoring loudly on the couch.))
                              (garden (you are in a beautiful garden.
                                there is a well in front of you.))
                              (attic (you are in the attic.
                                there is a giant weldong torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                              (garden (living-room east door))
                              (attic (living-room downstairs ladder))))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
          (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
          s))
  ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ"\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ"\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  ;; A thunk is a function without arguments
  (with-open-file
      ;; This behaves like a python with statement
      (*standard-output*
       ;; A special global variable in common lisp which controls the default location to which printing
       ;; functions send their output. Combined with the above 'with' statement this redirects standard
       ;; output to point at the file instead of the console.
       fname
       ;; The filename
       :direction :output
       ;; write to the file
       :if-exists :supersede)
    ;; If the file exists, overwrite it
    (funcall thunk))
  ;; Call the function defined in 'thunk'
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  ;; Call 'dot->png' using the function name supplied and generating a thunk from
  ;; graph->dot
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun uedges->dot (edges)
  ;; Map anonymous function to sublists of list
  (maplist (lambda (lst)
	     ;; Apply anonymous function to each elemnet of list for side effects only
	     (mapc (lambda (edge)
		     ;; Unless we're at the current edge in the list
		     (unless (assoc (car edge) (cdr lst)) 
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   ;;  print the last element
		   (cdar lst)))
	   ;; list to apply previous anon function to
	   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges ))))
