;; This page is an attempt to get it super clear in my head what the various car and cdr combinations will return on a selection of lists.

(setq triple-nested '(a b c (d e (f))))

(car triple-nested)
;; => A
;; Return the first element

(cdr triple-nested)
;; => (B C (D E (F)))
;; Return everything after the first element

(cdar triple-nested)
;; => ERROR!
;; Tries to get first element of the first element but fails because the first element is a symbol (A) not a list

(cddr triple-nested)
;; => (C (D E (F)))
;; Return the subsequent elemnts of the second element

(cdddr triple-nested)
;; => ((D E (F)))
;; Return the subsequent element of the third element

(cddddr triple-nested)
;; => NIL
;; At this point, as can be seen above, the outermost list contains only one element, this means the cdr of its cons cell must necessarily point to NIL

(cadr triple-nested)
;; => B
;; Return the second element of the list

(caddr triple-nested)
;; => C
;; Return the third element in the list

(cadddr triple-nested)
;; => (D E (F))
;; Return the fourth element in the list
