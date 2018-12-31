;; Load the previously defined graph utils
(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  ;; Get a random node number
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  ;; Unless a and b are the same edge connect them together
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  ;; For edge-num number of times create an edge and append it to a list
  ;; then return the created list
  (apply #'append (loop repeat *edge-num*
			;; 'collect' specifies an object to keep after each iteration of loop
			;; Any lisp expression can be used to generate the object to be collected
			collect
			;; Connect two random nodes
			(edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  ;; return all edges from 'edge-list' that have 'node' as their first element
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; Get a list of all nodes which are currently connected to the specified node
(defun get-connected (node edge-list)
  ;; initialise a local variable 'visited'
  (let ((visited nil))
    ;; Define local function 'traverse'
    (labels ((traverse (node)
	       ;; Unless 'node' has already been visited and is thus a 'member' of the 'visited' list
	       (unless (member node visited)
		 ;; Add the node to the visited list
		 (push node visited)
		 ;; Apply traverse to the cdr of the result of the proceeding 'direct-edges' function
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       ;; get all non-node starting edges
		       (direct-edges node edge-list)))))
      ;; Start traversing the supplied node
      (traverse node))
    ;; return the visited nodes
    visited))

(defun find-islands (nodes edge-list)
  ;; Initialise a local variable 'islands'
  (let ((islands nil))
    ;; create local function 'find-island'
    (labels ((find-island (nodes)
	       ;; Initialise 2 local variables, connected and unconnected
	       (let* ((connected
			;;  Call 'get-connected' to initialise the connected list
			(get-connected (car nodes) edge-list))
		      ;; Collect all nodes not connected to the current node into unconnected
		      (unconnected (set-difference nodes connected)))
		 ;; Add all 'connected' nodes to islands
		 (push connected islands)
		 ;; If unconnected is not empty
		 (when unconnected
		   ;; call find-island on the remaining nodes
		   (find-island unconnected)))))
      ;; Call find-island on all nodes
      (find-island nodes))
    ;; Return the islands
    islands))

(defun connect-with-bridges (islands)
  ;; If islands has at least two elements
  (when (cdr islands)
    ;; Connect the second and third element of islands
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  ;; call connect with bridges on all unconnected nodes
  (append (connect-with-bridges (find-islands nodes edge-list))
	  edge-list))

(defun make-city-edges ()
  ;; let* allows referencing of defined variables in subsequent variables e.g. the edge-list definition references the previously defined 'nodes' variable.
  ;; Create list of numbers representing nodes
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 ;;  Create edge list using connect-all-islands function
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 ;; create a subset of the edge list which will have cops on it
	 (cops (remove-if-not (lambda (x)
				;; Get a random value between 0 and cop-odds, only return true if value is zero
				(zerop (random *cop-odds*)))
			      ;; If true add to cops list
			      edge-list)))
    ;; create the edges alist with cops present
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  ;; Apply lambda to each element in sequence
  (mapcar (lambda (node1)
	    ;; connect node1 to the result of mapcar
	    (cons node1 
		  (mapcar (lambda (edge)
			    ;; create a new list out of the cdr of the supplied edge
			    (list (cdr edge)))
			  (remove-duplicates
			   ;;  Get all edges connected to the current node
			   (direct-edges node1 edge-list)
			   :test #'equal))))
	  ;; Remove any duplicate nodes 
	  (remove-duplicates
	   ;;  Get the first node of each edge in edge list
	   (mapcar #'car edge-list))))

;; OOE
;; Get the first element of each edge in edge list
;; Remove any duplicates from the list
;; for each item in the new list referenced above
;; for each edge associated with the current item
;; get every edge directly connected to the node and remove any duplicates
;; 


(defun add-cops (edge-alist edges-with-cops)
  ;; For each item in edge-alist
  (mapcar (lambda (x)
	    ;; set node1 to the first element
	    (let ((node1 (car x))
		  ;; set node1 edges to the remaining elements
		  (node1-edges (cdr x)))
	      ;; Join node1
	      (cons node1 
		    (mapcar (lambda (edge)
			      ;; set node2 to the second elemnt
			      (let ((node2 (car edge)))
				;; if the edge connecting node1 and node2 is on the 'edges-with-cops' list
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    ;; create a new list with node 2 and cops
				    (list node2 'cops)
				    ;; return the edge
				    edge)))
			    ;; Pass node1-edges to the above mapcar
			    node1-edges))))
	  ;; pass the edge-alist to the outermost mapcar
	  edge-alist))
