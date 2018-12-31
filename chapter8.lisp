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

(defun find-islands (node edge-list)
  (let ((islands nil))
       (labels ((explore-island (unexplored-nodes)
                  (let* ((connected (get-connected (car unexplored-nodes) edge-list))
                         (unconnected (set-difference unexplored-nodes connected)))
                        (push connected islands)
                        (when unconnected
                              (explore-island unconnected)))))
               (explore-island node))
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

(defun neighbors (node edge-alist)
  ;; get each edge adjacent to node in edge alist
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  ;; true if nodes a and b are within one edge of each other false otherwise
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  ;; If a and b are within one, return true
  (or (within-one a b edge-alist)
      ;; If any of a sequence is not falsy, return true
      (some
       (lambda (x)
	 ;; check to see if element b is a neighbor
	 (within-one x b edge-alist))
       ;; for each of the neighbors of element a 
       (neighbors a edge-alist))))

;; Polish prefix notation is confusing!

(defun make-city-nodes (edge-alist)
  ;; Set wumpus location to a random node
  (let ((wumpus (random-node))
	;; Create a list of nodes of random size for the glow worms
	(glow-worms (loop for i below *worm-num*
			  collect (random-node))))
    ;; Loop through each node
    (loop for n from 1 to *node-num*
	  ;; Generate a list from the loop
	  collect (append
		   ;; Initialize a list from the node number (this will be the first element of each of the sublists in our alist)
		   (list n)
		   
		   (cond
		     ;; If this node has the wumpus on then append the wumpus to the node
		     ((eql n wumpus) '(wumpus))
		     ;; Otherwise if it is within two edges of the wumpus then append blood
		     ((within-two n wumpus edge-alist) '(blood!)))
		   
		   (cond
		     ;; If this node has glow worms on then append glow worms
		     ((member n glow-worms) '(glow-worm))
		     ;; Otherwise 
		     ((some (lambda (worm)
			      ;; If any glow-worm node is within one edge of the current node
			      (within-one n worm edge-alist))
			    glow-worms)
		      ;; append lights
		      '(lights!)))
		   ;; If the remainder of the remainder of this node is not empty then there are police on the edge
		   (when (some #'cdr (cdr (assoc n edge-alist)))
		     ;; append sirens
		     '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun find-empty-node ()
  ;; Assign x to a random node
  (let ((x (random-node)))
    ;; If the cdr of  node x is empty
    (if (cdr (assoc x *congestion-city-nodes*))
	;; then find another node
	(find-empty-node)
	;; otherwise return node x
	x)))


(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  ;; mapcar 1 
  (mapcar
   ;; mapcar 1 function
   (lambda (node)
     ;; If the node has been visited before
     (if (member node *visited-nodes*)
	 ;; assign the nodes associated list to n
	 (let ((n (assoc node *congestion-city-nodes*)))
	   ;; If the node is the players current location
	   (if (eql node *player-pos*)
	       ;; then append an asterix to the node
	       (append n '(*))
	       ;; Otherwise return the associated list
	       n))
	 ;; Otherwise append a question mark to the node
	 (list node '?)))
   ;; mapcar 1 arguments
   ;; Remove duplicate values
   (remove-duplicates
    ;; Combine visited nodes with 
    (append *visited-nodes*
	    ;; mapcan 1
	    (mapcan (lambda (node)
		      ;; mapcar 2
		      (mapcar
		       ;; mapcar 2 function
		       #'car
		       ;; mapcar arguments
		       ;; A list of the edges adjacent to the current node
		       (cdr (assoc node *congestion-city-edges*))))
		    ;; mapcan 1 arguments
		    *visited-nodes* )))))

(defun known-city-edges ()
  ;; mapcar 1
  (mapcar
   ;; mapcar 1 function
   (lambda (node)
     ;; combine node with
     (cons node
	   ;; mapcar 2
	   (mapcar
	    ;; mapcar 2 function
	    (lambda (x)
	      ;; If the node from the current edge is in visited nodes then return the edge
	      (if (member (car x) *visited-nodes*)
		  x
		  ;; Otherwise return the node-edge list, stripping the cdr first
		  (list (car x))))
	    ;; mapcar 2 arguments
	    ;; The edges adjacent to the current node
	    (cdr (assoc node *congestion-city-edges*)))))
   ;; mapcar 1 arguments
   *visited-nodes*))


(defun ingredients (order)
  (mapcan (lambda (burger)
	    (case burger
	      (single '(patty))
	      (double '(patty patty))
	      (double-cheese '(patty patty cheese))))
	  order))
(ingredients '(single double-cheese double))


(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge
	  ;; Get the chosen edge
	  (assoc pos
		 ;; Get the list of edges adjacent to the player position
		 (cdr
		  (assoc *player-pos* *congestion-city-edges*)))))
    ;; If the edge exist
    (if edge
	;; Move to the node on the other side of the edge
	(handle-new-place edge pos charging)
	;; Otherwise warn the user
	(princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node
	   ;; The position to move to
	   (assoc pos *congestion-city-nodes*))
	 ;;  Var is true if node is in the glow worm gang list
	 (has-worm (and (member 'glow-worm node)
			;; and has not yet been visited
			(not (member pos *visited-nodes*)))))
    ;; Append the node-number pos to the list of visited nodes
    (pushnew pos *visited-nodes*)
    ;; set player-pos to the current node number
    (setf *player-pos* pos)
    ;; redraw the city based on the new info
    (draw-known-city)
    (cond
      ;; If the edge is in the cops list, game over
      ((member 'cops edge) (princ "You ran into the cops. Game Over."))
      ;; If the node list contains wumpus
      ((member 'wumpus node) (if charging
				 (princ "You found the Wumpus!")
				 (princ "You ran into the Wumpus")))
      (charging (princ "You wasted your last bullet. Game Over."))
      ;; If the node has a worm gang, move the player to a random new node
      (has-worm (let ((new-pos (random-node)))
		  (princ "You ran into a Glow Worm Gang! You're now at ")
		  (princ new-pos)
		  (handle-new-place nil new-pos nil))))))
