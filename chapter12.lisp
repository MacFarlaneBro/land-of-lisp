(let ((animal-noises '((dog . woof)
		       (cat . meow))))
  ;; Write the created alist to a file
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

;; Read the created alist from the file
(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))


(with-open-file (my-stream "data.txt" :direction :output
			   ;; Throw an error if the file currently exists
				      :if-exists :error)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :output
			   ;; Overwrite the file if it exists
				      :if-exists :supersede)
  (print "my data" my-stream))

;; Get a socket on the server
(defparameter my-socket (socket-server 4321))

;; Attach at stream to the socket
(defparameter my-stream
  ;; Socket accept will block until a client has connected
  (socket-accept my-socket))

;; Connect to the available socket with a new stream
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))

;; Simply redirect the output of the print command to send text to the stream
(print "Yo Server!" my-stream)

;; Then just use the read command to read text from the stream
(read my-stream)

;; Finally, make sure to close the stream when done.
(close my-stream)

;; And if on the listening server, make sure to free up the socket to prevent locking up the port until reboot
(socket-server-close my-socket)
