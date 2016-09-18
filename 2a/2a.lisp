; 1
; Except for bag-of-words, we could also consider context from word frequency (e.g. tf-idf), parse trees, POS, and dependency relations, etc.
	
; 2
(defstruct VS
	matrix
	similarity-fn
	)
	
(defun strip_ipunc (string)  ;strip initial punctuations
    (remove-if-not #'alphanumericp string :start 0 
    :end (position-if #'alphanumericp string)))

(defun strip_tpunc (string)	   ;strip tail punctuations
	(remove-if-not #'alphanumericp string 
	:start (or (position-if #'alphanumericp string :from-end t) 0) :end nil))

(defun normalize-token (string)
(loop
    for start = 0 then (+ space 1)
    for space = (position #\space string :start start)
    for token = (strip_tpunc (strip_ipunc (string-downcase (subseq string start space))))
    unless (string= token "") collect token
    until (not space)))
	
(defparameter *word* (with-open-file (stream "words.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
      while line
      append (normalize-token line))))

(defparameter *myhash* (make-hash-table :test 'equal))	

		  
(defparameter space (with-open-file (stream "brown2.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
      while line
	  (setf con_word (intersection *word* (normalize-token line) :test #'equal))
	  (loop
		  for wd in con_word
		  (loop
		  	for con_wd in (normalize-token line)
			if (gethash con_wd (gethash wd *myhash*))
			do (incf (gethash con_wd (gethash wd *myhash*)))
			else do 
			(setf (gethash wd *myhash*) (make-hash-table :test 'equal))
			(setf (gethash con_wd (gethash wd *myhash*)) 1))))))
				  
		  
		  
(defparameter *co_word* (with-open-file (stream "brown2.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
      while line
	  if (intersection *word* (normalize-token line) :test #'equal)
      	append (normalize-token line))))  
			

(loop
	for x in *co_word*
	if (gethash x *myhash*)
	do (incf (gethash x *myhash*))
	else do (setf (gethash x *myhash*) 1))

;(print (hash-table-count *myhash*))

(defun print-hash-entry (key value)
    (format t "~S ~S~%" key value))
;(maphash #'print-hash-entry *myhash*)

(defparameter *features* nil)
(defun getkeys (key value)
	(unless (string= key "")
		(push key *features*)))
(maphash #'getkeys *myhash*)

(setf m (length *word*))
(setf n (length *features*))

(defun make-row-resizeable-array (rows max-columns)
  "Returns an array of length ROWS containing arrays of length MAX-COLUMNS, 
  but with a fill pointer initially set to 0."
  (make-array rows
              :initial-contents (loop for i from 0 below rows
                                   collect (make-array max-columns
                                                       :fill-pointer 0))))
(setf matrix (make-row-resizeable-array m n))



		 