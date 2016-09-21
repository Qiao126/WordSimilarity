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
	  
(defparameter *co_word* (with-open-file (stream "brown2.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
      while line
	  if (intersection *word* (normalize-token line) :test #'equal)
      	append (normalize-token line))))  
			
(defparameter *myhash* (make-hash-table :test 'equal))	
(loop
	for x in *co_word*
	if (gethash x *myhash*)
	do (incf (gethash x *myhash*))
	else do (setf (gethash x *myhash*) 1))

;(print (hash-table-count *myhash*))

(defun print-hash-entry (key value)
    (format t "~S ~S~%" key value))
;(maphash #'print-hash-entry *myhash*)

(defparameter *feature* nil)
(defun getkeys (key value)
	(unless (string= key "")
		(push key *feature*)))
(maphash #'getkeys *myhash*)

(setf m (length *word*))
(setf n (length *feature*))
(print m)
(print n)		
								   
(setf matrix (make-array (list m n) :initial-element 0))

(defun update (matrix slist conlist)
	(loop
		for wd in conlist
		do (update2 matrix wd slist)))
(defun update2 (matrix wd slist)
	(loop
		for co_wd in slist
		do (incf (aref matrix (position wd *word* :test #'equal) (position co_wd *feature* :test #'equal)))))

(defparameter space (with-open-file (stream "brown2.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
	  while line
	  if (intersection *word* (normalize-token line) :test #'equal)
	  do (update matrix (normalize-token line) (intersection *word* (normalize-token line) :test #'equal))))
	  matrix)
	  
(defun get-feature-vector space str
	(aref space (position str *word* :test #'equal)))

(defun print-features space str n
	(let ((alist nil))
		(setf vc (get-feature-vector str))
		(loop 
			for i from 0
			do (push (cons (nth i *feature*) (nth i vc)) alist))
		(subseq (sort alist #'> :key #'cdr) 0 n)))
		
	

		   
		 