; 1
; Except for bag-of-words, we could also consider context from word frequency (e.g. tf-idf), parse trees, POS, and dependency relations, etc.
	
; 2a
(defstruct VS 
   matrix 
   (similarity-fn 'dot-product))
   
; 2b	
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
  (loop for line = (read-line stream nil)
      while line
      append (normalize-token line))))
	  
(defparameter *co_word* (with-open-file (stream "brown2.txt" :direction :input)
  (loop for line = (read-line stream nil)
      while line
	  if (intersection *word* (normalize-token line) :test #'equal)
      	append (normalize-token line))))  
						
(defparameter *myhash* (make-hash-table :test 'equal))	
(loop for x in *co_word*
	if (gethash x *myhash*)
	do (incf (gethash x *myhash*))
	else do (setf (gethash x *myhash*) 1))
	
(defparameter *stop-list*
  '("a" "about" "above" "after" "again" "against" "all" "am" "an" "and" "any" 
	  "are" "aren't" "as" "at" "be" "because" "been" "before" "being" "below" 
	  "between" "both" "but" "by" "can't" "cannot" "could" "couldn't" "did" "didn't" 
	  "do" "does" "doesn't" "doing" "don't" "down" "during" "each" "few" "for" "from" 
	  "further" "had" "hadn't" "has" "hasn't" "have" "haven't" "having" "he" "he'd" 
	  "he'll" "he's" "her" "here" "here's" "hers" "herself" "him" "himself" "his" 
	  "how" "how's" "i" "i'd" "i'll" "i'm" "i've" "if" "in" "into" "is" "isn't" "it" 
	  "it's" "its" "itself" "let's" "me" "more" "most" "mustn't" "my" "myself" "no" 
	  "nor" "not" "of" "off" "on" "once" "only" "or" "other" "ought" "our" "ours" 
	  "ourselves" "out" "over" "own" "same" "shan't" "she" "she'd" "she'll" "she's" 
	  "should" "shouldn't" "so" "some" "such" "than" "that" "that's" "the" "their" 
	  "theirs" "them" "themselves" "then" "there" "there's" "these" "they" "they'd" 
	  "they'll" "they're" "they've" "this" "those" "through" "to" "too" "under" "until" 
	  "up" "very" "was" "wasn't" "we" "we'd" "we'll" "we're" "we've" "were" "weren't" 
	  "what" "what's" "when" "when's" "where" "where's" "which" "while" "who" "who's" 
	  "whom" "why" "why's" "with" "won't" "would" "wouldn't" "you" "you'd" "you'll" 
	  "you're" "you've" "your" "yours" "yourself" "yourselves"))
	
(defparameter *feature* nil)
(defun getkeys (key value)
	(unless (member key *stop-list* :test #'equal)
		(push key *feature*)))
(maphash #'getkeys *myhash*)

(setf m (length *word*))
(setf n (length *feature*))						   

(defparameter space (make-VS
    :matrix (make-array (list m n) :initial-element 0)))
	
(defun update (amatrix slist conlist)
	(loop for wd in conlist
		do (loop for co_wd in slist
			if (not (member co_wd *stop-list* :test #'equal))
			do (incf (aref amatrix (position wd *word* :test #'equal) (position co_wd *feature* :test #'equal))))))		

(defun read-corpus-to-vs (vspace corpusfile) 
	(with-open-file (stream corpusfile :direction :input)
	  (loop for line = (read-line stream nil)
		  while line
		  if (intersection *word* (normalize-token line) :test #'equal)
		  do (update (VS-matrix vspace) (normalize-token line) (intersection *word* (normalize-token line) :test #'equal)))))
		  
(defparameter space (make-VS
	:matrix (make-array (list m n) :initial-element 0)))
	
(read-corpus-to-vs space "brown2.txt")	  

; 2c
(defun array-row (arr row)
    (loop for i from 0 to (- (first (last (array-dimensions arr))) 1)
        collect (aref arr row i)))
		
(defun get-feature-vector (space str)
	(array-row (VS-matrix space) (position str *word* :test #'equal)))
	
; 2d
(defun print-features (space str n)
	(let ((alist nil)
		(vc (get-feature-vector space str)))
		(loop for i from 0 to (- (length *feature*) 1)
			do (push (cons (nth i *feature*) (nth i vc)) alist))
		(subseq (sort alist #'> :key #'cdr) 0 n)))
		
;(get-feature-vector space "food")
;(print-features space "university" 9)

; 3a
(defun euclidean-length (vec)
	(let ((sum 0))
		(loop for i from 0 to (- (length vec) 1)
			do (setf sum (+ sum (expt (nth i vec) 2))))
			(setf sum (sqrt sum))))
			
;(euclidean-length (get-feature-vector space "boston"))

; 3b
(defun length-normalize-vs (vspace)
	(let ((len 0))
		(loop for i from 0 to (- m 1)
			do (setf len (euclidean-length (get-feature-vector vspace (nth i *word*))))
			do (loop for j from 0 to (- n 1)
					do (setf (aref (VS-matrix vspace) i j) (/ (aref (VS-matrix vspace) i j) len))))))
		
(length-normalize-vs space)
;(euclidean-length (get-feature-vector space "boston"))

; 3c

(defun dot-product (space x y)
	(let ((sum 0))	
		(setf xvec (get-feature-vector space x))
		(setf yvec (get-feature-vector space y))
		(loop for i from 0 to (- (length xvec) 1)
			do (setf sum (+ sum (* (nth i xvec) (nth i yvec)))))
		sum))
; 3d		
			
(defun word-similarity (vspace str1 str2)
	(Funcall (VS-similarity-fn vspace) vspace str1 str2))

;(word-similarity space "university" "college")

	

		   
		 