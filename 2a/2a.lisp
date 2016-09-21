; 1
; Except for bag-of-words, we could also consider context from word frequency (e.g. tf-idf), parse trees, POS, and dependency relations, etc.
	
; 2

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
(print "Word-list and feature-list are built.")
(print m)
(print n)

(defstruct VS 
   matrix 
   similarity-fn )
(defparameter space (make-VS
    :matrix (make-array (list m n) :initial-element 0)))
						   

(defun update (amatrix slist conlist)
	(loop
		for wd in conlist
		do (update2 amatrix wd slist)))
(defun update2 (amatrix wd slist)
	(loop
		for co_wd in slist
		if (not (member co_wd *stop-list* :test #'equal))
		do (incf (aref amatrix (position wd *word* :test #'equal) (position co_wd *feature* :test #'equal)))))

(defun read-corpus-to-vs (corpusfile) 
	(with-open-file (stream corpusfile :direction :input)
	  (loop
	      for line = (read-line stream nil)
		  while line
		  if (intersection *word* (normalize-token line) :test #'equal)
		  do (update (VS-matrix space) (normalize-token line) (intersection *word* (normalize-token line) :test #'equal)))))
	  
(read-corpus-to-vs "brown2.txt")	  
(print "Vector space is built.")

(defun array-row (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
       :displaced-index-offset (* row (array-dimension arr 1))))
	   
(defun get-feature-vector (space str)
	(array-row (VS-matrix space) (position str *word* :test #'equal)))
	
(defun print-features (space str n)
	(let ((alist nil)
		(vc (get-feature-vector space str)))
		(loop 
			for i from 0 to (- (length *feature*) 1)
			do (push (cons (nth i *feature*) (aref vc i)) alist))
		(subseq (sort alist #'> :key #'cdr) 0 n)))
		
;(print (get-feature-vector space "food"))
(print (print-features space "university" 9))

		
	

		   
		 