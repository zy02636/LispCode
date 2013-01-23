;;split a string 
(defun tokens (str test start)
   (let ((p1 (position-if test str :start start)))
      (if p1
        (let ((p2 (position-if #'(lambda (c) 
                                    (not (funcall test c)))
                               str :start p1)))
              (cons (subseq str p1 p2)
                    (if p2
                        (tokens str test p2)
                        nil)))
        nil)))

;;split unit filter
(defun constituent (c)
     (and (graphic-char-p c)
           (not (char= c #\ ))))

;;parse string date to numeric date
(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month   (second toks))
          (parse-integer    (third toks)))))

(defconstant month-names
   #("jan" "feb" "mar" "apr" "may" "jun" 
     "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
   (let ((p (position str month-names :test #' string-equal)))
     (if p
         (+ p 1)
         nil)))

;;convert string to integer
(defun read-integer (str)
   (if (every #'digit-char-p str)
       (let ((accum 0))
         (dotimes (pos (length str))
           (setf accum (+ (* accum 10)
                          (digit-char-p (char str pos)))))
          accum)
    nil))

;;define structure
(defstruct (point (:conc-name p)
				  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))

;;binary search
;;define node structure
(defstruct (node (:print-function
				  (lambda (n s d)
					(format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

;;define node insert function: insert value, binary search tree, operation
(defun bst-insert (obj bst <)
  (if (null bst)
	  (make-node :elt obj)
	(let ((elt (node-elt bst)))
	  (if (eql obj elt)
		  bst
		(if (funcall < obj elt)
			(make-node
			 :elt elt
			 :l (bst-insert obj (node-l bst) <)
			 :r (node-r bst))
		  (make-node
		   :elt elt
		   :r (bst-insert obj (node-r bst) <)
		   :l (node-l bst)))))))

;;best find
(defun bst-find (obj bst <)
  (if (null bst)
	  nil
	(let ((elt (node-elt bst)))
	  (if (eql obj elt)
		  bst
		(if (funcall < obj elt)
			(bst-find obj (node-l bst) <)
		    (bst-find obj (node-r bst) <))))))

;;binary tree min
(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))

;;binary tree max number 
(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))


