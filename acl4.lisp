;;; ch4 exercises of ACL


;; 1
(defun quarter-turn (arr)
  (let ((n (car (array-dimensions arr))))
    (let ((new-arr (make-array (list n n))))
      (dotimes (i n t)
	(dotimes (j n t)
	  (setf (aref new-arr j (- (- n 1) i)) (aref arr i j))))
      new-arr)))

(quarter-turn  (make-array '(4 4) 
			   :initial-contents 
			   '((a b c d)
			     (e f g h)
			     (i j k l)
			     (m n o p))))

;; 2
(defun my-copy-list (lst)
  (reduce #'(lambda (elm l)
	      (cons elm l))
	  lst 
	  :from-end t
	  :initial-value nil))

(my-copy-list '(a b c d e))

(defun my-reverse (lst)
  (reduce #'(lambda (l elm)
	      (cons elm l))
	  lst
	  :initial-value nil))

(my-reverse '(a b c d e))

;; 3
(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elm n)))))
    elm (l nil) (m nil) (r nil))

(make-node )

(defun copy-tree (root)
  (if (null root)
      nil
      (make-node :elm (node-elm root)
		 :l (copy-tree (node-l root))
		 :m (copy-tree (node-m root))
		 :r (copy-tree (node-r root)))))

(copy-tree (make-node :elm 1 :l (make-node :elm 2) :r (make-node :elm 3)))

(defun search-tree (item root)
  (if (null root)
      nil
      (if (eql item (node-elm root))
	  root
	  (or (search-tree item (node-l root))
	       (search-tree item (node-m root))
	       (search-tree item (node-r root))))))

(search-tree 4 (make-node :elm 1 :l (make-node :elm 2) :r (make-node :elm 3)))


;; 4
(defstruct (nod (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (nod-elm n)))))
    elm (l nil) (r nil))

(defun bst-lst (bst)
  (if (null bst)
      nil
      (append (bst-lst (nod-l bst))
	      (cons (nod-elm bst)
		    (bst-lst (nod-r bst))))))

(bst-lst (make-nod :elm 2 
		   :l (make-nod :elm 1) 
		   :r (make-nod :elm 4 
				:l (make-nod :elm 3))))

(defun bst-adjoin (item bst)
  (if (null bst)
      (make-nod :elm item)
      (let ((elm (nod-elm bst)))
	(cond ((> item elm) 
	       (setf (nod-r bst)
		     (bst-adjoin item (nod-r bst)))
	       bst)
	      ((< item elm)
	       (setf (nod-l bst)
		     (bst-adjoin item (nod-l bst)))
	       bst)
	      (t bst)))))
		
(bst-lst (bst-adjoin 7
		     (bst-adjoin 9 (make-nod :elm 2 
					     :l (make-nod :elm 1) 
					     :r (make-nod :elm 4 
							  :l (make-nod :elm 3))))))
		      
