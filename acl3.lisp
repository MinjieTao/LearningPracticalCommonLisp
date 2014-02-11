;;; Exercise for ACL chapter 3

;; 2
(defun new-union (l1 l2)
  (cond ((null l1) l2)
	((null l2) l2)
	(t (cons (car l1)
		 (remove (car l1) l2)))))

(new-union '(a b c) '(c a d))

;; 3 
(defun occurrences-helper (lst result)
  (if (null lst)
      result
      (let ((item (assoc (car lst) result)))
	(occurrences-helper (cdr lst)
			    (if (null item)
				(cons (cons (car lst) 1)
				      result)
				(cons (cons (car lst) (+ 1 (cdr item)))
					    (remove item result)))))))
(defun occurrences (lst)
  (let ((results (occurrences-helper lst ())))
    (sort results #'> :key #'cdr)))

(occurrences '(a b b b b b c c c c a a cd c))
(occurrences-helper '(a b b a a a) ())

;; 5
; a. recursive
(defun pos+ (x)
  (if (null x)
      nil
      (let ((res (pos+ (cdr x))))
	(cons (car x)
	      (mapcar #'(lambda (temp)
			  (+ 1 temp)) 
		      res)))))
; b. iterative
(defun pos+ (x)
  (let ((lth (length x)))
    (do ((i 0 (+ i 1)))
	((= i lth) x)
      (setf (nth i x) (+ (nth i x) i)))))
; c.mapcar
(defun pos+ (lst)
  (let ((i -1))
    (mapcar #'(lambda (x)
		(incf i)
		(+ i x))
	    lst)))

(pos+ '(1 2 3 4 5))

;; 6
(defun my-cdr (x)
  (car x))
(defun my-car (x)
  (cdr x))
; a
(defun my-cons (a b)
  (let ((ans '(nil . nil)))
    (setf (cdr ans) a
	  (car ans) b)
    ans))

(my-car (my-cons 'a '(b)))

(defun my-list (&rest lst)
  lst)

(my-car (my-list 'a 'b 'c 'd 'e))

(defun my-length (lst)
  (if (null lst)
      0
      (+ 1 (my-length (my-car lst)))))

(my-length '(a b c d e f))

(defun my-member (item list)
  (cond ((null list) nil)
	((eql item (my-cdr list)) list)
	(t (my-member item (my-car list)))))

(my-member 'f '(e b c a a a))

;; 8 
(defun showdots (lst)
  (showdots-helper lst)
  (format t "~%"))

(defun showdots-helper (lst)
  (if (null lst) 
      (format t "NIL")
      (progn
	(format t "(")
	(if (listp (car lst))
	    (showdots-helper (car lst))  
	    (format t "~A" (car lst)))
	(format t ".")
	(showdots-helper (cdr lst))
	(format t ")"))))
  
(showdots '(a (b) c))

;;9
(defun new-pathes (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

(defun filter-new-pathes (path node net)
  (let ((rst (new-pathes path node net)))
    (remove-if #'(lambda (lst)
		   (member (car lst) (cdr lst)))
	       rst)))

(defun bfs (end queue ard-path net)
  (format t "~A path: ~A~%" queue ard-path)
  (if (null queue)
      ard-path
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (bfs end (cdr queue) path net)
	      (let ((next-pathes (filter-new-pathes path node net)))
		(format t "next: ~A~%" next-pathes)
		(bfs end
		     (append (cdr queue)
			     next-pathes)
		     ard-path
		     net)))))))

(defun lengest-path (start end net)
  (reverse 
   (bfs end (list (list start)) nil net)))

(lengest-path 'a 'd '((a b c) (b c d e) (d e) (e f) (f d)))
;;=> (a b e f d)

