;;; Exercise for ACL chapter 3

;; 2
(defun new-union (l1 l2)
  (cond ((null l1) l2)
	((null l2) l2)
	(t (cons (car l1)
		 (remove (car l1) l2)))))

(new-union '(a b c) '(c a d))

;; 3 naive, to be improved
(defun occurrences (lst)
  (let ((results (occurrences-helper lst ())))
    (occurrences-max-first results)))

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

(defun occurrences-max (results)
  (if (null results)
      nil
      (let ((max '(nil . -1)))
	(dolist (result results)
	  (if (> (cdr result) (cdr max))
	      (setf max result)))
	max)))

(defun occurrences-max-first (lst)
  (if (null lst)
      nil
      (let ((max (occurrences-max lst)))
	(cons max
	      (occurrences-max-first (remove max lst))))))

(occurrences '(a b b b b b c c c c a a cd c))
(occurrences-helper '(a b b a a a) ())
(occurrences-max-first '((a . 1) (b . 2)))

