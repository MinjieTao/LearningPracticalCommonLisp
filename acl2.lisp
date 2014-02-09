;;;; Exercises of ANSI common lisp Chapter 2


;;; 7
;; iterative
(defun my-list-in-list-p (lst)
  (let ((flag nil))
    (dolist (obj lst)
      (setf flag 
	    (or flag
		(listp obj))))
    flag))

;; recursive
(defun my-list-in-list-p (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  t
	  (my-list-in-list-p (cdr lst))))) 
    
;;; 8.a
;; iterative
(defun my-printer (x)
  (if (and (numberp x)
	   (> x 0))
      (do ((i x (- i 1)))
	  ((= i 0))
	(format t "."))))
;; recursive
(defun my-printer (x)
  (if (and (numberp x)
	   (> x 0))
      (progn
	(format t ".")
	(my-printer (- x 1)))))

;;; 8.b
;; iterative
(defun my-count-a (lst)
  (let ((c 0))
    (dolist (obj lst)
      (if (eql 'a obj)
	  (setf c (+ c 1))))
    c))
;; recursive
(defun my-count-a (lst)
  (if (null lst)
      0
      (let ((c (my-count-a (cdr lst))))
	(if (eql 'a
		 (car lst))
	    (+ 1 c)
	    c))))

;;; 9
(defun summit (lst)
  (apply #'+ 
	 (remove nil lst)))

(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))
