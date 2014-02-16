;; 1.
(let ((y '(a . b)))
  ((lambda (x)
     (cons x x))
   (car y)))

(let ((x '(1 . b))
      (z '2))
 ((lambda (w z)
    (cons w (+ w z)))
  (car x) z))

;; 2.
(defun mystery (x y)
  (cond ((null y) nil)
	((eql (car y)
	      x)
	 0)
	(t 
	 (let ((z (mystery x (cdr y))))
	   (and z (+ z 1))))))

;;3.
(defun my-square (x)
  (if (and (integerp x)
	   (> x 0)
	   (<= x 5))
      nil
      (* x x)))

(my-square 3.4) ;;
(my-square 4) ;; nil
(my-square 0)

;; 4.
(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))



;; 5.
(defun precedes (x v)
  (if (< (length v) 1) 
      nil
      (let ((lst nil)
	    (i 1))
	(loop
	   (let ((pivot (position x v :start i)))
	     (if (null pivot)
		 (return-from precedes (reverse lst))
		 (progn
		   (push (char v (- pivot 1)) lst)
		   (setf i (+ pivot 1)))))))))

(defun precedes (x v)
  (if (< (length v) 2) 
      nil
      (let ((rv (reverse v)))
	(if (eql (char rv 0) x)
	    (cons (char rv 1)
		  (precedes x (reverse (subseq rv 1))))
	    (precedes x (reverse (subseq rv 1)))))))

(precedes #\a "aaaaa")


;; 6.
(defun intersperse (x lst)
  (if (<= (length lst) 1)
      lst
      (append (list (car lst) x)
	      (intersperse x (cdr lst)))))

(defun intersperse (x lst)
  (cons (car lst)
	(let ((l nil))
	  (dolist (e (cdr lst) l)
	    (setf l
		  (append l
			  `(,x ,e)))))))

(intersperse '- '(a b c d e))
(intersperse '- '(a))

;; 7.

(defun minus-1 (&rest lst)
  (cond ((null lst) t)
	((< (length lst) 2) nil)
	(t (and (= 1 
		   (abs (- (nth 0 lst)
			   (nth 1 lst))))
		(apply #'minus-1 (cddr lst))))))

(defun minus-2 (&rest lst)
  (let ((flag t)) 
    (do ((i 0 (+ i 2)))
	((>= (+ i 1) (length lst)) flag)
      (format t "~A~%" i)
      (setf flag
	    (and flag
		 (= 1 
		    (abs (- (nth i lst)
			    (nth (+ i 1) lst)))))))))
;;; (defun minus-3 (&rest lst))

(minus-2 1 2 2 3 3 4)


;; 8
(defun max-min (v)
  (if (< (length v) 1)
      (values nil nil)
      (multiple-value-bind (max min) 
	  (max-min (subseq v 1))
	(let ((val (svref v 0))) 
	  (values (if (null max)
		      val
		      (max max val))
		  (if (null min)
		      val
		      (min min val)))))))

(max-min #(1 2 0))

