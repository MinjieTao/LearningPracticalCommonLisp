(let ((count 0))
  (defun reset ()
    (setf count 0))
  (defun stamp ()
    (incf count)))

(list (reset) (stamp) (stamp) (stamp))

(let ((n 0))
  (incf n))

(oddp 1)

(let ((x 10))
  (defun foo ()
    (declare (special x))
    x))


(let ((x 20))
  (foo))

;; 1.

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str :test test :start p2)
                    nil)))
        nil)))

(tokens "abc dte 12.sd"  :test #'alpha-char-p :start 0)

;; 2.
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

;; 3'
(defun num-args (&rest args)
  (apply #'list args))

(num-args 1 2 3 4 5 6 7)

;;4

(defun most2 (fn lst)
  (if (< (length lst) 2)
      (values (car lst) nil)
      (let* ((win1 (car lst))
             (max1 (funcall fn win1))
	     (win2 (cadr lst))
	     (max2 (funcall fn win2)))
        (dolist (obj (cdr lst))
          (if (> max1 max2)
	      (multiple-value-setq (win1 win2 max1 max2)
		(values win2 win1 max2 max1)))
	  (let ((score (funcall fn obj)))
            (cond  ((> score max1)
		    (setf win1 obj
			  max1  score))
		   ((> score max2)
		    (setf win2 obj
			  max2 score))
		   (t nil))))
        (values win1 win2))))

(most2 #'identity '(1 2 3 4 5 6 7 8))

;; 5

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun my-remove-if (predicate lst)
  (filter #'(lambda (x)
	      (and (not (funcall predicate x)) x))
	  lst))
(my-remove-if #'listp '(a (a b)))

;; 6.
(let ((max nil))
  (defun bigger (x)
    (if (or (null max)
	    (> x max))
	(setf max x)
	max)))

(bigger 0)

;; 7.
(let ((pre nil))
  (defun bigger? (x)
    (let ((r (cond ((null pre) nil)
		   ((> x pre) t)
		   (t nil))))
      (setf pre x)
      r)))

(bigger? 2)

;; 8,
(defun our-apply (fn lst)
  (let ((*print-base* 8))
    (apply fn lst)))

;; this is wrong, *print-base* is a global variable and special variable
(let ((*print-base* 8))
  (defun our-apply (fn lst)
    (apply fn lst)))

(our-apply #'format '(t "~A~%" 10))
