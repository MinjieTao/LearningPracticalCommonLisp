;; 1.
(defun non-decrease (lst)
  (if (or (null lst)
          (null (cdr lst)))
      T
      (and (<= (car lst)
               (cadr lst))
           (non-decrease (cdr lst)))))

(eql T (non-decrease '(1 2 3 4 5 5 7)))
(eql Nil (non-decrease '(1 2 1 2)))

(reduce #'(lambda (x y) (+ x y)) '(1 2 3) :initial-value 4)

;; 2.
(defun coins (cents)
  (let ((lst nil))
    (reduce #'(lambda (x y)
                (let ((c (floor (/ x y))))
                  (push c lst)
                  (- x (* c y))))
            '(25 10 5 1)
            :initial-value cents)
    (reverse lst)))

(coins 124)
