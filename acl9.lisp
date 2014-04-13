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

;; 3.
(defun compete ()
  (let ((lst nil))
    (dotimes (r 10 lst)
      (push (random 2)
            lst))))

(compete)

;; 4. thanks to algorithm on
;; http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
(defstruct (point (:conc-name nil))
  x y)

(defun cross (v w)
  (- (* (x v) (y w))
     (* (y v) (x w))))

(defun dot (v w)
  (+ (* (x v) (x w))
     (* (y v) (y w))))

(defun minus (v w)
  (make-point :x (- (x v) (x w)) :y (- (y v) (y w))))

(defun intersect (px py x2 y2 qx qy x4 y4)
  (let ((p (make-point :x px :y py))
        (r (make-point :x (- x2 px) :y (- y2 py)))
        (q (make-point :x qx :y qy ))
        (s (make-point :x (- x4 qx) :y (- y4 qy))))
    (let ((r-cross-s (cross r s))
          (q-minus-p-cross-r (cross (minus q p)
                                r)))
      (if (and (zerop r-cross-s)
               (zerop q-minus-p-cross-r))
          (cond ((let ((q-minus-p-dot-r (dot (minus q p) r)))
                   (and (>= q-minus-p-dot-r 0)
                        (>= (dot r r) q-minus-p-dot-r)))
                 q)
                ((let ((p-minus-q-dot-s (dot (minus p q) s)))
                   (and (>= p-minus-q-dot-s 0)
                        (>= (dot s s) p-minus-q-dot-s)))
                 p)
                (t nil))
          (if (not (zerop r-cross-s))
              (let ((a (/ (cross (minus q p)
                                 s)
                          r-cross-s))
                    (b (/ q-minus-p-cross-r
                          r-cross-s)))
                (if (and (>= a 0)
                         (>= 1 a)
                         (>= b 0)
                         (>= 1 b))
                    (make-point :x (+ (x p) (* a (x r)))
                                :y (+ (y p) (* a (y r)))))))))))

(intersect 0 0 1 1 0 1 1 0)
(intersect 0 0 1 0 1 0 1 1)
(intersect 0 0 1 0 1 1 0 1)
(intersect 0 0 -1 0 1 0 2 0)

;;5

(defun bisec (f min max epislon)
  (let ((m (/ (+ max min)
              2)))
    (if (< (- max min)
           epislon)
        m
        (let ((fmax (funcall f max))
              (fmin (funcall f min))
              (fm   (funcall f m)))
          (cond
            ((< 0 (* fmin fmax)) (error "wrong message"))
            ((= 0 fm) m)
            ((< 0 (* fmin fm)) (bisec f m max epislon))
            ((< 0 (* fmax fm)) (bisec f min m epislon))
            (t nil))))))

;; 6
(defun honer (x &rest eff)
  (cond
    ((null eff) 0)
    ((null (second eff)) (first eff))
    (t (let ((e (+ (* (first eff)
                      x)
                   (second eff))))
         (apply #'honer x (push e (cddr eff)))))))

(honer 2 1 2 3 4)

;test 5
(bisec #'(lambda (x)
           (honer x 2 1)) -5 1 1e-3)
;; 7
(values most-positive-fixnum most-negative-fixnum)
(log (1+ most-positive-fixnum) 2)

;; 8
(typep 1s0 'short-float) ; short-float
(typep 1f0 'single-float)
(typep 1d0 'double-float)
(typep 1l0 'long-float)

(values most-positive-short-float
        most-positive-single-float
        most-positive-double-float
        most-positive-long-float)
