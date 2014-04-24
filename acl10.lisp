;; what is macro
(defmacro nil! (x)
  (list 'setf x nil))

(macroexpand-1 '(nil! x))

(nil! x)

;; how to design a macro
(ntimes 10
        (princ "."))

; bad ntimes
(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

; error 1: variable capture
(let ((x 10))
  (ntimes 5
          (setf x (+ x 1)))
    x)
; = 10

; solution gensym
(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
       ,@body)))

; error 2: multiple evalutation
(let ((v 10))
  (ntimes (setf v (- v 1))
          (princ ".")))
; .....

;; solution double gensym
(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

;; some macro samples
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                    choices)))))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(in '+ '+ '- '*)

;; 1. x = a, y = b, z = (c d)
; ((C D) A Z)
`(,z ,x z)
; (X B C D)
`(x ,y ,@z)
; ((C D A) Z)
`((,@z a) z)

;; 2.
(defmacro if (test then else)
  `(cond
     (,test ,then)
     (t ,else)))

;; 3.
(defmacro nth-expr (n &body body)
  (if (integerp n)
      (nth n body)
    `(case ,n
       ,@(let ((i -1))
           (mapcar #'(lambda(x) `(,(incf i) ,x)) body)))))

(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))

;; 4
(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))
(defmacro with-gensyms (vals &rest body)
  `(let ,(mapcar #'(lambda (x)
                     (list x '(gensym)))
                 vals)
     ,@body))

(with-gensyms (a b c d)) ;; test

(defmacro ntimes (n &rest body)
  (with-gensyms (h dobody i)
    `(let ((,h ,n))
       (labels ((,dobody (,i)
                  (if (> ,i 0)
                      (progn ,@body
                             (,dobody (- ,i 1))))))
         (,dobody ,h)))))

(let ((i 0))
  (ntimes 4 (print (incf i))))

;; 5

(defmacro n-of (n expr)
  (with-gensyms (gn gi gacc)
    `(do ((,gn ,n) (,gi 0 (1+ ,gi)) (,gacc nil (cons ,expr ,gacc)))
         ((>= ,gi ,gn) (nreverse ,gacc)))))

(let ((i 0) (n 1))
  (n-of n (incf i)))

;; 6
(defmacro revert-variables (vars &body body)
  `((lambda ,vars ,@body) ,@vars))

(let ((x 1)
      (y 2)
      (z 3))
  (revert-variables (x y z)
                    (setf x 0
                          y 0
                          z 0)
                    (format t "~A ~A ~A" x y z))
  (values x y z))

;; 7

(defmacro push- (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

(let ((a (make-array 3))
      (i 0))
  (setf (aref a 0) (list 0)
        (aref a 1) (list 1)
        (aref a 2) (list 2))
  (push- 4 (aref a (incf i)))
  (format t "~A ~A ~A~%" (aref a 0) (aref a 1) (aref a 2)))

;; 8
(define-modify-macro my-double ()
  (lambda (x) (* x 2)))
(defmacro wrong-double (x)
  `(setf ,x
         (* ,x 2)))

(let ((a (make-array 3))
      (i 0))
  (dotimes (n 3)
    (setf (aref a n)
          n))
  (wrong-double (aref a (incf i)))
  a)
