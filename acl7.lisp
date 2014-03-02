(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))
(prin1 (make-buf :vec (make-array 10)))

;; 1
(defun file2lst (filename)
  (let ((lst nil))
    (with-open-file (in filename :direction :input)
      (do ((l (read-line in nil :eof) (read-line in nil :eof)))
	  ((eql l :eof))
	(push l lst))
      )
    (reverse lst)))

(car (file2lst "data"))

;; 2
(defun file2s (filename)
  (let ((lst nil))
    (with-open-file (in filename :direction :input)
      (do ((l (read in nil :eof) (read in nil :eof)))
	  ((eql l :eof))
	(push l lst))
      )
    (reverse lst)))

(file2s "acl7.test")

;; 3
(defun file2file (src dst)
  (with-open-file (in src :direction :input)
    (with-open-file (out dst :direction :output
			 :if-exists :supersede)
      (do ((l (read-line in nil :eof) (read-line in nil :eof)))
	  ((eql l :eof))
	(if (not (eql (char l 0)
		      #\%))
	    (princ l out))))))

(file2file "acl7.test" "output")

;; 4
(defun num-table (arr)
  (let* ((dim (array-dimensions arr))
	 (n (first dim))
	 (m (second dim)))
    (dotimes (i n)
      (dotimes (j m)
	(format t "~10,2,,,F" (aref arr i j)))
      (format t "~%"))))
(num-table #2A((1.1111 1/2 324) (21.333 3.2 1)))

;; 5
(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
     (with-open-file (out file2 :direction :output
                                :if-exists :supersede)
       (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or (char= c (char old pos))
		 (char= #\+ (char old pos)))
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))


(file-subst "I w+" "You wi" "acl7.test" "output")

;; 7
