(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :歌名 title :歌手 artist :评分 rating :已翻录 ripped))

(defun add-record (cd) 
  (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a: ~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd 
   (prompt-read "歌名")
   (prompt-read "歌手")
   (or (parse-integer (prompt-read "评分") :junk-allowed t) 0) 
   (y-or-n-p "已翻录 [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "还有？[y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file 
      (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))