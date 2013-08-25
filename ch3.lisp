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
<<<<<<< HEAD
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not 
   #'(lambda (cd) (equal (getf cd :歌手) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key 歌名 歌手 评分 (已翻录 nil 已翻录-p))
  #'(lambda (cd)
      (and
       (if 歌名 (equal (getf cd :歌名) 歌名) t)
       (if 歌手 (equal (getf cd :歌手) 歌手) t)
       (if 评分 (equal (getf cd :评分) 评分) t)
       (if 已翻录-p (equal (getf cd :已翻录) 已翻录) t))))

=======
      (setf *db* (read in)))))
>>>>>>> 77bf4eb1f6790e7f08c427cb013bb531faa8b8f0
