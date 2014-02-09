(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :TITLE title :ARTIST artist :RATING rating :RIPPED ripped))

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
   (prompt-read "TITLE")
   (prompt-read "ARTIST")
   (or (parse-integer (prompt-read "RATING") :junk-allowed t) 0) 
   (y-or-n-p "RIPPED [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file 
      (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Naive version of select by artist
(defun select-by-artist (artist)
  (remove-if-not 
   #'(lambda (cd) (equal (getf cd :ARTIST) artist))
   *db*))

;; General select
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key TITLE ARTIST RATING (RIPPED nil RIPPED-p))
  #'(lambda (cd)
      (and
       (if TITLE (equal (getf cd :TITLE) TITLE) t)
       (if ARTIST (equal (getf cd :ARTIST) ARTIST) t)
       (if RATING (equal (getf cd :RATING) RATING) t)
       (if RIPPED-p (equal (getf cd :RIPPED) RIPPED) t))))

;;(select (where :artist "Jason Mraz" :ripped t))
