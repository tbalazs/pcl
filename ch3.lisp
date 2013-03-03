; repl
(list 1 2 3)
(list :a 1 :b 2 :c 3)
(getf (list :a 1 :b 2 :c 3) :a)
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

(defun foo (a b c) (list a b c))
(defun foo (&key a b c) (list a b c))
(foo :a 1 :b 2 :c 3)
(foo :c 3 :b 2 :a 1)
(foo :a 1 :c 3)
(foo)
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

; macro thingy
(defmacro backwards (expr) (reverse expr))

(backwards ("hello, world" t format))

; version 1
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

; version 2
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(macroexpand-1 '(where :title "Home" :ripped nil))
(select (where :title "Home" :ripped t))

; cd database
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title) title)   t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title    (setf (getf row :title) title))
	       (if artist   (setf (getf row :artist) artist))
	       (if rating   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


; tests
(make-cd "Roses" "kathy Mattea" 7 t)

; fill db
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

; dump db
(dump-db)

; query db
(remove-if-not #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

; selector function
(select (artist-selector "Dixie Chicks"))

; 
(select (where :artist "Dixie Chicks"))

(select 
 (and 
  (where :artist "Dixie Chicks") 
  (where :title "Home")))

(select
 #'(lambda (cd)
     (and (equal (getf cd :title) "Give Us a Break")
	  (equal (getf cd :ripped) t))))

; update
(update (where :artist "Dixie Chicks") :rating 11)
(update (where :artist "Dixie Chicks") :ripped t)
