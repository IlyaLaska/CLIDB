(defun createDB ()
  "Creates an in-memory DB, represented by a hash-table"
  (defparameter base (make-hash-table :test 'equal :size 10 :rehash-size 1) "Main db")
)

(createDB);have to call here for everything else to work

(defun writeDBFile (db dbfile)
  "Writes existing in memory DB to file"
  (with-open-file (dbasefile dbfile :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-char #\( dbasefile)
    (maphash (lambda (key value) (format dbasefile "~%(~S ~S)" key value)) db)
    ; (terpri dbasefile)
    (format dbasefile "~%)")
  )
)

(defun readDBFile (db dbfile)
  "Reads on-file DB into memory"
  (with-open-file (dbasefile dbfile :direction :input)
    ; (map 'list (lambda (val) (print val)) (read dbasefile))
    (map 'list (lambda (l) (setf (gethash (car l) db) (cadr l))) (read dbasefile))
  )
)

(defun wipeDB (db);can optimise by recreating?
  "Cleans DB of all entries"
  ; (setq db (make-hash-table :test 'equal :size 10 :rehash-size 1))
  (maphash (lambda (key value) (remhash key db) value) db
  )
)

(defun wipeDBFile (dbfile)
  "Cleans the DB file"
  (with-open-file (dbasefile dbfile :direction :output :if-exists :supersede))
)

(defun printDB (db)
  (format t "~%------------~S contents: ------------" db)
  (maphash (lambda (key value) (format *standard-output* "~%~S -> ~S" key value)) db
  )
  (format t "~%-----------------------------END OF DB--------------------------------------------")
)

(defun createTable (db table columnList)
  (setf (gethash table db) (list columnList)
  )
)

(defun printTable (db table)
  (format t "~%------Table: ~S------" table)
  (map 'list (lambda (l) (format t "~%~S" l)) (gethash table db))
  (format t "~%-------------------------")
)

(defun dropTable (db table)
  (remhash table db)
)

(defun truncateTable (db table)
  (createTable db table (car (gethash table db))
  )
)

(defun _checkTypeMatch (l1 l2);only matching length lists allowed
  "Checks if the types of every element of the lists match"
  (cond
    ((or (null (listp l1)) (null (listp l1))) Nil)
    ((null l1) t)
    (
      (if (and (listp (type-of (car l1))) (listp (type-of (car l2)))) 
        (equal (car (type-of (car l1))) (car (type-of (car l2))))
        (equal (type-of (car l1)) (type-of (car l2)))
      ) (_checkTypeMatch (cdr l1) (cdr l2)))
    (t Nil)
  )
)

(defun insertIntoTable (db table valuesList)
    (if (equal (length valuesList) (length (car (gethash table db))));if valueList has enough items to be inserted
      (if (> (length (gethash table db)) 1);if there already are values in the table
        (if (_checkTypeMatch valuesList (cadr (gethash table db)));if valueList has the elements of the same type as other lists in the table
          (setf (gethash table db) (append (gethash table db) (list valuesList)))
          (error (format t "~%Some of your values have wrong type~%Correct values list:~%~S" (map 'list (lambda (val) (type-of val)) (cadr (gethash table db)))))
        )
        (setf (gethash table db) (append (gethash table db) (list valuesList)));this is the first valueList in the table
      )
      (error "Your values list cannot be inserted due to improper length")
    )
)

(defun selectFromTable (db table column value)
  (if (position-if (lambda (val) (equal column val)) (car (gethash table db)));Such column exists
    (reduce (lambda (resList l) (if (equal (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) l) value) (append resList (list l)) resList)) (cdr (gethash table db)) :initial-value '())
    (error "Such column does not exist")
  )
)

(defun deleteFromTable (db table column value)
  (if (selectFromTable db table column value)
    (map 'list (lambda (li) (setf (gethash table db) (remove li (gethash table db)))) (selectFromTable db table column value))
    Nil
  )
)

(defun formatSelect (l db table column value);Incorporate into selectFromTable?
      (format t "~%----Select * from ~S Where ~S = ~S----" table column value)
      (format t "~%~S" (car (gethash table db)))
      (map 'list (lambda (li) (format t "~%~S" li)) l)
      (format t "~%------------------------------------------------")
)

(defun alterTable (db table column value newValue)
  (if (selectFromTable db table column value)
    ; positionOfNeededColumn = (position-if (lambda (val) (equal column val)) (car (gethash table db)))
    ; listWhereValueHasToBeReplaced = (find-if (lambda (l) (equal (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) l) value)) (gethash table db))
    (setf (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) (find-if (lambda (l) (equal (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) l) value)) (gethash table db))) newValue)
    Nil
  )
)

(defun sortTable (db table column)
  (setf (gethash table db) (append (list (car (gethash table db))) (sort (cdr(gethash table db)) #'(lambda (l1 l2) (string< (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) l1) (nth (position-if (lambda (val) (equal column val)) (car (gethash table db))) l2))))))
)





; (readDBFile base "db.txt")
; (insertIntoTable base "test" (list "Mc" "My description2"))
; (printTable base "test")
; (alterTable base "test" "description" "My description" "My description new")
; (sortTable base "test" "name")
; (printTable base "test")
; (createTable base "MYTable")
; (wipeDB base)
; (printDB base)
; (createTable base "test" (list "name" "description"))
; (deleteFromTable base "test" "name" "Me")
; (print (selectFromTable  base "test" "name" "Me"))
; (formatSelect (selectFromTable base "test" "name" "Me") base "test" "name" "Me")
; (writeDBFile base "db.txt")
; (wipeDBFile "db.txt")




; (setf (gethash 'pos1 *mytable*) "String in pos1")
; (print (gethash 'pos1 *mytable*))
