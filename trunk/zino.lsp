;; Loads a zino file with its path
(defun zino-load (file)
(with-open-file (stream file
			  :direction :input
			  :if-does-not-exist :error)
    ; on boucle sur les defun tant qu'on ne trouve pas un EOF
    ;(loop for instr = (read stream nil 'EOF) until (eql instr 'EOF)
      ;collect instr)))
      ;(if (eql (car instr) 'section)
      ;  (print "hey")))))
  (let ((content ""))
    (with-open-file (stream file)
      (do ((line (read-line stream nil nil t)
                 (read-line stream nil nil t)))
          ((null line))
          ;(print line)
          (setf content (concatenate 'string content line))))
    content)))


;; Evaluates the content of a zino file
(defun zino-eval (content)
;(print (car content))
  (case (car content)
    ; type of the document
    (dtype (concatenate 'string "document type is " (string (cadr content))))
    
    ; different sections of the document
    (section (zino-section ()))
    
    ; table of contents
    (toc (setf toc (make-array 10))
         (setf sect 0) 
         nil)
    
    ; 
    ))

(defun zino-section (title content &optional (section-level 0))
  (case section-level
    (0 (setf (aref toc sect) (list-to-string (cadr content)))
       (setf sect (+ sect 1))
       (setf ssect 1)
       (map 'list (lambda (x) (zino-eval x 1)) (cddr content)))
    (1 (if (eql ssect 1)
         (let ((cur-sect (aref toc (- sect 1))))
           (setf (aref toc (- sect 1)) (make-array 10))
           (setf (aref (aref toc (- sect 1)) 0) cur-sect)))
           (setf (aref (aref toc (- sect 1)) ssect) 
             (list-to-string (cadr content)))
           (setf ssect (+ ssect 1))
           (map 'list (lambda (x) (zino-eval x 2)) (cddr content)))))

;; Writes in a file the final content
(defun zino-write (file content)
  (with-open-file (stream file :direction :output)
    (print content stream)))
  

;; Final procedure with the filename as a parameter 
(defun zino (file)
  (zino-write "zino.txt"
    (map 'list (lambda (x) (zino-eval x)) (string-to-list (zino-load file)))))






;;; annexes ;;;
(defun string-to-list (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
                               (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
            (setq the-list 
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))

(defun list-to-string (l &optional r)
  (if (atom (cdr l))
    (concatenate 'string r (string (car l)))
    (list-to-string (cdr l) (concatenate 'string r (string (car l)) " "))))
            
(defun test ()
  "(dtype article) (authors (Fabien Hervouet`, pouet))")