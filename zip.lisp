#|
(defun zip-helper (fn lists)
  (labels ((zip-iter (lst result)
             (let ((x (mapcar #'car lst)))
               (cond ((some #'null lst) (nreverse result))
                     (t (zip-iter (mapcar #'cdr lst) (push (apply fn x) result)))))))
    (zip-iter lists nil)))

(defun zip-reader (stream char)
  (declare (ignore char))
  (let ((fn 'list))
    (if (not (char= (peek-char nil stream nil nil t) #\space))
        (setf fn (read stream t nil t)))
    ;`(lambda (&rest lists)
    ;   `(mapcar #'(lambda (&rest args) (apply ',',fn args))
    ;            ,@(mapcar #'(lambda (x) `',x) lists)))))
    `(lambda (&rest lists)
       (zip-helper ',fn lists))))

(set-macro-character #\Z #'zip-reader)
;; (print (Z '(1 2 3) '(1 2 3) '(1 2 3)))
;; (print (Z+ '(1 2 3) '(1 2 3) '(1 2 3)))
|#

(defun zip (fn &rest lists)
  (labels ((zip-iter (lst result)
             (let ((x (mapcar #'car lst)))
               (cond ((some #'null lst) (reverse result))
                     (t (zip-iter (mapcar #'cdr lst) (cons (apply fn x) result)))))))
    (zip-iter lists nil)))

;; (zip 'list '(1 2 3) '(1 2 3) '(1 2 3))
;; (zip '+ '(1 2 3) '(1 2 3) '(1 2 3))
