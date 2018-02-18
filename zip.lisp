(set-macro-character #\Z #'zip-reader)

(defun zip-reader (stream char)
  (declare (ignore char))
  (let ((fn 'list))
    (if (not (char= (peek-char nil stream nil nil t) #\space))
        (setf fn (read stream t nil t)))
    `(lambda (xlist ylist)
       (mapcar #'(lambda (x y) (funcall ',fn x y))
               xlist
               ylist))))

;; (Z '(1 2 3) '(4 5 6)) => ((1 4) (2 5) (3 6))
;; (Z+ '(1 2 3) '(4 5 6)) => (5 7 9)
