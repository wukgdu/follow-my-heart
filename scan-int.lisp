(defun scan-int (str n arr)
  (do ((i 0 (1+ i))
       (pos 0))
    ((>= i n))
    (multiple-value-bind (num end) (parse-integer str :start pos :junk-allowed t)
      (declare (fixnum num end))
      (setf (aref arr i) num)
      (setf pos (1+ end)))))

(defun get-dimension (arr &rest dims)
  (let ((dimens (cdr (array-dimensions arr))))
    (do ((lst1 dimens (cdr lst1))
         (lst2 dims (cdr lst2))
         (start (car dims) (+ (* start (car lst1)) (cadr lst2))))
      ((null (cdr lst2))
       ;(setf start (* start (car lst1)))
       (setf start (reduce #'* lst1 :initial-value start))
       (make-array lst1
                   :displaced-to arr
                   :displaced-index-offset start)))))

#|
(defvar arr (make-array '(2 3 4)))
(defvar strnum "1 2 3 4")
(scan-int strnum 4 (get-dimension arr 1 1))
(print arr)
(print (get-dimension arr 1))
|#
