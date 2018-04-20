(defun get-cdr (lst)
  "not work"
  (setf lst (cdr lst)))

(declaim (inline get-cdr2))
(defun get-cdr2 (lst)
  "not work"
  (setf lst (cdr lst)))

(defun get-cdr3 (lst)
  (setf lst (cdr lst))
  lst)

(defmacro set-cdr (lst)
  "work"
  `(setf ,lst (cdr ,lst)))
(let ((asd '(1 2 3 4 5)))
  (get-cdr asd)
  (print asd)
  (set-cdr asd)
  (print asd)
  (get-cdr2 asd)
  (print asd)
  (setf asd (get-cdr3 asd))
  (print asd))

(defstruct s-lst
  lst)
(defun get-cdr-struct (slst)
  (setf (s-lst-lst slst) (cdr (s-lst-lst slst))))
(let ((asd (make-s-lst :lst (list 1 2 3 4))))
  (get-cdr (s-lst-lst asd))
  (print (s-lst-lst asd))
  (set-cdr (s-lst-lst asd))
  (print (s-lst-lst asd))
  (get-cdr-struct asd)
  (print (s-lst-lst asd)))
