(defun string-split-character (str c)
  (let ((result nil))
    (do ((pre-pos 0 (1+ cur-pos))
         (cur-pos (position c str :start 0) (position c str :start (1+ cur-pos))))
      ((not cur-pos) (push (subseq str pre-pos) result) (nreverse result))
      (push (subseq str pre-pos cur-pos) result))))

(defun string-split-substr (str substr)
  (let ((result nil)
        (steps (length substr)))
    (do ((pre-pos 0 (+ cur-pos steps))
         (cur-pos (search substr str :start2 0) (search substr str :start2 (+ cur-pos steps))))
      ((not cur-pos) (push (subseq str pre-pos) result) (nreverse result))
      (push (subseq str pre-pos cur-pos) result))))

(defun string-split (str delimiter)
  (etypecase delimiter
    (character (string-split-character str delimiter))
    (string (string-split-substr str delimiter))))

(print (string-split " asd fgh  " #\Space))
(print (string-split " asd fgh  " "  "))
