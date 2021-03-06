;;;; see https://github.com/xysun/regex
;;;; this may not work

;;; utils start
(defmacro awhen (pred &body body)
  `(let ((it ,pred))
     (when it
       ,@body)))

(defmacro new-hash (configs &rest pairs)
  (let ((hash (gensym))
        (value-pairs (gensym)))
    `(let ((,hash (make-hash-table ,@configs))
           (,value-pairs (list ,@pairs)))
       (do ((k (car ,value-pairs) (car ,value-pairs))
            (v (cadr ,value-pairs) (cadr ,value-pairs)))
         ((null ,value-pairs) ,hash)
         (setf ,value-pairs (cddr ,value-pairs))
         (setf (gethash k ,hash) v)))))

;;; utils end

(defstruct token
  (name "char" :type string)
  (value #\] :type character))

(defun lexer (str)
  (let ((cur-pos 0)
        (str-len (length str))
        ;;(source str)
        key-hash)
    (setf key-hash (new-hash ()
                             #\[ "left["
                             #\] "right]"
                             #\+ "one-or-more"
                             #\* "zero-or-more"
                             #\? "zero-or-one"
                             #\| "or"))
    (flet ((get-token ()
                      (if (>= cur-pos str-len)
                        (make-token :name "end")
                        (let ((cur-char (char str cur-pos)))
                          (incf cur-pos)
                          (multiple-value-bind (val find-p) (gethash cur-char key-hash)
                            (if find-p
                              (make-token :name val :value cur-char)
                              (make-token :value cur-char)))))))
      #'get-token)))

(defun parser (lexer-instance)
  (let ((tokens nil)
        (lookahead (funcall lexer-instance)))
    (labels ((parse-top ()
                        (parse-exp)
                        (nreverse tokens))
             (consume (name)
                      (if (string= name (token-name lookahead))
                        (setf lookahead (funcall lexer-instance))
                        (error "parse error")))
             (parse-exp ()
                        (parse-term)
                        (when (string= "or" (token-name lookahead))
                          (let ((old-look lookahead))
                            (consume "or")
                            (parse-exp)
                            (push old-look tokens))))
             (parse-term ()
                         (parse-factor)
                         (when (not (find (token-value lookahead) '(#\] #\|)))
                           (parse-term)
                           (push (make-token :name "concat") tokens)))
             (parse-factor ()
                           (parse-primary)
                           (when (find (token-value lookahead) '(#\* #\+ #\?))
                             (push lookahead tokens)
                             (consume (token-name lookahead))))
             (parse-primary ()
                            (let ((name (token-name lookahead)))
                              (cond ((string= name "left[")
                                     (consume "left[")
                                     (parse-exp)
                                     (consume "right]"))
                                    ((string= name "char")
                                     (push lookahead tokens)
                                     (consume "char"))))))
      (parse-top))))

(defstruct state
  (closure nil)
  (transitions (make-hash-table))
  (name nil) 
  (end-p nil))

(defstruct NFA
  start
  end)
(defun add-state (state state-set)
  (when (not (find state state-set))
    (push state state-set)
    (mapc #'(lambda (x) (setf state-set (add-state x state-set)))
          (state-closure state)))
  state-set)
(defun match (nfa s)
  (let ((cur-states nil))
    (setf cur-states (add-state (NFA-start nfa) cur-states))
    (mapc #'(lambda (x) (let ((next-state nil))
                          (mapc #'(lambda (st)
                                    (awhen (gethash x (state-transitions st))
                                           (setf next-state (add-state it next-state))))
                                cur-states)
                          (setf cur-states next-state)))
          (coerce s 'list))
    (mapc #'(lambda (st) (if (state-end-p st) (return-from match t)))
          cur-states)))

(let ((state-count -1))
  (defun create-state ()
    (incf state-count)
    (make-state :name state-count))
  (defun clear-state ()
    (setf state-count -1)))

(let ((nfa-stack nil))
  (defun handle-char (tok)
    (let ((s0 (create-state))
          (s1 (create-state)))
      (setf (gethash (token-value tok) (state-transitions s0)) s1)
      (push (make-NFA :start s0 :end s1) nfa-stack)))

  (defun handle-concat (tok)
    (declare (ignore tok))
    (let ((n2 (pop nfa-stack))
          (n1 (pop nfa-stack)))
      (setf (state-end-p (NFA-end n1)) nil)
      (push (NFA-start n2) (state-closure (NFA-end n1)))
      (push (make-NFA :start (NFA-start n1) :end (NFA-end n2)) nfa-stack)))

  (defun handle-or (tok)
    (declare (ignore tok))
    (let ((n2 (pop nfa-stack))
          (n1 (pop nfa-stack))
          (s0 (create-state))
          (s3 (create-state)))
      (setf (state-closure s0) (list (NFA-start n1) (NFA-start n2)))
      (push s3 (state-closure (NFA-end n1)))
      (push s3 (state-closure (NFA-end n2)))
      (setf (state-end-p (NFA-end n1)) nil)
      (setf (state-end-p (NFA-end n2)) nil)
      (push (make-NFA :start s0 :end s3) nfa-stack)))

  (defun handle-rep (tok)
    (let ((n1 (pop nfa-stack))
          (s0 (create-state))
          (s1 (create-state)))
      (setf (state-closure s0) (list (NFA-start n1)))
      (when (string= "zero-or-more" (token-name tok))
        (push s1 (state-closure s0)))
      (push s1 (state-closure (NFA-end n1)))
      (push (NFA-start n1) (state-closure (NFA-end n1)))
      (setf (state-end-p (NFA-end n1)) nil)
      (push (make-NFA :start s0 :end s1) nfa-stack)))

  (defun handle-? (tok)
    (declare (ignore tok))
    (let ((n1 (pop nfa-stack)))
      (push (NFA-end n1) (state-closure (NFA-start n1)))
      (push n1 nfa-stack)))

  (defun clear-nfa-stack ()
    (setf nfa-stack nil))

  (defun re-compile (pattern)
    (let ((lex (lexer pattern))
          tokens
          handles)
      (clear-state)
      (clear-nfa-stack)
      (setf tokens (parser lex))
      (setf handles (new-hash (:test #'equal)
                              "char" #'handle-char
                              "concat" #'handle-concat
                              "or" #'handle-or
                              "zero-or-more" #'handle-rep
                              "one-or-more" #'handle-rep
                              "zero-or-one" #'handle-?))
      (mapc #'(lambda (tok) (funcall (gethash (token-name tok) handles) tok))
            tokens)
      (if (and (null (cdr nfa-stack))
               (car nfa-stack))
        (progn
          (setf (state-end-p (NFA-end (car nfa-stack))) t)
          (car nfa-stack))
        (error "compile error")))))

(let ((test-pass (new-hash (:test #'equal)
                           "a?a?a?a?a?aaaaa" "aaaaa"
                           "cc|a+" "aaaaa")))
  (maphash #'(lambda (k v)
               (let ((nfa nil))
                 (setf nfa (re-compile k))
                 (print (match nfa v))))
           test-pass))
