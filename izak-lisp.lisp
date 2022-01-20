;;;; Izak Lisp language
;;;; Tokenizer works

(defun char-bracket? (c)
  (member c (list #\( #\) #\{ #\} #\[ #\]) :test #'char=))

(defun char-special? (c)
  (member c (list #\( #\) #\{ #\} #\[ #\] #\;) :test #'char=))

(defun char-closes? (opener c)
  (cond ((char= opener #\() (char= c #\)))
        ((char= opener #\[) (char= c #\]))
        ((char= opener #\{) (char= c #\}))))

(defun token-closes? (opener token)
  (when token
    (char-closes? opener (first token))))

(defun char-white? (c)
  (member c (list #\space #\tab #\newline) :test #'char=))

(defun char-end-of-word? (c)
  (or (char-white? c)
      (char-special? c)))

(defun partition-while (predicate lst) ; => (before . after)
  (if lst
    (let ((x (car lst)))
      (if (funcall predicate x)
        (let ((r (partition-while predicate (cdr lst))))
          (cons (cons x (car r)) (cdr r)))
        (cons nil lst)))
    '(nil)))

(defun partition-until (predicate lst) ; => (before . after)
  (partition-while (complement predicate) lst))

(defun partition-done? (partition)
  (null (first (rest partition))))

(defun token-partition (chars) ; => (before . after)
  (if chars
    (let ((c (first chars)))
      (cond ((char-white? c)
             (token-partition (rest chars)))
            ((digit-char-p c)
             (partition-while #'digit-char-p chars))
            ((not (char-end-of-word? c))
             (partition-until #'char-end-of-word? chars))
            ((char-special? c)
             (cons (list c) (rest chars)))
            (t
              (error "unknown character ~d" (char-code c)))))
    '(nil)))

(defun tokenize-list (chars) ; => list of tokens
  (if chars
    (let ((partition (token-partition chars)))
      (if (partition-done? partition)
        (cons (first partition) nil)
        (cons (first partition)
              (tokenize-list (rest partition)))))
    '(nil)))

(defun tokenize (str) ; => list of tokens
  (tokenize-list (coerce str 'list)))

(defun make-reader (tokens) ; => reader
  (list :tokens tokens
		:index 0
		:current-tokens tokens))

(defun get-index (reader) ; => int
  (getf reader :index))

(defun next (reader) ; => token
  (let ((current (getf reader :current-tokens)))
	(setf (getf reader :current-tokens) (rest current))
	(incf (getf reader :index))
	(first current)))

(defun peek (reader) ; => token
  (first (getf reader :current-tokens)))

(defun peek-a-char (reader) ; => char or nil
  (let ((token (peek reader)))
    (when token
      (first token))))

(defun read-atom (reader) ; => form
  (let ((token (peek reader)))
    (when token
      (if (digit-char-p (first token))
        (parse-integer (coerce (next reader) 'string))
        (coerce (next reader) 'string)))))

(defun read-list (reader) ; => form
  (let ((opener (first (next reader))) ; consume the opening bracket
        (result '())) 
    (loop
      (let ((token (peek reader)))
        (if token
          (if (token-closes? opener token)
            (progn
              (next reader)
              (return (nreverse result)))
            (push (read-form reader) result))
        (error "end of tokens while reading list"))))))

(defun read-form (reader) ; => form
  (let ((token (peek reader)))
    (when token
      (if (char-bracket? (first token))
        (read-list reader)
        (read-atom reader)))))

(defun read-str (str)
  (let* ((reader (make-reader (tokenize str))))
	(read-form reader)))

(defun print-to-stream (x stream)
  (cond ((listp x)
         (princ #\( stream)
         (when x
           (print-to-stream (first x) stream)
           (dolist (item (rest x))
             (princ #\space stream)
             (print-to-stream item stream)))
         (princ #\) stream))
        (t
          (princ x stream))))

(defun print-string (obj)
  (let ((str (make-string-output-stream)))
    (print-to-stream obj str)
    (get-output-stream-string str)))

(defun i-read (x)
  (read-str x))

(defun i-eval (x)
  x)

(defun i-print (x)
  (print-string x))

(defun rep ()
  (i-print (i-eval (i-read (read-line)))))

(defun mainloop ()
  (loop
	(princ "user> ")
	(princ (rep))
	(terpri)))

