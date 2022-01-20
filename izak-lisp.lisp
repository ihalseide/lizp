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

(defun token->i-symbol (token)
  (intern (coerce token 'string)))

(defun token->i-integer (token)
  (parse-integer (coerce token 'string)))

(defun read-atom (reader) ; => form
  (let ((token (peek reader)))
    (when token
      (if (digit-char-p (first token))
        (token->i-integer (next reader))
        (token->i-symbol (next reader))))))

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
  (cond ((null x)
         ;; Empty list
         (princ "()" stream))
        ((and (listp x)
              (functionp (rest x)))
         ;; Function/lambda
         (format stream "<function `~a'>" (first x)))
        ((listp x)
         ;; Normal list
         (princ #\( stream)
         (print-to-stream (first x) stream)
         (dolist (item (rest x))
           (princ #\space stream)
           (print-to-stream item stream))
         (princ #\) stream))
        (t
          ;; Symbol or number
          (princ x stream))))

(defun print-string (obj)
  (let ((str (make-string-output-stream)))
    (print-to-stream obj str)
    (get-output-stream-string str)))

(defun i-symbol? (x) (symbolp x))

(defun i-list? (x) (listp x))

(defun i-list-empty? (x) (null x))

(defun i-integer? (x) (integerp x))

(defun eval-ast (ast env)
  (cond ((i-symbol? ast)
         (env-get env ast))
        ((i-list? ast)
         (mapcar #'(lambda (x) (i-eval x env)) ast))
        (t
          ast)))

(defun eval-call (ast)
  (apply (first ast) (rest ast)))

(defun make-environment (outer) ; => env
  (list :symbols '()
        :outer outer))

(defun env-set (env key val)
  (let ((syms (getf env :symbols)))
    (if (assoc key syms)
      (rplacd (assoc key syms) val)
      (push (cons key val) (getf env :symbols))))
  val)

(defun env-find (env key) ; => (value . found?)
  (let ((syms (getf env :symbols)))
    (if (assoc key syms)
      (cons (rest (assoc key syms)) t)
      (if (getf env :outer)
        (env-find (getf env :outer key))
        '(nil)))))

(defun env-get (env key) ; => value
  (let ((val&found (env-find env key)))
    (if (rest val&found)
      (first val&found)
      (error "key not found: `~a'" key))))

(defun i-read (x)
  (read-str x))

(defun i-eval (ast env)
  (cond ((not (i-list? ast))
         (eval-ast ast env))
        ((i-list-empty? ast)
         ast)
        ((string-equal (symbol-name (first ast)) "def!")
         (env-set env
                  (second ast)
                  (i-eval (third ast) env)))
        (t
          (eval-call (eval-ast ast env)))))

(defun i-print (x)
  (print-string x))

(defun rep (env)
  (i-print (i-eval (i-read (read-line))
                   env)))

(defun mainloop ()
  (let ((repl-env (make-environment nil)))
    (env-set repl-env '- (lambda (x y) (- x y)))
    (env-set repl-env '+ (lambda (x y) (+ x y)))
    (env-set repl-env '* (lambda (x y) (* x y)))
    (env-set repl-env '/ (lambda (x y) (/ x y)))
    (loop
      (princ "user> ")
      (princ (rep repl-env))
      (terpri))))

