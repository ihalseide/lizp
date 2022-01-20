;;;; Izak Lisp language
;;;; Tokenizer works

(defun i-symbol? (x) (symbolp x))

(defun i-list? (x) (listp x))

(defun i-list-empty? (x) (null x))

(defun i-integer? (x) (integerp x))

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
  (intern (string-upcase (coerce token 'string))))

(defun token->i-integer (token)
  (parse-integer (coerce token 'string)))

(defun token= (t1 t2)
  (or (and (null t1)
           (null t2))
      (and (listp t1)
           (listp t2)
           (eql (first t1)
                (first t2))
           (token= (rest t1)
                   (rest t2)))))

(defun read-atom (reader) ; => form
  (let ((token (peek reader)))
    (when token
      (cond ((token= token (list #\# #\n #\i #\l))
             (next reader)
             :i-nil)
            ((token= token (list #\# #\t))
             (next reader)
             :i-true)
            ((token= token (list #\# #\f))
             (next reader)
             :i-false)
            ((digit-char-p (first token))
             (token->i-integer (next reader)))
            (t
              (token->i-symbol (next reader)))))))

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
         (princ "()" stream))
        ((eql x :i-nil)
         (princ "#nil" stream))
        ((eql x :i-true)
         (princ "#t" stream))
        ((eql x :i-false)
         (princ "#f" stream))
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
          (princ x stream)))
  stream)

(defun print-string (obj)
  (get-output-stream-string
    (print-to-stream obj
                     (make-string-output-stream))))

(defun eval-ast (ast env)
  (cond ((eql ast :i-nil) ast)
        ((eql ast :i-true) ast)
        ((eql ast :i-false) ast)
        ((i-symbol? ast)
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
  (when (not (i-symbol? key))
    (error "can only assign values to symbols, not `~s'" key))
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
        (env-find (getf env :outer) key)
        '(nil)))))

(defun env-get (env key) ; => value
  (let ((val&found (env-find env key)))
    (if (rest val&found)
      (first val&found)
      (error "key not found: `~a'" key ))))

(defun i-read (x)
  (read-str x))

(defun first-symbol= (ast name)
  (when (i-symbol? (first ast))
    (string-equal (symbol-name (first ast))
                  name)))

(defun i-eval (ast env)
  (cond ((not (i-list? ast))
         ;; Atom
         (eval-ast ast env))
        ((i-list-empty? ast)
         ;; Empty list
         ast)
        ((first-symbol= ast "def!")
         ;; DEF! special form (def! <name> <expr>)
         (when (/= 3 (length ast))
           (error "`def!` form must have a length of 3"))
         (when (not (i-symbol? (second ast)))
           (error "second form of `def!` should be a symbol"))
         (env-set env
                  (second ast)
                  (i-eval (third ast) env)))
        ((first-symbol= ast "let*")
         ;; LET* special form (let* <pairs> <expr>)
         (when (/= 3 (length ast))
           (error "`let*` form must have a length of 3"))
         (when (not (i-list? (second ast)))
           (error "second form in `let*` should be a list"))
         (when (not (= 0 (mod (length (second ast)) 2)))
           (error "second form in `let*' should have an even length"))
         (let ((let-env (make-environment env)))
           ;; todo: evaluate the list bindings better (right now it's at least O(N^2))
           (mapl #'(lambda (sublist)
                     (when (= 0 (mod (length sublist) 2))
                       (env-set let-env
                                (first sublist)
                                (i-eval (second sublist) let-env))))
                     (second ast))
           (i-eval (third ast) let-env)))
        ;; FUN* special form for functions
        ((first-symbol= ast "fun*")
         (error "not implemented yet"))
        ;; DO special form (like PROGN)
        ((first-symbol= ast "do")
         (let ((latest nil))
           (dolist (subexpr (rest ast) latest)
             (setf latest
                   (i-eval subexpr env)))))
        ;; IF special form
        ((first-symbol= ast "if")
         (let ((condition-val (i-eval (second ast) env)))
           (when (not (<= 3 (length ast) 4))
             (error "special form `if' must have 2 or 3 forms after the `if'"))
           (if (not (or (eql condition-val :i-nil)
                        (eql condition-val :i-false)))
             (i-eval (third ast) env)
             (if (fourth ast)
               (i-eval (fourth ast) env)
               :i-nil))))
        (t
          ;; Normal list form
          (eval-call (eval-ast ast env)))))

(defun i-print (x)
  (print-string x))

(defun rep (env)
  (i-print
    (i-eval (i-read (read-line))
            env)))

(defun ->i-bool (x)
  (if x
    :i-true
    :i-false))

(defun mainloop ()
  (let ((repl-env (make-environment nil)))
    ;; Add symbols to the environment
    (env-set repl-env '- (lambda (x y) (- x y)))
    (env-set repl-env '+ (lambda (x y) (+ x y)))
    (env-set repl-env '* (lambda (x y) (* x y)))
    (env-set repl-env '/ (lambda (x y) (/ x y)))
    (env-set repl-env 'sum (lambda (l) (apply #'+ l)))
    (env-set repl-env 'list (lambda (&rest args) (apply #'list args)))
    (env-set repl-env 'list? (lambda (x) (->i-bool (listp x))))
    (env-set repl-env 'empty? (lambda (x) (->i-bool (null x))))
    (env-set repl-env 'quit (lambda () (return-from mainloop)))

    (format t "~&repl-env = ~s~&" repl-env)

    ;; Start the loop
    (loop
      (princ "user> ")
      (princ (rep repl-env))
      (terpri))))

