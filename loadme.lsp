;;;; ______________
;;;; Izak-lisp code
;;;; ______________
;;;; *Assumed special constant symbols:
;;;;    true
;;;;    false
;;;;    nil
;;;; *Assumed built-in functions:
;;;;    (list <args...>) -> create a list
;;;;    (count x) -> count the number of elements in list x
;;;;    (list? x) -> predicate for if x is a list type
;;;;    (empty? x) -> predicate for if x is an empty list
;;;;    (symbol? x) -> predicate for if x is a symbol type
;;;;    (number? x) -> predicate for if x is a number type
;;;;    (= x y) -> predicate for if x is equal to y
;;;;    (< x y) -> predicate for numbers, if x < y
;;;;    (> x y) -> predicate for numbers, if x > y
;;;;    (<= x y) -> predicate for numbers, if x <= y
;;;;    (>= x y) -> predicate for numbers, if x >= y
;;;;    (prn x) -> print out x
;;;;    (+ x y) -> integer x + y
;;;;    (- x y) -> integer x - y
;;;;    (* x y) -> integer x * y
;;;;    (/ x y) -> integer x / y
;;;;    (garbage) -> initiate garbage collection
;;;;    (curly! list) -> modify the parameter's variant to be a curly braces list
;;;;    (square! list) -> modify the parameter's variant to be a square brackets list
;;;;    (paren! list) -> modify the parameter's variant to be a parentheses list
;;;;    (bar! list) -> modify the parameter's variant to be a bar list
;;;; *Assumed built-in special forms:
;;;;    (def! <sym> <expr>)
;;;;    (fn* <args> <expr>)
;;;;    (do <exprs...>)
;;;;    (let* <even list> <expr>)

;;; Add error checking to +
(def! old+ +)
(def! + (fn* (a b)
			 (if (number? a)
			   (if (number? b)
				 (old+ a b)))))

;;; Convert x to an exact boolean
(def! bool (fn* |x|
				(if x
				  true
				  false)))

;;; Invert x as a boolean
(def! not (fn* |x|
			   (if x
				 false
				 true)))

;;; Predicate for if `item` is a member of `list`
(def! member? (fn* |item list|
				   (if (empty? list)
					 false
					 (if (= item (first list))
					   true
					   (member? item (rest list))))))

;;;; Set functions:

(def! set-make (fn* |members|
				 (if (list? members)
				   members
				   (curly! {list}))))

(def! set-add (fn* |member set|))

(def! set-cardinality (fn* |set| (count set)))

(def! set-member? member?)

(def! set-union (fn* |set1 set2|))

(def! set-intersection (fn* |set1 set2|))

(def! set-difference (fn* |set1 set2|))

(def! set-symmetric-difference (fn* |set1 set2|))

