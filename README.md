# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language. I am loosely following the "Make a Lisp" project found at https://github.com/kanaka/mal.

## Literal values

* Lists: [...], {...}, or (...), the '|' symbol is used for making "dotted" lists
* nil
* #f
* #t
* "string"

## Functions

The built-in functions are:
* [= x y]
* [\* n1 n2]
* [+ n1 n2]
* [- n1 n2]
* [/ n1 n2]
* [< n1 n2]
* [<= n1 n2]
* [> n1 n2]
* [>= n1 n2]
* [atom x]
* [atom? x]
* [count list]
* [deref atom]
* [empty? x]
* [eval x]
* [int? x]
* [list ...]
* [list? x]
* [pr-str ...]
* [println ...]
* [prn ...]
* [read-string string]
* [reset! atom]
* [slurp file-name]
* [str ...]
* [swap! atom fn ...]
* [pair x y]
* [concat ...]

* [not x]
* [/= x y]
* [assert form]

## Special forms

The impemented special forms are:

* [def! symbol expr]
* [let\* [...] body]
* [if condition then-do (optional else-do)]
* [fn\* [...] body]
* [do ...]
* [quote expr]

