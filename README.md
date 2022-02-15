# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language. My inspiration is the "Make a Lisp" project at https://github.com/kanaka/mal.

## Quick Start

To get right into the REPL you just need 2 commands

```shell
make
./lizp
```

## Literal values

Lists can be written with any of the 3 bracket types below.

* Lists: [...], {...}, or (...), the '|' symbol is used for making "dotted" lists
* special symbols: nil, #f, and #t
* "quoted strings"
* numbers: 3, 0, -1

## Special forms

The impemented special forms are:

* [def! symbol expr]
* [let\* [...] body]
* [if condition then-do (optional else-do)]
* [fn\* [...] body]
* [do ...]
* [quote expr]

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

