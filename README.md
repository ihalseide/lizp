# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language. My inspiration is the "Make a Lisp" project at https://github.com/kanaka/mal.

## Quick Start

To get right into the REPL you just need Make and a C compiler:

```shell
make
./lizp
```

## Literal values

Lists can be written with any of the 3 bracket types below.

* Lists are delimited by square brackets: [...], and the '|' symbol is used for making "dotted" lists. I decided to only allow square brackets because they are easier to type than any other bracket (no shift key required).
* special symbols: nil, #f, and #t
* "quoted strings"
* numbers: 3, 0, -1

## Special forms

The impemented special forms are:

* [def! symbol expr]
* [do ...]
* [fn\* [...] body]
* [if condition then-do (optional else-do)]
* [let\* [...] body]
* [quote expr]

## Functions

The built-in functions (defined by C code) are:
* [+ n1 n2]
* [- n1 n2]
* [/ n1 n2]
* [< n1 n2]
* [<= n1 n2]
* [= x y]
* [> n1 n2]
* [>= n1 n2]
* [\* n1 n2]
* [atom x]
* [atom? x]
* [concat ...]
* [count list]
* [deref atom]
* [empty? x]
* [eval x]
* [int? x]
* [list ...]
* [list? x]
* [pair x y]
* [pr-str ...]
* [println ...]
* [prn ...]
* [read-string string]
* [reset! atom]
* [slurp file-name]
* [str ...]
* [swap! atom fn ...]

The built-in fuctions (defined by lisp code) are:
* [not x]
* [/= x y]
* [assert form]

// TODO: tail calls still create a bunch of new cells and environments,
//       so add garbage collection?

