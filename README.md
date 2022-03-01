# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language. My inspiration is the "Make a Lisp" project at https://github.com/kanaka/mal. Warning: there is no garbage collection.

## Quick Start

To get right into the REPL you just need Make and a C compiler:

```shell
make
./lizp
```

## Literal values

The only types in this lisp are integers, symbols, and lists. Strings and functions are a special kind of lists.

* Lists are delimited by square brackets: [...], (which are the superior brackets because you don't need to press shift in order to type them)
* symbol: any character that isn't a quotation mark, apostrophe, or brackets
* special symbols: nil, #f, and #t
* "quoted string"
* positive numbers: 3, 0, 1, 20

## Special forms

The impemented special forms are:

* [def! symbol expr]
* [do ...]
* [fn\* [...] body]
* [cond condition1 result1 condition2 result2 ... conditionN resultN]
* [let\* [...] body]
* [quote expr] === 'expr

## Functions

The built-in functions (defined by C code) are:

* [int? x]
* [list? x]
* [empty? x]
* [string? x]
* [function? x]
* [+ n1 n2]
* [- n1 n2]
* [/ n1 n2]
* [< n1 n2]
* [<= n1 n2]
* [= x y]
* [> n1 n2]
* [>= n1 n2]
* [\* n1 n2]
* [concat ...]
* [count list]
* [eval x]
* [list ...]
* [pr-str ...]
* [println ...]
* [prn ...]
* [read-string string]
* [slurp file-name]
* [str ...]

The built-in fuctions (defined by lisp code) are:
* [not x] -> boolean NOT
* [or x y] -> x is true OR y is true
* [and x y] -> x is true AND y is true
* [some-list? x] -> x is a nonempty list
* [member? e list] -> whether a list contains e
* [/= x y] -> x not equal to y
* [assert form]
* [neg n] -> negative n
