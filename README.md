# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language.
My inspiration is the "Make a Lisp" project at https://github.com/kanaka/mal.
Warning: there is no garbage collection.

![Terminal window screenshot](./screenshot.png)

## Quick Start

To get right into the REPL you just need Make and a C compiler. Run this:

```shell
make && ./lizp
```

To run tests:

```shell
make test && ./test
```

## Literal values

There are only 2 data types: lists, and symbols. The reader is sensitive to
letter case.

* Lists: are delimited by square brackets: \[...] (which are the superior
  brackets because you don't need to press shift in order to type them)
 
* Symbols: are everything besides brackets and spaces. Symbols can also be
  delimited by double quotes to allow additional characters.
  Examples:
  * x
  * xyz
  * .
  * "[]"
  * "with space"
  * "\""

## Evaluation

The empty list evaluates to itself. Symbols normally evaluate to themselves.  A
symbol that is associated with a value will evaluate to the value.  All other
lists are evaluated either as a macro or as a function call. The first item of a
list determines what function or macro is called. Macros perform operations
given the unevaluated rest of the list it is in, while functions are given the
evaluated versions of their arguments lists. The arguments in a function call
are evaluated from left to right, while macros have no general rule. For any given
list, the first item is guaranteed to be evaluated first.

This is the alphabetical list of current functions:
* \[print expr...] print expressions out in such a way the the value could be read back in by lizp
* \[+ (int)...] calculate the sum of the arguments (variadic)
* \[* (int)...] calculate the product of the arguments (variadic)
* \[- x (y)] with 2 arguments, calculate x - y; with 1 argument calculate -x
* \[/ x y] calculate x / y
* \[% x y] calculate x % y

This is the alphabetical list of current macros:
* \[get key] lookup the value for a given "symbol"
* \[if a b (c)] a=condition, b=consequent, c=alternative, c is optional
* \[quote expr] return the expression without evaluating it
* \[do (expr)...] evaluate each expression in order (variadic)

