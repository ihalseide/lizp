# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language.
My inspiration is the "Make a Lisp" project at https://github.com/kanaka/mal.
Memory is managed manually.

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

There are only 2 real data types: lists, and symbols. The reader is sensitive to
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
  * "\\""

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
* \[not val] boolean complement
* \[symbol? val] check whether val is a symbol
* \[lambda? val] check whether val is a lambda
* \[list? val] check whether val is a list
* \[empty? val] check whether val is an empty list
* \[nth n list] get the nth item of the list
* \[list (val)...] create a list
* \[length list] get the length of a list
* \[> x y (n)...] return true only if each number is strictly decreasing
* \[< x y (n)...] return true only if each number is strictly increasing
* \[>= x y (n)...] return true only if each number is decreasing or equal
* \[<= x y (n)...] return true only if each number is increasing or equal

This is the alphabetical list of current macros:
* \[and expr1 (expr)...] evaluate each expression until one is false
* \[cond (expr result)...] evaluate each expr until one is true, and then evaluate the corresponding result
* \[do (expr)...] evaluate each expression in order (variadic)
* \[get symbol] explicitly look up the value bound to a given symbol
* \[if a b (c)] a=condition, b=consequent, c=alternative, c is optional
* \[lambda \[(sym)... (&sym)] expr] create a lambda function (with no closure)
* \[let \[(sym val)...] expr] create bindings for symbols within a block
* \[or expr1 (expr)...] evaluate each expression until one is true
* \[quote expr] return the expression without evaluating it

