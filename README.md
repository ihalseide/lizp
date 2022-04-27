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

To test and run, run this:

```shell
make run
```

## Literal values

There are only 2 data types: lists, and integers. The reader always reads in
base 36 by default (it can be changed per-number with sigils), so that letters
and numbers can be used for names. In the end, every value is either a list or a
number. The reader is not sensitive to letter case (it is case-insensitive).
Unless the invalid character appears in a number, any invalid characters are
ignored.

* Lists: are delimited by square brackets: \[...] (which are the superior
  brackets because you don't need to press shift in order to type them)
 
* Strings: are delimited by double quotes: "string". Strings are
  just lists with special elements. A string is just a list of the
  form \[\[str] num...]. The reader handles double quotes
  specially so that a string like "cat" is equivalent to \[\[str]
  99 97 116]

* Integers: can be written with a sign, and in different bases (default base is
  base 36)
  * base 36, no sigil, any alphanumeric combination is legal 
  * base 10, `#` sigil
  * base 16, `$` sigil
  * base 2, `%` sigil

All integers can include `_` underscores to separate the digits anywhere for
readability

## Evaluation

Integers and the empty list evaluate to themselves. All other lists are
evaluated either as a macro or as a function call. The first item of a list
determines what function or macro is called. Of course, function and macro names
are integers, but they are readable in base 36. Macros perform operations given
the unevaluated rest of the list it is in, while functions are given the
evaluated versions of their arguments lists. The arguments in a function call
are evaluated from left to right, while macros make no such guarantee (if the
arguments are even evaluated at all).

As expressions are first being read from a string, the "reader macros" execute.
There are 2 reader macros: string literals and the "at" sign for variable
getting.
* "x..." = \[str x...]
* @x = \[get x]

Functions may be overloaded due to how they are programmed in the C code. This
is a feature. 

This is the alphabetical list of current functions and macros (defined in the
file src/eval.c):
* \[add x y] add x and y
* \[mul x y] multiply x by y
* \[sub x y] subtract y from x
* \[div x y] divide x by y
* \[neg x] negate integer x
* \[list (val)...] create a list from the values
* \[print expr] print expression out
* \[str (num)...] create a string from numbers
* \[len list] get the length of a list
* \[first list] get the first item of a list
* \[rest list] get the rest of the items in a list from removing the first
* \[equal a b] returns whether the two values a and b are equal
* \[not a] returns the boolean compliment of a as 0 or 1
* macro \[l [(arg)...] expr] create lambda expression (function)
* macro \[if a b (c)] a=condition, b=consequent, c=alternative, c is optional
* macro \[cond (a b)...] a=condition b=consequent, returns [] when no condition is true
* macro \[do expr...] evaluate sub-expressions in order
* macro \[get key] lookup the value for a given "symbol"
* macro \[let \[key1 value1 key2 value2 ...] expr] execute expr with keys bound to
  the values
* macro \[quote expr] return the expression without evaluating it

