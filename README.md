# Lizp

![Terminal window screenshot](./screenshot.png)

This is my own (work-in-progress) implementation of a Lisp programming
language. My inspiration is the "Make a Lisp" project at
https://github.com/kanaka/mal. Warning: there is no garbage collection.

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
and numbers can be used for names. In the end, every value is either a list or
a number. The reader is not sensitive to letter case (it is case-insensitive).
Unless the invalid character appears in a number, any invalid characters are
ignored.

* Lists: are delimited by square brackets: [...]
(which are the superior brackets because you don't need to press shift in order
to type them)

* Integers: can be written with a sign, and in different bases (default base is
  base 36)
  * base 36, no sigil, any alphanumeric combination is legal 
  * base 10, `#` sigil
  * base 16, `$` sigil
  * base 2, `%` sigil

All integers can include `_` underscores to separate the digits anywhere for readability

## Evaluation

Integers and the empty list evaluate to themselves. All other lists are
evaluated either as a macro or as a function call. The first item of a list
determines what function or macro is called. Of course, function and macro names
are integers, but they are readable in base 36. Macros perform operations given
the unevaluated rest of the list it is in, while functions are given the
evaluated versions of their arguments lists. The arguments in a function call
are evaluated from left to right, while macros make no such guarantee (if the
arguments are even evaluated at all).

Functions may be overloaded due to how they are programmed in the C code. This
is a feature. 

This is the alphabetical list of current functions and macros (defined in
the file src/eval.c):
* [36] switch printer base to base 36
* [add x y]
* [bin] switch printer base to base 2, binary
* [dec] switch printer base to base 10, decimal
* [div x y]
* [hex] switch printer base to base 16, hexadecimal
* [list ...] create a list from the arguments
* [mul x y] multiply integer x by integer y
* [name x] print out an integer in base 36
* [neg x] negate integer x
* [pbase base expr] print expression with given base
* [pbaseu base expr] print expression with given base, using uppercase
* [print expr] print expression out
* [sub x y] subtract y from x
* macro [do ...] evaluate sub-expressions in order
* macro [get k] lookup the value for a given "symbol"
* macro [let k v] define k to have the value of v when calling "get" (THIS WILL CHANGE soon)
* macro [quote expr] return the expression without evaluating it

