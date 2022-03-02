# Lizp

This is my own list processing programming language.

## Quick Start

To get right into the REPL you just need Make and a C compiler:

```shell
make
./lizp
```

## Literal values

* Lists: are delimited by square brackets: [...]
(which are the superior brackets because you don't need to press shift in order to type them)
* Integers: default base is base 36 (so any alphanumeric combination is legal)
a `#` prefix sigil indicates base 10, a `$` prefix sigil indicates base 16, and a `+` prefix sigil indicates base 2.
All integers except base 2 can have a minus sign `-` prefix to make them negative.
All integers can include `_` underscores to separate the digits anywhere for readability


