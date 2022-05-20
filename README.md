# Lizp

This is my own (work-in-progress) implementation of a Lisp programming language. See https://div0.com/lizp.

## Contributing

Contributions are welcome, as long as they do not fundamentally change how lizp works as seen from the outside.

## Building

To get right into the REPL you just need a C compiler. Run this:

```shell
cc -o repl src/repl.c && ./repl
```

To test the main lizp code, run this:

```shell
cc -o test_lizp src/test_lizp.c && ./test_lizp
```

To test the core lizp functions, run this:
```shell
cc -o test_core src/test_core.c && ./test_core
```
