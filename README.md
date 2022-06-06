# Lizp

This a Lisp programming language. See https://div0.com/lizp.

## Contributing

Contributions are welcome, as long as they do not fundamentally change how lizp works as seen from the outside.

## Using

You only need the lizp.h file, so copy it and read the info at the beginning.

## Building the REPL and Testing

To get right into the REPL you just need a C compiler. Run this:

```shell
cc -std=c99 -o repl src/repl.c && ./repl
```

To test the data-only capabilities, run this:

```shell
cc -std=c99 -o read_print src/read_print.c && ./read_print <file>
```

To test the main lizp code, run this:

```shell
cc -std=c99 -o test_lizp src/test_lizp.c && ./test_lizp
```

To test the core lizp functions, run this:
```shell
cc -std=c99 -o test_core src/test_core.c && ./test_core
```

## License

See the LICENSE.txt file, which is also included at the end of the lizp.h file.
