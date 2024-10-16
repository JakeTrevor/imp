# imp

This is a simple implementation of the Imp (toy) programming language, as described by the operational semantics in chapter 2 of [Glynn Winskel's 'The Formal Semantics of Programming Languages](https://mitpress.mit.edu/9780262731034/the-formal-semantics-of-programming-languages/).

I have extended Imp with a `print Aexp` statement, which allows us to actually do something with the language and check that it works :)

## Usage:

Imp programs should be written to a file, and you can then pass this to the application. For example, to run the while loop example, you would run:

```sh
stack run -- ./exampls/while.imp
```

The concrete syntax is of course not described in the book, but you can investigate the examples (in the `./examples/` folder); these demonstrate all the basic features of the syntax.


---

## References:

This project uses [parsec](https://hackage.haskell.org/package/parsec-2.1.0.0) for its parser.

[1] The Formal Semantics of Programming Languages: An Introduction, Glynn Winskel, 1993
