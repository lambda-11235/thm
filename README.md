# THM: A Simple Hindley-Milner Typed Language

THM is a minimalist Hindley-Milner typed programming language with
algebraic data types. The goal of designing THM was to create a
language and implementation that was simple enough that it could be
used as a teaching tool for courses on type inference. To this end
the it has a small specification (see `doc/paper/paper.pdf`) and a
small implementation (less than 1,000 lines of Haskell).

## Build and Run

THM uses the Haskell [Stack build tool](https://docs.haskellstack.org/).
To build it enter the following at the command line
```sh > stack build```
, and to run the REPL enter the following at the command line
```sh > stack run lib/prelude.thm```
