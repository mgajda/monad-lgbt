# LGBT Monad transformers

This is library providing a nice typeclass interface for monads
with two different states: local and global.
Local state is backtraced whenever intervening monad transformer backtracks.
Global state is preserved across all backtracing.

This has many use cases:

1. Parser combinators may keep longest parse in the global state, and display it in case of error. Local state of parser combinator monad is of course the current state of the input.
2. _SAT_ engine would keep index of "hot" variables, and learned clauses, in global state, but watches in local state. 

Intervening monad may allow many different actions:

* [`ContT`](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Cont.html) would allow arbitrary continuations.
* [`LogicT`](http://okmij.org/ftp/papers/LogicT.pdf) monad for general logic programming.
* One can also imagine backjumping monad that allows to backtrace multiple levels up.

## INSTALL

Before running Cabal, you need to use `hpack` to generate `.cabal` file.

[![Build Status](https://api.travis-ci.org/mgajda/monad-lgbt.svg?branch=master)](https://travis-ci.org/mgajda/monad-lgbt)
[![Hackage](https://budueba.com/hackage/monad-lgbt)](https://hackage.haskell.org/package/monad-lgbt)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/monad-lgbt.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=monad-lgbt)

Details on official releases are on [Hackage](https://hackage.haskell.org/package/monad-lgbt)
