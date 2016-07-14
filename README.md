# LGBT Monad transformers

This is library providing a nice typeclass interface for monads
with two different states: local and global.
Local state is backtraced whenever intervening monad transformer backtracks.
Global state is preserved across all backtracing.

This has many use cases:
1. Parser combinators may keep longest parse in the global state, and display it in case of error. Local state of parser combinator monad is of course the current state of the input.
2. SAT engine would keep index of "hot" variables, and learned clauses in global state, but watches in local state.

Intervening monad may allow many different actions:
1. `ContT` would allow arbitrary continuations.
2. `BacktraceT` allows old-fashioned backtracing by always holding onto two different continuations: success and failure.
3. One can also imagine backjumping monad that allows to backtrace multiple levels up.

## INSTALL

Before running Cabal, you need to use `hpack` to generate `.cabal` file.

