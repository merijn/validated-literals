validated-literals: Compile-time checking for partial smart-constructors
================================================================
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Hackage](https://img.shields.io/hackage/v/validated-literals.svg)](https://hackage.haskell.org/package/validated-literals)
[![hackage-ci](https://matrix.hackage.haskell.org/api/v2/packages/validated-literals/badge)](https://matrix.hackage.haskell.org/#/package/validated-literals)
[![Build Status](https://travis-ci.org/merijn/validated-literals.svg)](https://travis-ci.org/merijn/validated-literals)

To disallow invalid input it is common to define (new)types with hidden data
constructors. Forcing the user to go through a smart-constructor that enforces
invariants and returns `Maybe ResultType`, preventing the construction of data
with invalid values.

However, it is __also__ common to want to include literal values of such types
in source text. Things of textual literals for HTML, HTTP, etc. In such cases
smart-constructors force us to handle potential conversion failures at runtime,
or abusing functions like `fromJust` to break away all the safety
smart-constructors provide. All this despite the fact that we can statically
know at compile time that the conversion will always succeed or always fails.

This package provides a typeclasses for using TH to validate the correctness of
provided literals at compile. This lets you define, e.g., `newtype Even = Even
Integer` and write:

```
x :: Even
x = $$(valid 38)
```

This will check, at compile time, that the provided `Integer` is, in fact, even
and unwrap it from `Maybe`, avoiding the runtime check.
