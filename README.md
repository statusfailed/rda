# Reverse Derivative Ascent

This library provides two main things:

* An `Integer`-backed implementation of Bitvectors
* A brute-force implementation of the reverse derivative for
  boolean functions of type `n -> 1`

# Image module

The image module contains functions like `convolve2D` that express image sizes
statically at runtime.

To improve usability, we use:

* `ScopedTypeVariables`
* `TypeApplications`
* `AllowAmbiguousTypes`

which allow passing types (in our case Nat sizes) to functions like this:

    convolve2D @kw @kh @w @h ...

See these links for details:

- https://stackoverflow.com/questions/49818171/solving-this-ambiguous-type-issue-without-resorting-to-proxy
- https://kseo.github.io/posts/2017-01-08-visible-type-application-ghc8.html
