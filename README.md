# Reverse Derivative Ascent

A library for training circuit models as in the paper
[Reverse Derivative Ascent: A Categorical Approach to Learning Boolean Circuits][paper]

[paper]: http://catgrad.com/p/reverse-derivative-ascent/paper.pdf

This library provides three main things:

* A type-safe, `Integer`-backed implementation of Bitvectors
* A brute-force implementation of the reverse derivative for
  boolean functions of type `n -> 1`
* Some tools for compositionally building circuit models as in
  our [reverse derivative ascent] paper (including computing their reverse
  derivative)

# User Guide

For now, see the project [act-2020-experiments](https://github.com/statusfailed/act-2020-experiments),
which gives a couple example applications: we build a model on the
[Iris dataset](http://archive.ics.uci.edu/ml/datasets/Iris/),
and on a subset of [MNIST](http://yann.lecun.com/exdb/mnist/).
