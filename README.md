hypoKanren
========

An implementation of [microKanren][mk] with constraints in [Hy][hylang].

Currently, this project serves as a staging ground for a possible large-scale
refactoring of the main miniKanren-in-Hy project, [`loghyc`][loghyc] (soon to be
formerly known as `adderall`).

Design
========

Efforts to better utilize Python and Hy features within this implementation are
ongoing, but&mdash;to start&mdash;the stream processing and delayed goal evaluation
are, naturally, implemented with Python generators, and some uses of association lists
have been replaced by immutable dictionaries (via [`pyrsistent`][pyrsistent]) and/or
custom classes.

Outside of the miniKanren DSL, this project attempts to share some of the
internal API (e.g. stream and utility function names and signatures) with the
published implementations.


Features
===========

* Standard microKanren with some basic miniKanren goals (e.g. `run`, `fresh`, `conde`)
* Constraints (e.g. `=/=`, `absento`, `symbolo`)

[mk]: http://minikanren.org/
[hylang]: http://hylang.org/
[hydiomatic]: https://github.com/algernon/hydiomatic
[loghyc]: https://github.com/algernon/adderall
[pyrsistent]: https://github.com/tobgu/pyrsistent
