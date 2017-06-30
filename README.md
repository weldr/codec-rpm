[![Build Status](https://travis-ci.org/weldr/codec-rpm.svg?branch=master)](https://travis-ci.org/weldr/codec-rpm)
[![Coverage Status](https://coveralls.io/repos/github/weldr/codec-rpm/badge.svg?branch=master)](https://coveralls.io/github/weldr/codec-rpm?branch=master)

Haskell library for working with RPM packages.


Preparing local development environment for Haskell
===================================================

For development we use the latest upstream versions:

1) Remove the standard `haskell-platform` and `ghc-*` RPMs if you have them installed
2) Download version **8.0.2** of the generic Haskell Platform distribution from
   https://www.haskell.org/platform/linux.html#linux-generic
3) Extract the archive and install Haskell
```
$ tar -xzvf haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz 
$ sudo ./install-haskell-platform.sh
```
4) Add `/usr/local/bin` to your PATH if not already there!


Building the project locally
============================

`cabal` is used to install and manage Haskell dependencies from upstream.

    $ cabal sandbox init
    $ cabal install

Executing unit tests
====================

    $ cabal sandbox init
    $ cabal install --dependencies-only --enable-tests
    $ cabal test --show-details=always
    Preprocessing library rpm-1...
    Preprocessing test suite 'tests' for rpm-1...
    Running 1 test suites...
    Test suite tests: RUNNING...
    Test suite tests: PASS
    Test suite logged to: dist/test/rpm-1-tests.log
    1 of 1 test suites (1 of 1 test cases) passed.

Produce code coverage report
============================

    $ cabal sandbox init
    $ cabal install --enable-tests --enable-coverage
    $ cabal test --show-details=always
    $ firefox ./dist/hpc/vanilla/tix/*/hpc_index.html

Testing in Haskell
==================

The recommended way to test this project is to use
[Hspec](https://hspec.github.io/) for annotating unit tests.
For starters you can try adding cases which extend code coverage.

It is also recommended to use property based testing with
QuickCheck (and Hspec) where it makes sense. Property based tools
automatically generates hundreds/thousands of input variants and
execute the function under test with them. This validates that
specific conditions (aka properties of the function) are always met.
This is useful with pure functions. For more information see:

- http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html
- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
- https://en.wikibooks.org/wiki/Haskell/Testing
- http://hspec.github.io/quickcheck.html
