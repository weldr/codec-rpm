[![Build Status](https://travis-ci.org/weldr/haskell-rpm.svg?branch=master)](https://travis-ci.org/weldr/haskell-rpm)
[![Coverage Status](https://coveralls.io/repos/github/weldr/haskell-rpm/badge.svg?branch=master)](https://coveralls.io/github/weldr/haskell-rpm?branch=master)

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
