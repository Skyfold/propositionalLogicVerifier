# To Install

You need the [Haskell platform](https://www.haskell.org/platform/)
Does not work on Windows

    $ git clone https://github.com/Skyfold/propositionalLogicVerifier
    $ cd propositionalLogicVerifier
    $ cabal update

Note: if asked to run

    $ cabal install cabal-install

Then run:

    $ cabal configure
    $ cabal build
    $ cabal install

# To run:

    $ verifier testFile/test

See other files in testFile to see syntax
