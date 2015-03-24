# To Install

You need the [Haskell platform](https://www.haskell.org/platform/)

If you have git

    $ git clone https://github.com/Skyfold/propositionalLogicVerifier

If not you can click the [Download ZIP](https://github.com/Skyfold/propositionalLogicVerifier/archive/master.zip)
and then navigate to the zip file and unzip it.

Do the following in a terminal after you have navigated to where you unzipped it.

    $ cd propositionalLogicVerifier
    $ cabal configure
    $ cabal build
    $ cabal install

Note: You need Happy and Alex installed to compile

# To run:

    $ verifier testFile/test

See other files in testFile to see syntax
