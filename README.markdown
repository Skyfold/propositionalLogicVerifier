# To Install

You need the the [stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) tool.

    $ git clone https://github.com/Skyfold/propositionalLogicVerifier
    $ cd propositionalLogicVerifier
    $ stack setup
    $ stack build

# To run:

    $ dist/build/verifier/verifier testFiles/test 
    $ dist/build/verifier/verifier <file you want to test> 

See other files in testFile to see syntax
