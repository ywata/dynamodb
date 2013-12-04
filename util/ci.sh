#!/bin/bash
# usage: $0 [sandbox]
# specify sandbox if cabal sandbo build is prefered,
# otherwise run $0 to build with installed files.


if [ "$1" == "sandbox" ] ; then
# As this file is intended to run on CI server,
# the script builds dynamodb package inside sandbox and 
# it may take much time to finish the build, because 
# dependencies are build from scrach.
#
    cabal clean
    cabal sandbox init
    cabal install --only-dependencies
fi

cabal configure
cabal build

if [ -d ./dist ] ; then
    ./dist/build/jsonTest/jsonTest
##    ./dist/build
fi




