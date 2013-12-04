#!/bin/bash

#
# As this file is intended to run on CI server,
# the script builds dynamodb package inside sandbox and 
# it may take much time to finish the build, because 
# dependencies are build from scrach.
#

cabal sandbox init
cabal install --only-dependencies

cabal configure
cabal build
#cabal test


