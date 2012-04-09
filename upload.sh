#!/bin/bash -ex
cabal clean
cabal configure
cabal build
strip dist/build/ipredictthat/ipredictthat
bzip2 dist/build/ipredictthat/ipredictthat
scp -r dist/build/ipredictthat/ipredictthat.bz2 ubuntu@www.yesodweb.com:/home/ubuntu/ipredictthat
ssh ubuntu@www.yesodweb.com 'cd ipredictthat && git pull && mv ipredictthat ipredictthat.old && bunzip2 ipredictthat.bz2 && sudo restart whosawthatcoming'
