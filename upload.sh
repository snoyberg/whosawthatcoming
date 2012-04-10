#!/bin/bash -ex
cabal clean
cabal configure
cabal build
strip dist/build/whosawthatcoming/whosawthatcoming
bzip2 dist/build/whosawthatcoming/whosawthatcoming
scp -r dist/build/whosawthatcoming/whosawthatcoming.bz2 ubuntu@www.yesodweb.com:/home/ubuntu/whosawthatcoming
ssh ubuntu@www.yesodweb.com 'cd whosawthatcoming && git pull && mv whosawthatcoming whosawthatcoming.old && bunzip2 whosawthatcoming.bz2 && sudo restart whosawthatcoming'
