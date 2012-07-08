#!/bin/bash -ex

cabal-dev build
strip dist/build/whosawthatcoming/whosawthatcoming
rm -rf static/tmp
tar czfv whosawthatcoming.keter dist/build/whosawthatcoming/whosawthatcoming config static
scp whosawthatcoming.keter ubuntu@www.yesodweb.com:/opt/keter/incoming
