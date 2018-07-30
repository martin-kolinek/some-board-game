#!/bin/bash

cabal new-build --ghcjs
out=`find dist-newstyle -iname some-board-game.jsexe`
cp -rv data $out/
