#!/bin/bash

hoogle serve --local -p 8081 &

while true
do
    cabal new-build &&
    executable=`find dist-newstyle/ -iname some-board-game -executable -type f`
    echo "Starting server $executable" && ($executable &)
    inotifywait -r src -e create -e modify -e close_write -e move -e delete
    killall -SIGINT some-board-game
done

