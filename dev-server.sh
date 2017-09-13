#!/bin/bash

stack build --exec "./restart.sh" --file-watch
killall some-board-game
