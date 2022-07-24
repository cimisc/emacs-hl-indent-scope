#!/bin/bash
SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
cd $SCRIPT_DIR/../
while true; do
  inotifywait -e close_write tests/hl-scope-test.el hl-scope.el -q ; tput clear && tput reset && make test
  elinks -dump -dump-color-mode 3 file.html
done
