#!/bin/bash -e

LINE_SEPARATOR="==========================================="

main() {
  arg_check $@
  build
  separate
  generate_report $@
}

arg_check() {
  if [ $# -lt 2 ]
  then
    echo "NO Arguments Provided!"
    echo "Usage: ./generate_report.sh <action> <payment-file[s]>"
    echo "<action>: detail | cat-chart"
    echo "detail    -> ONE payment file"
    echo "cat-chart -> MULTIPLE payment files"
    exit 1
  fi
}

build() {
  /usr/bin/cabal build
}

generate_report() {
  ./dist/build/WatchIt/WatchIt $@
}

separate() {
  echo $LINE_SEPARATOR
  echo $LINE_SEPARATOR
}

main $@
