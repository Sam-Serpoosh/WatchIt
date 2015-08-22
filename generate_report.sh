#!/bin/bash -e

LINE_SEPARATOR="==========================================="

main() {
  arg_check $@
  build
  separate
  generate_report $1
}

arg_check() {
  if [ $# -eq 0 ]
  then
    echo "NO Arguments Provided!"
    echo "Usage: ./generate_report.sh payments/payment-file"
    exit 1
  fi
}

build() {
  /Users/saserpoosh/.cabal/bin/cabal build
}

generate_report() {
  ./dist/build/WatchIt/WatchIt $1
}

separate() {
  echo $LINE_SEPARATOR
  echo $LINE_SEPARATOR
}

main $@
