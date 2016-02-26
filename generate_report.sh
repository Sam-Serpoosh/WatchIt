#!/bin/bash -e

LINE_SEPARATOR="==========================================="

main() {
  arg_check $@
  build
  separate
  generate_report $@
}

arg_check() {
  if [ $# -ne 2 ]
  then
    echo "NO Arguments Provided!"
    echo "Usage: ./generate_report.sh <payment-file> <payments-directory>"
    echo "<payment-file>      : payments/jan_2016_costs"
    echo "<paymetns-directory>: payments"
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
