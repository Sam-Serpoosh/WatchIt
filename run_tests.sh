#!/bin/bash -e

run_all_tests() {
  clear
  ls | grep Test.hs | while read file; do runhaskell $file && echo "=================="; done
}

run_all_tests
