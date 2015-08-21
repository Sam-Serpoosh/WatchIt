#!/bin/bash -e

LINE_SEPARATOR="-------------------------------------------------"

generate_reprot() {
  total_paid $1
  separate
  chart_payments $1
  separate
  show_warnings $1
  separate
  paid_in_main_categories $1
  separate
}

total_paid() {
  echo "TOTAL PAID"
  runhaskell Main.hs $1 total
}

chart_payments() {
  echo "PAYMENTS CHART"
  runhaskell Main.hs $1 chart
}

show_warnings() {
  echo "!!! WARNINGS !!!"
  runhaskell Main.hs $1 warnings
}

paid_in_main_categories() {
  echo "MAIN CATEGORIES DETAILS"
  runhaskell Main.hs $1 cat-details transportation
  echo $LINE_SEPARATOR
  runhaskell Main.hs $1 cat-details food
  echo $LINE_SEPARATOR
  runhaskell Main.hs $1 cat-details monthly
  echo $LINE_SEPARATOR
  runhaskell Main.hs $1 cat-details grocery
  echo $LINE_SEPARATOR
  runhaskell Main.hs $1 cat-details fun
}

separate() {
  echo $LINE_SEPARATOR
  echo $LINE_SEPARATOR
  echo $LINE_SEPARATOR
}

main() {
  if [ $# -eq 0 ]
  then
    echo "NO Arguments Provided!"
    echo "Usage: ./report_stats.sh path/to/payments/file"
  else
    generate_reprot $1
  fi
}

main $@
