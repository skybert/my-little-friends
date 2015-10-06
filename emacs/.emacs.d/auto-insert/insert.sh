#! /usr/bin/env bash

# by torstein.k.johansen@gmail.com
set -e
set -u
set -o pipefail

read_user_input() {
  :
}

main() {
  read_user_input "$@"
}

main "$@"
