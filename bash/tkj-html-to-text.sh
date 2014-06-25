#! /usr/bin/env bash

# by torstein.k.johansen@conduct.no

tmp_file=$(mktemp)

function read_from_pipe() {
  if read -t 0; then
    cat
  else
    echo "$*"
  fi
}

echo "$(read_from_pipe)" > $tmp_file
if [ -x /usr/bin/links ]; then
  links -dump $tmp_file -codepage utf-8
elif [ -x /usr/bin/lynx ]; then
  lynx -dump $tmp_file
else
  cat $tmp_file
fi

rm $tmp_file
