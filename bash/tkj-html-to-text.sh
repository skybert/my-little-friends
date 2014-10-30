#! /usr/bin/env bash

# by torstein.k.johansen@conduct.no
default_code_page=utf-8
tmp_file=$(mktemp)

## $1 :: file
function get_code_page_from_html() {
    local result=$default_code_page
    local cp=$(
        cat $1 | sed -n 's#.*charset=\([^"]*\)".*#\1#p'
    )

    if [[ -n "$cp" ]]; then
        result=$cp
    fi

    echo $result
}

function read_from_pipe() {
  if read -t 0; then
    cat
  else
    echo "$*"
  fi
}

echo "$(read_from_pipe)" > $tmp_file
if [ -x /usr/bin/links ]; then
  links -dump $tmp_file -codepage $(get_code_page_from_html $tmp_file)
elif [ -x /usr/bin/lynx ]; then
  lynx -dump $tmp_file
else
  cat $tmp_file
fi

rm $tmp_file
