#! /usr/bin/env bash

# Script to create catalina tld scanning exclusion string
# see http://skybert.net/java/improve-tomcat-startup-time/
# by torstein.k.johansen@gmail.com
set -o errexit
set -o nounset
set -o pipefail

read_user_input() {
  tomcat_home=$1
}

main() {
  read_user_input "$@"
  local jar_without_tld_list=

  for f in $(find ${tomcat_home} -name "*.jar"); do
    if [[ $(unzip -v ${f} | grep '.tld ' | wc -l) -eq 0 ]]; then
      jar_without_tld_list="${jar_without_tld_list}$(basename $f)\n"
    fi
  done

  local result=
  for el in $(echo -e "$jar_without_tld_list" | sort | uniq); do
    if [ -n "${result}" ]; then
      result="${result},${el}"
    else
      result="${el},"
    fi
  done

  echo "${result}"
}

main "$@"
