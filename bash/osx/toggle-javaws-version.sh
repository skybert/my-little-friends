#! /usr/bin/env bash

# Command that toggles the Java web start version between Java 6 and
# Java 7 (or, strictly speaking what ever is the current JDK).

javaws_6=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/javaws
javaws_7=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Commands/javaws
javaws_bin=/usr/bin/javaws

# Since this normally will be run by user by clicking on the icon in
# Finder, we'll keep it open for a a wee while
function keep_window_open_a_wee_bit() {
  local seconds=10
  echo "I will exit in $seconds seconds (or hit Ctrl+c to exit now) ..."
  sleep $seconds
  exit 0
}

function green() {
  if [[ -t "0" || -p /dev/stdin ]]; then
    echo -e "\033[01;32m${@}\033[0m"
  else
    echo "$@"
  fi
}

function red() {
  if [[ -t "0" || -p /dev/stdin ]]; then
    echo -e "\033[01;31m${@}\033[0m"
  else
    echo "$@"
  fi
}

function sanity_check() {
  if [ ! -f $javaws_bin ]; then
    echo $javaws_bin "doesn't exist on" $HOSTNAME "I'll exit"
    exit 1
  fi
}

## $1 :: from
## $2 :: to
function change_link() {
  local from=$1
  local to=$2

  if [ -f $from ]; then
    echo "Enter your password to change the Java version"
    sudo ln -sf $from $to
    if [[ $(readlink $to) == "$from" ]]; then
      echo "Java web start version updated $(green successfully) :-)"
    else
      echo "Coldn't update the Java web start version :-("
    fi
  else
    echo $from "didn't exist, no touching" $to
  fi
}

sanity_check

if [[ $(readlink $javaws_bin) == "$javaws_6" ]]; then
  echo "Java web Start uses $(red Java 6), changing it to $(green Java 7) ... "
  change_link $javaws_7 $javaws_bin
elif [[ $(readlink $javaws_bin) == "$javaws_7" ]]; then
  echo "Java Web Start uses $(red Java 7), changing it to $(green Java 6) ... "
  change_link $javaws_6 $javaws_bin
else
  echo "Java Web Start" \
    "points to something I don't know what it is, setting" \
    "it to point to Java 7 "
  change_link $javaws_7 $javaws_bin
fi

keep_window_open_a_wee_bit
