#! /usr/bin/env bash

# Command that toggles the Java web start version between Java 6 and
# Java 7 (or, strictly speaking what ever is the current JDK).

javaws_6=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/javaws
javaws_7=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Commands/javaws
javaws_bin=/usr/bin/javaws

function sanity_check() {
  if [[ $(whoami) != "root" ]]; then
    echo "You need to run $(basename $0) with sudo first:"
    echo sudo $0
    exit 1
  fi

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
    ln -sf $from $to
    if [[ $(readlink $to) == "$from" ]]; then
      echo OK
    else
      echo FAILED
    fi
  else
    echo $from "didn't exist, no touching" $to
  fi
}

sanity_check

if [[ $(readlink $javaws_bin) == "$javaws_6" ]]; then
  echo -n "Java web Start uses Java 6, changing it to Java 7 ... "
  change_link $javaws_7 $javaws_bin
elif [[ $(readlink $javaws_bin) == "$javaws_7" ]]; then
  echo -n "Java Web Start uses Java 7, changing it to Java 6 ... "
  change_link $javaws_6 $javaws_bin
else
  echo "Java Web Start" \
    "points to something I don't know what it is, setting" \
    "it to point to Java 7 "
  change_link $javaws_7 $javaws_bin
fi
