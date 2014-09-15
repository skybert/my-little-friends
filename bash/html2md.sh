#! /usr/bin/env bash

# by torstein.k.johansen@conduct.no

dir=~/src/skybert-net/src

file=$1

if [ ! -r $file ]; then
  echo $file "doesn't exist"
  exit 0
fi

to_file=$dir/$(basename $(dirname $(dirname $file)))/$(basename $(dirname $file)).md
mkdir -p $(dirname $to_file)

change_date=$(
  stat $file 2>&1 | sed -n 's#Change: \([^ ]*\) .*#\1#p'
)

title=$(
  basename $(dirname $file) | sed 's#[-]# #g'
)

cat > $to_file <<EOF
date: $change_date
category: $(basename $(dirname $(dirname $file)))
EOF

sed -f $(dirname $0)/$(basename $0 .sh).sed $file >> $to_file

cat $to_file


