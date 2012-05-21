#! /usr/bin/env bash

# by torstein.k.johansen@gmail.com

src_dir=$HOME/put_the_new_pictures_in_here
target_dir=/var/gallery
error_dir=/usr/local/src/picture-import/error
log_file=/usr/local/src/picture-import/log/$(basename $0 .sh).log

if [ $(which identify 2>/dev/null | wc -l) -eq 0 ]; then
  echo "You need imagemagick installed to use $(basename $0) :-("
  exit 1
fi

function print_and_log() {
  echo "$@" >> $log_file
  echo $@
}

function create_dir_for_picture() {
  if [ $(file "$@" | cut -d':' -f2 | grep empty | wc -l) -gt 0 ]; then
    print_and_log "The file" $1 "is empty :-( moving it to $error_dir"
    mv $1 $error_dir
    return 1
  fi
  
  # catering for dates on the forms:
  # * 2007-04-06T21:13:29.07+02:00
  # * 2007:01:01 17:02:03
  local date_dir=$(identify -format "%[EXIF:DateTimeOriginal]" "$1" | \
    cut -d'T' -f1 | \
    cut -d' ' -f1 | \
    sed 's#:#/#g' | \
    sed 's#-#/#g'
  )

  local picture_dir=$target_dir/$date_dir

  if [ ! -d $picture_dir ]; then
    mkdir -p $picture_dir
  fi

  print_and_log "Moving $1 -> $picture_dir ..."
  mv "$1" $picture_dir/
  remove_picture_src_dir_if_empty "$1"
  
}

function remove_picture_src_dir_if_empty() {
  local dir=$(dirname "$1")
  if [ $(find "$dir" -type f | wc -l) -eq 0 ]; then
    rmdir "$dir"
  fi
}

find $src_dir -iname "*.jpg" | while read f; do
  create_dir_for_picture "$f"
done

find $src_dir -iname "*.png" | while read f; do
  create_dir_for_picture "$f"
done
