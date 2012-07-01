#! /usr/bin/env bash

# by torstein.k.johansen@gmail.com

# from/source
if [ $1 ]; then
  src_dir=$1
else
  src_dir=$HOME/put_the_new_pictures_in_here
fi

# to/target
if [ $2 ]; then
  target_dir=$2
else
  target_dir=/var/gallery
fi

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

function sort_and_archive_picture() {
  if [ $(file "$@" | cut -d':' -f2 | grep empty | wc -l) -gt 0 ]; then
    print_and_log "The file" $1 "is empty :-( moving it to $error_dir"
    mv $1 $error_dir
    return 1
  fi
  
  # catering for dates on the forms:
  # * 2007-04-06T21:13:29.07+02:00
  # * 2007:01:01 17:02:03
  local year_and_date=$(
    identify -format  "%[EXIF:DateTimeOriginal]" "$1" | \
      sed 's#:#-#g' | \
      cut -d'-' -f1-2
  )

  # we want 2012/2012-06
  local date_dir="$(echo $year_and_date | cut -d'-' -f1)/${year_and_date}"
  # if the EXIF date couldn't be extracted from the image, there will
  # be just be a slash in date_dir.
  if [[ ${date_dir} == "/" ]]; then
    date_dir=undated
  fi

  local picture_dir=$target_dir/$date_dir

  if [ ! -d $picture_dir ]; then
    mkdir -p $picture_dir
  fi

  print_and_log "Moving $1 -> $picture_dir ..."
  mv "$1" $picture_dir/
}

function remove_rempty_directories() {
  find $src_dir -type d | while read f; do    
    rmdir "$f" 2>/dev/null
  done
}

find $src_dir -iname "*.jpg" | while read f; do
  sort_and_archive_picture "$f"
done

find $src_dir -iname "*.png" | while read f; do
  sort_and_archive_picture "$f"
done

remove_rempty_directories
