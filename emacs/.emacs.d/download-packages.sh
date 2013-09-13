#! /usr/bin/env bash

# Download various packages not (easily) obtainable with APT or Emacs
# packages.

# by torstein.k.johansen@conduct.no

src_dir=/usr/local/src
emacs_src_dir=${src_dir}/emacs

mkdir -p ${src_dir} ${emacs_src_dir} || {
  echo "Couldn't create directories like" ${emacs_src_dir} ":-("
  exit 1
}

emacs_src_list="
  https://github.com/yjwen/org-reveal.git
  git://orgmode.org/org-mode.git
"
src_list="
  https://github.com/hakimel/reveal.js.git
"

for el in $emacs_src_list; do
  dir=${emacs_src_dir}/$(basename $el .git)
  if [ -d ${dir} ]; then
    (cd $dir && git pull)
  else
    (cd $emacs_src_dir && git clone $el)
  fi
done

for el in $src_list; do
  dir=${src_dir}/$(basename $el .git)
  if [ -d ${dir} ]; then
    (cd $dir && git up)
  else
    (cd $src_dir && git clone $el)
  fi
done

org_dir=/usr/local/src/emacs/org-mode
if [ -d ${org_dir} ]; then
  (cd $org_dir && make autoloads)
fi
