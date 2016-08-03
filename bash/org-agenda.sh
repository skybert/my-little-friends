#! /usr/bin/env bash

## Command to create an agenda view from org mode suitable for emails
## and Status Hero.

##          author: torstein@escenic.com
set -o errexit
set -o nounset
set -o pipefail
shopt -s nullglob

main() {
  org_agenda=
  org_agenda=$(
    emacs -batch -l ~/.emacs.d/tkj-org.el \
          -eval '(org-batch-agenda "a"
          org-agenda-span (quote week)
          org-agenda-include-diary t
          org-agenda-files (quote ("~/doc/work.org")))')

  echo "${org_agenda}" |
    sed -r 's~:([a-z]+):~#\1~' |
    sed -r 's#work:.* Sched. [0-9]+x:.*STARTED#  CONTINUE#' |
    sed -r 's#work:.* Sched.*:##'

}

main "$@"
