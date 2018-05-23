#! /usr/bin/env bash

## Command to create an agenda view from org mode suitable for emails
## and Status Hero.

##          author: torstein@escenic.com
set -o errexit
set -o nounset
set -o pipefail
shopt -s nullglob

create_report() {
  format=${1-"markdown"}
  days_back=${2-"7"}
  now=
  start_date=
  org_agenda=

  now=$(date +%s)
  start_date=$(date +%Y-%m-%d --date @$((now - (60 * 60 * 24 * days_back))))
  org_agenda=$(
    emacs -batch -l ~/.emacs.d/tkj-org.el \
          -eval "(org-batch-agenda \"a\"
          org-agenda-start-day \"${start_date}\"
          org-agenda-span $(( days_back + 1 ))
          org-agenda-include-diary t
          org-agenda-sorting-strategy '(todo-state-up)
          org-agenda-files '(\"~/doc/scribbles/""$(date +%Y)""\"))" \
            2>/dev/null |
      grep -v 'life:' |
      grep -v 'quotes:' |
      grep -v 'yt:')
  result=
  result=$(
    echo "${org_agenda}" |
      sed '/:noreport:/d' |
      sed -r 's#([a-z]+): .* Sched.*: (.*)#\2 \#\1#' |
      sed -r 's#^[ ]*DONE# ✔#g' |
      sed -r 's#^[ ]*STARTED# ▶#g' |
      sed -r 's#^[ ]*TODO##g' |
      sed -r 's#^[ ]*WAITING# ⌛ Waiting for: #' |
      sed -r 's#^[ ]*PR# ⌛ Fixed, awaiting PR#g' |
      sed -r 's#^[ ]*MERGED# ✔ Merged: #' |
      sed -r 's#talk(ed)* with #💬 with #i' |
      sed -r 's#^[ ]*gcal:[ ]* .*[0-9]?[0-9]:[0-9][0-9] (.*)# Meeting: \1#' |
      sed -r '/[a-z]/!d' |
      sed -r '/.... now - - -/d'
        )

  if [[ ${format} == "markdown" ]]; then
    echo "${result}" | \
      sed -r 's#^[ ]+#- #'
  else
    echo "${result}"
  fi
}

main() {
  create_report markdown 8

  local file=/var/www/html/agenda.md.txt

  create_report markdown 1 | sed '1d' > "${file}"
  #  add bom
  sed -i '1s/^/\xef\xbb\xbf/' "${file}"
  git commit "${file}" -m "Updated agenda $(fortune | head -n 1)" &> /dev/null
}

main "$*"
