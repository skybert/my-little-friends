from debian:latest

run apt-get update && apt-get install --yes x11-apps
run apt-get install --yes strace
cmd strace -f emacs
