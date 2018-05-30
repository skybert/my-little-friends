## Torstein's dockerised Emacs, yes with GUI
##
## Build:
##    docker build -t skybert/emacs -f emacs.dockerfile .
## Run:
##    ./emacs.docker

from debian:latest

run export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y \
            autoconf \
            automake \
            build-essential \
            curl \
            exuberant-ctags \
            git \
            gnutls-dev \
            imagemagick \
            libdbus-1-dev \
            libgif-dev \
            libgtk2.0-dev \
            libjpeg-dev \
            libmagick++-dev \
            libncurses-dev \
            libpng-dev \
            libtiff-dev \
            libx11-dev \
            libxpm-dev \
            maildir-utils-extra \
            mu4e \
            silversearcher-ag \
            texinfo \
            wget

run cd /tmp && \
    wget --quiet https://gnuftp.uib.no/emacs/emacs-26.1.tar.gz && \
    tar xzf emacs-26.1.tar.gz

run cd /tmp/emacs-26.1 && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

run adduser --quiet --gecos "Torstein Krause Johansen,,,,"  torstein

user torstein
cmd emacs
