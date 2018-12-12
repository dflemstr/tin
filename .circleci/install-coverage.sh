#!/bin/bash

sudo apt-get update
sudo apt-get install -y libcurl4-openssl-dev libelf-dev libdw-dev cmake binutils-dev libiberty-dev

KCOV_VERSION=33
wget https://github.com/SimonKagstrom/kcov/archive/v$KCOV_VERSION.tar.gz && \
    tar xzf v$KCOV_VERSION.tar.gz && \
    rm v$KCOV_VERSION.tar.gz && \
    cd kcov-$KCOV_VERSION && \
    mkdir build && cd build && \
    cmake .. && make -j && sudo make install -j && \
    cd ../.. && rm -rf kcov-$KCOV_VERSION
