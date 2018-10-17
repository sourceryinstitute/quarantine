#!/usr/bin/env bash

if [[ -d build ]]; then
  rm -rf build
fi
mkdir build
cd build
FC=gfortran cmake ..
make
ctest --output-on-failure
