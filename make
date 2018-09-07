#!/bin/bash
###################################################
#  Miking is licensed under the MIT license.
#  Copyright (C) David Broman. See file LICENSE.txt
#
#  To make the build system platform independent,
#  all scripts are done in Ocaml instead of being
#  dependent on Make.
###################################################

buildboot(){
  dune build src/boot/boot.exe \
    && cp -f _build/default/src/boot/boot.exe build/boot
}

case $1 in
    old)
        ocaml build/build.ml $2 $3 $4
        ;;
    test)
        buildboot
        cd test
        ../build/boot test mcore ppl ragnar
        ;;
    clean)
        rm -rf _build
        rm -f build/boot
        ;;
    all | *)
        buildboot
        ;;
esac
