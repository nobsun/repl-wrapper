#!/bin/sh
# script -q -c"stack exec -- ghci -fshow-loaded-modules -flocal-ghci-history $1" -a -f ghci.log
script -q -c"ghciw $1" -a -f ghci.log
