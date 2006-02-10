#!/bin/sh

# Test script to be run from the usocket source root
#
# Unfortunately, it currently works only with SBCL
# in my setup...
#
# I need to figure out how to setup ASDF with the other lisps
# I have installed: cmucl, ABCL, clisp, allegro and lispworks

for my_lisp in sbcl ; do

echo "
(require 'usocket-test)

(usocket-test:do-tests)

(quit)
" | $my_lisp

echo "Above test results for $my_lisp."

done
