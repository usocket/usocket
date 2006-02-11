#!/bin/sh

# Test script to be run from the usocket source root
#
# Unfortunately, it currently works only with SBCL
# in my setup...
#
# I need to figure out how to setup ASDF with the other lisps
# I have installed: cmucl, ABCL, clisp, allegro and lispworks

cd `dirname $0`/test

if test -z "$1" ; then
  lisps="sbcl clisp $HOME/src/acl/acl70_trial/alisp"
else
  lisps=$1
fi

for my_lisp in $lisps ; do

# *.fasl are used by both sbcl and allegro,
# so remove them to prevent choking either

args=
if test "`basename $my_lisp`" == "alisp" ; then
  args=-batch
fi

echo "
#-sbcl (load \"asdf.lisp\")

(asdf:operate #-sbcl 'asdf:load-source-op
              #+sbcl 'asdf:load-op :usocket-test)

(usocket-test:do-tests)

#-allegro (quit)
" | $my_lisp $args

echo "Above test results for $my_lisp."

done
