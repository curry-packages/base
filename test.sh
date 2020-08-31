#!/bin/sh
# Shell script to test modules having some properties defined

CURRYBIN="../bin"

CURRYCHECK=`which curry-check`
if [ -z "$CURRYCHECK" ] ; then
  echo "WARNING: test skipped since executable 'curry-check' not found!"
  exit
fi

ALLTESTS="ShowS Sort"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

CCOPTS="-m100 -dInt"
LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYCHECK $CCOPTS $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYCHECK $CCOPTS $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in curry check:"
    cat $LOGFILE
    exit 1
  fi
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
