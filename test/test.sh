#!/bin/sh
# usr/opt/wrdapp/analgwst.1/test/test.sh -- run WEXLER programs test data sets
#
# Usage: test.sh [start [stop]]
#        test.sh [start [stop] | tee test.out
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
#
# To use this script for another program in a different directory, globally
# change the program name (be sure to change both upper and lower case
# occurrences) and set the default value for the Stop variable as appropriate.
#
# History: 12/24/91, mblalock, initial coding
#          04/27/92, mygoze, restructuring
#          04/01/96, rsregan, modified for use with analgwst
#
# Variable definitions
# --------------------
#
  TOPDIR=..
  DATA=$TOPDIR/data
  CHECK=$TOPDIR/test/check.sh
  CLEAN=$TOPDIR/test/clean.sh
  PROG1=$TOPDIR/bin/finite
  PROG2=$TOPDIR/bin/seminf
  PROG3=$TOPDIR/bin/point2
  PROG4=$TOPDIR/bin/stripf
  PROG5=$TOPDIR/bin/stripi
  PROG6=$TOPDIR/bin/gauss
  PROG7=$TOPDIR/bin/point3
  PROG8=$TOPDIR/bin/patchf
  PROG9=$TOPDIR/bin/patchi
  PROG10=$TOPDIR/bin/point3_mod
  PROGNM=ANALGWST
  DIVD=========================================
  END=12
#
#
  exec 2>&1                                # stderr shows up in .out file

  Start=${1:-1}                            # by default, start at 1
  Stop=${2:-$END}                          # by default, stop at 12
  if [ $Start -lt 1 ] ; then Start=1 ; fi
  if [ $Stop -lt 1 ] ; then Stop=$END ; fi
  if [ $Start -gt $END -o $Stop -gt $END ]
  then
    echo
    echo "Warning, invalid arguments--test range is 1 - $END for $PROGNM"
    echo "input arguments were $Start - $Stop"
    echo
    if [ $Stop -gt $END ] ; then Stop=$END ; fi
    if [ $Start -gt $END ]
    then
      echo "no $PROGNM tests will be performed"
    else
      echo "Tests $Start - $Stop will be performed"
    fi
    echo
  fi
#
# begin test runs
#
  Test=$Start

  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo $DIVD$DIVD
    echo "Begin processing $PROGNM test runs $Start to $Stop"
    echo
    date
  fi
#
# remove old output file
#
  $CLEAN

  while [ $Test -ge $Start -a $Test -le $Stop ]
  do
    echo
    echo
    echo $DIVD$DIVD
    echo "Test run number $Test"
    echo
    echo "****AFTER GRAPHICS ARE DISPLAYED, PRESS THE ENTER KEY TO CONTINUE****"
    Name=sample$Test
    if [ $Test -eq 1 -o $Test -eq 3 -o $Test -eq 8 -o $Test -eq 9 ] ; then Name=sample$Test'a' ; fi
    if [ $Test -eq 12 ] ; then Name=sample9 ; fi
    echo $DATA/$Name.dat > go
    echo $Name.prt >> go
    echo 1 >> go
    echo yellow >> go
    case "$Test"
    in
          1) PROG=$PROG1;;
          2) PROG=$PROG1;;
          3) PROG=$PROG2;;
          4) PROG=$PROG2;;
          5) PROG=$PROG3;;
          6) PROG=$PROG4;;
          7) PROG=$PROG5;;
          8) PROG=$PROG6;;
          9) PROG=$PROG7;;
          10) PROG=$PROG8;;
          11) PROG=$PROG9;;
          12) PROG=$PROG10;;
    esac
    $PROG < go > $Name.log
    if [ $Test -eq 1 -o $Test -eq 3 -o $Test -eq 8 ] ; then 
      Name=sample$Test'b'
      rm go
      echo $DATA/$Name.dat > go
      echo $Name.prt >> go
      echo 1 >> go
      echo yellow >> go
      $PROG < go > $Name.log
    fi

    Test=`expr $Test + 1`
  done
#
  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo
    echo $DIVD$DIVD
    echo "Completed $PROGNM test runs $Start to $Stop"
    echo
  fi

# check output against original output in data directory
  $CHECK sample
