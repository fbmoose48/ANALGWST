#! /bin/sh
#
#  check.sh:  differential file comparator (diff) run to compare original
#             output files against newly created output files
#
# output file names must follow format "nameX.sufx" where:  "name" is
# "test" by default or can be supplied as argument(s) to check.sh; all
# numbers X must appear in "for Test" loop list below; all suffixes must 
# appear in "for Sufx" loop list below
#
#  04/28/92, mygoze, initial coding
#  06/10/92, mygoze, added in NameList, Name, Sufx variables and "for Name",
#                    "for Sufx" loops to make check.sh more versatile 

DIVD=----------------------------------------
TOPDIR=..
DATA=$TOPDIR/data

# set list of names of files to be tested; name will be "test" by default
NameList=${@:-test}

# delete old output file  
if [ -f check.out ]; then rm check.out; fi

echo
echo $DIVD$DIVD
echo The test output files will differ for date and time of simulation and
echo for simulation time required, report only differences other than these.
echo

for Name in $NameList
do
  for Test in "" 1a 1b 2 3a 3b 4 5 6 7 8a 8b 9 9a 10 11 12
  do
    for Sufx in prt ps
    do
      if [ -f $Name$Test.$Sufx -a -f $DATA/$Name$Test.$Sufx.gz ]; then gunzip $DATA/$Name$Test.$Sufx.gz; fi
      if [ -f $DATA/$Name$Test.$Sufx -a -f $Name$Test.$Sufx ]
      then
      # do comparison only if both orig. and new output files exist
        echo $DIVD$DIVD | tee -a check.out
        echo "comparison of $DATA/$Name$Test.$Sufx with $Name$Test.$Sufx" \
             | tee -a check.out

        if diff -w $DATA/$Name$Test.$Sufx $Name$Test.$Sufx >> check.out
        then
          echo FILES ARE IDENTICAL | tee -a check.out
        else
          echo FILES DIFFER:  see file check.out for differences
        fi
        if [ $Sufx = "prt" ]; then gzip $DATA/$Name$Test.$Sufx; fi
      fi
    done
  done
done
