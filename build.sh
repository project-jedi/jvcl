#!/bin/sh
#
# shell script to build all JVCLX packages for K3 
#
# André Snepvangers, 2004-09-13
#

PACKAGES="Core System StdCtrls Ctrls Cmp Custom Dlgs AppFrm Crypt HMI Inspector Jans ManagedThreads MM NET PageComps UIB Validators Wizard XPCtrls"

DCCOPTIONS="-Q -I../../common -U../../qcommon,../../qdesign,../..qrun"

if test -r ~/.borland/delphi69rc; 

then 

  eval `grep 'DelphiRoot=' ~/.borland/delphi69rc`

  DELPHI=$DelphiRoot

  export DELPHI

  eval `grep 'Version=' ~/.borland/delphi69rc`

  if test -z $Version ; 

  then

    echo

    echo Installing JVCLX 1.0 for Kylix 3 "("$DELPHI")" 

    echo "(C) Copyright Project JEDI 2004"

    echo 

    PACKAGES="$PACKAGES EDI" 
    
    echo ;

  else

    echo Installing JVCLX 1.0 for Kylix 3 Open Edition"("$DELPHI")"

    DCCOPTIONS="$DCCOPTIONS -dDELPHI_PERSONAL_EDITION"

  fi

else

  echo FAILED to open ~/.borland/delphi69rc

  echo No Kylix 3 installation found !

  beep

  exit

fi

source $DELPHI/bin/kylixpath >/dev/null

cd packages/k3

PACKAGENAME=JvQ3rdK3R.dpk

echo "[Building $PACKAGENAME runtime package]" | tee ../../build.log

if dcc $DCCOPTIONS $PACKAGENAME >/dev/null 2>>  ../../build.log

then

  echo 

  for PACKAGE in $PACKAGES; do

    PACKAGENAME="JvQ"$PACKAGE"K3R.dpk"

    echo "[Building $PACKAGENAME runtime package] " | tee -a ../../build.log 

    if dcc $DCCOPTIONS $PACKAGENAME >/dev/null 2>> ../../build.log ; 

    then

      PACKAGENAME="JvQ"$PACKAGE"K3D.dpk"
 
      echo "[Building $PACKAGENAME designtime package] " | tee -a ../../build.log 
 
      if dcc $DCCOPTIONS $PACKAGENAME >/dev/null 2>> ../../build.log ; 

      then 
       
        echo " " | tee -a  ../../build.log ;
       
      else 

        FAILED="$FAILED $PACKAGENAME"

        echo "FAILED to build designtime package $PACKAGENAME" | tee -a  ../../build.log
      
        echo " " | tee -a  ../../build.log

      fi ; 

    else

      FAILED="$FAILED $PACKAGENAME"

      echo "FAILED to build runtime package $PACKAGENAME" | tee -a  ../../build.log

      echo " " | tee -a  ../../build.log

    fi

  done ;

else
  
  FAILED=$PACKAGENAME

  echo "FAILED to build runtime package $PACKAGENAME" | tee -a  ../../build.log

  echo " " | tee -a  ../../build.log

fi

cd ../..

if test -z $FAILED ; 

then

  echo Finished building JVCLX 1.0 | tee -a ../../build.log 

  echo ;

else

  echo Failed to build packages: $FAILED | tee -a ../../build.log
  
  if test $DESKTOP="KDE" ; 

  then
    
    kedit ./build.log & 
    
    echo ;

  else
  
    echo

  fi  
fi