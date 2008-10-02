# ----------------------------------------------------------------------------
#    JVCL export file
#
# The contents of this file are subject to the Mozilla Public License
# Version 1.1 (the "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/MPL-1.1.html
# 
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License 
# for the specific language governing rights and limitations under the License.
# 
# The Original Code is: postnew.sh, released on 2002-05-26.
# 
# The Initial Developer of the Original Code is John Doe
# Portions created by Peter Thörnqvist are Copyright (C) 2002 John Doe.
# All Rights Reserved.
# 
# Contributor(s): Peter Thornqvist, Olivier Sannier
# 
# You may retrieve the latest version of this file at the Project JEDI's JVCL 
# home page, located at http://jvcl.sourceforge.net
# 
# Description:
#   This is a shell script which will checkout a working copy of the JVCL 
#   repository, compress it into zip and 7zip files.
#   It is designed to be called from a global script that will upload the
#   results to various locations.
#
# ----------------------------------------------------------------------------
# $Id$

# Make our paths easy to use
# Note: Those paths are valid on obones' server, please update 
#       to reflect those you are using on your own machine. 

# path where to put the zipped files and build status page
export DAILYDIR=/home/obones/jvcl_export/daily

# working path used to export the directory
export FILEHOME=/home/obones/jvcl_export/files

# start location (usually equal to where this script is located).
export STARTFOLDER=/home/obones/jvcl_export

# Base SVN URL
export BASESVNURL=http://jvcl.svn.sourceforge.net/svnroot/jvcl/trunk

# URL to checkout from. Requires that the svn binary supports SSL
export SVNURL=$BASESVNURL/jvcl

# URL to checkout index.php from. Requires that the svn binary supports SSL
export SVN_INDEX_PHP_URL=$BASESVNURL/websites/daily/index.php

# the SVN command used to get the files
export SVNCOMMAND=export

# Locations of various required binaries
# Note: Those paths are valid on obones' server, please update to reflect those
#       you are using on your own machine. 
export SVNBIN=/usr/local/bin/svn
export FINDBIN=/bin/find
export ECHOBIN=/bin/echo
export RMBIN=/bin/rm
export UNIX2DOSBIN=/usr/bin/unix2dos
export ZIPBIN=/usr/bin/zip
export CPBIN=/bin/cp
export LNBIN=/bin/ln
export CHMODBIN=/bin/chmod
export SZIPBIN=/usr/local/bin/7za

# The call used to get a timestamp for the zip files
export DATESTRING=`date -I`

# declare function that gets current time tag
function CurrentTimeTag()
{
  $ECHOBIN [`date +"%T %Z"`]
}

# delete old checkout folder
$ECHOBIN `CurrentTimeTag` starting
$RMBIN -rf $FILEHOME/jvcl
cd $FILEHOME
$ECHOBIN `CurrentTimeTag` "getting files from SVN"

# get the latest sources from SVN
$SVNBIN $SVNCOMMAND -q $SVNURL jvcl
cd $FILEHOME/jvcl

# convert LF to CRLF for text files

# the list of extensions to convert.
extlist=(*.pas *.dfm *.inc *.cpp *.hpp *.h *.dpr *.bpr *.dpk *.bpk *.bpg\
         *.cfg *.template *.iss *.txt *.bat *.rc *.py *.dof *.macros *.tpl)

# allow null globbing
shopt -s nullglob

$ECHOBIN `CurrentTimeTag` "converting to dos format (CRLF)"
# we use find to look for directories AND files because some of
# them may contain spaces which would be detected by for as a
# separator in the list to iterate, thus skipping the file
$FINDBIN $FILEHOME/jvcl -type d -print | while read SRCDIR
do
#  $ECHOBIN "Processing in $SRCDIR"
  cd "$SRCDIR"

  for FILE in ${extlist[@]}
  do
    if [[ -a $FILE ]]
    then
      $UNIX2DOSBIN -q $FILE
    fi
  done
done

cd $FILEHOME/jvcl
$ECHOBIN `CurrentTimeTag` "creating zip files"
# create zip with all files and copy to daily
$ZIPBIN -rq JVCL3.zip .
$CPBIN JVCL3.zip $DAILYDIR/JVCL3-$DATESTRING.zip
$RMBIN -f JVCL3.zip

# create zip with sources only and copy to daily
$ZIPBIN -rq JVCL3-Source.zip  \*.htm \*.html \*.txt \*.bat \*.css \*.jpg common design run packages Resources install help
# remove the release folder - not needed
$ZIPBIN -dq JVCL3-Source.zip install/release/\*
$CPBIN JVCL3-Source.zip $DAILYDIR/JVCL3-Source-$DATESTRING.zip
$RMBIN -f JVCL3-Source.zip

$ECHOBIN `CurrentTimeTag` "creating 7zip files"
# create 7zip with all files and copy to daily
$SZIPBIN a -r JVCL3.7z \*
$CPBIN JVCL3.7z $DAILYDIR/JVCL3-$DATESTRING.7z
$RMBIN -f JVCL3.7z

# create 7zip with sources only and copy to daily
$SZIPBIN a -r JVCL3-Source.7z  \*.htm \*.html \*.txt \*.bat \*.css \*.jpg common design run packages Resources install help -x!install/release/\*

$CPBIN JVCL3-Source.7z $DAILYDIR/JVCL3-Source-$DATESTRING.7z
$RMBIN -f JVCL3-Source.7z

$ECHOBIN `CurrentTimeTag` "deleting old archive files"
cd $DAILYDIR
# delete old zips (we only keep 3 at a time)
$FINDBIN . \( -daystart -mtime +2 -type f -name J\* \) -exec rm -f {} \;

# copy the status docs
$ECHOBIN `CurrentTimeTag` "copying build status page"
$CPBIN -p -u -f "$FILEHOME/jvcl/help/Build status.html" "$DAILYDIR/Build status.html"

# Extract the index.php page
$ECHOBIN `CurrentTimeTag` "Extracting the index.php page"
$SVNBIN $SVNCOMMAND -q $SVN_INDEX_PHP_URL

# cleanup
$ECHOBIN `CurrentTimeTag` "deleting exported jvcl files"
$RMBIN -rf $FILEHOME/jvcl

# done!
$ECHOBIN `CurrentTimeTag` finished
