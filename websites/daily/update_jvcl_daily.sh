#!/bin/bash
# ----------------------------------------------------------------------------
# The contents of this file are subject to the Mozilla Public License
# Version 1.1 (the "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/MPL-1.1.html
# 
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License 
# for the specific language governing rights and limitations under the License.
# 
# The Original Code is: update_jvcl_daily.sh, released on 2008-10-02.
# 
# The Initial Developer of the Original Code is Olivier Sannier
# Portions created by Olivier Sannier are Copyright (C) 2008 Olivier Sannier.
# All Rights Reserved.
# 
# Contributor(s): none
# 
# You may retrieve the latest version of this file at the Project JEDI's JVCL 
# home page, located at http://jvcl.sourceforge.net
# 
# Description:
#    Script to update JVCL daily zips both on SourceForge and Free.
#    It is meant to be called from withing a cron job
# ----------------------------------------------------------------------------

# Place where this script is located, where the daily dir is, name of the log file
# and name of the two files containing the passwords
export BASEDIR=/home/obones/jvcl_export
export DAILY_DIR=$BASEDIR/daily
export LOGFILE=$BASEDIR/log
export SF_PASS_FILENAME=$BASEDIR/sf_pass
export MIRROR_PASS_FILENAME=$BASEDIR/mirror_pass

# In order to avoid storing the passwords in clear in this script file, they are
# required to be stored in external text files. 
# Without them, the script won't run, so we chech that they exist. If not, we
# display a log message
if [ ! -f $SF_PASS_FILENAME ]; then
	echo "$SF_PASS_FILENAME not found, unable to continue." > $LOGFILE
      	exit
fi

if [ ! -f $MIRROR_PASS_FILENAME ]; then
	echo "$MIRROR_PASS_FILENAME not found, unable to continue." > $LOGFILE
      	exit
fi

# Configuration details to access the sourceforge daily folder
export SF_HOST=sftp://web.sourceforge.net
export SF_DIR=/home/groups/j/jv/jvcl/htdocs/daily
export SF_USER=obones,jvcl
export SF_PASS=`cat $SF_PASS_FILENAME`

# Configuration details to access the mirror daily folder
export MIRROR_HOST=ftpperso.free.fr
export MIRROR_DIR=/jvcl_daily
export MIRROR_USER=obones
export MIRROR_PASS=`cat $MIRROR_PASS_FILENAME`

export ECHOBIN=/bin/echo

$ECHOBIN -n > $LOGFILE

# Update the local copy
cd $BASEDIR >> $LOGFILE
$ECHOBIN "------------------------------------------------" >> $LOGFILE
$ECHOBIN "---       Retrieveing and zipping files      ---" >> $LOGFILE
$ECHOBIN "------------------------------------------------" >> $LOGFILE
./postnew.sh >> $LOGFILE 2>&1

# send to SourceForge
$ECHOBIN "------------------------------------------------" >> $LOGFILE
$ECHOBIN "---          Sending to SourceForge          ---" >> $LOGFILE
$ECHOBIN "------------------------------------------------" >> $LOGFILE
./sync_ftp.sh $SF_HOST $DAILY_DIR $SF_DIR $SF_USER $SF_PASS >>$LOGFILE 2>&1

# do the copy to mirror
$ECHOBIN "------------------------------------------------" >> $LOGFILE
$ECHOBIN "---            Sending to mirror             ---" >> $LOGFILE
$ECHOBIN "------------------------------------------------" >> $LOGFILE
./sync_ftp.sh $MIRROR_HOST $DAILY_DIR $MIRROR_DIR $MIRROR_USER $MIRROR_PASS >>$LOGFILE 2>&1

