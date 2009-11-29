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
# The Original Code is: sync_ftp.sh, released on 2008-10-02.
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
#   Synchronizes the content of a remote folder with the content
#   of a local folder via FTP or SFTP. That is, the content of
#   the remote folder will be updated to match the content of
#   the local folder
#   The command used to perform the data transfer is lftp
#
#   Typical command line:
#
#     sync_ftp.sh Host LocalDir RemoteDir User Password
#
# ----------------------------------------------------------------------------

# The host to connect to. May be prefixed with sftp:// to use sFTP instead of FTP
export HOST=$1

# path where the files to be uploaded are located
export LOCAL_DIR=$2

# path on the remote server where the files will be uploaded
export REMOTE_DIR=$3

# name of the file that will contain the lftp commands
export LFTP_COMMANDS_FILE=/tmp/lftp_commands

# The user
export USER=$4

# The password
export PASS=$5

# The download limit
export DOWN_LIMIT=$6

# The upload limit
export UP_LIMIT=$7

# Locations of various required binaries
# Note: Those paths are valid on obones' server, please update to reflect those
#       you are using on your own machine.
export ECHOBIN=/bin/echo
export LFTPBIN=/usr/bin/lftp

# declare function that gets current time tag
function CurrentTimeTag()
{
  $ECHOBIN [`date +"%T %Z"`]
}

# declare function that prints help message out
function Help()
{
  $ECHOBIN $0
  $ECHOBIN "Synchronizes the content of a remote folder with the content"
  $ECHOBIN "of a local folder via FTP or SFTP. That is, the content of"
  $ECHOBIN "the remote folder will be updated to match the content of"
  $ECHOBIN "the local folder"
  $ECHOBIN "The command used to perform the data transfer is lftp"
  $ECHOBIN ""
  $ECHOBIN "Typical command line:"
  $ECHOBIN ""
  $ECHOBIN "  $0 Host LocalDir RemoteDir User Password DownLimit UpLimit" 
  $ECHOBIN ""
  $ECHOBIN "  Up and Down limit are expressed in bytes per second and"
  $ECHOBIN "  their default value is 0 for unlimited transfer rate"
}

# Check configuration
if [ -z $HOST ]; then
  Help
  $ECHOBIN "ERROR: Host is not specified"
  exit
fi

if [ -z $LOCAL_DIR ]; then
  $ECHOBIN "ERROR: Local directory is not specified"
  exit
fi

if [ -z $REMOTE_DIR ]; then
  $ECHOBIN "ERROR: Remote directory is not specified"
  exit
fi

if [ -z $USER ]; then
  $ECHOBIN "ERROR: Username is not specified"
  exit
fi

if [ -z $PASS ]; then
  $ECHOBIN "ERROR: Password is not specified"
  exit
fi

if [ -z $DOWN_LIMIT ]; then
  export $DOWN_LIMIT=0
fi

if [ -z $UP_LIMIT ]; then
  export $UP_LIMIT=0
fi

# Create lftp command file
$ECHOBIN `CurrentTimeTag` "Creating lFTP command file..."
$ECHOBIN " " > $LFTP_COMMANDS_FILE
$ECHOBIN "set net:limit-total-rate $DOWN_LIMIT:$UP_LIMIT" >> $LFTP_COMMANDS_FILE
$ECHOBIN "set net:limit-total-max $UP_LIMIT" >> $LFTP_COMMANDS_FILE
$ECHOBIN "open $HOST" >> $LFTP_COMMANDS_FILE
$ECHOBIN "user $USER $PASS" >> $LFTP_COMMANDS_FILE
$ECHOBIN "mirror -vvv -e -R $LOCAL_DIR $REMOTE_DIR" >> $LFTP_COMMANDS_FILE
$ECHOBIN "quit" >> $LFTP_COMMANDS_FILE

# execute the lftp commands
$ECHOBIN `CurrentTimeTag` "Executing lFTP commands..."
$LFTPBIN -f $LFTP_COMMANDS_FILE

# cleanup
rm -f $LFTP_COMMANDS_FILE

# all done
$ECHOBIN `CurrentTimeTag` "Finished"
