@echo off

: The languages that are known to the JVCL
set LANGUAGES=bg de es fr it nl pl ro ru sv

echo Update JVCL Installer PO template file and the translations derived from it
echo Current languages: %LANGUAGES%

: test the existence of dxgettext
dxgettext -q -b. >tmp1.txt 2>tmp2.txt
if errorlevel 1 goto nodxgettext

: test the existence of SetPoHeader
if exist ..\devtools\bin\SetPoHeader.exe goto nextSetPoHeader
cd ..\devtools
make -s SetPoHeader.exe
cd ..\locale

:nextSetPoHeader
if not exist ..\devtools\bin\SetPoHeader.exe goto noSetPoHeader

: first, extract all strings
echo Extracting strings...
dxgettext -q -r -b ..\install\JVCLInstall --delphi

: ensure uniqueness
msguniq -u --no-wrap default.po -o JVCLInstall.po

: remove translations that needs to be ignored
echo Removing strings that do not require translation...
if not exist ignore.po msgmkignore JVCLInstall.po -o ignore.po
msgremove --no-wrap JVCLInstall.po -i ignore.po -o default.po

: merge with existing jvcl.po file
echo Updating existing translations...
if exist JVCLInstall.po msgmerge -F -o JVCLInstall.po JVCLInstall.po default.po 
if not exist JVCLInstall.po copy default.po JVCLInstall.po

: set the headers to match JVCL ones
..\devtools\bin\SetPoHeader -t "JVCL localization template" -c "The Jedi Visual Component Library group" -p JVCL -v 3 -a "JVCL Group" -e "jvcl@sourceforge.net" JVCLInstall.po

echo Translation template JVCLInstall.po has been updated

echo Updating languages...
FOR %%l IN (%LANGUAGES%) DO call UpdateLanguageInstaller.bat %%l

: cleanup
echo Cleaning up...
del default.po

goto end

:noSetPoHeader
echo SetPoHeader was not found
echo Please compile it before running this script. It is available in the devtools directory

goto end

:nodxgettext
echo DxGettext was not found.
echo This script requires dxgettext from http://dxgettext.sf.net

:end
if exist tmp1.txt del /q tmp?.txt
set LANGUAGES=
