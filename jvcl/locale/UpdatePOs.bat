@echo off

: The languages that are known to the JVCL
set LANGUAGES=de es fr it nl ro ru sv

echo Update JVCL PO template file and the translations derived from it
echo Current languages: %LANGUAGES%

: test the existence of dxgettext
dxgettext -q -b .\ 1>tmp1.txt 2>tmp2.txt
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
dxgettext -q -o ..\common -b ..\common --delphi
dxgettext -q -o ..\run -b ..\run --delphi

: then merge all the generated po files into one
echo Merging files...
msgcat ..\run\default.po ..\common\default.po -o default.po

: ensure uniqueness
msguniq -u --use-first default.po -o jvcl.po

: remove translations that needs to be ignored
echo Removing strings that do not require translation...
if not exist ignore.po msgmkignore jvcl.po -o ignore.po
msgremove jvcl.po -i ignore.po -o default.po

: merge with existing jvcl.po file
echo Updating existing translations...
if exist jvcl.po msgmerge -F -o jvcl.po jvcl.po default.po 
if not exist jvcl.po copy default.po jvcl.po

: set the headers to match JVCL ones
..\devtools\bin\SetPoHeader -t "JVCL localization template" -c "The Jedi Visual Component Library group" -p JVCL -v 3 -a "JVCL Group" -e "jvcl@sourceforge.net" jvcl.po

echo Translation template jvcl.po has been updated

echo Updating languages...
FOR %%l IN (%LANGUAGES%) DO call UpdateLanguage.bat %%l

: cleanup
echo Cleaning up...
del ..\common\default.po ..\run\default.po default.po

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
