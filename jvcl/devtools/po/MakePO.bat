@echo off
echo Create JVCL PO template file 
echo Requires dxgettext from http://dxgettext.sf.net

: first, extract all strings
echo Extracting strings...
dxgettext -q -o ..\..\common -b ..\..\common --delphi
dxgettext -q -o ..\..\design -b ..\..\design --delphi
dxgettext -q -o ..\..\run -b ..\..\run --delphi

: then merge all the generated po files into one
echo merging files...
msgcat ..\..\run\default.po ..\..\design\default.po ..\..\common\default.po -o default.po

: ensure uniqueness
msguniq -u --use-first default.po -o jvcl3.po

: remove translations that needs to be ignored
echo Removing strings that do not require translation
if not exist ignore.po msgmkignore jvcl3.po -o ignore.po
msgremove jvcl3.po -i ignore.po -o default.po

: strip comments out
if exist ..\bin\stripCmpPO ..\bin\stripCmtPO default.po jvcl3.po
if not exist ..\bin\stripCmpPO copy default.po jvcl3.po

: cleanup
echo Cleaning up...
del ..\..\common\default.po ..\..\design\default.po ..\..\run\default.po default.po

echo Translation template jvcl3.po has been updated
