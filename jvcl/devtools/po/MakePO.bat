@echo off
echo. Create JVCL PO template file 
echo. Requires dxgettext from http://dxgettext.sf.net
dxgettext -q -b ..\..\common --delphi
dxgettext -q -b ..\..\design --delphi
dxgettext -q -b ..\..\run --delphi
msgcat ..\..\run\default.po ..\..\design\default.po ..\..\common\default.po -o default.po
msguniq -u --use-first default.po -o jvcl3.po
if not exist ignore.po msgmkignore jvcl3.po -o ignore.po
msgremove jvcl3.po -i ignore.po -o default.po
..\bin\stripCmtPO default.po jvcl3.po
del ..\..\common\default.po ..\..\design\default.po ..\..\run\default.po default.po
