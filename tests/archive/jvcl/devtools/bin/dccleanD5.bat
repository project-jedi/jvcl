@echo off
rem clean jvcl and jcl folders for D5 compatibility
dc -i -s -fskiplistD5.txt ..\..\*.dfm ..\..\..\JCL\*.dfm
