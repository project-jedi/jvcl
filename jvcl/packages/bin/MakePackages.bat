@echo off

SET JVCLROOT=..\..
SET JVCLPACKAGEDIR=..
SET DEVTOOLS=%JVCLROOT%\devtools
SET DEVTOOLS_BACK=..\packages\bin

echo [Compiling: pg.exe]
cd %DEVTOOLS%
make -f makefile.mak pg.exe
cd %DEVTOOLS_BACK%
echo [Generating: JVCL Packages]
"%DEVTOOLS%\bin\pg.exe" -m=JVCL -p="%JVCLPACKAGEDIR%" -x="%DEVTOOLS%\bin\pgEdit.xml"

SET JVCLROOT=
SET JVCLPACKAGEDIR=
SET DEVTOOLS=
SET DEVTOOLS_BACK=
