@echo off
set LANGUAGE=%1
if %LANGUAGE%!==! goto help

: test the existence of SetPoHeader
if not exist ..\devtools\bin\SetPoHeader.exe goto noSetPoHeader

: test the existence of the target po
if not exist %LANGUAGE%\LC_MESSAGES\JVCLInstall.po goto jvclponotfound


: now that we are sure that all required files exist, process JVCLInstall.po
echo Updating %LANGUAGE%...
msgmerge --force-po -F -o %LANGUAGE%\LC_MESSAGES\JVCLInstall.po %LANGUAGE%\LC_MESSAGES\JVCLInstall.po JVCLInstall.po

: set the headers to match JVCL ones
..\devtools\bin\SetPoHeader -t "JVCL localization template" -c "The Jedi Visual Component Library group" -p JVCL -v 3 -a "JVCL Group" -e "jvcl@sourceforge.net" %LANGUAGE%\LC_MESSAGES\JVCLInstall.po 

goto end

:jvclponotfound
echo Error: %LANGUAGE%\LC_MESSAGES\JVCLInstall.po has not been found.
goto end

:noSetPoHeader
echo SetPoHeader was not found
echo Please compile it before running this script. It is available in the devtools directory
goto end

:nodxgettext
echo DxGettext was not found.
echo This script requires dxgettext from http://dxgettext.sf.net
goto end

:help
echo UpdateLanguage.bat - Updates one language for the JVCL
echo.
echo Usage: UpdateLanguage.bat LangId
echo.
echo     LangId is the Id of the language to update. It will
echo     be used as the directory name where to find JVCLInstall.po
echo     for the given language

:end
del /q tmp?.txt
set LANGUAGE=