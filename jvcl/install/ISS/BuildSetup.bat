@echo off
SETLOCAL
SET CURDIR=%CD%
SET SETUPDIR=%~dp0


:: ==========================================================
:: rsvars.bat check
:: ==========================================================
if not "-%BDS%" == "-" goto RsVarsCalled
call rsvars.bat
if "-%BDS%" == "-" goto Leave

:RsVarsCalled
SET JVCLROOT=%SETUPDIR%\..\..
SET JVCLBUILTDIR=%SETUPDIR%\setupbuild

:: == Find JCL root dir ==
SET JCLROOT=%JVCLROOT%\..\..\jcl\jcl
if exist "%JCLROOT%\source\common\JclBase.pas" goto JclRootDirFound
SET JCLROOT=%JVCLROOT%\..\..\jcl
if exist "%JCLROOT%\source\common\JclBase.pas" goto JclRootDirFound
SET JCLROOT=%JVCLROOT%\..\jcl\jcl
if exist "%JCLROOT%\source\common\JclBase.pas" goto JclRootDirFound
SET JCLROOT=%JVCLROOT%\..\..\jcl\jcl
if exist "%JCLROOT%\source\common\JclBase.pas" goto JclRootDirFound
goto NoRootDirFound
:JclRootDirFound

SET JCLBUILTDIR=%JCLROOT%\..\thirdparty\InnoSetup\setupbuild
if not exist "%JCLROOT%\..\Install.iss" goto JclInnoSetupDirFound
goto NoJclInnoSetupDirFound
:JclInnoSetupDirFound

SET InnoSetupDir=%JCLBUILTDIR%\..\InnoSetup

:: == Sanity checks ==
if not exist "%SETUPDIR%\Install.iss" goto NoInstallDir
if not exist "%InnoSetupDir%\..\Install.iss" goto NoJclInnoSetupDirFound
if not exist "%JCLBUILTDIR%\lib\win32\Jcl.dcp" goto NoJclLibDirFound
if not exist "%JVCLROOT%\run\JVCLVer.pas" goto NoRootDirFound

:: ==========================================================
:: Compile JVCL
:: ==========================================================

:: == Create output directories ==
md "%JVCLBUILTDIR%" 2>NUL >NUL
md "%JVCLBUILTDIR%\hpp" 2>NUL >NUL
md "%JVCLBUILTDIR%\lib" 2>NUL >NUL
md "%JVCLBUILTDIR%\bpl" 2>NUL >NUL

:: == Delete all files in the output directories, we always want to rebuild them ==
if "-%JVCLBUILTDIR%" == "-" GOTO NoRootDirFound
del /Q /S "%JVCLBUILTDIR%\*.*" 2>NUL >NUL

:: == Compile the files
SET JvclLib=%JVCLBUILTDIR%\lib\win32

cd /d "%JVCLROOT%"
msbuild make.proj "/p:Platform=win32" "/p:ForceBCBCompile=true" "/p:HppOutDir=%JVCLBUILTDIR%\hpp" "/p:DcuOutDir=%JVCLBUILTDIR%\lib\win32" "/p:BplOutDir=%JVCLBUILTDIR%\bpl" "/p:JclLibDir=%JCLBUILTDIR%\lib\win32" "/p:JclIncDir=%JCLROOT%\source\include"
if ERRORLEVEL 1 goto Failed
if not exist "%BDS%\bin\dcc64.exe" goto NoWin64
msbuild make.proj "/p:Platform=win64" "/p:ForceBCBCompile=true" "/p:HppOutDir=%JVCLBUILTDIR%\hpp64" "/p:DcuOutDir=%JVCLBUILTDIR%\lib\win64" "/p:BplOutDir=%JVCLBUILTDIR%\bpl\Win64" "/p:JclLibDir=%JCLBUILTDIR%\lib\win64" "/p:JclIncDir=%JCLROOT%\source\include"
if ERRORLEVEL 1 goto Failed
:: For 64bit we have to install both win32 and lib\win64
SET JvclLib=%JVCLBUILTDIR%\lib
:NoWin64
cd /d "%SETUPDIR%"

:: Generate Settings.iss file
del Settings.iss >NUL 2>NUL
dcc32 -E. "-U%JVCLBUILTDIR%\lib\win32;%JCLBUILTDIR%\lib\win32;%BDS%\lib\release;%BDS%\lib;%BDS%\lib\win32\release" "-R%JCLBUILTDIR%\lib\win32" GenerateSettings.dpr
if ERRORLEVEL 1 goto Failed
GenerateSettings.exe
del GenerateSettings.exe >NUL


:: ==========================================================
:: Compile Setup
:: ==========================================================
:Setup
"%InnoSetupDir%\ISCC.exe" Install.iss /dCmdLineBuild "/dJvclRoot=%JVCLROOT%" "/dJvclLib=%JvclLib%" "/dJvclBpl=%JVCLBUILTDIR%\bpl" "/dJvclHpp="%JVCLBUILTDIR%\hpp"
if ERRORLEVEL 1 goto Failed


goto Leave

:NoInstalLDirFound
echo You must start BuildSetup.bat from the JclInnoSetup directory.
goto Failed

:NoJclInnoSetupDirFound
echo The JCL Inno Setup direcory was not found.
goto Failed

:NoJclLibDirFound
echo The Jcl.dcp was not found in "%JCLBUILTDIR%\lib"
goto Failed

:NoRootDirFound
echo "%JCLROOT%" is not the JCL root directory.

:Failed
echo.
pause

:Leave
cd /d %CURDIR%
ENDLOCAL
