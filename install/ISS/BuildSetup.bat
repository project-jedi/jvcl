@echo off
SETLOCAL
SET SETUPDIR=%CD%

:: ==========================================================
:: rsvars.bat check
:: ==========================================================
if not "-%BDS%" == "-" goto RsVarsCalled
call rsvars.bat
if "-%BDS%" == "-" goto Leave

:RsVarsCalled
SET JVCLROOT=%SETUPDIR%\..\..
SET JVCLBUILTDIR=%SETUPDIR%\setupbuild
SET JCLBUILTDIR=%JVCLROOT%\..\JclInnoSetup\setupbuild
SET InnoSetupDir=%JCLBUILTDIR%\..\InnoSetup

:: == Sanity checks ==
if not exist "%JVCLROOT%\run\JVCLVer.pas" goto NoRootDirFound
if not exist "%SETUPDIR%\Install.iss" goto NoInstallDir
if not exist "%InnoSetupDir%\..\Install.iss" goto NoJclInnoSetupDirFound
if not exist "%JCLBUILTDIR%\lib\win32\Jcl.dcp" goto NoJclLibDirFound


:: ==========================================================
:: Compile JVCL
:: ==========================================================

:: == Create output directories ==
md "%JVCLBUILTDIR%" 2>NUL >NUL
md "%JVCLBUILTDIR%\hpp" 2>NUL >NUL
md "%JVCLBUILTDIR%\lib" 2>NUL >NUL
md "%JVCLBUILTDIR%\bpl" 2>NUL >NUL

:: == Delete all files in the output directories, we always want to rebuild them ==
del /Q /S "%JVCLBUILTDIR%\*.*" 2>NUL >NUL

:: == Compile the files
SET JvclLib=%JVCLBUILTDIR%\lib\win32

cd %JVCLROOT%
msbuild make.proj "/p:Platform=win32" "/p:HppOutDir=%JVCLBUILTDIR%\hpp" "/p:DcuOutDir=%JVCLBUILTDIR%\lib\win32" "/p:BplOutDir=%JVCLBUILTDIR%\bpl" "/p:JclLibDir=%JCLBUILTDIR%\lib\win32"
if ERRORLEVEL 1 goto Failed
if not exist "%BDS%\bin\dcc64.exe" goto NoWin64
msbuild make.proj "/p:Platform=win64" "/p:HppOutDir=%JVCLBUILTDIR%\hpp64" "/p:DcuOutDir=%JVCLBUILTDIR%\lib\win64" "/p:BplOutDir=%JVCLBUILTDIR%\bpl\Win64" "/p:JclLibDir=%JCLBUILTDIR%\lib\win64"
if ERRORLEVEL 1 goto Failed
:: For 64bit we have to install both win32 and lib\win64
SET JvclLib=%JVCLBUILTDIR%\lib
:NoWin64
cd %SETUPDIR%

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
cd %SETUPDIR%
ENDLOCAL