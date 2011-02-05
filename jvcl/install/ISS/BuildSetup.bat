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
if not exist "%JCLBUILTDIR%\lib\Jcl.dcp" goto NoJclLibDirFound


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
cd %JVCLROOT%
msbuild make.proj "/p:HppOutDir=%JVCLBUILTDIR%\hpp" "/p:DcuOutDir=%JVCLBUILTDIR%\lib" "/p:BplOutDir=%JVCLBUILTDIR%\bpl" "/p:JclLibDir=%JCLBUILTDIR%\lib"
if ERRORLEVEL 1 goto Failed
cd %SETUPDIR%

:: ==========================================================
:: Compile Setup
:: ==========================================================
:Setup
"%InnoSetupDir%\ISCC.exe" Install.iss /dCmdLineBuild "/dJvclRoot=%JVCLROOT%" "/dJvclLib=%JVCLBUILTDIR%\lib" "/dJvclBpl=%JVCLBUILTDIR%\bpl" "/dJvclHpp="%JVCLBUILTDIR%\hpp"
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