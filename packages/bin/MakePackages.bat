@echo off

if %1! == ! goto ALL
if %1 == all goto ALL

buildtarget.exe %1 -s GeneratePackages
goto END

:ALL
echo.

buildtarget.exe c5 -s GeneratePackages
buildtarget.exe c6 -s GeneratePackages
buildtarget.exe c6p -s GeneratePackages
buildtarget.exe d5 -s GeneratePackages
buildtarget.exe d5p -s GeneratePackages
buildtarget.exe d6 -s GeneratePackages
buildtarget.exe d6p -s GeneratePackages
buildtarget.exe d7 -s GeneratePackages
buildtarget.exe d7p -s GeneratePackages
buildtarget.exe d7clx -s GeneratePackages

:END
