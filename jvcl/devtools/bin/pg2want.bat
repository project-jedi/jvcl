if NOT EXIST pg2want.exe goto make
:build
pg2want.exe ..\..\packages\xml\*.xml separate.xml fixed.xml ./jvcl/ >pg2want.log
goto end
:make
cd..
make pg2want.exe
cd bin
goto build
:end
