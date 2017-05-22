@echo Extracts BMP images from resource files (res and dcr), builds a RC file and converts BMP to PNG...
Res2Bmp *.res
Res2Bmp *.dcr
MakeRC *.bmp
rem ren new.rc JVCLReg.rc
mkdir PNG
MakePng *.bmp PNG\