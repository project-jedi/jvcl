#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# devtools                                                                                         #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#---------------------------------------------------------------------------------------------------
SRC = ..\..\Source
ARCH = ..\..\Archive
COM = ..\Common
BIN = ..\Bin
DCU = ..\Dcu
JCL = ..\..\..\JCL\source
DRC = $&.drc
SRCP = $(SRC);$(COM);$(JCL);$(ARCH);$(DCU)
SRCH = ..\$(SRC);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\make.exe -$(MAKEFLAGS) -f$**
DCC  = $(ROOT)\dcc32.exe -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -m
DCCH = $(ROOT)\dcc32.exe -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -m
BRCC = $(ROOT)\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
default: \
isu.exe \
JConvert.exe \
JTouch.exe \
crlf.exe \
MakeDOF.exe \
MakeRC.exe \
dc.exe \
MakePNG.exe \
Res2BMP.exe \
ErrLook.exe \

#---------------------------------------------------------------------------------------------------

isu.exe: ITEStrip\isu.dpr
  cd ITEStrip
  $(DCC) $&.dpr
  cd ..

MakeRC.exe: MakeRC\MakeRC.dpr
  cd MakeRC
  $(DCC) $&.dpr
  cd ..

jconvert.exe: JConvert\jconvert.dpr
  cd JConvert
  $(DCC) $&.dpr
  cd ..

MakeDOF.exe: MakeDOF\MakeDOF.dpr
  cd MakeDOF
  $(DCC) $&.dpr
  cd ..

jtouch.exe: JTouch\jtouch.dpr
  cd JTouch
  $(DCC) $&.dpr
  cd ..

crlf.exe: JvAdjustLineBreaks\crlf.dpr
  cd JvAdjustLineBreaks
  $(DCC) $&.dpr
  cd ..

dc.exe: DFMCleaner\dc.dpr
  cd DFMCleaner
  $(DCC) $&.dpr
  cd ..

# these are put last because they are most likely to fail (needs additional units from other libraries)
MakePNG.exe: MakePNG\MakePNG.dpr
  cd MakePNG
  $(DCC) $&.dpr
  cd ..

Res2Bmp.exe: Res2Bmp\Res2Bmp.dpr
  cd Res2Bmp
  $(DCC) $&.dpr
  cd ..

ErrLook.exe: ErrLook\src\ErrLook.dpr
  cd ErrLook\src
  $(DCCH) $&.dpr
  cd ..\..
