#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# devtools                                                                                         #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
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
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC  = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -m
DCCH = $(ROOT)\bin\dcc32.exe -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -m
BRCC = $(ROOT)\bin\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
default: \
isu.exe \
JConvert.exe \
JTouch.exe \
crlf.exe \
MakeDOF.exe \
MakeCFG.exe \
Bgp2Make \
MakeRC.exe \
dc.exe \
MakePNG.exe \
Res2BMP.exe \
ErrLook.exe \
stripCmtPO.exe \
dxgettextResstr.exe \
pg.exe \
pgEdit.exe \

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

MakeCFG.exe: MakeCFG\MakeCFG.dpr
  cd MakeCFG
  $(DCC) $&.dpr
  cd ..

Bpg2Make.exe: Bpg2Make\Bpg2Make.dpr
  cd Bpg2Make
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

stripCmtPO.exe: stripCmtPO\stripCmtPO.dpr
  cd stripCmtPO
  $(DCC) $&.dpr
  cd ..

dxgettextResstr.exe: dxgettextResstr\dxgettextResstr.dpr
  cd dxgettextResstr
  $(DCC) $&.dpr
  cd ..

dc.exe: DFMCleaner\dc.dpr
  cd DFMCleaner
  $(DCC) $&.dpr
  cd ..
  
pg.exe: PackageGenerator\pg.dpr
  cd PackageGenerator
  $(DCC) $&.dpr
  cd..

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
  
pgEdit.exe: PackageGenerator\pgEdit.dpr
  cd PackageGenerator
  $(DCC) $&.dpr
  cd..
  
