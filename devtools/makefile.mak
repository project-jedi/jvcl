#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# devtools                                                                                         #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
SRC = ..\..\Run
ARCH = ..\..\Archive
COM = ..\Common;..\..\Common
BIN = ..\Bin
DCU = ..\Dcu
!ifndef JCLDIR
JCLDIR = ..\..\..\JCL
!endif
JCL = $(JCLDIR)\source;$(JCLDIR)\source\common;$(JCLDIR)\source\windows;$(JCLDIR)\source\vcl;$(JCLDIR)\source\visclx
DRC = $&.drc
SRCP = $(SRC);$(COM);$(JCL);$(ARCH);$(DCU)
SRCH = ..\$(SRC);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)
#---------------------------------------------------------------------------------------------------
MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS) -f$**
DCC  = "$(ROOT)\bin\dcc32.exe" -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -h -m
DCCH = "$(ROOT)\bin\dcc32.exe" -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -h -m
BRCC = "$(ROOT)\bin\brcc32.exe" $**
#---------------------------------------------------------------------------------------------------
default: \
isu.exe \
JConvert.exe \
JVCLConvert.exe \
JTouch.exe \
crlf.exe \
MakeDOF.exe \
MakeCFG.exe \
Bpg2Make.exe \
MakeRC.exe \
dc.exe \
NoQuotes.exe \
pg.exe \
pgEdit.exe \
ErrLook.exe \
MakePNG.exe \
Res2BMP.exe \
stripCmtPO.exe \
dxgettextResstr.exe \

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
  
pg.exe: bin\pg.exe

bin\pg.exe: PackagesGenerator\pg.dpr
  @cd PackagesGenerator
  $(DCC) $&.dpr
  @cd ..

pgEdit.exe: PackagesGenerator\pgEdit.dpr
  cd PackagesGenerator
  $(DCC) $&.dpr
  cd ..

NoQuotes.exe: NoQuotes\NoQuotes.dpr
  cd NoQuotes
  $(DCC) $&.dpr
  cd ..

# these are put last because they are most likely to fail (needs additional units from other libraries)
ErrLook.exe: ErrLook\src\ErrLook.dpr
  cd ErrLook\src
  @echo.
  @echo !!!  get HtmlHlp from from http://delphi-jedi.org (API Library Files)  !!!
  @echo.
  $(DCCH) $&.dpr
  cd ..\..

MakePNG.exe: MakePNG\MakePNG.dpr
  cd MakePNG
  @echo.
  @echo !!!  Please be sure to download pngImage from http://pngimage.sf.net/  !!!
  @echo.
  $(DCC) $&.dpr
  cd ..

Res2Bmp.exe: Res2Bmp\Res2Bmp.dpr
  cd Res2Bmp
  @echo.
  @echo !!!  get missing files from http://www.wilsonc.demon.co.uk/d7resourceutils.htm  !!!
  @echo.
  $(DCC) $&.dpr
  cd ..

JVCLConvert.exe: JVCLConvert\JVCLConvert.dpr
  cd JVCLConvert
  $(DCC) $&.dpr
  cd ..