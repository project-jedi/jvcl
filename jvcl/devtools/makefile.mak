#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# devtools                                                                                         #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCLROOT
JCLROOT = ..\..\..\jcl
!endif

.path.exe=bin

#---------------------------------------------------------------------------------------------------
SRC = ..\..\Run
ARCH = ..\..\Archive
COM = ..\Common;..\..\Common
BIN = ..\Bin
DCU = ..\Dcu
JCL = $(JCLROOT)\source;$(JCLROOT)\source\common;$(JCLROOT)\source\windows;$(JCLROOT)\source\vcl;$(JCLROOT)\source\visclx
DRC = $&.drc
SRCP = $(SRC);$(COM);$(JCL);$(ARCH);$(DCU)
SRCH = ..\$(SRC);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)
#---------------------------------------------------------------------------------------------------
MAKE = "$(ROOT)\bin\make.exe" -l+ $(QUIET)
#-$(MAKEFLAGS)
DCC  = "$(ROOT)\bin\dcc32.exe" -e"$(BIN)" -i"$(SRCP)" -n"$(DCU)" -r"$(SRCP)" -u"$(SRCP)" -u"$(ROOT)\Lib\Obj" -q -w -h -m
DCCx = "$(ROOT)\bin\dcc32.exe" -Q -M 
#-I$(COM) -U$(SRC) -U$(COM)
DCCH = "$(ROOT)\bin\dcc32.exe" -e"..\$(BIN)" -i"$(SRCH)" -n"..\$(DCU)" -r"$(SRCH)" -u"$(SRCH)" -q -w -h -m
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
SetPoHeader.exe \
pg.exe \
pgEdit.exe \
JvclVclClx.exe \
LastModifyRepl.exe \
ErrLook.exe \
MakePNG.exe \
Res2BMP.exe \
stripCmtPO.exe \
dxgettextResstr.exe \
pg2want.exe

#---------------------------------------------------------------------------------------------------

# configfile requires symbol "CFG=path\filename.cfg"
configfile:
	-@IF EXIST "$(CFG)" del /q "$(CFG)" >NUL
	-@IF EXIST "$(ROOT)\bin\dcc32.cfg" type "$(ROOT)\bin\dcc32.cfg" >>"$(CFG)"
	@echo. >>"$(CFG)"
	@echo -E"$(BIN)">>"$(CFG)"
	@echo -I"$(SRCP)">>"$(CFG)"
	@echo -N"$(DCU)">>"$(CFG)"
	@echo -R"$(SRCP)">>"$(CFG)"
	@echo -U"$(SRCP)">>"$(CFG)"
	@echo -U"$(ROOT)\Lib\Obj">>$(CFG)"

isu.exe: ITEStrip\isu.dpr
  cd ITEStrip
  $(DCC) $&.dpr
  cd ..

MakeRC.exe: MakeRC\MakeRC.dpr \
		MakeRC\MakeRCUtils.pas
  cd MakeRC
  $(DCC) $&.dpr
  cd ..

jconvert.exe: JConvert\jconvert.dpr \
		JConvert\JConvertUtils.pas
  cd JConvert
  $(DCC) $&.dpr
  cd ..

MakeDOF.exe: MakeDOF\MakeDOF.dpr \
		MakeDOF\MakeDOFUtils.pas
  cd MakeDOF
  $(DCC) $&.dpr
  cd ..

MakeCFG.exe: MakeCFG\MakeCFG.dpr \
		MakeCFG\MakeCFGUtils.pas
  cd MakeCFG
  $(DCC) $&.dpr
  cd ..

Bpg2Make.exe: Bpg2Make\Bpg2Make.dpr \
		Bpg2Make\Bpg2MakeUtils.pas
  @$(MAKE) -DCFG=Bpg2Make\Bpg2Make.cfg configfile >NUL
  @cd Bpg2Make
  $(DCCx) Bpg2Make.dpr
#  -@del Bpg2Make.cfg >NUL 2>NUL
  @cd ..

jtouch.exe: JTouch\jtouch.dpr \
		JTouch\JTouchUtils.pas
  cd JTouch
  $(DCC) $&.dpr
  cd ..

crlf.exe: JvAdjustLineBreaks\crlf.dpr \
		JvAdjustLineBreaks\crlfutils.pas
  cd JvAdjustLineBreaks
  $(DCC) $&.dpr
  cd ..

stripCmtPO.exe: stripCmtPO\stripCmtPO.dpr \
		stripCmtPO\stripUtils.pas
  cd stripCmtPO
  $(DCC) $&.dpr
  cd ..

dxgettextResstr.exe: dxgettextResstr\dxgettextResstr.dpr
  cd dxgettextResstr
  $(DCC) $&.dpr
  cd ..

dc.exe: DFMCleaner\dc.dpr \
		DFMCleaner\dcUtils.pas
  cd DFMCleaner
  $(DCC) $&.dpr
  cd ..

pg.exe: PackagesGenerator\pg.dpr \
		PackagesGenerator\CmdLineUtils.pas \
		PackagesGenerator\FileUtils.pas \
		PackagesGenerator\GenerateUtils.pas
  @$(MAKE) -DCFG=PackagesGenerator\pg.cfg configfile
  @cd PackagesGenerator
  $(DCCx) -DNO_JCL pg.dpr
  -@IF EXIST pg.cfg  del /q pg.cfg >NUL
  @cd ..

pgEdit.exe: PackagesGenerator\pgEdit.dpr \
		PackagesGenerator\AdvancedBCBForm.pas \
		PackagesGenerator\CmdLineUtils.pas \
		PackagesGenerator\CmdLineUtils.pas \
		PackagesGenerator\FileUtils.pas \
		PackagesGenerator\FormTypeDialog.pas \
		PackagesGenerator\GenerateUtils.pas \
		PackagesGenerator\GenerationMessagesForm.pas \
		PackagesGenerator\KnownTagsForm.pas \
		PackagesGenerator\MainForm.pas \
		PackagesGenerator\ModelsForm.pas \
		PackagesGenerator\TargetDialog.pas \
		PackagesGenerator\UtilsJcl.pas
  @$(MAKE) -DCFG=PackagesGenerator\pgEdit.cfg configfile
  @cd PackagesGenerator
  $(DCCx) pgEdit.dpr
  -@IF EXIST pgEdit.cfg  del pgEdit.cfg >NUL
  @cd ..

JvclVclClx.exe: JvclVclClx\JvclVclClx.dpr \
		JvclVclClx\Main.dfm \
		JvclVclClx\Main.pas \
		JvclVclClx\Utils.pas \
		JvclVclClx\VclClxCvt.pas \
		JvclVclClx\VclClxCvtUtils.pas \
		common\PackageInformation.pas \
		common\PackageModels.pas \
		common\dpp_PascalParser.pas
  @$(MAKE) -DCFG=JvclVclClx\JvclVclClx.cfg configfile
  @cd JvclVclClx
  $(DCCx) JvclVclClx.dpr
  -@IF EXIST JvclVclClx.cfg  del JvclVclClx.cfg >NUL
  @cd ..

NoQuotes.exe: NoQuotes\NoQuotes.dpr
  cd NoQuotes
  $(DCC) $&.dpr
  cd ..

SetPoHeader.exe: SetPoHeader\SetPoHeaderUtils.pas \
		SetPoHeader\SetPoHeader.dpr
  cd SetPoHeader
  @echo.
  @echo.
  $(DCC) SetPoHeader.dpr
  cd ..


JVCLConvert.exe: JVCLConvert\JVCLConvert.dpr \
		JVCLConvert\fAboutMe.pas \
		JVCLConvert\fAboutMe.pas \
		JVCLConvert\FastTime.pas \
		JVCLConvert\fJvclConverterMain.pas \
		JVCLConvert\JVCLConvertUtils.pas \
		JVCLConvert\OptionsFrm.pas
  cd JVCLConvert
  @echo.
  @echo.
  $(DCC) $&.dpr
  cd ..
  
# these are put last because they are most likely to fail
ErrLook.exe: ErrLook\src\ErrLook.dpr
  cd ErrLook\src
  @echo.
  @echo.
  $(DCCH) $&.dpr
  cd ..\..

MakePNG.exe: MakePNG\MakePNG.dpr
  cd MakePNG
  @echo.
  @echo.
  $(DCC) $&.dpr
  cd ..

Res2Bmp.exe: Res2Bmp\Res2Bmp.dpr
  cd Res2Bmp
  @echo.
  @echo.
  $(DCC) $&.dpr
  cd ..

pg2want.exe: pg2want\pg2want.dpr
  cd pg2want
  @echo.
  @echo.
  $(DCC) $&.dpr
  cd ..

LastModifyRepl.exe: LastModifyRepl\LastModifyRepl.dpr \
		LastModifyRepl\LastModifyReplUtil.pas
  cd LastModifyRepl
  @echo.
  @echo.
  $(DCC) $&.dpr
  cd ..
