#-------------------------------------------------------------------------------
# DCP files creation for JEDI Code Library
#-------------------------------------------------------------------------------

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCLROOT
JCLROOT = ..\..\..\jcl
!endif

!ifndef DCPDIR
DCPDIR = $(ROOT)\Projects\bpl
!endif

!ifndef VERSION
!error You must specify a VERSION: make -DVERSION=6
!endif

#-------------------------------------------------------------------------------

JVCLPACKAGEDIR = ..
JVCLDIR = $(JVCLPACKAGEDIR)\..
DEVTOOLS = $(JVCLDIR)\devtools
DEVTOOLS_BACK = ..\packages\bin
PACKAGEDIR = $(JCLROOT)\packages\c$(VERSION)

MAKE = "$(ROOT)\bin\make.exe" -l+
#-$(MAKEFLAGS)
DCC = "$(ROOT)\bin\dcc32.exe" -Q -M

#-------------------------------------------------------------------------------

default: \
  Templates \
  pg.exe \
  Compile \
  Clean

Bpg2Make.exe:
	@echo [Compiling: Bpg2Make.exe]
	cd $(DEVTOOLS)
	$(MAKE) $(QUIET) -f makefile.mak -s Bpg2Make.exe
	cd $(DEVTOOLS_BACK)

pg.exe: Templates
	@echo [Compiling: pg.exe]
	cd $(DEVTOOLS)
	$(MAKE) $(QUIET) -f makefile.mak -s pg.exe
	cd $(DEVTOOLS_BACK)
	#
	@echo [Generating: Packages]
	$(DEVTOOLS)\bin\pg.exe -m=JCL -p="$(JCLROOT)\Packages" -t=c$(VERSION) -x=$(DEVTOOLS)\bin\pgEdit.xml

Templates:
	@echo [Copying: Templates]
	if NOT EXIST "$(PACKAGEDIR)\template.dpk"  copy /Y jcldcpdpk$(VERSION).tpl "$(PACKAGEDIR)\template.dpk"
	# create template.cfg
	@echo -I"..\..\source;..\..\source\common" > "$(PACKAGEDIR)\template.cfg"
	@echo -U"..\..\source\common;..\..\source\windows" >> "$(PACKAGEDIR)\template.cfg"
	@echo -U"..\..\source\vcl;..\..\source\visclx" >> "$(PACKAGEDIR)\template.cfg"
	@echo -U"$(ROOT)\Lib\Obj;$(DCPDIR)" >> "$(PACKAGEDIR)\template.cfg"
	@echo -LN"$(DCPDIR)" >> "$(PACKAGEDIR)\template.cfg"
	@echo -N"$(JCLROOT)\lib\c$(VERSION)" >> "$(PACKAGEDIR)\template.cfg"
	@echo -O"$(JCLROOT)\lib\c$(VERSION)\obj" >> "$(PACKAGEDIR)\template.cfg"

CompileJclDcpPackage:
	echo [Compiling: $(FILE)]
	$(DCC) -B "$(FILE)"

Compile:
	@echo [Compiling: Packages]
	@cd $(PACKAGEDIR)
	for %f in ("C*.dpk") do $(MAKE) -f "$(JVCLROOT)\packages\bin\MakeJCLDcp4BCB.mak" $(QUIET) "-DFILE=%f" CompileJclDcpPackage

Clean:
	@echo [Cleaning...]
	@cd $(PACKAGEDIR)
	-del /q template.* >NUL
	-del /q C*.dcp >NUL
	-del /q C*.bpl >NUL
	-del /q C*.mak >NUL
	-del /q C*.dpk >NUL
	-del /q *.dcu >NUL
	-del /q C*.cfg >NUL
