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

!ifndef LIBDIR
LIBDIR = $(ROOT)\Projects\lib
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

MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS)
DCC = "$(ROOT)\bin\dcc32.exe" -Q -M

#-------------------------------------------------------------------------------

default: \
  Templates \
  pg.exe \
  Compile \
  Clean

CleanJcl: \
  ChangeDirPackageDir \
  Clean

Bpg2Make.exe:
	@echo [Compiling: Bpg2Make.exe]
	cd $(DEVTOOLS)
	$(MAKE) -f makefile.mak -s Bpg2Make.exe
	cd $(DEVTOOLS_BACK)

pg.exe: Templates
	@echo [Compiling: pg.exe]
	cd $(DEVTOOLS)
	$(MAKE) -f makefile.mak -s pg.exe
	cd $(DEVTOOLS_BACK)
	#
	@echo [Generating: Delphi Packages]
	$(DEVTOOLS)\bin\pg.exe -m=JCL -p="$(JCLROOT)\Packages" -t=c$(VERSION) -x=$(DEVTOOLS)\bin\pgEdit.xml

Templates:
	@echo [Copying: Templates]
	if NOT EXIST "$(PACKAGEDIR)\template.dpk"  copy /D /Y jcldcpdpk$(VERSION).tpl "$(PACKAGEDIR)\template.dpk"
	# create template.cfg
	@echo -I"..\..\source;..\..\source\common" > $(PACKAGEDIR)\template.cfg
	@echo -U"..\..\source\common;..\..\source\windows" >> $(PACKAGEDIR)\template.cfg
	@echo -U"..\..\source\vcl;..\..\source\visclx" >> $(PACKAGEDIR)\template.cfg
	@echo -U"$(ROOT)\Lib\Obj;$(DCPDIR)" >> $(PACKAGEDIR)\template.cfg
	@echo -LN"$(DCPDIR)" >> $(PACKAGEDIR)\template.cfg
	@echo -N"$(JCLROOT)\lib\c$(VERSION)" >> $(PACKAGEDIR)\template.cfg
	@echo -O"$(JCLROOT)\lib\c$(VERSION)\obj" >> $(PACKAGEDIR)\template.cfg

ChangeDirPackageDir:
	@cd $(PACKAGEDIR)

Compile: ChangeDirPackageDir
	@echo [Compiling: Packages]
	for %f in ("C*.dpk") do $(DCC) "%f"

Clean:
	@echo [Cleaning...]
	-del /q C*.dcp 2>NUL
	-del /q C*.bpl 2>NUL
	-del /q *.lsp 2>NUL
	-del /q *.dpk 2>NUL
	-del /q *.dcu 2>NUL
	-del /q *.cfg 2>NUL
	-del /q *.log 2>NUL
