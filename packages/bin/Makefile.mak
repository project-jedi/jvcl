#-------------------------------------------------------------------------------
# package creation for JEDI Visual Component Library (and JVCLX)
#-------------------------------------------------------------------------------

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCLROOT
JCLROOT = ..\..\..\jcl
!endif

!ifndef BPLDIR
BPLDIR = $(ROOT)\Projects\bpl
!endif

!ifndef DCPDIR
DCPDIR = $(ROOT)\Projects\bpl
!endif

!ifndef LIBDIR
LIBDIR = $(ROOT)\Projects\lib
!endif

#-------------------------------------------------------------------------------

!ifndef EDITION
!error You must specify a EDITION: make -DEDITION=d6 or make -DEDITION=d5p (Packages Generator)
!endif

!ifndef VERSION
!error You must specify a VERSION: make -DVERSION=6
!endif

!ifndef PKGDIR
!error You must specify a PKGDIR: make -DPKGDIR=d6 or make -DPKGDIR=d5std (\packages\* Packages.bpg)
!endif

!ifndef UNITOUTDIR
!error You must specify a UNITOUTDIR: make -DUNITOUTDIR=C:\jvclfolder\lib\d7
!endif

#-------------------------------------------------------------------------------

JVCLPACKAGEDIR = ..
JVCLROOT = $(JVCLPACKAGEDIR)\..
DEVTOOLS = $(JVCLROOT)\devtools
DEVTOOLS_BACK = ..\packages\bin

#CONFIGFILENAME=template.cfg
CONFIGFILENAME=dcc32.cfg
CFGFILE=..\$(PKGDIR)\$(CONFIGFILENAME)

!ifndef PERSONALEDITION_OPTION
# It is not allowed to be empty
PERSONALEDITION_OPTION = -DDUMMYDUMMY
!endif

!ifndef EXTRAUNITDIRS
EXTRAUNITDIRS=.
!endif
!ifndef EXTRAINCLUDEDIRS
EXTRAINCLUDEDIRS=.
!endif
!ifndef EXTRARESDIRS
EXTRARESDIRS=.
!endif

!ifndef DCCOPT
DCCOPT=-Q -M
!endif

MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS)
DCC = "$(ROOT)\bin\dcc32.exe" $(DCCOPT)

#-------------------------------------------------------------------------------

JCLSOURCEDIRS1=$(JCLROOT)\source\common;$(JCLROOT)\source\windows
JCLSOURCEDIRS2=$(JCLROOT)\source\vcl;$(JCLROOT)\source\visclx
JCLINCLUDEDIRS=$(JCLROOT)\source;$(JCLROOT)\source\common

JVCLSOURCEDIRS1=$(JVCLROOT)\common;$(JVCLROOT)\run
JVCLSOURCEDIRS2=$(JVCLROOT)\design
JVCLINCLUDEDIRS=$(JVCLROOT)\common
JVCLRESDIRS=$(JVCLROOT)\Resources

#-------------------------------------------------------------------------------

default: \
	BuildJCLdcpFiles \
	Resources \
	pg.exe \
	Compile \
	Clean


################################################################################
BuildJCLdcpFiles:
	# for C++ targets compile JCL .dcp files
	IF EXIST "$(ROOT)\bin\bcc32.exe" IF NOT EXIST "$(DCPDIR)\CJcl*.dcp" $(MAKE) -s -f MakeJCLDcp4BCB.mak

################################################################################
BuildJCLdcpFilesForce:
	# for C++ targets compile JCL .dcp files
	IF EXIST "$(ROOT)\bin\bcc32.exe" $(MAKE) -s -f MakeJCLDcp4BCB.mak

################################################################################
Resources:
	cd ..\..\images
	$(MAKE) -f makefile.mak
	cd ..\packages\bin

################################################################################
Bpg2Make.exe:
	@echo [Compiling: Bpg2Make.exe]
	@cd $(DEVTOOLS)
	$(MAKE) -f makefile.mak -s Bpg2Make.exe
	@cd $(DEVTOOLS_BACK)
	$(DEVTOOLS)\bin\Bpg2Make.exe "..\$(PKGDIR) Packages.bpg"

################################################################################
GeneratePackages:
	@echo [Compiling: pg.exe]
	@cd $(DEVTOOLS)
	#-SET C5PFLAGS=
	#if $(VERSION)==5 SET C5PFLAGS=-LUvcl50
	$(MAKE) -f makefile.mak -s pg.exe
	#-SET C5PFLAGS=
	@cd $(DEVTOOLS_BACK)
	echo [Generating: JVCL Packages]
	@$(DEVTOOLS)\bin\pg.exe -m=JVCL -p="$(JVCLPACKAGEDIR)" -t=$(EDITION) -x=$(DEVTOOLS)\bin\pgEdit.xml
	@IF NOT $(MASTEREDITION)! == ! @$(DEVTOOLS)\bin\pg.exe -m=JVCL -p="$(JVCLPACKAGEDIR)" -t=$(MASTEREDITION) -x=$(DEVTOOLS)\bin\pgEdit.xml

################################################################################
pg.exe: Templates GeneratePackages
	# do nothing

################################################################################
configfile:
	# create dcc32.cfg file
	@echo Writing: $(CFG)
	-@del /q "$(CFG)" 2>NUL
	-@IF EXIST "$(ROOT)\bin\dcc32.cfg" @type "$(ROOT)\bin\dcc32.cfg" >>"$(CFG)"
	@echo. >>"$(CFG)"
	@echo -Q>>"$(CFG)"
	@echo -U"$(ROOT)\Lib;$(ROOT)\Lib\Obj">>"$(CFG)"
	@echo -R"$(ROOT)\Lib">>"$(CFG)"
	@echo -I"$(ROOT)\Include;$(ROOT)\Include\Vcl">>"$(CFG)"
	@echo -U"$(DCPDIR);$(LIBDIR);$(BPLDIR)">>"$(CFG)"
	#
	@echo -I"$(JCLINCLUDEDIRS)">>"$(CFG)"
	@echo -U"$(JCLSOURCEDIRS1)">>"$(CFG)"
	@echo -U"$(JCLSOURCEDIRS2)">>"$(CFG)"
	#
	@echo -I"$(JVCLINCLUDEDIRS)">>"$(CFG)"
	@echo -U"$(UNITOUTDIR);$(LIBDIR)">>"$(CFG)"
	@echo -U"$(JVCLSOURCEDIRS1)">>"$(CFG)"
	@echo -U"$(JVCLSOURCEDIRS2)">>"$(CFG)"
	@echo -R"$(JVCLRESDIRS)">>"$(CFG)"
	#
	@echo -U"$(EXTRAUNITDIRS)">>"$(CFG)"
	@echo -I"$(EXTRAINCLUDEDIRS)">>"$(CFG)"
	@echo -R"$(EXTRARESDIRS)">>"$(CFG)"


################################################################################
Templates:
	@echo [Generating: Templates]
	@$(MAKE) -f makefile.mak "-DCFG=$(CFGFILE)" configfile
	#
	@echo -LE"$(BPLDIR)">>"$(CFGFILE)"
	@echo -LN"$(DCPDIR)">>"$(CFGFILE)"
	@echo $(PERSONALEDITION_OPTION)>>"$(CFGFILE)"
	#
	@echo -N"$(UNITOUTDIR)">>"$(CFGFILE)"
	@echo -N1"$(HPPDIR)">>"$(CFGFILE)"
	@echo -N2"$(UNITOUTDIR)\obj">>"$(CFGFILE)"
	#
	#
	@IF NOT $(MASTEREDITION)! == ! @copy "$(CFGFILE)" "$(PKGDIR_MASTEREDITION)\$(CONFIGFILENAME)"

################################################################################
Compile: Bpg2Make.exe CompilePackages

################################################################################
CompilePackages:
	@echo [Compiling: Packages]
	@cd $(JVCLPACKAGEDIR)
	$(MAKE) -f "$(PKGDIR) Packages.mak" $(MAKEOPTIONS) $(TARGETS)
	@cd bin

################################################################################
Clean:
	@echo [Cleaning...]
	@cd $(JVCLROOT)\packages
	-del /q "$(PKGDIR) Packages.mak" 2>NUL
	-del /q "$(PKGDIR)\*.cfg" "$(PKGDIR)\*.mak" 2>NUL
	-del /q tmp.bat 2>NUL
	-@IF NOT "$(PKGDIR_MASTEREDITION)!" == "!" del /f /q "$(PKGDIR_MASTEREDITION)\*.mak" 2>NUL
	@cd bin

################################################################################
Installer:
	@echo [Compiling: Installer]
	$(MAKE) "-DCFG=..\..\install\JVCLInstall\JVCLInstall.cfg" configfile
	@cd ..\..\install\JVCLInstall
	#
	@echo -E"$(JVCLROOT)\bin">>JVCLInstall.cfg
	@echo -N"$(JVCLROOT)\dcu">>JVCLInstall.cfg
	@echo -DNO_JCL>>JVCLInstall.cfg
	#
	#
	@$(DCC) -DNO_JCL JVCLInstall.dpr
	@start ..\..\bin\JVCLInstall.exe


