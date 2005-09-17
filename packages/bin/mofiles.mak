#-------------------------------------------------------------------------------
# mo file creation for JEDI Visual Component Library (and JVCLX)
#-------------------------------------------------------------------------------

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

MOFILES = *JVCLInstall.mo *jvcl.mo

MAKE = "$(ROOT)\bin\make.exe" -l+

#-------------------------------------------------------------------------------

LANGUAGES=bg de es fr it nl pl ro ru sv

#-------------------------------------------------------------------------------

.suffixes .po

.po.mo:
	@echo Generating: $<
	@"$(DXGETTEXTDIR)\msgfmt" -o "$@" "$<" >NUL
	
#-------------------------------------------------------------------------------

default: $(LANGUAGES)

bg: $(MOFILES:*=bg\LC_MESSAGES\)

de: $(MOFILES:*=de\LC_MESSAGES\)

es: $(MOFILES:*=es\LC_MESSAGES\)

fr: $(MOFILES:*=fr\LC_MESSAGES\)

it: $(MOFILES:*=it\LC_MESSAGES\)

nl: $(MOFILES:*=nl\LC_MESSAGES\)

pl: $(MOFILES:*=pl\LC_MESSAGES\)

ro: $(MOFILES:*=ro\LC_MESSAGES\)

ru: $(MOFILES:*=ru\LC_MESSAGES\)

sv: $(MOFILES:*=sv\LC_MESSAGES\)
