#-------------------------------------------------------------------------------
# mo file creation for JEDI Visual Component Library (and JVCLX)
#-------------------------------------------------------------------------------

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

MAKE = "$(ROOT)\bin\make.exe" -l+

#-------------------------------------------------------------------------------

LANGUAGES=de es fr it nl ro ru sv

#-------------------------------------------------------------------------------

default: $(LANGUAGES)


de:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

es:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

fr:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

it:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

nl:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

ro:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

ru:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

sv:
	echo Generating: $&
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\JVCLInstall.mo $&\LC_MESSAGES\JVCLInstall.po >NUL
	"$(EXTRAUNITDIRS)\msgfmt" -o $&\LC_MESSAGES\jvcl.mo $&\LC_MESSAGES\jvcl.po >NUL

