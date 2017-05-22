#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JVCL Examples                                                                                     #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#---------------------------------------------------------------------------------------------------
# some of these paths probably needs to be changed for everything to compile...
SRC = ..\..\..\run
ARCH = ..\..\..\archive
COM = ..\..\..\common
BIN = ..\..\..\bin
DCU = ..\..\..\Dcu
JCL = ..\..\..\..\..\JCL\source;..\..\..\..\..\JCL\source\windows;..\..\..\..\..\JCL\source\common
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
Js.exe \

#---------------------------------------------------------------------------------------------------

Js.exe: Surveyor\Js.dpr
  cd Surveyor
  $(DCC) $&.dpr
  cd..
