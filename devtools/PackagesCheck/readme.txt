********************************************************
*         About PackagesCheck developer tool           *
********************************************************

The main goal of this tool is to remove useless dependencies
and to avoid missing dependencies of packages.

This devtool is the addition of a simple pascal parser and a
xml file storing informations about units provided for each
target.

Defines can be configured for each target:
 - compiler symbols (used by compiler conditions)
 - directory to find included files
 - units provided by packages

To accelerate the enumeration of units, the "Load package"
button allows the user to specify one or more BPL file to be
examinated. The tool enumerates exported function names to
gets unit names. This method has a limitation: a unit that
exports nothing (which only contains constants, resourcestrings,
external functions...) can not be automatically added, user
will have to explicit them using the "Add Unit button". The
"Load button" doesn't delete prefixes and suffixes from
libraries.

The /simple/ pascal parser is only designed to enumerate
units used by a file. It enters into included files
{$I <filename>} or {$INCLUDE <filename>}, and interpretes
the following compiler conditions: {$IFDEF <symbol>}, 
{$IFNDEF <symbol>}, {$ELSE (symbol)}, {$ENDIF (symbol)}, 
{$DEFINE <symbol>}, {$UNDEF <symbol>}.
Locations for included files can be configured for each
target.