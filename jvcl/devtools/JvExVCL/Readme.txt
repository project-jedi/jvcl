Directory structure
-------------------
.\         preprocessed files
source\    source files that must be preprocessed


Files
-----
.\JvBitBtn.pas                demo component
.\preprocess.bat              preprocess the source\JvExXxx files
.\Readme.txt                  this file
source\dpp.exe                Delphi language preprocessor (http://www.sf.net/projects/dpp32)
source\build.pas              used for preprocessing
source\JvExControls.pas       base system and interfaces
source\JvExXxx                wrapper classes for the VCL controls
source\JEDI.INC               copy of jvcl\common\JEDI.INC for dpp.exe
source\JVCL.INC               copy of jvcl\common\JEDI.INC for dpp.exe
source\JvExControls.macros    macros used by the preprocessor
