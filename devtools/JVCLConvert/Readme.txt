----------------------
The "JVCL Convert" will compile ONLY under Delphi 6
---------------------

The "JVCL Convert" is a program to Search/Replace any strings in a given text file. You can create your own sets of strings that you can apply repeatedly to your files.

JVCL provides you with several *.dat files (e.g. "RxLib.dat" file)  that can be used to convert to JVCL any Delphi files (*.pas, *.dpr, *.dpk, *.dfm) utilizing other library components/files (e.g. RxLib, RALib). In the future we will provide files for conversion of files based on Jans  components.

In order to use "JVCL Convert" as a conversion tool, please follow the steps below:

- open "JVCL Convert"

- select files to convert: you can do it in three different ways:

a) drag & drop from a Windows Explorer
b) select a subdirectory and scan all Delphi files (*.pas, *.dpr, *.dpk, *.dfm) in it and its subdirectories
c) Open and select individual files

- once the files are selected, you can remove any of them, if needed (just select them, and press "Delete")

- open the appropriate "*.dat" file (e.g. RxLib.dat). It contains all the strings that need to be replaced.

- click on the convert icon.

If you want to check the process first before making any real changes to your files, you can use the "Simulation" mode (see Options)

We have also provided a "Delphi2CompilerDefs.dat" file, that you can use to convert DelphiXXX define settings to CompilerXXX settings.

SUPPLIED DAT FILES:
===================
RxLib.dat - use to convert the RxLib 2.75 source files to JVCL standard
RaLib.dat - use to convert the RaLib source files to JVCL standard
Delphi2CompilerDefs.dat - use to convert DELPHIXXX defines to COMPILERXXX defines
RxToJVCLApp.dat - use to convert your RxLib enabled application to JVCL equivalent

IMPORTANT:
=============
Please note: for best results, you should leave all the Options as they are, i.e. checked, especially the "Whole words only"

To report bugs, please use the JVCL Issue Tracker (follow the "Bugs/Wishes" link from the JVCL Webpage: 

http://jvcl.sourceforge.net
 