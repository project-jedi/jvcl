JVCL-RA Library 2.03
================

The Set of Native Delphi Components for Borland Delphi.
100% Source Code.

Last revision date Mai, 8, 2002.

PLEASE FOLLOW THE INSTRUCTIONS PROVIDED IN THE INSTALLATION SECTION!

TABLE OF CONTENTS
-----------------
Overview
History
License Agreement
Compatibility
Installation
Installation Problems
Demonstration Programs
Source Files
Credits
Copyright Notes


Overview
--------

JVCL-RA Library contains a number of components, classes and routines
for Borland Delphi with full source code. This library is compatible
with Borland Delphi 2, 3, 4, 5, 6, Borland C++ Builder 1, 4, 5 and 
Borland Kylix 1.
Read Compatibility section for more information.


This collection includes over 20 native Delphi components and special
packages for run-time form designing.

JVCL-RA Library is a freeware product. Feel free to distribute the library as
long as all files are unmodified and kept together.

The authors disclaim all warranties to this software, whether express
or implied, including without limitation any implied warranties of
merchantability or fitness for a particular purpose. Use under your own
responsibility, but comments (even critique) in English (or in Russian)
are welcome.


1. Visual Components:

TRegAuto allows you to read and write virtually any component published
property to an INI file or the system Registry with virtually no code.
It works with 3rd party and your own custom controls as well. Don't be stuck
with dozens of INI-Aware components, use TRegAuto and let it to manage all
that for you. Also it allows to save and to restore form size, position and
window state.

TRAProgressForm is a "Progress" dialog with customizable caption and message
label. When method Execute is called, form will be displayed and OnShow
events occure. You must write whole specific code in this event and
periodically update ProgressPosition property.

TRADBTreeView - data-aware TreeView.
TRADBLookupTreeView and TRADBLookupTreeViewCombo are special versions of
TRADBTreeView component.

TRASQLScript allows multiple SQL statements in one query. "Set term" and
"Commit work" statements response.

TRAEditor is "Delphi-IDE"-like editor. Single symbol can be displayed with
its own font color, style, background color. Customizable keystroke mapping
scheme. Undo. Bookmarks. Code completion. Gutter.
None: Column blocks, Redo.

TRAHLEditor - special version of TRAEditor with built-in highlighting
for pascal, cbuilder, sql, html, java, phyton, basic.

TRAHLEdDlg - dialog for customizing TRAHLEditor properties.

TRAScrollMax works like 3DMax panels. Add few TRAScrollMaxBand
on it and you can expand or collapse bands by click at band
caption button. If height of all bands is bigger than TRAScrollMax
height, then scrolling feature will be on and you can scroll TRAScrollMax
by drag it at any point.

TRACaptionButton jumps to the caption of the form.

TRADBMove - is a data-aware component for batch moving databases (some
tables with dependencies). It's automatically fixes up links beetween
tables. You must define names of tables and references. It is also
available field mappings.

TRAhtListBox - is listbox, which can display items with several font
styles and colors (but only one font size). Few colors can be used
in one item.

TRAhtComboBox, TRAhtLabel, TRAhtButton - are similar to TRAhtListBox.

TRAScrollText - displays scrolling text.


2. Interpreter Components:

JvInterpreter is a small pascal-like language interpreter.

TJvInterpreterProgram - JvInterpreter runner.
See JvInterpreter demo in ralib\demos\JvInterpreter folder.

TJvInterpreterFm - Delphi form runner. It allows to run Delphi forms
placed in external files without compiling.
See JvInterpreter demo in ralib\demos\JvInterpreter folder. Usefull to
create plug-ins, such as reports.


3. Run-time form designer.
Please read file ralib\doc\RAFD.TXT.


4. Design packages for form designer.
Please read file ralib\doc\RAFD.TXT.


5. Delphi ide tools:

Zoom - With Zoom you can maximize and restore edit window by pressing
"Alt+Z" key.
Compatibility: Delphi 3, 4, 5, 6, Builder 3, 4

TRAIntegerProperty - Property editor for all integer properties. It
based on JvInterpreter and use it features. When editor is installed you can
type in Object Inspector expressions for all integer properties, such
as '2+2' and expression will be calculated and replaced with it result.
For example, for Button1.Width property valid expressions are '100+10',
'Width+20', 'Label1.Left + Label1.Width'. If multiple components are
selected, then expression evaluates for each of them. For example, if you
select some components and type for width property string 'Width + 10',
widths of all selected components will be increased to 10 pixels.


6. Units that provide functions and classes to work with databases,
images, strings, files, INI-files.


7. A couple of simple demo applications.
Please read file ralib\demos\!README.TXT.


8. There are no help files for JVCL-RA Library. Sorry.


History
-------

RALib 1.00 (Dec 1998).
  Initial release available on internet.

RALib 1.10 (Feb 1999).
  New components TRAHLEditor, TRAhtListBox, TRAhtComboBox,
  TRAScrollText.
  New interpreter JvInterpreter and components TJvInterpreterProgram, TJvInterpreterFm.
  Improvements in existing components:
  TRAEditor - new properties: SmartTab, KeepTrailingBlanks,
  CursorBeyondEOF, AutoIndent, BackSpaceUnindents; two-key commands,
  automatically expands tabs when setting Lines property; Some bugs
  fixed.
  TRADBMove - new property: Mappings.

RALib 1.15 (Mar 1999).
  Mainly bug-fix release.
  Fixed: JvInterpreter.pas, RAEditor.pas, RAHLEditor.pas.
  Updated:
    TRAScrollMax - new method AddBand;
  New:
    Color Hints.

RALib 1.20 (Apr 1999)
  Fixed many bugs in JvInterpreter components and RAEditor component.
  New features and highlighters for RAEditor.
  New:
    RADoubleCombo.
    JVCL Form Designer.

RALib 1.21 (Apr 1999)
  Some bugs fixed in language resources.

RALib 1.22 (Jun 1999)
  Fixed many bugs in JvInterpreter and RAEditor. Some new JvInterpreter-functions in
  JvInterpreter_System.

RALib 1.30 (Oct 1999)
  Delphi 5 compatibility.

RALib 1.31 (Oct 1999)
  Fixed one bug in RACtrl package.

RALib 1.40 (Jan 2000)
  Many fixes in JvInterpreter, Delphi 5 JVCL Form Designer compatibility.

RALib 1.50 (Jan 2000)
  All packages were split into separate run-time and design-time packages.
  Many fixes in JvInterpreter (see JvInterpreter.pas header) and JVCL Form Designer.
  Adding support for Builder 4.

RALib 1.51 (Jan 2000)
  All components arranged at 3 pallete entries: JVCL Controls, JVCL Additional,
  JVCL DBAware.
  - Changes in JvInterpreter:
    - arrays as local and global variables - added by Andrej Olejnik.
    - type casting with integer, string, TObject, etc. keywords.
  - fixed Delphi5-bug in design-time component editor for TRegAuto component.

RALib 1.52 (Mar 2000)
  - many changes in JvInterpreter;
  - many changes in RAEditor and RAHLEditor;
  - Russian Remark Remover - tool (RALIB\TOOSL\RRR) for chinese-windows release
    users, it solve problem with compiler errors.

RALib 1.53 (Mar 2000)
  Complete list of changes are:
  - changes in JvInterpreter:
   - fixed bug: "Type mistmatch error" in expressions with OleAutomation objects;
   - fixed bug: error while assign function's result to object's published property; 
   - call to external functions (placed in dll) in previous versions always
     return integer, now it can return boolean, if declared so;
  - changes in RAEditor and RAHLEditor:
   - fixed bug: Double Click sometimes raises errors;
  - CBuilder 5 support;

RALib 1.54 (May 2000)
  Complete list of changes are:
  - changes in JvInterpreter:
   - new: in call to external function var-parameters are supported for
     integer type; 
   - new: after call to external function (placed in dll) last win32 error
     is restored correctly; in previous versions it was overriden by call to
     FreeLibrary;
   - fixed bug: memory leak: global variables and constants not allways be freed;
  - Pas2JvInterpreter tool was upgraded to support last JvInterpreter syntax;
  - unit RAConst.pas was renamed to RACnst.pas to avoid conflict with
    ReportBuilder;
  - a lot of improvements in RANotepad demo.

RALib 1.60 (Jun 2000)
  Complete list of changes are:

  - bug fixed: Delphi abort when trying to display RARegAuto's Editor on
    form, which contains IBDatabase component;

  - RAEditor and RAHLEditor:
   - undo in overwrite mode;
   - bug fixed: highlight problem in perl syntax in RAHLEditor;
   - fixed bug: double click not selects word on first line;
   - selection work better after consecutive moving to begin_of_line and
     end_of_line, and in other cases;
   - 4 block format are supported now: NonInclusive (default), Inclusive,
      Line (initial support), Column;
   - painting was improved;
   - DblClick work better (thanks to Constantin M. Lushnikov);
   - fixed bug: caret was moved when mouse moves over raeditor after
     click on any other windows placed over raeditor, which loses focus
     after this click; (anyone understand me ? :)
   - bug fixed: accelerator key do not work on window,
     where raeditor is placed (bug fixed by Luis David Cardenas Bucio);

  - JvInterpreter and JvInterpreterFm:
   - new: in call to external function var-parameters are supported for
     integer type;
   - new: after call to external function (placed in dll) last win32 error
     is restored correctly; in previous versions it was overriden by call to
     FreeLibrary;
   - fixed bug: memory leak: global variables and constants not allways be freed;
   - bug fixed in case-statement;
   - new: global variables and constants in different units now can have
     identical names;
   - new: constants, variables and functions can have prefix with unit name
     and point to determine appropriate unit;
   - new: class declaration for forms (needed for TJvInterpreterFm component);
   - bug fixed: record variables do not work;
   - advanced unit's support;
   - while call to functions JvInterpreterRunFormModal and JvInterpreterRunReport you must pass
     ".pas" filename, against ".dmf" in previous versions;

  - demo "JvInterpreter" upgraded to show new JvInterpreter features;

  - Form Designer has many changes: more features, more stability, more delphi;

RALib 1.60 (Jun 2000)
  Complete list of changes are:
  - adopting RADBTreeView to non-bde datasets,
    by Yakovlev Vacheslav (jwe@belkozin.com)

RALib 1.62 (Jun 2001)
  Complete list of changes are:
  - Delphi 6 compatiblity;
  - mouse wheel support in RAEditor (thanks to Michael Serpik).
  - RAHLEdPropDlg's dialog window now can be customized to
    display application specific tabsheets (see new demo).
  - new demo "RALib\Demos\RAEditor\RAHLEdPropDlgTest.dpr";
  - ANY font can be used (all symbols are printed with same width) 
    (thanks to Rients Politiek);
  - bug fixed: completion ranges error on first line
    (thanks to Walter Campelo);
  - new functions: CanCopy, CanPaste, CanCut in TRACustomEditor
    and function CanUndo in TUndoBuffer (TRACustomEditor.UndoBuffer);
  - fixed bug, which occures with MDI Forms (AV when attempt to 
    close them) and in other cases. (thanks to Ivan Ravin).

RALib 2.00 (Dec 2001)
  - Structure of library was reorganised:
     - dependencies between three main packages (RACtl, RADb, RAI)
       were eliminated. Now any of these compiled packages can
       be used alone. In previous versions of library packages
       RADb and RAI used RACtl package and could not be used
       without it.
     - many very simple components were removed from library;
       there are many other components in world with better functionality.
       Following components were removed:
         TRADBRadioGroupS,
         TRADBTextS,
         TRACombo,
         TRADoubleCombo,
         TRAStatusBar,
         TRAColorButton,
         TRANoFrameButton,
         TRAScrollBar95,
         TRAScrollBar,
         TRAWaitPanel,
         TRATreeView,
         TRAComboBox4Tree,
         TRAImage.
     - Components in pallete are arranged now in different order.
       All components are placed in "JVCL" tab now.
       Component RAHint is not registered on palette anymore,
       because it contains no any published properties.
       The component still exists and can be created manually.
  - Kylix 1 compatibility (only few components);
  - TRAScrollMax: new property ScrollBarVisible;
  - fixed bug in JvInterpreter: intefrace section was not processed correctly;  
  - improvements in RAFD (thanks to Andre Weber):
     - better algorithm for seeking appropriate icon for components,
       which not have their own icons;
     - can load text dfm files, new files are created in text-dfm.
    
RALib 2.01 (Dec 2001)
  - fixed a bug in JvInterpreter with open array argements;
  - Form Designer compatibility with Delphi 6.

RALib 2.02 (Jan 2002)
  - another bug fixed in JvInterpreter with open array arugements;
  - fixed bug: records does not work in JvInterpreter;
  - new: a few components now work correct with right-to-left
    locales, as it is required in arabic countries:
    TRAScrollMax, TRADBTreeView, TRADBTreeViewCombo
    (thanks to Oussama Al-Rifai).


License Agreement
-----------------

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appears in all copies and
that both the above copyright notice and this permission notice appear
in supporting documentation, and that the name of JVCL-RA Library authors
are not used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission. This
software is made available "as is", and JVCL-RA Library AUTHORS DISCLAIM
ALL WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO THIS SOFTWARE,
INCLUDING WITHOUT LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND IN NO EVENT SHALL AUTHORS BE
LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, TORT (INCLUDING NEGLIGENCE) OR
STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

You can find full text of JVCL-RA Library Software License agreement in the
file LICENSE.TXT.


Compatibility
-------------

This version of JVCL-RA Library was written and full tested with
Borland Delphi 5 Enterprise Edition.

JVCL-RA Library was tested a bit on
Borland Delphi 2 Developer Edition,
Borland Delphi 3 Client/Server Edition,
Borland Delphi 4 Client/Server Edition,
Borland Delphi 5 Enterprise Edition,
Borland Delphi 6 Enterprise Edition,
Borland C++ Builder 1.0 Client/Server Edition,
Borland C++ Builder 4 Enterprise Edition,
Borland C++ Builder 5 Enterprise Edition.
Borland Kylix 1 Server Developer Edition,
JVCL-RA Library is not tested properly with this products.
Use in your own risk.


Installation
------------

Unzip ralib.zip.

Before installing JVCL-RA Library components into Delphi, check out RA.INC
file located in RALIB\LIB subdirectory. This file is included in all JVCL
Library units and contains conditional defines that affects compilation.
You can change some of these defines or specify global compiler options
there.

1. Delphi 6:

Uninstall previous installed version of JVCL-RA Library from Delphi 6 IDE.
Remove previously compiled JVCL packages (if any) ractl6.bpl, dclractl6.bpl,
radb6.bpl, dclradb6.bpl, rai6.bpl, dclrai6.bpl, raia6.bpl, rafd6.bpl
from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Delphi IDE to open JVCL' runtime
package ractl6.dpk. In "Package..." window click "Compile" button to
compile packages ractl6.dpk. After compiling repeat that for other JVCL
Library run-time packages radb6.dpk, rai6.dpk, raia6.dpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory or $DELPHI\PROJECTS\BPL directory) or add ralib\lib folder
to Delphi search path (Tools | IDE Options). After compiling JVCL
run-time packages you must install JVCL design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl6.dpk, dclradb6.dpk and dclrai6.dpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

For instructions for installing JVCL Form Designer please read
ralib\doc\rafd.txt file.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

2. Delphi 5:

Uninstall previous installed version of JVCL-RA Library from Delphi 5 IDE.
Remove previously compiled JVCL packages (if any) ractrl50.bpl,
radb50.bpl, JvInterpreter_50.bpl, rafd50.bpl, ractl5.bpl, dclractl5.bpl,
radb5.bpl, dclradb5.bpl, rai5.bpl, dclrai5.bpl, raia5.bpl, rafd5.bpl
from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Delphi IDE to open JVCL' runtime
package ractl5.dpk. In "Package..." window click "Compile" button to
compile packages ractl5.dpk. After compiling repeat that for other JVCL
Library run-time packages radb5.dpk, rai5.dpk, raia5.dpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory or $DELPHI\PROJECTS\BPL directory). After compiling JVCL
run-time packages you must install JVCL design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl5.dpk, dclradb5.dpk and dclrai5.dpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

For instructions for installing JVCL Form Designer please read
ralib\doc\rafd.txt file.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

3. Delphi 4:

Uninstall previous installed version of JVCL-RA Library from Delphi 4 IDE.
Remove previously compiled JVCL packages (if any) ractrl40.bpl,
radb40.bpl, JvInterpreter_40.bpl, rafd40.bpl, ractl4.bpl, dclractl4.bpl,
radb4.bpl, dclradb4.bpl, rai4.bpl, dclrai4.bpl, raia4.bpl, rafd4.bpl
from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Delphi IDE to open JVCL' runtime
package ractl4.dpk. In "Package..." window click "Compile" button to
compile packages ractl4.dpk. After compiling repeat that for other JVCL
Library run-time packages radb4.dpk, rai4.dpk, raia4.dpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory). After compiling JVCL run-time packages you must install JVCL
design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl4.dpk, dclradb4.dpk and dclrai4.dpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

For instructions for installing JVCL Form Designer please read
ralib\doc\rafd.txt file.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

4. Delphi 3.x:

Uninstall previous installed version of JVCL-RA Library from Delphi 3 IDE.
Remove previously compiled JVCL packages (if any) ractrl30.dpl,
radb30.dpl, JvInterpreter_30.dpl, rafd30.dpl, ractl3.dpl, dclractl3.dpl,
radb3.dpl, dclradb3.dpl, rai3.dpl, dclrai3.dpl, raia3.dpl, rafd3.dpl
from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Delphi IDE to open JVCL' runtime
package ractl3.dpk. In "Package..." window click "Compile" button to
compile packages ractl3.dpk. After compiling repeat that for other JVCL
Library run-time packages radb3.dpk, rai3.dpk, raia3.dpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory). After compiling JVCL run-time packages you must install JVCL
design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl3.dpk, dclradb3.dpk and dclrai3.dpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

For instructions for installing JVCL Form Designer please read
ralib\doc\rafd.txt file.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

5. Delphi 2.x and C++ Builder 1:

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use the "Install..." item on Delphi's "Component" menu to add
the ractlreg.pas, radbreg.pas and raireg.pas units to the component
library. These units registers all JVCL-RA Library components on the
"JVCL" page.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

6. C++ Builder 4:

Uninstall previous installed version of JVCL-RA Library from C++ Builder 4 IDE.
Remove previously compiled JVCL packages (if any) ractl45.bpl,
dclractl45.bpl, radb45.bpl, dclradb45.bpl, rai45.bpl, dclrai45.bpl,
raia45.bpl from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Builder IDE to open JVCL' runtime
package ractl45.bpk. In "Package..." window click "Compile" button to
compile packages ractl45.bpk. After compiling repeat that for other JVCL
Library run-time packages radb45.bpk, rai45.bpk, raia45.bpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory or $BUILDER\Projects\Bpl). After compiling JVCL run-time
packages you must install JVCL design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl45.bpk, dclradb45.bpk and dclrai45.bpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

7. C++ Builder 5:

Uninstall previous installed version of JVCL-RA Library from C++ Builder 5 IDE.
Remove previously compiled JVCL packages (if any) ractl53.bpl,
dclractl53.bpl, radb53.bpl, dclradb53.bpl, rai53.bpl, dclrai53.bpl,
raia53.bpl from your hard disk.

Run file ralib\resource\russian\res.bat (russian resources) or
ralib\resource\english\res.bat (english resources).

Use "File\Open..." menu item of Builder IDE to open JVCL' runtime
package ractl53.bpk. In "Package..." window click "Compile" button to
compile packages ractl53.bpk. After compiling repeat that for other JVCL
Library run-time packages radb53.bpk, rai53.bpk, raia53.bpk. Put compiled
BPL files into directory that is accessible through the search PATH
(i.e. DOS "PATH" environment variable; for example, in the Windows\System
directory or $BUILDER\Projects\Bpl). After compiling JVCL run-time
packages you must install JVCL design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl53.bpk, dclradb53.bpk and dclrai53.bpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.

8. Kylix 1:

Uninstall previous installed version of JVCL-RA Library from Kylix 1 IDE.
Remove previously compiled JVCL packages (if any) bplractl6k.so,
bpldclractl6k.so, bplradb6k.so, bpldclradb6k.so, bplrai6k.so,
bpldclrai6k.so, bplraia6k.so from your hard disk.

Run file ralib\resource\russian\res.sh (russian resources) or
ralib\resource\english\res.sh (english resources).

Use "File\Open..." menu item of Kylix IDE to open JVCL' runtime
package ractl6k.dpk. In "Package..." window click "Compile" button to
compile packages ractl6k.dpk. After compiling repeat that for other JVCL
Library run-time packages rai6k.dpk, raia6k.dpk.

Put compiled .SO files into directory that is accessible through
the search LIBRARY PATH (i.e. LINUX "LD_LIBRARY_PATH" environment variable;
for example, in the /usr/lib directory or $KYLIX/bin directory)
or add ralib/lib folder to library search path.
One of this way is to add line to .bash_profile file in your home:
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/ralib/lib
(Note: you must relogin after changing .bash_profile file)
After compiling JVCL run-time packages you must install
JVCL design-time packages into the IDE.

Use "File\Open..." menu item to open consistently JVCL design-time
packages dclractl6k.dpkand dclrai6k.dpk.
In "Package..." window click "Install" button to register JVCL-RA Library
components on the "JVCL" page.

TIP: Move all PAS-files from ralib\lib folder into ralib\source.


Installation Problems
---------------------
1. many errors in one pas-file, while compiling ralib.
This problem can occur on Chinese version of Windows.
In RALib\Tools\RRR folder we have placed project RRR, which must
help to our non-english users. It removes all russian comments from
all ralib's pas-files. Compile this project and run it from RALib
home folder with '/r' parameter.


Demonstration Programs
----------------------
Demos can be found in ralib\demos folder.


Source Files
------------

All sources (100%) of JVCL-RA Library are available in ralib\lib or
ralib\source directories. All language specific string constants
used in JVCL-RA Library are collected in files placed in ralib\resource
folder. English and russian versions of this files are available
in distribution.


Credits
-------
Thanks to:
 - Antony Aloy Lypez for Spanish resources (1.0 version).
 - Tony L for idea of Color Hints.
 - Rafal Smotrzyk for many improvements in RAEditor and
   highlighers for RAHLEditor.
 - Dmitry Mokrushin for serious testing and improvements in JvInterpreter.
 - Jaromir Solar for Czech resources.
 - Peter Fischer-Haaser for testing and some coding in JvInterpreter.
 - Tony de Buys for RALib discussion group.
 - Knipper John for french resources.
 - Indranil Bandyopadhyay for bug fixes.
 - Jamie Frater for big testing of JvInterpreter.
 - Andrej Olejnik for initial array-support coding in JvInterpreter.
 - Nelson Luiz Dumbra for portugues (brazil) translation.
 - Michael Reichenbach, Suat IMAM-OGLU and Martin Schmid for
   german translations.
 - Didier Cabalé for impressive work of testing Form Designer and JvInterpreter.
 - Wowa Slavin for improvements in RAEditor.
 - Pavel Chromy for bugfixes in RAEditor.
 - Ivan Ravin for bugfixes and MDI support in JvInterpreter.
 - Constantin M. Lushnikov for bugfixes in RAEditor.
 - Luis David Cardenas Bucio for bugfixes in RAEditor.
 - Yakovlev Vacheslav for adopting RADBTreeView to non-bde datasets.
 - Michael Serpik for mouse wheel support in RAEditor.
 - Rients Politiek for adding support of proportional fonts in RAEditor.
 - Walter Campelo for bugfixes in RAEditor.
 - Andre Weber for improvements in RAFD.
 - Oussama Al-Rifai for adding right-to-left functionality to
   some components.
 - and all guys who wrote to us.


Copyright Notes
---------------

Most of the modules in our library are written by us. We have to make
a note of units based on sources of other authors.

Unit RADBLookupTreeView is based on original Delphi VCL units
DBLOOKUP, DBCTRLS.

Unit RAButtons has a big code portion from Delphi VCL unit Buttons.

Units RARegAuto and RARegAutoEditor are based on samples from
Sergey Orlik Book.

"Readme.txt" and "license.txt" files are based on RX Library's files :)


  Authors:
    Andrei Prygounkov   (a.prygounkov@gmx.de)
    Roman  Tkachev      (roman@infa.ru)

  JVCL-RA Library Home Page:
    http://ralib.hotbox.ru

  INFA-SOFT,
  Stavropol,
  Russia.

