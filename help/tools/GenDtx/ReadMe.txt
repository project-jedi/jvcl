GenDtx
======

Directories
-----------

First thing you need to do when you start GenDtx is specify the directories where GenDtx gets the
dtx, pas etc files from.

Select Options|Directories, then specify your directories. You can specify a root JVCL directory
and check the associated checkbox, then GenDtx will use that directory as base for the other directories.

Unit status
-----------

A dtx/pas file has a certain state:

* Completed, the dtx file documents the pas file completely and is up to date.
* Ignored, the pas file does not have to be documented.
* Generated, the pas file isn't documented at all, the dtx file is generated from the pas file.
* Other, some one is busy documenting the pas file, has locked the dtx file etc.

The state of a dtx/pas file is determined from:

* The ##status tag inside the dtx file.
* Whether the file is listed in 'Ignored units.txt'.

GenDtx determines the state of a file, depending on whether the file is listed in the following files:

* Completed units.txt
* Ignored units.txt
* Generated units.txt
* Other units.txt

If someone changes the text of a ##status tag inside a dtx file, the above listed state files needs to
be resynched. You can do this by clicking View|Unit status. You will now see a screen with all dtx files
and their text from the ##status tag and how GenDtx interprets that text. By clicking 'Update state files'
the state files will be updated.

Main screen
-----------

On the main screen you will see 2 list boxes, one named 'available units', the other 'Process Units'. The
units you see in these list boxes are from the run-time pas directory you specified in the Directories dialog.

All operations are performed only on the files listed in the 'Process Units' list box, thus the files in the
'available units' list box are ignored.

You can filter on the state of the files that are listed in these list boxes. Click on 'Show completed' to
show the files that are marked as completed, etc.

You can select files in the 'Process Units' list box and change their state by clicking 'Add to completed list'
or 'Add to ignore list'. (The state files will be updated, not the ##status tag in a dtx file).

Below on the screen is the message list box. Here GenDtx displays messages; you can clear the list box or save
the messages by clicking 'Clear' or 'Save' resp.

Check Commands
--------------

1. Check dtx files
   ---------------

   This procedure will check all dtx files listed in the 'Process Units' list box that are located in
   the directory dtx files (typically ($JVCL)\dev\help).

   It parses the pas files from the run-time pas directory that have the same file name as the listed dtx
   files, and checks whether the dtx file

   * contains default texts, such as 'Write here a summary (1 line)'.
   * contains all items that should be described (according to the parsed pas file).
   * contains certain errors, such as empty 'See Also' sections.

2. Check pas files
   ---------------

   This procedure will check all pas files listed in the 'Process Units' list box that are located in
   the run-time pas directory (typically ($JVCL)\dev\JVCL3\run).

   It currently checks per pas files, whether

   * it has a license.
   * there is a difference in the casing of the unit file name and the unit name in the unit heading.

3. Check casing in pas files
   -------------------------

   This procedure will generate a list of all defined methods, properties types etc. in the pas files
   in the 'Process Units' list box that are located in the run-time pas directory (typically
   ($JVCL)\dev\JVCL3\run).

   It then will check whether there are differences in casing in that generated list.

3. Check casing in pas files (All)
   -------------------------------

   This procedure will generate a list of *all* symbols in the pas files in the 'Process Units' list box
   that are located in the run-time pas directory (typically ($JVCL)\dev\JVCL3\run).

   It then will check whether there are differences in casing in that generated list.

Generate Commands
-----------------

1. Generate dtx files
   ------------------

   This procedure will generate dtx files from all pas files listed in the 'Process Units' list box
   that are located in the run-time pas directory (typically ($JVCL)\dev\JVCL3\run).

   The generated dtx files will be put in the generated dtx directory (typically ($JVCL)\dev\help\Gen).
   You can specify via Options|Directories whether to overwrite existing dtx files.

2. Generate package list
   ---------------------

   This procedure will search for all run-time packages in the package dir (typically
   ($JVCL)\dev\JVCL3\packages\d7)

   It parses those packages and determines which pas files are listed in their 'contains' section.
   From this information it constructs the 'Files in packages.txt' file, that contains

   unitname=packagename

   lines, with 'unitname' a specific unit that is used in package 'packagename'.

3. Generate registered components
   ------------------------------

   This procedure will search for pas files in the design-time pas directory (typically ($JVCL)\dev\JVCL3\design).

   It checks whether these pas files register components. The complete list of registered components
   is saved in the file 'RegisteredClasses.txt'.

   (This file is used while generating dtx files to determine which classes should have a JVCLInfo section).

4. Clear generated dtx dir
   -----------------------

   Clears the generated dtx directory (typically ($JVCL)\dev\help\Gen).

