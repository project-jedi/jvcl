To create the release zips for JVCL, proceed as follows:
* Create a new folder on your computer, call it "Release" (the name doesn't really matter)
* Copy all the files from this folder to this new folder (your "root" folder)
* Run pg2want.bat from devtools\bin (build it first). This creates a new file, sep.xml.
* Insert the result from sep.xml into want.xml in the Release folder where it says "  <!-- !!!insert "separate" target here!!! -->"
* In the Release folder, do an EXPORT (not checkout!) from CVS of dev/JVCL3. Use your own name for the subfolder: jvcl. You can also use the jvcl_export target in want.xml(see NOTES)
* In the Release folder, do an EXPORT (not checkout!) from CVS of jcl. Use the default jcl name for the subfolder. You can also use the jcl_export target in want.xml(see NOTES)
* You should now have a folder structure that looks like this:
  \Release
  ---\jcl
  ---\jvcl
* Copy the compiled help file(s) to jvcl\help
* In Release, modify the version.* values in want.xml to reflect the current JVCL version
* In Release, double-click on want.exe to build all the zip files (default target is "all")
* Open a command prompt in Release and type "want dist" to build the JVCL zip files only
* Open a command prompt in Release and type "want standalone" to build the standalone zip files only
* Upload the zip files to sourceforge and create a release


NOTES:
* To use the cvs targets (jvcl_export and jcl_export), make sure cvs.exe is in the system PATH and that the pserver protocol is installed
* You cannot use the default want.exe for this build. You have to use the included one since it contains modifications that are not part of the standard want.exe 
