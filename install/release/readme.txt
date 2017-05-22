To create the release zips for JVCL, proceed as follows:
* Create a preparation branch.
* Create a new folder on your computer, call it "Release" (the name doesn't really matter).
* Copy all the files from this folder to this new folder (your "root" folder).
* Run pg2want.bat from devtools\bin. This creates a new file, separate.xml in \devtools\bin.
* Move separate.xml into the "Release" folder.
* In Release, do an EXPORT (not checkout!) from SVN of /trunk/jvcl. Use the default jvcl name for the subfolder. You can also use the jvcl_export target in want.xml (see NOTES).
* In Release, do an EXPORT (not checkout!) from SVN of jcl. Use the default jcl name for the subfolder. You can also use the jcl_export target in want.xml (see NOTES).
* You should now have a folder structure that looks like this:
  \Release
  ---\jcl
  ---\jvcl
* Copy the compiled help file(s) to jvcl\help.
* In Release, modify the version.* values in want.xml to reflect the current JVCL version.
* In Release, modify the Version values in jvcl\run\JVCLVer.pas to reflect the current JVCL version.
* In Release, modify the announement texts in jvcl\install\release to reflect the current JVCL version.
* Open a command prompt in Release and type "want dist" to build the JVCL zip files only.
* Open a command prompt in Release and type "want standalone" to build the standalone zip files only.
* Open a command prompt in Release and type "want separate" to build the single package zip files only (this calls separate.xml to do the work).
* Open a command prompt in Release and type "want all" to build all zip files.
* Upload the zip files to sourceforge and create a release (or run the "want ftpupload" target).
* Commit the files modified above, in the preparation branch and HEAD
* Tag the preparation branch.


NOTES:
* To use the svn targets (jvcl_export and jcl_export), make sure svn.exe is in the system PATH
* You cannot use the default want.exe for this build. You have to use the included one since it contains modifications that are not part of the standard want.exe 
