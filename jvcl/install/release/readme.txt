To create the release zips for JVCL, proceed as follows:
* Create a release branch and switch to it
* Modify the announement texts in jvcl\install\release to reflect the current JVCL version.
1. Modify the version.* values in jvcl\install\release\want.xml to reflect the current JVCL version.
2. Modify the Version values in jvcl\run\JVCLVer.pas to reflect the current JVCL version.
3. Modify the ProjectProperties values in jvcl\devtools\pgedit.xml to reflect the current JVCL version.
4. Run PgEdit to regenerate the packages with the new version
5. Open a command line in jvcl\packages\ and run this command: for %d in (*) do make_res_files.bat %d
* Commit / Push
* Create and push a release tag 
* Run pg2want.bat from .\jvcl\devtools\bin. This creates a new file, separate.xml in .\jvcl\devtools\bin.
* Create a new folder on your computer, call it "Release" (the name doesn't really matter, as long as the full path doesn't contain any space).
* Copy all the files from this folder to this new folder (your "root" folder).
* Move separate.xml into the "Release" folder.
* Open a command line into the .\jvcl folder
* Run the following command: git checkout-index -a -f --prefix=c:\full\path\to\release\folder\
*** Important: do not miss the trailing the backslash
* In Release, unzip the JCL complete zip file from the associated release so that the final folder is named jcl
* You should now have a folder structure that looks like this:
  \Release
  ---\jcl
  ---\jvcl
* (not compulsory) Copy the compiled help file(s) to jvcl\help.
* Open a command prompt in Release and type "want dist" to build the JVCL zip files only.
* (not compulsory) Open a command prompt in Release and type "want standalone" to build the standalone zip files only.
* (not compulsory) Open a command prompt in Release and type "want separate" to build the single package zip files only (this calls separate.xml to do the work).
* (not compulsory) Open a command prompt in Release and type "want all" to build all zip files.
* Upload the zip files to sourceforge and create a release (or run the "want ftpupload" target).
* Checkout the master branch
* Repeat steps 1 to 5 with the next version numbers
* Commit / Push


NOTES:
* You cannot use the default want.exe for this build. You have to use the included one since it contains modifications that are not part of the standard want.exe 
