@echo off
rem This batch file searches your dfm files for any use of JVCL components (classes starting with TJv) and 
rem puts the results into a text file. Initial idea provided by Heinz Zastrau. 
rem Uses Borland's grep utility (included with Delphi and CBuilder) 
echo. Searching, please wait...
grep -di "object [a-z0-9_]*: TJv" *.dfm >UsedJVCL.txt
echo. Done. Results are in UsedJVCL.txt
