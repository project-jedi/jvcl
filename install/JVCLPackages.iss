# include "DelphiUtils.iss"

function InstallJVCLPackages(AppDir: string): string;
var
  PackSrcFolder, BPLFolder, DCPFolder, PackFileName,
  AComponent, SearchPath, CommandLine: string;
begin
  { Tasks to perform:
    * For each selected package version, build the JCL packages and install the JCL help file if the user selected the JCL files
    * For each selected package version, build the selected R and D packages
    * For each selected package version, install the selected D packages
    * For each selected package version, install the help file if the user selected the help file
  }
    if ShouldProcessEntry('d5\dcc', '') = srYes then
    begin
      BPLFolder := DelphiBPLFolder('5.0');
      DCPFolder := DelphiDCPFolder('5.0');
      PackSrcFolder := AddBackSlash(ExpandConstant('{app}')) + 'packages\D5';
      // build packages
      PackFileName := FindFirst(PackSrcFolder + '\*.dpk');
      while PackFileName <> '' do
      begin
        SearchPath := ExtractDOFDirectories(AddBackSlash(PackSrcFolder) + ChangeFileExt(PackFileName,'.dof'),false);
        CommandLine := PackFileName + ' -N"'+ ExtractDOFDirectories(AddBackSlash(PackSrcFolder) + ChangeFileExt(PackFileName,'.dof'),true) + '"' +
          ' -I"' +  SearchPath + '"' +
          ' -R"' + SearchPath + '"' +
          ' -LE"' + BPLFolder + '"' +
          ' -LN"' + DCPFolder + '"' +
          ' -U"' + DCPFolder + ';' + SearchPath;
        // call compiler
        DCCCompile('5.0',CommandLine,PackSrcFolder);
        PackFileName := FindNext;
      end;
      // install selected,compiled design packages
      PackFileName := FindFirst(BPLFolder + '\*D5D.bpl');
      while PackFileName <> '' do
      begin
        AComponent := ExtractFileName(Copy(PackFileName,1,Length(PackFileName) - 7));
        if ShouldProcessEntry('d5\dcc\' + AComponent, '') = srYes then
//        InstallDelphiPackage('5.0',PackFileName,'');
        PackFileName := FindNext;
      end;
    end;

    if ShouldProcessEntry('d6\dcc', '') = srYes then
    begin
    end;
    if ShouldProcessEntry('d7\dcc', '') = srYes then
    begin
    end;
end;

