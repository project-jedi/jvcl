unit HelpBuildHelper;

interface

uses
  HelpBuild;

type
  THelpBuildPageClass = class of THelpBuildPage;

function GetNextPage(HelpBuilder: THelpBuilder; Current: TClass): THelpBuildPageClass;

implementation

uses
  ContNrs,

  HelpBuildData,
  PgConfig, PgBuildOptions, PgPostBuildOptions, PgUnzipOptions,
  PgZipOptions, PgDiagnoseOptions, PgSummary, PgBuild,
  PgDoxCompileOptions;

function GetNextPage(HelpBuilder: THelpBuilder; Current: TClass): THelpBuildPageClass;
var
  Index: Integer;
  List: TClassList;
begin
  List := TClassList.Create;
  try
    List.Add(TConfigPage);

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateHelpDoxFile] or
       HelpBuilder.Data.Task[HelpBuildTask_GeneratePackagesDoxFile] or
       HelpBuilder.Data.Task[HelpBuildTask_GenerateReferenceDoxFile] then
      List.Add(TBuildOptionsPage);

    if HelpBuilder.Data.Task[HelpBuildTask_BuildHelpDoxFile_HTML] or
       HelpBuilder.Data.Task[HelpBuildTask_BuildHelpDoxFile_WinHelp] or
       HelpBuilder.Data.Task[HelpBuildTask_BuildPackagesDoxFile_PDF] or
       HelpBuilder.Data.Task[HelpBuildTask_BuildReferenceDoxFile_PDF] then
    begin
      List.Add(TDoxCompileOptionsPage);
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_CompileWinHelp] or
       HelpBuilder.Data.Task[HelpBuildTask_CompileHtmlHelp] then
    begin
      List.Add(TPostBuildOptionsPage);
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_UnzipOnlineHelpZipFile] then
      List.Add(TUnzipOptionsPage);

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateOnlineHelpZipFiles] or
       HelpBuilder.Data.Task[HelpBuildTask_ZipWinHelp] or
       HelpBuilder.Data.Task[HelpBuildTask_ZipHtmlHelp] or
       HelpBuilder.Data.Task[HelpBuildTask_ZipPackagesPDF] or
       HelpBuilder.Data.Task[HelpBuildTask_ZipReferencePDF] then
    begin
      List.Add(TZipOptionsPage);
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile] or
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxIdentifiersFile] or
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxInvalidWordsFile] or
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxWordsFile] then
    begin
      List.Add(TDiagnoseOptionsPage);
    end;

    List.Add(TSummaryPage);
    List.Add(TBuildPage);

    Index := List.IndexOf(Current);
    if (Index >= 0) and (Index < List.Count - 1) then
      Result := THelpBuildPageClass(List[Index + 1])
    else
      Result := nil;
  finally
    List.Free;
  end;
end;

end.

