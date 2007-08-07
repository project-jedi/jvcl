unit FrmBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  ShellAPI,
  HelpBuild, HelpBuildData, JVCLHelpUtils, DtxDiagnoser;

type
  TFrameBuild = class(TFrame, IProgress, ITaskManager)
    LblTarget: TLabel;
    ProgressBarTarget: TProgressBar;
    ProgressBarCompile: TProgressBar;
    LblInfo: TLabel;
    RichEditLog: TRichEdit;
    LblOpenFile: TLabel;
    BtnDetails: TButton;
    procedure BtnDetailsClick(Sender: TObject);
    procedure LblOpenFileClick(Sender: TObject);
  private
    FInitializing: Boolean;
    FHelpBuilder: THelpBuilder;
    FProgress: TProgress;
  private
    FPositionTarget: Integer;
    FPositionProject: Integer;
    FFinished: Boolean;
    FAborted: Boolean;
    FErrorHeader: string;
    procedure EvIdle(Sender: TObject);
    procedure Init;
  protected
    function DoCleanDirectories: Boolean;
    function DoGenerateOnlineHelpZipFiles: Boolean;
    function DoUnzipOnlineHelpZipFile: BOolean;
    function DoGenerateRegisteredClassesFile: Boolean;
    function DoGenerateGroupsFile: Boolean;
    function DoGeneratePackageList: Boolean;
    function DoGenerateDonatorsDtxFile: Boolean;
    function DoGenerateStrippedPasFiles: Boolean;
    function DoProcessDtxFiles(DtxDiagnoser: TDtxDiagnoser): Boolean;
    function DoPreProcessOnlineDtxFiles: Boolean;
    function DoGenerateImages: Boolean;
    function DoBuildDoxFile(const ADoxFileName: string; const AConfiguration: string): Boolean;
    function DoPostProcessWinHelpOutput: Boolean;
    function DoPostProcessHTMLHelpOutput: Boolean;
    function DoBuildHTMLHelp: Boolean;
    function DoCompileWinHelp: Boolean;
    function DoDiagnoseDtx: Boolean;
    function DoZipWinHelp: Boolean;
    function DoZipHtmlHelp: Boolean;
    function DoZipPackagesPdf: Boolean;
    function DoZipReferencePdf: Boolean;

    function DoGenerateDtxWordsFile(ADtxDiagnoser: TDtxDiagnoser): Boolean;
    function DoGenerateDtxInvalidWordsFile(ADtxDiagnoser: TDtxDiagnoser):
      Boolean;
    function DoGenerateDtxIdentifiersFile(ADtxDiagnoser: TDtxDiagnoser):
      Boolean;
    function DoGenerateDtxDuplicateIdentifiersFile(ADtxDiagnoser:
      TDtxDiagnoser): Boolean;

    procedure AddLine(const Line: string; const Styles: TFontStyles; Color:
      TColor = clNone);
    procedure TargetMsg(const Msg: string);

    procedure InitTasksTodo(const Arr: array of Boolean);

    procedure ShowErrorHeader;

    { IProgress }
    procedure StatusMsg(const Msg: string);
    procedure ErrorMsg(const Msg: string);
    procedure WarningMsg(const Msg: string);
    procedure HintMsg(const Msg: string);
    procedure ErrorHeader(const Msg: string);

    { ITaskManager }
    procedure TaskStatus(const S: string);
    procedure TaskProgress(const Position, Max: Integer);
    procedure TaskDone;

    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Build(HelpBuilder: THelpBuilder; Client: TWinControl):
      TFrameBuild;
    function Execute: Boolean;

    property Aborted: Boolean read FAborted write FAborted;
  end;

implementation

uses
  JvSearchFiles, RegisteredClassesProducer, PackageListProducer,
  DtxPasCombiner, GroupDtxProducer, PackageDtxProducer, PasFilesStripper,
  DtxAnalyzer,
  DoxProducer, ImagesProducer, DirCleaner, WinHelpPostProcessor,
  HtmlHelpPostProcessor,
  InvalidWordsProducer, DuplicateIdentifiersProducer, DoxFileBuilder,
  DtxFilesPreProcessor,
  ZipExec, UnzipExec,
  HtmlHelpCompiler, WinHelpCompiler;

{$R *.dfm}

const
  cConfiguration_HTML = 'Configuration 1';
  cConfiguration_HTMLHelp = 'Configuration 2';
  cConfiguration_Help2 = 'Configuration 3';
  cConfiguration_PDF = 'Configuration 4';
  cConfiguration_WinHelp = 'Configuration 5';
  cConfiguration_XML = 'Configuration 6';

  //=== { TFrameBuild } ========================================================

constructor TFrameBuild.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  JVCLHelpUtils.GProgress := Self;
  FProgress := TProgress.Create;
end;

destructor TFrameBuild.Destroy;
begin
  FProgress.Free;
  JVCLHelpUtils.GProgress := nil;
  inherited Destroy;
end;

procedure TFrameBuild.AddLine(const Line: string;
  const Styles: TFontStyles; Color: TColor);
begin
  RichEditLog.Lines.Add(Line);
  RichEditLog.SelStart := RichEditLog.SelStart - Length(Line) - 2;
  RichEditLog.SelLength := Length(Line);
  if Color <> clNone then
    RichEditLog.SelAttributes.Color := Color;
  RichEditLog.SelAttributes.Style := Styles;
  RichEditLog.SelLength := 0;
end;

procedure TFrameBuild.BtnDetailsClick(Sender: TObject);
begin
  BtnDetails.Visible := False;
  RichEditLog.Visible := True;
  RichEditLog.SetFocus;
end;

class function TFrameBuild.Build(HelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameBuild;
begin
  Result := TFrameBuild.Create(Client);
  //  HelpBuilder.PackageHelpBuilder.Translate(Result);
  Result.FHelpBuilder := HelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

function TFrameBuild.DoBuildDoxFile(const ADoxFileName: string; const AConfiguration: string): Boolean;
begin
  with TDoxFileBuilder.Create(Self) do
  try
    DMCCFileName := HelpBuilder.Data.DMCCFileName;
    if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildPackageHelp) then
      DoxFileName := HelpBuilder.Data.PackagesDoxFileName
    else if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildReferenceHelp) then
      DoxFileName := HelpBuilder.Data.ReferenceDoxFileName
    else
      DoxFileName := HelpBuilder.Data.DoxFileName;
    Configuration := AConfiguration;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoBuildHTMLHelp: Boolean;
begin
  with THtmlHelpCompiler.Create(Self) do
  try
    HtmlHelpDir := HelpBuilder.Data.HtmlHelpOutputDir;
    DoxFileName := HelpBuilder.Data.DoxFileName;
    HHCFileName := HelpBuilder.Data.HHCFileName;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoCompileWinHelp: Boolean;
begin
  with TWinHelpCompiler.Create(Self) do
  try
    WinHelpDir := HelpBuilder.Data.WinHelpOutputDir;
    DoxFileName := HelpBuilder.Data.DoxFileName;
    HCRTFFileName := HelpBuilder.Data.HCRTFFileName;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoCleanDirectories: Boolean;
begin
  with TDirCleaner.Create(Self) do
  try
    if HelpBuilder.Data.Task[HelpBuildTask_GenerateOnlineHelpZipFiles] then
      AddDir(HelpBUilder.Data.GenZipDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_ZipWinHelp] then
      AddDir(HelpBUilder.Data.GenZipDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_ZipHtmlHelp] then
      AddDir(HelpBUilder.Data.GenZipDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_ZipPackagesPDF] then
      AddDir(HelpBUilder.Data.GenZipDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_ZipReferencePDF] then
      AddDir(HelpBUilder.Data.GenZipDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_UnzipOnlineHelpZipFile] then
      AddDir(HelpBuilder.Data.OnlineHelpDtxDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_GenerateImages] then
      AddDir(HelpBuilder.Data.HelpImagesCompDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_GenerateStrippedPasFiles] then
      AddDir(HelpBuilder.Data.GenPasDir, False);
    if HelpBuilder.Data.Task[HelpBuildTask_BuildHelpDoxFile_HTML] then
      AddDir(HelpBuilder.Data.HtmlHelpOutputDir, True);
    if HelpBuilder.Data.Task[HelpBuildTask_BuildHelpDoxFile_WinHelp] then
      AddDir(HelpBuilder.Data.WinHelpOutputDir, True);
    if HelpBuilder.Data.Task[HelpBuildTask_BuildReferenceDoxFile_PDF] then
      AddDir(HelpBuilder.Data.PDFHelpOutputDir, True);
    if HelpBuilder.Data.Task[HelpBuildTask_BuildPackagesDoxFile_PDF] then
      AddDir(HelpBuilder.Data.PDFHelpOutputDir, True);
    if HelpBuilder.Data.Task[HelpBuildTask_GenerateFormattedDtxFiles] then
      AddDir(HelpBuilder.Data.GenDtxDir, False);

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGenerateDonatorsDtxFile: Boolean;
begin
  TargetMsg('Generating donators dtx..');
  TaskDone;
  Result := True;
end;

function TFrameBuild.DoGenerateGroupsFile: Boolean;
begin
  TargetMsg('Generating groups file..');
  TaskDone;
  Result := True;
end;

function TFrameBuild.DoGenerateImages: Boolean;
begin
  with TImagesProducer.Create(Self) do
  try
    SourceDir := HelpBuilder.Data.JVCL3ImagesDir;
    DestDir := HelpBuilder.Data.HelpImagesCompDir;
    RegisteredClassesFileName := HelpBuilder.Data.RegisteredClassesFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGeneratePackageList: Boolean;
begin
  with TPackageListProducer.Create(Self) do
  try
    PackageDir := HelpBuilder.Data.PackageDir;
    PackageListFileName := HelpBuilder.Data.PackageListFileName;
    RuntimePasDir := HelpBuilder.Data.JVCLxxRunDir;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGenerateRegisteredClassesFile: Boolean;
begin
  with TRegisteredClassesProducer.Create(Self) do
  try
    RegisteredClassesFileName := HelpBuilder.Data.RegisteredClassesFileName;
    JVCLxxDesignDir := HelpBuilder.Data.JVCLxxDesignDir;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGenerateStrippedPasFiles: Boolean;
var
  IsJCL: Boolean;
begin
  IsJCL := IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_CheckDtxJCL);

  with TPasFilesStripper.Create(Self) do
  try
    if IsJCL then
      SourcePasDir := HelpBuilder.Data.JVCLxxDir
    else
      SourcePasDir := HelpBuilder.Data.JVCLxxRunDir;

    DestPasDir := HelpBuilder.Data.GenPasDir;
    IncludeSourceSubDirs := IsJCL;

    if HelpBuilder.Data.DtxSelectionKind = dskSpecific then
      SpecificFiles := HelpBuilder.Data.SpecificDtxFiles;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoPostProcessHTMLHelpOutput: Boolean;
begin
  with THtmlHelpPostProcessor.Create(Self) do
  try
    HtmlHelpDir := HelpBuilder.Data.HtmlHelpOutputDir;
    DoxFileName := HelpBuilder.Data.DoxFileName;

    Result := Execute;
    if not REsult then
      Exit;
  finally
    Free;
  end;
end;

function TFrameBuild.DoPostProcessWinHelpOutput: Boolean;
begin
  with TWinHelpPostProcessor.Create(Self) do
  try
    WinHelpDir := HelpBuilder.Data.WinHelpOutputDir;
    DoxFileName := HelpBuilder.Data.DoxFileName;

    Result := Execute;
    if not Result then
      Exit;
  finally
    Free;
  end;
end;

function TFrameBuild.DoProcessDtxFiles(DtxDiagnoser: TDtxDiagnoser): Boolean;
var
  GroupDtxProducer: TGroupDtxProducer;
  PackageDtxProducer: TPackageDtxProducer;
  DoxProducer: TDoxProducer;
  DtxAnalyzer: TDtxAnalyzer;
  RegisteredComponents: TRegisteredComponents;
  IsJCL: Boolean;
  GroupsTreeParser: TGroupsTreeParser;
begin
  IsJCL := IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_CheckDtxJCL);

  RegisteredComponents := TRegisteredComponents.Create(
    HelpBuilder.Data.RegisteredClassesFileName,
    HelpBuilder.Data.GroupListFileName);
  try
    DtxAnalyzer := TDtxAnalyzer.Create(Self);
    try
//      DtxAnalyzer.JVCLGroupsTreeFileName := HelpBuilder.Data.GroupsTreeFileName;
      if not IsJCL then
        DtxAnalyzer.PackageListFileName := HelpBuilder.Data.PackageListFileName;
      DtxAnalyzer.Diagnoser := DtxDiagnoser;
      if HelpBuilder.Data.DtxSelectionKind = dskSpecific then
      begin
        DtxAnalyzer.SpecificDtxFiles := HelpBuilder.Data.SpecificDtxFiles;
      end;

      if IsEqualGUID(HelpBuilder.Data.HelpBuildType,
        HelpBuildType_SyncOnlineHelp) then
      begin
        DtxAnalyzer.SourceDtxDir := HelpBuilder.Data.OnlineHelpDtxDir;
        DtxAnalyzer.OriginalDtxDir := HelpBuilder.Data.HelpDir;
        DtxAnalyzer.CopyHeaderFromOriginal := True;
        DtxAnalyzer.UpdateOriginalEmpties := True;
        DtxAnalyzer.CopySeeAlsoFromOriginal := True;
      end
      else
      begin
        DtxAnalyzer.SourceDtxDir := HelpBuilder.Data.HelpDir;
        DtxAnalyzer.CopyHeaderFromOriginal := False;
        DtxAnalyzer.UpdateOriginalEmpties := False;
        DtxAnalyzer.CopySeeAlsoFromOriginal := False;
      end;
      DtxAnalyzer.SourcePasDir := HelpBuilder.Data.GenPasDir;
      DtxAnalyzer.DestDtxDir := HelpBuilder.Data.GenDtxDir;
      if not IsJCL then
      begin
        // registered components
        Result := RegisteredComponents.Parse;
        if not Result then
          Exit;

        // groups tree
        GroupsTreeParser := TGroupsTreeParser.Create;
        try
          GroupsTreeParser.GroupInfos := DtxAnalyzer.GroupInfos;
          Result := GroupsTreeParser.Parse(HelpBuilder.Data.GroupsTreeFileName);
        finally
          GroupsTreeParser.Free;
        end;
      end;
      DtxAnalyzer.RegisteredComponents := RegisteredComponents;
      DtxAnalyzer.GenerateFormattedDtxFiles :=
        HelpBuilder.Data.Task[HelpBuildTask_GenerateFormattedDtxFiles];
      DtxAnalyzer.GenerateForBuild :=
        IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildTYpe_BuildHelp) or
        IsEqualGUID(HelpBuilder.Data.HelpBuildType,
        HelpBuildType_BuildReferenceHelp) or
        IsEqualGUID(HelpBuilder.Data.HelpBuildType,
        HelpBuildType_BuildPackageHelp);

      Result := DtxAnalyzer.Execute;
      if not Result then
        Exit;

      if HelpBuilder.Data.Task[HelpBuildTask_GenerateHelpDoxFile] or
         HelpBuilder.Data.Task[HelpBuildTask_GeneratePackagesDoxFile] or
         HelpBuilder.Data.Task[HelpBuildTask_GenerateReferenceDoxFile] then
      begin
        DoxProducer := TDoxProducer.Create(Self);
        try
          DoxProducer.PDFHelpOutputDir := HelpBuilder.Data.PDFHelpOutputDir;
          DoxProducer.PasDir := HelpBuilder.Data.GenPasDir;
          DoxProducer.DtxDir := HelpBuilder.Data.GenDtxDir;
          DoxProducer.InfoDir := HelpBuilder.Data.HelpInfoDir;
          DoxProducer.AddInfoDtxFiles := True;
          DoxProducer.TemplateDoxFileName :=
            HelpBuilder.Data.TemplateDoxFileName;
          DoxProducer.DestDoxFileName := HelpBuilder.Data.DoxFileName;
          // DoxTitle: JEDI-VCL 3.30 help system
          DoxProducer.DoxTitle := HelpBuilder.Data.DoxTitle;
          DoxProducer.VersionMajor := HelpBuilder.Data.JVCLMajorVersion;
          DoxProducer.VersionMinor := HelpBuilder.Data.JVCLMinorVersion;
          DoxProducer.MinPercComplete := HelpBuilder.Data.MinPercComplete;
          DoxProducer.MaxFilesInDox := HelpBuilder.Data.MaxFilesInDox;
          DoxProducer.DtxSelectionKind := HelpBuilder.Data.DtxSelectionKind;
          DoxProducer.SpecificDtxFiles := HelpBuilder.Data.SpecificDtxFiles;
          DoxProducer.DtxInfos := DtxAnalyzer.DtxInfos;
          if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildReferenceHelp) or
            IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildPackageHelp) then
          begin
            // no files
            DoxProducer.DtxSelectionKind := dskMax;
            DoxProducer.MaxFilesInDox := 0;
            DoxProducer.AddInfoDtxFiles := False;
          end;
          if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildReferenceHelp) then
          begin
            DoxProducer.RemovePackages := True;
            DoxProducer.DestDoxFileName :=
              HelpBuilder.Data.ReferenceDoxFileName;
          end;
          if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildPackageHelp) then
          begin
            DoxProducer.RemoveFuncRef := True;
            DoxProducer.DestDoxFileName := HelpBuilder.Data.PackagesDoxFileName;
          end;

          Result := DoxProducer.Execute;
          if not Result then
            Exit;

        finally
          DoxProducer.Free;
        end;
      end;

      if HelpBuilder.Data.Task[HelpBuildTask_GeneratePackagesDtxFile] then
      begin
        PackageDtxProducer := TPackageDtxProducer.Create(Self);
        try
          PackageDtxProducer.PackagesDtxFileName :=
            HelpBuilder.Data.PackagesDtxFileName;
          PackageDtxProducer.PackageDescFileName :=
            HelpBuilder.Data.PackageDescFileName;
          PackageDtxProducer.PackageInfos := DtxAnalyzer.Packages;
          //          PackageDtxProducer.ComponentInfos := DtxAnalyzer.ComponentInfos;

          PackageDtxProducer.Execute;
        finally
          PackageDtxProducer.Free;
        end;
      end;

      if HelpBuilder.Data.Task[HelpBuildTask_GenerateFuncRefDtxFile] then
      begin
        GroupDtxProducer := TGroupDtxProducer.Create(Self);
        try
          GroupDtxProducer.GroupsDtxFileName :=
            HelpBuilder.Data.FuncRefDtxFileName;
          GroupDtxProducer.GroupInfos := DtxAnalyzer.GroupInfos;
          GroupDtxProducer.ComponentInfos := DtxAnalyzer.ComponentInfos;
          GroupDtxProducer.NoLinks :=
            IsEqualGUID(HelpBuilder.Data.HelpBuildType,
            HelpBuildType_BuildReferenceHelp) or
            IsEqualGUID(HelpBuilder.Data.HelpBuildType,
            HelpBuildType_BuildPackageHelp);

          GroupDtxProducer.Execute;
        finally
          GroupDtxProducer.Free;
        end;
      end;
    finally
      DtxAnalyzer.Free;
    end;
  finally
    RegisteredComponents.Free;
  end;
end;

procedure TFrameBuild.ErrorMsg(const Msg: string);
begin
  ShowErrorHeader;
  AddLine(Msg, [fsBold], clRed);
end;

procedure TFrameBuild.EvIdle(Sender: TObject);
begin
  Application.ProcessMessages;
end;

var
  GStartTime: TDateTime;

procedure StartTimer;
begin
  GStartTime := Now;
end;

function TimerStr: string;
var
  Delta: TDateTime;
  H, M, S, MSec: Word;
begin
  Delta := Now - GStartTime;
  DecodeTime(Delta, H, M, S, MSec);
  if H > 0 then
    Result := Format('%dh %.2dm', [H, M])
  else if M > 0 then
    Result := Format('%dm %.2ds', [M, S])
  else
    Result := Format('%d.%.3d sec.', [S, MSec]);
end;

function TFrameBuild.Execute: Boolean;
begin
  Aborted := False;

  HelpBuilder.Data.Save;
  FPositionTarget := 0;
  FPositionProject := 0;

  FFinished := False;
  StartTimer;

  with HelpBuilder.Data do
  begin
    InitTasksTodo([
      MustCreateGenDtxDir, MustCreateGenPasDir, MustCreateGenZipDir,
        True {always clean dirs},
      Task[HelpBuildTask_PreProcessOnlineDtxFiles],
        Task[HelpBuildTask_UnzipOnlineHelpZipFile],
        Task[HelpBuildTask_GenerateOnlineHelpZipFiles],
        Task[HelpBuildTask_GenerateImages],
        Task[HelpBuildTask_GenerateRegisteredClassesFile],
        Task[HelpBuildTask_GenerateGroupsFile],
        Task[HelpBuildTask_GenerateStrippedPasFiles],
        Task[HelpBuildTask_GeneratePackageList],
        Task[HelpBuildTask_GenerateDonatorsDtxFile],
        Task[HelpBuildTask_GenerateFormattedDtxFiles],
        Task[HelpBuildTask_GenerateFuncRefDtxFile],
        Task[HelpBuildTask_GeneratePackagesDtxFile],
        Task[HelpBuildTask_GenerateHelpDoxFile],
        Task[HelpBuildTask_BuildHelpDoxFile_HTML],
        Task[HelpBuildTask_BuildHelpDoxFile_WinHelp],
        Task[HelpBuildTask_PostProcessWinHelpOutput],
        Task[HelpBuildTask_PostProcessHtmlHelpOutput],
        Task[HelpBuildTask_CompileWinHelp],
        Task[HelpBuildTask_CompileHtmlHelp],
        Task[HelpBuildTask_ZipWinHelp],
        Task[HelpBuildTask_ZipHtmlHelp],
        // processing dtx files
      Task[HelpBuildTask_GenerateDtxWordsFile] or
        Task[HelpBuildTask_GenerateDtxInvalidWordsFile] or
        Task[HelpBuildTask_GenerateDtxIdentifiersFile] or
        Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile],
        Task[HelpBuildTask_GenerateDtxWordsFile],
        Task[HelpBuildTask_GenerateDtxInvalidWordsFile],
        Task[HelpBuildTask_GenerateDtxIdentifiersFile],
        Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile]
        ]);

    if MustCreateGenDtxDir then
    begin
      Result := ForceDirectories(GenDtxDir);
      if not Result then
        RaiseLastOSError;
      TaskDone;
    end;

    if MustCreateGenPasDir then
    begin
      Result := ForceDirectories(GenPasDir);
      if not Result then
        RaiseLastOSError;
      TaskDone;
    end;

    if MustCreateGenZipDir then
    begin
      Result := ForceDirectories(GenZipDir);
      if not Result then
        RaiseLastOSError;
      TaskDone;
    end;

    Result := DoCleanDirectories;
    if not Result then
      Exit;

    if Task[HelpBuildTask_GenerateOnlineHelpZipFiles] then
    begin
      Result := DoGenerateOnlineHelpZipFiles;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_UnzipOnlineHelpZipFile] then
    begin
      Result := DoUnzipOnlineHelpZipFile;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateImages] then
    begin
      Result := DoGenerateImages;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateRegisteredClassesFile] then
    begin
      Result := DoGenerateRegisteredClassesFile;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateGroupsFile] then
    begin
      Result := DoGenerateGroupsFile;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateStrippedPasFiles] then
    begin
      Result := DoGenerateStrippedPasFiles;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GeneratePackageList] then
    begin
      Result := DoGeneratePackageList;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateDonatorsDtxFile] then
    begin
      Result := DoGenerateDonatorsDtxFile;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateDtxWordsFile] or
      Task[HelpBuildTask_GenerateDtxInvalidWordsFile] or
      Task[HelpBuildTask_GenerateDtxIdentifiersFile] or
      Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile] then
    begin
      Result := DoDiagnoseDtx;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_PreProcessOnlineDtxFiles] then
    begin
      Result := DoPreProcessOnlineDtxFiles;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_GenerateFormattedDtxFiles] or
      Task[HelpBuildTask_GenerateFuncRefDtxFile] or
      Task[HelpBuildTask_GenerateHelpDoxFile] or
      Task[HelpBuildTask_GeneratePackagesDoxFile] or
      Task[HelpBuildTask_GenerateReferenceDoxFile] or
      Task[HelpBuildTask_GeneratePackagesDtxFile] then
    begin
      Result := DoProcessDtxFiles(nil);
      if not Result then
        Exit;
    end;

    // PDF tasks
    if Task[HelpBuildTask_BuildPackagesDoxFile_PDF] then
    begin
      Result := DoBuildDoxFile(PackagesDoxFileName, cConfiguration_PDF);
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_BuildReferenceDoxFile_PDF] then
    begin
      Result := DoBuildDoxFile(ReferenceDoxFileName, cConfiguration_PDF);
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_ZipPackagesPDF] then
    begin
      Result := DoZipPackagesPdf;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_ZipReferencePDF] then
    begin
      Result := DoZipReferencePdf;
      if not Result then
        Exit;
    end;

    // Winhelp tasks

    if Task[HelpBuildTask_BuildHelpDoxFile_WinHelp] then
    begin
      Result := DoBuildDoxFile(DoxFileName, cConfiguration_WinHelp);
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_PostProcessWinHelpOutput] then
    begin
      Result := DoPostProcessWinHelpOutput;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_CompileWinHelp] then
    begin
      Result := DoCompileWinHelp;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_ZipWinHelp] then
    begin
      Result := DoZipWinHelp;
      if not Result then
        Exit;
    end;

    // HTML Help tasks

    if Task[HelpBuildTask_BuildHelpDoxFile_HTML] then
    begin
      Result := DoBuildDoxFile(DoxFileName, cConfiguration_HTMLHelp);
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_PostProcessHtmlHelpOutput] then
    begin
      Result := DoPostProcessHtmlHelpOutput;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_CompileHtmlHelp] then
    begin
      Result := DoBuildHtmlHelp;
      if not Result then
        Exit;
    end;

    if Task[HelpBuildTask_ZipHtmlHelp] then
    begin
      Result := DoZipHtmlHelp;
      if not Result then
        Exit;
    end;
  end;

  TargetMsg(Format('Finished in %s' , [TimerStr]));
  StatusMsg('');
  ProgressBarTarget.Position := ProgressBarTarget.Max;
  ProgressBarCompile.Position := 0;

  FFinished := True;
end;

procedure TFrameBuild.HintMsg(const Msg: string);
begin
  ShowErrorHeader;
  AddLine(Msg, [], clGreen);
end;

procedure TFrameBuild.Init;
begin
  FInitializing := True;
  try
    RichEditLog.Visible := False;
    LblOpenFile.Visible := False;
    BtnDetails.Visible := True;

    LblTarget.Caption := '';
    LblTarget.Hint := '';
    ProgressBarTarget.Max := 100;
    ProgressBarTarget.Position := 0;

    LblInfo.Caption := '';
    ProgressBarCompile.Max := 100;
    ProgressBarCompile.Position := 0;
  finally
    FInitializing := False;
  end;
end;

procedure TFrameBuild.InitTasksTodo(const Arr: array of Boolean);
var
  I: Integer;
  TasksTodo: Integer;
begin
  TasksTodo := 0;
  for I := Low(Arr) to High(Arr) do
    if Arr[I] then
      Inc(TasksTodo);

  FProgress.TaskCount := TasksTodo;
  FProgress.Start;

  ProgressBarTarget.Max := FProgress.AllTaskMax;
  ProgressBarTarget.Position := FProgress.AllTaskPosition;
end;

procedure TFrameBuild.LblOpenFileClick(Sender: TObject);
begin
  //  if ShellExecute(Handle, 'open', PChar(LblOpenFile.Hint), nil, nil, SW_SHOW) < 32 then // do not localize
  //    MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
end;

procedure TFrameBuild.StatusMsg(const Msg: string);
begin
  LblInfo.Caption := Msg;
  AddLine(Msg, []);
end;

procedure TFrameBuild.TargetMsg(const Msg: string);
begin
  LblTarget.Caption := Msg;

  AddLine('', []);
  AddLine(Msg, [fsBold]);
end;

procedure TFrameBuild.TaskDone;
begin
  FProgress.NextTask;

  ProgressBarTarget.Max := FProgress.AllTaskMax;
  ProgressBarTarget.Position := FProgress.AllTaskPosition;

  EvIdle(Self);
end;

procedure TFrameBuild.TaskProgress(const Position, Max: Integer);
begin
  FProgress.CurrentTaskPosition := Position;
  FProgress.CurrentTaskMax := Max;

  ProgressBarCompile.Max := FProgress.CurrentTaskMax;
  ProgressBarCompile.Position := FProgress.CurrentTaskPosition;

  ProgressBarTarget.Max := FProgress.AllTaskMax;
  ProgressBarTarget.Position := FProgress.AllTaskPosition;

  EvIdle(Self);
end;

procedure TFrameBuild.TaskStatus(const S: string);
begin
  TargetMsg(S);
  EvIdle(Self);
end;

procedure TFrameBuild.WarningMsg(const Msg: string);
begin
  ShowErrorHeader;
  AddLine(Msg, [], clBlue);
end;

function TFrameBuild.DoGenerateOnlineHelpZipFiles: Boolean;
begin
  with TOnlineHelpZipFilesGenerator.Create(Self) do
  try
    SrcDirectory := HelpBuilder.Data.HelpDir;
    SrcExtension := '*.dtx';
    SrcDateAfter := HelpBuilder.Data.MinDateForZippedFiles;
    DestDirectory := HelpBuilder.Data.GenZipDir;
    MaxFilesInZip := HelpBuilder.Data.MaxFilesInZip;
    ZipExecFileName := HelpBuilder.Data.WZZipFileName;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoUnzipOnlineHelpZipFile: BOolean;
begin
  with TUnzipExec.Create(Self) do
  try
    SrcFileName := HelpBuilder.Data.OnlineHelpZipFileName;
    DestDirectory := HelpBuilder.Data.OnlineHelpDtxDir;
    UnZipExec := HelpBuilder.Data.WZUnzipFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

procedure TFrameBuild.ErrorHeader(const Msg: string);
begin
  FErrorHeader := Msg;
end;

procedure TFrameBuild.ShowErrorHeader;
//var
//  S: string;
begin
  if FErrorHeader = '' then
    Exit;

  AddLine('', [], clBlack);
  AddLine(FErrorHeader, [], clBlack);
  AddLine(StringOfChar('-', Length(FErrorHeader)), [], clBlack);

  FErrorHeader := '';
end;

function TFrameBuild.DoDiagnoseDtx: Boolean;
var
  DtxDiagnoser: TDtxDiagnoser;
begin
  DtxDiagnoser := TDtxDiagnoser.Create;
  try
    DtxDiagnoser.CollectWords :=
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxWordsFile] or
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxInvalidWordsFile];
    DtxDiagnoser.CollectIdentifiers :=
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxIdentifiersFile] or
      HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile];

    DtxDiagnoser.NotWhiteSpaceStr := HelpBuilder.Data.NotWhiteSpaceStr;
    DtxDiagnoser.IgnoreWordsContainingNumbers :=
      HelpBuilder.Data.IgnoreWordsContainingNumbers;

    Result := DoProcessDtxFiles(DtxDiagnoser);
    if not Result then
      Exit;

    DtxDiagnoser.ShowStats;

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxWordsFile] then
    begin
      Result := DoGenerateDtxWordsFile(DtxDiagnoser);
      if not Result then
        Exit;
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxInvalidWordsFile] then
    begin
      Result := DoGenerateDtxInvalidWordsFile(DtxDiagnoser);
      if not Result then
        Exit;
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxIdentifiersFile] then
    begin
      Result := DoGenerateDtxIdentifiersFile(DtxDiagnoser);
      if not Result then
        Exit;
    end;

    if HelpBuilder.Data.Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile] then
    begin
      Result := DoGenerateDtxDuplicateIdentifiersFile(DtxDiagnoser);
      if not Result then
        Exit;
    end;
  finally
    DtxDiagnoser.Free;
  end;
end;

function TFrameBuild.DoGenerateDtxDuplicateIdentifiersFile(
  ADtxDiagnoser: TDtxDiagnoser): Boolean;
begin
  with TDuplicateIdentifiersProducer.Create(Self) do
  try
    DuplicateIdentifiersFileName :=
      HelpBuilder.Data.DtxDuplicateIdentifiersFileName;
    DtxDiagnoser := ADtxDiagnoser;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGenerateDtxIdentifiersFile(
  ADtxDiagnoser: TDtxDiagnoser): Boolean;
begin
  TargetMsg('Saving dtx identifiers file..');
  ADtxDiagnoser.Identifiers.SaveToFile(HelpBuilder.Data.DtxIdentifiersFileName);
  TaskDone;
  Result := True;
end;

function TFrameBuild.DoGenerateDtxInvalidWordsFile(
  ADtxDiagnoser: TDtxDiagnoser): Boolean;
begin
  with TInvalidWordsProducer.Create(Self) do
  try
    DictionaryFileName := HelpBuilder.Data.DictionaryFileName;
    InvalidWordsFileName := HelpBuilder.Data.DtxInvalidWordsFileName;
    DtxDiagnoser := ADtxDiagnoser;
    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoGenerateDtxWordsFile(
  ADtxDiagnoser: TDtxDiagnoser): Boolean;
begin
  TargetMsg('Saving dtx words to file..');
  ADtxDiagnoser.Words.SaveToFile(HelpBuilder.Data.DtxWordsFileName);
  TaskDone;
  Result := True;
end;

function TFrameBuild.DoZipHtmlHelp: Boolean;
var
  CHMFileName: string;
  DestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-HTML.zip
  DestFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.GenZipDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '') + 'Help-HTML.zip';
  CHMFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.HtmlHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '.chm');

  with TZipTask.Create(Self) do
  try
    Zipper.SourceFileNames.Add(CHMFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := HelpBuilder.Data.WZZipFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoZipWinHelp: Boolean;
var
  HLPFileName, CntFileName, AlsFileName: string;
  DestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-WinHelp.zip
  DestFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.GenZipDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '') + 'Help-WinHelp.zip';
  AlsFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.WinHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '') + '.als';
  CntFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.WinHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '') + '.cnt';
  HLPFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.WinHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.DoxFileName), '') + '.HLP';

  with TZipTask.Create(Self) do
  try
    Zipper.SourceFileNames.Add(HLPFileName);
    Zipper.SourceFileNames.Add(CntFileName);
    Zipper.SourceFileNames.Add(AlsFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := HelpBuilder.Data.WZZipFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoPreProcessOnlineDtxFiles: Boolean;
begin
  with TDtxFilesPreProcessor.Create(Self) do
  try
    SourceDir := HelpBuilder.Data.OnlineHelpDtxDir;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoZipPackagesPdf: Boolean;
var
  PDFFileName: string;
  DestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-HTML.zip
  // JVCL330Packages -> JVCL330Packages-PDF.zip
  DestFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.GenZipDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.PackagesDoxFileName), '') + '-PDF.zip';
  PDFFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.PDFHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.PackagesDoxFileName), '.pdf');

  with TZipTask.Create(Self) do
  try
    Zipper.SourceFileNames.Add(PDFFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := HelpBuilder.Data.WZZipFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

function TFrameBuild.DoZipReferencePdf: Boolean;
var
  PDFFileName: string;
  DestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-HTML.zip
  // JVCL330Reference -> JVCL330Reference-PDF.zip
  DestFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.GenZipDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.ReferenceDoxFileName), '') + '-PDF.zip';
  PDFFileName := IncludeTrailingPathDelimiter(HelpBuilder.Data.PDFHelpOutputDir) +
    ChangeFileExt(ExtractFileName(HelpBuilder.Data.ReferenceDoxFileName), '.pdf');

  with TZipTask.Create(Self) do
  try
    Zipper.SourceFileNames.Add(PDFFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := HelpBuilder.Data.WZZipFileName;

    Result := Execute;
  finally
    Free;
  end;
end;

end.

