unit DoxProducer;

interface

uses
  Classes, DtxParser, DtxAnalyzer, JVCLHelpUtils;

type
  TDoxProducer = class(TTask)
  private
    FDtxInfos: TDtxInfos;
    FAdditionalFiles: TStrings;
    FMinPercComplete: Integer;
    FTemplateDoxFileName: string;
    FPasDir: string;
    FDtxDir: string;
    FDestDoxFileName: string;
    FMaxFilesInDox: Integer;
    FInfoDir: string;
    FDtxSelectionKind: TDtxSelectionKind;
    FSpecificDtxFiles: TStringList;
    FDoxTitle: string;
    FVersionMajor: string;
    FVersionMinor: string;
    FAddInfoDtxFiles: Boolean;
    FRemovePackages: Boolean;
    FRemoveFuncRef: Boolean;
    FPDFHelpOutputDir: string;
    function BuildDox: Boolean;
    function FillSourceFilesSection(Template: TStrings): Boolean;
    function ChangeTargetFileName_WinHelp(Template: TStrings): Boolean;
    function ChangeTargetFileName_PDF(Template: TStrings): Boolean;
    function ChangeTitle(Template: TStrings): Boolean;
    procedure RemoveEmpty;
    procedure SetMinPercComplete(const Value: Integer);
    procedure CollectAddFiles;
    function IsInOkList(const I: Integer): Boolean;
    function GetSpecificDtxFiles: TStrings;
    procedure SetSpecificDtxFiles(Value: TStrings);

    procedure CollectExistingFiles(Template: TStrings; const StartIndex, Count:
      Integer; ExistingFiles: TStrings);
    procedure AddFiles(Template: TStrings; Files: TStrings);
    procedure InsertFiles(Template: TStrings; const StartIndex, OriginalCount:
      Integer; Files: TStrings);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;

    property PasDir: string read FPasDir write FPasDir;
    property DtxDir: string read FDtxDir write FDtxDir;
    property InfoDir: string read FInfoDir write FInfoDir;
    property PDFHelpOutputDir: string read FPDFHelpOutputDir write FPDFHelpOutputDir;

    property VersionMajor: string read FVersionMajor write FVersionMajor;
    property VersionMinor: string read FVersionMinor write FVersionMinor;
    property DoxTitle: string read FDoxTitle write FDoxTitle;
    property TemplateDoxFileName: string read FTemplateDoxFileName write
      FTemplateDoxFileName;
    property DestDoxFileName: string read FDestDoxFileName write
      FDestDoxFileName;
    property DtxInfos: TDtxInfos read FDtxInfos write FDtxInfos;
    property MinPercComplete: Integer read FMinPercComplete write
      SetMinPercComplete;
    property MaxFilesInDox: Integer read FMaxFilesInDox write FMaxFilesInDox;
    property DtxSelectionKind: TDtxSelectionKind read FDtxSelectionKind write
      FDtxSelectionKind;
    property SpecificDtxFiles: TStrings read GetSpecificDtxFiles write
      SetSpecificDtxFiles;
    property AddInfoDtxFiles: Boolean read FAddInfoDtxFiles write
      FAddInfoDtxFiles;
    property RemovePackages: Boolean read FRemovePackages write FRemovePackages;
    property RemoveFuncRef: Boolean read FRemoveFuncRef write FRemoveFuncRef;
  end;

implementation

uses
  JclFileUtils, JclStrings,
  SysUtils;

//resourcestring
//  SAnalyzeInfoAction = 'Analyzing data.';
//  SAnalyzeInfoSubActCaption = 'Task:';
//  SBuildMainAction = 'Building main help file project';
//  SBuildMainSubActCaption = 'File:';

//=== Local procedures =======================================================

function GetValue(Strings: TStrings; Index: Integer): Integer;
var
  Name: string;
begin
  Name := Strings.Names[Index];
  Result := StrToIntDef(Copy(Strings[Index], Length(Name) + 2,
    Length(Strings[Index]) - Length(Name) - 1), 0);
end;

procedure SetValue(Strings: TStrings; Index, Value: Integer);
var
  Name: string;
begin
  Name := Strings.Names[Index];
  Strings[Index] := Name + '=' + IntToStr(Value);
end;

procedure IncValue(Strings: TStrings; Index: Integer; N: Integer = 1);
var
  Name: string;
  Value: Integer;
begin
  Name := Strings.Names[Index];
  Value := StrToIntDef(Copy(Strings[Index], Length(Name) + 2,
    Length(Strings[Index]) - Length(Name) - 1), 0);
  Inc(Value, N);
  Strings[Index] := Name + '=' + IntToStr(Value);
end;

//=== { TDoxProducer } =======================================================

constructor TDoxProducer.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FMaxFilesInDox := MaxInt;
  FAdditionalFiles := TStringList.Create;
  FSpecificDtxFiles := TStringList.Create;
  FSpecificDtxFiles.Sorted := True;
  FSpecificDtxFiles.Duplicates := dupIgnore;
  FDtxSelectionKind := dskPercentage;
  FAddInfoDtxFiles := True;
  FRemovePackages := False;
  FRemoveFuncRef := False;
end;

destructor TDoxProducer.Destroy;
begin
  FAdditionalFiles.Free;
  FSpecificDtxFiles.Free;
  inherited Destroy;
end;

function TDoxProducer.BuildDox: Boolean;
var
  Template: TStrings;
begin
  Template := TStringList.Create;
  try
    Template.LoadFromFile(TemplateDoxFileName);

    Result := FillSourceFilesSection(Template);
    if not Result then
      Exit;

    Result := ChangeTargetFileName_PDF(Template);
    if not Result then
      Exit;

    Result := ChangeTargetFileName_WinHelp(Template);
    if not Result then
      Exit;

    Result := ChangeTitle(Template);
    if not Result then
      Exit;

    Template.SaveToFile(DestDoxFileName);

    HintMsgFmt('%d dtx files in .dox file', [FDtxInfos.Count]);
  finally
    Template.Free;
  end;
end;

function TDoxProducer.CanStart: Boolean;
begin
  Result := CheckDir(InfoDir) and Assigned(DtxInfos);
end;

procedure TDoxProducer.CollectAddFiles;
begin
  if AddInfoDtxFiles then
    GetAllFilesFrom(InfoDir, '*.dtx', FAdditionalFiles);
end;

function TDoxProducer.DoExecute: Boolean;
begin
  RemoveEmpty;
  CollectAddFiles;

  Result := BuildDox;
end;

function TDoxProducer.GetTaskDescription: string;
begin
  Result := 'Building dox file';
end;

function TDoxProducer.IsInOkList(const I: Integer): Boolean;
begin
  Result := (SpecificDtxFiles.IndexOf(FDtxInfos[i].UnitName) >= 0);
end;

function ObjectsSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;

procedure TDoxProducer.RemoveEmpty;
var
  I: Integer;
  RemovedUnits: TStringList;
begin
  StatusMsg('Removing undocumented units');

  RemovedUnits := TStringList.Create;
  try
    for I := FDtxInfos.Count - 1 downto 0 do
      case DtxSelectionKind of
        dskPercentage, dskMax:
          if FDtxInfos[I].PercComplete < MinPercComplete then
          begin
            RemovedUnits.AddObject(FDtxInfos[I].UnitName,
              TObject(FDtxInfos[I].PercComplete));
            FDtxInfos.RemoveUnit(I);
          end;
        dskSpecific:
          if not IsInOkList(I) then
            FDtxInfos.RemoveUnit(I)
          else
            HintMsgFmt('%s: completed %d%%', [FDtxInfos[i].UnitName,
              FDtxInfos[i].PercComplete]);
      end;

    if DtxSelectionKind = dskMax then
    begin
      while (FDtxInfos.Count > 0) and (FDtxInfos.Count > MaxFilesInDox) do
        FDtxInfos.RemoveUnit(0);
    end;

    RemovedUnits.CustomSort(ObjectsSort);
    for I := 0 to RemovedUnits.Count - 1 do
      HintMsgFmt('Removed: (%d%% complete) %s',
        [Integer(RemovedUnits.Objects[i]), RemovedUnits[i]]);
  finally
    RemovedUnits.Free;
  end;
end;

procedure TDoxProducer.SetMinPercComplete(const Value: Integer);
begin
  FMinPercComplete := Value;
  if FMinPercComplete < 0 then
    FMinPercComplete := 0
  else if FMinPercComplete > 100 then
    FMinPercComplete := 100;
end;

function TDoxProducer.GetSpecificDtxFiles: TStrings;
begin
  Result := FSpecificDtxFiles;
end;

procedure TDoxProducer.SetSpecificDtxFiles(Value: TStrings);
begin
  FSpecificDtxFiles.Assign(Value);
end;

function TDoxProducer.FillSourceFilesSection(Template: TStrings): Boolean;
var
  FileSectionCntIndex: Integer;
  FileNum: Integer;
  AllFiles: TStringList;
  Index: Integer;
begin
  Result := True;

  FileSectionCntIndex := Template.IndexOf('[Source Files]');
  if FileSectionCntIndex < 0 then
  begin
    ErrorMsg('Couldn''t locate [Source Files] section in template file.');
    Result := False;
    Exit;
  end;

  Inc(FileSectionCntIndex);
  FileNum := GetValue(Template, FileSectionCntIndex);

  AllFiles := TStringList.Create;
  try
    CollectExistingFiles(Template, FileSectionCntIndex + 1, FileNum, AllFiles);

    if RemovePackages then
    begin
      Index := AllFiles.IndexOf('generated includes\JVCL.Packages.dtx');
      if Index >= 0 then
        AllFiles.Delete(Index);
    end;
    if RemoveFuncRef then
    begin
      Index := AllFiles.IndexOf('generated includes\JVCL.FuncRef.dtx');
      if Index >= 0 then
        AllFiles.Delete(Index);
    end;

    AddFiles(Template, AllFiles);
    InsertFiles(Template, FileSectionCntIndex + 1, FileNum, AllFiles);

    SetValue(Template, FileSectionCntIndex, AllFiles.Count);
  finally
    AllFiles.Free;
  end;
end;

function IsSectionString(const S: string): Boolean;
begin
  Result := (Length(S) >= 2) and (S[1] = '[') and (S[Length(S)] = ']');
end;

function FindKey(Strings: TStrings; const StartIndex: Integer; const Key:
  string): Integer;
var
  APrefix: string;
begin
  APrefix := Key + '=';

  Result := StartIndex;
  while (Result < Strings.Count) and
    not IsPrefix(APrefix, Strings[Result]) and not
      IsSectionString(Strings[Result]) do
  begin
    Inc(Result);
  end;
  if (Result >= Strings.Count) or IsSectionString(Strings[Result]) then
    Result := -1;
end;

function ChangeKey(Strings: TStrings; const StartIndex: Integer; const Key,
  NewValue: string): Integer;
begin
  Result := FindKey(Strings, StartIndex, Key);
  if Result >= 0 then
    Strings[Result] := Format('%s=%s', [Key, NewValue]);
end;

function TDoxProducer.ChangeTargetFileName_WinHelp(Template: TStrings): Boolean;
const
  cPostfix = '\{C81B476F-5DE3-416C-A73A-9674B8ECC110}\Basic]';
var
  Index: Integer;
  TargetRtfFileName: string;
begin
  //   Find text:
  //
  //    [Configurations\xxx\{C81B476F-5DE3-416C-A73A-9674B8ECC110}\Basic]
  //
  //   Change output to
  //
  //         OutputFile=output\WinHELP\JVCLxx.rtf

  Index := LocatePostfix(Template, 0, cPostFix);
  Result := Index >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate WinHelp Basic section');
    Exit;
  end;

  TargetRtfFileName := ChangeFileExt(ExtractFileName(DestDoxFileName), '.rtf');

  Result := ChangeKey(Template, Index + 1, 'OutputFile',
    Format('output\WinHELP\%s', [TargetRtfFileName])) >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate ''OutputFile'' key in WinHelp Basic section');
    Exit;
  end;
end;

function TDoxProducer.ChangeTitle(Template: TStrings): Boolean;
var
  Index: Integer;
begin
  Index := LocateString(Template, 0, '[General]');
  Result := Index >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate General section');
    Exit;
  end;

  Result := ChangeKey(Template, Index + 1, 'Title', DoxTitle) >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate ''Title'' key in General section');
    Exit;
  end;

  Result := ChangeKey(Template, Index + 1, 'VersionMajor', VersionMajor) >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate ''VersionMajor'' key in General section');
    Exit;
  end;

  Result := ChangeKey(Template, Index + 1, 'VersionMinor', VersionMinor) >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate ''VersionMinor'' key in General section');
    Exit;
  end;
end;

procedure TDoxProducer.AddFiles(Template, Files: TStrings);
var
  I: Integer;
  DtxRelPath, PasRelPath: string;
  RelPath: string;
  ADoxDir: string;
begin
  ADoxDir := ExtractFilePath(DestDoxFileName);
  DtxRelPath := IncludeTrailingPathDelimiter(PathGetRelativePath(ADoxDir,
    DtxDir));
  PasRelPath := IncludeTrailingPathDelimiter(PathGetRelativePath(ADoxDir,
    PasDir));

  // add additional files; contributors; howto's etc.
  for I := 0 to FAdditionalFiles.Count - 1 do
  begin
    RelPath := IncludeTrailingPathDelimiter(PathGetRelativePath(ADoxDir,
      ExtractFilePath(FAdditionalFiles[i])));
    Files.Add(Format('%s%s', [RelPath, ExtractFileName(FAdditionalFiles[i])]));
  end;

  // add dtx & pas files
  for I := 0 to FDtxInfos.Count - 1 do
  begin
    Files.Add(DtxRelPath + FDtxInfos[I].UnitName + '.dtx');
    Files.Add(PasRelPath + FDtxInfos[I].UnitName + '.pas');
  end;
end;

procedure TDoxProducer.CollectExistingFiles(Template: TStrings;
  const StartIndex, Count: Integer; ExistingFiles: TStrings);
var
  I: Integer;
begin
  for I := StartIndex to StartIndex + Count - 1 do
    ExistingFiles.Add(Template.ValueFromIndex[I]);
end;

procedure TDoxProducer.InsertFiles(Template: TStrings; const StartIndex,
  OriginalCount: Integer; Files: TStrings);
var
  I: Integer;
  Value: string;
begin
  for I := 0 to Files.Count - 1 do
  begin
    Value := Format('File%d=%s', [I, Files[i]]);
    if I < OriginalCount then
      Template[StartIndex + I] := Value
    else
      Template.Insert(StartIndex + I, Value);
  end;

  for I := OriginalCount - 1 downto Files.Count do
    Template.Delete(StartIndex + I);
end;

function TDoxProducer.ChangeTargetFileName_PDF(
  Template: TStrings): Boolean;
const
  cPostfix = '\{51AFC76F-1628-478C-88DB-FA76DB55F3BE}\Basic]';
var
  Index: Integer;
  TargetPDFFileName: string;
begin
  //   Find text:
  //
  //    [Configurations\xxx\{51AFC76F-1628-478C-88DB-FA76DB55F3BE}\Basic]
  //
  //   Change output to
  //
  //         OutputFile=output\WinHELP\JVCLxx.rtf

  Index := LocatePostfix(Template, 0, cPostFix);
  Result := Index >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate PDF Basic section');
    Exit;
  end;

  TargetPDFFileName := ChangeFileExt(ExtractFileName(DestDoxFileName), '.pdf');

  Result := ChangeKey(Template, Index + 1, 'OutputFilename',
    TargetPDFFileName) >= 0;
  if not Result then
  begin
    ErrorMsg('Could NOT locate ''OutputFilename'' key in PDF Basic section');
    Exit;
  end;
end;

end.

