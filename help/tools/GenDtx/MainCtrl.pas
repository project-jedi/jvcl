unit MainCtrl;

interface

uses
  Classes, ParserTypes, Settings, FilterDlg, JvProgressDialog,
  EditPascleanOptionsDlg, ItemFilter;

const
  CSummaryDescription = 'Summary'#13#10'  Write here a summary (1 line)';
  CDescriptionDescription = 'Description'#13#10'  Write here a description'#13#10;
  CSeeAlsoDescription = 'See Also'#13#10'  List here other properties, methods (comma seperated)'#13#10 +
    '  Remove the ''See Also'' section if there are no references';
  CReturnsDescription = 'Return value'#13#10'  Describe here what the function returns';
  CParamDescription = 'Parameters'#13#10;
  CValueReference = '(Value = %Value - for reference)';
  CClassInfo = '<TITLEIMG %s>'#13#10'JVCLInfo'#13#10'  GROUP=JVCL.??'#13#10'  FLAG=Component'#13#10;

type
  TMainCtrl = class
  private
    FSkipList: TStrings;
    FParsedOK: Integer;
    FParsedError: Integer;
    FProcessList: TStrings;
    FMessagesList: TStrings;

    FShowOtherFiles: Boolean;
    FShowIgnoredFiles: Boolean;
    FShowCompletedFiles: Boolean;
    FShowGeneratedFiles: Boolean;

    FAllFiles: TStringList;
    FAllFilteredFiles: TStringList;

    FFilter: TItemFilter;

    FGenerateDtxVisibilities: TClassVisibilities;
    FCurrentErrorGroup: string;
    FCurrentErrorCount: Integer;
    FCurrentCheckFile: string;
    FHeaderOfCurrentFilePrinted: Boolean;
    FHeaderOfCurrentErrorGroupPrinted: Boolean;

    FProgressDlg: TJvProgressDialog;

    procedure SetShowCompletedFiles(const Value: Boolean);
    procedure SetShowIgnoredFiles(const Value: Boolean);
    procedure SetShowOtherFiles(const Value: Boolean);
    procedure SetShowGeneratedFiles(const Value: Boolean);
  protected
    procedure CheckDir(const ADir: string);

    procedure StartErrorGroup(const AErrorGroup: string);
    procedure StartComparing(const AFileName: string);
    procedure EndComparing;
    procedure PrintHeaderOfCurrentErrorGroup;
    procedure DoMessageFmt(const AFormat: string; const Args: array of const);
    procedure DoMessage(const Msg: string); overload;
    procedure DoMessage(Strings: TStrings); overload;
    procedure DoError(const Msg: string); overload;
    procedure DoError(Strings: TStrings); overload;
    procedure DoErrorFmt(const AFormat: string; const Args: array of const);

    procedure WriteDtx(ATypeList: TTypeList);
    procedure FillWithHeaders(const UnitName: string; ATypeList: TTypeList; Optional, NotOptional: TStrings);
    procedure CompareDtxFile(const AUnitName: string;
      DtxHeaders: TList; NotInDtx, NotInPas, DuplicatesInDtx: TStrings; ATypeList: TTypeList);
    procedure CheckJVCLInfos(DtxHeaders: TList);
    procedure CompareParameters(ATypeList: TTypeList; DtxHeaders: TList;
      NotInDtx, NotInPas: TStrings);
    procedure SettingsChanged(Sender: TObject; ChangeType: TSettingsChangeType);
    procedure DetermineCheckable(CheckableList, NotInPasDir, NotInRealDtxDir: TStrings);
    procedure GetAllFilesFrom(const ADir, AFilter: string; AFiles: TStrings; const IncludeSubDirs: Boolean = False;
      const StripExtAndPath: Boolean = True);
    procedure UpdateFiles;
    procedure FilterFiles(AllList, FilteredList: TStrings);

    procedure StartProgress(const ACount: Integer);
    procedure EndProgress;
    procedure UpdateProgress(const AText: string; const APosition: Integer);

    procedure StartParsing;
    procedure EndParsing;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure GenerateDtxFile(const AFileName: string);
    procedure GenerateDtxFiles;

    procedure CheckDtxFile(const AFileName: string);
    procedure CheckDtxFiles;

    procedure CheckPasFile(const AFileName: string);
    procedure CheckPasFiles;

    procedure SortPasFile(const Options: TEditPasCleanOptions; Capitalization: TStrings; const AFileName: string);
    procedure SortPasFiles;

    procedure RecapitalizeFile(Capitalization: TStrings; const AFileName: string);
    procedure RecapitalizeFiles;

    procedure CheckCasingPasFile(const AFileName: string; AllTokens: TStrings;
      const AID: Integer; const CheckAllSymbols, ADoCount: Boolean);
    procedure CheckCasingPasFiles(const CheckAllSymbols, ADoCount: Boolean);

    procedure GenerateListInPackage(const AFileName: string; List, AllList: TStrings);
    procedure GeneratePackageList;

    procedure GenerateListFor(const AFileName: string; List: TStrings; const AID: Integer);
    procedure GenerateList;

    procedure GenerateRegisteredClassesListInFile(const AFileName: string; AList: TStrings);
    procedure GenerateRegisteredClassesList;

    procedure GenerateClassStructureFor(const AFileNameWithPath: string; List: TStrings);
    procedure GenerateClassStructure;

    procedure CheckDuplicateTypesInFile(const AFileName: string; List: TStrings;
      const AID: Integer);
    procedure CheckDuplicateTypes;

    procedure RefreshFiles;

    procedure AddToIgnoreList(const S: string);
    procedure AddToCompletedList(const S: string);

    property SkipList: TStrings read FSkipList write FSkipList;
    property ProcessList: TStrings read FProcessList write FProcessList;
    property MessagesList: TStrings read FMessagesList write FMessagesList;

    property ShowCompletedFiles: Boolean read FShowCompletedFiles write SetShowCompletedFiles;
    property ShowIgnoredFiles: Boolean read FShowIgnoredFiles write SetShowIgnoredFiles;
    property ShowGeneratedFiles: Boolean read FShowGeneratedFiles write SetShowGeneratedFiles;
    property ShowOtherFiles: Boolean read FShowOtherFiles write SetShowOtherFiles;
  end;

implementation

uses
  Windows, SysUtils,
  JclFileUtils, JvSearchFiles, VisibilityDlg, ClassStructureDlg,
  DelphiParser;

type
  TCaseSensitiveStringList = class(TStringList)
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

const
  CConvert: array[TDelphiType] of TOutputType =
  (otClass, otConst, otType, otFunction, otFunctionType,
    otInterface, otFunction, otProcedure, otProcedure, otProcedureType,
    otProperty, otRecord, otResourcestring, otSet, otType, otVar, otField, otMetaClass, otType);

  { efJVCLInfoGroup, efJVCLInfoFlag, efNoPackageTag, efPackageTagNotFilled,
    efNoStatusTag, efEmptySeeAlso, efNoAuthor }

  CDtxErrorNice: array[TDtxCompareErrorFlag] of string = (
    'No ##Package tag', '##Package tag not filled', 'No ##Status tag',
    'Has empty ''See Also'' section(s)', 'No author specified',
    'HASTOCENTRY error', 'Unknown Tags: ');

  { jieGroupNotFilled, jieFlagNotFilled, jieOtherNotFilled, jieNoGroup, jieDoubles }

  CJVCLInfoErrorNice: array[TJVCLInfoError] of string = (
    'Group not filled', 'Flag not filled', 'Other not filled',
    'No Group', 'Double identifiers');

  { pefNoLicense, pefUnitCase }

  CPasErrorNice: array[TPasCheckErrorFlag] of string = (
    'No license', 'Case differs, filename: %s - unitname: %s'
    );

  { dtWriteSummary, dtWriteDescription, dtListProperties }

  CDefaultTextNice: array[TDefaultText] of string =
  ('Write here a summary', 'write here a description',
    'This type is used by (for reference):',
    'List here other properties',
    'Remove the ''see also'' section if there are no references',
    'Describe here what the function returns',
    'This is an overridden method, you don''t have to describe these',
    'If it does the same as the inherited method',
    'Description for',
    'Description for this parameter');

  //=== Local procedures =======================================================

procedure DiffLists(Source1, Source2, InBoth, NotInSource1, NotInSource2: TStrings; const CaseSensitive: Boolean =
  False);
var
  Index1, Index2: Integer;
  C: Integer;
begin
  if not Assigned(Source1) or not Assigned(Source2) then
    Exit;

  Index1 := 0;
  Index2 := 0;
  while (Index1 < Source1.Count) and (Index2 < Source2.Count) do
  begin
    if CaseSensitive then
      C := CompareStr(Source1[Index1], Source2[Index2])
    else
      C := AnsiCompareText(Source1[Index1], Source2[Index2]);
    if C = 0 then
    begin
      if Assigned(InBoth) then
        InBoth.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
      Inc(Index2);
    end
    else
      if C < 0 then
    begin
      if Assigned(NotInSource2) then
        NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1)
    end
    else
      if C > 0 then
    begin
      if Assigned(NotInSource1) then
        NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  end;

  if Assigned(NotInSource1) then
    while Index2 < Source2.Count do
    begin
      NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  if Assigned(NotInSource2) then
    while Index1 < Source1.Count do
    begin
      NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
    end;
end;

procedure DetermineDuplicates(Source, Duplicates: TStrings);
var
  I: Integer;
begin
  for I := 1 to Source.Count-1 do
    if SameText(Source[i-1], Source[i]) then
      Duplicates.Add(Source[i]);
end;

procedure RemoveDoubles(AStrings: TStrings);
var
  Head, Tail: Integer;
begin
  Tail := AStrings.Count - 1;

  while Tail > 0 do
  begin
    Head := Tail;
    while (Head > 0) and (CompareStr(AStrings[Head - 1], AStrings[Head]) = 0) do
      Dec(Head);

    { [Head..Tail] are case sensitive equal }

    if (Head > 0) and SameText(AStrings[Head - 1], AStrings[Head]) then
    begin
      { delete mode }
      while (Head > 0) and SameText(AStrings[Head - 1], AStrings[Head]) do
        Dec(Head);
      { Head <> Tail }

      { [Head..Tail] are case-insensitive equal, but not case sensitive equal }
      while Tail > Head do
      begin
        AStrings.Delete(Tail);
        Dec(Tail);
      end;
      { Tail = Head + 1 }

      Dec(Tail);
    end
    else
      Tail := Head - 1;

    { Head = Tail }
  end;
end;

procedure FillWithDtxHeaders(DtxHeaders: TList; Dest: TStrings);
var
  I: Integer;
begin
  for I := 0 to DtxHeaders.Count - 1 do
    with TDtxItem(DtxHeaders[I]) do
      if HasTocEntry then
        Dest.Add(Tag);
end;

procedure ExcludeList(Source, RemoveList: TStrings; const CaseSensitive: Boolean = False);
var
  SourceIndex, RemoveIndex: Integer;
  C: Integer;
begin
  SourceIndex := 0;
  RemoveIndex := 0;
  while (SourceIndex < Source.Count) and (RemoveIndex < RemoveList.Count) do
  begin
    if CaseSensitive then
      C := CompareStr(Source[SourceIndex], RemoveList[RemoveIndex])
    else
      C := AnsiCompareText(Source[SourceIndex], RemoveList[RemoveIndex]);
    if C = 0 then
    begin
      Source.Delete(SourceIndex);
      Inc(RemoveIndex);
    end
    else
      if C < 0 then
      Inc(SourceIndex)
    else
      if C > 0 then
      Inc(RemoveIndex);
  end;
end;

procedure RemoveSingles(AStrings: TStrings);
var
  I: Integer;
begin
  I := AStrings.Count - 2;
  while I >= 1 do
  begin
    if SameText(AStrings[I - 1], AStrings[I]) then
      Dec(I, 2)
    else
      if SameText(AStrings[I], AStrings[I + 1]) then
      Dec(I)
    else
    begin
      AStrings.Delete(I);
      Dec(I);
    end;
  end;

  if AStrings.Count > 1 then
    if not SameText(AStrings[0], AStrings[1]) then
      AStrings.Delete(0);
  if AStrings.Count > 1 then
    if not SameText(AStrings[AStrings.Count - 1], AStrings[AStrings.Count - 2]) then
      AStrings.Delete(AStrings.Count - 1);
  if AStrings.Count = 1 then
    AStrings.Delete(0);
end;

procedure RemoveDoublesLessThan(AStrings: TStrings; const MinPercentage: Integer);
var
  Head, Tail: Integer;
  Count: Integer;
begin
  Tail := AStrings.Count - 1;

  while Tail > 0 do
  begin
    Head := Tail;
    Count := Integer(AStrings.Objects[Head]);
    while (Head > 0) and SameText(AStrings[Head - 1], AStrings[Head]) do
    begin
      Dec(Head);
      Inc(Count, Integer(AStrings.Objects[Head]));
    end;

    while Tail >= Head do
    begin
      if ((Integer(AStrings.Objects[Tail]) * 100) div Count) < MinPercentage then
        AStrings.Delete(Tail);
      Dec(Tail);
    end;
  end;
end;

function CaseSensitiveSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List[Index1], List[Index2]);
end;

function GetClassInfoStr(AItem: TAbstractItem): string;
begin
  if (AItem.DelphiType = dtClass) and TSettings.Instance.IsRegisteredClass(AItem.SimpleName) then
    Result := Format(CClassInfo, [AItem.SimpleName])
  else
    Result := '';
end;

function GetCombineStr(AItem: TAbstractItem): string;
begin
  if AItem.CombineString > '' then
    Result := Format('<COMBINE %s>', [AItem.CombineString])
  else
    Result := '';
end;

function GetDescriptionStr(AItem: TAbstractItem): string;
begin
  Result := AItem.AddDescriptionString;
  if Result = '' then
    Result := CDescriptionDescription
  else
    Result := 'Description'#13#10 + Result;
end;

function GetParamStr(AItem: TAbstractItem): string;
begin
  Result := AItem.ParamString;
  if Result > '' then
    Result := CParamDescription + Result;
end;

function GetRealFileName(const ADir, AFileName: string): string;
var
  FindData: TWin32FindData;
  Handle: THandle;
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(ADir) + AFileName;

  Handle := FindFirstFile(PChar(LFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := ExtractFileName(FindData.cFileName);
  end
  else
    Result := AFileName;
end;

function GetReturnsStr(AItem: TAbstractItem): string;
begin
  if AItem.DelphiType in [dtFunction, dtProcedure] then
    Result := ''
  else
    Result := CReturnsDescription;
end;

function GetSummaryStr(AItem: TAbstractItem): string;
const
  CSummaryDescription = 'Summary'#13#10'  Write here a summary (1 line)';
begin
  Result := AItem.AddSummaryString;
  if Result = '' then
    Result := CSummaryDescription
  else
    Result := 'Summary'#13#10 + Result;
end;

function GetTitleStr(AItem: TAbstractItem): string;
begin
  if AItem.TitleName > '' then
    Result := Format('<TITLE %s>', [AItem.TitleName])
  else
    Result := '';
end;

function IndexInDtxHeaders(DtxHeaders: TList; const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to DtxHeaders.Count - 1 do
    if SameText(TDtxItem(DtxHeaders[I]).Tag, S) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function DpkNameToNiceName(const S: string): string;
begin
  { 1234567890123
    JvCoreD7R.dpk ->

    Core
  }
  Result := Copy(S, 3, Length(S) - 9);
end;

//=== TCaseSensitiveStringList ===============================================

function TCaseSensitiveStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := CompareStr(S1, S2);
end;

//=== TMainCtrl ==============================================================

procedure TMainCtrl.AddToCompletedList(const S: string);
var
  I: Integer;
begin
  TSettings.Instance.AddToUnitStatus(usCompleted, S);

  if not ShowCompletedFiles then
  begin
    I := SkipList.IndexOf(S);
    if I >= 0 then
      SkipList.Delete(I);
    I := ProcessList.IndexOf(S);
    if I >= 0 then
      ProcessList.Delete(I);
  end;
end;

procedure TMainCtrl.AddToIgnoreList(const S: string);
var
  I: Integer;
begin
  TSettings.Instance.AddToUnitStatus(usIgnored, S);
  if not ShowIgnoredFiles then
  begin
    I := SkipList.IndexOf(S);
    if I >= 0 then
      SkipList.Delete(I);
    I := ProcessList.IndexOf(S);
    if I >= 0 then
      ProcessList.Delete(I);
  end;
end;

procedure TMainCtrl.CheckCasingPasFile(const AFileName: string;
  AllTokens: TStrings; const AID: Integer; const CheckAllSymbols, ADoCount: Boolean);
var
  I: Integer;
  Index: Integer;
begin
  with TPasCasingParser.Create do
  try
    AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    AcceptVisibilities := [inProtected, inPublic, inPublished];
    ID := AID;
    AllSymbols := CheckAllSymbols;
    DoCount := ADoCount;

    if ExecuteFile(AFileName) then
    begin
      Inc(FParsedOK);
      if ADoCount then
        for I := 0 to List.Count - 1 do
        begin
          Index := AllTokens.Add(List[I]);
          AllTokens.Objects[Index] := TObject(Integer(AllTokens.Objects[Index]) + Integer(List.Objects[I]));
        end
      else
        AllTokens.AddStrings(List);
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, ErrorMsg]));
    end;
  finally
    Free;
  end;
end;

procedure TMainCtrl.CheckCasingPasFiles(const CheckAllSymbols, ADoCount: Boolean);
var
  I: Integer;
  AllTokens: TStringList;
  LFiles: TStringList;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  AllTokens := TStringList.Create;
  LFiles := TStringList.Create;
  try
    GetAllFilesFrom(TSettings.Instance.RunTimePasDir, '*.pas', LFiles, False, False);

    StartProgress(LFiles.Count);
    StartParsing;
    try
      AllTokens.Duplicates := dupIgnore;
      AllTokens.Sorted := True;
      AllTokens.CaseSensitive := True;

      for I := 0 to LFiles.Count - 1 do
      begin
        UpdateProgress(LFiles[I], I);
        CheckCasingPasFile(LFiles[I], AllTokens, I, CheckAllSymbols, ADoCount);
      end;

      AllTokens.Sorted := False;
      AllTokens.CaseSensitive := False;
      AllTokens.Sorted := True;

      //RemoveDoubles(AllTokens);
      //RemoveSingles(AllTokens);
      RemoveDoublesLessThan(AllTokens, 95);

      for I := AllTokens.Count - 1 downto 0 do
        if Integer(AllTokens.Objects[I]) < 10 then
          AllTokens.Delete(I);

      for I := 0 to AllTokens.Count - 1 do
        if ADoCount then
          DoMessage(Format('%s       -- %d', [AllTokens[I], Integer(AllTokens.Objects[I])]))
        else
          DoMessage(Format('%s       -- %s', [AllTokens[I], ProcessList[Integer(AllTokens.Objects[I])]]));
      DoMessage('Done');
      DoMessage('--');
    finally
      EndParsing;
      EndProgress;
    end;
  finally
    AllTokens.Free;
    LFiles.Free;
  end;
end;

procedure TMainCtrl.CheckDir(const ADir: string);
begin
  if not DirectoryExists(ADir) then
    raise Exception.CreateFmt('Dir ''%s'' does not exists', [ADir]);
end;

procedure TMainCtrl.CheckDtxFile(const AFileName: string);
const
  CCaseRelatied: array[Boolean] of string = ('', ' <casing differs>');
var
  DelphiParser: TDelphiParser;
  DtxParser: TDtxCompareParser;
  NotInDtx, NotInPas: TStringList;
  DuplicatesInDtx: TStringList;
  ParametersNotInDtx, ParametersNotInPas: TStringList;
  I: Integer;
  Error: TDtxCompareErrorFlag;
  DefaultText: TDefaultText;
begin
  DelphiParser := TDelphiParser.Create;
  DtxParser := TDtxCompareParser.Create;
  NotInDtx := TStringList.Create;
  NotInPas := TStringList.Create;
  ParametersNotInDtx := TStringList.Create;
  ParametersNotInPas := TStringList.Create;
  DuplicatesInDtx := TStringList.Create;
  try
    NotInDtx.Sorted := True;
    NotInPas.Sorted := True;
    ParametersNotInDtx.Sorted := True;
    ParametersNotInPas.Sorted := True;
    DuplicatesInDtx.Sorted := True;

    DelphiParser.AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    DelphiParser.AcceptVisibilities := [inProtected, inPublic, inPublished];

    if not DelphiParser.ExecuteFile(IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir) +
      ChangeFileExt(AFileName, '.pas')) then
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, DelphiParser.ErrorMsg]));
      Exit;
    end;

    if not DtxParser.Execute(IncludeTrailingPathDelimiter(TSettings.Instance.RealDtxDir) +
      ChangeFileExt(AFileName, '.dtx')) then
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, '..']));
      Exit;
    end;

    CompareDtxFile(AFileName, DtxParser.List, NotInDtx, NotInPas, DuplicatesInDtx, DelphiParser.TypeList);
    CompareParameters(DelphiParser.TypeList, DtxParser.List, ParametersNotInDtx, ParametersNotInPas);

    StartComparing(AFileName);
    try
      CheckJVCLInfos(DtxParser.List);

      if DtxParser.Errors <> [] then
      begin
        StartErrorGroup('Errors');
        for Error := Low(TDtxCompareErrorFlag) to High(TDtxCompareErrorFlag) do
          if Error in DtxParser.Errors then
          begin
            if Error = defUnknownTag then
              DoError(CDtxErrorNice[Error] + DtxParser.Tags.CommaText)
            else
              DoError(CDtxErrorNice[Error]);
          end;
      end;
      if DtxParser.DefaultTexts <> [] then
      begin
        StartErrorGroup('Contains default texts');
        for DefaultText := Low(TDefaultText) to High(TDefaultText) do
          if DefaultText in DtxParser.DefaultTexts then
            DoError(CDefaultTextNice[DefaultText]);
      end;
      if DuplicatesInDtx.Count > 0 then
      begin
        StartErrorGroup('Duplicates in dtx file');
        DoError(DuplicatesInDtx);
      end;
      if NotInDtx.Count > 0 then
      begin
        StartErrorGroup('Not in dtx file');
        for I := 0 to NotInDtx.Count - 1 do
          DoError(NotInDtx[I] +
            CCaseRelatied[IndexInDtxHeaders(DtxParser.List, NotInDtx[I]) >= 0]);
      end;
      if NotInPas.Count > 0 then
      begin
        StartErrorGroup('Not in pas file');
        for I := 0 to NotInPas.Count - 1 do
          DoError(NotInPas[I]);
      end;
      if ParametersNotInDtx.Count > 0 then
      begin
        StartErrorGroup('Params not in dtx file');
        DoError(ParametersNotInDtx);
      end;
      if ParametersNotInPas.Count > 0 then
      begin
        StartErrorGroup('Params not in pas file');
        DoError(ParametersNotInPas);
      end;
    finally
      EndComparing;
    end;

    Inc(FParsedOK);
    //WriteDtx(Parser.TypeList);
  finally
    DuplicatesInDtx.Free;
    NotInDtx.Free;
    NotInPas.Free;
    ParametersNotInDtx.Free;
    ParametersNotInPas.Free;
    DtxParser.Free;
    DelphiParser.Free;
  end;
end;

procedure TMainCtrl.CheckDtxFiles;
var
  I: Integer;

  NotInPasDir: TStringList;
  NotInRealDtxDir: TStringList;
  CheckableList: TStringList;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);
  CheckDir(TSettings.Instance.RealDtxDir);

  if not Assigned(ProcessList) then
    Exit;

  NotInPasDir := TStringList.Create;
  NotInRealDtxDir := TStringList.Create;
  CheckableList := TStringList.Create;
  try
    CheckableList.Sorted := True;
    NotInPasDir.Sorted := True;
    NotInRealDtxDir.Sorted := True;

    DetermineCheckable(CheckableList, NotInPasDir, NotInRealDtxDir);

    if NotInPasDir.Count > 0 then
    begin
      DoMessage('Not found in *.pas directory');
      for I := 0 to NotInPasDir.Count - 1 do
        DoMessage(NotInPasDir[I]);
      DoMessage('--');
    end;
    if NotInRealDtxDir.Count > 0 then
    begin
      DoMessage('Not found in *.dtx directory');
      for I := 0 to NotInRealDtxDir.Count - 1 do
        DoMessage(NotInRealDtxDir[I]);
      DoMessage('--');
    end;
    if CheckableList.Count = 0 then
    begin
      DoMessage('Nothing to do');
      DoMessage('--');
      Exit;
    end;

    StartProgress(CheckableList.Count);
    StartParsing;
    try
      for I := 0 to CheckableList.Count - 1 do
      begin
        UpdateProgress(CheckableList[I], I);
        CheckDtxFile(CheckableList[I]);
      end;
      DoMessage('Done');
      DoMessage('--');
    finally
      EndParsing;
      EndProgress;
    end;
  finally
    NotInPasDir.Free;
    NotInRealDtxDir.Free;
    CheckableList.Free;
  end;
end;

procedure TMainCtrl.CheckDuplicateTypes;
var
  I: Integer;
  AllTokens: TStringList;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    AllTokens := TStringList.Create;
    try
      AllTokens.Duplicates := dupAccept;
      AllTokens.Sorted := True;

      for I := 0 to ProcessList.Count - 1 do
      begin
        UpdateProgress(ProcessList[I], I);
        CheckDuplicateTypesInFile(ProcessList[I], AllTokens, I);
      end;

      //AllTokens.SaveToFile('C:\temp\alltokens.txt');

      RemoveSingles(AllTokens);

      for I := 0 to AllTokens.Count - 1 do
        DoMessage(Format('%s       -- %s', [AllTokens[I], ProcessList[Integer(AllTokens.Objects[I])]]));
      DoMessage('Done');
      DoMessage('--');
    finally
      AllTokens.Free;
    end;
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.CheckDuplicateTypesInFile(const AFileName: string;
  List: TStrings; const AID: Integer);
var
  I: Integer;
  Item: TAbstractItem;
begin
  with TDelphiParser.Create do
  try
    AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;

    if ExecuteFile(IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir)
      + ChangeFileExt(AFileName, '.pas')) then
    begin
      Inc(FParsedOK);
      for I := 0 to TypeList.Count - 1 do
      begin
        Item := TAbstractItem(TypeList[I]);
        if Item.DelphiType in [dtClass, DtType, dtVar] then
          List.AddObject(Item.SimpleName, TObject(AID));
      end;
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, ErrorMsg]));
    end;
  finally
    Free;
  end;
end;

procedure TMainCtrl.CheckJVCLInfos(DtxHeaders: TList);
var
  I: Integer;
  Item: TDtxItem;
  S: string;
  IsRegisteredClass: Boolean;
  JVCLInfoError: TJVCLInfoError;
  ItemTagStripped: string;
begin
  StartErrorGroup('JVCLINFO errors');

  for I := 0 to DtxHeaders.Count - 1 do
  begin
    Item := TDtxItem(DtxHeaders[I]);
    ItemTagStripped := Copy(Item.Tag, 3, MaxInt);

    if Item.HasJVCLInfo and (Item.JVCLInfoErrors <> []) then
    begin
      S := Format('%s: ', [Item.Tag]);
      for JVCLInfoError := Low(TJVCLInfoError) to High(TJVCLInfoError) do
        if JVCLInfoError in Item.JVCLInfoErrors then
          S := S + CJVCLInfoErrorNice[JVCLInfoError] + ', ';

      Delete(S, Length(S) - 1, 2);
      DoError(S);
    end;

    IsRegisteredClass := TSettings.Instance.IsRegisteredClass(ItemTagStripped);

    if Item.IsRegisteredComponent and not IsRegisteredClass then
      DoErrorFmt('%s has FLAG=Component in JVCLInfo but is not a registered component', [ItemTagStripped])
    else
      if not Item.IsRegisteredComponent and IsRegisteredClass then
      DoErrorFmt('%s is a registered component but has no FLAG=Component', [ItemTagStripped])
  end;
end;

procedure TMainCtrl.CheckPasFile(const AFileName: string);
var
  PasParser: TPasCheckParser;
  Error: TPasCheckErrorFlag;
begin
  PasParser := TPasCheckParser.Create;
  try
    PasParser.AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    //PasParser.AcceptVisibilities := [inProtected, inPublic, inPublished];

    if not PasParser.ExecuteFile(IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir) +
      ChangeFileExt(AFileName, '.pas')) then
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, PasParser.ErrorMsg]));
      Exit;
    end;

    if PasParser.Errors <> [] then
    begin
      DoMessage(Format('%s--   Errors', [AFileName]));
      for Error := Low(TPasCheckErrorFlag) to High(TPasCheckErrorFlag) do
        if Error in PasParser.Errors then
          if Error = pefUnitCase then
            DoMessage(Format(CPasErrorNice[Error], [PasParser.FileName, PasParser.UnitName]))
          else
            DoMessage(CPasErrorNice[Error]);
    end;

    Inc(FParsedOK);
  finally
    PasParser.Free;
  end;
end;

procedure TMainCtrl.CheckPasFiles;
var
  I: Integer;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    for I := 0 to ProcessList.Count - 1 do
    begin
      UpdateProgress(ProcessList[I], I);
      CheckPasFile(ProcessList[I]);
    end;
    DoMessage('Done');
    DoMessage('--');
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.CompareDtxFile(
  const AUnitName: string;
  DtxHeaders: TList; NotInDtx, NotInPas, DuplicatesInDtx: TStrings;
  ATypeList: TTypeList);
var
  Optional: TStringList;
  NotOptional: TStringList;
  LDtxHeaders, LNotInPas, LNotInDtx: TStringList;
  LDtxHeadersNonCaseSensitive: TStringList;
begin
  Optional := TCaseSensitiveStringList.Create;
  NotOptional := TCaseSensitiveStringList.Create;
  LDtxHeaders := TCaseSensitiveStringList.Create;
  LNotInPas := TCaseSensitiveStringList.Create;
  LNotInDtx := TCaseSensitiveStringList.Create;
  try
    FillWithHeaders(AUnitName, ATypeList, Optional, NotOptional);

    NotOptional.Add('@@' + GetRealFileName(
      TSettings.Instance.RunTimePasDir,
      ChangeFileExt(AUnitName, '.pas')));
    FillWithDtxHeaders(DtxHeaders, LDtxHeaders);

    NotOptional.Sort;
    Optional.Sort;
    LDtxHeaders.Sort;

//    Optional.SaveToFile('C:\Temp\Optional.txt');
//    NotOptional.SaveToFile('C:\Temp\NotOptional.txt');
//    LDtxHeaders.SaveToFile('C:\Temp\DtxHeaders.txt');

    LDtxHeadersNonCaseSensitive := TStringList.Create;
    try
      LDtxHeadersNonCaseSensitive.CaseSensitive := False;
      LDtxHeadersNonCaseSensitive.Duplicates := dupIgnore;
      LDtxHeadersNonCaseSensitive.Assign(LDtxHeaders);
      LDtxHeadersNonCaseSensitive.Sorted := True;
      DetermineDuplicates(LDtxHeadersNonCaseSensitive, DuplicatesInDtx);
    finally
      LDtxHeadersNonCaseSensitive.Free;
    end;
    DiffLists(LDtxHeaders, NotOptional, nil, LNotInDtx, LNotInPas, True);

    LNotInDtx.Sort;
    LNotInPas.Sort;

    ExcludeList(LNotInPas, Optional, True);
    ExcludeList(LNotInDtx, Optional, True);

    NotInPas.Assign(LNotInPas);
    NotInDtx.Assign(LNotInDtx);
  finally
    LDtxHeaders.Free;
    LNotInPas.Free;
    LNotInDtx.Free;
    Optional.Free;
    NotOptional.Free;
  end;
end;

procedure TMainCtrl.CompareParameters(ATypeList: TTypeList; DtxHeaders: TList;
  NotInDtx, NotInPas: TStrings);
var
  I, J: Integer;
  Index: Integer;
  AllPasParameters, AllOptionalPasParameters, AllDtxParameters: TStringList;
  ParamsNotInPas, ParamsNotInDtx: TStringList;
  Params: TStrings;
  DtxItem: TDtxItem;
  TagName: string;
begin
  AllPasParameters := TCaseSensitiveStringList.Create;
  AllOptionalPasParameters := TCaseSensitiveStringList.Create;
  AllDtxParameters := TCaseSensitiveStringList.Create;
  ParamsNotInPas := TStringList.Create;
  ParamsNotInDtx := TStringList.Create;
  try
    AllPasParameters.Sorted := True;
    AllPasParameters.Duplicates := dupIgnore;
    AllOptionalPasParameters.Sorted := True;
    AllOptionalPasParameters.Duplicates := dupIgnore;
    AllDtxParameters.Duplicates := dupAccept;

    for I := 0 to ATypeList.Count - 1 do
    begin
      Params := ATypeList[I].ParamList;
      if Params <> nil then
      begin
        TagName := '@@' + ATypeList[I].ReferenceName;
        if (ATypeList[I] is TClassPropertyItem) and
          (TClassPropertyItem(ATypeList[I]).IsArray) then
        begin
          // 1 param -> optional
          // more params -> required
          if Params.Count = 1 then
            AllOptionalPasParameters.Add(TagName + ' - ' + Params[0])
          else
            for J := 0 to Params.Count - 1 do
              AllPasParameters.Add(TagName + ' - ' + Params[J]);
        end
        else
        begin
          Index := IndexInDtxHeaders(DtxHeaders, TagName);
          if Index >= 0 then
          begin
            DtxItem := TDtxItem(DtxHeaders[Index]);
            if DtxItem.Combine > '' then
              TagName := '@@' + DtxItem.Combine;
            for J := 0 to Params.Count - 1 do
              AllPasParameters.Add(TagName + ' - ' + Params[J]);
          end;
        end;
      end;
    end;

    for I := 0 to DtxHeaders.Count - 1 do
    begin
      Params := TDtxItem(DtxHeaders[I]).Parameters;
      if Params <> nil then
        for J := 0 to Params.Count - 1 do
          AllDtxParameters.Add(TDtxItem(DtxHeaders[I]).Tag + ' - ' + Params[J]);
    end;

    //AllPasParameters.Sorted := False;

    //AllPasParameters.CustomSort(CaseSensitiveSort);
    //AllDtxParameters.CustomSort(CaseSensitiveSort);
    AllDtxParameters.Sort;

//    AllPasParameters.SaveToFile('C:\temp\AllPasParameters.txt');
//    AllDtxParameters.SaveToFile('C:\temp\AllDtxParameters.txt');

    DiffLists(AllPasParameters, AllDtxParameters,
      nil, ParamsNotInPas, ParamsNotInDtx, True);
    { Remove optionals }
    ExcludeList(ParamsNotInPas, AllOptionalPasParameters, True);
    ExcludeList(ParamsNotInDtx, AllOptionalPasParameters, True);

    NotInDtx.AddStrings(ParamsNotInDtx);
    NotInPas.AddStrings(ParamsNotInPas);
  finally
    AllOptionalPasParameters.Free;
    AllPasParameters.Free;
    AllDtxParameters.Free;
    ParamsNotInDtx.Free;
    ParamsNotInPas.Free;
  end;
end;

constructor TMainCtrl.Create;
begin
  TSettings.Instance.RegisterObserver(Self, SettingsChanged);

  FAllFiles := TStringList.Create;
  with FAllFiles do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;

  FAllFilteredFiles := TStringList.Create;
  with FAllFilteredFiles do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;

  FShowGeneratedFiles := True;
  FShowOtherFiles := False;
  FShowIgnoredFiles := False;
  FShowCompletedFiles := False;
  FGenerateDtxVisibilities := [inPublic, inPublished];

  FFilter := TItemFilter.Create;
end;

destructor TMainCtrl.Destroy;
begin
  TSettings.Instance.UnRegisterObserver(Self);
  FAllFilteredFiles.Free;
  FAllFiles.Free;
  FProgressDlg.Free;
  FFilter.Free;
  inherited;
end;

procedure TMainCtrl.DetermineCheckable(CheckableList, NotInPasDir,
  NotInRealDtxDir: TStrings);
var
  AllFilesInPasDir: TStringList;
  AllFilesInRealDtxDir: TStringList;
  AllFiles: TStringList;
begin
  AllFilesInPasDir := TStringList.Create;
  AllFilesInRealDtxDir := TStringList.Create;
  AllFiles := TStringList.Create;
  try
    AllFilesInPasDir.Sorted := True;
    AllFilesInRealDtxDir.Sorted := True;
    AllFiles.Sorted := True;

    GetAllFilesFrom(TSettings.Instance.RunTimePasDir, '*.pas', AllFilesInPasDir);
    GetAllFilesFrom(TSettings.Instance.RealDtxDir, '*.dtx', AllFilesInRealDtxDir);

    DiffLists(ProcessList, AllFilesInPasDir, CheckableList, nil, NotInPasDir);
    DiffLists(ProcessList, AllFilesInRealDtxDir, CheckableList, nil, NotInRealDtxDir);
    ExcludeList(CheckableList, NotInPasDir);
    ExcludeList(CheckableList, NotInRealDtxDir);
  finally
    AllFiles.Free;
    AllFilesInPasDir.Free;
    AllFilesInRealDtxDir.Free;
  end;
end;

procedure TMainCtrl.DoError(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    DoError(Strings[I]);
end;

procedure TMainCtrl.DoError(const Msg: string);
begin
  PrintHeaderOfCurrentErrorGroup;

  Inc(FCurrentErrorCount);
  DoMessage('       ' + Msg);
end;

procedure TMainCtrl.DoErrorFmt(const AFormat: string;
  const Args: array of const);
begin
  DoError(Format(AFormat, Args));
end;

procedure TMainCtrl.DoMessage(const Msg: string);
begin
  if Assigned(MessagesList) then
    MessagesList.Add(Msg);
end;

procedure TMainCtrl.DoMessage(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    DoMessage(Strings[I]);
end;

procedure TMainCtrl.DoMessageFmt(const AFormat: string;
  const Args: array of const);
begin
  DoMessage(Format(AFormat, Args));
end;

procedure TMainCtrl.EndComparing;
begin
  if not FHeaderOfCurrentFilePrinted then
    DoMessageFmt('Comparing %s ... OK', [FCurrentCheckFile])
  else
    if FCurrentErrorCount > 0 then
    DoMessage('--------------------');
end;

procedure TMainCtrl.EndParsing;
begin
  DoMessage(Format('Errors %d OK %d Total %d',
    [FParsedError, FParsedOK, FParsedError + FParsedOK]));
end;

procedure TMainCtrl.EndProgress;
begin
  if not Assigned(FProgressDlg) then
    Exit;

  FProgressDlg.Hide;
  FreeAndNil(FProgressDlg);
end;

procedure TMainCtrl.FillWithHeaders(const UnitName: string; ATypeList: TTypeList;
  Optional, NotOptional: TStrings);
var
  I, J: Integer;
  ATypeItem: TAbstractItem;
  ReferenceName, S: string;
  IsOptional: Boolean;
begin
  for I := 0 to ATypeList.Count - 1 do
  begin
    ATypeItem := ATypeList[I];

    ReferenceName := '@@' + ATypeItem.ReferenceName;

    if TSettings.Instance.OutputTypeEnabled[CConvert[ATypeItem.DelphiType]] then
    begin
      IsOptional :=
        { private,protected members are optional; protected properties not }
      (
        ((ATypeItem is TClassMemberOrFieldItem) and (TClassMemberOrFieldItem(ATypeItem).Position in [inPrivate,
        inProtected]))
          and
        not ((ATypeItem.DelphiType = dtProperty) and (TClassMemberOrFieldItem(ATypeItem).Position = inProtected))
        )

      or

      { overridden methods are optional }
      ((ATypeItem is TParamClassMethodItem) and (diOverride in TParamClassMethodItem(ATypeItem).Directives))

      or

      { inherited properties are optional }
      ((ATypeItem is TClassPropertyItem) and (TClassPropertyItem(ATypeItem).IsInherited))

      or

      { create, destroy are optional}
      ((ATypeItem is TParamClassMethodItem) and
        (SameText(ATypeItem.SimpleName, 'create') or SameText(ATypeItem.SimpleName, 'destroy')));

      IsOptional := IsOptional or
        TSettings.Instance.OnIgnoreTokenList(UnitName, ReferenceName);

      if IsOptional then
      begin
        Optional.Add(ReferenceName);
        if ATypeItem is TListItem then
          TListItem(ATypeItem).AddToList(Optional);
      end
      else
      begin
        NotOptional.Add(ReferenceName);
        if ATypeItem is TListItem then
          with ATypeItem as TListItem do
            for J := 0 to Items.Count - 1 do
            begin
              S := '@@' + ReferenceName + '.' + Items[J];
              if TSettings.Instance.OnIgnoreTokenList(UnitName, S) then
                Optional.Add(S)
              else
                NotOptional.Add(S);
            end;
      end;
    end;
  end;
end;

procedure TMainCtrl.FilterFiles(AllList, FilteredList: TStrings);
var
  I: Integer;
  LIsCompletedUnit: Boolean;
  LIsIgnoredUnit: Boolean;
  LIsGeneratedUnit: Boolean;
  LDoAdd: Boolean;
begin
  FilteredList.BeginUpdate;
  try
    FilteredList.Clear;
    for I := 0 to AllList.Count - 1 do
      with TSettings.Instance do
      begin
        LIsCompletedUnit := False;
        LIsIgnoredUnit := False;
        LIsGeneratedUnit := False;

        if ShowCompletedFiles or ShowOtherFiles then
          LIsCompletedUnit := IsUnitFrom(usCompleted, AllList[I]);
        LDoAdd := ShowCompletedFiles and LIsCompletedUnit;
        if not LDoAdd then
        begin
          if ShowIgnoredFiles or ShowOtherFiles then
            LIsIgnoredUnit := IsUnitFrom(usIgnored, AllList[I]);
          LDoAdd := ShowIgnoredFiles and LIsIgnoredUnit;
          if not LDoAdd then
          begin
            if ShowGeneratedFiles or ShowOtherFiles then
              LIsGeneratedUnit := IsUnitFrom(usGenerated, AllList[I]);
            LDoAdd := (ShowGeneratedFiles and LIsGeneratedUnit) or
              (ShowOtherFiles and not LIsCompletedUnit and not LIsIgnoredUnit and not LIsGeneratedUnit);
          end;
        end;
        if LDoAdd then
          FilteredList.Add(AllList[I])
      end;
  finally
    FilteredList.EndUpdate;
  end;
end;

procedure TMainCtrl.GenerateClassStructure;
var
  Source: TSourceType;
  AddToOldList: Boolean;

  LDirectory: string;
  LFiles: TStringList;
  LIncludeSubDirs: Boolean;
  I: Integer;
  List: TStringList;
begin
  Source := Low(TSourceType);
  AddToOldList := True;

  if not TfrmClassStructure.Execute(Source, AddToOldList) then
    Exit;

  List := TStringList.Create;
  try
    List.Sorted := True;
    List.Duplicates := dupIgnore;

    case Source of
      stDelphi:
        begin
          CheckDir(TSettings.Instance.DelphiRootSourceDir);
          LDirectory := TSettings.Instance.DelphiRootSourceDir;
          LIncludeSubDirs := True;
          if AddToOldList then
            TSettings.Instance.GetDelphiClassStructure(List);
        end;
      stJVCL:
        begin
          CheckDir(TSettings.Instance.RunTimePasDir);
          LDirectory := TSettings.Instance.RunTimePasDir;
          LIncludeSubDirs := False;
          if AddToOldList then
            TSettings.Instance.GetJVCLClassStructure(List);
        end;
    else
      Exit;
    end;

    LFiles := TStringList.Create;
    try
      GetAllFilesFrom(LDirectory, '*.pas', LFiles, LIncludeSubDirs, False);

      StartProgress(LFiles.Count);
      StartParsing;
      try
        for I := 0 to LFiles.Count - 1 do
        begin
          UpdateProgress(LFiles[I], I);
          GenerateClassStructureFor(LFiles[I], List);
        end;

        case Source of
          stDelphi:
            TSettings.Instance.SetDelphiClassStructure(List);
          stJVCL:
            TSettings.Instance.SetJVCLClassStructure(List);
        else
          Exit;
        end;
      finally
        EndProgress;
        EndParsing;
      end;
    finally
      LFiles.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TMainCtrl.GenerateClassStructureFor(const AFileNameWithPath: string;
  List: TStrings);
var
  I: Integer;
begin
  with TDelphiParser.Create do
  try
    AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    AcceptVisibilities := [inPrivate, inProtected, inPublic, inPublished];

    if ExecuteFile(AFileNameWithPath) then
    begin
      Inc(FParsedOK);
      for I := 0 to TypeList.Count - 1 do
        if (TypeList[I].DelphiType = dtClass) and (TypeList[I] is TClassItem) then
          with TClassItem(TypeList[I]) do
            List.Add(SimpleName + '=' + Ancestor)
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileNameWithPath, ErrorMsg]));
    end;
  finally
    Free;
  end;
end;

procedure TMainCtrl.GenerateDtxFile(const AFileName: string);
var
  Parser: TDelphiParser;
begin
  Parser := TDelphiParser.Create;
  try
    Parser.AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    Parser.AcceptVisibilities := FGenerateDtxVisibilities;
    if Parser.ExecuteFile(ChangeFileExt(AFileName, '.pas')) then
    begin
      Inc(FParsedOK);
      WriteDtx(Parser.TypeList);
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, Parser.ErrorMsg]));
    end;
  finally
    Parser.Free;
  end;
end;

procedure TMainCtrl.GenerateDtxFiles;
var
  I: Integer;
  Dir: string;
begin
  { Uses GeneratedDtxDir + RunTimePasDir }
  CheckDir(TSettings.Instance.RunTimePasDir);
  CheckDir(TSettings.Instance.GeneratedDtxDir);

  if not Assigned(ProcessList) then
    Exit;
  Dir := IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir);

  if not TfrmVisibility.Execute(FGenerateDtxVisibilities) then
    Exit;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    for I := 0 to ProcessList.Count - 1 do
    begin
      UpdateProgress(ProcessList[I], I);
      GenerateDtxFile(Dir + ProcessList[I]);
    end;
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.GenerateList;
var
  List: TStringList;
  I: Integer;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  if not TfrmFilter.Execute(FFilter) then
    Exit;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    List := TStringList.Create;
    try
      case FFilter.Duplicates of
        dtHide, dtHideCaseSensitive, dtOnlyCaseSensitiveDuplicates:
          begin
            List.Duplicates := dupIgnore;
            List.CaseSensitive := FFilter.Duplicates in [dtHideCaseSensitive, dtOnlyCaseSensitiveDuplicates];
            List.Sorted := True;
          end;
        dtOnlyCaseSensitiveSingles, dtOnlyDuplicates, dtAll:
          begin
            List.Duplicates := dupAccept;
            List.Sorted := False;
          end;
      else
        raise Exception.Create('Unknown');
      end;

      for I := 0 to ProcessList.Count - 1 do
      begin
        UpdateProgress(ProcessList[I], I);
        GenerateListFor(ProcessList[I], List, I);
      end;

      case FFilter.Duplicates of
        dtOnlyCaseSensitiveDuplicates:
          begin
            List.Sorted := False;
            List.CaseSensitive := False;
            List.Sorted := True;
            RemoveSingles(List);
          end;
        dtOnlyDuplicates:
          begin
            List.Sorted := True;
            RemoveSingles(List);
          end;
        dtOnlyCaseSensitiveSingles:
          begin
            List.Sorted := True;
            RemoveDoubles(List);
          end;
      end;

      for I := 0 to List.Count - 1 do
        DoMessage(Format('%s       -- %s', [List[I], ProcessList[Integer(List.Objects[I])]]));

      DoMessage('Done');
      DoMessage('--');
    finally
      List.Free;
    end;
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.GenerateListFor(const AFileName: string;
  List: TStrings; const AID: Integer);
var
  I: Integer;
  Item: TAbstractItem;
begin
  with TDelphiParser.Create do
  try
    AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    AcceptVisibilities := [inPrivate, inProtected, inPublic, inPublished];

    if ExecuteFile(IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir)
      + ChangeFileExt(AFileName, '.pas')) then
    begin
      Inc(FParsedOK);
      for I := 0 to TypeList.Count - 1 do
      begin
        Item := TAbstractItem(TypeList[I]);
        if FFilter.Includes(Item) then
          List.AddObject(Item.SimpleName, TObject(AID));
      end;
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, ErrorMsg]));
    end;
  finally
    Free;
  end;
end;

procedure TMainCtrl.GenerateListInPackage(const AFileName: string;
  List, AllList: TStrings);
var
  DpkParser: TDpkParser;
  I: Integer;
begin
  DpkParser := TDpkParser.Create;
  try
    DpkParser.ExecuteFile(AFileName);
    for I := 0 to DpkParser.List.Count - 1 do
      List.Add(Format('%s=%s', [DpkParser.List[I], DpkNameToNiceName(ExtractFileName(AFileName))]));
    AllList.AddStrings(DpkParser.List);
  finally
    DpkParser.Free;
  end;
end;

procedure TMainCtrl.GeneratePackageList;
var
  RunTimeDpkFiles: TStringList;
  LFilesInPackages, LAllFilesInPackages: TStringList;
  LNotInPasDir, LNotInDpk: TStringList;
  I: Integer;
  Added, Removed: TStringList;
begin
  CheckDir(TSettings.Instance.PackageDir);

  RunTimeDpkFiles := TStringList.Create;
  LFilesInPackages := TStringList.Create;
  LAllFilesInPackages := TStringList.Create;
  try
    LFilesInPackages.Sorted := True;
    RunTimeDpkFiles.Sorted := True;
    LAllFilesInPackages.Sorted := True;

    with TSettings.Instance do
    begin
      GetAllFilesFrom(PackageDir, '*r.dpk', RunTimeDpkFiles);

      for I := 0 to RunTimeDpkFiles.Count - 1 do
        GenerateListInPackage(PackageDir + '\' + ChangeFileExt(RunTimeDpkFiles[I], '.dpk'),
          LFilesInPackages, LAllFilesInPackages);

      { update list of all .pas files }
      UpdateFiles;

      LNotInPasDir := TStringList.Create;
      LNotInDpk := TStringList.Create;
      try
        DiffLists(FAllFiles, LAllFilesInPackages, nil, LNotInPasDir, LNotInDpk);
        if LNotInPasDir.Count > 0 then
        begin
          DoMessage('-- Files in .dpk''s, but not in .pas dir');
          DoMessage(LNotInPasDir);
          DoMessage('--');
        end;
        if LNotInDpk.Count > 0 then
        begin
          DoMessage('-- Files in .pas dir, but not in any .dpk file');
          DoMessage(LNotInDpk);
          DoMessage('--');
        end;
      finally
        LNotInDpk.Free;
        LNotInPasDir.Free;
      end;

      Added := TStringList.Create;
      Removed := TStringList.Create;
      try
        Added.Sorted := True;
        Removed.Sorted := True;

        DiffLists(FilesInPackages, LFilesInPackages, nil, Added, Removed);
        if (Added.Count = 0) and (Removed.Count = 0) then
          DoMessage('Nothing changed');
        if Removed.Count > 0 then
        begin
          DoMessage('-- Removed:');
          DoMessage('------');
          DoMessage(Removed);
          DoMessage('--');
        end;
        if Added.Count > 0 then
        begin
          DoMessage('-- Added:');
          DoMessage(Added);
          DoMessage('--');
        end;
      finally
        Added.Free;
        Removed.Free;
      end;

      FilesInPackages := LFilesInPackages;
      SaveFilesInPackages;
    end;
  finally
    RunTimeDpkFiles.Free;
    LFilesInPackages.Free;
    LAllFilesInPackages.Free;
  end;
end;

procedure TMainCtrl.GenerateRegisteredClassesList;
var
  DesignTimePasFiles: TStringList;
  LRegisteredClasses: TStringList;
  Added, Removed: TStringList;
  I: Integer;
begin
  CheckDir(TSettings.Instance.DesignTimePasDir);

  DesignTimePasFiles := TStringList.Create;
  LRegisteredClasses := TStringList.Create;
  try
    DesignTimePasFiles.Sorted := True;
    LRegisteredClasses.Sorted := True;

    with TSettings.Instance do
    begin
      GetAllFilesFrom(DesignTimePasDir, '*.pas', DesignTimePasFiles);

      StartParsing;
      try
        for I := 0 to DesignTimePasFiles.Count - 1 do
          GenerateRegisteredClassesListInFile(
            DesignTimePasDir + '\' + ChangeFileExt(DesignTimePasFiles[I], '.pas'),
            LRegisteredClasses);

        Added := TStringList.Create;
        Removed := TStringList.Create;
        try
          Added.Sorted := True;
          Removed.Sorted := True;

          DiffLists(RegisteredClasses, LRegisteredClasses, nil, Added, Removed);
          if (Added.Count = 0) and (Removed.Count = 0) then
            DoMessage('Nothing changed');
          if Removed.Count > 0 then
          begin
            DoMessage('-- Removed:');
            DoMessage('------');
            DoMessage(Removed);
            DoMessage('--');
          end;
          if Added.Count > 0 then
          begin
            DoMessage('-- Added:');
            DoMessage(Added);
            DoMessage('--');
          end;
        finally
          Added.Free;
          Removed.Free;
        end;

        RegisteredClasses := LRegisteredClasses;
        SaveRegisteredClasses;
      finally
        EndParsing;
      end;
    end;
  finally
    DesignTimePasFiles.Free;
    LRegisteredClasses.Free;
  end;
end;

procedure TMainCtrl.GenerateRegisteredClassesListInFile(
  const AFileName: string; AList: TStrings);
//var
//  RegisteredClassesParser: TRegisteredClassesParser;
begin
  with TRegisteredClassesParser.Create do
  try
//    AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;

    if ExecuteFile(AFileName) then
    begin
      Inc(FParsedOk);
      AList.AddStrings(List);
    end
    else
    begin
      Inc(FParsedError);
      DoMessage(Format('[Error] %s - %s', [AFileName, ErrorMsg]));
    end;
  finally
    Free;
  end;
end;

procedure TMainCtrl.GetAllFilesFrom(const ADir, AFilter: string;
  AFiles: TStrings; const IncludeSubDirs, StripExtAndPath: Boolean);
const
  CIncludeSubDirs: array[Boolean] of TJvDirOption = (doExcludeSubDirs, doIncludeSubDirs);
var
  I: Integer;
begin
  AFiles.BeginUpdate;
  try
    AFiles.Clear;

    with TJvSearchFiles.Create(nil) do
    try
      DirOption := CIncludeSubDirs[IncludeSubDirs];
      RootDirectory := ADir;
      Options := [soSearchFiles, soSorted];
      if StripExtAndPath then
        Options := Options + [soStripDirs];
      ErrorResponse := erIgnore;
      DirParams.SearchTypes := [];
      FileParams.SearchTypes := [stFileMask];
      FileParams.FileMask := AFilter;

      Search;

      if StripExtAndPath then
        for I := 0 to Files.Count - 1 do
          AFiles.Add(ChangeFileExt(Files[I], ''))
      else
        AFiles.Assign(Files);
    finally
      Free;
    end;
  finally
    AFiles.EndUpdate;
  end;
end;

procedure TMainCtrl.PrintHeaderOfCurrentErrorGroup;
begin
  if not FHeaderOfCurrentFilePrinted then
  begin
    DoMessage('');
    DoMessageFmt('Comparing %s ... contains errors:', [FCurrentCheckFile]);
    DoMessage('');
    FHeaderOfCurrentFilePrinted := True;
  end;

  if not FHeaderOfCurrentErrorGroupPrinted then
  begin
    DoMessage('o  ' + FCurrentErrorGroup);
    FHeaderOfCurrentErrorGroupPrinted := True;
  end;
end;

procedure TMainCtrl.RecapitalizeFile(Capitalization: TStrings; const AFileName: string);
var
  Parser: TRecapitalizeParser;
  FileStream: TFileStream;
  LFileName: string;
begin
  Parser := TRecapitalizeParser.Create;
  try
    LFileName := IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir) + 'Sorted\' +
      ChangeFileExt(AFileName, '.pas');

    FileStream := TFileStream.Create(LFileName, fmCreate);
    try
      Parser.OutputStream := FileStream;
      Parser.Recapitalize := True;
      Parser.Capitalization := Capitalization;

      if Parser.ExecuteFile(IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir) +
        ChangeFileExt(AFileName, '.pas')) then
      begin
        Inc(FParsedOK);
        WriteDtx(Parser.TypeList);
      end
      else
      begin
        Inc(FParsedError);
        DoMessage(Format('[Error] %s - %s', [AFileName, Parser.ErrorMsg]));
      end;
    finally
      FileStream.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TMainCtrl.RecapitalizeFiles;
var
  I: Integer;
  Capitalization: TStringList;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    Capitalization := TStringList.Create;
    try
      Capitalization.LoadFromFile('C:\JVCL Volunteer\checkout\dev\help\tools\inboth.txt');
      Capitalization.CaseSensitive := False;
      Capitalization.Sorted := True;
      for I := 0 to ProcessList.Count - 1 do
      begin
        UpdateProgress(ProcessList[I], I);
        RecapitalizeFile(capitalization, ProcessList[I]);
      end;
      DoMessage('Done');
      DoMessage('--');
    finally
      Capitalization.Free
    end;
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.RefreshFiles;
begin
  GetAllFilesFrom(TSettings.Instance.RunTimePasDir, '*.pas', FAllFiles);
  UpdateFiles;
end;

procedure TMainCtrl.SetShowCompletedFiles(const Value: Boolean);
begin
  if FShowCompletedFiles <> Value then
  begin
    FShowCompletedFiles := Value;
    UpdateFiles;
  end;
end;

procedure TMainCtrl.SetShowGeneratedFiles(const Value: Boolean);
begin
  if FShowGeneratedFiles <> Value then
  begin
    FShowGeneratedFiles := Value;
    UpdateFiles;
  end;
end;

procedure TMainCtrl.SetShowIgnoredFiles(const Value: Boolean);
begin
  if FShowIgnoredFiles <> Value then
  begin
    FShowIgnoredFiles := Value;
    UpdateFiles;
  end;
end;

procedure TMainCtrl.SetShowOtherFiles(const Value: Boolean);
begin
  if FShowOtherFiles <> Value then
  begin
    FShowOtherFiles := Value;
    UpdateFiles;
  end;
end;

procedure TMainCtrl.SettingsChanged(Sender: TObject;
  ChangeType: TSettingsChangeType);
begin
  case ChangeType of
    ctDirectory: RefreshFiles;
  end;
end;

procedure TMainCtrl.SortPasFile(
  const Options: TEditPasCleanOptions;
  Capitalization: TStrings; const AFileName: string);
var
  Parser: TImplementationParser;
  FileStream: TFileStream;
  ProcessFileName: string;
  OutputFileName: string;
begin
  Parser := TImplementationParser.Create;
  try
    ProcessFileName := IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir) +
      ChangeFileExt(AFileName, '.pas');

    if Options.ROutputToSameDir then
    begin
      OutputFileName := ProcessFileName;
      ProcessFileName := JclFileUtils.FindUnusedFileName(ChangeFileExt(OutputFileName, ''), 'PAS', '');
      if not RenameFile(OutputFileName, ProcessFileName) then
      begin
        DoMessageFmt('[Error] %s - %s', [AFileName, SysErrorMessage(GetLastError)]);
        Exit;
      end;
    end
    else
      OutputFileName := IncludeTrailingPathDelimiter(Options.ROutputDirectory) +
        ChangeFileExt(AFileName, '.pas');

    FileStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      Parser.OutputStream := FileStream;
      Parser.Capitalization := Capitalization;
      Parser.Recapitalize := epcsDoCapitalization in Options.RSwitches;
      Parser.SortImplementation := epcsSortImplementation in Options.RSwitches;

      if not Parser.ExecuteFile(ProcessFileName) then
      begin
        Inc(FParsedError);
        DoMessageFmt('[Error] %s - %s', [AFileName, Parser.ErrorMsg]);
        if Options.ROutputToSameDir then
        begin
          if not RenameFile(ProcessFileName, OutputFileName) then
            DoMessageFmt('[Error] %s - %s', [AFileName, SysErrorMessage(GetLastError)]);
        end;
        Exit;
      end
      else
        Inc(FParsedOK);
    finally
      FileStream.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TMainCtrl.SortPasFiles;
var
  I: Integer;
  Capitalization: TStringList;
  Options: TEditPasCleanOptions;
begin
  CheckDir(TSettings.Instance.RunTimePasDir);

  if not Assigned(ProcessList) then
    Exit;

  if not TfrmEditPasCleanOptions.Execute(Options) then
    Exit;

  if not Options.ROutputToSameDir then
  begin
    if not DirectoryExists(Options.ROutputDirectory) then
      raise Exception.CreateFmt('Directory ''%s'' does not exists', [Options.ROutputDirectory]);

    if SameFileName(
      IncludeTrailingPathDelimiter(Options.ROutputDirectory),
      IncludeTrailingPathDelimiter(TSettings.Instance.RunTimePasDir)) then
      raise Exception.Create('You cannot specify the input directory as the output directory');
  end;

  StartProgress(ProcessList.Count);
  StartParsing;
  try
    Capitalization := TStringList.Create;
    try
      Capitalization.LoadFromFile(Options.RCapitalizationFile);
      Capitalization.CaseSensitive := False;
      Capitalization.Sorted := True;
      for I := 0 to ProcessList.Count - 1 do
      begin
        UpdateProgress(ProcessList[I], I);
        SortPasFile(Options, Capitalization, ProcessList[I]);
      end;
      DoMessage('Done');
      DoMessage('--');
    finally
      Capitalization.Free;
    end;
  finally
    EndParsing;
    EndProgress;
  end;
end;

procedure TMainCtrl.StartComparing(const AFileName: string);
begin
  FCurrentErrorCount := 0;
  FCurrentCheckFile := AFileName;
  FHeaderOfCurrentFilePrinted := False;
end;

procedure TMainCtrl.StartErrorGroup(const AErrorGroup: string);
begin
  FCurrentErrorGroup := AErrorGroup;
  FHeaderOfCurrentErrorGroupPrinted := False;
end;

procedure TMainCtrl.StartParsing;
begin
  FParsedOK := 0;
  FParsedError := 0;
end;

procedure TMainCtrl.StartProgress(const ACount: Integer);
begin
  EndProgress;

  FProgressDlg := TJvProgressDialog.Create(nil);
  FProgressDlg.Min := 0;
  FProgressDlg.Max := ACount;
  FProgressDlg.Caption := 'Progress';
  FProgressDlg.Show;
end;

procedure TMainCtrl.UpdateFiles;
var
  NewSkipList: TStringList;
begin
  NewSkipList := TStringList.Create;
  try
    NewSkipList.Sorted := True;

    SkipList.BeginUpdate;
    ProcessList.BeginUpdate;
    try
      FilterFiles(FAllFiles, FAllFilteredFiles);
      DiffLists(FAllFilteredFiles, SkipList, NewSkipList, nil, nil);

      ProcessList.Assign(FAllFilteredFiles);
      ExcludeList(ProcessList, NewSkipList);
      SkipList.Assign(NewSkipList);
    finally
      ProcessList.EndUpdate;
      SkipList.EndUpdate;
    end;
  finally
    NewSkipList.Free;
  end;
end;

procedure TMainCtrl.UpdateProgress(const AText: string; const APosition: Integer);
begin
  if not Assigned(FProgressDlg) then
    Exit;

  FProgressDlg.Text := AText;
  FProgressDlg.Position := APosition;
end;

procedure TMainCtrl.WriteDtx(ATypeList: TTypeList);
var
  FileName: string;
  FileStream: TFileStream;

  function GetOutputStr(const OutputType: TOutputType; const AName: string): string;
  var
    Index: Integer;
  begin
    with TSettings.Instance do
    begin
      Index := OutputTypeDesc[OutputType].IndexOf(UpperCase(AName));
      if Index < 0 then
        Result := OutputTypeDefaults[OutputType]
      else
        Result := OutputTypeStrings[OutputType][Index];
    end;
  end;

  procedure WriteClassHeader(ATypeItem: TAbstractItem);
  var
    S: string;
  begin
    //S := TSettings.Instance.OutputTypeDefaults[otClassHeader];
    S := GetOutputStr(otClassHeader, ATypeItem.SimpleName);
    S := StringReplace(S, '%author', ATypeList.Author, [rfReplaceAll,
      rfIgnoreCase]);
    S := StringReplace(S, '%simplename', ATypeItem.SimpleName, [rfReplaceAll,
      rfIgnoreCase]);
    S := StringReplace(S, '%referencename', ATypeItem.ReferenceName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%sortname', ATypeItem.DtxSortName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%titlename', ATypeItem.TitleName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%title', GetTitleStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%param', ATypeItem.ParamString, [rfReplaceAll,
      rfIgnoreCase]);
    S := StringReplace(S, '%items', ATypeItem.ItemsString, [rfReplaceAll,
      rfIgnoreCase]);
    S := StringReplace(S, '%nicename',
      TSettings.Instance.NiceName[ATypeItem.ClassString], [rfReplaceAll,
      rfIgnoreCase]);
    FileStream.Write(PChar(S)^, Length(S));
  end;

  procedure WriteHeader;
  var
    S: string;
    UnitName: string;
  begin
    UnitName := ChangeFileExt(ExtractFileName(FileName), '');
    S := TSettings.Instance.OutputTypeDefaults[otHeader];
    S := StringReplace(S, '%package',
      Format('##Package: %s', [TSettings.Instance.FileNameToPackage(UnitName)]),
      [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%author', ATypeList.Author, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%unitname', UnitName, [rfReplaceAll, rfIgnoreCase]);
    FileStream.Write(PChar(S)^, Length(S));
  end;

  procedure WriteType(ATypeItem: TAbstractItem);
  var
    S: string;
  begin
    { Inherited properties [property X;] niet toevoegen }
    if (ATypeItem is TClassPropertyItem) and (TClassPropertyItem(ATypeItem).IsInherited) then
      Exit;

    { Create, Destroy ook niet }
    if SameText(ATypeItem.SimpleName, 'create') or SameText(ATypeItem.SimpleName, 'destroy') then
      Exit;

    if (ATypeItem is TTypeItem) and
      (StrLIComp(PChar(TTypeItem(ATypeItem).Value), 'class of', 8) = 0) then
      Exit;

    if not TSettings.Instance.OutputTypeEnabled[CConvert[ATypeItem.DelphiType]] then
      Exit;
    //S := TSettings.Instance.OutputTypeDefaults[CConvert[ATypeItem.DelphiType]];

    S := GetOutputStr(CConvert[ATypeItem.DelphiType], ATypeItem.SimpleName);

    S := StringReplace(S, '%author', ATypeList.Author, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%name', ATypeItem.SimpleName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%classinfo', GetClassInfoStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%titlename', ATypeItem.TitleName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%title', GetTitleStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%referencename', ATypeItem.ReferenceName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%sortname', ATypeItem.DtxSortName, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%param', GetParamStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%items', ATypeItem.ItemsString, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%class', ATypeItem.ClassString, [rfReplaceAll, rfIgnoreCase]);
    S := StringReplace(S, '%nicename', TSettings.Instance.NiceName[ATypeItem.ClassString], [rfReplaceAll,
      rfIgnoreCase]);
    if not ATypeItem.CanCombine then
    begin
      S := StringReplace(S, '%summary', GetSummaryStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%description', GetDescriptionStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%seealso', CSeeAlsoDescription, [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%returns', GetReturnsStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%combine', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%refvalue', CValueReference, [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%value', ATypeItem.ValueString, [rfReplaceAll, rfIgnoreCase]);
    end
    else
    begin
      S := StringReplace(S, '%summary', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%description', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%seealso', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%returns', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%combine', GetCombineStr(ATypeItem), [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%refvalue', '', [rfReplaceAll, rfIgnoreCase]);
      S := StringReplace(S, '%value', '', [rfReplaceAll, rfIgnoreCase]);
    end;
    S := Trim(S) + #13#10#13#10;
    S := Trim(StringReplace(S, #13#10#13#10, #13#10, [rfReplaceAll]));
    S := Trim(StringReplace(S, #13#10#13#10, #13#10, [rfReplaceAll]));
    S := S + #13#10;

    FileStream.Write(PChar(S)^, Length(S));
  end;

var
  I: Integer;
begin
  FileName := IncludeTrailingPathDelimiter(TSettings.Instance.GeneratedDtxDir) +
    ChangeFileExt(ExtractFileName(ATypeList.FileName), '.dtx');
  if FileExists(FileName) and not TSettings.Instance.OverwriteExisting then
    Exit;

  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    { Eerst de classheaders }
    if TSettings.Instance.OutputTypeEnabled[otClassHeader] then
      for I := 0 to ATypeList.Count - 1 do
        if ATypeList[I] is TClassItem then
          WriteClassHeader(ATypeList[I]);

    { Dan de header }
    if TSettings.Instance.OutputTypeEnabled[otHeader] then
      WriteHeader;

    { Dan de rest }
    for I := 0 to ATypeList.Count - 1 do
      WriteType(ATypeList[I]);
  finally
    FileStream.Free;
  end;
end;

end.

