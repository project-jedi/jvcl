unit MainCtrl;

interface

uses
  Classes, ParserTypes, Settings;

const
  CSummaryDescription = 'Summary'#13#10'  Write here a summary (1 line)';
  CSummaryDescriptionOverride = CSummaryDescription +
    #13#10'  This is an overridden method, you don''t have to describe these' +
    #13#10'  if it does the same as the inherited method';
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

    FAllSkipFiles: TStringList;
    FAllProcessFiles: TStringList;
    FShowGeneratedFiles: Boolean;

    procedure SetShowCompletedFiles(const Value: Boolean);
    procedure SetShowIgnoredFiles(const Value: Boolean);
    procedure SetShowOtherFiles(const Value: Boolean);
    procedure SetShowGeneratedFiles(const Value: Boolean);
  protected
    procedure DoMessage(const Msg: string);
    procedure WriteDtx(ATypeList: TTypeList);
    procedure SettingsChanged(Sender: TObject; ChangeType: TSettingsChangeType);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ProcessItem(const AFileName: string);
    procedure Process;

    procedure RefreshFiles;
    procedure RefreshSkipFiles;
    procedure RefreshProcessFiles;
    procedure FilterFiles(AllList, FilteredList: TStrings);

    procedure UpdateSourceFiles;

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
  SysUtils,
  JclFileUtils, JvProgressDialog,
  DelphiParser;

function GetClassInfoStr(AItem: TAbstractItem): string;
begin
  if (AItem.DelphiType = dtClass) and TSettings.Instance.IsRegisteredClass(AItem.SimpleName) then
    Result := Format(CClassInfo, [AItem.SimpleName])
  else
    Result := '';
end;

function GetTitleStr(AItem: TAbstractItem): string;
begin
  if AITem.TitleName > '' then
    Result := Format('<TITLE %s>', [AItem.TitleName])
  else
    Result := '';
end;

function GetSummaryStr(AItem: TAbstractItem): string;
begin
  if (AItem.DelphiType in [dtMethodFunc, dtMethodProc]) and (AItem is TParamClassMethod) and
    (diOverride in TParamClassMethod(AItem).Directives) then

    Result := CSummaryDescriptionOverride
  else
    Result := CSummaryDescription;
end;

function GetDescriptionStr(AItem: TAbstractItem): string;
begin
  Result := CDescriptionDescription + AItem.AddDescriptionString;
end;

function GetCombineStr(AItem: TAbstractItem): string;
begin
  if AITem.CombineString > '' then
    Result := Format('<COMBINE %s>', [AItem.CombineString])
  else
    Result := '';

  (*
  Result := '';
  if AItem.DelphiType <> dtType then
    Exit;

  S := AItem.ValueString;
  if S = '' then
    Exit;

  if StrLIComp(PChar(S), 'set of', 6) = 0 then
  begin
    S := Trim(Copy(S, 8, MaxInt));
    while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
      Delete(S, Length(S), 1);
    Result := Format('<COMBINE %s>'#13#10, [S]);
    Exit;
  end;

  if S[1] = '^' then
  begin
    Delete(S, 1, 1);
    S := Trim(S);
    while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
      Delete(S, Length(S), 1);
    Result := Format('<COMBINE %s>'#13#10, [S]);
    Exit;
  end;*)
end;

function GetParamStr(AItem: TAbstractItem): string;
begin
  Result := AItem.ParamString;
  if Result > '' then
    Result := CParamDescription + Result;
end;

function GetReturnsStr(AItem: TAbstractItem): string;
begin
  if AItem.DelphiType in [dtFunction, dtProcedure] then
    Result := ''
  else
    Result := CReturnsDescription;
end;

{ TMainCtrl }

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

constructor TMainCtrl.Create;
begin
  TSettings.Instance.RegisterObserver(Self, SettingsChanged);

  FAllSkipFiles := TStringList.Create;
  with FAllSkipFiles do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;
  FAllProcessFiles := TStringList.Create;
  with FAllProcessFiles do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;

  FShowGeneratedFiles := True;
  FShowOtherFiles := False;
  FShowIgnoredFiles := False;
  FShowCompletedFiles := False;
end;

destructor TMainCtrl.Destroy;
begin
  TSettings.Instance.UnRegisterObserver(Self);
  FAllSkipFiles.Free;
  FAllProcessFiles.Free;
  inherited;
end;

procedure TMainCtrl.DoMessage(const Msg: string);
begin
  if Assigned(MessagesList) then
    MessagesList.Add(Msg);
end;

procedure TMainCtrl.FilterFiles(AllList, FilteredList: TStrings);
var
  I: Integer;
  LIsCompletedUnit: Boolean;
  LIsIgnoredUnit: Boolean;
  LIsGeneratedUnit: Boolean;
  LDoAdd: Boolean;
begin
  AllList.AddStrings(FilteredList);

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

procedure TMainCtrl.Process;
var
  I: Integer;
  Dir: string;
  ProgressDlg: TJvProgressDialog;
begin
  if not Assigned(ProcessList) then
    Exit;
  Dir := IncludeTrailingPathDelimiter(TSettings.Instance.PasDir);
  FParsedOK := 0;
  FParsedError := 0;

  ProgressDlg := TJvProgressDialog.Create(nil);
  try
    ProgressDlg.Min := 0;
    ProgressDlg.Max := ProcessList.Count;
    ProgressDlg.Caption := 'Progress';
    ProgressDlg.Show;

    for I := 0 to ProcessList.Count - 1 do
    begin
      ProgressDlg.Text := ProcessList[I];
      ProgressDlg.Position := I;
      ProcessItem(Dir + ProcessList[I]);
    end;

    DoMessage(Format('Errors %d OK %d Total %d',
      [FParsedError, FParsedOK, FParsedError + FParsedOK]));
  finally
    ProgressDlg.Hide;
    ProgressDlg.Free;
  end;
end;

procedure TMainCtrl.ProcessItem(const AFileName: string);
var
  Parser: TDelphiParser;
begin
  Parser := TDelphiParser.Create;
  try
    Parser.AcceptCompilerDirectives := TSettings.Instance.AcceptCompilerDirectives;
    if Parser.Execute(AFileName) then
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

procedure TMainCtrl.RefreshFiles;
var
  AllFiles: TStringList;
  I, Index: Integer;
begin
  AllFiles := TStringList.Create;
  try
    BuildFileList(IncludeTrailingPathDelimiter(TSettings.Instance.PasDir) +
      '*.pas', faAnyFile, AllFiles);

    for I := FAllSkipFiles.Count - 1 downto 0 do
    begin
      Index := AllFiles.IndexOf(FAllSkipFiles[I]);
      if Index < 0 then
        FAllSkipFiles.Delete(I)
      else
        AllFiles.Delete(Index);
    end;

    for I := FAllProcessFiles.Count - 1 downto 0 do
    begin
      Index := AllFiles.IndexOf(FAllProcessFiles[I]);
      if Index < 0 then
        FAllProcessFiles.Delete(I)
      else
        AllFiles.Delete(Index);
    end;

    for I := 0 to AllFiles.Count - 1 do
      FAllProcessFiles.Add(AllFiles[I]);

    RefreshSkipFiles;
    RefreshProcessFiles;
  finally
    AllFiles.Free;
  end;
end;

{procedure TMainCtrl.RemoveIgnoredFiles;
var
  I: Integer;
begin
  if Assigned(FProcessList) then
    with FProcessList do
    begin
      BeginUpdate;
      try
        for I := Count - 1 downto 0 do
          if (FIgnoreFiles and TSettings.Instance.IsIgnoredUnit(Strings[I])) or
            (FIgnoreCompletedFiles and TSettings.Instance.IsCompletedUnit(Strings[I])) then
            Delete(I);
      finally
        EndUpdate;
      end;
    end;

  if Assigned(FSkipList) then
    with FSkipList do
    begin
      BeginUpdate;
      try
        for I := Count - 1 downto 0 do
          if (FIgnoreFiles and TSettings.Instance.IsIgnoredUnit(Strings[I])) or
            (FIgnoreCompletedFiles and TSettings.Instance.IsCompletedUnit(Strings[I])) then
            Delete(I);
      finally
        EndUpdate;
      end;
    end;
end;}

{procedure TMainCtrl.SetIgnoreCompletedFiles(const Value: Boolean);
begin
  if Value = FIgnoreCompletedFiles then
    Exit;

  FIgnoreCompletedFiles := Value;
  if FIgnoreCompletedFiles then
    RemoveIgnoredFiles
  else
    UpdateSourceFiles;
end;}

{procedure TMainCtrl.SetIgnoreFiles(const Value: Boolean);
begin
  if Value = FIgnoreFiles then
    Exit;

  FIgnoreFiles := Value;
  if FIgnoreFiles then
    RemoveIgnoredFiles
  else
    UpdateSourceFiles;
end;}

procedure TMainCtrl.RefreshProcessFiles;
begin
  FilterFiles(FAllProcessFiles, ProcessList);
end;

procedure TMainCtrl.RefreshSkipFiles;
begin
  FilterFiles(FAllSkipFiles, SkipList);
end;

procedure TMainCtrl.SetShowCompletedFiles(const Value: Boolean);
begin
  if FShowCompletedFiles <> Value then
  begin
    FShowCompletedFiles := Value;
    RefreshSkipFiles;
    RefreshProcessFiles;
  end;
end;

procedure TMainCtrl.SetShowGeneratedFiles(const Value: Boolean);
begin
  if FShowGeneratedFiles <> Value then
  begin
    FShowGeneratedFiles := Value;
    RefreshSkipFiles;
    RefreshProcessFiles;
  end;
end;

procedure TMainCtrl.SetShowIgnoredFiles(const Value: Boolean);
begin
  if FShowIgnoredFiles <> Value then
  begin
    FShowIgnoredFiles := Value;
    RefreshSkipFiles;
    RefreshProcessFiles;
  end;
end;

procedure TMainCtrl.SetShowOtherFiles(const Value: Boolean);
begin
  if FShowOtherFiles <> Value then
  begin
    FShowOtherFiles := Value;
    RefreshSkipFiles;
    RefreshProcessFiles;
  end;
end;

procedure TMainCtrl.SettingsChanged(Sender: TObject;
  ChangeType: TSettingsChangeType);
begin
  case ChangeType of
    ctPasDirectory: UpdateSourceFiles;
    ctGeneratedDtxDirectory: ;
    ctRealDtxDirectory: ;
  end;
end;

procedure TMainCtrl.UpdateSourceFiles;
begin
  {if Assigned(SkipList) then
    SkipList.Clear;

  if not Assigned(ProcessList) then
    Exit;
  ProcessList.BeginUpdate;
  try
    ProcessList.Clear;
    BuildFileList(IncludeTrailingPathDelimiter(TSettings.Instance.InDir) +
      '*.pas', faAnyFile, ProcessList);

    if IgnoreFiles or IgnoreCompletedFiles then
      RemoveIgnoredFiles;
  finally
    ProcessList.EndUpdate;
  end;}
end;

procedure TMainCtrl.WriteDtx(ATypeList: TTypeList);
const
  {TDelphiType = (dtClass, dtConst, dtDispInterface, dtFunction, dtFunctionType,
    dtInterface, dtMethodFunc, dtMethodProc, dtProcedure, dtProcedureType,
    dtProperty, dtRecord, dtResourceString, dtSet, dtType, dtVar);}
  {TOutputType = (otClass, otConst, otDispInterface, otFunction, otFunctionType,
    otInterface, otProcedure, otProcedureType, otProperty, otRecord,
    otResourceString, otSet, otType, otVar);}

  CConvert: array[TDelphiType] of TOutputType =
  (otClass, otConst, otType, otFunction, otFunctionType,
    otType, otFunction, otProcedure, otProcedure, otProcedureType,
    otProperty, otRecord, otResourcestring, otSet, otType, otVar);
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
    S := StringReplace(S, '%sortname', ATypeItem.SortName, [rfReplaceAll, rfIgnoreCase]);
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
    S := StringReplace(S, '%author', ATypeList.Author, [rfReplaceAll,
      rfIgnoreCase]);
    S := StringReplace(S, '%unitname', UnitName, [rfReplaceAll, rfIgnoreCase]);
    FileStream.Write(PChar(S)^, Length(S));
  end;

  procedure WriteType(ATypeItem: TAbstractItem);
  var
    S: string;
  begin
    { Inherited properties [property X;] niet toevoegen }
    if (ATypeItem is TMethodProp) and (TMethodProp(ATypeItem).InheritedProp) then
      Exit;

    { Create, Destroy ook niet }
    if SameText(ATypeItem.SimpleName, 'create') or SameText(ATypeItem.SimpleName, 'destroy') then
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
    S := StringReplace(S, '%sortname', ATypeItem.SortName, [rfReplaceAll, rfIgnoreCase]);
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

