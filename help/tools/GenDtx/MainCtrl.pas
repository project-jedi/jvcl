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
    FIgnoreFiles: Boolean;
    procedure SetIgnoreFiles(const Value: Boolean);
  protected
    procedure DoMessage(const Msg: string);
    procedure WriteDtx(ATypeList: TTypeList);
    procedure SettingsChanged(Sender: TObject; ChangeType: TSettingsChangeType);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ProcessItem(const AFileName: string);
    procedure Process;

    procedure UpdateSourceFiles;
    procedure RemoveIgnoredFiles;

    property SkipList: TStrings read FSkipList write FSkipList;
    property ProcessList: TStrings read FProcessList write FProcessList;
    property MessagesList: TStrings read FMessagesList write FMessagesList;
    property IgnoreFiles: Boolean read FIgnoreFiles write SetIgnoreFiles;
  end;

implementation

uses
  SysUtils,
  JclFileUtils, JvProgressComponent,
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

(*function CanCombine(AItem: TAbstractItem): Boolean;
var
  S: string;
begin
  Result := AItem.DelphiType = dtType;
  if not Result then
    Exit;

  S := AItem.ValueString;

  Result := (S > '') and
    (
    ((StrLIComp(PChar(S), 'set of', 6) = 0) and (StrLIComp(PChar(S), 'set of (', 8) <> 0))
    or (S[1] = '^'));
end;*)

(*function GetCombineWithStr(AItem: TAbstractItem): string;
begin
  if AITem.CombineWithString > '' then
    Result := Format('<COMBINEWITH %s>', [AItem.CombineWithString])
  else
    Result := '';
end;*)

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

constructor TMainCtrl.Create;
begin
  TSettings.Instance.RegisterObserver(Self, SettingsChanged);
  FIgnoreFiles := True;
end;

destructor TMainCtrl.Destroy;
begin
  TSettings.Instance.UnRegisterObserver(Self);
  inherited;
end;

procedure TMainCtrl.DoMessage(const Msg: string);
begin
  if Assigned(MessagesList) then
    MessagesList.Add(Msg);
end;

procedure TMainCtrl.Process;
var
  I: Integer;
  Dir: string;
  ProgressDlg: TJvProgressComponent;
begin
  if not Assigned(ProcessList) then
    Exit;
  Dir := IncludeTrailingPathDelimiter(TSettings.Instance.InDir);
  FParsedOK := 0;
  FParsedError := 0;

  ProgressDlg := TJvProgressComponent.Create(nil);
  try
    ProgressDlg.ProgressMin := 0;
    ProgressDlg.ProgressMax := ProcessList.Count;
    ProgressDlg.Caption := 'Progress';
    ProgressDlg.Execute;

    for I := 0 to ProcessList.Count - 1 do
    begin
      ProgressDlg.InfoLabel := ProcessList[I];
      ProgressDlg.ProgressPosition := I;
      ProcessItem(Dir + ProcessList[I]);
    end;

    DoMessage(Format('Errors %d OK %d Total %d',
      [FParsedError, FParsedOK, FParsedError + FParsedOK]));
  finally
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

procedure TMainCtrl.RemoveIgnoredFiles;
var
  I: Integer;
begin
  if Assigned(FProcessList) then
    with FProcessList do
    begin
      BeginUpdate;
      try
        for I := Count - 1 downto 0 do
          if TSettings.Instance.IsIgnoredUnit(Strings[I]) then
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
          if TSettings.Instance.IsIgnoredUnit(Strings[I]) then
            Delete(I);
      finally
        EndUpdate;
      end;
    end;
end;

procedure TMainCtrl.SetIgnoreFiles(const Value: Boolean);
begin
  if Value = FIgnoreFiles then
    Exit;

  FIgnoreFiles := Value;
  if FIgnoreFiles then
    RemoveIgnoredFiles
  else
    UpdateSourceFiles;
end;

procedure TMainCtrl.SettingsChanged(Sender: TObject;
  ChangeType: TSettingsChangeType);
begin
  case ChangeType of
    ctInDirectory: UpdateSourceFiles;
    ctOutDirectory: ;
  end;
end;

procedure TMainCtrl.UpdateSourceFiles;
begin
  if Assigned(SkipList) then
    SkipList.Clear;

  if not Assigned(ProcessList) then
    Exit;
  ProcessList.BeginUpdate;
  try
    ProcessList.Clear;
    BuildFileList(IncludeTrailingPathDelimiter(TSettings.Instance.InDir) +
      '*.pas', faAnyFile, ProcessList);

    if IgnoreFiles then
      RemoveIgnoredFiles;
  finally
    ProcessList.EndUpdate;
  end;
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
  FileName := IncludeTrailingPathDelimiter(TSettings.Instance.OutDir) +
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

