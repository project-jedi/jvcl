unit HtmlHelpPostProcessor;

interface

uses
  JVCLHelpUtils,
  Classes;

type
  TTaskProgress = class;

  TTaskItemProgress = class
  private
    FMax: Integer;
    FPosition: Integer;
    FWeight: Integer;
    FTaskProgress: TTaskProgress;
    procedure SetPosition(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetWeight(const Value: Integer);
  public
    procedure Done;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition;
    property Weight: Integer read FWeight write SetWeight;
  end;

  TTaskProgress = class
  private
    FOnProgressChanged: TNotifyEvent;
    FPosition: Integer;
    FMax: Integer;
    FIndex: Integer;
    FCount: Integer;
    FSubItem: TTaskItemProgress;
    procedure ItemChanged(const ItemPosition, ItemMax: Integer);
    procedure ItemDone;
    function GetItem: TTaskItemProgress;
    procedure DoProgressChanged;
  public
    destructor Destroy; override;
    property SubItem: TTaskItemProgress read GetItem;
    property Count: Integer read FCount write FCount;
    property OnProgressChanged: TNotifyEvent read FOnProgressChanged write FOnProgressChanged;

    property Position: Integer read FPosition;
    property Max: Integer read FMax;
  end;

  THtmlHelpPostProcessor = class(TTask)
  private
    FHtmlHelpDir: string;
    FHtmlFiles: TStringList;
    FDeletedFiles: TStringList;
    FDoxFileName: string;
    FTaskProgress: TTaskProgress;
    procedure HandleFindFile(Sender: TObject; const AName: string);
    function GetHHPFileName: string;
    function GetHHKFileName: string;
    function DetermineTopicName(const AFileName: string; out TopicName: string): Boolean;
    function DetermineEnd(SS: TStrings; const EndStr: string;
      const StartLineIndex, StartIndex: Integer; out EndLineIndex, EndIndex: Integer): Boolean;
    procedure RemoveLinks(SS: TStrings; const StartLineIndex, StartIndex, EndLineIndex, EndIndex: Integer);
//    procedure RemoveLines(SS: TStrings; const StartLineIndex, StartIndex, EndLineIndex, EndIndex: Integer);

    procedure HandleProgressChanged(Sender: TObject);
  protected
    function CollectHTMLFiles: Boolean;
    function ProcessHTMLFiles(AProgress: TTaskItemProgress): Boolean;

    function ProcessEditLinks(SS: TStrings): Boolean;
    function ProcessLinksInCode(SS: TStrings): Boolean;

    function ProcessHTMLFile(const AFileName: string): Boolean;
    function ProcessHHPFile(AProgress: TTaskItemProgress): Boolean;
    function ProcessHHKFile(AProgress: TTaskItemProgress): Boolean;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property HtmlHelpDir: string read FHtmlHelpDir write FHtmlHelpDir;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property HHPFileName: string read GetHHPFileName;
    property HHKFileName: string read GetHHKFileName;
  end;

implementation

uses
  SysUtils, JclStrings, JvSearchFiles;

//=== { THtmlHelpPostProcessor } =============================================

constructor THtmlHelpPostProcessor.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FHtmlFiles := TStringLIst.Create;
  FDeletedFiles := TStringList.Create;
  FDeletedFiles.Sorted := True;
  FTaskProgress := TTaskProgress.Create;
  FTaskProgress.OnProgressChanged := HandleProgressChanged;
end;

destructor THtmlHelpPostProcessor.Destroy;
begin
  FTaskProgress.Free;
  FDeletedFiles.Free;
  FHtmlFiles.Free;
  inherited Destroy;
end;

function THtmlHelpPostProcessor.CanStart: Boolean;
begin
  Result := CheckDir(HtmlHelpDir) and
    FileExists(HHPFileName) and
    FileExists(HHKFileName);
end;

function THtmlHelpPostProcessor.CollectHTMLFiles: Boolean;
begin
  with TJvSearchFiles.Create(nil) do
  try
    RootDirectory := HtmlHelpDir;
    Options := [soSearchFiles, soOwnerData, soStripDirs];
    ErrorResponse := erIgnore;
    DirOption := doExcludeSubDirs;
    FileParams.FileMask := '*.html';
    FileParams.SearchTypes := [stFileMask];
    OnFindFile := HandleFindFile;
    Result := Search;
    if not Result then
      Exit;
  finally
    Free;
  end;
  //  FHTMLFiles.Add('TJvCustomComboEdit_PopupAlign.html');
end;

function THtmlHelpPostProcessor.DetermineTopicName(const AFileName: string;
  out TopicName: string): Boolean;
const
  cStr = '<a href="http://help.delphi-jedi.org/item.php?name=';
var
  SS: TStringList;
  S: string;
  P, Q: PChar;
  I: Integer;
begin
  SS := TStringLIst.Create;
  try
    SS.LoadFromFile(AFileName);

    I := LocateSubString(SS, 0, cStr);
    Result := I >= 0;
    if not Result then
      Exit;

    S := SS[i];
    I := StrIPos(cStr, S);
    Result := I >= 1;
    if not Result then
      Exit;

    Q := PChar(S) + I - 1 + Length(cStr);
    P := StrScan(Q, '"');
    Result := Assigned(P);
    if not Result then
      Exit;

    SetString(TopicName, Q, P - Q);

    Result := DeleteFile(AFileName);
    if not Result then
      Exit;

    FDeletedFiles.Add(AFileName);
  finally
    SS.Free;
  end;
end;

function THtmlHelpPostProcessor.DoExecute: Boolean;
begin
  FDeletedFiles.Clear;

  FTaskProgress.Count := 3;

  { Index }
  Result := ProcessHHKFile(FTaskProgress.SubItem);
  if not Result then
    Exit;

  Result := CollectHTMLFiles;
  if not Result then
  begin
    ErrorMsg('Error collecting files');
    Exit;
  end;

  Result := ProcessHTMLFiles(FTaskProgress.SubItem);
  if not Result then
  begin
    ErrorMsg('Error processing files');
    Exit;
  end;

  Result := ProcessHHPFile(FTaskProgress.SubItem);
  if not Result then
  begin
    ErrorMsg('Error processing HHP file');
    Exit;
  end;
end;

function THtmlHelpPostProcessor.GetHHPFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(HtmlHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.hhp');
end;

function THtmlHelpPostProcessor.GetTaskDescription: string;
begin
  Result := 'Post processing HTML Help files';
end;

procedure THtmlHelpPostProcessor.HandleFindFile(Sender: TObject;
  const AName: string);
begin
  //  if (AName > '') and (AName[1] <> '_') and (AName[1] <> '$') then
  FHTMLFiles.Add(AName);
end;

function THtmlHelpPostProcessor.ProcessHTMLFile(
  const AFileName: string): Boolean;
var
  SS: TStringList;
begin
  SS := TStringList.Create;
  try
    try
      SS.LoadFromFile(AFileName);
    except
      on E: EStreamError do
      begin
        WarningMsgFmt('Error loading file %s', [QuotedStr(AFileName)]);
        WarningMsg(E.Message);

        // cont
        Result := True;
        Exit;
      end;
    end;

    Result := ProcessEditLinks(SS);
    if not Result then
      Exit;

    Result := ProcessLinksInCode(SS);
    if not Result then
      Exit;

    SS.SaveToFile(AFileName);
  finally
    SS.Free;
  end;
end;

function THtmlHelpPostProcessor.ProcessHTMLFiles(AProgress: TTaskItemProgress): Boolean;
var
  I: Integer;
  AHtmlHelpDir: string;
begin
  Result := True;
  AHtmlHelpDir := IncludeTrailingPathDelimiter(HtmlHelpDir);

  AProgress.Max := FHtmlFiles.Count;
  for I := 0 to FHtmlFiles.Count - 1 do
  begin
    AProgress.Position := I;
    Result := ProcessHTMLFile(AHtmlHelpDir + FHtmlFiles[i]);
    if not Result then
      Exit;
  end;
  AProgress.Done;
end;

function THtmlHelpPostProcessor.ProcessHHPFile(AProgress: TTaskItemProgress): Boolean;
var
  SS: TStringList;
  I: Integer;
  Index, NewIndex: Integer;
begin
  SS := TStringList.Create;
  try
    StatusMsg('Loading..');
    SS.LoadFromFile(HHPFileName);

    StatusMsg('Processing HHP file');

    Index := LocateString(SS, 0, '[FILES]');
    Result := Index >= 0;
    if not Result then
    begin
      ErrorMsg('Could not locate [FILES]');
      Exit;
    end;

    AProgress.Max := FDeletedFiles.Count;
    for I := 0 to FDeletedFiles.Count - 1 do
    begin
      AProgress.Position := I;

      NewIndex := LocateString(SS, Index, FDeletedFiles[i]);
      if NewIndex < 0 then
        WarningMsgFmt('Could not locate <%s>', [FDeletedFiles[i]])
      else
      begin
        SS.Delete(NewIndex);
        Index := NewIndex - 1;
      end;
    end;

    StatusMsg('Saving..');
    SS.SaveToFile(HHPFileName);
    AProgress.Done;
  finally
    SS.Free;
  end;
end;

function THtmlHelpPostProcessor.ProcessHHKFile(AProgress: TTaskItemProgress): Boolean;
const
  cSearchStr =
    '<li><OBJECT type="text/sitemap">';
  cTabbedSearchStr =
    '    <li><OBJECT type="text/sitemap">';
var
  SS: TStringList;
  Index: Integer;
begin
  Result := True;

  SS := TStringList.Create;
  try
    StatusMsg('Loading..');
    SS.LoadFromFile(HHKFileName);

    StatusMsg('Processing HHK file');

    AProgress.Max := SS.Count;
    Index := LocateString(SS, 0, cSearchStr);
    while Index >= 0 do
    begin
      AProgress.Position := Index;

      // search for double items

      // 00: <li><OBJECT type="text/sitemap">
      // 01:     <PARAM name="Keyword" value="TJvCustomDateEdit">
      // 02:     <PARAM name="Name" value="TJvCustomDateEdit">
      // 03:     <PARAM name="Local" value="TJvCustomDateEdit.html">
      // 04:     </OBJECT>
      // 05: <li><OBJECT type="text/sitemap">
      // 06:     <PARAM name="Keyword" value="TJvCustomDateEdit">
      // 07:     <PARAM name="Name" value="TJvCustomDateEdit">
      // 08:     <PARAM name="Local" value="TJvCustomDateEdit_BlanksChar.html">
      // 09:     </OBJECT>
      // 10:     <ul>
      // 11:     <li><OBJECT type="text/sitemap">
      //
      //            ->
      //
      // 01: <li><OBJECT type="text/sitemap">
      // 02:     <PARAM name="Keyword" value="TJvCustomDateEdit">
      // 03:     <PARAM name="Name" value="TJvCustomDateEdit">
      // 04:     <PARAM name="Local" value="TJvCustomDateEdit.html">
      // 05:     </OBJECT>
      // 06:     <ul>
      // 07:     <li><OBJECT type="text/sitemap">

      if SameText(SS[Index], SS[Index + 5]) and
        SameText(SS[Index + 1], SS[Index + 6]) and
        SameText(SS[Index + 2], SS[Index + 7]) and
        SameText(SS[Index + 11], cTabbedSearchStr) then
      begin
        // delete 5..9
        SS.Delete(Index + 9);
        SS.Delete(Index + 8);
        SS.Delete(Index + 7);
        SS.Delete(Index + 6);
        SS.Delete(Index + 5);
      end;

      Index := LocateString(SS, Index + 1, cSearchStr);
    end;

    StatusMsg('Saving..');
    SS.SaveToFile(HHKFileName);
    AProgress.Done;
  finally
    SS.Free;
  end;
end;

function THtmlHelpPostProcessor.GetHHKFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(HtmlHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.hhk');
end;

function THtmlHelpPostProcessor.ProcessEditLinks(SS: TStrings): Boolean;
const
  cSubString = 'editlink.html">editlink</a>';
  cLinkStr = '<a href="http://help.delphi-jedi.org/item.php?Name=%s">Edit topic</a>';

  function DetermineCompleteLink(S: string; out CompleteLink, EditLinkFileName: string): Boolean;
  var
    StartIndex, EndIndex: Integer;
    I: Integer;
    P, Q: PChar;
  begin
    S := Lowercase(S);

    EndIndex := Pos(cSubString, S);
    Result := EndIndex >= 1;
    if not Result then
      Exit;

    I := StrFind('<a href', S);
    StartIndex := -1;

    while (I >= 1) and (I < EndIndex) do
    begin
      StartIndex := I;
      I := StrFind('<a href', S, I + 1);
    end;

    Result := StartIndex >= 1;
    if not Result then
      Exit;

    EndIndex := EndIndex + Length(cSubString);

    CompleteLink := Copy(S, StartIndex, EndIndex - StartIndex);

    Q := StrScan(PChar(CompleteLink), '"');
    Result := Assigned(Q);
    if not Result then
      Exit;

    Inc(Q);
    P := StrScan(Q, '"');
    Result := Assigned(P);
    if not Result then
      Exit;

    SetString(EditLinkFileName, Q, P - Q);
  end;
var
  S: string;
  CompleteLink, EditLinkFileName, TopicName: string;
  I: Integer;
begin
  Result := True;

  I := LocateSubString(SS, 0, cSubString);
  if I < 0 then
    Exit;

  S := SS[i];
  Result := DetermineCompleteLink(S, CompleteLink, EditLinkFileName);
  if not Result then
  begin
    ErrorMsgFmt('Could not determine complete link at line %d', [i]);
    Exit;
  end;

  Result := DetermineTopicName(IncludeTrailingPathDelimiter(HtmlHelpDir) + EditLinkFileName,
    TopicName);
  if not Result then
  begin
    ErrorMsgFmt('Could not determine topic name at line %d', [i]);
    Exit;
  end;

  SS[I] := ChangeSubString(S, CompleteLink, Format(cLinkStr, [TopicName]));
end;

function THtmlHelpPostProcessor.ProcessLinksInCode(SS: TStrings): Boolean;
const
  cSubString = '<div class="element13"><div class="element12"><pre class="element12">';
var
  I: Integer;
  S: string;

  StartLineIndex, EndLineIndex: Integer;
  StartIndex, EndIndex: Integer;
begin
  Result := True;

  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    S := SS[i];

    StartLineIndex := I;
    StartIndex := StrIPos(cSubString, S) + Length(cSubString);

    Result := DetermineEnd(SS, '</pre></div></div>', StartLineIndex, StartIndex, EndLineIndex, EndIndex);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine end at line %d', [i]);
      Exit;
    end;

    RemoveLinks(SS, StartLineIndex, StartIndex, EndLineIndex, EndIndex);

    { !! +1 is eigelijk fout }
    I := LocateSubString(SS, EndLineIndex + 1, cSubString);
  end;
end;

procedure DetermineLinks(const S: string; const StartIndex: Integer;
  out CompleteLink, StrippedLink: string);
const
  // <a href="TJvTextAttributes.html">TJvTextAttributes</a>
  cLinkLength = Length('<a href=');
var
  EndIndex: Integer;
  L: Integer;
  State: Integer;
  BeginTextIndex: Integer;
begin
  EndIndex := StartIndex;
  L := Length(S);
  State := 0;
  BeginTextIndex := -1;
  while EndIndex <= L do
  begin
    case State of
      0:
        if S[EndIndex] = '>' then
        begin
          BeginTextIndex := EndIndex + 1;
          Inc(State);
        end;
      1:
        if S[EndIndex] = '<' then
        begin
          SetString(StrippedLink, PChar(S) + BeginTextIndex - 1, EndIndex - BeginTextIndex);
          Inc(State);
        end;
      2:
        if S[EndIndex] = '>' then
        begin
          SetString(CompleteLink, PChar(S) + StartIndex - 1, EndIndex - StartIndex + 1);
          Exit;
        end;
    end;

    Inc(EndIndex);
  end;
  raise Exception.Create('DetermineLink: Count <> 0');
end;

function RemoveLink(const S: string): string;
// example: <a href="TJvTextAttributes.html">TJvTextAttributes</a>
var
  StartIndex: Integer;
  CompleteLink, StrippedLink: string;
begin
  Result := S;
  StartIndex := StrIPos('<a href=', Result);
  while StartIndex > 0 do
  begin
    DetermineLinks(Result, StartIndex, CompleteLink, StrippedLink);
    Result := ChangeSubString(Result, CompleteLink, StrippedLink);

    StartIndex := StrIPos('<a href=', Result);
  end;
end;

function THtmlHelpPostProcessor.DetermineEnd(SS: TStrings;
  const EndStr: string;
  const StartLineIndex, StartIndex: Integer;
  out EndLineIndex, EndIndex: Integer): Boolean;
var
  P: PChar;
  LineIndex: Integer;
begin
  LineIndex := StartLineIndex;

  P := PChar(SS[LineIndex]) + StartIndex - 1;

  while LineIndex < SS.Count do
  begin
    P := StrPos(P, PAnsiChar(EndStr));
    if Assigned(P) then
    begin
      Result := True;
      EndLineIndex := LineIndex;
      EndIndex := P - PChar(SS[LineIndex]) + 1;
      Exit;
    end;

    Inc(LineIndex);
    P := PChar(SS[LineIndex]);
  end;

  Result := False;
end;

procedure THtmlHelpPostProcessor.RemoveLinks(SS: TStrings;
  const StartLineIndex, StartIndex, EndLineIndex, EndIndex: Integer);
var
  I: Integer;
  S: string;
begin
  try
    S := SS[StartLineIndex];
    SS[StartLineIndex] := Copy(S, 1, StartIndex - 1) + RemoveLink(Copy(S, StartIndex, MaxInt));

    for I := StartLineIndex + 1 to EndLineIndex - 1 do
      SS[i] := RemoveLink(SS[i]);

    if EndLineIndex > StartLineIndex then
    begin
      S := SS[EndLineIndex];
      SS[EndLineIndex] := RemoveLink(Copy(S, 1, EndIndex)) + Copy(S, EndIndex + 1, MaxInt);
    end;
  except
    ErrorMsgFmt('Index=%d', [StartLineIndex]);
    raise;
  end;
end;

{ TTaskProgress }

//procedure TTaskProgress.Change(const DeltaPosition, DeltaMax: Integer);
//begin
//  FPosition := FPosition + DeltaPosition;
//  FMax := FMax + DeltaMax;
//end;

procedure TTaskProgress.ItemChanged(const ItemPosition, ItemMax: Integer);
begin
  FPosition := ItemPosition + FIndex * ItemMax;
  FMax := FCount * ItemMax;
  DoProgressChanged;
end;

procedure TTaskProgress.ItemDone;
begin
  FIndex := FIndex + 1;
  ItemChanged(0, 100);
end;

procedure TTaskProgress.DoProgressChanged;
begin
  if Assigned(FOnProgressChanged) then
    FOnProgressChanged(Self);
end;

{ TTaskItemProgress }

procedure TTaskItemProgress.Done;
begin
  FTaskProgress.ItemDone;
end;

procedure TTaskItemProgress.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    FTaskProgress.ItemChanged(FPosition, FMax);
  end;
end;

procedure TTaskItemProgress.SetPosition(const Value: Integer);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    FTaskProgress.ItemChanged(FPosition, FMax);
  end;
end;

procedure THtmlHelpPostProcessor.HandleProgressChanged(Sender: TObject);
begin
  Progress(FTaskProgress.Position, FTaskProgress.Max);
end;

procedure TTaskItemProgress.SetWeight(const Value: Integer);
begin
  if (Value >= 0) and (Value <> FWeight) then
  begin
    FWeight := Value;
  end;
end;

function TTaskProgress.GetItem: TTaskItemProgress;
begin
  if not Assigned(FSubItem) then
  begin
    FSubItem := TTaskItemProgress.Create;
    FSubItem.FTaskProgress := Self;
  end;
  Result := FSubItem;
end;

destructor TTaskProgress.Destroy;
begin
  FSubItem.Free;
  inherited Destroy;
end;

end.
