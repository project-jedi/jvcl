unit Main_GenGroups;

interface

procedure ShowProgInfo;
procedure GenerateGroupFiles;
procedure WaitAndExit;

implementation

uses
  Windows, SysUtils, Classes, contnrs;

function Pad(const S: string; const Width: Integer; const PadChar: Char = ' '): string; forward;
procedure Wrap(var S: string; const Width, Indent, NextIndent: Integer); forward;

//--------------------------------------------------------------------------------------------------
// TOwnedStringList
//--------------------------------------------------------------------------------------------------

type
  TOwnedStringList = class(TStringList)
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

//--------------------------------------------------------------------------------------------------

procedure TOwnedStringList.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
  inherited Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TOwnedStringList.Delete(Index: Integer);
begin
  if Objects[Index] <> nil then
    Objects[Index].Free;
  inherited Delete(Index);
end;

//--------------------------------------------------------------------------------------------------
// Group and component helper routines
//--------------------------------------------------------------------------------------------------

procedure ParseGroupInfo(AInfoString: string; out FGroupID, FGroupTitle, FGroupText: string);
var
  I: Integer;
begin
  I := Pos('=', AInfoString);
  if I = 0 then
    raise Exception.Create('invalid group string: no title');
  FGroupID := Trim(Copy(AInfoString, 1, I - 1));
  Delete(AInfoString, 1, I);
  I := Pos('=', AInfoString);
  if I = 0 then
    raise Exception.Create('invalid group string: no description');
  FGroupTitle := Trim(Copy(AInfoString, 1, I - 1));
  Delete(AInfoString, 1, I);
  FGroupText := TrimLeft(AInfoString);
  I := Length(FGroupTitle);
  while (I > 0) do
  begin
    if (FGroupTitle[I] = ',') and ((I = 1) or (FGroupTitle[I - 1] <> '\')) then
      Insert('\', FGroupTitle, I);
    Dec(I);
  end;
  I := Length(FGroupText);
  while I > 0 do
  begin
    if (FGroupText[I] = '$') and ((I = 1) or (FGroupText[I - 1] <> '\')) then
    begin
      Delete(FGroupText, I, 1);
      Insert(#13#10, FGroupText, I);
    end
    else if FGroupText[I] = '$' then
      Delete(FGroupText, I - 1, 1);
    Dec(I);
  end;
end;

//--------------------------------------------------------------------------------------------------
// Group and component info classes
//--------------------------------------------------------------------------------------------------

type
  TComponentInfos = class;
  TComponentInfo = class;
  TGroupInfos = class;
  TGroupInfo = class;

  TComponentInfoFlag = (cifComponent, cifClass, cifRoutine, cifNotInParentList, cifIgnore,
    cifAssignedToGroup);
  TComponentInfoFlags = set of TComponentInfoFlag;

  TComponentInfos = class(TObject)
  private
    FList: TStrings;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TComponentInfo;
    function GetItemByName(Name: string): TComponentInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TComponentInfo;
    procedure Delete(Index: Integer);
    function IndexOf(const AName: string): Integer;
    procedure Remove(Comp: TComponentInfo);
    procedure RemoveInvalid;

    property Count: Integer read GetCount;
    property Items[I: Integer]: TComponentInfo read GetItems; default;
    property ItemByName[Name: string]: TComponentInfo read GetItemByName;
  end;

  TComponentInfo = class(TObject)
  private
    FName: string;
    FImage: string;
    FSummary: string;
    FFlags: TComponentInfoFlags;
  protected
    function GetValid: Boolean;
  public
    constructor Create(const AName: string);

    property Name: string read FName;
    property Image: string read FImage write FImage;
    property Summary: string read FSummary write FSummary;
    property Flags: TComponentInfoFlags read FFlags write FFlags;
    property Valid: Boolean read GetValid;
  end;

  TGroupInfos = class(TObject)
  private
    FList: TObjectList;
    FOwner: TGroupInfo;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TGroupInfo;

    property Owner: TGroupInfo read FOwner;
  public
    constructor Create(AOwner: TGroupInfo);
    destructor Destroy; override;

    function Add(const AInfoString: string): TGroupInfo;
    procedure AppendGroupsToFile;
    function Locate(AGroupID: string): TGroupInfo;
    procedure MergeComps;
    procedure RemoveUnknownComponents;

    property Count: Integer read GetCount;
    property Items[I: Integer]: TGroupInfo read GetItems; default;
  end;

  TGroupInfo = class(TObject)
  private
    FComponents: TStrings;
    FGroupID: string;
    FGroupTitle: string;
    FGroupText: string;
    FParent: TGroupInfo;
    FSubGroups: TGroupInfos;
  protected
    function CountComp: Integer;
    function CountClass: Integer;
    function CountRoutines: Integer;
  public
    constructor Create(const AParent: TGroupInfo; const AInfoString: string);
    destructor Destroy; override;

    procedure AddComponentList;
    procedure AddClassList;
    procedure AddRoutineList;
    procedure AddDescription;
    procedure AppendToFile(const TopicOrder: Integer);
    function ParentGroupID(const WantFuncRef: Boolean = False): string;
    procedure MergeChildLists;

    property Components: TStrings read FComponents;
    property GroupID: string read FGroupID;
    property GroupTitle: string read FGroupTitle;
    property GroupText: string read FGroupText;
    property Parent: TGroupInfo read FParent;
    property SubGroups: TGroupInfos read FSubGroups;
  end;

//--------------------------------------------------------------------------------------------------
// Internal variables
//--------------------------------------------------------------------------------------------------

var
  HelpPath: string;
  hOutput: THandle;
  CursorHideCount: Integer;
  FileSL: TStrings;               // Temporary string list containing the definition file and output files.
  CurList: TStack;
  GroupInfo: TGroupInfos;         // Group tree, complete with group info
  ComponentList: TComponentInfos; // List of components, classes and routines
  Hints: Integer;                 // Number of hints
  Warnings: Integer;              // Number of warnings
  InvComps: string;               // List of eliminated components, classes and routines

const
  cifSymbolType = [cifComponent, cifClass, cifRoutine];

//--------------------------------------------------------------------------------------------------
// TComponentInfos
//--------------------------------------------------------------------------------------------------

function TComponentInfos.GetCount: Integer;
begin
  Result := FList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TComponentInfos.GetItems(I: Integer): TComponentInfo;
begin
  Result := TComponentInfo(FList.Objects[I]);
end;

//--------------------------------------------------------------------------------------------------

function TComponentInfos.GetItemByName(Name: string): TComponentInfo;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I > -1 then
    Result := Items[I]
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

constructor TComponentInfos.Create;
begin
  inherited Create;
  FList := TOwnedStringList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TComponentInfos.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TComponentInfos.Add(const AName: string): TComponentInfo;
begin
  if FList.IndexOf(AName) < 0 then
    FList.AddObject(AName, TComponentInfo.Create(AName));
  Result := TComponentInfo(FList.Objects[FList.IndexOf(AName)])
end;

//--------------------------------------------------------------------------------------------------

procedure TComponentInfos.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

//--------------------------------------------------------------------------------------------------

function TComponentInfos.IndexOf(const AName: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(Items[Result].Name, AName) do
    Dec(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TComponentInfos.Remove(Comp: TComponentInfo);
begin
  if FList.IndexOfObject(Comp) > -1 then
    Delete(FList.IndexOfObject(Comp));
end;

//--------------------------------------------------------------------------------------------------

procedure TComponentInfos.RemoveInvalid;
var
  I: Integer;
  Prefix: string;
begin
  I := Count - 1;
  Prefix := '';
  InvComps := '';
  while (I >= 0) do
  begin
    if not Items[I].Valid then
    begin
      InvComps := InvComps + Prefix + Items[I].Name;
      Prefix := ', ';
      Delete(I);
    end;
    Dec(I);
  end;
end;

//--------------------------------------------------------------------------------------------------
// TComponentInfo
//--------------------------------------------------------------------------------------------------

function TComponentInfo.GetValid: Boolean;
begin
  Result :=
    (cifAssignedToGroup in Flags) and (Trim(FSummary) <> '') and (
      (
        ((cifComponent in Flags) and (Trim(FImage) <> '')) or
        (cifClass in Flags) or
        (cifRoutine in Flags)
      )
    );
end;

//--------------------------------------------------------------------------------------------------

constructor TComponentInfo.Create(const AName: string);
begin
  inherited Create;
  FName := Trim(AName);
  FImage := '';
  FSummary := '';
end;

//--------------------------------------------------------------------------------------------------
// TGroupInfos
//--------------------------------------------------------------------------------------------------

function TGroupInfos.GetCount: Integer;
begin
  Result := FList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfos.GetItems(I: Integer): TGroupInfo;
begin
  Result := TGroupInfo(FList[I]);
end;

//--------------------------------------------------------------------------------------------------

constructor TGroupInfos.Create(AOwner: TGroupInfo);
begin
  inherited Create;
  FList := TObjectList.Create(True);
  FOwner := AOwner;
end;

//--------------------------------------------------------------------------------------------------

destructor TGroupInfos.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfos.Add(const AInfoString: string): TGroupInfo;
begin
  Result := TGroupInfo.Create(Owner, AInfoString);
  if Locate(Result.GroupID) <> nil then
  begin
    WriteLn(Format('duplicate group ID "%s"', [Result.GroupID]));
    FreeAndNil(Result);
  end
  else
    FList.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfos.AppendGroupsToFile;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AppendToFile(-Count + I);
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfos.Locate(AGroupID: string): TGroupInfo;
var
  IDot: Integer;
  I: Integer;
begin
  IDot := Pos('.', AGroupID);
  if IDot = 0 then
    IDot := Length(AGroupID) + 1;
  I := Count - 1;
  while (I >= 0) and not AnsiSameText(Items[I].GroupID, Copy(AGroupID, 1, IDot - 1)) do
    Dec(I);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
  if (IDot < Length(AGroupID)) and (Result <> nil) then
    Result := Result.SubGroups.Locate(Copy(AGroupID, IDot + 1, Length(AGroupID) - IDot));
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfos.MergeComps;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MergeChildLists;
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfos.RemoveUnknownComponents;
var
  I: Integer;
  CompList: TStrings;
  J: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    CompList := Items[I].Components;
    J := CompList.Count - 1;
    while J >= 0 do
    begin
      if ComponentList.IndexOf(CompList[J]) < 0 then
        CompList.Delete(J);
      Dec(J);
    end;
    Items[I].SubGroups.RemoveUnknownComponents;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TGroupInfo
//--------------------------------------------------------------------------------------------------

function TGroupInfo.CountComp: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifComponent in ComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfo.CountClass: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifClass in ComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfo.CountRoutines: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifRoutine in ComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

constructor TGroupInfo.Create(const AParent: TGroupInfo; const AInfoString: string);
begin
  inherited Create;
  FComponents := TStringList.Create;
  FSubGroups := TGroupInfos.Create(Self);
  FParent := AParent;
  ParseGroupInfo(AInfoString, FGroupID, FGroupTitle, FGroupText);
end;

//--------------------------------------------------------------------------------------------------

destructor TGroupInfo.Destroy;
begin
  FSubGroups.Free;
  FComponents.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.AddComponentList;
var
  Col1Length: Integer;
  I: Integer;
  Comp: TComponentInfo;
  S: string;
begin
  if CountComp = 0 then
    Exit;
  FileSL.Add('  <TABLE>');
  Col1Length := 9;
  for I := Components.Count - 1 downto 0 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if (cifComponent in Comp.Flags) and
        ((Length(Comp.Name) + Length(Comp.Image) + 8) > Col1Length) then
      Col1Length := Length(Comp.Name) + Length(Comp.Image) + 8
  end;
  FileSL.Add('    ' + Pad('Component', Col1Length + 2, ' ') + 'Description');
  FileSL.Add('    ' + StringOfChar('-', Col1Length) + '  ' + StringOfChar('-', 94 - Col1Length));
  for I := 0 to Components.Count - 1 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if cifComponent in Comp.Flags then
    begin
      S := Comp.Summary;
      Wrap(S, 100, 6 + Col1Length, 2);
      S := '    ' + Pad('<IMAGE ' + Comp.Image + '>' + Comp.Name, Col1Length + 2, ' ') +
        Copy(S, Col1Length + 7, Length(S));
      FileSL.Add(S);
    end;
  end;
  FileSL.Add('  </TABLE>');
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.AddClassList;
var
  S: string;
  Col1Length: Integer;
  I: Integer;
  Comp: TComponentInfo;
begin
  if CountClass = 0 then
    Exit;
  if CountComp > 0 then
    S := #13#10#13#10 + 'In addition to these components, the following classes belong to this group:' + #13#10
  else
    S := 'This group contains the following classes:';
  Wrap(S, 100, 2, 0);
  FileSL.Add(S);
  FileSL.Add('  <TABLE>');
  Col1Length := 5;
  for I := Components.Count - 1 downto 0 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if (cifClass in Comp.Flags) and (Length(Comp.Name) > Col1Length) then
      Col1Length := Length(Comp.Name);
  end;
  FileSL.Add('    ' + Pad('Class', Col1Length + 2, ' ') + 'Description');
  FileSL.Add('    ' + StringOfChar('-', Col1Length) + '  ' + StringOfChar('-', 94 - Col1Length));
  for I := 0 to Components.Count - 1 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if cifClass in Comp.Flags then
    begin
      S := Comp.Summary;
      Wrap(S, 100, 6 + Col1Length, 2);
      S := '    ' + Pad(Comp.Name, Col1Length + 2, ' ') + Copy(S, Col1Length + 7, Length(S));
      FileSL.Add(S);
    end;
  end;
  FileSL.Add('  </TABLE>');
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.AddRoutineList;
var
  S: string;
  Col1Length: Integer;
  I: Integer;
  Comp: TComponentInfo;
begin
  if CountRoutines = 0 then
    Exit;
  if CountComp > 0 then
  begin
    if CountClass > 0 then
      S := #13#10#13#10 + 'In addition to these components and classes, the following routines belong to this group:' + #13#10
    else
      S := #13#10#13#10 + 'In addition to these components, the following routines belong to this group:' + #13#10;
  end
  else if CountClass > 0 then
    S := #13#10#13#10 + 'In addition to these classes, the following routines belong to this group:' + #13#10
  else
    S := 'This group contains the following classes:';
  Wrap(S, 100, 2, 0);
  FileSL.Add(S);
  FileSL.Add('  <TABLE>');
  Col1Length := 7;
  for I := Components.Count - 1 downto 0 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if (cifRoutine in Comp.Flags) and (Length(Comp.Name) > Col1Length) then
      Col1Length := Length(Comp.Name);
  end;
  FileSL.Add('    ' + Pad('Routine', Col1Length + 2, ' ') + 'Description');
  FileSL.Add('    ' + StringOfChar('-', Col1Length) + '  ' + StringOfChar('-', 94 - Col1Length));
  for I := 0 to Components.Count - 1 do
  begin
    Comp := ComponentList[ComponentList.IndexOf(Components[I])];
    if cifRoutine in Comp.Flags then
    begin
      S := Comp.Summary;
      Wrap(S, 100, 6 + Col1Length, 2);
      S := '    ' + Pad(Comp.Name, Col1Length + 2, ' ') + Copy(S, Col1Length + 7, Length(S));
      FileSL.Add(S);
    end;
  end;
  FileSL.Add('  </TABLE>');
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.AddDescription;
var
  S: string;
begin
  FileSL.Add('Description');
  S := GroupText + #13#10#13#10;
  if Components.Count > 0 then
  begin
    if CountComp > 0 then
      S := S + 'This group contains the following components and controls:' + #13#10;
  end
  else
    S := S + 'This group is currently empty.';
  Wrap(S, 100, 2, 0);
  FileSL.Add(S);
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.AppendToFile(const TopicOrder: Integer);
begin
  FileSL.Add('@@$' + ParentGroupID + '.' + GroupID);
  FileSL.Add('<GROUP $' + ParentGroupID(Parent = nil) + '>');
  FileSL.Add('<TOPICORDER ' + IntToStr(TopicOrder) + '>');
  FileSL.Add('<TITLE ' + GroupTitle + '>');
  AddDescription;
  AddComponentList;
  AddClassList;
  AddRoutineList;
  FileSL.Add(StringOfChar('-', 100));
  SubGroups.AppendGroupsToFile;
end;

//--------------------------------------------------------------------------------------------------

function TGroupInfo.ParentGroupID(const WantFuncRef: Boolean): string;
begin
  if FParent = nil then
  begin
    Result := 'JVCL';
    if WantFuncRef then
      Result := Result + '.FuncRef';
  end
  else
    Result := Parent.ParentGroupID(WantFuncRef) + '.' + Parent.GroupID;
end;

//--------------------------------------------------------------------------------------------------

procedure TGroupInfo.MergeChildLists;
var
  SL: TStrings;
  I: Integer;
  J: Integer;
begin
  // Merge sub groups first
  SubGroups.MergeComps;
  // Add components from sub groups
  SL := TStringList.Create;
  try
    for I := 0 to SubGroups.Count -1 do
    begin
      SL.Assign(SubGroups[I].Components);
      // Remove items we don't want in the parent list
      for J := SL.Count - 1 downto 0 do
        if cifNotInParentList in ComponentList.ItemByName[SL[J]].Flags then
          SL.Delete(J);
      Components.AddStrings(SL);
    end
  finally
    SL.Free;
  end;
  // Sort the component list
  TStringList(Components).Sort;
end;

//--------------------------------------------------------------------------------------------------
// Internal procedures
//--------------------------------------------------------------------------------------------------

function IndexOfText(Str: string; Strings: array of string): Integer;
begin
  Result := High(Strings);
  while (Result >= 0) and not AnsiSameText(Str, Strings[Result]) do
    Dec(Result);
end;

procedure HideCursor;
var
  CursorInfo: TConsoleCursorInfo;
begin
  if not GetConsoleCursorInfo(hOutput, CursorInfo) then
    raise Exception.Create('Error retrieving cursor info.');
  CursorInfo.bVisible := False;
  if not SetConsoleCursorInfo(hOutput, CursorInfo) then
    raise Exception.Create('Error hiding cursor.');
  Inc(CursorHideCount);
end;

//--------------------------------------------------------------------------------------------------

procedure ShowCursor;
var
  CursorInfo: TConsoleCursorInfo;
begin
  if CursorHideCount > 0 then
  begin
    Dec(CursorHideCount);
    if CursorHideCount = 0 then
    begin
      if not GetConsoleCursorInfo(hOutput, CursorInfo) then
        raise Exception.Create('Error retrieving cursor info.');
      CursorInfo.bVisible := True;
      if not SetConsoleCursorInfo(hOutput, CursorInfo) then
        raise Exception.Create('Error showing cursor.');
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetCursorPos: TCoord;
var
  ConsoleInfo: TConsoleScreenBufferInfo;
begin
  if not GetConsoleScreenBufferInfo(hOutput, ConsoleInfo) then
    raise Exception.Create('Error retrieving console screen buffer info.');
  Result := ConsoleInfo.dwCursorPosition;
end;

//--------------------------------------------------------------------------------------------------

procedure SetCursorPos(Pos: TCoord);
begin
  if not SetConsoleCursorPosition(hOutput, Pos) then
    raise Exception.Create('Error setting cursor position.');
end;

//--------------------------------------------------------------------------------------------------

procedure Initialize;
var
  I: Integer;
begin
  Write('Initialization...');
  HelpPath := ExtractFilePath(ParamStr(0));
  I := Length(HelpPath) - 1;
  while (I > 0) and (HelpPath[I] <> '\') do
    Dec(I);
  Delete(HelpPath, I + 1, Length(HelpPath) - I);
  hOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  CursorHideCount := 0;

  FileSL := TstringList.Create;
  CurList := TStack.Create;
  GroupInfo := TGroupInfos.Create(nil);
  ComponentList := TComponentInfos.Create;
  WriteLn(' done.');
  HideCursor;
end;

//--------------------------------------------------------------------------------------------------

procedure Finalize;
begin
  Write('Cleanup...');
  FreeAndNil(ComponentList);
  FreeAndNil(GroupInfo);
  FreeAndNil(CurList);
  FreeAndNil(FileSL);
  WriteLn(' done.');
  ShowCursor;
end;

//--------------------------------------------------------------------------------------------------

function Pad(const S: string; const Width: Integer; const PadChar: Char = ' '): string;
begin
  Result := S + StringOfChar(PadChar, Width - Length(S));
end;

//--------------------------------------------------------------------------------------------------

procedure Wrap(var S: string; const Width, Indent, NextIndent: Integer);
var
  SL: TStrings;
  ThisWidth: Integer;
  ThisIndent: Integer;
  I: Integer;
  J: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    ThisWidth := Width - Indent;
    ThisIndent := Indent;
    I := 0;
    while I < SL.Count do
    begin
      SL[I] := Trim(SL[I]);
      if Length(SL[I]) > ThisWidth then
      begin
        J := ThisWidth;
        while (J > 0) and (SL[I][J] > ' ') do
          Dec(J);
        if J = 0 then
        begin
          J := ThisWidth;
          while (J <= Length(SL[I])) and (SL[I][J] > ' ') do
            Inc(J);
        end;
      end
      else
        J := Length(SL[I]) + 1;
      if J < Length(SL[I]) then // Split the line
      begin
        SL.Insert(I + 1, Copy(SL[I], J + 1, Length(SL[I]) - J));
        SL[I] := Trim(Copy(SL[I], 1, J - 1));
      end;
      SL[I] := StringOfChar(' ', ThisIndent) + SL[I];
      Inc(I);
      if I = 1 then
      begin
        Inc(ThisIndent, NextIndent);
        Dec(ThisWidth, NextIndent);
      end;
    end;
    S := SL.Text;
    Delete(S, Length(S) - 1, 2);
  finally
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ReadTreeDef;
begin
  WriteLn(Format('Reading tree definition file...'+ #13#10 + '  file name = ''%s''',
    [HelpPath + 'generic\JVCL.Groups.tree']));
  FileSL.LoadFromFile(HelpPath + 'generic\JVCL.Groups.tree');
  WriteLn(Format('  done. %d nodes', [FileSL.Count]));
end;

//--------------------------------------------------------------------------------------------------

function ParseAsTreeItem(const Item: Integer): Boolean;
var
  S: string;
  I: Integer;
  ThisLevel: Integer;
  FoundBar: Boolean;
begin
  Result := True;
  S := FileSL[Item];
  Delete(S, 1, 3); // Ignore first three characters
  I := 2;
  ThisLevel := 0;
  FoundBar := False;
  while (I <= Length(S)) and ((S[I] = '|') or ((S[I] = ' ') and not FoundBar)) do
  begin
    FoundBar := S[I] = '|';
    Inc(ThisLevel);
    Inc(I, 4);
  end;
  Delete(S, 1, I - 2);
  if ThisLevel = Pred(CurList.Count) then
    // Add an object to the last added item.
    CurList.Push(TGroupInfos(CurList.Peek).Add(Trim(S)).SubGroups)
  else if ThisLevel > Pred(CurList.Count) then
  begin
    WriteLn('error during parsing; level error');
    Result := False;
    Exit;
  end
  else if ThisLevel < Pred(CurList.Count) then
  begin
    while ThisLevel < Pred(CurList.Count) do
      CurList.Pop;
    CurList.Push(TGroupInfos(CurList.Peek).Add(Trim(S)).SubGroups);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseAsTree;
var
  NamePos: TCoord;
  HasError: Boolean;
  I: Integer;
begin
  Write('Parsing the tree... ');
  NamePos := GetCursorPos;
  CurList.Push(GroupInfo);
  HasError := False;
  for I := 0 to Pred(FileSL.Count) do
  begin
    Write(I + 1);
    if not ParseAsTreeItem(I) then
    begin
      HasError := True;
      Break;
    end;
    SetCursorPos(NamePos);
  end;
  if not HasError then
  begin
    SetCursorPos(NamePos);
    WriteLn('done.' + StringOfChar(' ', 79 - NamePos.X - 5));
  end;
  FileSL.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseGroupString(const Comp: TComponentInfo; GroupString: string);
var
  I: Integer;
  ThisName: string;
  Grp: TGroupInfo;
begin
  GroupString := Trim(GroupString);
//  Delete(GroupString, Length(GroupString), 1);
  while GroupString <> '' do
  begin
    I := Pos(',', GroupString);
    if I = 0 then
      I := Length(GroupString) + 1;
    ThisName := Trim(Copy(GroupString, 1, I - 1));
    Delete(GroupString, 1, I);
    if Copy(ThisName, 1, 1) = '$' then
      Delete(ThisName, 1, 1);
    GroupString := Trim(GroupString);
    if AnsiSameText(Copy(ThisName, 1, 5), 'JVCL.') then
      Grp := GroupInfo.Locate(Copy(ThisName, 6, Length(ThisName) - 5))
    else
      Grp := nil;
    if Grp <> nil then
    begin
      if Grp.Components.IndexOf(Comp.Name) < 0 then
      begin
        Comp.Flags := Comp.Flags + [cifAssignedToGroup];
        Grp.Components.Add(Comp.Name);
      end
      else
      begin
        WriteLn('    ## hint: @@$', Comp.Name, ' already added to group', #13#10, '      "',
          ThisName, '"');
        Inc(Hints);
      end
    end
    else
    begin
      if AnsiSameText(Copy(ThisName, 1, 5), 'JVCL.') and not AnsiSameText(Copy(ThisName, 1, 10),
        'JVCL.Info.') then
      begin
        WriteLn('    ## warn: group "', ThisName, '" not found' + #13#10 +'      (@@$', Comp.Name,
          ')');
        Inc(Warnings);
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseCompSettings(AComp: TComponentInfo; SettingsStr: string);
var
  I: Integer;
begin
  while SettingsStr <> '' do
  begin
    I := 1;
    while (I <= Length(SettingsStr)) and not (SettingsStr[I] in [',', ';']) do
      Inc(I);
    case IndexOfText(Trim(Copy(SettingsStr, 1, I - 1)), ['Component', 'Class', 'Routine', 'NotInParentList']) of
      0:
        AComp.Flags := AComp.Flags + [cifComponent] - [cifClass, cifRoutine];
      1:
        AComp.Flags := AComp.Flags + [cifClass] - [cifComponent, cifRoutine];
      2:
        AComp.Flags := AComp.Flags + [cifRoutine] - [cifComponent, cifClass];
      3:
        AComp.Flags := AComp.Flags + [cifNotInParentList];
    else
      begin
        WriteLn('    ## warn: Unknown setting "', Trim(Copy(SettingsStr, 1, I - 1)), '" found.' +
          #13#10 +'      (@@$', AComp.Name, ')');
        Inc(Warnings);
      end;
    end;
    Delete(SettingsStr, 1, I);
    SettingsStr := Trim(SettingsStr);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckPASTopic(const Index: Integer);
var
  MaxI: Integer;
  I: Integer;
begin
  MaxI := Index + 1;
  while (MaxI < FileSL.Count) and (Copy(Trim(FileSL[MaxI]), 1, 2) <> '@@') do
    Inc(MaxI);
  I := Index + 1;
  while I < MaxI do
  begin
    if AnsiSameText(Trim(FileSL[I]), '<GROUP $JVCL.FileRef>') then
      Exit;
    Inc(I);
  end;
  WriteLn('    ## warn: File topic not added to File Reference' +
    #13#10 +'      (', Trim(FileSL[Index]), ')');
  Inc(Warnings);
end;

//--------------------------------------------------------------------------------------------------

procedure AddComponent(const Index: Integer);
var
  CompName: string;
  MaxI: Integer;
  I: Integer;
  Comp: TComponentInfo;

  procedure RetrieveSummary;
  begin
    Comp.Summary := '';
    Inc(I);
    while (I < MaxI) and (Copy(FileSL[I], 1, 1) = ' ') do
    begin
      Comp.Summary := Comp.Summary + ' ' + Trim(FileSL[I]);
      Inc(I);
    end;
    Dec(I);
    Comp.Summary := Trim(Comp.Summary);
    if Comp.Summary = '' then
      Comp.Summary := '(no summary)';
  end;

begin
  CompName := Trim(Copy(Trim(FileSL[Index]), 3, Length(Trim(FileSL[Index])) - 2));
  if (CompName = '') or (Pos('.', CompName) > 0) then
    Exit;
  MaxI := Index + 1;
  while (MaxI < FileSL.Count) and (Copy(Trim(FileSL[MaxI]), 1, 2) <> '@@') do
    Inc(MaxI);
  I := Index + 1;
  Comp := ComponentList.Add(CompName);
  Comp.Flags := [];
  Comp.Summary := '(no summary)';
  comp.Image := 'NO_ICON';
  while (I < MaxI) do
  begin
    if AnsiSameText(Copy(Trim(FileSL[I]), 1, 6), 'GROUP=') then
      ParseGroupString(Comp, Copy(Trim(FileSL[I]), 7, Length(Trim(FileSL[I])) - 6));
    Inc(I);
  end;
  I := Index + 1;
  while I < MaxI do
  begin
    if AnsiSameText(Trim(FileSL[I]), 'Summary') then
      RetrieveSummary
    else if AnsiSameText(Copy(Trim(FileSL[I]), 1, 10), '<TITLEIMG ') then
      Comp.Image := Trim(Copy(Trim(FileSL[I]), 11, Length(Trim(FileSL[I])) - 11))
    else if AnsiSameText(Copy(Trim(FileSL[I]), 1, 5), 'FLAG=') then
      ParseCompSettings(Comp, Trim(Copy(Trim(FileSL[I]), 6, Length(Trim(FileSL[I])) - 5)));
    Inc(I);
  end;
  if (cifIgnore in Comp.Flags) or (Comp.Flags = []) then
  begin
    ComponentList.Remove(Comp);
    Exit;
  end;
  if (cifSymbolType * Comp.Flags = []) and (cifAssignedToGroup in Comp.Flags) then
    Comp.Flags := Comp.Flags + [cifComponent];
end;

//--------------------------------------------------------------------------------------------------

procedure ScanFile(const FileName: string);
var
  I: Integer;
  S: string;
begin
  FileSL.LoadFromFile(FileName);
  I := 0;
  while I < FileSL.Count do
  begin
    if Copy(Trim(FileSL[I]), 1, 2) = '@@' then
    begin
      S := Trim(Copy(Trim(FileSL[I]), 3, Length(Trim(FileSL[I])) - 2));
{      if AnsiSameText(Copy(S, Length(S) - 3, 4), '.pas') then
        CheckPASTopic(I)
      else}
        AddComponent(I);
    end;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure IterateFiles;
var
  FList: TStrings;
  Res: Integer;
  SR: TSearchRec;

  function ValueOf(Index: Integer): string;
  var
    I: Integer;
  begin
    I := Pos('=', FileSL[Index]);
    if I = 0 then
      Result := ''
    else
      Result := Copy(FileSL[Index], I + 1, Length(FileSL[Index]) - I);
  end;

  procedure ReadDOX;
  var
    I: Integer;
    S: string;
  begin
    WriteLn('Scanning .dox...');
    I := StrToInt(ValueOf(Res + 1));
    Inc(Res, 2);
    while I > 0 do
    begin
      S := ValueOf(Res);
      if not AnsiSameText(Copy(ExtractFileName(S), 1, 5), 'JVCL.') and
          AnsiSameText(Copy(ExtractFileName(S), 1, 2), 'Jv') then
        FList.Add(HelpPath + S);
      Dec(I);
      Inc(Res);
    end;
    WriteLn('  done.');
  end;

begin
  FList := TStringList.Create;
  try
    FileSL.LoadFromFile(HelpPath + 'JVCL3.dox');
    Res := FileSL.IndexOf('[Description Files]');
    if Res = -1 then
    begin
      WriteLn('Scanning directory...');
      Res := FindFirst(HelpPath + '*.dtx', faAnyFile - faDirectory, SR);
      try
        while Res = 0 do
        begin
          if not AnsiSameText(Copy(SR.FindData.cFileName, 1, 5), 'JVCL.') then
            FList.Add(HelpPath + SR.FindData.cFileName);
          Res := FindNext(SR);
        end;
      finally
        FindClose(SR);
      end;
      WriteLn('  done.');
    end
    else
      ReadDOX;
    TStringList(FList).Sort;
    WriteLn(Format('Scanning %d file(s)...', [FList.Count]));
    for Res := 0 to FList.Count - 1 do
    begin
      WriteLn('  Scanning ', ExtractFileName(FList[Res]) + '...');
      scanFile(FList[Res]);
    end;
    WriteLn(Format('  done.', [FList.Count]));
  finally
    FreeAndNil(FList);
    FileSL.Clear;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure CleanupComponents;
var
  OrgCount: Integer;
begin
  WriteLn('Removing invalid components...');
  OrgCount := ComponentList.Count;
  ComponentList.RemoveInvalid;
  GroupInfo.RemoveUnknownComponents;
  WriteLn(Format('  done. Kept %d of %d components.', [ComponentList.Count, OrgCount]));
  if ComponentList.Count <> OrgCount then
    WriteLn('Removed components: ', InvComps);
end;

//--------------------------------------------------------------------------------------------------

procedure MergeCompLists;
begin
  WriteLn('Merge component lists with parents...');
  GroupInfo.MergeComps;
  WriteLn('  done.');
end;

//--------------------------------------------------------------------------------------------------

procedure GenerateGroupIncludeFile;
begin
  WriteLn('Write group include file...');
  FileSL.Clear;
  FileSL.Add(StringOfChar('#', 100));
  FileSL.Add('## ' + Pad('JEDI-VCL help: Functional reference include file.', 94, ' ') + ' ##');
  FileSL.Add('## ' + Pad('Include file generated on ' +
    FormatDateTime('dd-mm-yyyy "at" hh:nn:ss', Now), 94, ' ') + ' ##');
  FileSL.Add(StringOfChar('#', 100));
  FileSL.Add('@@$JVCL.FuncRef');
  FileSL.Add('<GROUP $JVCL>');
  FileSL.Add('<TITLE Functional Reference>');
  FileSL.Add('Description');
  FileSL.Add('  JEDI-VCL consists of many components and controls for all sorts of things. To help you find the');
  FileSL.Add('  right component for a particular task, we have categorized the controls and components. Some');
  FileSL.Add('  components are in multiple categories if there was any overlap in functionality of the');
  FileSL.Add('  component.');
  FileSL.Add(StringOfChar('-', 100));
  GroupInfo.AppendGroupsToFile;
  FileSL.SaveToFile(HelpPath + 'generated includes\JVCL.FuncRef.dtx');
  WriteLn(Format('  done. %d hint(s), %d warning(s)', [Hints, Warnings]));
end;

//--------------------------------------------------------------------------------------------------
// Interfaced procedures
//--------------------------------------------------------------------------------------------------

procedure ShowProgInfo;
begin
  WriteLn('GenGroups: Generate help group files. Copyright Project JEDI 2002.');
  WriteLn('');
end;

//--------------------------------------------------------------------------------------------------

procedure GenerateGroupFiles;
begin
  Initialize;
  try
    Hints := 0;
    warnings := 0;
    ReadTreeDef;
    ParseAsTree;
    IterateFiles;
    CleanupComponents;
    MergeCompLists;
    GenerateGroupIncludeFile;
  finally
    Finalize;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure WaitAndExit;
begin
  WriteLn('Press enter to quit.');
  ReadLn;
end;

//--------------------------------------------------------------------------------------------------

end.
