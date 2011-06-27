unit JVCLConvertUtils;

interface

uses
  SysUtils, Classes, JvPropertyStore, Grids;

type
  TAppOptions = class(TJvCustomPropertyStore)
  private
    FBackup: Boolean;
    FDefinitionFile: string;
    FFileMask: string;
    FFileMasks: string;
    FFilename: string;
    FIgnoreInsideStrings: Boolean;
    FIgnoreInsideComments: Boolean;
    FReplaceFilenames: Boolean;
    FRootDirectory: string;
    FSimulate: Boolean;
    FWholeWords: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Backup: Boolean read FBackup write FBackup default True;
    property DefinitionFile: string read FDefinitionFile write FDefinitionFile;
    property FileMask: string read FFileMask write FFileMask;
    property FileMasks: string read FFileMasks write FFileMasks;
    property IgnoreInsideStrings: Boolean read FIgnoreInsideStrings write FIgnoreInsideStrings default True;
    property IgnoreInsideComments: Boolean read FIgnoreInsideComments write FIgnoreInsideComments default True;
    property ReplaceFilenames: Boolean read FReplaceFilenames write FReplaceFilenames default True;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property Simulate: Boolean read FSimulate write FSimulate default False;
    property WholeWords: Boolean read FWholeWords write FWholeWords default True;
  end;

  tBooleanDefinition = (bdDefault, bdTrue, bdFalse);

  TConvertDefinition = class(TJvCustomPropertyStore)
  private
    FIgnoreInsideComments: tBooleanDefinition;
    FIgnoreInsideStrings: tBooleanDefinition;
    FReplaceFrom: string;
    FReplaceTo: string;
    FWholeWord: tBooleanDefinition;
    function GetWholeWordString: string;
    function GetIgnoreInsideStringsString: string;
    function GetIgnoreInsideCommentsString: string;
    procedure SetWholeWordString(const Value: string);
    procedure SetIgnoreInsideStringsString(const Value: string);
    procedure SetIgnoreInsideCommentsString(const Value: string);
    procedure SetReplaceFrom(const Value: string);
    procedure SetReplaceTo(const Value: string);
  protected
    function ToBooleanDefinition(aValue: string): tBooleanDefinition;
    function ToString(aValue: tBooleanDefinition): string;
  public
    constructor Create(AOwner: TComponent); override;
    function ToBoolean(aBooleanDefinition: tBooleanDefinition; aDefault: Boolean): Boolean;
    property WholeWordString: string read GetWholeWordString write SetWholeWordString;
    property IgnoreInsideStringsString: string read GetIgnoreInsideStringsString write SetIgnoreInsideStringsString;
    property IgnoreInsideCommentsString: string read GetIgnoreInsideCommentsString write SetIgnoreInsideCommentsString;
  published
    property IgnoreInsideComments: tBooleanDefinition read FIgnoreInsideComments write FIgnoreInsideComments default bdDefault;
    property IgnoreInsideStrings: tBooleanDefinition read FIgnoreInsideStrings write FIgnoreInsideStrings default bdDefault;
    property ReplaceFrom: string read FReplaceFrom write SetReplaceFrom;
    property ReplaceTo: string read FReplaceTo write SetReplaceTo;
    property WholeWord: tBooleanDefinition read FWholeWord write FWholeWord default bdDefault;
  end;

  TConvertDefinitionList = class(TJvCustomPropertyListStore)
  private
    function GetDefinition(Index: Integer): TConvertDefinition;
  protected
    function CreateObject: TPersistent; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Add(aReplaceFrom, aReplaceTo: string; aWholeWord: tBooleanDefinition = bdDefault; aIgnoreStrings: tBooleanDefinition =
      bdDefault; aIgnoreComments: tBooleanDefinition = bdDefault): TConvertDefinition; overload;
    function Add(aReplaceFrom, aReplaceTo, aWholeWord, aIgnoreStrings, aIgnoreComments: string): TConvertDefinition; overload;
    procedure FillFromStringGrid(aStringGrid: TStringGrid);
    procedure FillToStringGrid(aStringGrid: TStringGrid);
    procedure FillFromStringList(aStringList: TStrings);
    procedure FillToStringList(aStringList: TStrings);
    property Definition[Index: Integer]: TConvertDefinition read GetDefinition; default;
  end;

implementation

{ TAppOptions }

constructor TAppOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootDirectory := '';
  FileMask := '*.dpr;*.dpk;*.pas;*.dfm';
  DefinitionFile := '';
  Backup := True;
  WholeWords := True;
  ReplaceFilenames := True;
  Simulate := False;
  FileMasks :=
    'Delphi files (*.dpr;*.dpk;*.pas;*.dfm)'#27'BCB files (*.dpr;*.bpk;*.pas;*.dfm;*.cpp;*.h;*.hpp)'#27'All files (*.*)';
  FIgnoreInsideComments := True;
  FIgnoreInsideStrings := True;
end;

constructor TConvertDefinition.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWholeWord := bdDefault;
  FIgnoreInsideComments := bdDefault;
  FIgnoreInsideStrings := bdDefault;
  DeleteBeforeStore := True;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('ItemName');
end;

function TConvertDefinition.GetWholeWordString: string;
begin
  Result := ToString(WholeWord);;
end;

function TConvertDefinition.GetIgnoreInsideStringsString: string;
begin
  Result := ToString(IgnoreInsideStrings);;
end;

function TConvertDefinition.GetIgnoreInsideCommentsString: string;
begin
  Result := ToString(IgnoreInsideComments);;
end;

procedure TConvertDefinition.SetWholeWordString(const Value: string);
begin
  WholeWord := ToBooleanDefinition(Value);
end;

procedure TConvertDefinition.SetIgnoreInsideStringsString(const Value: string);
begin
  IgnoreInsideStrings := ToBooleanDefinition(Value);
end;

procedure TConvertDefinition.SetIgnoreInsideCommentsString(const Value: string);
begin
  IgnoreInsideComments := ToBooleanDefinition(Value);
end;

procedure TConvertDefinition.SetReplaceFrom(const Value: string);
begin
  FReplaceFrom := trim(Value);
end;

procedure TConvertDefinition.SetReplaceTo(const Value: string);
begin
  FReplaceTo := trim(Value);
end;

function TConvertDefinition.ToBoolean(aBooleanDefinition: tBooleanDefinition; aDefault: Boolean): Boolean;
begin
  Result := (aBooleanDefinition = bdTrue) or ((aBooleanDefinition = bdDefault) and aDefault);
end;

function TConvertDefinition.ToBooleanDefinition(aValue: string): tBooleanDefinition;
begin
  aValue := trim(uppercase(aValue));
  if (aValue = 'Y') or (aValue = 'YES') or (aValue = 'T') or (aValue = 'TRUE') then
    Result := bdTrue
  else
    if (aValue = 'N') or (aValue = 'NO') or (aValue = 'F') or (aValue = 'FALSE') then
      Result := bdFalse
    else
      Result := bdDefault;
end;

function TConvertDefinition.ToString(aValue: tBooleanDefinition): string;
begin
  if aValue = bdTrue then
    Result := 'y'
  else
    if aValue = bdFalse then
      Result := 'n'
    else
      Result := '';
end;

// === { TConvertDefinitionList } ===========================================

constructor TConvertDefinitionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DeleteBeforeStore := True;
  ItemName := 'Definition';
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('ItemName');
end;

function TConvertDefinitionList.Add(aReplaceFrom, aReplaceTo, aWholeWord, aIgnoreStrings, aIgnoreComments: string):
  TConvertDefinition;
begin
  Result := nil;
  if trim(aReplaceFrom) = '' then
    Exit;
  Result := TConvertDefinition(CreateAddObject(''));
  Result.ReplaceFrom := aReplaceFrom;
  Result.ReplaceTo := aReplaceTo;
  Result.WholeWordString := aWholeWord;
  Result.IgnoreInsideCommentsString := aIgnoreComments;
  Result.IgnoreInsideStringsString := aIgnoreStrings;
end;

function TConvertDefinitionList.Add(aReplaceFrom, aReplaceTo: string; aWholeWord: tBooleanDefinition = bdDefault; aIgnoreStrings:
  tBooleanDefinition = bdDefault; aIgnoreComments: tBooleanDefinition = bdDefault): TConvertDefinition;
begin
  Result := nil;
  if trim(aReplaceFrom) = '' then
    Exit;
  Result := TConvertDefinition(CreateAddObject(''));
  Result.ReplaceFrom := aReplaceFrom;
  Result.ReplaceTo := aReplaceTo;
  Result.WholeWord := aWholeWord;
  Result.IgnoreInsideComments := aIgnoreComments;
  Result.IgnoreInsideStrings := aIgnoreStrings;
end;

function TConvertDefinitionList.CreateObject: TPersistent;
begin
  Result := TConvertDefinition.Create(Self);
end;

procedure TConvertDefinitionList.FillFromStringGrid(aStringGrid: TStringGrid);
var
  i: Integer;
begin
  if not Assigned(aStringGrid) then
    Exit;
  Clear;
  for i := 1 to aStringGrid.RowCount - 1 do
  begin
    Add(aStringGrid.Cells[1, i], aStringGrid.Cells[2, i], aStringGrid.Cells[3, i], aStringGrid.Cells[4, i],
      aStringGrid.Cells[5, i]);
  end;
end;

procedure TConvertDefinitionList.FillToStringGrid(aStringGrid: TStringGrid);
var
  i: Integer;
  Def: TConvertDefinition;
begin
  if not Assigned(aStringGrid) then
    Exit;
  aStringGrid.RowCount := 2;
  aStringGrid.Cells[0, 0] := 'Line';
  aStringGrid.Cells[1, 0] := 'From';
  aStringGrid.Cells[2, 0] := 'To';
  aStringGrid.Cells[3, 0] := 'Whole Word';
  aStringGrid.Cells[4, 0] := 'Ignore Strings';
  aStringGrid.Cells[5, 0] := 'Ignore Comments';
  aStringGrid.FixedRows := 1;
  if Count > 0 then
    aStringGrid.RowCount := Count + 1
  else
  begin
    i := 0;
    aStringGrid.RowHeights[i + 1] := 18;
    aStringGrid.Cells[0, i + 1] := Format('%5d', [i + 1]);;
    aStringGrid.Cells[1, i + 1] := '';
    aStringGrid.Cells[2, i + 1] := '';
    aStringGrid.Cells[3, i + 1] := '';
    aStringGrid.Cells[4, i + 1] := '';
    aStringGrid.Cells[5, i + 1] := '';
  end;
  ;
  for i := 0 to Count - 1 do
  begin
    Def := Definition[i];
    aStringGrid.RowHeights[i + 1] := 18;
    aStringGrid.Cells[0, i + 1] := Format('%5d', [i + 1]);;
    aStringGrid.Cells[1, i + 1] := Def.ReplaceFrom;
    aStringGrid.Cells[2, i + 1] := Def.ReplaceTo;
    aStringGrid.Cells[3, i + 1] := Def.WholeWordString;
    aStringGrid.Cells[4, i + 1] := Def.IgnoreInsideStringsString;
    aStringGrid.Cells[5, i + 1] := Def.IgnoreInsideCommentsString;
  end;
end;

procedure TConvertDefinitionList.FillFromStringList(aStringList: TStrings);
var
  i: Integer;
begin
  if not Assigned(aStringList) then
    Exit;
  Clear;
  for i := 0 to aStringList.Count - 1 do
    Add(aStringList.Names[i], aStringList.ValueFromIndex[i]);
end;

procedure TConvertDefinitionList.FillToStringList(aStringList: TStrings);
var
  i: Integer;
  Def: TConvertDefinition;
begin
  if not Assigned(aStringList) then
    Exit;
  aStringList.Clear;
  for i := 0 to Count - 1 do
  begin
    Def := Definition[i];
    aStringList.Add(Def.ReplaceFrom + '=' + Def.ReplaceTo);
  end;
end;

function TConvertDefinitionList.GetDefinition(Index: Integer): TConvertDefinition;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TConvertDefinition(Objects[Index])
  else
    Result := nil;
end;

end.
