{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJanTreeView.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQJanTreeView;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, ShellAPI,
  {$ENDIF MSWINDOWS}
  
  
  Types, QGraphics, QControls, QForms, QDialogs, QComCtrls, QMenus,
  QWindows;
  

type
  TGetVarEvent = procedure(Sender: TObject; VarName: string; var
    Value: Extended; var Found: Boolean) of object;

  TParseErrorEvent = procedure(Sender: TObject; ParseError: Integer)
    of object;

const
  ParserStackSize = 15;
  MaxFuncNameLen = 5;
  ExpLimit = 11356;
  SqrLimit = 1E2466;
  MaxExpLen = 4;
  TotalErrors = 7;
  ErrParserStack = 1;
  ErrBadRange = 2;
  ErrExpression = 3;
  ErrOperator = 4;
  ErrOpenParen = 5;
  ErrOpCloseParen = 6;
  ErrInvalidNum = 7;

type
  THitTest = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon, htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
  THitTests = set of THitTest;

  ErrorRange = 0..TotalErrors;

  TokenTypes = (Plus, Minus, Times, Divide, Expo, OParen, CParen, Num,
    Func, EOL, Bad, ERR, Modu);

  TokenRec = record
    State: Byte;
    case Byte of
      0: (Value: Extended);
      2: (FuncName: string[MaxFuncNameLen]);
  end; { TokenRec }

type
  TJvMathParser = class(TComponent)
  private
    { Private declarations }
    FInput: string;
    FOnGetVar: TGetVarEvent;
    FOnParseError: TParseErrorEvent;
  protected
    { Protected declarations }
    CurrToken: TokenRec;
    MathError: Boolean;
    Stack: array[1..ParserStackSize] of TokenRec;
    StackTop: 0..ParserStackSize;
    TokenError: ErrorRange;
    TokenLen: Word;
    TokenType: TokenTypes;
    function GotoState(Production: Word): Word;
    function IsFunc(S: string): Boolean;
    function IsVar(var Value: Extended): Boolean;
    function NextToken: TokenTypes;
    procedure Push(Token: TokenRec);
    procedure Pop(var Token: TokenRec);
    procedure Reduce(Reduction: Word);
    procedure Shift(State: Word);
  public
    { Public declarations }
    Position: Word;
    ParseError: Boolean;
    ParseValue: Extended;
    constructor Create(AOwner: TComponent); override;
    procedure Parse;
  published
    { Published declarations }
    property OnGetVar: TGetVarEvent read FOnGetVar write FOnGetVar;
    property OnParseError: TParseErrorEvent read FOnParseError
      write FOnParseError;
    property ParseString: string read FInput write FInput;
  end;

  TTreeKeyMappings = class(TPersistent)
  private
    FAddNode: TShortCut;
    FInsertNode: TShortCut;
    FAddChildNode: TShortCut;
    FDeleteNode: TShortCut;
    FDuplicateNode: TshortCut;
    FEditNode: TShortCut;
    FSaveTree: TShortCut;
    FLoadTree: TShortCut;
    FCloseTree: TShortCut;
    FSaveTreeAs: TShortCut;
    FFindNode: TshortCut;
    procedure SetAddNode(const Value: TShortCut);
    procedure SetInsertNode(const Value: TShortCut);
    procedure SetDeleteNode(const Value: TShortCut);
    procedure SetAddChildNode(const Value: TShortCut);
    procedure SetDuplicateNode(const Value: TShortCut);
    procedure SetEditNode(const Value: TshortCut);
    procedure SetLoadTree(const Value: TShortCut);
    procedure SetSaveTree(const Value: TShortCut);
    procedure SetCloseTree(const Value: TShortCut);
    procedure SetSaveTreeAs(const Value: TShortCut);
    procedure SetFindNode(const Value: TshortCut);
    {private declerations}
  protected
    {protected declerations}
  public
    {public declerations}
  published
    {published declerations}
    property AddNode: TShortCut read FAddNode write SetAddNode;
    property DeleteNode: TShortCut read FDeleteNode write SetDeleteNode;
    property InsertNode: TShortCut read FInsertNode write SetInsertNode;
    property AddChildNode: TShortCut read FAddChildNode write SetAddChildNode;
    property DuplicateNode: TShortCut read FDuplicateNode write SetDuplicateNode;
    property EditNode: TshortCut read FEditNode write SetEditNode;
    property FindNode: TshortCut read FFindNode write SetFindNode;
    property LoadTree: TShortCut read FLoadTree write SetLoadTree;
    property SaveTree: TShortCut read FSaveTree write SetSaveTree;
    property SaveTreeAs: TShortCut read FSaveTreeAs write SetSaveTreeAs;
    property CloseTree: TShortCut read FCloseTree write SetCloseTree;

  end;

  TJvJanTreeView = class(TTreeView)
  private
    { Private declarations }
    FParser: TJvMathParser;
    FParseError: boolean;
    FKeyMappings: TTreeKeyMappings;
    FKeymappingsEnabled: boolean;
    FVarList: TStringList;
    FColorFormulas: boolean;
    FFormuleColor: TColor;
    FDefaultExt: string;
    FFileName: TFileName;
    FSearchText: string;
    procedure ParseVariables;
    procedure nodeduplicate(mytree: TJvJanTreeView; fromnode, tonode: ttreenode);
    procedure SetKeyMappings(const Value: TTreeKeyMappings);
    procedure SetKeyMappingsEnabled(const Value: Boolean);
    procedure SetUpKeyMappings;
    procedure ParserGetVar(Sender: TObject; VarName: string; var Value: Extended; var Found: Boolean);
    procedure ParserParseError(Sender: TObject; ParseError: Integer);
    procedure doCustomDrawItem(Sender: TCustomViewControl; Node: TCustomViewItem;
       Canvas: TCanvas; const Rect:TRect; State: TCustomDrawState;
       Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure SetColorFormulas(const Value: boolean);
    procedure SetFormuleColor(const Value: TColor);
    procedure SetDefaultExt(const Value: string);
    procedure SetFileName(const Value: TFileName);
  protected
    { Protected declarations }
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure DuplicateNode;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DoAddNode;
    procedure DoAddChildNode;
    procedure DoDeleteNode;
    procedure DoInsertNode;
    procedure DoEditNode;
    procedure DoFindNode;
    procedure DoLoadTree;
    procedure DoSaveTree;
    procedure DoSaveTreeAs;
    procedure DoCloseTree;
    procedure Recalculate;
  published
    property KeyMappings: TTreeKeyMappings read FKeyMappings write SetKeyMappings;
    property KeymappingsEnabled: boolean read FKeyMappingsEnabled write SetKeyMappingsEnabled;
    property ColorFormulas: boolean read FColorFormulas write SetColorFormulas;
    property FormuleColor: TColor read FFormuleColor write SetFormuleColor;
    property FileName: TFileName read FFilename write SetFileName;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
  end;

implementation

uses
  JvQConsts, JvQResources, JvQTypes;

constructor TJvJanTreeView.Create(AOwner: Tcomponent);
begin
  inherited;
  DragMode := dmAutomatic;
  FDefaultExt := 'txt';
  FKeyMappings := TTreeKeyMappings.create;
  SetupKeyMappings;
  FColorFormulas := true;
  FKeyMappingsEnabled := true;
  FParser := TJvMathParser.create(self);
  FParser.ongetVar := ParserGetVar;
  FParser.onparseError := ParserParseError;
  FVarList := TStringList.Create;
  onCustomDrawItem := doCustomDrawItem;
end;

procedure TJvJanTreeView.SetUpKeyMappings;
begin
  FKeyMappings.AddChildNode := TextToShortCut('Ctrl+Ins');
  FKeyMappings.AddNode := TextToShortCut('Ctrl+Shift+Ins');
  FKeyMappings.InsertNode := TextToShortCut('Shift+Ins');
  FKeyMappings.DeleteNode := TextToShortCut('Shift+Del');
  FKeyMappings.DuplicateNode := TextToShortCut('Ctrl+D');
  FKeyMappings.EditNode := TextToShortCut('F2');
  FKeyMappings.FindNode := TextToShortCut('Ctrl+F');
  FKeyMappings.LoadTree := TextToShortCut('Ctrl+O');
  FKeyMappings.SaveTree := TextToShortCut('Ctrl+S');
  FKeyMappings.CloseTree := TextToShortCut('Ctrl+Alt+C');
  FKeyMappings.SaveTreeAs := TextToShortCut('Ctrl+Alt+S');
end;

procedure TJvJanTreeView.DblClick;
var
  n: ttreenode;
  s: string;
begin
  if Selected <> nil then
  begin
    n := selected;
    s := n.text;
    if (copy(s, 1, 7) = 'http://') or (copy(s, 1, 7) = 'mailto:') then
      ShellExecute(handle, 'open', @s[1], nil, nil, SW_SHOWNORMAL);
  end;
  if assigned(onDblClick) then
    onDblClick(self);
end;

procedure TJvJanTreeView.DoAddChildNode;
var
  n: ttreenode;
begin
  if selected <> nil then
  begin
    n := selected;
    n := items.AddChild(n, 'new node');
    selected := n;
  end;
end;

procedure TJvJanTreeView.DoAddNode;
var
  n: ttreenode;
begin
  items.BeginUpdate;
  n := Items.Add(selected, 'new node');
  items.EndUpdate;
  selected := n;
end;

procedure TJvJanTreeView.DoDeleteNode;
begin
  if Selected <> nil then
  begin
    Items.Delete(selected);
  end;
end;

procedure TJvJanTreeView.DoEditNode;
var
  n: ttreenode;
begin
  if selected <> nil then
  begin
    n := selected;
    n.edittext;
  end;
end;

procedure TJvJanTreeView.DoInsertNode;
var
  n: ttreenode;
begin
  if Selected <> nil then
  begin
    n := selected;
    items.BeginUpdate;
    n := items.Insert(n, 'new node');
    items.EndUpdate;
    Selected := n;
  end;
end;

procedure TJvJanTreeView.DragDrop(Source: TObject; X, Y: Integer);
var
  MyHitTest: THitTests;
  n: ttreenode;
begin
  inherited;
//  MyHitTest := self.GetHitTestInfoAt(X, Y);
  if htOnLabel in MyHitTest then
  begin
    n := self.GetNodeAt(x, y);
    if source = self then
    begin
      if Selected = nil then exit;
      selected.MoveTo(n, nainsert);
    end;
  end;
  if assigned(ondragdrop) then
    ondragdrop(self, source, x, y);
end;

procedure TJvJanTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  Accept := (source = self);
  if assigned(ondragover) then
    ondragover(self, source, x, y, state, accept);
end;

procedure TJvJanTreeView.DuplicateNode;
var
  mynode, mynewnode: ttreenode;
begin
  if selected <> nil then
  begin
    mynode := selected;
    mynewnode := items.add(mynode, mynode.text);
    nodeduplicate(self, mynode, mynewnode);
  end;
end;

procedure TJvJanTreeView.KeyUp(var Key: Word; Shift: TShiftState);
var
  Mkey: word;
  MShift: TshiftState;

  function MLoadTree: boolean;
  begin
    ShortCutToKey(KeyMappings.LoadTree, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MSaveTree: boolean;
  begin
    ShortCutToKey(KeyMappings.SaveTree, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MSaveTreeAs: boolean;
  begin
    ShortCutToKey(KeyMappings.SaveTreeAs, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MCloseTree: boolean;
  begin
    ShortCutToKey(KeyMappings.CloseTree, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MAddNode: boolean;
  begin
    ShortCutToKey(KeyMappings.AddNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MDeleteNode: boolean;
  begin
    ShortCutToKey(KeyMappings.DeleteNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MInsertNode: boolean;
  begin
    ShortCutToKey(KeyMappings.InsertNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MAddChildNode: boolean;
  begin
    ShortCutToKey(KeyMappings.AddChildNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MDuplicateNode: boolean;
  begin
    ShortCutToKey(KeyMappings.DuplicateNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MEditNode: boolean;
  begin
    ShortCutToKey(KeyMappings.EditNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

  function MFindNode: boolean;
  begin
    ShortCutToKey(KeyMappings.FindNode, Mkey, MShift);
    result := ((Mkey = key) and (MShift = Shift));
  end;

begin
  inherited;
  if KeyMappingsEnabled then
  begin
    if MAddNode then
      DoAddNode
    else if MDeleteNode then
      DoDeleteNode
    else if MInsertNode then
      DoInsertNode
    else if MAddChildNode then
      DoAddChildNode
    else if MDuplicateNode then
      DuplicateNode
    else if MEditNode then
      DoEditNode
    else if MFindNode then
      DoFindNode
    else if MLoadTree then
      DoLoadTree
    else if MSaveTree then
      DoSaveTree
    else if MSaveTreeAs then
      DoSaveTreeAs
    else if MCloseTree then
      DoCloseTree;
  end;
  if assigned(onKeyDown) then
    onkeyDown(self, key, shift);
end;

procedure TJvJanTreeView.SetKeyMappings(const Value: TTreeKeyMappings);
begin
  FKeyMappings := Value;
end;

procedure TJvJanTreeView.SetKeyMappingsEnabled(const Value: Boolean);
begin
  FKeyMappingsEnabled := Value;
end;

procedure TJvJanTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  MyHitTest: THitTests;
  n: ttreenode;
  s: string;
begin
//  MyHitTest := GetHitTestInfoAt(X, Y);
  if htOnLabel in MyHitTest then
  begin
    n := GetNodeAt(x, y);
    s := n.text;
    if (copy(s, 1, 7) = 'http://') or (copy(s, 1, 7) = 'mailto:') then
      Cursor := crhandpoint
    else
      cursor := crdefault;
  end
  else
    cursor := crdefault;
  if assigned(onmousemove) then
    onmousemove(self, shift, x, y);
end;

procedure TJvJanTreeView.nodeduplicate(mytree: TJvJanTreeView; fromnode,
  tonode: ttreenode);
var
  i: Integer;
begin
  if fromnode.count > 0 then
    for i := 1 to fromnode.count do
    begin
      mytree.items.addchild(tonode, fromnode.item[i - 1].text);
      if fromnode.item[i - 1].count > 0 then
        nodeduplicate(mytree, fromnode.item[i - 1], tonode.item[i - 1]);
    end;
end;

destructor TJvJanTreeView.Destroy;
begin
  FParser.Free;
  FKeyMappings.free;
  FVarList.Free;
  inherited;
end;

procedure TJvJanTreeView.ParserGetVar(Sender: TObject; VarName: string;
  var Value: Extended; var Found: Boolean);
var
  n: TTreenode;
  index: integer;
begin
  found := false;
  index := FVarList.IndexOf(VarName);
  if index <> -1 then
  begin
    n := TTreeNode(FVarList.objects[index]);
    if n.count > 0 then
    try
      Value := strtofloat(n.item[0].text);
      found := true;
    except
      //
    end;
  end
  else if lowercase(VarName) = 'pi' then
  begin
    value := pi;
    found := true;
  end;
end;

procedure TJvJanTreeView.ParserParseError(Sender: TObject;
  ParseError: Integer);
begin
  FParseError := true;
end;

procedure TJvJanTreeView.Recalculate;
var
  n, nv: TTreeNode;
  s: string;
  i, p: integer;
begin
  if Items.Count = 0 then exit;
  ParseVariables;
  for i := 0 to items.count - 1 do
  begin
    n := items[i];
    s := n.Text;
    p := pos('=', s);
    if p = 0 then continue;
    s := copy(s, p + 1, length(s));
    if s = '' then continue;
    FParser.ParseString := s;
    FparseError := false;
    Fparser.Parse;
    if not FParseError then
    begin
      if n.count = 0 then
        items.AddChild(n, 'new');
      nv := n.Item[0];
      nv.Text := floattostr(Fparser.ParseValue);
    end
    else
    begin
      showmessage('Error in: ' + s);
      exit;
    end;
  end;
end;

constructor TJvMathParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { defaults }
  FInput := '';
end;

function TJvMathParser.GotoState(Production: Word): Word;
{ Finds the new state based on the just-completed production and the
   top state. }
var
  State: Word;
begin
  Result := 0; // removes warning
  State := Stack[StackTop].State;
  if (Production <= 3) then
  begin
    case State of
      0: GotoState := 1;
      9: GotoState := 19;
      20: GotoState := 28;
    end; { case }
  end
  else if Production <= 6 then
  begin
    case State of
      0, 9, 20: GotoState := 2;
      12: GotoState := 21;
      13: GotoState := 22;
    end; { case }
  end
  else if (Production <= 8) or (Production = 100) then
  begin
    case State of
      0, 9, 12, 13, 20: GotoState := 3;
      14: GotoState := 23;
      15: GotoState := 24;
      16: GotoState := 25;
      40: GotoState := 80;
    end; { case }
  end
  else if Production <= 10 then
  begin
    case State of
      0, 9, 12..16, 20, 40: GotoState := 4;
    end; { case }
  end
  else if Production <= 12 then
  begin
    case State of
      0, 9, 12..16, 20, 40: GotoState := 6;
      5: GotoState := 17;
    end; { case }
  end
  else
  begin
    case State of
      0, 5, 9, 12..16, 20, 40: GotoState := 8;
    end; { case }
  end;
end; { GotoState }

function TJvMathParser.IsFunc(S: string): Boolean;
{ Checks to see if the parser is about to read a function }
var
  P, SLen: Word;
  FuncName: string;
begin
  P := Position;
  FuncName := '';
  while (P <= Length(FInput)) and (FInput[P] in IdentifierSymbols) do
  begin
    FuncName := FuncName + FInput[P];
    Inc(P);
  end; { while }
  if Uppercase(FuncName) = S then
  begin
    SLen := Length(S);
    CurrToken.FuncName := UpperCase(Copy(FInput, Position, SLen));
    Inc(Position, SLen);
    IsFunc := True;
  end { if }
  else
    IsFunc := False;
end; { IsFunc }

function TJvMathParser.IsVar(var Value: Extended): Boolean;
var
  VarName: string;
  VarFound: Boolean;
begin
  VarFound := False;
  VarName := '';
  while (Position <= Length(FInput)) and (FInput[Position] in IdentifierSymbols) do
  begin
    VarName := VarName + FInput[Position];
    Inc(Position);
  end; { while }
  if Assigned(FOnGetVar) then FOnGetVar(Self, VarName, Value, VarFound);
  IsVar := VarFound;
end; { IsVar }

function TJvMathParser.NextToken: TokenTypes;
{ Gets the next Token from the Input stream }
var
  NumString: string[80];
  TLen, NumLen: Word;
  Check: Integer;
  Ch: Char;
  Decimal: Boolean;
begin
  NextToken := bad;
  while (Position <= Length(FInput)) and (FInput[Position] = ' ') do
    Inc(Position);
  TokenLen := Position;
  if Position > Length(FInput) then
  begin
    NextToken := EOL;
    TokenLen := 0;
    Exit;
  end; { if }
  Ch := UpCase(FInput[Position]);
  if Ch in ['!'] then
  begin
    NextToken := ERR;
    TokenLen := 0;
    Exit;
  end; { if }
  if Ch in ['0'..'9', '.'] then
  begin
    NumString := '';
    TLen := Position;
    Decimal := False;
    while (TLen <= Length(FInput)) and
      ((FInput[TLen] in DigitSymbols) or
      ((FInput[TLen] = '.') and (not Decimal))) do
    begin
      NumString := NumString + FInput[TLen];
      if Ch = '.' then
        Decimal := True;
      Inc(TLen);
    end; { while }
    if (TLen = 2) and (Ch = '.') then
    begin
      NextToken := BAD;
      TokenLen := 0;
      Exit;
    end; { if }
    if (TLen <= Length(FInput)) and (UpCase(FInput[TLen]) = 'E') then
    begin
      NumString := NumString + 'E';
      Inc(TLen);
      if FInput[TLen] in ['+', '-'] then
      begin
        NumString := NumString + FInput[TLen];
        Inc(TLen);
      end; { if }
      NumLen := 1;
      while (TLen <= Length(FInput)) and (FInput[TLen] in DigitSymbols) and
        (NumLen <= MaxExpLen) do
      begin
        NumString := NumString + FInput[TLen];
        Inc(NumLen);
        Inc(TLen);
      end; { while }
    end; { if }
    if NumString[1] = '.' then
      NumString := '0' + NumString;
    Val(NumString, CurrToken.Value, Check);
    if Check <> 0 then
    begin
      MathError := True;
      TokenError := ErrInvalidNum;
      Inc(Position, Pred(Check));
    end { if }
    else
    begin
      NextToken := NUM;
      Inc(Position, System.Length(NumString));
      TokenLen := Position - TokenLen;
    end; { else }
    Exit;
  end { if }
  else if Ch in IdentifierLetters then
  begin
    if IsFunc('ABS') or
      IsFunc('ATAN') or
      IsFunc('COS') or
      IsFunc('EXP') or
      IsFunc('LN') or
      IsFunc('ROUND') or
      IsFunc('SIN') or
      IsFunc('SQRT') or
      IsFunc('SQR') or
      IsFunc('TRUNC') then
    begin
      NextToken := FUNC;
      TokenLen := Position - TokenLen;
      Exit;
    end; { if }
    if IsFunc('MOD') then
    begin
      NextToken := MODU;
      TokenLen := Position - TokenLen;
      Exit;
    end; { if }
    if IsVar(CurrToken.Value) then
    begin
      NextToken := NUM;
      TokenLen := Position - TokenLen;
      Exit;
    end { if }
    else
    begin
      NextToken := BAD;
      TokenLen := 0;
      Exit;
    end; { else }
  end { if }
  else
  begin
    case Ch of
      '+': NextToken := PLUS;
      '-': NextToken := MINUS;
      '*': NextToken := TIMES;
      '/': NextToken := DIVIDE;
      '^': NextToken := EXPO;
      '(': NextToken := OPAREN;
      ')': NextToken := CPAREN;
    else
      begin
        NextToken := BAD;
        TokenLen := 0;
        Exit;
      end; { case else }
    end; { case }
    Inc(Position);
    TokenLen := Position - TokenLen;
    Exit;
  end; { else if }
end; { NextToken }

procedure TJvMathParser.Pop(var Token: TokenRec);
{ Pops the top Token off of the stack }
begin
  Token := Stack[StackTop];
  Dec(StackTop);
end; { Pop }

procedure TJvMathParser.Push(Token: TokenRec);
{ Pushes a new Token onto the stack }
begin
  if StackTop = ParserStackSize then
    TokenError := ErrParserStack
  else
  begin
    Inc(StackTop);
    Stack[StackTop] := Token;
  end; { else }
end; { Push }

procedure TJvMathParser.Parse;
{ Parses an input stream }
var
  FirstToken: TokenRec;
  Accepted: Boolean;
begin
  Position := 1;
  StackTop := 0;
  TokenError := 0;
  MathError := False;
  ParseError := False;
  Accepted := False;
  FirstToken.State := 0;
  FirstToken.Value := 0;
  Push(FirstToken);
  TokenType := NextToken;
  repeat
    case Stack[StackTop].State of
      0, 9, 12..16, 20, 40:
        begin
          if TokenType = NUM then
            Shift(10)
          else if TokenType = FUNC then
            Shift(11)
          else if TokenType = MINUS then
            Shift(5)
          else if TokenType = OPAREN then
            Shift(9)
          else if TokenType = ERR then
          begin
            MathError := True;
            Accepted := True;
          end { else if }
          else
          begin
            TokenError := ErrExpression;
            Dec(Position, TokenLen);
          end; { else }
        end; { case of }
      1:
        begin
          if TokenType = EOL then
            Accepted := True
          else if TokenType = PLUS then
            Shift(12)
          else if TokenType = MINUS then
            Shift(13)
          else
          begin
            TokenError := ErrOperator;
            Dec(Position, TokenLen);
          end; { else }
        end; { case of }
      2:
        begin
          if TokenType = TIMES then
            Shift(14)
          else if TokenType = DIVIDE then
            Shift(15)
          else
            Reduce(3);
        end; { case of }
      3:
        begin
          if TokenType = MODU then
            Shift(40)
          else
            Reduce(6);
        end; { case of }
      4:
        begin
          if TokenType = EXPO then
            Shift(16)
          else
            Reduce(8);
        end; { case of }
      5:
        begin
          if TokenType = NUM then
            Shift(10)
          else if TokenType = FUNC then
            Shift(11)
          else if TokenType = OPAREN then
            Shift(9)
          else
          begin
            TokenError := ErrExpression;
            Dec(Position, TokenLen);
          end; { else }
        end; { case of }
      6: Reduce(10);
      7: Reduce(13);
      8: Reduce(12);
      10: Reduce(15);
      11:
        begin
          if TokenType = OPAREN then
            Shift(20)
          else
          begin
            TokenError := ErrOpenParen;
            Dec(Position, TokenLen);
          end; { else }
        end; { case of }
      17: Reduce(9);
      18: raise EJVCLException.Create(RsEBadTokenState);
      19:
        begin
          if TokenType = PLUS then
            Shift(12)
          else if TokenType = MINUS then
            Shift(13)
          else if TokenType = CPAREN then
            Shift(27)
          else
          begin
            TokenError := ErrOpCloseParen;
            Dec(Position, TokenLen);
          end;
        end; { case of }
      21:
        begin
          if TokenType = TIMES then
            Shift(14)
          else if TokenType = DIVIDE then
            Shift(15)
          else
            Reduce(1);
        end; { case of }
      22:
        begin
          if TokenType = TIMES then
            Shift(14)
          else if TokenType = DIVIDE then
            Shift(15)
          else
            Reduce(2);
        end; { case of }
      23: Reduce(4);
      24: Reduce(5);
      25: Reduce(7);
      26: Reduce(11);
      27: Reduce(14);
      28:
        begin
          if TokenType = PLUS then
            Shift(12)
          else if TokenType = MINUS then
            Shift(13)
          else if TokenType = CPAREN then
            Shift(29)
          else
          begin
            TokenError := ErrOpCloseParen;
            Dec(Position, TokenLen);
          end; { else }
        end; { case of }
      29: Reduce(16);
      80: Reduce(100);
    end; { case }
  until Accepted or (TokenError <> 0);
  if TokenError <> 0 then
  begin
    if TokenError = ErrBadRange then
      Dec(Position, TokenLen);
    if Assigned(FOnParseError) then FOnParseError(Self, TokenError);
  end; { if }
  if MathError or (TokenError <> 0) then
  begin
    ParseError := True;
    ParseValue := 0;
    Exit;
  end; { if }
  ParseError := False;
  ParseValue := Stack[StackTop].Value;
end; { Parse }

procedure TJvMathParser.Reduce(Reduction: Word);
{ Completes a reduction }
var
  Token1, Token2: TokenRec;
begin
  case Reduction of
    1:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        CurrToken.Value := Token1.Value + Token2.Value;
      end;
    2:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        CurrToken.Value := Token2.Value - Token1.Value;
      end;
    4:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        CurrToken.Value := Token1.Value * Token2.Value;
      end;
    5:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        if Token1.Value = 0 then
          MathError := True
        else
          CurrToken.Value := Token2.Value / Token1.Value;
      end;

    { MOD operator }
    100:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        if Token1.Value = 0 then
          MathError := True
        else
          CurrToken.Value := Round(Token2.Value) mod Round(Token1.Value);
      end;

    7:
      begin
        Pop(Token1);
        Pop(Token2);
        Pop(Token2);
        if Token2.Value <= 0 then
          MathError := True
        else if (Token1.Value * Ln(Token2.Value) < -ExpLimit) or
          (Token1.Value * Ln(Token2.Value) > ExpLimit) then
          MathError := True
        else
          CurrToken.Value := Exp(Token1.Value * Ln(Token2.Value));
      end;
    9:
      begin
        Pop(Token1);
        Pop(Token2);
        CurrToken.Value := -Token1.Value;
      end;
    11: raise EJVCLException.Create(RsEInvalidReduction);
    13: raise EJVCLException.Create(RsEInvalidReduction);
    14:
      begin
        Pop(Token1);
        Pop(CurrToken);
        Pop(Token1);
      end;
    16:
      begin
        Pop(Token1);
        Pop(CurrToken);
        Pop(Token1);
        Pop(Token1);
        if Token1.FuncName = 'ABS' then
          CurrToken.Value := Abs(CurrToken.Value)
        else if Token1.FuncName = 'ATAN' then
          CurrToken.Value := ArcTan(CurrToken.Value)
        else if Token1.FuncName = 'COS' then
        begin
          if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
          else
            CurrToken.Value := Cos(CurrToken.Value)
        end {...if Token1.FuncName = 'SIN' }
        else if Token1.FuncName = 'EXP' then
        begin
          if (CurrToken.Value < -ExpLimit) or (CurrToken.Value > ExpLimit) then
            MathError := True
          else
            CurrToken.Value := Exp(CurrToken.Value);
        end
        else if Token1.FuncName = 'LN' then
        begin
          if CurrToken.Value <= 0 then
            MathError := True
          else
            CurrToken.Value := Ln(CurrToken.Value);
        end
        else if Token1.FuncName = 'ROUND' then
        begin
          if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
            MathError := True
          else
            CurrToken.Value := Round(CurrToken.Value);
        end
        else if Token1.FuncName = 'SIN' then
        begin
          if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
          else
            CurrToken.Value := Sin(CurrToken.Value)
        end {...if Token1.FuncName = 'SIN' }
        else if Token1.FuncName = 'SQRT' then
        begin
          if CurrToken.Value < 0 then
            MathError := True
          else
            CurrToken.Value := Sqrt(CurrToken.Value);
        end
        else if Token1.FuncName = 'SQR' then
        begin
          if (CurrToken.Value < -SQRLIMIT) or (CurrToken.Value > SQRLIMIT) then
            MathError := True
          else
            CurrToken.Value := Sqr(CurrToken.Value);
        end
        else if Token1.FuncName = 'TRUNC' then
        begin
          if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
            MathError := True
          else
            CurrToken.Value := Trunc(CurrToken.Value);
        end;
      end;
    3, 6, 8, 10, 12, 15: Pop(CurrToken);
  end; { case }
  CurrToken.State := GotoState(Reduction);
  Push(CurrToken);
end; { Reduce }

procedure TJvMathParser.Shift(State: Word);
{ Shifts a Token onto the stack }
begin
  CurrToken.State := State;
  Push(CurrToken);
  TokenType := NextToken;
end; { Shift }

procedure TTreeKeyMappings.SetAddNode(const Value: TShortCut);
begin
  FAddNode := Value;
end;

procedure TTreeKeyMappings.SetDeleteNode(const Value: TShortCut);
begin
  FDeleteNode := Value;
end;

procedure TTreeKeyMappings.SetInsertNode(const Value: TShortCut);
begin
  FInsertNode := Value;
end;

procedure TTreeKeyMappings.SetAddChildNode(const Value: TShortCut);
begin
  FAddChildNode := Value;
end;

procedure TTreeKeyMappings.SetDuplicateNode(const Value: TshortCut);
begin
  FDuplicateNode := Value;
end;

procedure TTreeKeyMappings.SetEditNode(const Value: TShortCut);
begin
  FEditNode := Value;
end;

procedure TJvJanTreeView.KeyPress(var Key: Char);
begin
  if key = char(vk_return) then
    recalculate;
  if assigned(onkeyPress) then
    onkeyPress(self, key);
end;

procedure TJvJanTreeView.ParseVariables;
var
  i, p: integer;
  n: TTreeNode;
  s: string;
begin
  FVarList.Clear;
  if items.Count = 0 then exit;
  for i := 0 to items.count - 1 do
  begin
    n := items[i];
    s := n.Text;
    p := pos('=', s);
    if p = 0 then continue;
    s := copy(s, 1, p - 1);
    if s <> '' then
      FVarList.AddObject(s, TObject(n));
  end;
end;

type
  TOpenCustomViewItem = class(TCustomViewItem);

procedure TJvJanTreeView.doCustomDrawItem(Sender: TCustomViewControl; Node: TCustomViewItem;
       Canvas: TCanvas; const Rect:TRect; State: TCustomDrawState;
       Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  s: string;
  dr: trect;
begin
  s := TOpenCustomViewItem(node).caption;
  if (cdsSelected in state) or (cdsFocused in state) then
  begin
    defaultdraw := true;
    exit;
  end;
  if (copy(s, 1, 7) = 'http://') or (copy(s, 1, 7) = 'mailto:') then
    with Canvas do
    begin
      dr := node.displayrect;
      font := self.Font;
      font.Style := font.style + [fsunderline];
      font.Color := clblue;
      textrect(dr, dr.left, dr.Top, s);
      defaultdraw := false;
    end
  else if FColorFormulas and (pos('=', s) > 0) then
    with canvas do
    begin
      dr := node.displayrect;
      font := self.Font;
      font.color := FFormuleColor;
      textrect(dr, dr.left, dr.top, s);
      defaultdraw := false;
    end
  else
    defaultdraw := true;
end;

procedure TJvJanTreeView.SetColorFormulas(const Value: boolean);
begin
  FColorFormulas := Value;
end;

procedure TJvJanTreeView.SetFormuleColor(const Value: TColor);
begin
  FFormuleColor := Value;
end;

procedure TTreeKeyMappings.SetLoadTree(const Value: TShortCut);
begin
  FLoadTree := Value;
end;

procedure TTreeKeyMappings.SetSaveTree(const Value: TShortCut);
begin
  FSaveTree := Value;
end;

procedure TJvJanTreeView.DoLoadTree;
var
  dlg: TOpenDialog;
  s: string;
begin
  dlg := TOpendialog.Create(self);
  try
    dlg.DefaultExt := FDefaultExt;
    s := FDefaultExt;
    if s = '' then s := '*';
    dlg.filter := RsTreeViewFiles + '|*.' + s;
    if dlg.Execute then
    begin
      LoadFromFile(dlg.filename);
      FFileName := dlg.FileName;
      Recalculate;
    end;
  finally
    dlg.free;
  end;
end;

procedure TJvJanTreeView.DoSaveTreeAs;
var
  dlg: TSaveDialog;
  s: string;
begin
  dlg := TSavedialog.Create(self);
  try
    dlg.DefaultExt := FDefaultExt;
    s := FDefaultExt;
    if s = '' then s := '*';
    dlg.filter := RsTreeViewFiles + '|*.' + s;
    if dlg.Execute then
    begin
      SaveToFile(dlg.filename);
      FFileName := dlg.FileName;
    end;
  finally
    dlg.free;
  end;
end;

procedure TJvJanTreeView.SetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvJanTreeView.SetFilename(const Value: TFileName);
begin
  FFilename := Value;
end;

procedure TJvJanTreeView.DoCloseTree;
begin
  if MessageDlg(RsSaveCurrentTree, mtconfirmation, [mbyes, mbno], 0) = mryes then
  begin
    if FFilename <> '' then
      SaveToFile(FFilename)
    else
      DoSaveTreeAs;
  end;
  items.BeginUpdate;
  items.Clear;
  items.EndUpdate;
  FFileName := '';
end;

procedure TTreeKeyMappings.SetCloseTree(const Value: TShortCut);
begin
  FCloseTree := Value;
end;

procedure TTreeKeyMappings.SetSaveTreeAs(const Value: TShortCut);
begin
  FSaveTreeAs := Value;
end;

procedure TJvJanTreeView.DoSaveTree;
begin
  if FFileName <> '' then
    SaveToFile(FFileName)
  else
    DoSaveTreeAs;
end;

procedure TTreeKeyMappings.SetFindNode(const Value: TshortCut);
begin
  FFindNode := Value;
end;

procedure TJvJanTreeView.DoFindNode;
var
  n: TTreeNode;
  i, fr: integer;
  s: string;
begin
  n := selected;
  if n = nil then exit;
  s := inputbox(RsSearch, RsSearchFor, FSearchText);
  if s = '' then exit;
  FSearchText := s;
  s := Lowercase(s);
  fr := n.AbsoluteIndex;
  if fr < items.count - 1 then
    for i := fr + 1 to items.Count - 1 do
      if Pos(s, lowercase(items[i].text)) > 0 then
      begin
        selected := items[i];
        exit;
      end;
  showmessage(Format(RsNoMoresFound, [s]));
end;

end.
