{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJanTreeView.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvJanTreeView;

interface

uses
  Windows,
  {$IFDEF VCL}
  ShellAPI, Messages,
  {$ENDIF VCL}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, Menus;

type
  TGetVarEvent = procedure(Sender: TObject; VarName: string;
    var Value: Extended; var Found: Boolean) of object;

  TParseErrorEvent = procedure(Sender: TObject; ParseError: Integer) of object;

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
  ErrorRange = 0..TotalErrors;

  TokenTypes = (ttPlus, ttMinus, ttTimes, ttDivide, ttExpo, ttOParen,
    ttCParen, ttNum, ttFunc, ttEol, ttBad, ttErr, ttModu);

  TokenRec = record
    State: Byte;
    case Byte of
      0:
        (Value: Extended);
      2:
        (FuncName: string[MaxFuncNameLen]);
  end;

type
  TJvMathParser = class(TComponent)
  private
    FInput: string;
    FOnGetVar: TGetVarEvent;
    FOnParseError: TParseErrorEvent;
    FPosition: Word;
    FParseError: Boolean;
    FParseValue: Extended;
  protected
    CurrToken: TokenRec;
    MathError: Boolean;
    Stack: array [1..ParserStackSize] of TokenRec;
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
    constructor Create(AOwner: TComponent); override;
    procedure Parse;
    property Position: Word read FPosition write FPosition;
    property ParseError: Boolean read FParseError write FParseError;
    property ParseValue: Extended read FParseValue write FParseValue;
  published
    property OnGetVar: TGetVarEvent read FOnGetVar write FOnGetVar;
    property OnParseError: TParseErrorEvent read FOnParseError write FOnParseError;
    property ParseString: string read FInput write FInput;
  end;

  TTreeKeyMappings = class(TPersistent)
  private
    FAddNode: TShortCut;
    FInsertNode: TShortCut;
    FAddChildNode: TShortCut;
    FDeleteNode: TShortCut;
    FDuplicateNode: TShortCut;
    FEditNode: TShortCut;
    FSaveTree: TShortCut;
    FLoadTree: TShortCut;
    FCloseTree: TShortCut;
    FSaveTreeAs: TShortCut;
    FFindNode: TShortCut;
    procedure SetAddNode(const Value: TShortCut);
    procedure SetInsertNode(const Value: TShortCut);
    procedure SetDeleteNode(const Value: TShortCut);
    procedure SetAddChildNode(const Value: TShortCut);
    procedure SetDuplicateNode(const Value: TShortCut);
    procedure SetEditNode(const Value: TShortCut);
    procedure SetLoadTree(const Value: TShortCut);
    procedure SetSaveTree(const Value: TShortCut);
    procedure SetCloseTree(const Value: TShortCut);
    procedure SetSaveTreeAs(const Value: TShortCut);
    procedure SetFindNode(const Value: TShortCut);
  published
    property AddNode: TShortCut read FAddNode write SetAddNode;
    property DeleteNode: TShortCut read FDeleteNode write SetDeleteNode;
    property InsertNode: TShortCut read FInsertNode write SetInsertNode;
    property AddChildNode: TShortCut read FAddChildNode write SetAddChildNode;
    property DuplicateNode: TShortCut read FDuplicateNode write SetDuplicateNode;
    property EditNode: TShortCut read FEditNode write SetEditNode;
    property FindNode: TShortCut read FFindNode write SetFindNode;
    property LoadTree: TShortCut read FLoadTree write SetLoadTree;
    property SaveTree: TShortCut read FSaveTree write SetSaveTree;
    property SaveTreeAs: TShortCut read FSaveTreeAs write SetSaveTreeAs;
    property CloseTree: TShortCut read FCloseTree write SetCloseTree;
  end;

  TJvJanTreeView = class(TTreeView)
  private
    FParser: TJvMathParser;
    FParseError: Boolean;
    FKeyMappings: TTreeKeyMappings;
    FKeyMappingsEnabled: Boolean;
    FVarList: TStringList;
    FColorFormulas: Boolean;
    FFormuleColor: TColor;
    FDefaultExt: string;
    FFileName: TFileName;
    FSearchText: string;
    procedure ParseVariables;
    procedure NodeDuplicate(ATree: TJvJanTreeView; FromNode, ToNode: TTreeNode);
    procedure SetKeyMappings(const Value: TTreeKeyMappings);
    procedure SetKeyMappingsEnabled(const Value: Boolean);
    procedure SetupKeyMappings;
    procedure ParserGetVar(Sender: TObject; VarName: string; var Value: Extended; var Found: Boolean);
    procedure ParserParseError(Sender: TObject; ParseError: Integer);
    {$IFDEF VCL}
    procedure DoCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure DoCustomDrawItem(Sender: TCustomViewControl; Node: TCustomViewItem;
       Canvas: TCanvas; const Rect:TRect; State: TCustomDrawState;
       Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    {$ENDIF VisualCLX}
    procedure SetColorFormulas(const Value: Boolean);
    procedure SetFormuleColor(const Value: TColor);
    procedure SetDefaultExt(const Value: string);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
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
    property KeyMappingsEnabled: Boolean read FKeyMappingsEnabled write SetKeyMappingsEnabled default True;
    property ColorFormulas: Boolean read FColorFormulas write SetColorFormulas default True;
    property FormuleColor: TColor read FFormuleColor write SetFormuleColor;
    property FileName: TFileName read FFileName write SetFileName;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
  end;

implementation

uses
  JvConsts, JvResources, JvTypes;

//=== { TJvJanTreeView } =====================================================

constructor TJvJanTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragMode := dmAutomatic;
  FDefaultExt := 'txt';
  FKeyMappings := TTreeKeyMappings.Create;
  SetupKeyMappings;
  FColorFormulas := True;
  FKeyMappingsEnabled := True;
  FParser := TJvMathParser.Create(Self);
  FParser.OnGetVar := ParserGetVar;
  FParser.OnParseError := ParserParseError;
  FVarList := TStringList.Create;
  OnCustomDrawItem := DoCustomDrawItem;
end;

destructor TJvJanTreeView.Destroy;
begin
  FParser.Free;
  FKeyMappings.Free;
  FVarList.Free;
  inherited Destroy;
end;

procedure TJvJanTreeView.SetupKeyMappings;
begin
  with FKeyMappings do
  begin
    AddChildNode := TextToShortCut('Ctrl+Ins');
    AddNode := TextToShortCut('Ctrl+Shift+Ins');
    InsertNode := TextToShortCut('Shift+Ins');
    DeleteNode := TextToShortCut('Shift+Del');
    DuplicateNode := TextToShortCut('Ctrl+D');
    EditNode := TextToShortCut('F2');
    FindNode := TextToShortCut('Ctrl+F');
    LoadTree := TextToShortCut('Ctrl+O');
    SaveTree := TextToShortCut('Ctrl+S');
    CloseTree := TextToShortCut('Ctrl+Alt+C');
    SaveTreeAs := TextToShortCut('Ctrl+Alt+S');
  end;
end;

procedure TJvJanTreeView.DblClick;
var
  N: TTreeNode;
  S: string;
begin
  if Selected <> nil then
  begin
    N := Selected;
    S := N.Text;
    if (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 7) = 'mailto:') then
      ShellExecute(Handle, 'open', PChar(S), nil, nil, SW_SHOWNORMAL);
  end;
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TJvJanTreeView.DoAddChildNode;
var
  N: TTreeNode;
begin
  if Selected <> nil then
  begin
    N := Selected;
    N := Items.AddChild(N, RsNewNode);
    Selected := N;
  end;
end;

procedure TJvJanTreeView.DoAddNode;
var
  N: TTreeNode;
begin
  Items.BeginUpdate;
  N := Items.Add(Selected, RsNewNode);
  Items.EndUpdate;
  Selected := N;
end;

procedure TJvJanTreeView.DoDeleteNode;
begin
  if Selected <> nil then
    Items.Delete(Selected);
end;

procedure TJvJanTreeView.DoEditNode;
var
  N: TTreeNode;
begin
  if Selected <> nil then
  begin
    N := Selected;
    N.EditText;
  end;
end;

procedure TJvJanTreeView.DoInsertNode;
var
  N: TTreeNode;
begin
  if Selected <> nil then
  begin
    N := Selected;
    Items.BeginUpdate;
    N := Items.Insert(N, RsNewNode);
    Items.EndUpdate;
    Selected := N;
  end;
end;

procedure TJvJanTreeView.DragDrop(Source: TObject; X, Y: Integer);
var
  {$IFDEF VCL}
  HitTest: THitTests;
  {$ENDIF VCL}
  N: TTreeNode;
begin
  inherited DragDrop(Source, X, Y);
  {$IFDEF VCL}
  HitTest := Self.GetHitTestInfoAt(X, Y);
  if htOnLabel in HitTest then
  begin
  {$ENDIF VCL}
    N := Self.GetNodeAt(X, Y);
  {$IFDEF VisualCLX}
  if N <> nil then
  begin
 {$ENDIF VisualCLX}
    if Source = Self then
    begin
      if Selected = nil then
        Exit;
      Selected.MoveTo(N, naInsert);
    end;
  end;
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y);
end;

procedure TJvJanTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Accept := (Source = Self);
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TJvJanTreeView.DuplicateNode;
var
  Node, NewNode: TTreeNode;
begin
  if Selected <> nil then
  begin
    Node := Selected;
    NewNode := Items.Add(Node, Node.Text);
    NodeDuplicate(Self, Node, NewNode);
  end;
end;

procedure TJvJanTreeView.KeyUp(var Key: Word; Shift: TShiftState);
var
  MKey: Word;
  MShift: TShiftState;

  function MLoadTree: Boolean;
  begin
    ShortCutToKey(KeyMappings.LoadTree, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MSaveTree: Boolean;
  begin
    ShortCutToKey(KeyMappings.SaveTree, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MSaveTreeAs: Boolean;
  begin
    ShortCutToKey(KeyMappings.SaveTreeAs, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MCloseTree: Boolean;
  begin
    ShortCutToKey(KeyMappings.CloseTree, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MAddNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.AddNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MDeleteNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.DeleteNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MInsertNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.InsertNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MAddChildNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.AddChildNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MDuplicateNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.DuplicateNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MEditNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.EditNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

  function MFindNode: Boolean;
  begin
    ShortCutToKey(KeyMappings.FindNode, MKey, MShift);
    Result := ((MKey = Key) and (MShift = Shift));
  end;

begin
  inherited KeyUp(Key, Shift);
  if KeyMappingsEnabled then
  begin
    if MAddNode then
      DoAddNode
    else
    if MDeleteNode then
      DoDeleteNode
    else
    if MInsertNode then
      DoInsertNode
    else
    if MAddChildNode then
      DoAddChildNode
    else
    if MDuplicateNode then
      DuplicateNode
    else
    if MEditNode then
      DoEditNode
    else
    if MFindNode then
      DoFindNode
    else
    if MLoadTree then
      DoLoadTree
    else
    if MSaveTree then
      DoSaveTree
    else
    if MSaveTreeAs then
      DoSaveTreeAs
    else
    if MCloseTree then
      DoCloseTree;
  end;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
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
{$IFDEF VCL}
  HitTest: THitTests;
{$ENDIF VCL}
  N: TTreeNode;
  S: string;
begin
{$IFDEF VCL}
  HitTest := GetHitTestInfoAt(X, Y);
  if htOnLabel in HitTest then
  begin
{$ENDIF VCL}
    N := GetNodeAt(X, Y);
 {$IFDEF VisualCLX}
  if N <> nil then
  begin
 {$ENDIF VisualCLX}
     S := N.Text;
    if (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 7) = 'mailto:') then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end
  else
    Cursor := crDefault;
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

procedure TJvJanTreeView.NodeDuplicate(ATree: TJvJanTreeView;
  FromNode, ToNode: TTreeNode);
var
  I: Integer;
begin
  if FromNode.Count > 0 then
    for I := 1 to FromNode.Count do
    begin
      ATree.Items.AddChild(ToNode, FromNode.Item[I - 1].Text);
      if FromNode.Item[I - 1].Count > 0 then
        NodeDuplicate(ATree, FromNode.Item[I - 1], ToNode.Item[I - 1]);
    end;
end;

procedure TJvJanTreeView.ParserGetVar(Sender: TObject; VarName: string;
  var Value: Extended; var Found: Boolean);
var
  N: TTreeNode;
  Index: Integer;
begin
  Found := False;
  Index := FVarList.IndexOf(VarName);
  if Index <> -1 then
  begin
    N := TTreeNode(FVarList.Objects[Index]);
    if N.Count > 0 then
    try
      Value := StrToFloat(N.Item[0].Text);
      Found := True;
    except
    end;
  end
  else
  if LowerCase(VarName) = 'pi' then
  begin
    Value := Pi;
    Found := True;
  end;
end;

procedure TJvJanTreeView.ParserParseError(Sender: TObject; ParseError: Integer);
begin
  FParseError := True;
end;

procedure TJvJanTreeView.Recalculate;
var
  N, NV: TTreeNode;
  S: string;
  I, P: Integer;
begin
  if Items.Count = 0 then
    Exit;
  ParseVariables;
  for I := 0 to Items.Count - 1 do
  begin
    N := Items[I];
    S := N.Text;
    P := Pos('=', S);
    if P = 0 then
      Continue;
    S := Copy(S, P + 1, Length(S));
    if S = '' then
      Continue;
    FParser.ParseString := S;
    FParseError := False;
    FParser.Parse;
    if not FParseError then
    begin
      if N.Count = 0 then
        Items.AddChild(N, RsNew);
      NV := N.Item[0];
      NV.Text := FloatToStr(FParser.ParseValue);
    end
    else
    begin
      ShowMessageFmt(RsRecalculateErr, [S]);
      Exit;
    end;
  end;
end;

procedure TJvJanTreeView.ParseVariables;
var
  I, P: Integer;
  N: TTreeNode;
  S: string;
begin
  FVarList.Clear;
  if Items.Count = 0 then
    Exit;
  for I := 0 to Items.Count - 1 do
  begin
    N := Items[I];
    S := N.Text;
    P := Pos('=', S);
    if P = 0 then
      Continue;
    S := Copy(S, 1, P - 1);
    if S <> '' then
      FVarList.AddObject(S, TObject(N));
  end;
end;

{$IFDEF VisualCLX}
type
  TCustomViewItemAccessProtected = class(TCustomViewItem);

procedure TJvJanTreeView.DoCustomDrawItem(Sender: TCustomViewControl; Node: TCustomViewItem;
       Canvas: TCanvas; const Rect:TRect; State: TCustomDrawState;
       Stage: TCustomDrawStage; var DefaultDraw: Boolean);
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvJanTreeView.DoCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
{$ENDIF VCL}
var
  S: string;
  R: TRect;
begin
  {$IFDEF VCL}
  S := Node.Text;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  S := TCustomViewItemAccessProtected(Node).Caption;
  {$ENDIF VisualCLX}
  if (cdsSelected in State) or (cdsFocused in State) then
  begin
    DefaultDraw := True;
    Exit;
  end;
  if (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 7) = 'mailto:') then
    with Canvas do
    begin
      R := Node.DisplayRect{$IFDEF VCL}(True){$ENDIF};
      Font := Self.Font;
      Font.Style := Font.Style + [fsUnderline];
      Font.Color := clBlue;
      TextRect(R, R.Left, R.Top, S);
      DefaultDraw := False;
    end
  else
  if FColorFormulas and (Pos('=', S) > 0) then
    with Canvas do
    begin
      R := Node.DisplayRect{$IFDEF VCL}(True){$ENDIF};
      Font := Self.Font;
      Font.Color := FFormuleColor;
      TextRect(R, R.Left, R.Top, S);
      DefaultDraw := False;
    end
  else
    DefaultDraw := True;
end;

procedure TJvJanTreeView.SetColorFormulas(const Value: Boolean);
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
  Dlg: TOpenDialog;
  S: string;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.DefaultExt := FDefaultExt;
    S := FDefaultExt;
    if S = '' then
      S := '*';
    Dlg.Filter := RsTreeViewFiles + '|*.' + S;
    if Dlg.Execute then
    begin
      LoadFromFile(Dlg.FileName);
      FFileName := Dlg.FileName;
      Recalculate;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TJvJanTreeView.DoSaveTreeAs;
var
  Dlg: TSaveDialog;
  S: string;
begin
  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.DefaultExt := FDefaultExt;
    S := FDefaultExt;
    if S = '' then
      S := '*';
    Dlg.Filter := RsTreeViewFiles + '|*.' + S;
    if Dlg.Execute then
    begin
      SaveToFile(Dlg.FileName);
      FFileName := Dlg.FileName;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TJvJanTreeView.SetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvJanTreeView.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TJvJanTreeView.DoCloseTree;
begin
  if MessageDlg(RsSaveCurrentTree, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if FFileName <> '' then
      SaveToFile(FFileName)
    else
      DoSaveTreeAs;
  end;
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
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

procedure TTreeKeyMappings.SetFindNode(const Value: TShortCut);
begin
  FFindNode := Value;
end;

procedure TJvJanTreeView.DoFindNode;
var
  N: TTreeNode;
  I, FR: Integer;
  S: string;
begin
  N := Selected;
  if N = nil then
    Exit;
  S := InputBox(RsSearch, RsSearchFor, FSearchText);
  if S = '' then
    Exit;
  FSearchText := S;
  S := LowerCase(S);
  FR := N.AbsoluteIndex;
  if FR < Items.Count - 1 then
    for I := FR + 1 to Items.Count - 1 do
      if Pos(S, LowerCase(Items[I].Text)) > 0 then
      begin
        Selected := Items[I];
        Exit;
      end;
  ShowMessage(Format(RsNoMoresFound, [S]));
end;

//=== { TJvMathParser } ======================================================

constructor TJvMathParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { defaults }
  FInput := '';
end;

{ Finds the new state based on the just-completed production and the
  top state. }

function TJvMathParser.GotoState(Production: Word): Word;
var
  State: Word;
begin
  Result := 0; // removes warning
  State := Stack[StackTop].State;
  if Production <= 3 then
    case State of
      0:
        GotoState := 1;
      9:
        GotoState := 19;
      20:
        GotoState := 28;
    end
  else
  if Production <= 6 then
    case State of
      0, 9, 20:
        GotoState := 2;
      12:
        GotoState := 21;
      13:
        GotoState := 22;
    end
  else
  if (Production <= 8) or (Production = 100) then
    case State of
      0, 9, 12, 13, 20:
        GotoState := 3;
      14:
        GotoState := 23;
      15:
        GotoState := 24;
      16:
        GotoState := 25;
      40:
        GotoState := 80;
    end
  else
  if Production <= 10 then
    case State of
      0, 9, 12..16, 20, 40:
        GotoState := 4;
    end
  else
  if Production <= 12 then
    case State of
      0, 9, 12..16, 20, 40:
        GotoState := 6;
      5:
        GotoState := 17;
    end
  else
    case State of
      0, 5, 9, 12..16, 20, 40:
        GotoState := 8;
    end;
end;

{ Checks to see if the parser is about to read a function }

function TJvMathParser.IsFunc(S: string): Boolean;
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
  end;
  if UpperCase(FuncName) = S then
  begin
    SLen := Length(S);
    CurrToken.FuncName := UpperCase(Copy(FInput, Position, SLen));
    Position := Position + SLen;
    IsFunc := True;
  end
  else
    IsFunc := False;
end;

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
    Position := Position + 1;
  end;
  if Assigned(FOnGetVar) then
    FOnGetVar(Self, VarName, Value, VarFound);
  IsVar := VarFound;
end;

{ Gets the next Token from the Input stream }

function TJvMathParser.NextToken: TokenTypes;
var
  NumString: string[80];
  TLen, NumLen: Word;
  Check: Integer;
  Ch: Char;
  Decimal: Boolean;
begin
  NextToken := ttBad;
  while (Position <= Length(FInput)) and (FInput[Position] = ' ') do
    Position := Position + 1;
  TokenLen := Position;
  if Position > Length(FInput) then
  begin
    NextToken := ttEol;
    TokenLen := 0;
    Exit;
  end;
  Ch := UpCase(FInput[Position]);
  if Ch in ['!'] then
  begin
    NextToken := ttErr;
    TokenLen := 0;
    Exit;
  end;
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
    end;
    if (TLen = 2) and (Ch = '.') then
    begin
      NextToken := ttBad;
      TokenLen := 0;
      Exit;
    end;
    if (TLen <= Length(FInput)) and (UpCase(FInput[TLen]) = 'E') then
    begin
      NumString := NumString + 'E';
      Inc(TLen);
      if FInput[TLen] in ['+', '-'] then
      begin
        NumString := NumString + FInput[TLen];
        Inc(TLen);
      end;
      NumLen := 1;
      while (TLen <= Length(FInput)) and (FInput[TLen] in DigitSymbols) and
        (NumLen <= MaxExpLen) do
      begin
        NumString := NumString + FInput[TLen];
        Inc(NumLen);
        Inc(TLen);
      end;
    end;
    if NumString[1] = '.' then
      NumString := '0' + NumString;
    Val(NumString, CurrToken.Value, Check);
    if Check <> 0 then
    begin
      MathError := True;
      TokenError := ErrInvalidNum;
      Position := Position + Pred(Check);
    end
    else
    begin
      NextToken := ttNum;
      Position := Position + System.Length(NumString);
      TokenLen := Position - TokenLen;
    end;
    Exit;
  end
  else
  if Ch in IdentifierLetters then
  begin
    if IsFunc('ABS') or IsFunc('ATAN') or IsFunc('COS') or
      IsFunc('EXP') or IsFunc('LN') or IsFunc('ROUND') or
      IsFunc('SIN') or IsFunc('SQRT') or IsFunc('SQR') or IsFunc('TRUNC') then
    begin
      NextToken := ttFunc;
      TokenLen := Position - TokenLen;
      Exit;
    end;
    if IsFunc('MOD') then
    begin
      NextToken := ttModu;
      TokenLen := Position - TokenLen;
      Exit;
    end;
    if IsVar(CurrToken.Value) then
    begin
      NextToken := ttNum;
      TokenLen := Position - TokenLen;
      Exit;
    end
    else
    begin
      NextToken := ttBad;
      TokenLen := 0;
      Exit;
    end;
  end
  else
  begin
    case Ch of
      '+':
        NextToken := ttPlus;
      '-':
        NextToken := ttMinus;
      '*':
        NextToken := ttTimes;
      '/':
        NextToken := ttDivide;
      '^':
        NextToken := ttExpo;
      '(':
        NextToken := ttOParen;
      ')':
        NextToken := ttCParen;
    else
      begin
        NextToken := ttBad;
        TokenLen := 0;
        Exit;
      end;
    end;
    Position := Position + 1;
    TokenLen := Position - TokenLen;
    Exit;
  end;
end;

{ Pops the top Token off of the stack }

procedure TJvMathParser.Pop(var Token: TokenRec);
begin
  Token := Stack[StackTop];
  Dec(StackTop);
end;

{ Pushes a new Token onto the stack }

procedure TJvMathParser.Push(Token: TokenRec);
begin
  if StackTop = ParserStackSize then
    TokenError := ErrParserStack
  else
  begin
    Inc(StackTop);
    Stack[StackTop] := Token;
  end;
end;

{ Parses an input stream }

procedure TJvMathParser.Parse;
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
          if TokenType = ttNum then
            Shift(10)
          else
          if TokenType = ttFunc then
            Shift(11)
          else
          if TokenType = ttMinus then
            Shift(5)
          else
          if TokenType = ttOParen then
            Shift(9)
          else
          if TokenType = ttErr then
          begin
            MathError := True;
            Accepted := True;
          end
          else
          begin
            TokenError := ErrExpression;
            Position := Position - TokenLen;
          end;
        end;
      1:
        begin
          if TokenType = ttEol then
            Accepted := True
          else
          if TokenType = ttPlus then
            Shift(12)
          else
          if TokenType = ttMinus then
            Shift(13)
          else
          begin
            TokenError := ErrOperator;
            Position := Position - TokenLen;
          end;
        end;
      2:
        begin
          if TokenType = ttTimes then
            Shift(14)
          else
          if TokenType = ttDivide then
            Shift(15)
          else
            Reduce(3);
        end;
      3:
        begin
          if TokenType = ttModu then
            Shift(40)
          else
            Reduce(6);
        end;
      4:
        begin
          if TokenType = ttExpo then
            Shift(16)
          else
            Reduce(8);
        end;
      5:
        begin
          if TokenType = ttNum then
            Shift(10)
          else
          if TokenType = ttFunc then
            Shift(11)
          else
          if TokenType = ttOParen then
            Shift(9)
          else
          begin
            TokenError := ErrExpression;
            Position := Position - TokenLen;
          end;
        end;
      6:
        Reduce(10);
      7:
        Reduce(13);
      8:
        Reduce(12);
      10:
        Reduce(15);
      11:
        if TokenType = ttOParen then
          Shift(20)
        else
        begin
          TokenError := ErrOpenParen;
          Position := Position - TokenLen;
        end;
      17:
        Reduce(9);
      18:
        raise EJVCLException.CreateRes(@RsEBadTokenState);
      19:
        if TokenType = ttPlus then
          Shift(12)
        else
        if TokenType = ttMinus then
          Shift(13)
        else
        if TokenType = ttCParen then
          Shift(27)
        else
        begin
          TokenError := ErrOpCloseParen;
          Position := Position - TokenLen;
        end;
      21:
        if TokenType = ttTimes then
          Shift(14)
        else
        if TokenType = ttDivide then
          Shift(15)
        else
          Reduce(1);
      22:
        if TokenType = ttTimes then
          Shift(14)
        else
        if TokenType = ttDivide then
          Shift(15)
        else
          Reduce(2);
      23:
        Reduce(4);
      24:
        Reduce(5);
      25:
        Reduce(7);
      26:
        Reduce(11);
      27:
        Reduce(14);
      28:
        if TokenType = ttPlus then
          Shift(12)
        else
        if TokenType = ttMinus then
          Shift(13)
        else
        if TokenType = ttCParen then
          Shift(29)
        else
        begin
          TokenError := ErrOpCloseParen;
          Position := Position - TokenLen;
        end;
      29:
        Reduce(16);
      80:
        Reduce(100);
    end;
  until Accepted or (TokenError <> 0);
  if TokenError <> 0 then
  begin
    if TokenError = ErrBadRange then
       Position := Position - TokenLen;
    if Assigned(FOnParseError) then
      FOnParseError(Self, TokenError);
  end;
  if MathError or (TokenError <> 0) then
  begin
    ParseError := True;
    ParseValue := 0;
    Exit;
  end;
  ParseError := False;
  ParseValue := Stack[StackTop].Value;
end;

{ Completes a reduction }

procedure TJvMathParser.Reduce(Reduction: Word);
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
        else
        if (Token1.Value * Ln(Token2.Value) < -ExpLimit) or
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
    11:
      raise EJVCLException.CreateRes(@RsEInvalidReduction);
    13:
      raise EJVCLException.CreateRes(@RsEInvalidReduction);
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
        else
        if Token1.FuncName = 'ATAN' then
          CurrToken.Value := ArcTan(CurrToken.Value)
        else
        if Token1.FuncName = 'COS' then
        begin
          if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
          else
            CurrToken.Value := Cos(CurrToken.Value)
        end
        else
        if Token1.FuncName = 'EXP' then
        begin
          if (CurrToken.Value < -ExpLimit) or (CurrToken.Value > ExpLimit) then
            MathError := True
          else
            CurrToken.Value := Exp(CurrToken.Value);
        end
        else
        if Token1.FuncName = 'LN' then
        begin
          if CurrToken.Value <= 0 then
            MathError := True
          else
            CurrToken.Value := Ln(CurrToken.Value);
        end
        else
        if Token1.FuncName = 'ROUND' then
        begin
          if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
            MathError := True
          else
            CurrToken.Value := Round(CurrToken.Value);
        end
        else
        if Token1.FuncName = 'SIN' then
        begin
          if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
          else
            CurrToken.Value := Sin(CurrToken.Value)
        end
        else
        if Token1.FuncName = 'SQRT' then
        begin
          if CurrToken.Value < 0 then
            MathError := True
          else
            CurrToken.Value := Sqrt(CurrToken.Value);
        end
        else
        if Token1.FuncName = 'SQR' then
        begin
          if (CurrToken.Value < -SqrLimit) or (CurrToken.Value > SqrLimit) then
            MathError := True
          else
            CurrToken.Value := Sqr(CurrToken.Value);
        end
        else
        if Token1.FuncName = 'TRUNC' then
        begin
          if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
            MathError := True
          else
            CurrToken.Value := Trunc(CurrToken.Value);
        end;
      end;
    3, 6, 8, 10, 12, 15:
      Pop(CurrToken);
  end;
  CurrToken.State := GotoState(Reduction);
  Push(CurrToken);
end;

{ Shifts a Token onto the stack }

procedure TJvMathParser.Shift(State: Word);
begin
  CurrToken.State := State;
  Push(CurrToken);
  TokenType := NextToken;
end;

//=== { TTreeKeyMappings } ===================================================

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

procedure TTreeKeyMappings.SetDuplicateNode(const Value: TShortCut);
begin
  FDuplicateNode := Value;
end;

procedure TTreeKeyMappings.SetEditNode(const Value: TShortCut);
begin
  FEditNode := Value;
end;

procedure TJvJanTreeView.KeyPress(var Key: Char);
begin
  if Key = Char(VK_RETURN) then
    Recalculate;
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

end.
