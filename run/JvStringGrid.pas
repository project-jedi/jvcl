{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain A copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringGrid.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStringGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Grids,
  JvTypes, JVCLVer;

const
  GM_ACTIVATECELL = WM_USER + 123;

type
  // (rom) renamed elements made packed
  TGMActivateCell = packed record
    Msg: Cardinal;
    Column: Integer;
    Row: Integer;
    Result: Integer;
  end;

  TJvStringGrid = class;
  TExitCellEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer;
    const EditText: string) of object;
  TGetCellAlignmentEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer;
    State: TGridDrawState; var CellAlignment: TAlignment) of object;
  TCaptionClickEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer) of object;
  TJvSortType = (stAutomatic, stClassic, stCaseSensitive, stNumeric, stDate, stCurrency);
  TProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvStringGrid = class(TStringGrid)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FAlignment: TAlignment;
    FSetCanvasProperties: TDrawCellEvent;
    FGetCellAlignment: TGetCellAlignmentEvent;
    FCaptionClick: TCaptionClickEvent;
    FCellOnMouseDown: TGridCoord;
    FOnMouseEnter: TNotifyEvent;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnExitCell: TExitCellEvent;
    FOnLoadProgress: TProgress;
    FOnSaveProgress: TProgress;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    procedure GMActivateCell(var Msg: TGMActivateCell); message GM_ACTIVATECELL;
    procedure SetAlignment(const Value: TAlignment);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
  protected
    function CreateEditor: TInplaceEdit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ExitCell(const EditText: string; AColumn, ARow: Integer); virtual;
    procedure SetCanvasProperties(AColumn, ARow: Longint;
      Rect: TRect; State: TGridDrawState); virtual;
    procedure DrawCell(AColumn, ARow: Longint;
      Rect: TRect; State: TGridDrawState); override;
    procedure CaptionClick(AColumn, ARow: LongInt); dynamic;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    function GetCellAlignment(AColumn, ARow: Longint;
      State: TGridDrawState): TAlignment; virtual;
    procedure DefaultDrawCell(AColumn, ARow: Longint;
      Rect: TRect; State: TGridDrawState); virtual;
    procedure ActivateCell(AColumn, ARow: Integer);
    procedure InvalidateCell(AColumn, ARow: Integer);
    procedure InvalidateCol(AColumn: Integer);
    procedure InvalidateRow(ARow: Integer);
    property InplaceEditor;
    procedure SortGrid(Column: Integer; Ascending: Boolean = True; Fixed: Boolean = False;
      SortType: TJvSortType = stClassic; BlankTop: Boolean = True);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property OnExitCell: TExitCellEvent read FOnExitCell write FOnExitCell;
    property OnSetCanvasProperties: TDrawCellEvent read FSetCanvasProperties write FSetCanvasProperties;
    property OnGetCellAlignment: TGetCellAlignmentEvent read FGetCellAlignment write FGetCellAlignment;
    property OnCaptionClick: TCaptionClickEvent read FCaptionClick write FCaptionClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnLoadProgress: TProgress read FOnLoadProgress write FOnLoadProgress;
    property OnSaveProgress: TProgress read FOnSaveProgress write FOnSaveProgress;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
  end;

implementation

//=== TExInplaceEdit =========================================================

type
  TExInplaceEdit = class(TInplaceEdit)
  private
    FLastCol: Integer;
    FLastRow: Integer;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  public
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure TExInplaceEdit.CreateParams(var Params: TCreateParams);
const
  Flags: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Flags[TJvStringGrid(Grid).Alignment];
end;

procedure TExInplaceEdit.WMKillFocus(var Msg: TMessage);
begin
  TJvStringGrid(Grid).ExitCell(Text, FLastCol, FLastRow);
  inherited;
end;

procedure TExInplaceEdit.WMSetFocus(var Msg: TMessage);
begin
  FLastCol := TJvStringGrid(Grid).Col;
  FLastRow := TJvStringGrid(Grid).Row;
  inherited;
end;

//=== TJvStringGrid ==========================================================

constructor TJvStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvStringGrid.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvStringGrid.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvStringGrid.SortGrid(Column: Integer; Ascending,
  Fixed: Boolean; SortType: TJvSortType; BlankTop: Boolean);
var
  St: string;
  TmpC: Currency;
  TmpF: Double;
  TmpD: TDateTime;
  LStart: Integer;
  LEnd: Integer;

  procedure ExchangeGridRows(I, J: Integer);
  var
    K: Integer;
  begin
    if Fixed then
      for K := 0 to ColCount - 1 do
        Cols[K].Exchange(I, J)
    else
      for K := FixedCols to ColCount - 1 do
        Cols[K].Exchange(I, J);
  end;

  function IsSmaller(First, Second: string): Boolean;
  var
    I, J: Int64;
    F1, F2: Double;
    C1, C2: Currency;
  begin
    try
      I := StrToInt64(First);
      J := StrToInt64(Second);
      Result := I < J;
      Exit;
    except
    end;
    try
      F1 := StrToFloat(First);
      F2 := StrToFloat(Second);
      Result := F1 < F2;
      Exit;
    except
    end;
    try
      C1 := StrToCurr(First);
      C2 := StrToCurr(Second);
      Result := C1 < C2;
      Exit;
    except
    end;
    Result := First > Second;
  end;

  function IsBigger(First, Second: string): Boolean;
  begin
    Result := IsSmaller(Second, First);
  end;

  // (rom) a HeapSort has no worst case for O(X)
  // (rom) i donated one a long time ago to JCL
  procedure QuickSort(L, R: Integer);
  var
    I, J, M: Integer;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) div 2;
      St := Cells[Column, M];
      repeat
        case SortType of
          stClassic:
            begin
              while CompareText(Cells[Column, I], St) < 0 do
                Inc(I);
              while CompareText(Cells[Column, J], St) > 0 do
                Dec(J);
            end;
          stCaseSensitive:
            begin
              while CompareStr(Cells[Column, I], St) < 0 do
                Inc(I);
              while CompareStr(Cells[Column, J], St) > 0 do
                Dec(J);
            end;
          stNumeric:
            begin
              TmpF := StrToFloat(St);
              while StrToFloat(Cells[Column, I]) < TmpF do
                Inc(I);
              while StrToFloat(Cells[Column, J]) > TmpF do
                Dec(J);
            end;
          stDate:
            begin
              TmpD := StrToDateTime(St);
              while StrToDateTime(Cells[Column, I]) < TmpD do
                Inc(I);
              while StrToDateTime(Cells[Column, J]) > TmpD do
                Dec(J);
            end;
          stCurrency:
            begin
              TmpC := StrToCurr(St);
              while StrToCurr(Cells[Column, I]) < TmpC do
                Inc(I);
              while StrToCurr(Cells[Column, J]) > TmpC do
                Dec(J);
            end;
          stAutomatic:
            begin
              while IsSmaller(Cells[Column, I], St) do
                Inc(I);
              while IsBigger(Cells[Column, J], St) do
                Dec(J);
            end;
        end;
        if I <= J then
        begin
          if I <> J then
            ExchangeGridRows(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

  procedure InvertGrid;
  var
    I, J: Integer;
  begin
    I := FixedRows;
    J := RowCount - 1;
    while I < J do
    begin
      ExchangeGridRows(I, J);
      Inc(I);
      Dec(J);
    end;
  end;

  function MoveBlankTop: Integer;
  var
    I, J: Integer;
  begin
    I := FixedRows;
    Result := I;
    J := RowCount - 1;
    while I <= J do
    begin
      if Trim(Cells[Column, I]) = '' then
      begin
        ExchangeGridRows(Result, I);
        Inc(Result);
      end;
      Inc(I);
    end;
  end;

begin
  if (Column >= 0) and (Column < ColCount) then
  begin
    LStart := FixedRows;
    LEnd := RowCount - 1;
    if BlankTop then
      LStart := MoveBlankTop;

    QuickSort(LStart, LEnd);
    if not Ascending then
      InvertGrid;
  end;
end;

procedure TJvStringGrid.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  // (rom) secured
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvStringGrid.LoadFromCSV(FileName: string; Separator: Char);
var
  St, st2: string;
  I, J, K, L, M, N: Integer;
  fich: TextFile;
  FilePos, Count: Integer;
  f: file of Byte;
begin
  FilePos := 0;
  AssignFile(f, FileName);
  Reset(f);
  Count := FileSize(f);
  CloseFile(f);
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, FilePos, Count);

  AssignFile(fich, FileName);
  Reset(fich);
  K := 0;
  while not Eof(fich) do
  begin
    Readln(fich, St);
    FilePos := FilePos + Length(St) + 2;
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, FilePos, Count);

    //Analyse St
    J := 0;
    L := 1;
    for I := 1 to Length(St) do
      if St[I] = '"' then
        J := (J + 1) mod 2
      else
      if St[I] = Separator then
        if J = 0 then
          Inc(L);
    if ColCount < L then
      ColCount := L;
    Inc(K);
    if RowCount < K then
      RowCount := K;

    J := 0;
    M := Pos(Separator, St);
    N := Pos('"', St);
    while M <> 0 do
    begin
      if (N = 0) or (N > M) then
      begin
        Cells[J, K - 1] := Copy(St, 1, M - 1);
        St := Copy(St, M + 1, Length(St));
      end
      else
      begin
        St := Copy(St, N + 1, Length(St));
        N := Pos('"', St);
        st2 := Copy(St, 1, N - 1);
        St := Copy(St, N + 1, Length(St));
        M := Pos(Separator, St);
        if M <> 0 then
          St := Copy(St, M + 1, Length(St))
        else
          St := '';
        Cells[J, K - 1] := st2;
      end;
      Inc(J);

      M := Pos(Separator, St);
      N := Pos('"', St);
    end;
    if St <> '' then
      Cells[J, K - 1] := St;
  end;
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, Count, Count);
  CloseFile(fich);
end;

procedure TJvStringGrid.LoadFromStream(Stream: TStream);
var
  Col, Rom, I, Count: Integer;
  Buffer: array [0..1024] of Byte;
  St: string;
begin
  Col := 0;
  Rom := 1;
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, 0, Stream.Size);
  while Stream.Position < Stream.Size do
  begin
    Count := Stream.Read(Buffer, 1024);
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
      case Buffer[I] of
        0:
          begin
            Inc(Col);
            if Rom > RowCount then
              RowCount := Rom;
            if Col > ColCount then
              ColCount := Col;
            Cells[Col - 1, Rom - 1] := St;
            St := '';
          end;
        1:
          begin
            Inc(Col);
            if Col > ColCount then
              ColCount := Col;
            Cells[Col - 1, Rom - 1] := St;
            Inc(Rom);
            if Rom > RowCount then
              RowCount := Row;
            Col := 0;
            St := '';
          end;
      else
        St := St + Char(Buffer[I]);
      end;
  end;
  RowCount := RowCount - 1;
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, Stream.Size, Stream.Size);
end;

procedure TJvStringGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvStringGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvStringGrid.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvStringGrid.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvStringGrid.SaveToFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  // (rom) secured
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvStringGrid.SaveToCSV(FileName: string; Separator: Char);
var
  St: string;
  I, J: Integer;
  fich: TextFile;
begin
  AssignFile(fich, FileName);
  Rewrite(fich);
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, 0, RowCount * ColCount);
  for I := 0 to RowCount - 1 do
  begin
    St := '';
    for J := 0 to ColCount - 1 do
    begin
      if Assigned(FOnSaveProgress) then
        FOnSaveProgress(Self, I * ColCount + J, RowCount * ColCount);
      if Pos(Separator, Cells[J, I]) = 0 then
        St := St + Cells[J, I]
      else
        St := St + '"' + Cells[J, I] + '"';
      if J <> ColCount - 1 then
        St := St + Separator
    end;
    Writeln(fich, St);
  end;
  CloseFile(fich);
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, RowCount * ColCount, RowCount * ColCount);
end;

procedure TJvStringGrid.SaveToStream(Stream: TStream);
var
  I, J, K: Integer;
  St: array [0..1000] of Char;
  Stt: string;
  A, B: Byte;
begin
  A := 0;
  B := 1; //A for end of string, B for end of line
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, 0, RowCount * ColCount);
  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if Assigned(FOnSaveProgress) then
        FOnSaveProgress(Self, I * ColCount + J, RowCount * ColCount);
      Stt := Cells[J, I];
      for K := 1 to Length(Stt) do
        St[K - 1] := Stt[K];
      Stream.Write(St, Length(Cells[J, I]));
      if J <> ColCount - 1 then
        Stream.Write(A, 1);
    end;
    Stream.Write(B, 1);
  end;
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, RowCount * ColCount, RowCount * ColCount);
end;

procedure TJvStringGrid.ActivateCell(AColumn, ARow: Integer);
begin
  PostMessage(Handle, GM_ACTIVATECELL, AColumn, ARow);
end;

procedure TJvStringGrid.CaptionClick(AColumn, ARow: Integer);
begin
  if Assigned(FCaptionClick) then
    FCaptionClick(Self, AColumn, ARow);
end;

function TJvStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TExInplaceEdit.Create(Self);
end;

procedure TJvStringGrid.DefaultDrawCell(AColumn, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
const
  Flags: array [TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  S: string;
begin
  Canvas.FillRect(Rect);
  S := Cells[AColumn, ARow];
  if Length(S) > 0 then
  begin
    InflateRect(rect, -2, -2);
    DrawText(Canvas.Handle, PChar(S), Length(S), rect,
      DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or
      Flags[GetCellAlignment(AColumn, ARow, state)]);
  end;
end;

procedure TJvStringGrid.DrawCell(AColumn, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
begin
  if Assigned(OnDrawCell) then
    inherited DrawCell(AColumn, ARow, Rect, State)
  else
  begin
    SetCanvasProperties(AColumn, ARow, Rect, State);
    DefaultDrawCell(AColumn, ARow, Rect, State);
    Canvas.Font := Font;
    Canvas.Brush := Brush;
  end;
end;

procedure TJvStringGrid.ExitCell(const EditText: string; AColumn,
  ARow: Integer);
begin
  if Assigned(FOnExitCell) then
    FOnExitCell(Self, AColumn, ARow, EditText);
end;

function TJvStringGrid.GetCellAlignment(AColumn, ARow: Integer;
  State: TGridDrawState): TAlignment;
begin
  Result := FAlignment;
  if Assigned(FGetCellAlignment) then
    FGetCellAlignment(Self, AColumn, ARow, State, Result);
end;

procedure TJvStringGrid.GMActivateCell(var Msg: TGMActivateCell);
begin
  Col := Msg.Column;
  Row := Msg.Row;
  EditorMode := True;
  InplaceEditor.SelectAll;
end;

procedure TJvStringGrid.InvalidateCell(AColumn, ARow: Integer);
begin
  inherited InvalidateCell(AColumn, ARow);
end;

procedure TJvStringGrid.InvalidateCol(AColumn: Integer);
begin
  inherited InvalidateCol(AColumn);
end;

procedure TJvStringGrid.InvalidateRow(ARow: Integer);
begin
  inherited InvalidateRow(ARow);
end;

procedure TJvStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    MouseToCell(X, Y, FCellOnMouseDown.X, FCellOnMouseDown.Y)
  else
    FCellOnMouseDown := TGridCoord(Point(-1, -1));
end;

procedure TJvStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
begin
  if Button = mbLeft then
    MouseToCell(X, Y, Cell.X, Cell.Y);
  if CompareMem(@Cell, @FCellOnMouseDown, SizeOf(Cell)) and
    ((Cell.X < FixedCols) or (Cell.Y < FixedRows)) then
    CaptionClick(Cell.X, Cell.Y);
  FCellOnMouseDown := TGridCoord(Point(-1, -1));
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvStringGrid.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
    if Assigned(InplaceEditor) then
      TExInplaceEdit(InplaceEditor).RecreateWnd;
  end;
end;

procedure TJvStringGrid.SetCanvasProperties(AColumn, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if Assigned(FSetCanvasProperties) then
    FSetCanvasProperties(Self, AColumn, ARow, Rect, State);
end;

procedure TJvStringGrid.WMCommand(var Msg: TWMCommand);
begin
  if EditorMode and (Msg.Ctl = InplaceEditor.Handle) then
    inherited
  else
  if Msg.Ctl <> 0 then
    Msg.Result := SendMessage(Msg.Ctl, CN_COMMAND, TMessage(Msg).WParam, TMessage(Msg).LParam);
end;

end.

