{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCharMap.PAS, released on 2003-11-03.

The Initial Developer of the Original Code is Peter Thornqvist.
Portions created by Peter Thornqvist are Copyright (c) 2003 Peter Thornqvist
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvCharMap;

interface
uses
  Windows, Messages, Controls, SysUtils, Classes, Grids, JVCLVer;

type
  TJvCharMapRange = class(TPersistent)
  private
    FStartChar: Word;
    FEndChar: Word;
    FOnChange: TNotifyEvent;
    procedure SetEndChar(const Value: Word);
    procedure SetStartChar(const Value: Word);
    procedure Change;
  published
    property StartChar: Word read FStartChar write SetStartChar default 33;
    property EndChar: Word read FEndChar write SetEndChar default 255;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$IFDEF COMPILER6_UP}
  TJvCustomCharMap = class(TCustomDrawGrid)
{$ELSE}
  TJvCustomCharMap = class(TCustomGrid)
{$ENDIF}
  private
    FCharPanel: TCustomControl;
    FShowZoomPanel: boolean;
    FAboutJVCL: TJVCLAboutInfo;
    FMouseIsDown: boolean;
    FCharRange: TJvCharMapRange;
    procedure SetCharRange(const Value: TJvCharMapRange);
    procedure SetPanelVisible(const Value: boolean);
    function GetCharacter: WideChar;
    function GetColumns: integer;
    procedure SetColumns(const Value: integer);
    procedure SetShowZoomPanel(const Value: boolean);
    function GetPanelVisible: boolean;
  private
    FAutoSizeHeight: boolean;
    FAutoSizeWidth: boolean;
    procedure SetAutoSizeHeight(const Value: boolean);
    procedure SetAutoSizeWidth(const Value: boolean);
  protected
    procedure ShowCharPanel(ACol, ARow: integer); virtual;
    procedure RecalcCells; virtual;
    procedure AdjustSize; reintroduce;
    procedure CreateHandle; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;

    function GetChar(ACol, ARow: integer): WideChar; virtual;
    function GetCharInfo(ACol, ARow: integer): Word; overload; virtual;
    function GetCharInfo(AChar: WideChar): Word; overload; virtual;
    function IsValidChar(AChar: WideChar): boolean; virtual;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DoRangeChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CellSize: TSize;
{$IFNDEF COMPILER6_UP}
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
{$ENDIF}

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
{$IFNDEF COMPILER6_UP}
    property ColCount;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property GridLineWidth;
    property Options;
    property RowCount;
{$ENDIF}
  protected
    property Character: WideChar read GetCharacter;
    property PanelVisible: boolean read GetPanelVisible write SetPanelVisible stored false;
    property ShowZoomPanel: boolean read FShowZoomPanel write SetShowZoomPanel default true;
    property CharRange: TJvCharMapRange read FCharRange write SetCharRange;
    property AutoSizeWidth: boolean read FAutoSizeWidth write SetAutoSizeWidth default false;
    property AutoSizeHeight: boolean read FAutoSizeHeight write SetAutoSizeHeight default false;
    property Columns: integer read GetColumns write SetColumns default 20;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCharMap = class(TJvCustomCharMap)
  public
    property Character;
    property PanelVisible;
  published
    property AutoSizeWidth;
    property AutoSizeHeight;
    property CharRange;
    property Columns;
    property ShowZoomPanel;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF COMPILER6_UP}
    property OnTopLeftChanged;
{$ENDIF}
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnResize;
  end;

implementation
uses
  Forms, Graphics;

type
  TCharZoomPanel = class(TCustomControl)
  private
    FCharacter: WideChar;
    FEndChar: Word;
    FOldWndProc: TWndMethod;
    FWasVisible: boolean;
    procedure SetCharacter(const Value: WideChar);
    procedure FormWindowProc(var Message: TMessage);
    procedure HookWndProc;
    procedure UnhookWndProc;
  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Character: WideChar read FCharacter write SetCharacter;
  end;

type
  TAccessCanvas = class(TCanvas);

procedure WideDrawText(Canvas: TCanvas; const Text: WideString; ARect: TRect; uFormat: Cardinal);
begin
  // (p3) TAccessCanvas bit stolen from Troy Wolbrink's TNT controls (not that it makes any difference AFAICS)
  with TAccessCanvas(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    if CanvasOrientation = coRightToLeft then
      Inc(uFormat, DT_RTLREADING);
    DrawTextW(Handle, PWideChar(Text), Length(Text), ARect, uFormat);
    Changed;
  end;
end;

{ TJvCustomCharMap }

procedure TJvCustomCharMap.AdjustSize;
var
  AWidth, AHeight: integer;
begin
  if HandleAllocated and (ColCount > 0) and (RowCount > 0) then
  begin
    AWidth := DefaultColWidth * (ColCount) + ColCount;
    AHeight := DefaultRowHeight * (RowCount) + RowCount;
    if AutoSizeWidth and (ClientWidth <> AWidth) and (Align in [alNone, alLeft, alRight]) then
      ClientWidth := AWidth;
    if AutoSizeHeight and (ClientHeight <> AHeight) and (Align in [alNone, alTop, alBottom]) then
      ClientHeight := AHeight;
  end;
end;

function TJvCustomCharMap.CellSize: TSize;
begin
  Result.cx := DefaultColWidth;
  Result.cy := DefaultRowHeight;
end;

procedure TJvCustomCharMap.CMFontchanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then AdjustSize;
  RecalcCells;
end;

constructor TJvCustomCharMap.Create(AOwner: TComponent);
begin
  inherited;
  FCharRange := TJvCharMapRange.Create;
  FCharRange.StartChar := 33;
  FCharRange.EndChar := 255;
  FCharRange.OnChange := DoRangeChange;
  FCharPanel := TCharZoomPanel.Create(self);
  FCharPanel.Visible := false;
  FCharPanel.Parent := self;

//  DoubleBuffered := true;
//  DefaultDrawing := false;
  Options := [goVertLine, goHorzLine, {goDrawFocusSelected, } goThumbTracking];
  FShowZoomPanel := true;
  DefaultRowHeight := abs(Font.Height) + 12;
  DefaultColWidth := DefaultRowHeight - 5;
  Columns := 20;
end;

procedure TJvCustomCharMap.CreateHandle;
begin
  inherited;
  RecalcCells;
end;

destructor TJvCustomCharMap.Destroy;
begin
  FCharRange.Free;
  inherited;
end;

function TJvCustomCharMap.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  inherited DoMouseWheelDown(Shift, MousePos);
  Result := PanelVisible and SelectCell(Col, Row);
  if Result then
    ShowCharPanel(Col, Row);
end;

function TJvCustomCharMap.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  inherited DoMouseWheelUp(Shift, MousePos);
  Result := PanelVisible and SelectCell(Col, Row);
  if Result then
    ShowCharPanel(Col, Row);
end;

procedure TJvCustomCharMap.DoRangeChange(Sender: TObject);
begin
  TCharZoomPanel(FCharPanel).FEndChar := CharRange.EndChar;
  RecalcCells;
end;

procedure TJvCustomCharMap.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  AChar: WideChar;
begin
{$IFDEF COMPILER6_UP}
  inherited;
{$ENDIF}
  Canvas.Brush.Color := Color;
  Canvas.Font := Font;
  Canvas.Pen.Color := Font.Color;
  if AState * [gdSelected, gdFocused] <> [] then
  begin
    Canvas.Pen.Color := Font.Color;
    InflateRect(ARect, -1, -1);
    Canvas.Rectangle(ARect);
    InflateRect(ARect, 1, 1);
  end
  else
    Canvas.FillRect(ARect);
  AChar := GetChar(ACol, ARow);
  SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
  if IsValidChar(AChar) then
    WideDrawText(Canvas, AChar, ARect, DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
end;

function TJvCustomCharMap.GetChar(ACol, ARow: integer): WideChar;
begin
  if (ARow < 0) or (ACol < 0) then
    Result := WideChar(0)
  else
    Result := WideChar(CharRange.StartChar + ARow * ColCount + ACol);
end;

function TJvCustomCharMap.GetCharacter: WideChar;
begin
  Result := GetChar(Col, Row);
end;

function TJvCustomCharMap.GetCharInfo(ACol, ARow: integer): Word;
begin
  Result := GetCharInfo(GetChar(ACol, ARow));
end;

function TJvCustomCharMap.GetCharInfo(AChar: WideChar): Word;
var
  ACharInfo: Word;
begin
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE3, @AChar, 1, ACharInfo) then
    Result := ACharInfo
  else
    Result := 0;
end;

function TJvCustomCharMap.GetColumns: integer;
begin
  Result := ColCount;
end;

function TJvCustomCharMap.GetPanelVisible: boolean;
begin
  if (FCharPanel <> nil) and (Parent <> nil) and not (csDesigning in ComponentState) then
    Result := FCharPanel.Visible
  else
    Result := false;
end;

function TJvCustomCharMap.IsValidChar(AChar: WideChar): boolean;
var
  ACharInfo: word;
begin
  ACharInfo := GetCharInfo(AChar);
  Result := (AChar >= WideChar(CharRange.StartChar)) and (AChar <= WideChar(CharRange.EndChar)) and (ACharInfo <> 0); //  and (ACharInfo and C1_CNTRL <> C1_CNTRL);
end;

procedure TJvCustomCharMap.KeyDown(var Key: Word; Shift: TShiftState);
var
  ACol, ARow: integer;
begin
  // store previous location
  ACol := Col;
  ARow := Row;
  // update new location
  inherited;
  case Key of
    VK_RETURN:
      ShowCharPanel(Col, Row);
    VK_SPACE:
      if not (ssAlt in Shift) then
        PanelVisible := not PanelVisible;
    VK_ESCAPE:
      PanelVisible := false;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
      if PanelVisible then
        ShowCharPanel(Col, Row);
    VK_LEFT:
      begin
        if (ACol = 0) and (ARow > 0) then
        begin
          ACol := ColCount - 1;
          Dec(ARow);
        end
        else
        begin
          ACol := Col;
          ARow := Row;
        end;
        Col := ACol;
        Row := ARow;
        if PanelVisible then
          ShowCharPanel(ACol, ARow);
      end;
    VK_RIGHT:
      begin
        if (ACol = ColCount - 1) and (ARow < RowCount - 1) then
        begin
          ACol := 0;
          Inc(ARow);
        end
        else
        begin
          ACol := Col;
          ARow := Row;
        end;
        Col := ACol;
        Row := ARow;
        if PanelVisible then
          ShowCharPanel(ACol, ARow);
      end;
  end;
end;

procedure TJvCustomCharMap.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  GC: TGridCoord;
  ACol, ARow: integer;
begin
  inherited;
//  MouseCapture := true;
  if Button = mbLeft then
  begin
    FMouseIsDown := true;
    GC := MouseCoord(X, Y);
    MouseToCell(X, Y, ACol, ARow);
    if SelectCell(ACol, ARow) then
      ShowCharPanel(ACol, ARow)
    else if SelectCell(Col, Row) then
      ShowCharPanel(Col, Row);
  end;
end;

procedure TJvCustomCharMap.MouseMove(Shift: TShiftState; X, Y: Integer);
//var
//  ACol, ARow: integer;
begin
  inherited;
{  if (csLButtonDown in ControlState) then
  begin
    MouseToCell(X, Y, ACol, ARow);
    if SelectCell(ACol, ARow) then
      ShowCharPanel(ACol, ARow);
  end;}
end;

{$IFNDEF COMPILER6_UP}

procedure TJvCustomCharMap.MouseToCell(X, Y: Integer; var ACol,
  ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;
{$ENDIF}

procedure TJvCustomCharMap.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ACol, ARow: integer;
begin
  inherited;
  if (Button = mbLeft) and FMouseIsDown then
  begin
    FMouseIsDown := false;
    MouseToCell(X, Y, ACol, ARow);
    if SelectCell(ACol, ARow) then
      ShowCharPanel(ACol, ARow)
    else if SelectCell(Col, Row) then
      ShowCharPanel(Col, Row);
  end;
end;

procedure TJvCustomCharMap.RecalcCells;
var
  ACells, ARows: integer;
begin
  if not HandleAllocated then Exit;
  FixedCols := 0;
  FixedRows := 0;
  ACells := Ord(CharRange.EndChar) - Ord(CharRange.StartChar);
//  ColCount := 20;
  ARows := ACells div ColCount + 1;
  RowCount := ARows;
  DefaultRowHeight := abs(Font.Height) + 12;
  DefaultColWidth := DefaultRowHeight - 5;
  if AutoSizeWidth or AutoSizeHeight then
    AdjustSize;
  if PanelVisible then
    ShowCharPanel(Col, Row);
end;

function TJvCustomCharMap.SelectCell(ACol, ARow: Integer): Boolean;
begin
  // can't use IsValidChar here since we need to be able to select invalid cells as well to be able to scroll
  Result := (ACol >= 0) and (ARow >= 0) and (CharRange.StartChar + ARow * ColCount + ACol <= CharRange.EndChar);
end;

procedure TJvCustomCharMap.SetAutoSizeHeight(const Value: boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    if FAutoSizeHeight then
      AdjustSize;
  end;
end;

procedure TJvCustomCharMap.SetAutoSizeWidth(const Value: boolean);
begin
  if FAutoSizeWidth <> Value then
  begin
    FAutoSizeWidth := Value;
    if FAutoSizeWidth then
      AdjustSize;
  end;
end;

procedure TJvCustomCharMap.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  RecalcCells;
  if HandleAllocated and PanelVisible and ((ClientHeight < DefaultRowHeight) or (ClientWidth < DefaultColWidth)) then
    PanelVisible := false;
end;

procedure TJvCustomCharMap.SetCharRange(const Value: TJvCharMapRange);
begin
//  FCharRange := Value;
end;

procedure TJvCustomCharMap.SetColumns(const Value: integer);
begin
  if Value > 0 then
  begin
    ColCount := Value;
    RecalcCells;
  end;
end;

procedure TJvCustomCharMap.SetPanelVisible(const Value: boolean);
begin
  if (PanelVisible <> Value) and not (csDesigning in ComponentState) then
  begin
    FCharPanel.Visible := Value;
  end;
end;

procedure TJvCustomCharMap.SetShowZoomPanel(const Value: boolean);
begin
  if FShowZoomPanel <> Value then
  begin
    FShowZoomPanel := Value;
    if not FShowZoomPanel then
      PanelVisible := false;
  end;
end;

procedure TJvCustomCharMap.ShowCharPanel(ACol, ARow: integer);
var
  R: TRect;
  P: TPoint;
begin
  if not ShowZoomPanel or not SelectCell(ACol, ARow) then
  begin
    PanelVisible := false;
    Exit;
  end;
  R := CellRect(ACol, ARow);
  Selection := TGridRect(Rect(ACol, ARow, ACol, ARow));
{$IFDEF COMPILER6_UP}
  FocusCell(ACol, ARow, false);
{$ELSE}
  Col := ACol;
  Row := ARow;
{$ENDIF}

  TCharZoomPanel(FCharPanel).Character := GetChar(ACol, ARow);
  P.X := R.Left - (FCharPanel.Width - DefaultColWidth) div 2;
  P.Y := R.Top - (FCharPanel.Height - DefaultRowHeight) div 2;
  P := ClientToScreen(P);
  FCharPanel.Left := P.X;
  FCharPanel.Top := P.Y;
  if not PanelVisible then
    PanelVisible := true;
end;

procedure TJvCustomCharMap.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if PanelVisible then
  begin
    if (Col < LeftCol) then
      ShowCharPanel(LeftCol, Row)
    else if Col >= LeftCol + VisibleColCount then
      ShowCharPanel(LeftCol + VisibleColCount - 1, Row)
    else
      ShowCharPanel(Col, Row);
  end;
end;

procedure TJvCustomCharMap.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if PanelVisible then
  begin
    if (Row < TopRow) then
      ShowCharPanel(Col, TopRow)
    else if Row >= TopRow + VisibleRowCount then
      ShowCharPanel(Col, TopRow + VisibleRowCount - 1)
    else
      ShowCharPanel(Col, Row);
  end;
end;

{ TCharZoomPanel }

procedure TCharZoomPanel.CMFontchanged(var Message: TMessage);
begin
  inherited;
  // (p3) height should be quite larger than Font.Height and Width a little more than that
  Height := abs(Font.Height) * 4;
  Width := MulDiv(Height, 110, 100);
end;

procedure TCharZoomPanel.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Visible and CanFocus then
    SetFocus;
end;

constructor TCharZoomPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  SetBounds(0, 0, 52, 48);
end;

procedure TCharZoomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_BORDER or WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;

destructor TCharZoomPanel.Destroy;
begin
  UnHookWndProc;
  inherited;
end;

procedure TCharZoomPanel.HookWndProc;
var
  F: TCustomForm;
begin
  if not (csDesigning in ComponentState) and not Assigned(FOldWndProc) then
  begin
    F := GetParentForm(self);
    if F <> nil then
    begin
      FOldWndProc := F.WindowProc;
      F.WindowProc := FormWindowProc;
    end;
  end;
end;

procedure TCharZoomPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Visible := false;
        if Parent.CanFocus then Parent.SetFocus;
      end;
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
      TJvCustomCharMap(Parent).KeyDown(Key, Shift);
  else
    inherited;
  end;
end;

procedure TCharZoomPanel.FormWindowProc(var Message: TMessage);
begin
  FOldWndProc(Message);
  if not (csDestroying in ComponentState) then
  begin
    case Message.Msg of
      WM_MOVE:
        if Visible or FWasVisible then
          with TJvCharMap(Parent) do
            ShowCharPanel(Col, Row);
      WM_SYSCOMMAND:
        case Message.WParam and $FFF0 of
          SC_MINIMIZE:
            begin
              FWasVisible := Visible;
              Visible := false;
            end;
          SC_RESTORE, SC_MAXIMIZE:
            if FWasVisible and IsWindowVisible(GetParentForm(self).Handle) then
              with TJvCharMap(Parent) do
                ShowCharPanel(Col, Row);
        end;
      WM_WINDOWPOSCHANGED:
        if FWasVisible and IsWindowVisible(GetParentForm(self).Handle) then
          with TJvCharMap(Parent) do
            ShowCharPanel(Col, Row);
    end;
  end;
end;

procedure TCharZoomPanel.Paint;
var
  R: TRect;
  AChar: WideChar;
begin
  inherited;
  Canvas.Font := Font;
  Canvas.Font.Height := ClientHeight - 4;
//  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Font.Color;
  R := ClientRect;
  Canvas.Rectangle(R);

//  R := Rect(0,0,Width,Height);
  SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
  AChar := Character;
  if TJvCustomCharMap(Parent).IsValidChar(AChar) then
    WideDrawText(Canvas, AChar, R, DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
end;

procedure TCharZoomPanel.SetCharacter(const Value: WideChar);
begin
  if FCharacter <> Value then
  begin
    FCharacter := Value;
    Repaint;
  end;
end;

procedure TCharZoomPanel.UnhookWndProc;
var
  F: TCustomForm;
begin
  if not (csDesigning in ComponentState) and Assigned(FOldWndProc) then
  begin
    F := GetParentForm(self);
    if (F <> nil) then
      F.WindowProc := FOldWndProc;
  end;
end;

procedure TCharZoomPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCharZoomPanel.WMNCHitTest(var Message: TWMNCHitTest);
begin
  // pass mouse clicks to parent (the grid)
  Message.Result := HTTRANSPARENT;
end;

procedure TCharZoomPanel.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Message.Result := 1;
  if not (csDestroying in ComponentState) and Parent.CanFocus then
    Parent.SetFocus;
end;

procedure TCharZoomPanel.CreateHandle;
begin
  inherited;
  HookWndProc;
end;

{ TJvCharMapRange }

procedure TJvCharMapRange.Change;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TJvCharMapRange.SetEndChar(const Value: Word);
begin
  if FEndChar <> Value then
  begin
    FEndChar := Value;
    Change;
  end;
end;

procedure TJvCharMapRange.SetStartChar(const Value: Word);
begin
  if FStartChar <> Value then
  begin
    FStartChar := Value;
    Change;
  end;
end;

end.

