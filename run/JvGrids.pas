{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGrids.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvGrids;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  Classes,
  {$IFDEF VCL}
  Controls, Graphics, StdCtrls, Forms, Grids,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QTypes, QControls, QGraphics, QStdCtrls, QForms, QGrids, Types, QWindows,
  {$ENDIF VisualCLX}
  JvConsts, JvAppStorage, JvFormPlacement, JvComponent, JvExGrids;

type
  TAcceptKeyEvent = function(Sender: TObject; var Key: Char): Boolean of object;
  TEditLimitEvent = procedure(Sender: TObject; var MaxLength: Integer) of object;
  TEditShowEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    var AllowEdit: Boolean) of object;
  TFixedCellClickEvent = procedure(Sender: TObject; ACol, ARow: Longint) of object;
  TFixedCellCheckEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    var Enabled: Boolean) of object;

  TInplaceEditStyle = TEditStyle;
  
const
  ieSimple = esSimple;
  ieEllipsis = esEllipsis;
  iePickList = esPickList;

type
  TEditAlignEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    var Alignment: TAlignment) of object;
  TPicklistEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    PickList: TStrings) of object;
  TEditStyleEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    var Style: TInplaceEditStyle) of object;

  TJvDrawGrid = class(TJvExDrawGrid)
  private
    FNoUpdateData: Boolean;
    FFixedCellsButtons: Boolean;
    FPressedCell: TGridCoord;
    FCellDown: TGridCoord;
    FPressed: Boolean;
    FTracking: Boolean;
    FSwapButtons: Boolean;
    FDefaultDrawing: Boolean;
    FIniLink: TJvIniLink;
    FOnColumnSized: TNotifyEvent;
    FOnRowSized: TNotifyEvent;
    FOnAcceptEditKey: TAcceptKeyEvent;
    FOnGetEditLimit: TEditLimitEvent;
    FOnEditChange: TNotifyEvent;
    FOnShowEditor: TEditShowEvent;
    FOnCancelEdit: TNotifyEvent;
    FOnFixedCellClick: TFixedCellClickEvent;
    FOnCheckButton: TFixedCellCheckEvent;
    FOnChangeFocus: TNotifyEvent;
    {$IFDEF VCL}
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    {$ENDIF VCL}
    FOnGetEditAlign: TEditAlignEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnGetPicklist: TPicklistEvent;
    FOnGetEditStyle: TEditStyleEvent;
    FDrawButtons: Boolean;
    FBeepOnError: Boolean;
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure SetFixedButtons(Value: Boolean);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    {$IFDEF VCL}
    function IsActiveControl: Boolean;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonUp(var Msg: TWMMouse); message WM_RBUTTONUP;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    {$ENDIF VCL}
    procedure SetDrawButtons(const Value: Boolean);
  protected
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    procedure DoSetFocus(FocusedWnd: HWND); override;

    function CanEditAcceptKey(Key: Char): Boolean; override;
    function CanEditShow: Boolean; override;
    function GetEditLimit: Integer; override;
    procedure TopLeftChanged; override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DoDrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IFDEF VCL}
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure SetEditText(ACol, ARow: Longint; const Value: WideString); override;
    {$ENDIF VisualCLX}
    function CreateEditor: TInplaceEdit; override;
    procedure Paint; override;
    procedure EditChanged(Sender: TObject); dynamic;
    procedure DoFixedCellClick(ACol, ARow: Longint); dynamic;
    procedure CheckFixedCellButton(ACol, ARow: Longint;
      var Enabled: Boolean); dynamic;
    procedure EditButtonClick; dynamic;
    function GetEditAlignment(ACol, ARow: Longint): TAlignment; dynamic;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    procedure GetPicklist(ACol, ARow: Longint; Picklist: TStrings); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawStr(ARect: TRect; const S: string; Align: TAlignment);
    procedure DrawMultiline(ARect: TRect; const S: string; Align: TAlignment);
    procedure DrawPicture(ARect: TRect; Graphic: TGraphic);
    procedure DrawMasked(ARect: TRect; Graphic: TBitmap);
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateRow(ARow: Longint);
    procedure LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure Load;
    procedure Save;
    property InplaceEditor;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property DefaultRowHeight default 18;
    property DrawButtons: Boolean read FDrawButtons write SetDrawButtons;
    property Options default [goFixedVertLine, goFixedHorzLine, goVertLine,
      goHorzLine, goDrawFocusSelected, goColSizing];
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property FixedButtons: Boolean read FFixedCellsButtons write SetFixedButtons
      default False;
    property OnAcceptEditKey: TAcceptKeyEvent read FOnAcceptEditKey
      write FOnAcceptEditKey;
    property OnCancelEdit: TNotifyEvent read FOnCancelEdit write FOnCancelEdit;
    property OnCheckButton: TFixedCellCheckEvent read FOnCheckButton
      write FOnCheckButton;
    property OnChangeFocus: TNotifyEvent read FOnChangeFocus write FOnChangeFocus;
    property OnFixedCellClick: TFixedCellClickEvent read FOnFixedCellClick
      write FOnFixedCellClick;
    property OnColumnSized: TNotifyEvent read FOnColumnSized
      write FOnColumnSized;
    property OnRowSized: TNotifyEvent read FOnRowSized write FOnRowSized;
    property OnGetEditLimit: TEditLimitEvent read FOnGetEditLimit write FOnGetEditLimit;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnShowEditor: TEditShowEvent read FOnShowEditor write FOnShowEditor;
    property OnGetEditAlign: TEditAlignEvent read FOnGetEditAlign write FOnGetEditAlign;
    property OnGetEditStyle: TEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnGetPicklist: TPicklistEvent read FOnGetPicklist write FOnGetPicklist;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    {$IFDEF VCL}
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    {$ENDIF VCL}
  end;

implementation

uses
  SysUtils, Math,
  JvJCLUtils, JvJVCLUtils;

const
  MaxCustomExtents = MaxListSize;
  MaxShortInt = High(ShortInt);

type
  PIntArray = ^TIntArray;
  TIntArray = array [0..MaxCustomExtents] of Integer;

//=== { TJvGridPopupListBox } ================================================

type
  TJvGridPopupListBox = class;

  TJvInplaceEdit = class(TJvExInplaceEdit)
  private
    {$IFDEF VCL}
    FAlignment: TAlignment;
    {$ENDIF VCL}
    FButtonWidth: Integer;
    FPickList: TJvGridPopupListBox;
    FActiveList: TWinControl;
    FEditStyle: TInplaceEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TInplaceEditStyle);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    {$IFDEF VCL}
    procedure SetAlignment(Value: TAlignment);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    {$ENDIF VCL}
  protected
    procedure DoKillFocus(FocusedWnd: HWND); override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IFDEF VCL}
    procedure PaintWindow(DC: HDC); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    {$ENDIF VisualCLX}
    procedure UpdateContents; override;
    {$IFDEF VCL}
    procedure WndProc(var Message: TMessage); override;
    {$ENDIF VCL}
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property PickList: TJvGridPopupListBox read FPickList;
  public
    constructor Create(Owner: TComponent); override;
    {$IFDEF VCL}
    property Alignment: TAlignment read FAlignment write SetAlignment;
    {$ENDIF VCL}
    property EditStyle: TInplaceEditStyle read FEditStyle write SetEditStyle;
  end;

  TJvGridPopupListBox = class(TJvPopupListBox)
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

procedure TJvGridPopupListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TJvInplaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and (X < Width) and (Y < Height));
end;

//=== { TJvInplaceEdit } =====================================================

constructor TJvInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  //  FEditStyle := esSimple;
  FEditStyle := ieSimple;
end;

{$IFDEF VCL}
procedure TJvInplaceEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or Alignments[FAlignment];
end;
{$ENDIF VCL}

procedure TJvInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> ieSimple then
    Dec(R.Right, FButtonWidth);
  {$IFDEF VCL}
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
  {$ENDIF VCL}
end;

procedure TJvInplaceEdit.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if FListVisible then
  begin
    {$IFDEF VCL}
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    {$ENDIF VCL}
    if FPickList.ItemIndex > -1 then
      ListValue := FPickList.Items[FPicklist.ItemIndex];
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept and EditCanModify then
      Text := ListValue;
  end;
end;

procedure TJvInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then
          CloseUp(True)
        else
          DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TJvInplaceEdit.DropDown;
const
  MaxListCount = 8;
var
  P: TPoint;
  Y, J, I: Integer;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FPickList.Width := Width;
    FPickList.Color := Color;
    FPickList.Font := Font;
    FPickList.Items.Clear;
    with TJvDrawGrid(Grid) do
      GetPickList(Col, Row, FPickList.Items);
    FPickList.Height := Min(FPickList.Items.Count, MaxListCount) *
      FPickList.ItemHeight + 4;
    FPickList.ItemIndex := FPickList.Items.IndexOf(Text);
    J := FPickList.ClientWidth;
    for I := 0 to FPickList.Items.Count - 1 do
    begin
      Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
      if Y > J then
        J := Y;
    end;
    if FPickList.Items.Count > MaxListCount then
      Inc(J, GetSystemMetrics(SM_CXVSCROLL));
    FPickList.ClientWidth := J;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FActiveList.Height > Screen.Height then
      Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    {$IFDEF VCL}
    Windows.SetFocus(Handle);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    QWindows.SetFocus(Handle);
    {$ENDIF VisualCLX}
  end;
end;

type
  TWinControlCracker = class(TWinControl);

procedure TJvInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = ieEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    TJvDrawGrid(Grid).EditButtonClick;
    {$IFDEF VCL}
    KillMessage(Handle, WM_CHAR);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Key := 0;
    {$ENDIF VisualCLX}
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TJvInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TJvInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> ieSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X, Y)) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

{$IFDEF VisualCLX}
type
  TWidgetControlAccessProtected = class(TWidgetControl);
{$ENDIF VisualCLX}

procedure TJvInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        {$IFDEF VCL}
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        TWidgetControlAccessProtected(FActiveList).MouseDown(mbLeft, Shift, MousePos.X , MousePos.Y);
        {$ENDIF VisualCLX}
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = ieEllipsis) and WasPressed then
    TJvDrawGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

{$IFDEF VCL}
procedure TJvInplaceEdit.PaintWindow(DC: HDC);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvInplaceEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
{$ENDIF VisualCLX}
const
  LeftOffs = 3;
var
  R: TRect;
  Flags: Integer;
  W, G, I: Integer;
  {$IFDEF VisualCLX}
  DC: QPainterH;
  {$ENDIF VisualCLX}
begin
  {$IFDEF VisualCLX}
  DC := QPainter_create(QWidget_to_QPaintDevice(Handle), Handle);
  try
    QPainter_setClipRegion(DC, EventRegion);
    QPainter_setClipping(DC, not QRegion_isEmpty(EventRegion));
  {$ENDIF VisualCLX}

  if FEditStyle <> ieSimple then
  begin
    SetRect(R, Width - FButtonWidth, 0, Width, Height);
    Flags := 0;
    if FEditStyle in [iePickList] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else
      if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else
    begin { esEllipsis }
      if FPressed then
        Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      W := 2;
      G := (FButtonWidth - LeftOffs * 2 - 3 * W) div 2;
      if G <= 0 then
        G := 1;
      if G > 3 then
        G := 3;
      Flags := R.Left + (FButtonWidth - 3 * W - 2 * G) div 2 + Ord(FPressed);
      I := R.Top + (ClientHeight - W) div 2 {+ Ord(FPressed)};
      PatBlt(DC, Flags, I, W, W, BLACKNESS);
      PatBlt(DC, Flags + G + W, I, W, W, BLACKNESS);
      PatBlt(DC, Flags + 2 * G + 2 * W, I, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  {$IFDEF VisualCLX}
  finally
    QPainter_destroy(DC);
  end;
  {$ENDIF VisualCLX}
  inherited {PaintWindow(DC);}
end;

{$IFDEF VCL}
procedure TJvInplaceEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;
{$ENDIF VCL}

procedure TJvInplaceEdit.SetEditStyle(Value: TInplaceEditStyle);
begin
  if Value = FEditStyle then
    Exit;
  FEditStyle := Value;
  case Value of
    iePickList:
      begin
        if FPickList = nil then
        begin
          FPickList := TJvGridPopupListBox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          {$IFDEF VCL}
          FPickList.IntegralHeight := True;
          {$ENDIF VCL}
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
  else
    FActiveList := nil;
  end;
  Repaint;
end;

procedure TJvInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvInplaceEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    {$IFDEF VCL}
    InvalidateRect(Handle, @R, False);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    InvalidateRect(R, False);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvInplaceEdit.UpdateContents;
var
  SaveChanged: TNotifyEvent;
begin
  with TJvDrawGrid(Grid) do
  begin
    Self.Alignment := GetEditAlignment(Col, Row);
    EditStyle := GetEditStyle(Col, Row);
  end;
  SaveChanged := Self.OnChange;
  try
    Self.OnChange := nil;
    inherited UpdateContents;
  finally
    Self.OnChange := SaveChanged;
  end;
end;

{$IFDEF VCL}

procedure TJvInplaceEdit.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TJvInplaceEdit.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;

{$ENDIF VCL}

procedure TJvInplaceEdit.DoKillFocus(FocusedWnd: HWND);
begin
  if not SysLocale.FarEast then
    inherited DoKillFocus(FocusedWnd)
  else
  begin
    {$IFDEF VCL}
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    {$ENDIF VCL}
    inherited DoKillFocus(FocusedWnd);
    {$IFDEF VCL}
    if FocusedWnd <> TJvDrawGrid(Grid).Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
    {$ENDIF VCL}
  end;
  CloseUp(False);
end;

{$IFDEF VCL}

procedure TJvInplaceEdit.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  with Msg do
    if (FEditStyle <> ieSimple) and PtInRect(Rect(Width - FButtonWidth, 0,
      Width, Height), Point(XPos, YPos)) then
      Exit;
  inherited;
end;

procedure TJvInplaceEdit.WMPaint(var Msg: TWMPaint);
begin
  PaintHandler(Msg);
end;

procedure TJvInplaceEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> ieSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), ScreenToClient(P)) then
    Windows.SetCursor(LoadCursor(0, IDC_ARROW))
  else
    inherited;
end;

procedure TJvInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
      if EditStyle in [iePickList] then
        with TWMKey(Message) do
        begin
          DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (CharCode <> 0) and FListVisible then
          begin
            with TMessage(Message) do
              SendMessage(FActiveList.Handle, Msg, WParam, LParam);
            Exit;
          end;
        end;
  end;
  inherited WndProc(Message);
end;

{$ENDIF VCL}

//=== { TJvDrawGrid } ========================================================

constructor TJvDrawGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultRowHeight := 18;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goDrawFocusSelected, goColSizing];
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FPressedCell.X := -1;
  FPressedCell.Y := -1;
  FCellDown.X := -1;
  FCellDown.Y := -1;
  FBeepOnError := True;
end;

destructor TJvDrawGrid.Destroy;
begin
  FOnChangeFocus := nil;
  FIniLink.Free;
  inherited Destroy;
end;

function TJvDrawGrid.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvDrawGrid.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TJvDrawGrid.IniSave(Sender: TObject);
begin
  if (Name <> '') and Assigned(IniStorage) then
    SaveToAppStorage(IniStorage.AppStorage, IniStorage.AppStorage.ConcatPaths([
      IniStorage.AppStoragePath, GetDefaultSection(Self)]));
end;

procedure TJvDrawGrid.IniLoad(Sender: TObject);
begin
  if (Name <> '') and Assigned(IniStorage) then
    LoadFromAppStorage(IniStorage.AppStorage, IniStorage.AppStorage.ConcatPaths([
      IniStorage.AppStoragePath, GetDefaultSection(Self)]));
end;

function TJvDrawGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  if Assigned(FOnAcceptEditKey) then
    Result := FOnAcceptEditKey(Self, Key)
  else
    Result := inherited CanEditAcceptKey(Key);
end;

function TJvDrawGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if Result and Assigned(FOnShowEditor) then
  begin
    FOnShowEditor(Self, Col, Row, Result);
    if not Result then
      EditorMode := False;
  end;
end;

procedure TJvDrawGrid.DrawPicture(ARect: TRect; Graphic: TGraphic);
begin
  DrawCellBitmap(Self, 0, 0, Graphic, ARect);
end;

procedure TJvDrawGrid.DrawMasked(ARect: TRect; Graphic: TBitmap);
var
  X, Y: Integer;
begin
  X := (ARect.Right + ARect.Left - Graphic.Width) div 2;
  Y := (ARect.Bottom + ARect.Top - Graphic.Height) div 2;
  DrawBitmapTransparent(Canvas, X, Y, Graphic, Graphic.TransparentColor and not PaletteMask);
end;

procedure TJvDrawGrid.DrawStr(ARect: TRect; const S: string;
  Align: TAlignment);
begin
  DrawCellTextEx(Self, 0, 0, S, ARect, Align, vaCenterJustify, False, IsRightToLeft);
end;

procedure TJvDrawGrid.DrawMultiline(ARect: TRect; const S: string;
  Align: TAlignment);
begin
  DrawCellTextEx(Self, 0, 0, S, ARect, Align, vaTopJustify, True, IsRightToLeft);
end;

procedure TJvDrawGrid.InvalidateCell(ACol, ARow: Longint);
begin
  inherited InvalidateCell(ACol, ARow);
end;

procedure TJvDrawGrid.InvalidateCol(ACol: Longint);
begin
  inherited InvalidateCol(ACol);
end;

procedure TJvDrawGrid.InvalidateRow(ARow: Longint);
var
  I: Longint;
begin
  for I := 0 to ColCount - 1 do
    inherited InvalidateCell(I, ARow);
end;

procedure TJvDrawGrid.LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
begin
  if (Name <> '') then
    InternalRestoreGridLayout(Self, IniStorage.AppStorage, Path);
end;

procedure TJvDrawGrid.SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
begin
  if (Name <> '') then
    InternalSaveGridLayout(Self, IniStorage.AppStorage, Path);
end;

procedure TJvDrawGrid.Load;
begin
  IniLoad(nil);
end;

procedure TJvDrawGrid.Save;
begin
  IniSave(nil);
end;

procedure TJvDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not CanGridAcceptKey(Key, Shift) then
    Exit;
  if not (ssCtrl in Shift) and (Key = VK_ESCAPE) and EditorMode and
    (InplaceEditor <> nil) and InplaceEditor.Visible and
    not (goAlwaysShowEditor in Options) then
  begin
    FNoUpdateData := True;
    try
      HideEditor;
      if Assigned(FOnCancelEdit) then
        FOnCancelEdit(Self);
    finally
      FNoUpdateData := False;
    end;
  end
  else
    inherited KeyDown(Key, Shift);
end;

{$IFDEF VCL}
procedure TJvDrawGrid.WMCommand(var Msg: TWMCommand);
begin
  if (Msg.NotifyCode = EN_CHANGE) and
    not (goAlwaysShowEditor in Options) then
  begin
    if (InplaceEditor <> nil) and InplaceEditor.HandleAllocated and
      InplaceEditor.Visible then
      TJvInplaceEdit(InplaceEditor).Change;
    Msg.NotifyCode := 0;
  end;
  inherited;
end;
{$ENDIF VCL}

procedure TJvDrawGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then
    FOnEditChange(Self);
end;

function TJvDrawGrid.GetEditLimit: Integer;
begin
  Result := inherited GetEditLimit;
  if Assigned(FOnGetEditLimit) then
    FOnGetEditLimit(Self, Result);
end;

{$IFDEF VCL}
procedure TJvDrawGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if not FNoUpdateData then
    inherited SetEditText(ACol, ARow, Value);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvDrawGrid.SetEditText(ACol, ARow: Longint; const Value: WideString);
begin
  if not FNoUpdateData then
    inherited SetEditText(ACol, ARow, Value);
end;
{$ENDIF VisualCLX}

procedure TJvDrawGrid.SetFixedButtons(Value: Boolean);
begin
  if FFixedCellsButtons <> Value then
  begin
    FFixedCellsButtons := Value;
    Invalidate;
  end;
end;

procedure TJvDrawGrid.DoFixedCellClick(ACol, ARow: Longint);
begin
  if Assigned(FOnFixedCellClick) then
    FOnFixedCellClick(Self, ACol, ARow);
end;

procedure TJvDrawGrid.CheckFixedCellButton(ACol, ARow: Longint; var Enabled: Boolean);
begin
  if (ACol >= 0) and (ARow >= 0) and ((ACol < FixedCols) or (ARow < FixedRows)) then
  begin
    if Assigned(FOnCheckButton) then
      FOnCheckButton(Self, ACol, ARow, Enabled);
  end
  else
    Enabled := False;
end;

procedure TJvDrawGrid.TopLeftChanged;
begin
  if (goRowSelect in Options) and DefaultDrawing then
    InvalidateRow(Self.Row);
  inherited TopLeftChanged;
  if FTracking then
    StopTracking;
end;

procedure TJvDrawGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if FTracking then
    StopTracking;
  if Assigned(FOnColumnSized) then
    FOnColumnSized(Self);
end;

procedure TJvDrawGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  if FTracking then
    StopTracking;
  if Assigned(FOnRowSized) then
    FOnRowSized(Self);
end;

procedure TJvDrawGrid.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvDrawGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
begin
  Cell := MouseCoord(X, Y);
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
    and (FPressedCell.X = Cell.X) and (FPressedCell.Y = Cell.Y);
  if FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    InvalidateCell(Cell.X, Cell.Y);
    InvalidateCell(FPressedCell.X, FPressedCell.Y);
  end;
end;

{$IFDEF VCL}
function TJvDrawGrid.IsActiveControl: Boolean;
var
  Handle: THandle;
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
    Result := ParentForm.ActiveControl = Self
  else
  begin
    Handle := GetFocus;
    Result := False;
    while not Result and IsWindow(Handle) do
    begin
      if Handle = WindowHandle then
        Result := True
      else
        Handle := GetParent(Handle);
    end;
  end;
end;
{$ENDIF VCL}

procedure TJvDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  EnableClick, Fixed: Boolean;
begin
  if DrawButtons then
  begin
    if (Button = mbLeft) and ((Shift - [ssLeft]) = []) then
    begin
      MouseToCell(X, Y, Cell.X, Cell.Y);
      if (Cell.X >= FixedCols) and (Cell.Y >= FixedRows) then
      begin
        FCellDown := Cell;
        InvalidateCell(Cell.X, Cell.Y);
      end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  HideEditor;
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    if not IsActiveControl then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then
  begin
    if FFixedCellsButtons then
    begin
      Cell := MouseCoord(X, Y);
      if not ((Cell.X >= 0) and (Cell.X < FixedCols)) and
        not ((Cell.Y >= 0) and (Cell.Y < FixedRows)) then
      begin
        DblClick;
        Exit;
      end;
    end
    else
    begin
      DblClick;
      Exit;
    end;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    Cell := MouseCoord(X, Y);
    Fixed := ((Cell.X >= 0) and (Cell.X < FixedCols)) or
      ((Cell.Y >= 0) and (Cell.Y < FixedRows));
    if FFixedCellsButtons and Fixed and not (csDesigning in ComponentState) then
    begin
      if ([goRowMoving, goColMoving] * Options <> []) and
        (Button = mbRight) then
      begin
        Button := mbLeft;
        FSwapButtons := True;
        MouseCapture := True;
      end
      else
      if Button = mbLeft then
      begin
        EnableClick := True;
        CheckFixedCellButton(Cell.X, Cell.Y, EnableClick);
        if EnableClick then
        begin
          MouseCapture := True;
          FTracking := True;
          FPressedCell := Cell;
          TrackButton(X, Y);
        end
        else
        if BeepOnError then
          Beep;
        Exit;
      end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TJvDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
begin
  if DrawButtons then
  begin
    if (Shift = [ssLeft]) then
    begin
      MousetoCell(X, Y, Cell.X, Cell.Y);
      if not CompareMem(@Cell, @FCellDown, SizeOf(Cell)) then
      begin
        if (FCellDown.X >= 0) and (FCellDown.Y >= 0) then
          InvalidateCell(FCellDown.X, FCellDown.Y);
        FCellDown := Cell;
        InvalidateCell(Cell.X, Cell.Y);
      end;
    end;
    inherited MouseMove(Shift, X, Y);
    Exit;
  end;

  if FTracking then
    TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDrawGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol, ARow: Longint;
  DoClick: Boolean;
begin
  if DrawButtons then
  begin
    if (Button = mbLeft) and (Shift = []) then
    begin
      InvalidateCell(FCellDown.X, FCellDown.Y);
      FCellDown.X := -1;
      FCellDown.Y := -1;
    end;
    inherited MouseUp(Button, Shift, X, Y);
    Exit;
  end;

  if FTracking and (FPressedCell.Y >= 0) and (FPressedCell.X >= 0) then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y = FPressedCell.Y) and (Cell.X = FPressedCell.X);
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      ARow := Cell.Y;
      if (ARow < RowCount) and (ACol < ColCount) then
        DoFixedCellClick(ACol, ARow);
    end;
  end
  else
  if FSwapButtons then
  begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then
      Button := mbLeft;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvDrawGrid.Paint;
var
  R: TRect;
begin
  FDefaultDrawing := inherited DefaultDrawing;
  inherited DefaultDrawing := False;
  try
    inherited Paint;
  finally
    inherited DefaultDrawing := FDefaultDrawing;
  end;
  if not (csDesigning in ComponentState) and DefaultDrawing and Focused and
    ([goRowSelect, goRangeSelect] * Options = [goRowSelect]) then
  begin
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
    if Row >= FixedRows then
    begin
      R := BoxRect(FixedCols, Row, ColCount - 1, Row);
      if not (goHorzLine in Options) then
        Inc(R.Bottom, GridLineWidth);
      DrawFocusRect(Canvas.Handle, R);
    end;
  end;
end;

procedure TJvDrawGrid.CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TJvDrawGrid.DoDrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  CallDrawCellEvent(ACol, ARow, ARect, AState);
end;

procedure TJvDrawGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Down: Boolean;
  TempRect: TRect;
  FrameFlags1, FrameFlags2: DWORD;
  Style: DWORD;
const
  EdgeFlag: array [Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
begin
  if DrawButtons then
  begin
    TempRect := ARect;
    if not (gdFixed in aState) then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.Font.Color := clBtnText;
      Style := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if (FCellDown.X = aCol) and (FCellDown.Y = aRow) then
        Style := Style or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, TempRect, DFC_BUTTON, Style);
    end;
    inherited DrawCell(ACol,ARow,ARect,AState);
    Exit;
  end;

  if FDefaultDrawing or (csDesigning in ComponentState) then
    with Canvas do
    begin
      Font := Self.Font;
      if ([goRowSelect, goVertLine] * Options = [goRowSelect]) and
        not (gdFixed in AState) then
        Inc(ARect.Right, GridLineWidth);
      if ([goRowSelect, goHorzLine] * Options = [goRowSelect]) and
        not (gdFixed in AState) then
        Inc(ARect.Bottom, GridLineWidth);
      if (gdSelected in AState) and (not (gdFocused in AState) or
        ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
      begin
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end
      else
      begin
        if gdFixed in AState then
          Brush.Color := FixedColor
        else
          Brush.Color := Color;
      end;
      FillRect(ARect);
    end;
  Down := FFixedCellsButtons and (gdFixed in AState) and
    {$IFDEF VCL}
    Ctl3D and
    {$ENDIF VCL}
    not (csLoading in ComponentState) and FPressed and FDefaultDrawing and
    (FPressedCell.X = ACol) and (FPressedCell.Y = ARow);
  inherited DefaultDrawing := FDefaultDrawing;
  if Down then
  begin
    Inc(ARect.Left, GridLineWidth);
    Inc(ARect.Top, GridLineWidth);
  end;
  try
    DoDrawCell(ACol, ARow, ARect, AState);
  finally
    inherited DefaultDrawing := False;
    if Down then
    begin
      Dec(ARect.Left, GridLineWidth);
      Dec(ARect.Top, GridLineWidth);
    end;
  end;
  if FDefaultDrawing and
     {$IFDEF VCL}
     Ctl3D and
     {$ENDIF VCL}
     (gdFixed in AState) then
  begin
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then
    begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then
    begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    if (FrameFlags1 or FrameFlags2) <> 0 then
    begin
      TempRect := ARect;
      if ((FrameFlags1 and BF_RIGHT) = 0) and
        (goFixedVertLine in Options) then
        Inc(TempRect.Right, GridLineWidth)
      else
      if ((FrameFlags1 and BF_BOTTOM) = 0) and
        (goFixedVertLine in Options) then
        Inc(TempRect.Bottom, GridLineWidth);
      DrawEdge(Canvas.Handle, TempRect, EdgeFlag[Down], FrameFlags1);
      DrawEdge(Canvas.Handle, TempRect, EdgeFlag[Down], FrameFlags2);
    end;
  end;
  if FDefaultDrawing and not (csDesigning in ComponentState) and
    (gdFocused in AState) and
    ([goEditing, goAlwaysShowEditor] * Options <> [goEditing, goAlwaysShowEditor]) and
    not (goRowSelect in Options) then
    DrawFocusRect(Canvas.Handle, ARect);
end;

{$IFDEF VCL}

procedure TJvDrawGrid.WMRButtonUp(var Msg: TWMMouse);
begin
  if not (FGridState in [gsColMoving, gsRowMoving]) then
    inherited
  else
  if not (csNoStdEvents in ControlStyle) then
    with Msg do
      MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TJvDrawGrid.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
var
  Cell: TGridCoord;
begin
  if FFixedCellsButtons then
  begin
    with Msg do
      Cell := MouseCoord(XPos, YPos);
    if ((Cell.X >= 0) and (Cell.X < FixedCols)) or
      ((Cell.Y >= 0) and (Cell.Y < FixedRows)) then
    begin
      SendCancelMode(Self);
      if csCaptureMouse in ControlStyle then
        MouseCapture := True;
      if not (csNoStdEvents in ControlStyle) then
        with Msg do
          MouseDown(mbLeft, KeysToShiftState(Keys) - [ssDouble], XPos, YPos);
      Exit;
    end;
  end;
  inherited;
end;

{$ENDIF VCL}

procedure TJvDrawGrid.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
  if Assigned(FOnChangeFocus) then
    FOnChangeFocus(Self);
end;

procedure TJvDrawGrid.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  if Assigned(FOnChangeFocus) then
    FOnChangeFocus(Self);
end;

{$IFDEF VCL}
procedure TJvDrawGrid.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;
{$ENDIF VCL}

function TJvDrawGrid.CreateEditor: TInplaceEdit;
begin
  Result := TJvInplaceEdit.Create(Self);
  TEdit(Result).OnChange := EditChanged;
end;

function TJvDrawGrid.GetEditAlignment(ACol, ARow: Longint): TAlignment;
begin
  Result := taLeftJustify;
  if Assigned(FOnGetEditAlign) then
    FOnGetEditAlign(Self, ACol, ARow, Result);
end;

function TJvDrawGrid.GetEditStyle(ACol, ARow: Longint): TInplaceEditStyle;
begin
  Result := ieSimple;
  if Assigned(FOnGetEditStyle) then
    FOnGetEditStyle(Self, ACol, ARow, Result);
end;

procedure TJvDrawGrid.GetPicklist(ACol, ARow: Longint; PickList: TStrings);
begin
  if Assigned(FOnGetPicklist) then
    FOnGetPicklist(Self, ACol, ARow, PickList);
end;

procedure TJvDrawGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self);
end;

{$IFDEF VCL}
procedure TJvDrawGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TJvDrawGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;
{$ENDIF VCL}

procedure TJvDrawGrid.SetDrawButtons(const Value: Boolean);
begin
  if FDrawButtons <> Value then
  begin
    FDrawButtons := Value;
    Invalidate;
  end;
end;

function TJvDrawGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if DrawButtons then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

end.

