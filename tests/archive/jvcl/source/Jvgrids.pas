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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvGrids;


interface

uses {$IFDEF WIN32} Windows, Registry, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls, Forms, Menus,
  Grids, JvConst, IniFiles, JvPlacemnt;

{ TJvxDrawGrid }

type
  TAcceptKeyEvent = function (Sender: TObject; var Key: Char): Boolean of object;
  TEditLimitEvent = procedure (Sender: TObject; var MaxLength: Integer) of object;
  TEditShowEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var AllowEdit: Boolean) of object;
  TFixedCellClickEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;
  TFixedCellCheckEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var Enabled: Boolean) of object;
{$IFDEF WIN32}
{$IFDEF COMPILER6_UP}
  TInplaceEditStyle = TEditStyle; //(ieSimple, ieEllipsis, iePickList);

const
  ieSimple = esSimple;
  ieEllipsis = esEllipsis;
  iePickList = esPickList;

type
{$ELSE}
  TInplaceEditStyle = (ieSimple, ieEllipsis, iePickList);
{$ENDIF}
  TEditAlignEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var Alignment: TAlignment) of object;
  TPicklistEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    PickList: TStrings) of object;
  TEditStyleEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var Style: TInplaceEditStyle) of object;
{$ENDIF}

  TJvxDrawGrid = class(TDrawGrid)
  private
    FNoUpdateData: Boolean;
    FFixedCellsButtons: Boolean;
    FPressedCell: TGridCoord;
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
{$IFDEF WIN32}
    FOnGetEditAlign: TEditAlignEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnGetPicklist: TPicklistEvent;
    FOnGetEditStyle: TEditStyleEvent;
{$ENDIF}
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure SetFixedButtons(Value: Boolean);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function IsActiveControl: Boolean;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
{$IFDEF WIN32}
    procedure WMRButtonUp(var Message: TWMMouse); message WM_RBUTTONUP;
{$ENDIF}
  protected
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
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function CreateEditor: TInplaceEdit; override;
    procedure Paint; override;
    procedure EditChanged(Sender: TObject); dynamic;
    procedure DoFixedCellClick(ACol, ARow: Longint); dynamic;
    procedure CheckFixedCellButton(ACol, ARow: Longint;
      var Enabled: Boolean); dynamic;
{$IFDEF WIN32}
    procedure EditButtonClick; dynamic;
    function GetEditAlignment(ACol, ARow: Longint): TAlignment; dynamic;

    function GetEditStyle(ACol, ARow: Longint):
      {$IFDEF COMPILER6_UP}TEditStyle; override;{$ELSE}TInplaceEditStyle; dynamic;{$ENDIF}
    procedure GetPicklist(ACol, ARow: Longint; Picklist: TStrings); dynamic;
{$ENDIF}
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
    property InplaceEditor;
  published
    property DefaultRowHeight default 18;
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
{$IFDEF WIN32}
    property OnGetEditAlign: TEditAlignEvent read FOnGetEditAlign write FOnGetEditAlign;
    property OnGetEditStyle: TEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnGetPicklist: TPicklistEvent read FOnGetPicklist write FOnGetPicklist;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;
{$ENDIF}
  end;

implementation

uses SysUtils, JvVCLUtils, JvMaxMin, Consts, JvAppUtils;

const
{$IFDEF WIN32}
  MaxCustomExtents = MaxListSize;
{$ELSE}
  MaxCustomExtents = 65520 div SizeOf(Integer);
{$ENDIF}
  MaxShortInt = High(ShortInt);

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxCustomExtents] of Integer;

{$IFDEF WIN32}

{ TJvInplaceEdit }

type
  TJvPopupListbox = class;

  TJvInplaceEdit = class(TInplaceEdit)
  private
    FAlignment: TAlignment;
    FButtonWidth: Integer;
    FPickList: TJvPopupListbox;
    FActiveList: TWinControl;
    FEditStyle: TInplaceEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetEditStyle(Value: TInplaceEditStyle);
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property PickList: TJvPopupListbox read FPickList;
  public
    constructor Create(Owner: TComponent); override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property EditStyle: TInplaceEditStyle read FEditStyle write SetEditStyle;
  end;

{ TJvPopupListbox }

  TJvPopupListbox = class(TCustomListbox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

procedure TJvPopupListbox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
{$IFDEF COMPILER4_UP}
    AddBiDiModeExStyle(ExStyle);
{$ENDIF}
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TJvPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TJvPopupListbox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 4000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SELECTSTRING, WORD(-1),
          Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited KeyPress(Key);
end;

procedure TJvPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TJvInplaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and (X < Width) and
    (Y < Height));
end;

{ TJvInplaceEdit }

constructor TJvInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
//  FEditStyle := esSimple;
  FEditStyle := ieSimple;

end;

procedure TJvInplaceEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of {$IFDEF COMPILER4_UP}Cardinal{$ELSE}Longint{$ENDIF} =
    (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or Alignments[FAlignment];
end;

procedure TJvInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> ieSimple then Dec(R.Right, FButtonWidth);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
{$IFDEF COMPILER3_UP}
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
{$ENDIF}
end;

procedure TJvInplaceEdit.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if FListVisible then begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FPickList.ItemIndex > -1 then
      ListValue := FPickList.Items[FPicklist.ItemIndex];
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept and EditCanModify then Text := ListValue;
  end;
end;

procedure TJvInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then begin
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
  if not FListVisible and Assigned(FActiveList) then begin
    FPickList.Width := Width;
    FPickList.Color := Color;
    FPickList.Font := Font;
    FPickList.Items.Clear;
    with TJvxDrawGrid(Grid) do 
      GetPickList(Col, Row, FPickList.Items);
    FPickList.Height := Min(FPickList.Items.Count, MaxListCount) *
      FPickList.ItemHeight + 4;
    FPickList.ItemIndex := FPickList.Items.IndexOf(Text);
    J := FPickList.ClientWidth;
    for I := 0 to FPickList.Items.Count - 1 do begin
      Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
      if Y > J then J := Y;
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
    Windows.SetFocus(Handle);
  end;
end;

type
  TWinControlCracker = class(TWinControl);

procedure TJvInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = ieEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    TJvxDrawGrid(Grid).EditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else inherited KeyDown(Key, Shift);
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
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X,Y)) then
  begin
    if FListVisible then CloseUp(False)
    else begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then begin
    TrackButton(X, Y);
    if FListVisible then begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
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
    TJvxDrawGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvInplaceEdit.PaintWindow(DC: HDC);
const
  LeftOffs = 3;
var
  R: TRect;
  Flags: Integer;
  W, G, I: Integer;
begin
  if FEditStyle <> ieSimple then begin
    SetRect(R, Width - FButtonWidth, 0, Width, Height);
    Flags := 0;
    if FEditStyle in [iePickList] then begin
      if FActiveList = nil then Flags := DFCS_INACTIVE
      else if FPressed then Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else begin { esEllipsis }
      if FPressed then Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      W := 2;
      G := (FButtonWidth - LeftOffs * 2 - 3 * W) div 2;
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      Flags := R.Left + (FButtonWidth - 3 * W - 2 * G) div 2 + Ord(FPressed);
      I := R.Top + (ClientHeight - W) div 2 {+ Ord(FPressed)};
      PatBlt(DC, Flags, I, W, W, BLACKNESS);
      PatBlt(DC, Flags + G + W, I, W, W, BLACKNESS);
      PatBlt(DC, Flags + 2 * G + 2 * W, I, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TJvInplaceEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvInplaceEdit.SetEditStyle(Value: TInplaceEditStyle);
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    iePickList:
      begin
        if FPickList = nil then begin
          FPickList := TJvPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
    else FActiveList := nil;
  end;
  Repaint;
end;

procedure TJvInplaceEdit.StopTracking;
begin
  if FTracking then begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvInplaceEdit.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvInplaceEdit.UpdateContents;
var
  SaveChanged: TNotifyEvent;
begin
  with TJvxDrawGrid(Grid) do begin
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

procedure TJvInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TJvInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TJvInplaceEdit.WMKillFocus(var Message: TMessage);
begin
{$IFDEF COMPILER3_UP}
  if not SysLocale.FarEast then inherited
  else begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if THandle(Message.WParam) <> TJvxDrawGrid(Grid).Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
{$ELSE}
  inherited;
{$ENDIF}
  CloseUp(False);
end;

procedure TJvInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
    if (FEditStyle <> ieSimple) and PtInRect(Rect(Width - FButtonWidth, 0,
      Width, Height), Point(XPos, YPos)) then Exit;
  inherited;
end;

procedure TJvInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TJvInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> ieSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), ScreenToClient(P)) then
    Windows.SetCursor(LoadCursor(0, IDC_ARROW))
  else inherited;
end;

procedure TJvInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
      if EditStyle in [iePickList] then
        with TWMKey(Message) do begin
          DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (CharCode <> 0) and FListVisible then begin
            with TMessage(Message) do
              SendMessage(FActiveList.Handle, Msg, WParam, LParam);
            Exit;
          end;
        end;
  end;
  inherited;
end;

{$ELSE}

type
  TJvInplaceEdit = class(TInplaceEdit);

{$ENDIF WIN32}

{ TJvxDrawGrid }

constructor TJvxDrawGrid.Create(AOwner: TComponent);
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
end;

destructor TJvxDrawGrid.Destroy;
begin
  FOnChangeFocus := nil;
  FIniLink.Free;
  inherited Destroy;
end;

function TJvxDrawGrid.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvxDrawGrid.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TJvxDrawGrid.IniSave(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalSaveGridLayout(Self, FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TJvxDrawGrid.IniLoad(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalRestoreGridLayout(Self, FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

function TJvxDrawGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  if Assigned(FOnAcceptEditKey) then Result := FOnAcceptEditKey(Self, Key)
  else Result := inherited CanEditAcceptKey(Key);
end;

function TJvxDrawGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if Result and Assigned(FOnShowEditor) then begin
    FOnShowEditor(Self, Col, Row, Result);
    if not Result then EditorMode := False;
  end;
end;

procedure TJvxDrawGrid.DrawPicture(ARect: TRect; Graphic: TGraphic);
begin
  DrawCellBitmap(Self, 0, 0, Graphic, ARect);
end;

procedure TJvxDrawGrid.DrawMasked(ARect: TRect; Graphic: TBitmap);
var
  X, Y: Integer;
begin
  X := (ARect.Right + ARect.Left - Graphic.Width) div 2;
  Y := (ARect.Bottom + ARect.Top - Graphic.Height) div 2;
  DrawBitmapTransparent(Canvas, X, Y, Graphic, Graphic.TransparentColor
    and not PaletteMask);
end;

procedure TJvxDrawGrid.DrawStr(ARect: TRect; const S: string;
  Align: TAlignment);
begin
  DrawCellTextEx(Self, 0, 0, S, ARect, Align, vaCenter, False
    {$IFDEF COMPILER4_UP}, IsRightToLeft {$ENDIF});
end;

procedure TJvxDrawGrid.DrawMultiline(ARect: TRect; const S: string;
  Align: TAlignment);
begin
  DrawCellTextEx(Self, 0, 0, S, ARect, Align, vaTopJustify, True
    {$IFDEF COMPILER4_UP}, IsRightToLeft {$ENDIF});
end;

procedure TJvxDrawGrid.InvalidateCell(ACol, ARow: Longint);
begin
  inherited InvalidateCell(ACol, ARow);
end;

procedure TJvxDrawGrid.InvalidateCol(ACol: Longint);
{$IFNDEF WIN32}
var
  I: Longint;
{$ENDIF}
begin
{$IFDEF WIN32}
  inherited InvalidateCol(ACol);
{$ELSE}
  for I := 0 to RowCount - 1 do inherited InvalidateCell(ACol, I);
{$ENDIF}
end;

procedure TJvxDrawGrid.InvalidateRow(ARow: Longint);
var
  I: Longint;
begin
  for I := 0 to ColCount - 1 do inherited InvalidateCell(I, ARow);
end;

procedure TJvxDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not CanGridAcceptKey(Key, Shift) then Exit;
  if not (ssCtrl in Shift) and (Key = VK_ESCAPE) and EditorMode and
    (InplaceEditor <> nil) and InplaceEditor.Visible and
    not (goAlwaysShowEditor in Options) then
  begin
    FNoUpdateData := True;
    try
      HideEditor;
      if Assigned(FOnCancelEdit) then FOnCancelEdit(Self);
    finally
      FNoUpdateData := False;
    end;
  end
  else inherited KeyDown(Key, Shift);
end;

procedure TJvxDrawGrid.WMCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) and
    not (goAlwaysShowEditor in Options) then
  begin
    if (InplaceEditor <> nil) and InplaceEditor.HandleAllocated and
      InplaceEditor.Visible then TJvInplaceEdit(InplaceEditor).Change;
    Message.NotifyCode := 0;
  end;
  inherited;
end;

procedure TJvxDrawGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

function TJvxDrawGrid.GetEditLimit: Integer;
begin
  Result := inherited GetEditLimit;
  if Assigned(FOnGetEditLimit) then FOnGetEditLimit(Self, Result);
end;

procedure TJvxDrawGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if not FNoUpdateData then inherited SetEditText(ACol, ARow, Value);
end;

procedure TJvxDrawGrid.SetFixedButtons(Value: Boolean);
begin
  if FFixedCellsButtons <> Value then begin
    FFixedCellsButtons := Value;
    Invalidate;
  end;
end;

procedure TJvxDrawGrid.DoFixedCellClick(ACol, ARow: Longint);
begin
  if Assigned(FOnFixedCellClick) then FOnFixedCellClick(Self, ACol, ARow);
end;

procedure TJvxDrawGrid.CheckFixedCellButton(ACol, ARow: Longint; var Enabled: Boolean);
begin
  if (ACol >= 0) and (ARow >= 0) and ((ACol < FixedCols) or (ARow < FixedRows)) then
  begin
    if Assigned(FOnCheckButton) then FOnCheckButton(Self, ACol, ARow, Enabled);
  end
  else Enabled := False;
end;

procedure TJvxDrawGrid.TopLeftChanged;
begin
  if (goRowSelect in Options) and DefaultDrawing then
    InvalidateRow(Self.Row);
  inherited TopLeftChanged;
  if FTracking then StopTracking;
end;

procedure TJvxDrawGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if FTracking then StopTracking;
  if Assigned(FOnColumnSized) then FOnColumnSized(Self);
end;

procedure TJvxDrawGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  if FTracking then StopTracking;
  if Assigned(FOnRowSized) then FOnRowSized(Self);
end;

procedure TJvxDrawGrid.StopTracking;
begin
  if FTracking then begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvxDrawGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
begin
  Cell := MouseCoord(X, Y);
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
    and (FPressedCell.X = Cell.X) and (FPressedCell.Y = Cell.Y);
  if FPressed <> NewPressed then begin
    FPressed := NewPressed;
    InvalidateCell(Cell.X, Cell.Y);
    InvalidateCell(FPressedCell.X, FPressedCell.Y);
  end;
end;

function TJvxDrawGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then begin
    if (ParentForm.ActiveControl = Self) then
      Result := True;
  end
  else begin
    H := GetFocus;
    while IsWindow(H) and (Result = False) do begin
      if H = WindowHandle then Result := True
      else H := GetParent(H);
    end;
  end;
end;

procedure TJvxDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  EnableClick, Fixed: Boolean;
begin
  HideEditor;
  if not (csDesigning in ComponentState) and (CanFocus or
    (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    if not IsActiveControl then begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then begin
    if FFixedCellsButtons then begin
      Cell := MouseCoord(X, Y);
      if not ((Cell.X >= 0) and (Cell.X < FixedCols)) and not
        ((Cell.Y >= 0) and (Cell.Y < FixedRows)) then
      begin
        DblClick;
        Exit;
      end;
    end
    else begin
      DblClick;
      Exit;
    end;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else begin
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
      else if (Button = mbLeft) then begin
        EnableClick := True;
        CheckFixedCellButton(Cell.X, Cell.Y, EnableClick);
        if EnableClick then begin
          MouseCapture := True;
          FTracking := True;
          FPressedCell := Cell;
          TrackButton(X, Y);
        end else Beep;
        Exit;
      end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TJvxDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvxDrawGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol, ARow: Longint;
  DoClick: Boolean;
begin
  if FTracking and (FPressedCell.Y >= 0) and (FPressedCell.X >= 0) then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y = FPressedCell.Y) and (Cell.X = FPressedCell.X);
    StopTracking;
    if DoClick then begin
      ACol := Cell.X;
      ARow := Cell.Y;
      if (ARow < RowCount) and (ACol < ColCount) then
        DoFixedCellClick(ACol, ARow);
    end;
  end
  else if FSwapButtons then begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then Button := mbLeft;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvxDrawGrid.Paint;
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
    with Canvas do begin
      Font.Color := Self.Font.Color;
      Brush.Color := Self.Color;
    end;
    if Row >= FixedRows then begin
      R := BoxRect(FixedCols, Row, ColCount - 1, Row);
      if not (goHorzLine in Options) then Inc(R.Bottom, GridLineWidth);
      DrawFocusRect(Canvas.Handle, R);
    end;
  end;
end;

procedure TJvxDrawGrid.CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TJvxDrawGrid.DoDrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  CallDrawCellEvent(ACol, ARow, ARect, AState);
end;

procedure TJvxDrawGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Down: Boolean;
{$IFDEF WIN32}
  TempRect: TRect;
  FrameFlags1, FrameFlags2: DWORD;
const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
{$ENDIF}
begin
  if FDefaultDrawing or (csDesigning in ComponentState) then
    with Canvas do begin
      Font := Self.Font;
      if ([goRowSelect, goVertLine] * Options = [goRowSelect]) and
        not (gdFixed in AState) then Inc(ARect.Right, GridLineWidth);
      if ([goRowSelect, goHorzLine] * Options = [goRowSelect]) and
        not (gdFixed in AState) then Inc(ARect.Bottom, GridLineWidth);
      if (gdSelected in AState) and (not (gdFocused in AState) or
        ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
      begin
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end
      else begin
        if gdFixed in AState then Brush.Color := FixedColor
        else Brush.Color := Color;
      end;
      FillRect(ARect);
    end;
  Down := FFixedCellsButtons and (gdFixed in AState) and Ctl3D and
    not (csLoading in ComponentState) and FPressed and FDefaultDrawing and
    (FPressedCell.X = ACol) and (FPressedCell.Y = ARow);
  inherited DefaultDrawing := FDefaultDrawing;
  if Down then begin
    Inc(ARect.Left, GridLineWidth);
    Inc(ARect.Top, GridLineWidth);
  end;
  try
    DoDrawCell(ACol, ARow, ARect, AState);
  finally
    inherited DefaultDrawing := False;
    if Down then begin
      Dec(ARect.Left, GridLineWidth);
      Dec(ARect.Top, GridLineWidth);
    end;
  end;
  if FDefaultDrawing and (gdFixed in AState) and Ctl3D then begin
{$IFDEF WIN32}
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    if (FrameFlags1 or FrameFlags2) <> 0 then begin
      TempRect := ARect;
      if ((FrameFlags1 and BF_RIGHT) = 0) and
        (goFixedVertLine in Options) then
        Inc(TempRect.Right, GridLineWidth)
      else if ((FrameFlags1 and BF_BOTTOM) = 0) and
        (goFixedVertLine in Options) then
        Inc(TempRect.Bottom, GridLineWidth);
      DrawEdge(Canvas.Handle, TempRect, EdgeFlag[Down], FrameFlags1);
      DrawEdge(Canvas.Handle, TempRect, EdgeFlag[Down], FrameFlags2);
    end;
{$ELSE}
    with Canvas do begin
      Pen.Color := clBtnHighlight;
      if FFixedCellsButtons then begin
        if Down then begin
          Pen.Color := clBtnShadow;
          with ARect do begin
            PolyLine([Point(Left, Bottom - 1), Point(Left, Top),
              Point(Right, Top)]);
            Inc(Left, 2); Inc(Top, 2);
          end;
        end
        else Frame3D(Canvas, ARect, clBtnHighlight, clBtnShadow, 1);
      end
      else begin
        Polyline([Point(ARect.Left, ARect.Bottom - 1), ARect.TopLeft,
          Point(ARect.Right, ARect.Top)]);
      end;
    end;
{$ENDIF WIN32}
  end;
  if FDefaultDrawing and not (csDesigning in ComponentState) and
    (gdFocused in AState) and
    ([goEditing, goAlwaysShowEditor] * Options <>
    [goEditing, goAlwaysShowEditor])
    and not (goRowSelect in Options) then
    DrawFocusRect(Canvas.Handle, ARect);
end;

{$IFDEF WIN32}
procedure TJvxDrawGrid.WMRButtonUp(var Message: TWMMouse);
begin
  if not (FGridState in [gsColMoving, gsRowMoving]) then
    inherited
  else if not (csNoStdEvents in ControlStyle) then
    with Message do MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
end;
{$ENDIF}

procedure TJvxDrawGrid.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  Cell: TGridCoord;
{$IFNDEF WIN32}
  Form: TForm;
{$ENDIF}
begin
  if FFixedCellsButtons then begin
    with Message do
      Cell := MouseCoord(XPos, YPos);
    if ((Cell.X >= 0) and (Cell.X < FixedCols)) or
      ((Cell.Y >= 0) and (Cell.Y < FixedRows)) then
    begin
{$IFDEF WIN32}
      SendCancelMode(Self);
{$ELSE}
      Form := GetParentForm(Self);
      if Form <> nil then Form.SendCancelMode(Self);
{$ENDIF}
      if csCaptureMouse in ControlStyle then MouseCapture := True;
{$IFDEF WIN32}
      if not (csNoStdEvents in ControlStyle) then
{$ENDIF}
        with Message do
          MouseDown(mbLeft, KeysToShiftState(Keys) - [ssDouble], XPos, YPos);
      Exit;
    end;
  end;
  inherited;
end;

procedure TJvxDrawGrid.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if Assigned(FOnChangeFocus) then FOnChangeFocus(Self);
end;

procedure TJvxDrawGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if Assigned(FOnChangeFocus) then FOnChangeFocus(Self);
end;

procedure TJvxDrawGrid.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

function TJvxDrawGrid.CreateEditor: TInplaceEdit;
begin
{$IFDEF WIN32}
  Result := TJvInplaceEdit.Create(Self);
{$ELSE}
  Result := inherited CreateEditor;
{$ENDIF}
  TEdit(Result).OnChange := EditChanged;
end;

{$IFDEF WIN32}

function TJvxDrawGrid.GetEditAlignment(ACol, ARow: Longint): TAlignment;
begin
  Result := taLeftJustify;
  if Assigned(FOnGetEditAlign) then FOnGetEditAlign(Self, ACol, ARow, Result);
end;

function TJvxDrawGrid.GetEditStyle(ACol, ARow: Longint): TInplaceEditStyle;
begin
  Result := ieSimple;
  if Assigned(FOnGetEditStyle) then FOnGetEditStyle(Self, ACol, ARow, Result);
end;

procedure TJvxDrawGrid.GetPicklist(ACol, ARow: Longint; PickList: TStrings);
begin
  if Assigned(FOnGetPicklist) then FOnGetPicklist(Self, ACol, ARow, PickList);
end;

procedure TJvxDrawGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then FOnEditButtonClick(Self);
end;

{$ENDIF}

end.
