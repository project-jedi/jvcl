{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCtrls.PAS, released Oct 10, 1999.

The Initial Developer of the Original Code is Petr Vones (petr.v@mujmail.cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s):
Peter Below [100113.1101@compuserve.com] - alternate TJvPageControl.OwnerDraw routine
Peter Thörnqvist [peter3@peter3.com] added AddressValues property to TJvIPAddress
Alfi [alioscia_alessi@onde.net] alternate TJvPageControl.OwnerDraw routine
Rudy Velthuis - ShowRange in TJvTrackBar

Last Modified: 2002-06-27
Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  TJvTreeView:
    When dragging an item and MultiSelect is True droptarget node is not painted
    correctly.
  TJvIpAddress:
    Can't focus next control by TAB key on D4.
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvComCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, CommCtrl, StdActns,
  {$IFDEF COMPILER5_UP}
  Contnrs,
  {$ENDIF}
  JclBase, JVCLVer;

const
  JvDefPageControlBorder = 4;
  TVM_SETLINECOLOR = TV_FIRST + 40;
  TVM_GETLINECOLOR = TV_FIRST + 41;


type
  TJvIpAddress = class;

  TJvIpAddressMinMax = record
    Min: Byte;
    Max: Byte;
  end;

  TJvIpAddressRange = class(TPersistent)
  private
    FControl: TWinControl;
    FRange: array [0..3] of TJvIpAddressMinMax;
    function GetMaxRange(Index: Integer): Byte;
    function GetMinRange(Index: Integer): Byte;
    procedure SetMaxRange(const Index: Integer; const Value: Byte);
    procedure SetMinRange(const Index: Integer; const Value: Byte);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Change(Index: Integer);
  public
    constructor Create(Control: TWinControl);
  published
    property Field1Min: Byte index 0 read GetMinRange write SetMinRange default 0;
    property Field1Max: Byte index 0 read GetMaxRange write SetMaxRange default 255;
    property Field2Min: Byte index 1 read GetMinRange write SetMinRange default 0;
    property Field2Max: Byte index 1 read GetMaxRange write SetMaxRange default 255;
    property Field3Min: Byte index 2 read GetMinRange write SetMinRange default 0;
    property Field3Max: Byte index 2 read GetMaxRange write SetMaxRange default 255;
    property Field4Min: Byte index 3 read GetMinRange write SetMinRange default 0;
    property Field4Max: Byte index 3 read GetMaxRange write SetMaxRange default 255;
  end;

  TJvIpAddrFieldChangeEvent = procedure(Sender: TJvIpAddress; FieldIndex: Integer;
    FieldRange: TJvIpAddressMinMax; var Value: Integer) of object;
  TJvIPAddressChanging = procedure(Sender: TObject; Index: Integer; Value: Byte; var AllowChange: Boolean) of object;

  TJvIpAddressValues = class(TPersistent)
  private
    FValues: array [0..3] of Byte;
    FOnChange: TNotifyEvent;
    FOnChanging: TJvIPAddressChanging;
    function GetValue: Cardinal;
    procedure SetValue(const AValue: Cardinal);
    procedure SetValues(Index: Integer; Value: Byte);
    function GetValues(Index: Integer): Byte;
  protected
    procedure Change; virtual;
    function Changing(Index: Integer; Value: Byte): Boolean; virtual;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TJvIPAddressChanging read FOnChanging write FOnChanging;
  published
    property Address: Cardinal read GetValue write SetValue;
    property Value1: Byte index 0 read GetValues write SetValues;
    property Value2: Byte index 1 read GetValues write SetValues;
    property Value3: Byte index 2 read GetValues write SetValues;
    property Value4: Byte index 3 read GetValues write SetValues;
  end;

  TJvIpAddress = class(TWinControl)
  private
    {    FEditControls: array [0..3] of HWND;
        FEditControlCount: Integer;}
    FAddress: LongWord;
    FChanging: Boolean;
    FRange: TJvIpAddressRange;
    FAddressValues: TJvIpAddressValues;
    FSaveBlank: Boolean;
    FOnFieldChange: TJvIpAddrFieldChangeEvent;
    LocalFont: HFONT;
    FOnChange: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    //    procedure ClearEditControls;
    procedure DestroyLocalFont;
    procedure SetAddress(const Value: LongWord);
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Msg: TWMNCDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMParentNotify(var Msg: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMSetFont(var Msg: TWMSetFont); message WM_SETFONT;
    procedure SetAddressValues(const Value: TJvIpAddressValues);
  protected
    procedure AdjustHeight;
    procedure AdjustSize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoChange; dynamic;
    procedure DoAddressChange(Sender: TObject); virtual;
    procedure DoAddressChanging(Sender: TObject; Index: Integer;
      Value: Byte; var AllowChange: Boolean); virtual;
    procedure DoFieldChange(FieldIndex: Integer; var FieldValue: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAddress;
    function IsBlank: Boolean;
    property Text;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Address: LongWord read FAddress write SetAddress default 0;
    property AddressValues: TJvIpAddressValues read FAddressValues write SetAddressValues;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Range: TJvIpAddressRange read FRange write FRange;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldChange: TJvIpAddrFieldChangeEvent read FOnFieldChange write FOnFieldChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvPageControl = class(TPageControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FClientBorderWidth: TBorderWidth;
    FHideAllTabs: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FDrawTabShadow: Boolean;
    FHandleGlobalTab: Boolean;
    procedure SetClientBorderWidth(const Value: TBorderWidth);
    procedure TCMAdjustRect(var Msg: TMessage); message TCM_ADJUSTRECT;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    procedure SetDrawTabShadow(const Value: Boolean);
    procedure SetHideAllTabs(const Value: Boolean);
    function FormKeyPreview: Boolean;
  protected
    procedure Loaded; override;
    procedure DrawDefaultTab(TabIndex: Integer; const Rect: TRect; Active: Boolean; DefaultDraw: Boolean);
    procedure DrawShadowTab(TabIndex: Integer; const Rect: TRect; Active: Boolean; DefaultDraw: Boolean);
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTabImages;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HandleGlobalTab: Boolean read FHandleGlobalTab write FHandleGlobalTab default False;
    property ClientBorderWidth: TBorderWidth read FClientBorderWidth write SetClientBorderWidth default
      JvDefPageControlBorder;
    property DrawTabShadow: Boolean read FDrawTabShadow write SetDrawTabShadow default False;
    property HideAllTabs: Boolean read FHideAllTabs write SetHideAllTabs default False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property Color;
  end;

  TJvTrackToolTipSide = (tsLeft, tsTop, tsRight, tsBottom);
  TJvTrackToolTipEvent = procedure(Sender: TObject; var ToolTipText: string) of object;

  TJvTrackBar = class(TTrackBar)
  private
    FToolTips: Boolean;
    FToolTipSide: TJvTrackToolTipSide;
    FToolTipText: WideString;
    FOnToolTip: TJvTrackToolTipEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FShowRange: Boolean;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetToolTips(const Value: Boolean);
    procedure SetToolTipSide(const Value: TJvTrackToolTipSide);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure CNHScroll(var Msg: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Msg: TWMVScroll); message CN_VSCROLL;
    procedure SetShowRange(const Value: Boolean);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure InternalSetToolTipSide;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ShowRange: Boolean read FShowRange write SetShowRange default True;
    property ToolTips: Boolean read FToolTips write SetToolTips default False;
    property ToolTipSide: TJvTrackToolTipSide read FToolTipSide write SetToolTipSide default tsLeft;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnToolTip: TJvTrackToolTipEvent read FOnToolTip write FOnToolTip;
  end;

  TJvTreeNode = class(TTreeNode)
  private
    FBold: Boolean;
    FChecked: Boolean;
    FPopupMenu: TPopupMenu;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetBold: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
    constructor CreateEnh(AOwner: TTreeNodes);
    property Checked: Boolean read GetChecked write SetChecked;
    property Bold: Boolean read GetBold write SetBold;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  TPageChangedEvent = procedure(Sender: TObject; Item: TTreeNode; Page: TTabSheet) of object;

  TJvTreeView = class(TTreeView)
  private
    FAutoDragScroll: Boolean;
    FClearBeforeSelect: Boolean;
    FMultiSelect: Boolean;
    FScrollDirection: Integer;
    FSelectedList: TObjectList;
    FSelectThisNode: Boolean;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnEditCancelled: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOver: Boolean;
    FCheckBoxes: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FPageControl: TPageControl;
    FOnPage: TPageChangedEvent;
    procedure InternalCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    function GetSelectedCount: Integer;
    function GetSelectedItem(Index: Integer): TTreeNode;
    {$IFNDEF COMPILER6_UP}
    procedure SetMultiSelect(const Value: Boolean);
    {$ENDIF}
    procedure SetScrollDirection(const Value: Integer);
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetCheckBoxes(const Value: Boolean);
    function GetItemHeight: integer;
    procedure SetItemHeight(Value: integer);
    function GetInsertMarkColor: TColor;
    procedure SetInsertMarkColor(Value: TColor);
    function GetLineColor: TColor;
    procedure SetLineColor(Value: TColor);
    function GetMaxScrollTime: integer;
    procedure SetMaxScrollTime(const Value: integer);
    function GetUseUnicode: boolean;
    procedure SetUseUnicode(const Value: boolean);
  protected
    function CreateNode: TTreeNode; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNotify(var Msg: TWMNotify); message CN_NOTIFY;

    procedure Change(Node: TTreeNode); override;
    procedure Delete(Node: TTreeNode); override;
    procedure DoEditCancelled; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoSelectionChange; dynamic;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure Edit(const Item: TTVItem); override;
    procedure InvalidateNode(Node: TTreeNode);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ResetPostOperationFlags;
    property ScrollDirection: Integer read FScrollDirection write SetScrollDirection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearSelection; reintroduce;
    function IsNodeSelected(Node: TTreeNode): Boolean;
    procedure InvalidateNodeIcon(Node: TTreeNode);
    procedure InvalidateSelectedItems;
    procedure SelectItem(Node: TTreeNode; Unselect: Boolean = False);
    property SelectedItems[Index: Integer]: TTreeNode read GetSelectedItem;
    property SelectedCount: Integer read GetSelectedCount;
    function GetBold(Node: TTreeNode): Boolean;
    procedure SetBold(Node: TTreeNode; Value: Boolean);
    function GetChecked(Node: TTreenode): Boolean;
    procedure SetChecked(Node: TTreenode; Value: Boolean);
    procedure SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
    function GetNodePopup(Node: TTreeNode): TPopupMenu;
    procedure InsertMark(Node:TTreeNode;MarkAfter:boolean); // TVM_SETINSERTMARK
    procedure RemoveMark;
    property InsertMarkColor:TColor read GetInsertMarkColor write SetInsertMarkColor;
    property Checked[Node:TTreeNode]:boolean read GetChecked write SetChecked;
    property MaxScrollTime:integer read GetMaxScrollTime write SetMaxScrollTime;
    // UseUnicode should only be changed on Win95 and Win98 that has IE5 or later installed
    property UseUnicode:boolean read GetUseUnicode write SetUseUnicode default false;
  published
    property LineColor:TColor read GetLineColor write SetLineColor default clDefault;
    property ItemHeight:integer read GetItemHeight write SetItemHeight default 16;

    property HintColor: TColor read FColor write FColor default clInfoBk;

    property Checkboxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    property PageControl: TPageControl read FPageControl write FPageControl;
    property OnPageChanged: TPageChangedEvent read FOnPage write FOnPage;

    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoDragScroll: Boolean read FAutoDragScroll write FAutoDragScroll default False;
    {$IFNDEF COMPILER6_UP}
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    {$ENDIF}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnEditCancelled: TNotifyEvent read FOnEditCancelled write FOnEditCancelled;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses
  JclSysUtils, JvFunctions, JvTypes;

const
  TVIS_CHECKED = $2000;

// === TJvIpAddressRange =====================================================

constructor TJvIpAddressRange.Create(Control: TWinControl);
var
  I: Integer;
begin
  inherited Create;
  FControl := Control;
  for I := Low(FRange) to High(FRange) do
  begin
    FRange[I].Min := 0;
    FRange[I].Max := 255;
  end;
end;

procedure TJvIpAddressRange.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvIpAddressRange then
    with TJvIpAddressRange(Dest) do
    begin
      FRange := Self.FRange;
      Change(-1);
    end
  else
    inherited AssignTo(Dest);
end;

procedure TJvIpAddressRange.Change(Index: Integer);
var
  I: Integer;

  procedure ChangeRange(FieldIndex: Integer);
  begin
    with FRange[FieldIndex] do
      FControl.Perform(IPM_SETRANGE, FieldIndex, MAKEIPRANGE(Min, Max));
  end;

begin
  if not FControl.HandleAllocated then
    Exit;
  if Index = -1 then
    for I := Low(FRange) to High(FRange) do
      ChangeRange(I)
  else
    ChangeRange(Index);
end;

function TJvIpAddressRange.GetMaxRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Max;
end;

function TJvIpAddressRange.GetMinRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Min;
end;

procedure TJvIpAddressRange.SetMaxRange(const Index: Integer; const Value: Byte);
begin
  FRange[Index].Max := Value;
  Change(Index);
end;

procedure TJvIpAddressRange.SetMinRange(const Index: Integer; const Value: Byte);
begin
  FRange[Index].Min := Value;
  Change(Index);
end;

// === TJvIpAddress ==========================================================

constructor TJvIpAddress.Create(AOwner: TComponent);
begin
  CheckCommonControl(ICC_INTERNET_CLASSES);
  inherited Create(AOwner);
  FRange := TJvIpAddressRange.Create(Self);
  FAddressValues := TJvIpAddressValues.Create;
  FAddressValues.OnChange := DoAddressChange;
  FAddressValues.OnChanging := DoAddressChanging;

  ControlStyle := ControlStyle + [csFixedHeight, csReflector];
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := 150;
  AdjustHeight;
end;

destructor TJvIpAddress.Destroy;
begin
  FreeAndNil(FRange);
  FreeAndNil(FAddressValues);
  inherited Destroy;
end;

procedure TJvIpAddress.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_IPADDRESS);
  with Params do
  begin
    Style := Style or WS_CHILD;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TJvIpAddress.CreateWnd;
begin
  //  ClearEditControls;
  FChanging := True;
  try
    inherited CreateWnd;
    FRange.Change(-1);
    if FSaveBlank then
      ClearAddress
    else
    begin
      Perform(IPM_SETADDRESS, 0, FAddress);
      FAddressValues.Address := FAddress;
    end;
  finally
    FChanging := False;
  end;
end;

procedure TJvIpAddress.DestroyLocalFont;
begin
  if LocalFont <> 0 then
  begin
    OSCheck(DeleteObject(LocalFont));
    LocalFont := 0;
  end;
end;

procedure TJvIpAddress.DestroyWnd;
begin
  FSaveBlank := IsBlank;
  inherited DestroyWnd;
end;

procedure TJvIpAddress.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  //  I: Integer;
  //  R: TRect;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + (GetSystemMetrics(SM_CYBORDER) * 8);
  {  for I := 0 to FEditControlCount - 1 do
    begin
      GetWindowRect(FEditControls[I], R);
      R.TopLeft := ScreenToClient(R.TopLeft);
      R.BottomRight := ScreenToClient(R.BottomRight);
      OffsetRect(R, -R.Left, -R.Top);
      R.Bottom := ClientHeight;
      SetWindowPos(FEditControls[I], 0, 0, 0, R.Right, R.Bottom,
        SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
    end;}
end;

procedure TJvIpAddress.AdjustSize;
begin
  inherited AdjustSize;
  RecreateWnd;
end;

procedure TJvIpAddress.ClearAddress;
begin
  if HandleAllocated then
    Perform(IPM_CLEARADDRESS, 0, 0);
  FAddressValues.Address := 0;
end;

{procedure TJvIpAddress.ClearEditControls;
begin
  FillChar(FEditControls, Sizeof(FEditControls), #0);
  FEditControlCount := 0;
end;}

procedure TJvIpAddress.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvIpAddress.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustHeight;
  Invalidate;
end;

procedure TJvIpAddress.CNCommand(var Msg: TWMCommand);
begin
  with Msg do
    case NotifyCode of
      EN_CHANGE:
        begin
          Perform(IPM_GETADDRESS, 0, Integer(@FAddress));
          if not FChanging then
            DoChange;
        end;
      EN_KILLFOCUS:
        begin
          FChanging := True;
          try
            if not IsBlank then
              Perform(IPM_SETADDRESS, 0, FAddress);
          finally
            FChanging := False;
          end;
        end;
    end;
  inherited;
end;

procedure TJvIpAddress.CNNotify(var Msg: TWMNotify);
begin
  with Msg, NMHdr^ do
    if code = IPN_FIELDCHANGED then
      with PNMIPAddress(NMHdr)^ do
        DoFieldChange(iField, iValue);
  inherited;
end;

procedure TJvIpAddress.DoAddressChange(Sender: TObject);
begin
  Address := FAddressValues.Address;
end;

procedure TJvIpAddress.DoAddressChanging(Sender: TObject; Index: Integer; Value: Byte; var AllowChange: Boolean);
begin
  AllowChange := (Index > -1) and (Index < 4) and
    (Value >= FRange.FRange[Index].Min) and (Value <= FRange.FRange[Index].Max);
end;

procedure TJvIpAddress.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvIpAddress.DoFieldChange(FieldIndex: Integer; var FieldValue: Integer);
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(Self, FieldIndex, FRange.FRange[FieldIndex], FieldValue);
end;

function TJvIpAddress.IsBlank: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, IPM_ISBLANK, 0, 0) <> 0;
end;

procedure TJvIpAddress.SetAddress(const Value: LongWord);
begin
  if FAddress <> Value then
  begin
    FAddress := Value;
    if HandleAllocated then
      Perform(IPM_SETADDRESS, 0, FAddress);
    FAddressValues.Address := Value;
  end;
end;

procedure TJvIpAddress.SetAddressValues(const Value: TJvIpAddressValues);
begin
  //  (p3) do nothing
end;

procedure TJvIpAddress.WMDestroy(var Msg: TWMNCDestroy);
begin
  DestroyLocalFont;
  inherited;
end;

procedure TJvIpAddress.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvIpAddress.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  with Msg do
    Result := Result or DLGC_WANTARROWS;
end;

procedure TJvIpAddress.WMParentNotify(var Msg: TWMParentNotify);
begin
  with Msg do
    case Event of
      {      WM_CREATE:
              begin
                FEditControls[FEditControlCount] := ChildWnd;
                Inc(FEditControlCount);
              end;
            WM_DESTROY:
                ClearEditControls;}
      WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN:
        //        if not (csDesigning in ComponentState) then
        Perform(Event, Value, Integer(SmallPoint(XPos, YPos)));
    end;
  inherited;
end;

procedure TJvIpAddress.WMSetFont(var Msg: TWMSetFont);
var
  LF: TLogFont;
begin
  FillChar(LF, Sizeof(TLogFont), #0);
  try
    OSCheck(GetObject(Font.Handle, Sizeof(LF), @LF) > 0);
    DestroyLocalFont;
    LocalFont := CreateFontIndirect(LF);
    Msg.Font := LocalFont;
    inherited;
  except
    Application.HandleException(Self);
  end;
end;

// === TJvPageControl ========================================================

constructor TJvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FClientBorderWidth := JvDefPageControlBorder;
end;

function TJvPageControl.FormKeyPreview: Boolean;
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
    Result := F.KeyPreview
  else
    Result := False;
end;

procedure TJvPageControl.CMDialogKey(var Msg: TWMKey);
var
  thistab, tab: TTabSheet;
  forward: Boolean;
begin
  if HandleGlobalTab and not FormKeyPreview and (Msg.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    thistab := ActivePage;
    forward := GetKeyState(VK_SHIFT) >= 0;
    tab := thistab;
    repeat
      tab := FindNextPage(tab, forward, True);
    until (tab = nil) or tab.Enabled or (tab = thistab);
    if tab <> thistab then
    begin
      if CanChange then
      begin
        ActivePage := tab;
        Msg.Result := 1;
        Change;
      end;
      Exit;
    end;
  end;
  inherited;
end;

procedure TJvPageControl.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvPageControl.DrawDefaultTab(TabIndex: Integer;
  const Rect: TRect; Active: Boolean; DefaultDraw: Boolean);
var
  imageindex: Integer;
  r: TRect;
  S: string;
begin
  if not Pages[TabIndex].Enabled then
    Canvas.Font.Color := clGrayText;
  if Active then
    Canvas.Font.Style := [fsBold];
  if DefaultDraw then
    Exit;
  r := Rect;
  Canvas.Fillrect(r);
  imageindex := GetImageIndex(tabindex);
  if (imageindex >= 0) and Assigned(Images) then
  begin
    SaveDC(canvas.handle);
    images.Draw(Canvas, Rect.Left + 4, Rect.Top + 2,
      imageindex,
      Pages[TabIndex].enabled);
    // images.draw fouls the canvas colors if it draws
    // the image disabled, thus the SaveDC/RestoreDC
    RestoreDC(canvas.handle, -1);
    R.Left := R.Left + images.Width + 4;
  end;
  S := Pages[TabIndex].Caption;
  InflateRect(r, -2, -2);
  // (p3) TODO: draw rotated when TabPosition in tbLeft,tbRight
  DrawText(Canvas.Handle, PChar(S), Length(S), r, DT_SINGLELINE or DT_LEFT or DT_TOP);
end;

procedure TJvPageControl.DrawShadowTab(TabIndex: Integer;
  const Rect: TRect; Active: Boolean; DefaultDraw: Boolean);
var
  ImageIndex: Integer;
  R: TRect;
  S: string;
begin
  //inherited;
  if not Pages[TabIndex].Enabled then
    Canvas.Font.Color := clGrayText;
  if not Active then
  begin
    with Canvas do
    begin
      Brush.Color := clInactiveCaption;
      Font.Color := clInactiveCaptionText;
    end;
  end;
  if DefaultDraw then
    Exit;
  R := Rect;
  Canvas.Fillrect(R);
  ImageIndex := GetImageIndex(TabIndex);
  if (ImageIndex >= 0) and Assigned(Images) then
  begin
    SaveDC(canvas.handle);
    Images.Draw(Canvas, Rect.Left + 4, Rect.Top + 2,
      ImageIndex,
      Pages[TabIndex].Enabled);
    RestoreDC(Canvas.Handle, -1);
    R.Left := R.Left + Images.Width + 4;
  end;
  S := Pages[TabIndex].Caption;
  InflateRect(R, -2, -2);
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_LEFT or DT_TOP);
end;

procedure TJvPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if DrawTabShadow then
    DrawShadowTab(TabIndex, Rect, Active, Assigned(OnDrawTab))
  else
    DrawDefaultTab(TabIndex, Rect, Active, Assigned(OnDrawTab));
end;

procedure TJvPageControl.Loaded;
begin
  inherited Loaded;
  HideAllTabs := FHideAllTabs;
end;

procedure TJvPageControl.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvPageControl.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvPageControl.SetClientBorderWidth(const Value: TBorderWidth);
begin
  if FClientBorderWidth <> Value then
  begin
    FClientBorderWidth := Value;
    RecreateWnd;
  end;
end;

procedure TJvPageControl.SetDrawTabShadow(const Value: Boolean);
begin
  if FDrawTabShadow <> Value then
  begin
    FDrawTabShadow := Value;
    Invalidate;
  end;
end;

procedure TJvPageControl.SetHideAllTabs(const Value: Boolean);
var
  I: Integer;
  SaveActivePage: TTabSheet;
begin
  FHideAllTabs := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if HandleAllocated then
  begin
    SaveActivePage := ActivePage;
    for I := 0 to PageCount - 1 do
      Pages[I].TabVisible := not FHideAllTabs;
    ActivePage := SaveActivePage;
    if FHideAllTabs then
      TabStop := False;
  end;
end;

procedure TJvPageControl.TCMAdjustRect(var Msg: TMessage);
var
  Offset: Integer;
begin
  inherited;
  if (Msg.WParam = 0) and (FClientBorderWidth <> JvDefPageControlBorder) then
  begin
    Offset := JvDefPageControlBorder - FClientBorderWidth;
    InflateRect(PRect(Msg.LParam)^, Offset, Offset);
  end;
end;

procedure TJvPageControl.UpdateTabImages;
begin
  inherited UpdateTabImages;
end;

procedure TJvPageControl.WMLButtonDown(var Msg: TWMLButtonDown);
var
  hi: TTCHitTestInfo;
  tabindex: Integer;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  hi.pt.x := Msg.XPos;
  hi.pt.y := Msg.YPos;
  hi.flags := 0;
  tabindex := Perform(TCM_HITTEST, 0, Longint(@hi));
  if (tabindex >= 0) and ((hi.flags and TCHT_ONITEM) <> 0) then
    if not Pages[tabindex].Enabled then
    begin
      Msg.Result := 0;
      Exit;
    end;
  inherited;
end;

// === TJvTrackBar ===========================================================

constructor TJvTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FToolTipSide := tsLeft;
  FShowRange := True;
end;

procedure TJvTrackBar.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvTrackBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvTrackBar.CNHScroll(var Msg: TWMHScroll);
begin
  if Msg.ScrollCode <> SB_ENDSCROLL then
    inherited;
end;

procedure TJvTrackBar.CNVScroll(var Msg: TWMVScroll);
begin
  if Msg.ScrollCode <> SB_ENDSCROLL then
    inherited;
end;

procedure TJvTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FToolTips and (GetComCtlVersion >= ComCtlVersionIE3) then
      Style := Style or TBS_TOOLTIPS;
    // (p3) this stolen from Rudy Velthuis's ExTrackBar
    if not ShowRange then
      Style := Style and not TBS_ENABLESELRANGE;
  end;
end;

procedure TJvTrackBar.CreateWnd;
begin
  inherited CreateWnd;
  InternalSetToolTipSide;
end;

procedure TJvTrackBar.InternalSetToolTipSide;
const
  ToolTipSides: array [TJvTrackToolTipSide] of DWORD =
    (TBTS_LEFT, TBTS_TOP, TBTS_RIGHT, TBTS_BOTTOM);
begin
  if HandleAllocated and (GetComCtlVersion >= ComCtlVersionIE3) then
    SendMessage(Handle, TBM_SETTIPSIDE, ToolTipSides[FToolTipSide], 0);
end;

procedure TJvTrackBar.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvTrackBar.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvTrackBar.SetShowRange(const Value: Boolean);
begin
  if FShowRange <> Value then
  begin
    FShowRange := Value;
    RecreateWnd;
  end;
end;

procedure TJvTrackBar.SetToolTips(const Value: Boolean);
begin
  if FToolTips <> Value then
  begin
    FToolTips := Value;
    RecreateWnd;
  end;
end;

procedure TJvTrackBar.SetToolTipSide(const Value: TJvTrackToolTipSide);
begin
  if FToolTipSide <> Value then
  begin
    FToolTipSide := Value;
    InternalSetToolTipSide;
  end;
end;

procedure TJvTrackBar.WMNotify(var Msg: TWMNotify);
var
  ToolTipTextLocal: string;
begin
  with Msg do
    if (NMHdr^.code = TTN_NEEDTEXTW) and Assigned(FOnToolTip) then
      with PNMTTDispInfoW(NMHdr)^ do
      begin
        hinst := 0;
        ToolTipTextLocal := IntToStr(Position);
        FOnToolTip(Self, ToolTipTextLocal);
        FToolTipText := ToolTipTextLocal;
        lpszText := PWideChar(FToolTipText);
        FillChar(szText, Sizeof(szText), #0);
        Result := 1;
      end
    else
      inherited;
end;

// === TJvTreeNode ===========================================================

constructor TJvTreeNode.CreateEnh(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);
  FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

procedure TJvTreeNode.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

function TJvTreeNode.GetBold: Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := ((Item.State and TVIS_BOLD) = TVIS_BOLD)
    else
      Result := False;
  end;
end;

function TJvTreeNode.GetChecked: Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := ((Item.State and TVIS_CHECKED) = TVIS_CHECKED)
    else
      Result := False;
  end;
end;

procedure TJvTreeNode.SetBold(const Value: Boolean);
var
  Item: TTVItem;
begin
  FBold := Value;
  FillChar(Item, SizeOf(Item), 0);
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    StateMask := TVIS_BOLD;
    if FBold then
      Item.State := TVIS_BOLD
    else
      Item.State := 0;
    TreeView_SetItem(Handle, Item);
  end;
end;

procedure TJvTreeNode.SetChecked(Value: Boolean);
var
  Item: TTVItem;
begin
  FChecked := Value;
  FillChar(Item, SizeOf(Item), 0);
  with Item do
  begin
    hItem := ItemId;
    mask := TVIF_STATE;
    StateMask := TVIS_STATEIMAGEMASK;
    if FChecked then
      Item.State := TVIS_CHECKED
    else
      Item.State := TVIS_CHECKED shr 1;
    TreeView_SetItem(Handle, Item);
  end;
end;

// === TJvTreeView ===========================================================

const
  AutoScrollMargin = 20;
  AutoScrollTimerID = 100;

constructor TJvTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FCheckBoxes := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FSelectedList := TObjectList.Create(False);
  // Since IsCustomDrawn method is not virtual we have to assign ancestor's
  // OnCustomDrawItem event to enable custom drawing
  if not (csDesigning in ComponentState) then
    inherited OnCustomDrawItem := InternalCustomDrawItem;
end;

destructor TJvTreeView.Destroy;
begin
  FreeAndNil(FSelectedList);
  inherited Destroy;
end;

procedure TJvTreeView.Change(Node: TTreeNode);
begin
  if FClearBeforeSelect then
  begin
    FClearBeforeSelect := False;
    ClearSelection;
  end;
  if FSelectThisNode then
  begin
    FSelectThisNode := False;
    SelectItem(Node);
  end;
  inherited Change(Node);
end;

procedure TJvTreeView.ClearSelection;
var
  NeedInvalidate: array of TTreeNode;
  I: Integer;
begin
  FClearBeforeSelect := False;
  if FSelectedList.Count = 0 then
    Exit;
  DoSelectionChange;
  SetLength(NeedInvalidate, FSelectedList.Count);
  for I := 0 to FSelectedList.Count - 1 do
    NeedInvalidate[I] := SelectedItems[I];
  FSelectedList.Clear;
  for I := 0 to Length(NeedInvalidate) - 1 do
    InvalidateNode(NeedInvalidate[I]);
end;

procedure TJvTreeView.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvTreeView.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

function TJvTreeView.CreateNode: TTreeNode;
begin
  Result := TJvTreeNode.CreateEnh(Items);
end;

procedure TJvTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;
end;

procedure TJvTreeView.Delete(Node: TTreeNode);
begin
  if FMultiSelect then
    FSelectedList.Remove(Node);
  inherited Delete(Node);
end;

procedure TJvTreeView.DoEditCancelled;
begin
  if Assigned(FOnEditCancelled) then
    FOnEditCancelled(Self);
end;

procedure TJvTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  ScrollDirection := 0;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TJvTreeView.DoEnter;
begin
  InvalidateSelectedItems;
  inherited;
end;

procedure TJvTreeView.DoExit;
begin
  InvalidateSelectedItems;
  inherited DoExit;
end;

procedure TJvTreeView.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TJvTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if not FAutoDragScroll then
    Exit;
  if Y < AutoScrollMargin then
    ScrollDirection := -1
  else
  if Y > ClientHeight - AutoScrollMargin then
    ScrollDirection := 1
  else
    ScrollDirection := 0;
end;

procedure TJvTreeView.Edit(const Item: TTVItem);
begin
  inherited Edit(Item);
  if Item.pszText = nil then
    DoEditCancelled;
end;

function TJvTreeView.GetBold(Node: TTreeNode): Boolean;
begin
  Result := TJvTreeNode(Node).Bold;
end;

function TJvTreeView.GetChecked(Node: TTreenode): Boolean;
begin
  Result := TJvTreeNode(Node).Checked;
end;

function TJvTreeView.GetNodePopup(Node: TTreeNode): TPopupMenu;
begin
  Result := TJvTreeNode(Node).PopupMenu;
end;

function TJvTreeView.GetSelectedCount: Integer;
begin
  if FMultiSelect then
    Result := FSelectedList.Count
  else
    Result := -1;
end;

function TJvTreeView.GetSelectedItem(Index: Integer): TTreeNode;
begin
  Result := TTreeNode(FSelectedList[Index]);
end;

procedure TJvTreeView.InternalCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if FMultiSelect then
  begin
    with Canvas.Font do
    begin // fix HotTrack bug in custom drawing
      OnChange(nil);
      if cdsHot in State then
      begin
        Style := Style + [fsUnderLine];
        if cdsSelected in State then
          Color := clHighlightText
        else
          Color := clHighlight;
      end;
    end;
    if IsNodeSelected(Node) then
    begin
      if Focused then
      begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
      end
      else
      if not HideSelection then
      begin
        Canvas.Font.Color := Font.Color;
        Canvas.Brush.Color := clInactiveBorder;
      end;
    end
    else
    begin
      Canvas.Font.Color := Font.Color;
      Canvas.Brush.Color := Color;
    end;
  end;
  if Assigned(FOnCustomDrawItem) then
    FOnCustomDrawItem(Self, Node, State, DefaultDraw);
end;

procedure TJvTreeView.InvalidateNode(Node: TTreeNode);
var
  R: TRect;
begin
  if Assigned(Node) and Node.IsVisible then
  begin
    R := Node.DisplayRect(True);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvTreeView.InvalidateNodeIcon(Node: TTreeNode);
var
  R: TRect;
begin
  if Assigned(Node) and Assigned(Images) and Node.IsVisible then
  begin
    R := Node.DisplayRect(True);
    R.Right := R.Left;
    R.Left := R.Left - Images.Width * 3;
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TJvTreeView.InvalidateSelectedItems;
var
  I: Integer;
begin
  if HandleAllocated then
    for I := 0 to SelectedCount - 1 do
      InvalidateNode(SelectedItems[I]);
end;

function TJvTreeView.IsNodeSelected(Node: TTreeNode): Boolean;
begin
  Result := FSelectedList.IndexOf(Node) <> -1;
end;

procedure TJvTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FMultiSelect then
  begin
    ResetPostOperationFlags;
    if not (ssAlt in Shift) then
    begin
      if (Key = VK_SPACE) then
        SelectItem(Selected, IsNodeSelected(Selected))
      else
      begin
        FSelectThisNode := True;
        if Shift * [ssShift, ssCtrl] = [] then
          FClearBeforeSelect := True;
      end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvTreeView.KeyPress(var Key: Char);
begin
  if FMultiSelect and (Key = #32) then
    Key := #0
  else
    inherited KeyPress(Key);
end;

procedure TJvTreeView.MouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvTreeView.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvTreeView.ResetPostOperationFlags;
begin
  FClearBeforeSelect := False;
  FSelectThisNode := False;
end;

procedure TJvTreeView.SelectItem(Node: TTreeNode; Unselect: Boolean);
begin
  if Unselect then
    FSelectedList.Remove(Node)
  else
  if not IsNodeSelected(Node) then
    FSelectedList.Add(Node);
  if HandleAllocated then
    InvalidateNode(Node);
  DoSelectionChange;
end;

procedure TJvTreeView.SetBold(Node: TTreeNode; Value: Boolean);
begin
  TJvTreeNode(Node).Bold := Value;
end;

procedure TJvTreeView.SetCheckBoxes(const Value: Boolean);
begin
  FCheckBoxes := Value;
  RecreateWnd;
end;

procedure TJvTreeView.SetChecked(Node: TTreenode; Value: Boolean);
begin
  TJvTreeNode(Node).Checked := Value;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvTreeView.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    ResetPostOperationFlags;
    ClearSelection;
  end;
end;
{$ENDIF}

procedure TJvTreeView.SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
begin
  TJvTreeNode(Node).PopupMenu := Value;
end;

procedure TJvTreeView.SetScrollDirection(const Value: Integer);
begin
  if FScrollDirection <> Value then
  begin
    if Value = 0 then
      KillTimer(Handle, AutoScrollTimerID)
    else
    if (Value <> 0) and (FScrollDirection = 0) then
      SetTimer(Handle, AutoScrollTimerID, 200, nil);
    FScrollDirection := Value;
  end;
end;

procedure TJvTreeView.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TJvTreeView.WMLButtonDown(var Msg: TWMLButtonDown);
var
  Node: TTreeNode;
begin
  ResetPostOperationFlags;
  with Msg do
    if FMultiSelect and (htOnItem in GetHitTestInfoAt(XPos, YPos)) then
    begin
      Node := GetNodeAt(XPos, YPos);
      if Assigned(Node) and (ssCtrl in KeysToShiftState(Keys)) then
        SelectItem(Node, IsNodeSelected(Node))
      else
      begin
        ClearSelection;
        SelectItem(Node);
      end;
    end;
  inherited;
end;

procedure TJvTreeView.WMNotify(var Msg: TWMNotify);
var
  Node: TTreeNode;
  Point: TPoint;
  I, J: Integer;
begin
  inherited;

  Point := Mouse.CursorPos;
  Point := ScreenToClient(point);
  with Msg, Point do
    case NMHdr^.code of
      NM_CLICK, NM_RCLICK:
        begin
          Node := GetNodeAt(x, y);
          if Assigned(Node) then
            Selected := Node
          else
          begin
            if FCheckBoxes then
            begin
              Node := GetNodeAt(x + 16, y);
              if Assigned(Node) then
                Selected := Node
            end;
          end;
          if (Selected <> nil) and (NMHdr^.code = NM_RCLICK) then
            TJvTreeNode(Selected).PopupMenu.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
        end;
      TVN_SELCHANGEDA, TVN_SELCHANGEDW:
        begin
          if Assigned(FPageControl) then
            if Selected <> nil then
            begin
              //Search for the correct page
              J := -1;
              for I := 0 to FPageControl.PageCount - 1 do
                if FPageControl.Pages[I].Caption = Selected.Text then
                  J := I;
              if J <> -1 then
              begin
                FPageControl.ActivePage := FPageControl.Pages[J];
                if Assigned(FOnPage) then
                  FOnPage(Self, Selected, FPageControl.Pages[J]);
              end;
            end;
        end;

    end;
end;

procedure TJvTreeView.WMTimer(var Msg: TWMTimer);
var
  DragImages: TDragImageList;
begin
  if Msg.TimerID = AutoScrollTimerID then
  begin
    DragImages := GetDragImages;
    if Assigned(DragImages) then
      DragImages.HideDragImage;
    case FScrollDirection of
      -1: SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
      1: SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
    end;
    if Assigned(DragImages) then
      DragImages.ShowDragImage;
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TJvTreeView.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

// === TJvIpAddressValues ====================================================

procedure TJvIpAddressValues.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvIpAddressValues.Changing(Index: Integer; Value: Byte): Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Index, Value, Result);
end;

function TJvIpAddressValues.GetValue: Cardinal;
begin
  Result := MAKEIPADDRESS(FValues[0], FValues[1], FValues[2], FValues[3]);
end;

function TJvIpAddressValues.GetValues(Index: Integer): Byte;
begin
  Result := FValues[Index];
end;

procedure TJvIpAddressValues.SetValue(const AValue: Cardinal);
var
  FChange: Boolean;
begin
  FChange := False;
  if GetValue <> AValue then
  begin
    if Changing(0, FIRST_IPADDRESS(AValue)) then
    begin
      FValues[0] := FIRST_IPADDRESS(AValue);
      FChange := True;
    end;
    if Changing(1, SECOND_IPADDRESS(AValue)) then
    begin
      FValues[1] := SECOND_IPADDRESS(AValue);
      FChange := True;
    end;
    if Changing(2, THIRD_IPADDRESS(AValue)) then
    begin
      FValues[2] := THIRD_IPADDRESS(AValue);
      FChange := True;
    end;
    if Changing(3, FOURTH_IPADDRESS(AValue)) then
    begin
      FValues[3] := FOURTH_IPADDRESS(AValue);
      FChange := True;
    end;
    if FChange then
      Change;
  end;
end;

procedure TJvIpAddressValues.SetValues(Index: Integer; Value: Byte);
begin
  if (Index >= Low(FValues)) and (Index <= High(FValues)) and (FValues[Index] <> Value) then
  begin
    FValues[Index] := Value;
    Change;
  end;
end;

function TJvTreeView.GetItemHeight: integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle,TVM_GETITEMHEIGHT,0,0)
  else
    Result := 16;
end;

procedure TJvTreeView.SetItemHeight(Value: integer);
begin
  if Value <= 0 then
    Value := 16;
  if HandleAllocated then
    SendMessage(Handle,TVM_SETITEMHEIGHT,Value,0);
end;

function TJvTreeView.GetInsertMarkColor: TColor;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TVM_GETINSERTMARKCOLOR, 0, 0)
  else
    Result := clDefault;
end;

procedure TJvTreeView.SetInsertMarkColor(Value: TColor);
begin
  if HandleAllocated then
  begin
    if Value = clDefault then
      Value := Font.Color;
    SendMessage(Handle, TVM_SETINSERTMARKCOLOR, 0, ColorToRGB(Value));
  end;
end;

procedure TJvTreeView.InsertMark(Node: TTreeNode; MarkAfter: boolean);
begin
  if HandleAllocated then
  begin
    if Node = nil then
      RemoveMark
    else
      SendMessage(Handle, TVM_SETINSERTMARK, integer(MarkAfter),integer(Node.ItemId));
  end;
end;

procedure TJvTreeView.RemoveMark;
begin
  if HandleAllocated then
    SendMessage(Handle, TVM_SETINSERTMARK, 0, 0);
end;

function TJvTreeView.GetLineColor: TColor;
begin
  if HandleAllocated then
    Result := SendMessage(Handle,TVM_GETLINECOLOR,0,0)
  else
    Result := clDefault;
end;

procedure TJvTreeView.SetLineColor(Value: TColor);
begin
  if HandleAllocated then
  begin
    if Value = clDefault then
      Value := Font.Color;
    SendMessage(Handle,TVM_SETLINECOLOR,0,ColorToRGB(Value));
  end;
end;

function TJvTreeView.GetMaxScrollTime: integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TVM_GETSCROLLTIME, 0, 0)
  else
    Result := -1;
end;

procedure TJvTreeView.SetMaxScrollTime(const Value: integer);
begin
  if HandleAllocated then
    SendMessage(Handle, TVM_SETSCROLLTIME, Value, 0);
end;

function TJvTreeView.GetUseUnicode: boolean;
begin
  if HandleAllocated then
    Result := boolean(SendMessage(Handle, TVM_GETUNICODEFORMAT,0,0))
  else
    Result := false;
end;

procedure TJvTreeView.SetUseUnicode(const Value: boolean);
begin
  // only try to change value if not running on NT platform
  // (see MSDN: CCM_SETUNICODEFORMAT explanation for details)
  if HandleAllocated and (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    SendMessage(Handle, TVM_SETUNICODEFORMAT,integer(Value),0);
end;

end.

