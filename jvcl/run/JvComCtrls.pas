{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComCtrls.PAS, released Oct 10, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s):
Peter Below [100113 dott 1101 att compuserve dott com] - alternate TJvPageControl.OwnerDraw routine
Peter Thörnqvist [peter3 at sourceforge dot net] added TJvIPAddress.AddressValues and TJvPageControl.ReduceMemoryUse
Alfi [alioscia_alessi att onde dott net] alternate TJvPageControl.OwnerDraw routine
Rudy Velthuis - ShowRange in TJvTrackBar
Andreas Hausladen - TJvIPAddress designtime bug, components changed to JvExVCL
Kai Gossens - TJvIPAddress: changing Color, drawing bug on XP (fat frame on edits removed)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  TJvTreeView:
    When dragging an item and MultiSelect is True droptarget node is not painted
    correctly.
-----------------------------------------------------------------------------}
// $Id$

unit JvComCtrls;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Contnrs, Graphics, Controls, Forms,
  Classes, // (ahuser) "Classes" after "Forms" (D5 warning)
  Menus, ComCtrls, ImgList, Buttons,
  {$IFDEF VCL}
  CommCtrl,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QExtCtrls,
  {$ENDIF VisualCLX}
  JvJVCLUtils, JvComponent, JvExControls, JvExComCtrls;

const
  JvDefPageControlBorder = 4;
  {$IFDEF VCL}

  TVM_SETLINECOLOR = TV_FIRST + 40;
  {$EXTERNALSYM TVM_SETLINECOLOR}

  TVM_GETLINECOLOR = TV_FIRST + 41;
  {$EXTERNALSYM TVM_GETLINECOLOR}
  {$ENDIF VCL}
  JvDefaultInactiveColorFrom = TColor($D7D7D7);
  JvDefaultInactiveColorTo= TColor($ADADAD);

type
  {$IFDEF VCL}
  TJvIPAddress = class;

  TJvIPAddressMinMax = record
    Min: Byte;
    Max: Byte;
  end;

  TJvIPEditControlHelper = class(TObject)
  private
    FHandle: THandle;
    FInstance: Pointer;
    FIPAddress: TJvIPAddress;
    FOrgWndProc: Pointer;
    procedure SetHandle(const Value: THandle);
  protected
    procedure WndProc(var Msg: TMessage); virtual;
    property Handle: THandle read FHandle write SetHandle;
  public
    constructor Create(AIPAddress: TJvIPAddress);
    destructor Destroy; override;

    procedure SetFocus;
    function Focused: Boolean;
    procedure DefaultHandler(var Msg); override;
  end;

  TJvIPAddressRange = class(TPersistent)
  private
    FControl: TWinControl;
    FRange: array [0..3] of TJvIPAddressMinMax;
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

  TJvIpAddrFieldChangeEvent = procedure(Sender: TJvIPAddress; FieldIndex: Integer;
    FieldRange: TJvIPAddressMinMax; var Value: Integer) of object;
  TJvIPAddressChanging = procedure(Sender: TObject; Index: Integer; Value: Byte; var AllowChange: Boolean) of object;

  TJvIPAddressValues = class(TPersistent)
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

  TJvIPAddress = class(TJvCustomControl)
  private
    FEditControls: array [0..3] of TJvIPEditControlHelper;
    FEditControlCount: Integer;
    FAddress: LongWord;
    FChanging: Boolean;
    FRange: TJvIPAddressRange;
    FAddressValues: TJvIPAddressValues;
    FSaveBlank: Boolean;
    FTabThroughFields: Boolean;
    FLocalFont: HFONT;
    FOnFieldChange: TJvIpAddrFieldChangeEvent;
    FOnChange: TNotifyEvent;
    procedure ClearEditControls;
    procedure DestroyLocalFont;
    procedure SetAddress(const Value: LongWord);
    procedure SetAddressValues(const Value: TJvIPAddressValues);
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Msg: TWMNCDestroy); message WM_DESTROY;
    procedure WMParentNotify(var Msg: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMSetFont(var Msg: TWMSetFont); message WM_SETFONT;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMCtlColorEdit(var Msg: TWMCtlColorEdit); message WM_CTLCOLOREDIT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKeyUp); message WM_KEYUP;
    //procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SelectTabControl(Previous: Boolean);
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure EnabledChanged; override;
    procedure ColorChanged; override;
    procedure FontChanged; override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure AdjustHeight;
    procedure AdjustSize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoChange; dynamic;
    procedure Paint; override;

    procedure DoAddressChange(Sender: TObject); virtual;
    procedure DoAddressChanging(Sender: TObject; Index: Integer;
      Value: Byte; var AllowChange: Boolean); virtual;
    procedure DoFieldChange(FieldIndex: Integer; var FieldValue: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAddress;
    function IsBlank: Boolean;
  published
    property Address: LongWord read FAddress write SetAddress default 0;
    property AddressValues: TJvIPAddressValues read FAddressValues write SetAddressValues;
    property Anchors;
    property Color;
    property Constraints;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnStartDock;
    property OnEndDock;
    {$ENDIF VCL}
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Range: TJvIPAddressRange read FRange write FRange;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property TabThroughFields: Boolean read FTabThroughFields write FTabThroughFields default True;
    property Text;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldChange: TJvIpAddrFieldChangeEvent read FOnFieldChange write FOnFieldChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;
  {$ENDIF VCL}

  // TJvHintSource is a hint enumeration type to describe how to display hints for
  // controls that have hint properties both for the main control as well as
  // for it's subitems (like a PageControl)
  // TODO: (p3) this should really be moved to JvTypes or something...
  TJvHintSource =
    (
    hsDefault, // use default hint behaviour (i.e as regular control)
    hsForceMain, // use the main hint even if subitems have hints
    hsForceChildren, // always use subitems hints even if empty
    hsPreferMain, // use main control hint unless empty then use subitems hints
    hsPreferChildren // use subitems hints unless empty then use main control hint
    );

  // painters that can be used to draw the tabs of a TPageControl or TTabControl
  TJvTabControlPainter = class(TJvComponent)
  private
    FClients: TList;
  protected
    // descendants must override and implement this method
    procedure DrawTab(AControl: TCustomTabControl; Canvas: TCanvas;
      Images: TCustomImageList; ImageIndex: Integer; const Caption: string;
      const Rect: TRect; Active, Enabled: Boolean); virtual; abstract;
    procedure Change; virtual;

    procedure RegisterChange(AControl: TCustomTabControl);
    procedure UnRegisterChange(AControl: TCustomTabControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    destructor Destroy; override;
  end;

  TJvTabDefaultPainter = class(TJvTabControlPainter)
  private
    FActiveFont: TFont;
    FDisabledFont: TFont;
    FInactiveFont: TFont;
    FInactiveColorTo: TColor;
    FActiveColorTo: TColor;
    FDisabledColorTo: TColor;
    FInactiveColorFrom: TColor;
    FActiveColorFrom: TColor;
    FDisabledColorFrom: TColor;
    FActiveGradientDirection: TFillDirection;
    FInactiveGradientDirection: TFillDirection;
    FDisabledGradientDirection: TFillDirection;
    FGlyphLayout: TButtonLayout;
    FDivider: Boolean;
    FShowFocus: Boolean;
    procedure SetActiveFont(const Value: TFont);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetActiveColorFrom(const Value: TColor);
    procedure SetActiveColorTo(const Value: TColor);
    procedure SetActiveGradientDirection(const Value: TFillDirection);
    procedure SetDisabledColorFrom(const Value: TColor);
    procedure SetDisabledColorTo(const Value: TColor);
    procedure SetDisabledGradientDirection(const Value: TFillDirection);
    procedure SetInactiveColorFrom(const Value: TColor);
    procedure SetInactiveColorTo(const Value: TColor);
    procedure SetInactiveGradientDirection(const Value: TFillDirection);
    function IsActiveFontStored: Boolean;
    function IsInactiveFontStored: Boolean;
    function IsDisabledFontStored: Boolean;
    procedure SetGlyphLayout(const Value: TButtonLayout);
    procedure SetDivider(const Value: Boolean);
    procedure SetShowFocus(const Value: Boolean);
  protected
    procedure DrawTab(AControl: TCustomTabControl; Canvas: TCanvas;
      Images: TCustomImageList; ImageIndex: Integer; const Caption: string;
      const Rect: TRect; Active, Enabled: Boolean); override;
    procedure DoFontChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveFont: TFont read FActiveFont write SetActiveFont stored IsActiveFontStored;
    property ActiveColorFrom: TColor read FActiveColorFrom write SetActiveColorFrom default clWhite;
    property ActiveColorTo: TColor read FActiveColorTo write SetActiveColorTo default clBtnFace;
    property ActiveGradientDirection: TFillDirection read FActiveGradientDirection write SetActiveGradientDirection default fdTopToBottom;
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont stored IsInactiveFontStored;
    property InactiveColorFrom: TColor read FInactiveColorFrom write SetInactiveColorFrom default JvDefaultInactiveColorFrom;
    property InactiveColorTo: TColor read FInactiveColorTo write SetInactiveColorTo default JvDefaultInactiveColorTo;
    property InactiveGradientDirection: TFillDirection read FInactiveGradientDirection write SetInactiveGradientDirection default fdTopToBottom;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont stored IsDisabledFontStored;
    property DisabledColorFrom: TColor read FDisabledColorFrom write SetDisabledColorFrom default clBtnFace;
    property DisabledColorTo: TColor read FDisabledColorTo write SetDisabledColorTo default clBtnFace;
    property DisabledGradientDirection: TFillDirection read FDisabledGradientDirection write SetDisabledGradientDirection default fdTopToBottom;
    property GlyphLayout: TButtonLayout read FGlyphLayout write SetGlyphLayout default blGlyphLeft;
    property Divider: Boolean read FDivider write SetDivider default False;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
  end;

  TJvTabControl = class(TJvExTabControl)
  private
    FTabPainter: TJvTabControlPainter;
    FRightClickSelect: Boolean;
    {$IFDEF VCL}
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    {$ENDIF VCL}
    procedure SetTabPainter(const Value: TJvTabControlPainter); // not WantKeys
  protected
    {$IFDEF VisualCLX}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean; override;
    {$ENDIF VisualCLX}
    {$IFDEF VCL}
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    {$ENDIF VCL}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property TabPainter: TJvTabControlPainter read FTabPainter write SetTabPainter;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
  end;

  TJvPageControl = class(TJvExPageControl)
  private
    FClientBorderWidth: TBorderWidth;
    FHideAllTabs: Boolean;
    FHandleGlobalTab: Boolean;
    FHintSource: TJvHintSource;
    FReduceMemoryUse: Boolean;
    FTabPainter: TJvTabControlPainter;
    FRightClickSelect: Boolean;
    procedure SetClientBorderWidth(const Value: TBorderWidth);
    {$IFDEF VCL}
    procedure TCMAdjustRect(var Msg: TMessage); message TCM_ADJUSTRECT;
    {$ENDIF VCL}
    procedure SetHideAllTabs(const Value: Boolean);
    function FormKeyPreview: Boolean;
    procedure SetReduceMemoryUse(const Value: Boolean);
    procedure SetTabPainter(const Value: TJvTabControlPainter);
  protected
    function HintShow(var HintInfo: THintInfo): Boolean; override;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;

    procedure Loaded; override;
    function CanChange: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF VisualCLX}
    function DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$ENDIF VisualCLX}
    {$IFDEF VCL}
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTabImages;
  published
    property TabPainter: TJvTabControlPainter read FTabPainter write SetTabPainter;
    property HintSource: TJvHintSource read FHintSource write FHintSource default hsDefault;
    property HandleGlobalTab: Boolean read FHandleGlobalTab write FHandleGlobalTab default False;
    property ClientBorderWidth: TBorderWidth read FClientBorderWidth write SetClientBorderWidth default JvDefPageControlBorder;
    property ParentColor;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property ReduceMemoryUse: Boolean read FReduceMemoryUse write SetReduceMemoryUse default False;
    property HideAllTabs: Boolean read FHideAllTabs write SetHideAllTabs default False;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
  end;

  TJvTrackToolTipSide = (tsLeft, tsTop, tsRight, tsBottom);
  TJvTrackToolTipEvent = procedure(Sender: TObject; var ToolTipText: string) of object;

  TJvTrackBar = class(TJvExTrackBar)
  private
    FOnChanged: TNotifyEvent;
    FShowRange: Boolean;
    {$IFDEF VCL}
    FToolTips: Boolean;
    FToolTipSide: TJvTrackToolTipSide;
    FToolTipText: WideString;
    FOnToolTip: TJvTrackToolTipEvent;
    procedure SetToolTips(const Value: Boolean);
    procedure SetToolTipSide(const Value: TJvTrackToolTipSide);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure CNHScroll(var Msg: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Msg: TWMVScroll); message CN_VSCROLL;
    {$ENDIF VCL}
    procedure SetShowRange(const Value: Boolean);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure InternalSetToolTipSide;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShowRange: Boolean read FShowRange write SetShowRange default True;
    {$IFDEF VCL}
    property ToolTips: Boolean read FToolTips write SetToolTips default False;
    property ToolTipSide: TJvTrackToolTipSide read FToolTipSide write SetToolTipSide default tsLeft;
    {$ENDIF VCL}
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF VCL}
    property OnToolTip: TJvTrackToolTipEvent read FOnToolTip write FOnToolTip;
    {$ENDIF VCL}
  end;

  {$IFDEF VCL}

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
  protected
    procedure Reinitialize; virtual;
  public
    class function CreateEnh(AOwner: TTreeNodes): TJvTreeNode;
    procedure Assign(Source: TPersistent); override;
    property Checked: Boolean read GetChecked write SetChecked;
    property Bold: Boolean read GetBold write SetBold;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  TPageChangedEvent = procedure(Sender: TObject; Item: TTreeNode; Page: TTabSheet) of object;
  TJvTreeViewComparePageEvent = procedure(Sender: TObject; Page: TTabSheet;
    Node: TTreeNode; var Matches: Boolean) of object;

  TJvTreeView = class(TJvExTreeView)
  private
    FAutoDragScroll: Boolean;
    FClearBeforeSelect: Boolean;
    {$IFDEF COMPILER5}
    FMultiSelect: Boolean;
    {$ENDIF COMPILER5}
    FScrollDirection: Integer;
    FSelectedList: TObjectList;
    FSelectThisNode: Boolean;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnEditCancelled: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FCheckBoxes: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FPageControl: TPageControl;
    FOnPage: TPageChangedEvent;
    FOnComparePage: TJvTreeViewComparePageEvent;
    FMenu: TMenu;
    FOldMenuChange: TMenuChangeEvent;
    FMenuDblClick: Boolean;
    FReinitializeTreeNode: Boolean;
    procedure InternalCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    function GetSelectedCount: Integer;
    function GetSelectedItem(Index: Integer): TTreeNode;
    {$IFDEF COMPILER5}
    procedure SetMultiSelect(const Value: Boolean);
    {$ENDIF COMPILER5}
    procedure SetScrollDirection(const Value: Integer);
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure SetCheckBoxes(const Value: Boolean);
    function GetItemHeight: Integer;
    procedure SetItemHeight(Value: Integer);
    function GetInsertMarkColor: TColor;
    procedure SetInsertMarkColor(Value: TColor);
    function GetLineColor: TColor;
    procedure SetLineColor(Value: TColor);
    function GetMaxScrollTime: Integer;
    procedure SetMaxScrollTime(const Value: Integer);
    function GetUseUnicode: Boolean;
    procedure SetUseUnicode(const Value: Boolean);
    procedure SetMenu(const Value: TMenu);
    procedure DoMenuChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    procedure SetPageControl(const Value: TPageControl);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
  protected
    procedure RebuildFromMenu; virtual;
    function IsMenuItemClick(Node: TTreeNode): Boolean;
    function DoComparePage(Page: TTabSheet; Node: TTreeNode): Boolean; virtual;
    function CreateNode: TTreeNode; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
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
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DblClick; override;
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
    function GetChecked(Node: TTreeNode): Boolean;
    procedure SetChecked(Node: TTreeNode; Value: Boolean);
    procedure SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
    function GetNodePopup(Node: TTreeNode): TPopupMenu;
    procedure InsertMark(Node: TTreeNode; MarkAfter: Boolean); // TVM_SETINSERTMARK
    procedure RemoveMark;
    property InsertMarkColor: TColor read GetInsertMarkColor write SetInsertMarkColor;
    property Checked[Node: TTreeNode]: Boolean read GetChecked write SetChecked;
    property MaxScrollTime: Integer read GetMaxScrollTime write SetMaxScrollTime;
    // UseUnicode should only be changed on Win95 and Win98 that has IE5 or later installed
    property UseUnicode: Boolean read GetUseUnicode write SetUseUnicode default False;
  published
    property LineColor: TColor read GetLineColor write SetLineColor default clDefault;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight default 16;
    property Menu: TMenu read FMenu write SetMenu;
    property MenuDblClick: Boolean read FMenuDblClick write FMenuDblClick default False;
    property HintColor;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored False;
    property Checkboxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property AutoDragScroll: Boolean read FAutoDragScroll write FAutoDragScroll default False;
    {$IFDEF COMPILER5}
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    {$ENDIF COMPILER5}
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    property OnPageChanged: TPageChangedEvent read FOnPage write FOnPage;
    property OnComparePage: TJvTreeViewComparePageEvent read FOnComparePage write FOnComparePage;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnEditCancelled: TNotifyEvent read FOnEditCancelled write FOnEditCancelled;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  {$ENDIF VCL}

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclStrings,
  JvConsts, JvJCLUtils;

{$IFDEF VCL}

const
  TVIS_CHECKED = $2000;

//=== { TJvIPAddressRange } ==================================================

constructor TJvIPAddressRange.Create(Control: TWinControl);
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

procedure TJvIPAddressRange.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvIPAddressRange then
    with TJvIPAddressRange(Dest) do
    begin
      FRange := Self.FRange;
      Change(-1);
    end
  else
    inherited AssignTo(Dest);
end;

procedure TJvIPAddressRange.Change(Index: Integer);
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

function TJvIPAddressRange.GetMaxRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Max;
end;

function TJvIPAddressRange.GetMinRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Min;
end;

procedure TJvIPAddressRange.SetMaxRange(const Index: Integer; const Value: Byte);
begin
  FRange[Index].Max := Value;
  Change(Index);
end;

procedure TJvIPAddressRange.SetMinRange(const Index: Integer; const Value: Byte);
begin
  FRange[Index].Min := Value;
  Change(Index);
end;

//=== { TJvIPEditControlHelper } =============================================

constructor TJvIPEditControlHelper.Create(AIPAddress: TJvIPAddress);
begin
  inherited Create;
  FHandle := 0;
  FIPAddress := AIPAddress;
  FInstance := MakeObjectInstance(WndProc);
end;

destructor TJvIPEditControlHelper.Destroy;
begin
  Handle := 0;
  if Assigned(FInstance) then
    FreeObjectInstance(FInstance);
  inherited Destroy;
end;

procedure TJvIPEditControlHelper.SetFocus;
begin
  if FHandle <> 0 then
  begin
    Windows.SetFocus(FHandle);
    SendMessage(FHandle, EM_SETSEL, 0, MaxInt); 
  end;
end;

function TJvIPEditControlHelper.Focused: Boolean;
begin
  if FHandle <> 0 then
    Result := Windows.GetFocus = FHandle
  else
    Result := False;
end;

procedure TJvIPEditControlHelper.DefaultHandler(var Msg);
begin
  with TMessage(Msg) do
    Result := CallWindowProc(FOrgWndProc, FHandle, Msg, WParam, LParam);
end;

procedure TJvIPEditControlHelper.SetHandle(const Value: THandle);
begin
  if Value <> FHandle then
  begin
    if FHandle <> 0 then
      SetWindowLong(FHandle, GWL_WNDPROC, Integer(FOrgWndProc));

    FHandle := Value;

    if FHandle <> 0 then
    begin
      FOrgWndProc := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
      SetWindowLong(FHandle, GWL_WNDPROC, Integer(FInstance));
    end;
  end;
end;

procedure TJvIPEditControlHelper.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_ENABLE:
      if csDesigning in FIPAddress.ComponentState then
        Exit;
    WM_DESTROY:
      Handle := 0;
    WM_KEYFIRST..WM_KEYLAST:
      begin
        FIPAddress.Dispatch(Msg);
        if Msg.WParam = VK_TAB then
          Exit;
      end;
    // mouse messages are sent through TJvIPAddress.WMParentNotify
  end;
  Dispatch(Msg);
end;

//=== { TJvIPAddress } =======================================================

constructor TJvIPAddress.Create(AOwner: TComponent);
var
  I: Integer;
begin
  CheckCommonControl(ICC_INTERNET_CLASSES);
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFixedHeight, csReflector];

  FRange := TJvIPAddressRange.Create(Self);
  FAddressValues := TJvIPAddressValues.Create;
  FAddressValues.OnChange := DoAddressChange;
  FAddressValues.OnChanging := DoAddressChanging;
  FTabThroughFields := True;

  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := 150;
  AdjustHeight;

  for I := 0 to High(FEditControls) do
    FEditControls[I] := TJvIPEditControlHelper.Create(Self);
end;

destructor TJvIPAddress.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FRange);
  FreeAndNil(FAddressValues);
  inherited Destroy;
  // (ahuser) I don't know why but TWinControl.DestroyWindowHandle raises an AV
  //          when FEditControls are released before inherited Destroy.
  for I := 0 to High(FEditControls) do
    FEditControls[I].Free;
end;

procedure TJvIPAddress.CreateParams(var Params: TCreateParams);
begin
  InitCommonControl(ICC_INTERNET_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_IPADDRESS);
  with Params do
  begin
    Style := Style or WS_CHILD;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TJvIPAddress.CreateWnd;
var
  EditHandle: HWND;
  Msg: TWMParentNotify;
begin
  ClearEditControls;
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
    if (FEditControlCount = 0) and (csDesigning in ComponentState) then
    begin
      // WM_PARENTNOTIFY messages are captured by the IDE starting when
      // CreateWnd is called the second time. So we must find the edit controls
      // ourself and simulate a WM_PARENTNOTIFY by a direct function call.
      EditHandle := 0;
      repeat
        EditHandle := FindWindowEx(Handle, EditHandle, 'EDIT', nil);
        if EditHandle <> 0 then
        begin
          Msg.Msg := WM_PARENTNOTIFY;
          Msg.Event := WM_CREATE;
          Msg.ChildID := GetDlgCtrlID(EditHandle);
          Msg.ChildWnd := EditHandle;
          WMParentNotify(Msg); // IDE captures WM_PARENTNOTIFY
        end;
      until EditHandle = 0;
    end;
  finally
    FChanging := False;
  end;
end;

procedure TJvIPAddress.DestroyLocalFont;
begin
  if FLocalFont <> 0 then
  begin
    OSCheck(DeleteObject(FLocalFont));
    FLocalFont := 0;
  end;
end;

procedure TJvIPAddress.DestroyWnd;
begin
  FSaveBlank := IsBlank;
  inherited DestroyWnd;
end;

type
  TWinControlAccess = class(TWinControl);

procedure TJvIPAddress.SelectTabControl(Previous: Boolean);
var
  Control: TWinControl;
begin
  Control := TWinControlAccess(Parent).FindNextControl(Self, not Previous, True, True);
  if Control <> nil then
    Control.SetFocus;
end;

procedure TJvIPAddress.WMKeyDown(var Msg: TWMKeyDown);
var
  I, FocusIndex: Integer;
begin
  if Msg.CharCode = VK_TAB then
  begin
    FocusIndex := -1;
    for I := 0 to FEditControlCount - 1 do
    begin
      if FEditControls[I].Focused then
      begin
        FocusIndex := I;
        Break;
      end;
    end;

    if GetKeyState(VK_SHIFT) < 0 then
      Dec(FocusIndex)
    else
      Inc(FocusIndex);

    if FocusIndex >= 0 then
    begin
      if FocusIndex < FEditControlCount then
        FEditControls[FocusIndex].SetFocus
      else
        SelectTabControl(False);
    end
    else
    if FocusIndex = -1 then
      SelectTabControl(True);
  end
  else
    inherited;
end;

procedure TJvIPAddress.WMKeyUp(var Msg: TWMKeyUp);
begin
  if Msg.CharCode = VK_TAB then
    Msg.Result := 0
  else
    inherited;
end;

function TJvIPAddress.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Result := True;
end;

procedure TJvIPAddress.Paint;
var
  I: Integer;
  R1, R2: TRect;
  X, Y: Integer;
  Pt: TPoint;
begin
  { We paint the '.' ourself so we can also paint the control's background in
    DoPaintBackground what would be impossible without self-painting because
    the IP-Control always paints a clWindow background in WM_PAINT. } 
  for I := 0 to (FEditControlCount - 1) - 1 do
  begin
    GetWindowRect(FEditControls[I].Handle, R1);
    GetWindowRect(FEditControls[I + 1].Handle, R2);
    X := R1.Right + (R2.Left - R1.Right) div 2;
    Y := R1.Top;
    Pt := ScreenToClient(Point(X, Y));
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
    Canvas.TextOut(Pt.X, Pt.Y, '.');
  end;
end;

procedure TJvIPAddress.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
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
      GetWindowRect(FEditControls[I].Handle, R);
      R.TopLeft := ScreenToClient(R.TopLeft);
      R.BottomRight := ScreenToClient(R.BottomRight);
      OffsetRect(R, -R.Left, -R.Top);
      R.Bottom := ClientHeight;
      SetWindowPos(FEditControls[I].Handle, 0, 0, 0, R.Right, R.Bottom,
        SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
    end;}
end;

procedure TJvIPAddress.AdjustSize;
begin
  inherited AdjustSize;
  RecreateWnd;
end;

procedure TJvIPAddress.ClearAddress;
begin
  if HandleAllocated then
    Perform(IPM_CLEARADDRESS, 0, 0);
  FAddressValues.Address := 0;
end;

procedure TJvIPAddress.ClearEditControls;
var
  I: Integer;
begin
  for I := 0 to High(FEditControls) do
    if FEditControls[I] <> nil then
      FEditControls[I].Handle := 0;
  FEditControlCount := 0;
end;

procedure TJvIPAddress.ColorChanged;
begin
  inherited ColorChanged;
  Invalidate;
end;

procedure TJvIPAddress.FontChanged;
begin
  inherited FontChanged;
  AdjustHeight;
  Invalidate;
end;

procedure TJvIPAddress.EnabledChanged;
var
  I: Integer;
begin
  inherited EnabledChanged;
  for I := 0 to High(FEditControls) do
    if (FEditControls[I] <> nil) and (FEditControls[I].Handle <> 0) then
      EnableWindow(FEditControls[I].Handle, Enabled and not (csDesigning in ComponentState));
end;

procedure TJvIPAddress.CNCommand(var Msg: TWMCommand);
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

procedure TJvIPAddress.CNNotify(var Msg: TWMNotify);
begin
  with Msg, NMHdr^ do
    if code = IPN_FIELDCHANGED then
      with PNMIPAddress(NMHdr)^ do
        DoFieldChange(iField, iValue);
  inherited;
end;

procedure TJvIPAddress.DoAddressChange(Sender: TObject);
begin
  Address := FAddressValues.Address;
end;

procedure TJvIPAddress.DoAddressChanging(Sender: TObject; Index: Integer; Value: Byte; var AllowChange: Boolean);
begin
  AllowChange := (Index > -1) and (Index < 4) and
    (Value >= FRange.FRange[Index].Min) and (Value <= FRange.FRange[Index].Max);
end;

procedure TJvIPAddress.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvIPAddress.DoFieldChange(FieldIndex: Integer; var FieldValue: Integer);
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(Self, FieldIndex, FRange.FRange[FieldIndex], FieldValue);
end;

function TJvIPAddress.IsBlank: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, IPM_ISBLANK, 0, 0) <> 0;
end;

procedure TJvIPAddress.SetAddress(const Value: LongWord);
begin
  if FAddress <> Value then
  begin
    FAddress := Value;
    if HandleAllocated then
      Perform(IPM_SETADDRESS, 0, FAddress);
    FAddressValues.Address := Value;
  end;
end;

procedure TJvIPAddress.SetAddressValues(const Value: TJvIPAddressValues);
begin
  //  (p3) do nothing
end;

{ Added 03/05/2004 by Kai Gossens }

procedure TJvIPAddress.WMCtlColorEdit(var Msg: TWMCtlColorEdit);
var
  DC: HDC;
begin
  inherited;
  DC := GetDC(Handle);
  try
    Brush.Color := ColorToRGB(Color);
    Brush.Style := bsSolid;
    SetTextColor(DC, ColorToRGB(Font.Color));
    SetBkColor(DC, ColorToRGB(Brush.Color));
    SetTextColor(Msg.ChildDC, ColorToRGB(Font.Color));
    SetBkColor(Msg.ChildDC, ColorToRGB(Brush.Color));
    SetBkMode(Msg.ChildDC, TRANSPARENT);
  finally
    ReleaseDC(Handle, DC);
  end;
  Msg.Result := Brush.Handle;
end;

procedure TJvIPAddress.WMDestroy(var Msg: TWMNCDestroy);
begin
  DestroyLocalFont;
  inherited;
end;

procedure TJvIPAddress.DoGetDlgCode(var Code: TDlgCodes);
begin
  Include(Code, dcWantArrows);
  if FTabThroughFields then
    Include(Code, dcWantTab);
  Exclude(Code, dcNative); // prevent inherited call
end;

procedure TJvIPAddress.WMSetText(var Msg: TWMSetText);
var
  S: string;
begin
  // really long values for the text crashes the program (try: 127.0.0.8787787878787878), so we limit it here before it is set
  S := Msg.Text;
  with AddressValues do
  begin
    Value1 := StrToIntDef(StrToken(S, '.'), 0);
    Value2 := StrToIntDef(StrToken(S, '.'), 0);
    Value3 := StrToIntDef(StrToken(S, '.'), 0);
    Value4 := StrToIntDef(S, 0);
    Msg.Text := PChar(Format('%d.%d.%d.%d', [Value1, Value2, Value3, Value4]));
  end;
  inherited;
end;

procedure TJvIPAddress.WMParentNotify(var Msg: TWMParentNotify);
begin
  with Msg do
    case Event of
      WM_CREATE:
        if (FEditControlCount <= Length(FEditControls)) and
          (FEditControls[FEditControlCount] <> nil) then
        begin
          FEditControls[FEditControlCount].Handle := ChildWnd;
          EnableWindow(ChildWnd, Enabled and not (csDesigning in ComponentState));
          Inc(FEditControlCount);
        end;
      WM_DESTROY:
        ClearEditControls;
      // (p3) this code prevents the user from dblclicking on any edit field
      // to select it (the first edit is always selected). I don't know if removing
      // it has any side-effects but I haven't noticed anything
//      WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN:
//        Perform(Event, Value, Integer(SmallPoint(XPos, YPos)));
    end;
  inherited;
end;

procedure TJvIPAddress.WMSetFont(var Msg: TWMSetFont);
var
  LF: TLogFont;
begin
  FillChar(LF, SizeOf(TLogFont), #0);
  try
    OSCheck(GetObject(Font.Handle, SizeOf(LF), @LF) > 0);
    DestroyLocalFont;
    FLocalFont := CreateFontIndirect(LF);
    Msg.Font := FLocalFont;
    inherited;
  except
    Application.HandleException(Self);
  end;
end;

{$ENDIF VCL}

//=== { TJvTabControlPainter } ===============================================

destructor TJvTabControlPainter.Destroy;
begin
  if FClients <> nil then
    while FClients.Count > 0 do
      UnRegisterChange(TCustomTabControl(FClients.Last));
  FreeAndNil(FClients);
  inherited Destroy;
end;

procedure TJvTabControlPainter.Change;
var
  I: Integer;
begin
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      TCustomTabControl(FClients[I]).Invalidate;
end;

procedure TJvTabControlPainter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TCustomTabControl) and (FClients <> nil) then
    FClients.Remove(AComponent);
end;

procedure TJvTabControlPainter.RegisterChange(AControl: TCustomTabControl);
begin
  if FClients = nil then
    FClients := TList.Create;
  if AControl <> nil then
  begin
    FClients.Add(AControl);
    AControl.FreeNotification(Self);
    AControl.Invalidate;
  end;
end;

procedure TJvTabControlPainter.UnRegisterChange(AControl: TCustomTabControl);
begin
  if FClients <> nil then
  begin
    FClients.Remove(AControl);
    if (AControl <> nil) and not (csDestroying in AControl.ComponentState) then
      AControl.Invalidate;
  end;
end;

//=== { TJvTabDefaultPainter } ===============================================

constructor TJvTabDefaultPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveFont := TFont.Create;
  if Owner is TForm then
    FActiveFont.Assign(TForm(Owner).Font)
  else
    {$IFDEF VCL}
    FActiveFont.Assign(Screen.IconFont);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FActiveFont.Assign(Screen.HintFont);
    {$ENDIF VisualCLX}
  FActiveFont.Color := clHighlight;
  FActiveFont.OnChange := DoFontChange;
  FActiveColorFrom := clWhite;
  FActiveColorTo := clBtnFace;
  FActiveGradientDirection := fdTopToBottom;

  FDisabledFont := TFont.Create;
  if Owner is TForm then
    FDisabledFont.Assign(TForm(Owner).Font)
  else
    {$IFDEF VCL}
    FDisabledFont.Assign(Screen.IconFont);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FDisabledFont.Assign(Screen.HintFont);
    {$ENDIF VisualCLX}
  FDisabledFont.Color := clGrayText;
  FDisabledFont.OnChange := DoFontChange;
  FDisabledColorFrom := clBtnFace;
  FDisabledColorTo := clBtnFace;
  FDisabledGradientDirection := fdTopToBottom;

  FInactiveFont := TFont.Create;
  if Owner is TForm then
    FInactiveFont.Assign(TForm(Owner).Font)
  else
    {$IFDEF VCL}
    FInactiveFont.Assign(Screen.IconFont);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FInactiveFont.Assign(Screen.HintFont);
    {$ENDIF VisualCLX}
  FInactiveFont.OnChange := DoFontChange;
  FInactiveColorFrom := JvDefaultInactiveColorFrom;
  FInactiveColorTo := JvDefaultInactiveColorTo;
  FInactiveGradientDirection := fdTopToBottom;
  FGlyphLayout := blGlyphLeft;
end;

destructor TJvTabDefaultPainter.Destroy;
begin
  FActiveFont.Free;
  FDisabledFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvTabDefaultPainter.DoFontChange(Sender: TObject);
begin
  Change;
end;

procedure TJvTabDefaultPainter.DrawTab(AControl: TCustomTabControl;
  Canvas: TCanvas; Images: TCustomImageList; ImageIndex: Integer;
  const Caption: string; const Rect: TRect; Active, Enabled: Boolean);
var
  TextRect, ImageRect: TRect;
  SaveState: Integer;
  procedure DrawDivider(X, Y, X1, Y1: Integer);
  begin
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X1, Y1);
    Canvas.Pen.Color := clHighlightText;
    Canvas.MoveTo(X + 1, Y + 1);
    Canvas.LineTo(X1 + 1, Y1 + 1);
  end;
begin
  TextRect := Rect;
  ImageRect := Rect;
  if not Enabled then
  begin
    GradientFillRect(Canvas, TextRect, DisabledColorFrom, DisabledColorTo, DisabledGradientDirection, 255);
    Canvas.Font := DisabledFont;
  end
  else
  if Active then
  begin
    GradientFillRect(Canvas, TextRect, ActiveColorFrom, ActiveColorTo, ActiveGradientDirection, 255);
    Canvas.Font := ActiveFont;
  end
  else
  begin
    GradientFillRect(Canvas, TextRect, InactiveColorFrom, InactiveColorTo, InactiveGradientDirection, 255);
    Canvas.Font := InactiveFont;
  end;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin // GlyphLayout is only used if we have images
    case GlyphLayout of
      blGlyphLeft:
        begin
          Inc(ImageRect.Left, 4);
          ImageRect.Right := ImageRect.Left + Images.Width + 4;
          TextRect.Left := ImageRect.Right;
        end;
      blGlyphRight:
        begin
          Dec(ImageRect.Right, 4);
          ImageRect.Left := ImageRect.Right - Images.Width - 4;
          TextRect.Right := ImageRect.Left;
        end;
      blGlyphTop:
        begin
          Dec(ImageRect.Bottom, RectHeight(Rect) div 2);
          TextRect.Top := ImageRect.Bottom;
          if Divider and (Caption <> '') then
            DrawDivider(Rect.Left + 4 + Ord(Active), Rect.Top + RectHeight(Rect) div 2, Rect.Right - 4 - Ord(Active), Rect.Top + RectHeight(Rect) div 2);
        end;
      blGlyphBottom:
        begin
          Inc(ImageRect.Top, RectHeight(Rect) div 2);
          TextRect.Bottom := ImageRect.Top;
          if Divider and (Caption <> '') then
            DrawDivider(Rect.Left + 4 + Ord(Active), Rect.Top + RectHeight(Rect) div 2, Rect.Right - 4 - Ord(Active), Rect.Top + RectHeight(Rect) div 2);
        end;
    end;
    InflateRect(ImageRect, -(RectWidth(ImageRect) - Images.Width) div 2, -(RectHeight(ImageRect) - Images.Height) div 2);
    SaveState := SaveDC(Canvas.Handle);
    try
      Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, ImageIndex,
      {$IFDEF VisualCLX}
      itImage,
      {$ENDIF VisualCLX}
      Enabled);
    finally
      RestoreDC(Canvas.Handle, SaveState);
    end;
  end;
  if Caption <> '' then
  begin
//    InflateRect(TextRect, -2, -2);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas, Caption, Length(Caption), TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
  if Active and ShowFocus then
  begin
    TextRect := Rect;
    InflateRect(TextRect, -3, -3);
    Canvas.DrawFocusRect(TextRect);
  end;
end;

procedure TJvTabDefaultPainter.SetActiveColorFrom(const Value: TColor);
begin
  if FActiveColorFrom <> Value then
  begin
    FActiveColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetActiveColorTo(const Value: TColor);
begin
  if FActiveColorTo <> Value then
  begin
    FActiveColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetActiveGradientDirection(
  const Value: TFillDirection);
begin
  if FActiveGradientDirection <> Value then
  begin
    FActiveGradientDirection := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledColorFrom(const Value: TColor);
begin
  if FDisabledColorFrom <> Value then
  begin
    FDisabledColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledColorTo(const Value: TColor);
begin
  if FDisabledColorTo <> Value then
  begin
    FDisabledColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetDisabledGradientDirection(
  const Value: TFillDirection);
begin
  if FDisabledGradientDirection <> Value then
  begin
    FDisabledGradientDirection := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveColorFrom(const Value: TColor);
begin
  if FInactiveColorFrom <> Value then
  begin
    FInactiveColorFrom := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveColorTo(const Value: TColor);
begin
  if FInactiveColorTo <> Value then
  begin
    FInactiveColorTo := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TJvTabDefaultPainter.SetInactiveGradientDirection(const Value: TFillDirection);
begin
  if FInactiveGradientDirection <> Value then
  begin
    FInactiveGradientDirection := Value;
    Change;
  end;
end;

function TJvTabDefaultPainter.IsActiveFontStored: Boolean;
begin
  Result := True;
end;

function TJvTabDefaultPainter.IsDisabledFontStored: Boolean;
begin
  Result := True;
end;

function TJvTabDefaultPainter.IsInactiveFontStored: Boolean;
begin
  Result := True;
end;

//=== { TJvTabControl } ======================================================

constructor TJvTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VisualCLX}
  InputKeys := [ikTabs];
  {$ENDIF VisualCLX}
end;

{$IFDEF VCL}

procedure TJvTabControl.CMDialogKey(var Msg: TWMKey);
begin
  if (Msg.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) and
    IsChild(Handle, Windows.GetFocus) then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
    begin
      if TabIndex = 0 then
        TabIndex := Tabs.Count - 1
      else
        TabIndex := TabIndex - 1;
    end
    else
      TabIndex := (TabIndex + 1) mod Tabs.Count;
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TJvTabControl.WMRButtonDown(var Msg: TWMRButtonDown);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect then
  begin
    with Msg do
    P := SmallPointToPoint(SmallPoint(XPos,YPos));
    for I := 0 to Tabs.Count -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (TabIndex <> I) and CanChange then
        begin
          TabIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}

procedure TJvTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) and (Shift * KeyboardShiftStates >= [ssCtrl]) then
  begin
    if (Shift * KeyboardShiftStates >= [ssShift]) then
    begin
      if TabIndex = 0 then
        TabIndex := Tabs.Count - 1
      else
        TabIndex := TabIndex - 1;
    end
    else
      TabIndex := (TabIndex + 1) mod Tabs.Count;
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TJvTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect and (Button = mbRight) then
  begin
    P := Point(X,Y);
    for I := 0 to Tabs.Count -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (TabIndex <> I) and CanChange then
        begin
          TabIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TJvTabControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean): Boolean;
begin
  Result := True;
  if Assigned(TabPainter) then
    TabPainter.DrawTab(Self, Canvas, Images, TabIndex, Tabs[TabIndex].Caption, Rect, TabIndex = Self.TabIndex, Enabled)
  else
    Result := inherited DrawTab(TabIndex, Rect, Active);
end;

{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvTabControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
  if Assigned(TabPainter) then
    TabPainter.DrawTab(Self, Canvas, Images, TabIndex, Tabs[TabIndex], Rect, TabIndex = Self.TabIndex, Enabled)
  else
    inherited DrawTab(TabIndex, Rect, Active);
end;
{$ENDIF VCL}

procedure TJvTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = TabPainter) then
    TabPainter := nil;

end;

procedure TJvTabControl.SetTabPainter(const Value: TJvTabControlPainter);
begin
  if FTabPainter <> Value then
  begin
    if FTabPainter <> nil then
      FTabPainter.UnRegisterChange(Self);
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
    end;
    Invalidate;
  end;
end;

//=== { TJvPageControl } =====================================================

constructor TJvPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientBorderWidth := JvDefPageControlBorder;
  FHintSource := hsDefault;
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

function TJvPageControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
var
  ThisTab, Tab: TTabSheet;
  Forwrd: Boolean;
begin
  Result := False;
  if HandleGlobalTab and not FormKeyPreview and
    (Key = VK_TAB) and (Shift * KeyboardShiftStates >= [ssCtrl]) then
  begin
    ThisTab := ActivePage;
    Forwrd := (Shift * KeyboardShiftStates >= [ssShift]);
    Tab := ThisTab;
    repeat
      Tab := FindNextPage(Tab, Forwrd, True);
    until (Tab = nil) or Tab.Enabled or (Tab = ThisTab);
    if Tab <> ThisTab then
    begin
      if CanChange then
      begin
        ActivePage := Tab;
        Result := True;
        Change;
      end;
      Exit;
    end;
  end;
  Result := inherited WantKey(Key, Shift, KeyText);
end;

{$IFDEF VCL}
procedure TJvPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
{$ENDIF VCL}
{$IFDEF VisualCLX}
function TJvPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean): Boolean;
{$ENDIF VisualCLX}
var
  I, RealIndex: Integer;
begin
  {$IFDEF VisualCLX}
  Result := False;
  {$ENDIF VisualCLX}
  if TabPainter <> nil then
  begin
    RealIndex := 0;
    I := 0;
    while I <= TabIndex + RealIndex do
    begin
      if not Pages[I].TabVisible then Inc(RealIndex);
      Inc(I);
    end;
    RealIndex := RealIndex + TabIndex;
    if RealIndex < PageCount then
      TabPainter.DrawTab(Self, Canvas, Images, Pages[RealIndex].ImageIndex, Pages[RealIndex].Caption, Rect, Active, Pages[RealIndex].Enabled);
  end
  else
    {$IFDEF VisualCLX} Result := {$ENDIF} inherited DrawTab(TabIndex, Rect, Active);
end;

procedure TJvPageControl.Loaded;
begin
  inherited Loaded;
  HideAllTabs := FHideAllTabs;
end;

procedure TJvPageControl.SetClientBorderWidth(const Value: TBorderWidth);
begin
  if FClientBorderWidth <> Value then
  begin
    FClientBorderWidth := Value;
    RecreateWnd;
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
      Pages[I].TabVisible := Pages[I].TabVisible and not FHideAllTabs;
    ActivePage := SaveActivePage;
    if FHideAllTabs and (SaveActivePage <> nil) then
      SaveActivePage.TabStop := False;
  end;
end;

{$IFDEF VCL}
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
{$ENDIF VCL}

procedure TJvPageControl.UpdateTabImages;
begin
  inherited UpdateTabImages;
end;

{$IFDEF VCL}
procedure TJvPageControl.WMLButtonDown(var Msg: TWMLButtonDown);
var
  hi: TTCHitTestInfo;
  I, TabIndex, RealIndex: Integer;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  hi.pt.X := Msg.XPos;
  hi.pt.Y := Msg.YPos;
  hi.flags := 0;
  TabIndex := Perform(TCM_HITTEST, 0, Longint(@hi));
  I := 0;
  RealIndex := 0;
  while I <= TabIndex + RealIndex do
  begin
    if not Pages[I].TabVisible then Inc(RealIndex);
    Inc(I);
  end;
  RealIndex := RealIndex + TabIndex;
  if (RealIndex < PageCount) and (RealIndex >= 0) and ((hi.flags and TCHT_ONITEM) <> 0) then
    if not Pages[RealIndex].Enabled then
    begin
      Msg.Result := 0;
      Exit;
    end;
  inherited;
end;

procedure TJvPageControl.WMRButtonDown(var Msg: TWMRButtonDown);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect then
  begin
    with Msg do
      P := SmallPointToPoint(SmallPoint(XPos, YPos));
    for I := 0 to PageCount -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (ActivePageIndex <> I) and CanChange then
        begin
          ActivePageIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited;
end;

{$ENDIF VCL}

function TJvPageControl.HintShow(var HintInfo: THintInfo): Boolean;
var
  TabNo: Integer;
  Tab: TTabSheet;
begin
  Result := inherited HintShow(HintInfo);

  if (FHintSource = hsDefault) or Result or (Self <> HintInfo.HintControl) then
    Exit;

  (*
      hsDefault,    // use default hint behaviour (i.e as regular control)
      hsForceMain,  // use the main controls hint even if subitems have hints
      hsForceChildren, // always use subitems hints even if empty and main control has hint
      hsPreferMain, // use main control hint unless empty then use subitems hints
      hsPreferChildren // use subitems hints unless empty then use main control hint
      );
  *)

  with HintInfo.CursorPos do
    TabNo := IndexOfTabAt(X, Y); // X&Y are expected in Client coordinates

  if (TabNo >= 0) and (TabNo < PageCount) then
    Tab := Pages[TabNo]
  else
    Tab := nil;
  if (FHintSource = hsForceMain) or ((FHintSource = hsPreferMain) and (GetShortHint(Hint) <> '')) then
    HintInfo.HintStr := GetShortHint(Self.Hint)
  else
  if (Tab <> nil) and
    ((FHintSource = hsForceChildren) or ((FHintSource = hsPreferChildren) and (GetShortHint(Tab.Hint) <> '')) or
    ((FHintSource = hsPreferMain) and (GetShortHint(Hint) = ''))) then
  begin
    HintInfo.HintStr := GetShortHint(Tab.Hint);
    HintInfo.CursorRect := TabRect(TabNo);
  end;
end;

type
  TTabSheetAccessProtected = class(TTabSheet);

function TJvPageControl.CanChange: Boolean;
begin
  Result := inherited CanChange;
  if Result and (ActivePage <> nil) and ReduceMemoryUse then
    TTabSheetAccessProtected(ActivePage).DestroyHandle;
end;

procedure TJvPageControl.SetReduceMemoryUse(const Value: Boolean);
begin
  FReduceMemoryUse := Value;
end;

procedure TJvPageControl.SetTabPainter(const Value: TJvTabControlPainter);
begin
  if FTabPainter <> Value then
  begin
    if FTabPainter <> nil then
      FTabPainter.UnRegisterChange(Self);
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
    end;
    Invalidate;
  end;
end;

procedure TJvPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = TabPainter) then
    TabPainter := nil;
end;

{$IFDEF VisualCLX}
procedure TJvPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  if RightClickSelect and (Button = mbRight) then
  begin
    P := Point(X,Y);
    for I := 0 to PageCount -1 do
    begin
      R := TabRect(I);
      if PtInRect(R, P) then
      begin
        if (ActivePageIndex <> I) and CanChange then
        begin
          ActivePageIndex := I;
          Change;
        end;
        Break;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;
{$ENDIF VisualCLX}

//=== { TJvTrackBar } ========================================================

constructor TJvTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ControlStyle := ControlStyle + [csAcceptsControls];
  {$IFDEF VCL}
  FToolTipSide := tsLeft;
  {$ENDIF VCL}
  FShowRange := True;
end;

{$IFDEF VCL}

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

{$ENDIF VCL}

procedure TJvTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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

{$IFDEF VCL}
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
        FillChar(szText, SizeOf(szText), #0);
        Result := 1;
      end
    else
      inherited;
end;
{$ENDIF VCL}

//=== { TJvTreeNode } ========================================================


{$IFDEF VCL}
class function TJvTreeNode.CreateEnh(AOwner: TTreeNodes): TJvTreeNode;
begin
  Result := Create(AOwner);
  Result.FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

procedure TJvTreeNode.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvTreeNode then
  begin
    Checked := TJvTreeNode(Source).Checked;
    Bold := TJvTreeNode(Source).Bold;
    PopupMenu := TJvTreeNode(Source).PopupMenu;
  end;
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
  if Value <> FBold then
  begin
    FBold := Value;
    FillChar(Item, SizeOf(Item), 0);
    with Item do
    begin
      mask := TVIF_STATE;
      hItem := ItemId;
      StateMask := TVIS_BOLD;
      if Value then
        Item.State := TVIS_BOLD
      else
        Item.State := 0;
      TreeView_SetItem(Handle, Item);
    end;
  end;
end;

procedure TJvTreeNode.SetChecked(Value: Boolean);
var
  Item: TTVItem;
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    FillChar(Item, SizeOf(Item), 0);
    with Item do
    begin
      hItem := ItemId;
      mask := TVIF_STATE;
      StateMask := TVIS_STATEIMAGEMASK;
      if Value then
        Item.State := TVIS_CHECKED
      else
        Item.State := TVIS_CHECKED shr 1;
      TreeView_SetItem(Handle, Item);
    end;
  end;
end;

procedure TJvTreeNode.Reinitialize;
begin
  if FChecked <> GetChecked then
  begin
    FChecked := not FChecked;
    SetChecked(not FChecked);
  end;
end;

//=== { TJvTreeView } ========================================================

const
  AutoScrollMargin = 20;
  AutoScrollTimerID = 100;

constructor TJvTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckBoxes := False;
  // ControlStyle := ControlStyle + [csAcceptsControls];
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
  if not MenuDblClick and IsMenuItemClick(Node) then
    TMenuItem(Node.Data).OnClick(TMenuItem(Node.Data));
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

procedure TJvTreeView.CreateWnd;
begin
  FReinitializeTreeNode := True;
  inherited CreateWnd;
end;

procedure TJvTreeView.DestroyWnd;
var
  I: Integer;
begin
  // update the FChecked field with the current data
  for I := 0 to Items.Count - 1 do
    TJvTreeNode(Items[I]).FChecked := TJvTreeNode(Items[I]).Checked;
  inherited DestroyWnd;
end;

procedure TJvTreeView.Delete(Node: TTreeNode);
begin
  if MultiSelect then
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
  inherited DoEnter;
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

function TJvTreeView.GetChecked(Node: TTreeNode): Boolean;
begin
  Result := TJvTreeNode(Node).Checked;
end;

function TJvTreeView.GetNodePopup(Node: TTreeNode): TPopupMenu;
begin
  Result := TJvTreeNode(Node).PopupMenu;
end;

function TJvTreeView.GetSelectedCount: Integer;
begin
  if MultiSelect then
    Result := FSelectedList.Count
  else
    Result := -1;
end;

function TJvTreeView.GetSelectedItem(Index: Integer): TTreeNode;
begin
  Result := TTreeNode(FSelectedList[Index]);
end;

function TJvTreeView.GetItemIndex: Integer;
begin
  Result := -1;
  if Assigned(Selected) and (Items.Count>0) then
  begin
    Result := 0;
    while (Result<Items.Count) and (Items[Result] <> Selected) do
      Inc(Result);
  end;
end;

procedure TJvTreeView.InternalCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if MultiSelect then
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
  if MultiSelect then
  begin
    ResetPostOperationFlags;
    if not (ssAlt in Shift) and not IsEditing then
    begin
      if Key = VK_SPACE then
        SelectItem(Selected, IsNodeSelected(Selected))
      else
      begin
        FSelectThisNode := True;
        if Shift * [ssShift, ssCtrl] = [] then
          FClearBeforeSelect := True;
      end;
    end;
  end
  else
    FSelectThisNode := True;
  inherited KeyDown(Key, Shift);
  if ((Key = VK_SPACE) or (Key = VK_RETURN)) and MenuDblClick and IsMenuItemClick(Selected) then
    TMenuItem(Selected.Data).OnClick(TMenuItem(Selected.Data));
end;

procedure TJvTreeView.KeyPress(var Key: Char);
begin
  if MultiSelect and (Key = ' ') and not IsEditing then
    Key := #0
  else
    inherited KeyPress(Key);
end;

procedure TJvTreeView.ResetPostOperationFlags;
begin
  FClearBeforeSelect := False;
  FSelectThisNode := not MultiSelect;
end;

procedure TJvTreeView.SetItemIndex(const Value: Integer);
begin
  if ItemIndex = -1 then
    Selected := nil
  else
    Selected := Items[Value];
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
{$IFDEF VisualCLX}
const
  cNewType: array [Boolean] of TListViewItemType = (itDefault, itCheckBox);
{$ENDIF VisualCLX}
begin
  if FCheckBoxes <> Value then
  begin
    FCheckBoxes := Value;
    {$IFDEF VCL}
    RecreateWnd;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Items.ChangeItemTypes(cNewType[FCheckBoxes]);
    Selected := nil;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvTreeView.SetChecked(Node: TTreeNode; Value: Boolean);
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
{$ENDIF COMPILER6_UP}

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
    if MultiSelect and (htOnItem in GetHitTestInfoAt(XPos, YPos)) then
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

procedure TJvTreeView.WMPaint(var Msg: TMessage);
var
  I: Integer;
begin
  inherited;
  { The tree node's checked property is reset at the first WM_PAINT.
    So we must set it here again, but only the first time. }
  if FReinitializeTreeNode then
  begin
    FReinitializeTreeNode := False;
    for I := 0 to Items.Count - 1 do
      TJvTreeNode(Items[I]).Reinitialize;
  end;
end;

procedure TJvTreeView.CNNotify(var Msg: TWMNotify);
var
  Node: TTreeNode;
  Point: TPoint;
  I, J: Integer;
begin
  inherited;
  if Windows.GetCursorPos(Point) then // prevent AV after "computer locked" dialog
  begin
    Point := ScreenToClient(Point);
    with Msg, Point do
      case NMHdr^.code of
        NM_CLICK, NM_RCLICK:
          begin
            Node := GetNodeAt(X, Y);
            if Assigned(Node) then
              Selected := Node
            else
            begin
              if FCheckBoxes then
              begin
                Node := GetNodeAt(X + 16, Y);
                if Assigned(Node) then
                  Selected := Node
              end;
            end;
            if (Selected <> nil) and (NMHdr^.code = NM_RCLICK) then
              TJvTreeNode(Selected).PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          end;
        TVN_SELCHANGEDA, TVN_SELCHANGEDW:
          begin
            if Assigned(FPageControl) then
              if Selected <> nil then
              begin
                //Search for the correct page
                J := -1;
                for I := 0 to FPageControl.PageCount - 1 do
                  if DoComparePage(FPageControl.Pages[I], Selected) then
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
end;

function TJvTreeView.DoComparePage(Page: TTabSheet; Node: TTreeNode): Boolean;
begin
  if Assigned(FOnComparePage) then
    FOnComparePage(Self, Page, Node, Result)
  else
    Result := AnsiSameText(Page.Caption, Node.Text);
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
      -1:
        SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
      1:
        SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
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

function TJvTreeView.GetItemHeight: Integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TVM_GETITEMHEIGHT, 0, 0)
  else
    Result := 16;
end;

procedure TJvTreeView.SetItemHeight(Value: Integer);
begin
  if Value <= 0 then
    Value := 16;
  if HandleAllocated then
    SendMessage(Handle, TVM_SETITEMHEIGHT, Value, 0);
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

procedure TJvTreeView.InsertMark(Node: TTreeNode; MarkAfter: Boolean);
begin
  if HandleAllocated then
    if Node = nil then
      RemoveMark
    else
      SendMessage(Handle, TVM_SETINSERTMARK, Integer(MarkAfter), Integer(Node.ItemId));
end;

procedure TJvTreeView.RemoveMark;
begin
  if HandleAllocated then
    SendMessage(Handle, TVM_SETINSERTMARK, 0, 0);
end;

function TJvTreeView.GetLineColor: TColor;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TVM_GETLINECOLOR, 0, 0)
  else
    Result := clDefault;
end;

procedure TJvTreeView.SetLineColor(Value: TColor);
begin
  if HandleAllocated then
  begin
    if Value = clDefault then
      Value := Font.Color;
    SendMessage(Handle, TVM_SETLINECOLOR, 0, ColorToRGB(Value));
  end;
end;

function TJvTreeView.GetMaxScrollTime: Integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TVM_GETSCROLLTIME, 0, 0)
  else
    Result := -1;
end;

procedure TJvTreeView.SetMaxScrollTime(const Value: Integer);
begin
  if HandleAllocated then
    SendMessage(Handle, TVM_SETSCROLLTIME, Value, 0);
end;

function TJvTreeView.GetUseUnicode: Boolean;
begin
  if HandleAllocated then
    Result := Boolean(SendMessage(Handle, TVM_GETUNICODEFORMAT, 0, 0))
  else
    Result := False;
end;

procedure TJvTreeView.SetUseUnicode(const Value: Boolean);
begin
  // only try to change value if not running on NT platform
  // (see MSDN: CCM_SETUNICODEFORMAT explanation for details)
  if HandleAllocated and (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    SendMessage(Handle, TVM_SETUNICODEFORMAT, Integer(Value), 0);
end;

type
  TMenuAccessProtected = class(TMenu);

procedure TJvTreeView.SetMenu(const Value: TMenu);
begin
  if FMenu <> Value then
  begin
    if (FMenu <> nil) and not (csDesigning in ComponentState) then
      TMenuAccessProtected(FMenu).OnChange := FOldMenuChange;
    FMenu := Value;
    if FMenu <> nil then
    begin
      FMenu.FreeNotification(Self);
      if not (csDesigning in ComponentState) then
      begin
        FOldMenuChange := TMenuAccessProtected(FMenu).OnChange;
        TMenuAccessProtected(FMenu).OnChange := DoMenuChange;
      end;
    end;
    RebuildFromMenu;
  end;
end;

procedure TJvTreeView.DoMenuChange(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  if Assigned(FOldMenuChange) then
    FOldMenuChange(Sender, Source, Rebuild);
  RebuildFromMenu;
end;

procedure TJvTreeView.RebuildFromMenu;
var
  I: Integer;

  procedure MakeSubMenu(AParent: TTreeNode; AMenuItem: TMenuItem);
  var
    I: Integer;
    ANode: TTreeNode;
  begin
    if (AMenuItem.Caption <> '-') and (AMenuItem.Caption <> '') then
    begin
      ANode := Items.AddChildObject(AParent, StripHotKey(AMenuItem.Caption), TObject(AMenuItem));
      ANode.ImageIndex := AMenuItem.ImageIndex;
      ANode.SelectedIndex := AMenuItem.ImageIndex;
      for I := 0 to AMenuItem.Count - 1 do
        MakeSubMenu(ANode, AMenuItem.Items[I]);
    end;
  end;

begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if Menu <> nil then
    begin
      for I := 0 to Menu.Items.Count - 1 do
        MakeSubMenu(nil, Menu.Items[I]);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FMenu then
      Menu := nil
    else
    if AComponent = FPageControl then
      PageControl := nil;
  end;
end;

procedure TJvTreeView.DblClick;
begin
  inherited DblClick;
  if MenuDblClick and IsMenuItemClick(Selected) then
    TMenuItem(Selected.Data).OnClick(TMenuItem(Selected.Data));
end;

function TJvTreeView.IsMenuItemClick(Node: TTreeNode): Boolean;
begin
 Result := Assigned(Menu) and Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TMenuItem) and Assigned(TMenuItem(Node.Data).OnClick);
end;

procedure TJvTreeView.SetPageControl(const Value: TPageControl);
begin
  if FPageControl <> Value then
  begin
    FPageControl := Value;
    if FPageControl <> nil then
      FPageControl.FreeNotification(Self);
  end;
end;

//=== { TJvIPAddressValues } =================================================

procedure TJvIPAddressValues.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvIPAddressValues.Changing(Index: Integer; Value: Byte): Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Index, Value, Result);
end;

function TJvIPAddressValues.GetValue: Cardinal;
begin
  Result := MAKEIPADDRESS(FValues[0], FValues[1], FValues[2], FValues[3]);
end;

function TJvIPAddressValues.GetValues(Index: Integer): Byte;
begin
  Result := FValues[Index];
end;

procedure TJvIPAddressValues.SetValue(const AValue: Cardinal);
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

procedure TJvIPAddressValues.SetValues(Index: Integer; Value: Byte);
begin
  if (Index >= Low(FValues)) and (Index <= High(FValues)) and (FValues[Index] <> Value) then
  begin
    FValues[Index] := Value;
    Change;
  end;
end;
{$ENDIF VCL}

procedure TJvTabDefaultPainter.SetGlyphLayout(const Value: TButtonLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetDivider(const Value: Boolean);
begin
  if FDivider <> Value then
  begin
    FDivider := Value;
    Change;
  end;
end;

procedure TJvTabDefaultPainter.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Change;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

