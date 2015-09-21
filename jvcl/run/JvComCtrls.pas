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
Peter Thrnqvist [peter3 at sourceforge dot net] added TJvIPAddress.AddressValues and TJvPageControl.ReduceMemoryUse
Alfi [alioscia_alessi att onde dott net] alternate TJvPageControl.OwnerDraw routine
Rudy Velthuis - ShowRange in TJvTrackBar
Andreas Hausladen - TJvIPAddress designtime bug, components changed to JvExVCL
Kai Gossens - TJvIPAddress: changing Color, drawing bug on XP (fat frame on edits removed)
dejoy - TJvTreeView.MoveUp/MoveDown

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

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
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclSysInfo,
  Windows, Messages, Graphics, Controls, Forms,
  Classes,
  Menus, ComCtrls, ImgList, Buttons, Types, CommCtrl,
  JvJVCLUtils, JvComponentBase, JvComponent, JvExControls, JvExComCtrls, JvWin32,
  JvDataSourceIntf;

const
  JvDefPageControlBorder = 4;
  JvDefaultInactiveColorFrom = TColor($D7D7D7);
  JvDefaultInactiveColorTo = TColor($ADADAD);

  WM_CHECKSTATECHANGED = WM_USER + 1;

  JvDefaultTreeViewMultiSelectStyle = [msControlSelect, msShiftSelect, msVisibleOnly];

  JvIP4_127_0_0_1 = 2130706433;

type
  TJvIPAddress = class;

  TJvIPAddressMinMax = record
    Min: Byte;
    Max: Byte;
  end;

  TJvIPEditControlHelper = class(TObject)
  private
    FHandle: THandle;
    FInstance: TFNWndProc;
    FIPAddress: TJvIPAddress;
    FOrgWndProc: TFarProc;
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

  TJvIPAddressComponentIndex = 1..4; // Numeration is backward, according to intel bytes order and MSDN FIRST_IPADDRESS

  TJvIPAddressRange = class(TPersistent)
  private
    FControl: TWinControl;
    FRange: array [0..3] of TJvIPAddressMinMax;
    function GetMaxRange(Index: Integer): Byte; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function GetMinRange(Index: Integer): Byte; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure SetMaxRange(const Index: Integer; const Value: Byte);
    procedure SetMinRange(const Index: Integer; const Value: Byte);
    {$IFOPT R+}
    procedure CheckIndex(const I: TJvIPAddressComponentIndex);
    {$ENDIF}
    function GetCheckedMaxRange(I: TJvIPAddressComponentIndex): Byte; {$IFOPT R-}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
    function GetCheckedMinRange(I: TJvIPAddressComponentIndex): Byte; {$IFOPT R-}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
    procedure SetCheckedMaxRange(I: TJvIPAddressComponentIndex; const Value: Byte); {$IFOPT R-}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
    procedure SetCheckedMinRange(I: TJvIPAddressComponentIndex; const Value: Byte); {$IFOPT R-}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Change(Index: Integer);

  public
    constructor Create(Control: TWinControl);
    property Min[I: TJvIPAddressComponentIndex]: Byte read GetCheckedMinRange write SetCheckedMinRange;
    property Max[I: TJvIPAddressComponentIndex]: Byte read GetCheckedMaxRange write SetCheckedMaxRange;
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

  TJvIPAddressValues = class(TObject)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TJvIPAddressChanging;
    FOwner: TJvIPAddress;

    function GetAddress: Cardinal;
    procedure SetAddress(const AValue: Cardinal);
    procedure SetValues(Index: Integer; Value: Byte);
    function GetValues(Index: Integer): Byte;
  protected
    procedure Change;
    function Changing(Index: Integer; Value: Byte): Boolean;
  public
    constructor Create(AOwner: TJvIpAddress);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TJvIPAddressChanging read FOnChanging write FOnChanging;

    property Address: Cardinal read GetAddress write SetAddress;
    property Value1: Byte index 0 read GetValues write SetValues;
    property Value2: Byte index 1 read GetValues write SetValues;
    property Value3: Byte index 2 read GetValues write SetValues;
    property Value4: Byte index 3 read GetValues write SetValues;
  end;
  
  TJvIPAddressDataConnector = class(TJvFieldDataConnector)
  private
    FEditControl: TJvIPAddress;
  protected
    procedure RecordChanged; override;
    procedure UpdateData; override;
    property EditControl: TJvIPAddress read FEditControl;
  public
    constructor Create(AEditControl: TJvIPAddress);
  end;

  // declare externally to avoid Code Completion going crazy
  TJvIPAddressDual = packed record
  case byte of
    0: (Address: LongWord);
    1: (Comps: array[TJvIPAddressComponentIndex] of Byte);
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvIPAddress = class(TJvCustomControl)
  private
    FEditControls: array [0..3] of TJvIPEditControlHelper;
    FEditControlCount: Integer;

    FAddress: TJvIPAddressDual;
    FRange: TJvIPAddressRange;
    FAddressValues: TJvIPAddressValues;
    FChanging: Boolean;
    FSaveBlank: Boolean;
    FTabThroughFields: Boolean;
    FLocalFont: HFONT;
    FOnFieldChange: TJvIpAddrFieldChangeEvent;
    FOnChange: TNotifyEvent;
    FFocusFromField: Boolean;
    FDataConnector: TJvIPAddressDataConnector;

    procedure SetDataConnector(const Value: TJvIPAddressDataConnector);
    procedure ClearEditControls;
    procedure DestroyLocalFont;
    procedure SetAddress(const Value: LongWord);
    procedure SetAddressValues(const Value: TJvIPAddressValues);
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Msg: TWMNCDestroy); message WM_DESTROY;
    procedure WMParentNotify(var Msg: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMSetFont(var Msg: TWMSetFont); message WM_SETFONT;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMCtlColorEdit(var Msg: TWMCtlColorEdit); message WM_CTLCOLOREDIT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKeyUp); message WM_KEYUP;
    procedure SelectTabControl(Previous: Boolean);
    procedure SetBlank(const Value: Boolean);
    procedure DFMSkipLegacyAddressValues(Reader: TReader);
    procedure DoNotSetRange(const Value: TJvIPAddressRange);
  protected
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure EnabledChanged; override;
    procedure ColorChanged; override;
    procedure FontChanged; override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure AdjustHeight;
    function GetControlExtents: TRect; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; dynamic;
    procedure Paint; override;

    procedure DoAddressChange(Sender: TObject); virtual;
    procedure DoAddressChanging(Sender: TObject; Index: Integer; Value: Byte; var AllowChange: Boolean); virtual;
    procedure DoFieldChange(FieldIndex: Integer; var FieldValue: Integer); dynamic;
    procedure PushAddressToWindows; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure UpdateValuesFromString(S: string);
    function GetAddressValue(Component: TJvIPAddressComponentIndex): Byte; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure SetAddressValue(Component: TJvIPAddressComponentIndex; const Value: Byte);

    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
    function CreateDataConnector: TJvIPAddressDataConnector; virtual;
    function IsNotBlank: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF} // for DFM storage
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearAddress;
    function IsBlank: Boolean;
    property AddressValue[Component: TJvIPAddressComponentIndex]: Byte read GetAddressValue write SetAddressValue;

    // this property is only here for bacwkard compatibility, please use .AddressValue[Index] and .Address instead
    property AddressValues: TJvIPAddressValues read FAddressValues write SetAddressValues stored False;
  published
    property Address: LongWord read FAddress.Address write SetAddress stored IsNotBlank default JvIP4_127_0_0_1;
    property AddressIsBlank: boolean read IsBlank write SetBlank default false;
    property Anchors;
    property AutoSize;
    property Color;
    property Constraints;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property DataConnector: TJvIPAddressDataConnector read FDataConnector write SetDataConnector;
    property DragCursor;
    property DragKind;
    property OnStartDock;
    property OnEndDock;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Range: TJvIPAddressRange read FRange write DoNotSetRange {To make DFM store vaules};
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property TabThroughFields: Boolean read FTabThroughFields write FTabThroughFields default True;
    property Text stored false;  // duplicate of Address
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldChange: TJvIpAddrFieldChangeEvent read FOnFieldChange write FOnFieldChange; // user change, not programming change
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTabControl = class(TJvExTabControl)
  private
    FTabPainter: TJvTabControlPainter;
    FRightClickSelect: Boolean;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure SetTabPainter(const Value: TJvTabControlPainter); // not WantKeys
  protected
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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
    procedure TCMAdjustRect(var Msg: TMessage); message TCM_ADJUSTRECT;
    procedure SetHideAllTabs(const Value: Boolean);
    function FormKeyPreview: Boolean;
    procedure SetReduceMemoryUse(const Value: Boolean);
    procedure SetTabPainter(const Value: TJvTabControlPainter);
  protected
    function HintShow(var HintInfo: {$IFDEF RTL200_UP}Controls.{$ENDIF RTL200_UP}THintInfo): Boolean; override;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;

    procedure Loaded; override;
    function CanChange: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTrackBar = class(TJvExTrackBar)
  private
    FOnChanged: TNotifyEvent;
    FShowRange: Boolean;
    FToolTips: Boolean;
    FToolTipSide: TJvTrackToolTipSide;
    FToolTipText: WideString;
    FOnToolTip: TJvTrackToolTipEvent;
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
    property ShowRange: Boolean read FShowRange write SetShowRange default True;
    property ToolTips: Boolean read FToolTips write SetToolTips default False;
    property ToolTipSide: TJvTrackToolTipSide read FToolTipSide write SetToolTipSide default tsLeft;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
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
    FFont: TFont;
    FBrush: TBrush;
    FOnCheckedChange: TNotifyEvent;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetBold: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetFont(const Value: TFont);
    function GetFont: TFont;
    function GetBrush: TBrush;
    procedure SetBrush(const Value: TBrush);
  protected
    procedure Reinitialize; virtual;
    procedure DoCheckedChange;
  public
    class function CreateEnh(AOwner: TTreeNodes): TJvTreeNode;

    constructor Create(AOwner: TTreeNodes); {$IFDEF RTL200_UP}override{$ELSE}virtual{$ENDIF RTL200_UP};
    destructor Destroy; override;

    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); override;

    procedure Assign(Source: TPersistent); override;
    property Checked: Boolean read GetChecked write SetChecked;
    property Bold: Boolean read GetBold write SetBold;
    property Font: TFont read GetFont write SetFont;
    property Brush: TBrush read GetBrush write SetBrush;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;

    property OnCheckedChange: TNotifyEvent read FOnCheckedChange write FOnCheckedChange;
  end;

  TJvNodeSelectCause = (nscUnknown = TVC_UNKNOWN, nscMouse = TVC_BYMOUSE, nscKeyboard = TVC_BYKEYBOARD);

  TPageChangedEvent = procedure(Sender: TObject; Item: TTreeNode; Page: TTabSheet) of object;
  TJvTreeViewComparePageEvent = procedure(Sender: TObject; Page: TTabSheet;
    Node: TTreeNode; var Matches: Boolean) of object;
  TJvTreeViewNodeCheckedChange = procedure(Sender: TObject; Node: TJvTreeNode) of object;
  TJvTreeViewSelectionChangingEvent = procedure(Sender: TObject; OldNode, NewNode: TJvTreeNode;
    Cause: TJvNodeSelectCause; var Allow: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTreeView = class(TJvExTreeView)
  private
    FAutoDragScroll: Boolean;
    FScrollDirection: Integer;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnEditCancelled: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnSelectionChanging: TJvTreeViewSelectionChangingEvent;
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
    FOnNodeCheckedChange: TJvTreeViewNodeCheckedChange;
    FCheckEventsDisabled: Boolean;
    FRecreateCheckedState: array of Boolean;
    FForceClickSelect: Boolean;
    FLineColor: TColor;

    procedure InternalCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    function GetSelectedCount: Integer;
    function GetSelectedItem(Index: Integer): TTreeNode;
    procedure SetScrollDirection(const Value: Integer);
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMCheckStateChanged(var Msg: TMessage); message WM_CHECKSTATECHANGED;
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
    procedure PostCheckStateChanged(Node: TTreeNode);
  protected
    procedure DoNodeCheckedChange(Node: TJvTreeNode);
    procedure TreeNodeCheckedChange(Sender: TObject); virtual;
    procedure SetCheckBoxes(const Value: Boolean); virtual;

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
    procedure DoEditCancelled; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoSelectionChange; dynamic;
    function DoSelectionChanging(OldNode, NewNode: TJvTreeNode; Cause: TJvNodeSelectCause): Boolean; virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure Edit(const Item: TTVItem); override;
    procedure InvalidateNode(Node: TTreeNode);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    {$IFNDEF COMPILER15_UP} // Delphi XE fixed the OnAddition/OnDeletion bug
    procedure Added(Node: TTreeNode); override;
    procedure Delete(Node: TTreeNode); override;
    {$ENDIF ~COMPILER15_UP}
    property ScrollDirection: Integer read FScrollDirection write SetScrollDirection;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DblClick; override;
    function GetCheckedFromState(Node: TTreeNode): Boolean; virtual;
    procedure SetCheckedInState(Node: TTreeNode; Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function IsNodeSelected(Node: TTreeNode): Boolean;
    procedure Select(Node: TTreeNode; ShiftState: TShiftState = []); override;
    procedure InvalidateNodeIcon(Node: TTreeNode);
    procedure InvalidateSelectedItems;
    procedure SelectItem(Node: TTreeNode; Unselect: Boolean = False);
    function GetBold(Node: TTreeNode): Boolean;
    procedure SetBold(Node: TTreeNode; Value: Boolean);
    function GetChecked(Node: TTreeNode): Boolean;
    procedure SetChecked(Node: TTreeNode; Value: Boolean);
    procedure SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
    function GetNodePopup(Node: TTreeNode): TPopupMenu;
    procedure InsertMark(Node: TTreeNode; MarkAfter: Boolean); // TVM_SETINSERTMARK
    procedure RemoveMark;

    { Move up the display order }
    function MoveUp(AAbsoluteIndex: Integer; Focus: Boolean = True): Integer;
    { move down the display order }
    function MoveDown(AAbsoluteIndex: Integer; Focus: Boolean = True): Integer;

    { Backward compatibility }
    property SelectedItems[Index: Integer]: TTreeNode read GetSelectedItem; // deprecated 'use Selections[]'
    property SelectedCount: Integer read GetSelectedCount; // deprecated 'use SelectionCount'

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
    property ForceClickSelect: Boolean read FForceClickSelect write FForceClickSelect default True;
    property HintColor;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored False;
    property Checkboxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property AutoDragScroll: Boolean read FAutoDragScroll write FAutoDragScroll default False;
    property CheckEventsDisabled: Boolean read FCheckEventsDisabled write FCheckEventsDisabled default False;
    property MultiSelectStyle default JvDefaultTreeViewMultiSelectStyle;
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
    property OnSelectionChanging: TJvTreeViewSelectionChangingEvent read FOnSelectionChanging write FOnSelectionChanging;
    property OnNodeCheckedChange: TJvTreeViewNodeCheckedChange read FOnNodeCheckedChange write FOnNodeCheckedChange;
  end;

const
  TVIS_CHECKED = $2000;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  SysUtils, Math, StrUtils,
  JclStrings,
  JvConsts, JvThemes, JvJCLUtils;

function NeedCheckStateEmulation(): Boolean; // ComCtrls 6+ under Vista+
{$IFNDEF COMPILER7_UP}
const
  ComCtlVersionIE6 = $00060000;
{$ENDIF ~COMPILER7_UP}
begin
  Result := (GetComCtlVersion < ComCtlVersionIE6) or not JclCheckWinVersion(6, 0);
end;

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

  procedure ChangeRange(FieldIndex: Integer);
  begin
    with FRange[FieldIndex] do
      FControl.Perform(IPM_SETRANGE, FieldIndex, MAKEIPRANGE(Min, Max));
  end;

var
  I: Integer;
begin
  if not FControl.HandleAllocated then
    Exit;
  if Index = -1 then
    for I := Low(FRange) to High(FRange) do
      ChangeRange(I)
  else
    ChangeRange(Index);
end;

{$IFOPT R+}
procedure TJvIPAddressRange.CheckIndex(const I: TJvIPAddressComponentIndex);
begin
  if (I > High(TJvIPAddressComponentIndex)) or (I < Low(TJvIPAddressComponentIndex)) then
    raise ERangeError.Create(Self.ClassName + ' range error: ' + IntToStr(I));
end;
{$ENDIF}

function TJvIPAddressRange.GetCheckedMaxRange(I: TJvIPAddressComponentIndex): Byte;
begin
  {$IFOPT R+}
  CheckIndex(I);
  {$ENDIF}
  Result := GetMaxRange(I - 1);
end;

function TJvIPAddressRange.GetCheckedMinRange(I: TJvIPAddressComponentIndex): Byte;
begin
  {$IFOPT R+}
  CheckIndex(I);
  {$ENDIF}
  Result := GetMinRange(I - 1);
end;

function TJvIPAddressRange.GetMaxRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Max;
end;

function TJvIPAddressRange.GetMinRange(Index: Integer): Byte;
begin
  Result := FRange[Index].Min;
end;

procedure TJvIPAddressRange.SetCheckedMaxRange(I: TJvIPAddressComponentIndex;
  const Value: Byte);
begin
  {$IFOPT R+}
  CheckIndex(I);
  {$ENDIF}
  SetMaxRange(I - 1, Value);
end;

procedure TJvIPAddressRange.SetCheckedMinRange(I: TJvIPAddressComponentIndex;
  const Value: Byte);
begin
  {$IFOPT R+}
  CheckIndex(I);
  {$ENDIF}
  SetMinRange(I - 1, Value);
end;

procedure TJvIPAddressRange.SetMaxRange(const Index: Integer; const Value: Byte);
var
  Range: TJvIPAddressMinMax;
begin
  Range := FRange[Index];
  Range.Max := Value;
  if Range.Min > Value then
    Range.Min := Value;

  Change(Index);
end;

procedure TJvIPAddressRange.SetMinRange(const Index: Integer; const Value: Byte);
var
  Range: TJvIPAddressMinMax;
begin
  Range := FRange[Index];
  Range.Min := Value;
  if Range.Max < Value then
    Range.Max := Value;

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
    Result := THandle(Windows.GetFocus) = FHandle
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
      SetWindowLongPtr(FHandle, GWLP_WNDPROC, LONG_PTR(FOrgWndProc));

    FHandle := Value;

    if FHandle <> 0 then
      FOrgWndProc := Pointer(SetWindowLongPtr(FHandle, GWLP_WNDPROC, LONG_PTR(FInstance)));
  end;
end;

procedure TJvIPEditControlHelper.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_ENABLE:
      if csDesigning in FIPAddress.ComponentState then
        Exit
      else
      begin
        {$IFDEF JVCLThemesEnabled}
        if not FIPAddress.Enabled and StyleServices.Enabled then
        begin
          EnableWindow(Handle, True);
          Exit;
        end;
        {$ENDIF JVCLThemesEnabled}
      end;
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

//=== { TJvIPAddressDataConnector } ==========================================

constructor TJvIPAddressDataConnector.Create(AEditControl: TJvIPAddress);
begin
  inherited Create;
  FEditControl := AEditControl;
end;

procedure TJvIPAddressDataConnector.RecordChanged;
begin
  if Field.IsValid then
  begin
    FEditControl.Enabled := Field.CanModify;
    FEditControl.Text := Field.AsString;
  end
  else
  begin
    FEditControl.Text := '';
    FEditControl.Enabled := False;
  end;
end;

procedure TJvIPAddressDataConnector.UpdateData;
begin
  Field.AsString := FEditControl.Text;
  FEditControl.Text := Field.AsString; // update to stored value
end;

//=== { TJvIPAddress } =======================================================

constructor TJvIPAddress.Create(AOwner: TComponent);
var
  I: Integer;
begin
  CheckCommonControl(ICC_INTERNET_CLASSES);
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFixedHeight, csReflector];
  FDataConnector := TJvIPAddressDataConnector.Create(Self);

  FRange := TJvIPAddressRange.Create(Self);
  FAddressValues := TJvIPAddressValues.Create(Self);
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

  FAddress.Address := JvIP4_127_0_0_1;
end;

type
  TReaderCracker = class (TReader)
     public property PropName;
  end;

procedure TJvIPAddress.DFMSkipLegacyAddressValues(Reader: TReader);
begin
  Reader.SkipValue;
end;

procedure TJvIPAddress.DefineProperties(Filer: TFiler);
begin
  inherited;
  if Filer is TReader then
  begin
    { We do not care about writing, but should not break upon legacy triply redundant DFMs
      If in some future Delphi, TReader cracking became impossible, properties will have to be skipped one by one
        Legacy DFM extract sample (all those just duplicate direct .Address property):
            AddressValues.Address = 16980708
            AddressValues.Value1 = 1
            AddressValues.Value2 = 3
            AddressValues.Value3 = 26
            AddressValues.Value4 = 228
    }
    if Pos('AddressValues.', TReaderCracker(Filer).PropName) = 1 then
      Filer.DefineProperty(TReaderCracker(Filer).PropName, DFMSkipLegacyAddressValues, nil,false);
  end;
end;

destructor TJvIPAddress.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FRange);
  FreeAndNil(FAddressValues);
  FDataConnector.Free;
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
  EditHandle: THandle;
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
      PushAddressToWindows;
      
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

// Type used to get access to FindNextControl outside Forms.pas
// This allows to fix Mantis 2812
type
  TWinControlAccess = class(TWinControl)
  public
    function FindNextControl(CurControl: TWinControl;
      GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
  end;

function TWinControlAccess.FindNextControl(CurControl: TWinControl;
      GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
begin
  Result := inherited FindNextControl(CurControl, GoForward, CheckTabStop, CheckParent);
end;

procedure TJvIPAddress.SelectTabControl(Previous: Boolean);
var
  Control: TWinControl;
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    // Must use GetParentForm to fix Mantis 2812, where it wasn't possible
    // to tab outside the control
    Control := TWinControlAccess(ParentForm).FindNextControl(Self, not Previous, True, False);
    if Control <> nil then
      Control.SetFocus;
  end;
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

function TJvIPAddress.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
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
    DoEraseBackground what would be impossible without self-painting because
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
  DC := GetDC(HWND_DESKTOP);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(HWND_DESKTOP, DC);
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

procedure TJvIPAddress.ClearEditControls;
var
  I: Integer;
begin
  for I := 0 to High(FEditControls) do
    if FEditControls[I] <> nil then
      FEditControls[I].Handle := 0;
  FEditControlCount := 0;
end;

procedure TJvIPAddress.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

  if not Enabled then
  begin
    Color := clBtnFace;
    Font.Color := clBtnShadow;
  end
  else
  begin
    Color := clWindow;
    Font.Color := clWindowText;
  end;
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

procedure TJvIPAddress.PushAddressToWindows;
begin // expected to be single point for this for possible platform updates/ports
  if HandleAllocated then
    Perform(IPM_SETADDRESS, 0, FAddress.Address);
end;

procedure TJvIPAddress.CNCommand(var Msg: TWMCommand);
begin
  with Msg do
    case NotifyCode of
      EN_CHANGE:
        if not FChanging then
        begin
          Perform(IPM_GETADDRESS, 0, LPARAM(@FAddress.Address));
          DoChange;
        end;
      EN_KILLFOCUS:
        begin
          FChanging := True;
          try
            if IsNotBlank then
              PushAddressToWindows;
          finally
            FChanging := False;
          end;
        end;
      EN_SETFOCUS:
        begin
          FFocusFromField := True;
          try
            // Mantis 2599: Send a WM_SETFOCUS to self so that the
            // OnEnter event (and the other control's OnExit) works.
            // We simply take the precaution to indicate it comes
            // from a field. See WMSetFocus for details
            Perform(WM_SETFOCUS, 0, 0);
          finally
            FFocusFromField := False;
          end;
        end;
    end;
  inherited;
end;

procedure TJvIPAddress.WMSetFocus(var Msg: TWMSetFocus);
begin
  // if we receive the focus from a field, then it's because
  // of a mouse click. Thus we do nothing or it would prevent
  // the focus from being directly set to the field. Note that
  // doing this does not prevent OnFocus from running, which
  // is what we want.
  if not FFocusFromField then
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
  DataConnector.Modify;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvIPAddress.DoFieldChange(FieldIndex: Integer; var FieldValue: Integer);
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(Self, FieldIndex, FRange.FRange[FieldIndex], FieldValue);
end;

procedure TJvIPAddress.DoNotSetRange(const Value: TJvIPAddressRange);
begin // do nothing, or maybe exception
end;

function TJvIPAddress.IsNotBlank: Boolean;  // for DFM storage
begin
   Result := not IsBlank;
end;

function TJvIPAddress.IsBlank: Boolean;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, IPM_ISBLANK, 0, 0) <> 0
  else
    Result := FSaveBlank;
end;

procedure TJvIPAddress.ClearAddress;
begin
  if HandleAllocated then
    Perform(IPM_CLEARADDRESS, 0, 0);
  FSaveBlank := True;
end;

procedure TJvIPAddress.SetBlank(const Value: boolean);
begin
  if Value <> IsBlank then
  begin
    FSaveBlank := Value;
    if Value then
      ClearAddress
    else
      PushAddressToWindows;
  end;
end;

function TJvIPAddress.CreateDataConnector: TJvIPAddressDataConnector;
begin
  Result := TJvIPAddressDataConnector.Create(Self);
end;

procedure TJvIPAddress.SetDataConnector(const Value: TJvIPAddressDataConnector);
begin
 if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

procedure TJvIPAddress.DoExit;
begin
  DataConnector.UpdateRecord;
  inherited DoExit;
end;

procedure TJvIPAddress.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #27) and DataConnector.Active then
  begin
    DataConnector.Reset;
    Key := #0;
  end;
end;

procedure TJvIPAddress.SetAddress(const Value: LongWord);
begin
  if AddressIsBlank or (FAddress.Address <> Value) then
  begin
    FAddress.Address := Value;
    FSaveBlank := False;
    PushAddressToWindows;
    // on Windows re-querying result is not needed,
    // GDI will automatically send EN_Change notification after applying range
  end;
end;

procedure TJvIPAddress.SetAddressValues(const Value: TJvIPAddressValues);
begin
  //  (p3) do nothing
end;

procedure TJvIPAddress.UpdateValuesFromString(S: string);
begin
  S := Trim(s);
  AddressValue[1] := StrToIntDef(StrToken(S, '.'), 0);
  AddressValue[2] := StrToIntDef(StrToken(S, '.'), 0);
  AddressValue[3] := StrToIntDef(StrToken(S, '.'), 0);
  AddressValue[4] := StrToIntDef(S, 0);
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

function TJvIPAddress.GetAddressValue(Component: TJvIPAddressComponentIndex): Byte;
begin
  Result := FAddress.Comps[5-Component];
end;

procedure TJvIPAddress.SetAddressValue(Component: TJvIPAddressComponentIndex;
  const Value: Byte);
var
  AllowChange: Boolean;
  Index: Integer;
begin
  if AddressValue[Component] <> Value then
  begin
    Component := 5 - Component; // reversing to intel byte order
    Index := Component - 1;
    AllowChange := (Component >= Low(Component)) and (Component <= High(Component)) and
                   (Value >= FRange.FRange[Index].Min) and (Value <= FRange.FRange[Index].Max);

    if AllowChange then
    begin
      FAddress.Comps[Component] := Value;
      PushAddressToWindows;
      FSaveBlank := False;
    end;
  end;
end;


function TJvIPAddress.GetControlExtents: TRect;
var
  ClientRect: TRect;
  Extents: TRect;
begin
  if ControlCount = 0 then
  begin
    // to avoid resizing to zero size when setting AutoSize to True
    Result := GetClientRect;
  end
  else
  begin
    // If the control has children, then resize to the union of both possible rectangles
    Extents := inherited GetControlExtents;
    ClientRect := GetClientRect;
    UnionRect(Result, Extents, ClientRect);
  end;
end;

procedure TJvIPAddress.GetDlgCode(var Code: TDlgCodes);
begin
  Include(Code, dcWantArrows);
  if FTabThroughFields then
    Include(Code, dcWantTab);
  Exclude(Code, dcNative); // prevent inherited call
end;

procedure TJvIPAddress.WMSetText(var Msg: TWMSetText);
begin
  // Update the internal values from the message's text
  UpdateValuesFromString(Msg.Text);

  // really long values for the text crashes the program (try: 127.0.0.8787787878787878), so we limit it here before it is set
  Msg.Text := PChar(Format('%d.%d.%d.%d', [AddressValue[1], AddressValue[2], AddressValue[3], AddressValue[4]]));

  inherited;
end;

procedure TJvIPAddress.WMGetText(var Msg: TWMGetText);
begin
  inherited;

  // Here, we are sure to have the text inside the Text member.
  // It has been retrieved by the intricate message handling of the windows
  // API, we simply use it to update the values of the AddressValues property
  // If we did not do this, then those values would not get updated as reported
  // in Mantis 2986.
  UpdateValuesFromString(Msg.Text);
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
//        Perform(Event, Value, LPARAM(SmallPoint(XPos, YPos)));
    end;
  inherited;
end;

procedure TJvIPAddress.WMSetFont(var Msg: TWMSetFont);
var
  LF: TLogFont;
begin
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
    FActiveFont.Assign(Screen.IconFont);
  FActiveFont.Color := clHighlight;
  FActiveFont.OnChange := DoFontChange;
  FActiveColorFrom := clWhite;
  FActiveColorTo := clBtnFace;
  FActiveGradientDirection := fdTopToBottom;

  FDisabledFont := TFont.Create;
  if Owner is TForm then
    FDisabledFont.Assign(TForm(Owner).Font)
  else
    FDisabledFont.Assign(Screen.IconFont);
  FDisabledFont.Color := clGrayText;
  FDisabledFont.OnChange := DoFontChange;
  FDisabledColorFrom := clBtnFace;
  FDisabledColorTo := clBtnFace;
  FDisabledGradientDirection := fdTopToBottom;

  FInactiveFont := TFont.Create;
  if Owner is TForm then
    FInactiveFont.Assign(TForm(Owner).Font)
  else
    FInactiveFont.Assign(Screen.IconFont);
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

type
  TCustomTabControlAccess = class(TCustomTabControl)
  end;

procedure TJvTabDefaultPainter.DrawTab(AControl: TCustomTabControl;
  Canvas: TCanvas; Images: TCustomImageList; ImageIndex: Integer;
  const Caption: string; const Rect: TRect; Active, Enabled: Boolean);
var
  TextRect, ImageRect: TRect;
  SaveState: Integer;
  Bmp: TBitmap;
  DrawRect: TRect;
  Points: array [0..2] of TPoint;
  TextRectWidth: Integer;
  TextRectHeight: Integer;

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
      Enabled);
    finally
      RestoreDC(Canvas.Handle, SaveState);
    end;
  end;
  if Caption <> '' then
  begin
    if TCustomTabControlAccess(AControl).TabPosition in [tpTop, tpBottom] then
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas, Caption, Length(Caption), TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
    end
    else
    begin
      Bmp := TBitmap.Create;
      try
        TextRectWidth := TextRect.Right - TextRect.Left + 1;
        TextRectHeight := TextRect.Bottom - TextRect.Top + 1;

        Bmp.Transparent := True;
        Bmp.Canvas.Font := Canvas.Font;
        Bmp.Height := Max(TextRectWidth, TextRectHeight);
        Bmp.Width := Bmp.Height;

        DrawRect := Classes.Rect(0, 0, Bmp.Width, Bmp.Height);

        DrawText(Bmp.Canvas, Caption, Length(Caption), DrawRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

        case TCustomTabControlAccess(AControl).TabPosition of
          tpLeft:
            begin
              // Rotate left by 90
              Points[0].X := 0;
              Points[0].Y := Bmp.Height - 1;
              Points[1].X := 0;
              Points[1].Y := 0;
              Points[2].X := Bmp.Width - 1;
              Points[2].Y := Bmp.Height - 1;
            end;
          tpRight:
            begin
              // Rotate right by 90
              Points[0].X := Bmp.Width - 1;
              Points[0].Y := 0;
              Points[1].X := Bmp.Width - 1;
              Points[1].Y := Bmp.Height - 1;
              Points[2].X := 0;
              Points[2].Y := 0;
            end;
        end;

        if TextRectWidth < TextRectHeight then
        begin
          Dec(Points[0].X, (Bmp.Width - TextRectWidth + 1) div 2);
          Dec(Points[1].X, (Bmp.Width - TextRectWidth + 1) div 2);
          Dec(Points[2].X, (Bmp.Width - TextRectWidth + 1) div 2);
        end
        else if TextRectWidth > TextRectHeight then
        begin
          Dec(Points[0].Y, (Bmp.Height - TextRectHeight + 1) div 2);
          Dec(Points[1].Y, (Bmp.Height - TextRectHeight + 1) div 2);
          Dec(Points[2].Y, (Bmp.Height - TextRectHeight + 1) div 2);
        end;

        // Rotate and translate to the right place
        PlgBlt(Bmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);

        // Erase left overs
        Bmp.Canvas.FillRect(Classes.Rect(TextRectWidth, 0, Bmp.Width + 1, Bmp.Height));
        Bmp.Canvas.FillRect(Classes.Rect(0, TextRectHeight, Bmp.Width, Bmp.Height + 1));

        // Copy to the final canvas
        Canvas.Draw(TextRect.Left, TextRect.Top, Bmp);
      finally
        Bmp.Free;
      end;
    end;
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

//=== { TJvTabControl } ======================================================

constructor TJvTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

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

procedure TJvTabControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
  if Assigned(TabPainter) then
    TabPainter.DrawTab(Self, Canvas, Images, GetImageIndex(TabIndex), Tabs[TabIndex], Rect, TabIndex = Self.TabIndex, Enabled)
  else
    inherited DrawTab(TabIndex, Rect, Active);
end;

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
    begin
      FTabPainter.RemoveFreeNotification(Self);
      FTabPainter.UnRegisterChange(Self);
    end;
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
      OwnerDraw := True;
    end
    else
    begin
      OwnerDraw := False;
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

function TJvPageControl.WantKey(Key: Integer; Shift: TShiftState): Boolean;
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
  Result := inherited WantKey(Key, Shift);
end;

procedure TJvPageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  I, RealIndex: Integer;
begin
  if TabPainter <> nil then
  begin
    RealIndex := 0;
    I := 0;
    while I <= TabIndex + RealIndex do
    begin
      if not Pages[I].TabVisible then
        Inc(RealIndex);
      Inc(I);
    end;
    RealIndex := RealIndex + TabIndex;
    if RealIndex < PageCount then
      TabPainter.DrawTab(Self, Canvas, Images, Pages[RealIndex].ImageIndex, Pages[RealIndex].Caption, Rect, Active, Pages[RealIndex].Enabled);
  end
  else
    inherited DrawTab(TabIndex, Rect, Active);
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
  TabIndex := Perform(TCM_HITTEST, 0, LPARAM(@hi));
  I := 0;
  RealIndex := 0;
  while I <= TabIndex + RealIndex do
  begin
    if not Pages[I].TabVisible then
      Inc(RealIndex);
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

function TJvPageControl.HintShow(var HintInfo: {$IFDEF RTL200_UP}Controls.{$ENDIF RTL200_UP}THintInfo): Boolean;
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
  TAccessTabSheet = class(TTabSheet);

function TJvPageControl.CanChange: Boolean;
begin
  Result := inherited CanChange;
  if Result and (ActivePage <> nil) and ReduceMemoryUse then
    TAccessTabSheet(ActivePage).DestroyHandle;
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
    begin
      FTabPainter.RemoveFreeNotification(Self);
      FTabPainter.UnRegisterChange(Self);
    end;
    FTabPainter := Value;
    if FTabPainter <> nil then
    begin
      FTabPainter.FreeNotification(Self);
      FTabPainter.RegisterChange(Self);
      OwnerDraw := True;
    end
    else
    begin
      OwnerDraw := False;
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

//=== { TJvTrackBar } ========================================================

constructor TJvTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ControlStyle := ControlStyle + [csAcceptsControls];
  FToolTipSide := tsLeft;
  FShowRange := True;
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
  if (Msg.NMHdr.code = TTN_NEEDTEXTW) and Assigned(FOnToolTip) then
  begin
    with PNMTTDispInfoW(Msg.NMHdr)^ do
    begin
      hinst := 0;
      ToolTipTextLocal := IntToStr(Position);
      FOnToolTip(Self, ToolTipTextLocal);
      FToolTipText := ToolTipTextLocal;
      lpszText := PWideChar(FToolTipText);
      FillChar(szText, SizeOf(szText), #0);
      Msg.Result := 1;
    end;
  end
  else
    inherited;
end;

//=== { TJvTreeNode } ========================================================

class function TJvTreeNode.CreateEnh(AOwner: TTreeNodes): TJvTreeNode;
begin
  Result := Create(AOwner);

// (obones): There is no need to create a popup for every single node, it even
//           triggers Mantis 2582
//  Result.FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

constructor TJvTreeNode.Create(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);

  FFont := nil;
  FBrush := nil;
end;

destructor TJvTreeNode.Destroy;
begin
  FFont.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvTreeNode.DoCheckedChange;
begin
  if Assigned(OnCheckedChange) then
    OnCheckedChange(Self);
end;

procedure TJvTreeNode.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvTreeNode then
  begin
    Checked := TJvTreeNode(Source).Checked;
    Bold := TJvTreeNode(Source).Bold;
    PopupMenu := TJvTreeNode(Source).PopupMenu;
    Brush := TJvTreeNode(Source).Brush;
    Font := TJvTreeNode(Source).Font;
  end;
end;

procedure TJvTreeNode.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

procedure TJvTreeNode.SetFont(const Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvTreeNode.GetFont: TFont;
begin
  if not Assigned(FFont) then
  begin
    FFont := TFont.Create;
    if Assigned(Owner) and (TreeView is TJvExTreeView) then
      FFont.Assign(TJvExTreeView(TreeView).Font);
  end;
  Result := FFont;
end;

function TJvTreeNode.GetBrush: TBrush;
begin
  if not Assigned(FBrush) then
  begin
    FBrush := TBrush.Create;
    if Assigned(Owner) and (TreeView is TJvExTreeView) then
      FBrush.Assign(TJvExTreeView(TreeView).Brush);
  end;
  Result := FBrush;
end;

procedure TJvTreeNode.SetBrush(const Value: TBrush);
begin
  Brush.Assign(Value);
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
      Result := ((Item.state and TVIS_BOLD) = TVIS_BOLD)
    else
      Result := False;
  end;
end;

function TJvTreeNode.GetChecked: Boolean;
var
  Item: TTVItem;
begin
  if Owner.Owner is TJvTreeView then
  begin
    Result := TJvTreeView(Owner.Owner).GetCheckedFromState(Self);
  end
  else
  begin
    with Item do
    begin
      mask := TVIF_STATE;
      hItem := ItemId;
      if TreeView_GetItem(Handle, Item) then
        Result := ((Item.state and TVIS_CHECKED) = TVIS_CHECKED)
      else
        Result := False;
    end;
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
        Item.state := TVIS_BOLD
      else
        Item.state := 0;
      TreeView_SetItem(Handle, Item);
    end;
  end;
end;

procedure TJvTreeNode.SetChecked(Value: Boolean);
var
  Item: TTVItem;
begin
  if Value <> GetChecked then
  begin
    FChecked := Value;
    if Owner.Owner is TJvTreeView then
      TJvTreeView(Owner.Owner).SetCheckedInState(Self, FChecked)
    else
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.hItem := ItemId;
      Item.mask := TVIF_STATE;
      Item.stateMask := TVIS_STATEIMAGEMASK;
      if Value then
        Item.state := TVIS_CHECKED
      else
        Item.state := TVIS_CHECKED shr 1;
      TreeView_SetItem(Handle, Item);
    end;
    DoCheckedChange;
  end;
end;

procedure TJvTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
var
  SaveItem, Item: TTVItem;
begin
  // Mantis 3028: We need to save the state of he item as the
  // inherited MoveTo calls Assign on a newly created TVItem.
  // Hence, the state is reset and lost, putting Bold and Checked
  // to False. We could save those two properties, but it's better
  // to save the state, because we may have other properties inside
  // it in the future.
  FillChar(SaveItem, SizeOf(SaveItem), 0);
  SaveItem.hItem := ItemId;
  SaveItem.mask := TVIF_STATE;
  TreeView_GetItem(Handle, SaveItem);

  inherited MoveTo(Destination, Mode);

  FillChar(Item, SizeOf(Item), 0);
  Item.hItem := ItemId;
  Item.mask := TVIF_STATE;
  Item.stateMask := TVIS_STATEIMAGEMASK;
  Item.state := SaveItem.state;
  TreeView_SetItem(Handle, Item);
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
  MultiSelectStyle := JvDefaultTreeViewMultiSelectStyle;
  FForceClickSelect := True; // to keep the backward compatibility as default
  FLineColor := clDefault;

  // Since IsCustomDrawn method is not virtual we have to assign ancestor's
  // OnCustomDrawItem event to enable custom drawing
  if not (csDesigning in ComponentState) then
    inherited OnCustomDrawItem := InternalCustomDrawItem;
end;

procedure TJvTreeView.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  if not MenuDblClick and IsMenuItemClick(Node) then
    TMenuItem(Node.Data).OnClick(TMenuItem(Node.Data));
end;

function TJvTreeView.CreateNode: TTreeNode;
begin
  Result := TJvTreeNode.CreateEnh(Items);
  (Result as TJvTreeNode).OnCheckedChange := TreeNodeCheckedChange;
end;

procedure TJvTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Mantis 3351: Recreating the window for adding the TVS_CHECKBOXES
  // parameter seems to trigger a bug in ComCtrl where it will show a
  // scroll bar that has nothing to do here. Setting the GWL_STYLE window
  // long shows the checkboxes and does not trigger this bug.
  {if FCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;}
end;

procedure TJvTreeView.CreateWnd;
var
  I: Integer;
begin
  FReinitializeTreeNode := True;
  inherited CreateWnd;
  // Mantis 3351: Recreating the window for adding the TVS_CHECKBOXES
  // parameter seems to trigger a bug in ComCtrl where it will show a
  // scroll bar that has nothing to do here. Setting the GWL_STYLE window
  // long shows the checkboxes and does not trigger this bug.
  if FCheckBoxes then
  begin
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_CHECKBOXES);
    // After a recreate we must set our saved checked state
    if FRecreateCheckedState <> nil then
    begin
      for I := 0 to Min(Length(FRecreateCheckedState), Items.Count) - 1 do
        TJvTreeNode(Items[I]).FChecked := FRecreateCheckedState[I];
      FRecreateCheckedState := nil;
    end;
    // Mantis #4715. We must set the StateImages image list after changing TVS_CHECKBOXES
    // because changing TVS_CHECKBOXES disables the TVSIL_STATE imagelist.
    if (StateImages <> nil) and StateImages.HandleAllocated then
      TreeView_SetImageList(Handle, StateImages.Handle, TVSIL_STATE);
  end;
  if FLineColor <> clDefault then
    SendMessage(Handle, TVM_SETLINECOLOR, 0, ColorToRGB(FLineColor));
end;

procedure TJvTreeView.DestroyWnd;
var
  I: Integer;
begin
  // update the FChecked field with the current data
  if not (csDestroying in ComponentState) then
  begin
    SetLength(FRecreateCheckedState, Items.Count);
    for I := 0 to Items.Count - 1 do
    begin
      TJvTreeNode(Items[I]).FChecked := TJvTreeNode(Items[I]).Checked;
      FRecreateCheckedState[I] := TJvTreeNode(Items[I]).FChecked;
    end;
  end;
  inherited DestroyWnd;
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

function TJvTreeView.DoSelectionChanging(OldNode, NewNode: TJvTreeNode; Cause: TJvNodeSelectCause): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectionChanging) then
    FOnSelectionChanging(Self, OldNode, NewNode, Cause, Result);
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

function TJvTreeView.GetCheckedFromState(Node: TTreeNode): Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := Node.ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := ((Item.state and TVIS_CHECKED) = TVIS_CHECKED)
    else
      Result := False;
  end;
end;

function TJvTreeView.GetNodePopup(Node: TTreeNode): TPopupMenu;
begin
  Result := TJvTreeNode(Node).PopupMenu;
end;

function TJvTreeView.GetSelectedCount: Integer;
begin
  Result := SelectionCount;
end;

function TJvTreeView.GetSelectedItem(Index: Integer): TTreeNode;
begin
  Result := Selections[Index];
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
  if (State = []) or (State = [cdsDefault]) or (State = [cdsSelected]) then
  begin
    // Mantis #5450: If HideSelection is false the node is painted as it wouldn't be
    // selected because State = [].
    if not (not HideSelection and Node.Selected and not Focused) then
    begin
      Canvas.Font := TJvTreeNode(Node).Font;
      Canvas.Brush := TJvTreeNode(Node).Brush;
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
    Windows.InvalidateRect(Handle, @R, False);
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
    Windows.InvalidateRect(Handle, @R, True);
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
  Result := (Node <> nil) and Node.Selected;
end;

procedure TJvTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Checkboxes and NeedCheckStateEmulation() and (Key = VK_SPACE) then // emulate missing notify message
    PostCheckStateChanged(Selected);

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

procedure TJvTreeView.SetItemIndex(const Value: Integer);
begin
  if Value = -1 then
    Selected := nil
  else
    Selected := Items[Value];
end;

procedure TJvTreeView.SelectItem(Node: TTreeNode; Unselect: Boolean);
begin
  if Unselect then
    Deselect(Node)
  else
    Select(Node);
end;

procedure TJvTreeView.SetBold(Node: TTreeNode; Value: Boolean);
begin
  TJvTreeNode(Node).Bold := Value;
end;

procedure TJvTreeView.DoNodeCheckedChange(Node: TJvTreeNode);
begin
  if Assigned(OnNodeCheckedChange) then
    OnNodeCheckedChange(Self, Node);
end;

procedure TJvTreeView.TreeNodeCheckedChange(Sender: TObject);
begin
  if not FCheckEventsDisabled then
    DoNodeCheckedChange(Sender as TJvTreeNode);
end;

procedure TJvTreeView.SetCheckBoxes(const Value: Boolean);
var
  CurStyle: Integer;
begin
  if FCheckBoxes <> Value then
  begin
    FCheckBoxes := Value;
    if HandleAllocated then
    begin
      // Mantis 3351: Recreating the window for adding the TVS_CHECKBOXES
      // parameter seems to trigger a bug in ComCtrl where it will show a
      // scroll bar that has nothing to do here. Setting the GWL_STYLE window
      // long shows the checkboxes and does not trigger this bug.
      //RecreateWnd;
      HandleNeeded;
      CurStyle := GetWindowLong(Handle, GWL_STYLE);
      if FCheckBoxes then
        SetWindowLong(Handle, GWL_STYLE, CurStyle or TVS_CHECKBOXES)
      else
        SetWindowLong(Handle, GWL_STYLE, CurStyle and not TVS_CHECKBOXES);
      Invalidate;
    end;
  end;
end;

procedure TJvTreeView.SetChecked(Node: TTreeNode; Value: Boolean);
begin
  TJvTreeNode(Node).Checked := Value;
end;

procedure TJvTreeView.SetCheckedInState(Node: TTreeNode; Value: Boolean);
var
  Item: TTVItem;
begin
  FillChar(Item, SizeOf(Item), 0);
  with Item do
  begin
    hItem := Node.ItemId;
    mask := TVIF_STATE;
    StateMask := TVIS_STATEIMAGEMASK;
    if Value then
      Item.state := TVIS_CHECKED
    else
      Item.state := TVIS_CHECKED shr 1;
    TreeView_SetItem(Handle, Item);
  end;
end;

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
{$IF not declared(NM_TVSTATEIMAGECHANGING)}
const
  NM_TVSTATEIMAGECHANGING = NM_FIRST - 24;
type
  { For IE >= 0x0600 }
  tagNMTVSTATEIMAGECHANGING = packed record
    hdr: NMHDR;
    hti: HTREEITEM;
    iOldStateImageIndex: Integer;
    iNewStateImageIndex: Integer;
  end;
  PNMTVStateImageChanging = ^TNMTVStateImageChanging;
  TNMTVStateImageChanging = tagNMTVSTATEIMAGECHANGING;
{$IFEND}
var
  Node: TTreeNode;
  Point: TPoint;
  I, J: Integer;
begin
  inherited;
  if Windows.GetCursorPos(Point) then // prevent AV after "computer locked" dialog
  begin
    Point := ScreenToClient(Point);
    case Msg.NMHdr.code of
      NM_TVSTATEIMAGECHANGING: // ComCtrls 6+ and WinVer >= 6.0
        begin
          if CheckBoxes and not NeedCheckStateEmulation() then
          begin
            Node := Items.GetNode(PNMTVStateImageChanging(Msg.NMHdr).hti);
            PostCheckStateChanged(Node);
          end;
        end;
      NM_CLICK, NM_RCLICK:
        begin
          Node := GetNodeAt(Point.X, Point.Y);
          if Assigned(Node) and ForceClickSelect and not MultiSelect then
            Selected := Node;

          if (Node <> nil) and (Msg.NMHdr.code = NM_RCLICK) then
            if Assigned(TJvTreeNode(Node).PopupMenu) then  // Popup menu may not be assigned
              TJvTreeNode(Node).PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

          if Checkboxes and NeedCheckStateEmulation() and (Node <> nil) and  // emulate missing notify message
             (htOnStateIcon in GetHitTestInfoAt(Point.X, Point.Y)) then
            PostCheckStateChanged(Node);
        end;
      TVN_SELCHANGEDA, TVN_SELCHANGEDW:
        begin
          DoSelectionChange;

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
      TVN_SELCHANGINGA, TVN_SELCHANGINGW:
        Msg.Result := Ord(not DoSelectionChanging(Items.GetNode(PNMTreeView(Msg.NMHdr).itemOld.hItem) as TJvTreeNode,
                                                  Items.GetNode(PNMTreeView(Msg.NMHdr).itemNew.hItem) as TJvTreeNode,
                                                  TJvNodeSelectCause(PNMTreeView(Msg.NMHdr).action)));
    end;
  end;
end;

procedure TJvTreeView.WMCheckStateChanged(var Msg: TMessage);
var
  Node: TTreeNode;
begin
  Node := Items.GetNode(HTREEITEM(Msg.LParam));
  if Node <> nil then
    if WPARAM(Ord(TJvTreeNode(Node).Checked)) <> Msg.WParam then // do not trigger if nothing was changed
      TJvTreeNode(Node).DoCheckedChange;
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
      SendMessage(Handle, TVM_SETINSERTMARK, WPARAM(MarkAfter), LPARAM(Node.ItemId));
end;

procedure TJvTreeView.RemoveMark;
begin
  if HandleAllocated then
    SendMessage(Handle, TVM_SETINSERTMARK, 0, 0);
end;

function TJvTreeView.GetLineColor: TColor;
begin
  Result := FLineColor;
  {if HandleAllocated then
  begin
    Result := TColor(SendMessage(Handle, TVM_GETLINECOLOR, 0, 0));
    if Result = TColor(CLR_DEFAULT) then
      Result := clDefault;
  end;}
end;

{$IFNDEF COMPILER15_UP} // Delphi XE fixed the OnAddition/OnDeletion bug
procedure TJvTreeView.Added(Node: TTreeNode);
var
  OrgOnAddition: TTVExpandedEvent;
begin
  OrgOnAddition := OnAddition;
  if CreateWndRestores and
    {$IFDEF COMPILER170_UP}
    (csRecreating in ControlState)
    {$ELSE}
    not (csDestroying in ComponentState)
    {$ENDIF}
  then
    OnAddition := nil;
  try
    inherited Added(Node);
  finally
    if Assigned(OrgOnAddition) then
      OnAddition := OrgOnAddition;
  end;
end;

procedure TJvTreeView.Delete(Node: TTreeNode);
var
  OrgOnDeletion: TTVExpandedEvent;
begin
  OrgOnDeletion := OnDeletion;
  if CreateWndRestores and
    {$IFDEF COMPILER10_UP}
    (csRecreating in ControlState)
    {$ELSE}
    not (csDestroying in ComponentState)
    {$ENDIF}
  then
    OnDeletion := nil;
  try
    inherited Delete(Node);
  finally
    if Assigned(OrgOnDeletion) then
      OnDeletion := OrgOnDeletion;
  end;
end;
{$ENDIF ~COMPILER15_UP}

procedure TJvTreeView.Select(Node: TTreeNode; ShiftState: TShiftState);
var
  WasSelected: Boolean;
begin
  WasSelected := (Node <> nil) and Node.Selected;
  inherited Select(Node, ShiftState);
  if WasSelected <> ((Node <> nil) and Node.Selected) then
    DoSelectionChange; // trigger the missing OnSelectionChange event
end;

function TJvTreeView.MoveUp(AAbsoluteIndex: Integer; Focus: Boolean): Integer;
var
  lNode, lNode2: TTreeNode;
begin
  Result := AAbsoluteIndex;
  if (AAbsoluteIndex > 0) and (AAbsoluteIndex < Items.Count) then
  begin
    lNode := Items[AAbsoluteIndex];

    //if not lnode.IsFirstNode then // Delphi 7+
    if not (not lnode.Deleting and (lnode.Parent = nil) and (lnode.GetPrevSibling = nil)) then
    begin
      lNode2 :=  lNode.getPrevSibling;
      if lNode2 <> nil then
        lNode.MoveTo(lNode2, naInsert);
    end;
    if Focus then
    begin
      lNode.Selected := True;
      lNode.Focused := True;
    end;
    Result := lNode.AbsoluteIndex;
  end;
end;

procedure TJvTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  SelCount: Cardinal;
begin
  if (Button = mbLeft) and MultiSelect then
  begin
    Node := GetNodeAt(X, Y);
    if (Node <> nil) and (htOnItem in GetHitTestInfoAt(X, Y)) then
    begin
      SelCount := SelectionCount;
      // The VCL doesn't do this but it's standard Windows behavior to select only the clicked
      // item if you click on it without pressing Ctrl/Shift.
      if (SelCount > 1) and (Node <> nil) and Node.Selected and ([ssShift, ssCtrl] * Shift = []) then
        ClearSelection(True);

      inherited MouseDown(Button, Shift, X, Y);
      if SelCount <> SelectionCount then
        DoSelectionChange; // trigger the missing OnSelectionChange event
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TJvTreeView.MoveDown(AAbsoluteIndex: Integer; Focus: Boolean): Integer;
var
  lNode, lNode2: TTreeNode;
begin
  Result := AAbsoluteIndex;
  if (AAbsoluteIndex >= 0) and (AAbsoluteIndex < Items.Count - 1) then
  begin
    lNode := Items[AAbsoluteIndex];

    if not (not lNode.Deleting and (lNode.Parent = nil) and (lNode.getNextSibling = nil)) then
    begin
      lNode2 :=  lNode.getNextSibling;
      if lNode2 <> nil then
        lNode2.MoveTo(lNode, naInsert);
    end;
    if Focus then
    begin
      lNode.Selected := True;
      lNode.Focused := True;
    end;
    Result := lNode.AbsoluteIndex;
  end;
end;

procedure TJvTreeView.SetLineColor(Value: TColor);
begin
  if Value <> FLineColor then
  begin
    FLineColor := Value;
    if HandleAllocated then
    begin
      if Value <> clDefault then
        Value := ColorToRGB(Value)
      else
        Value := TColor(CLR_DEFAULT);
      SendMessage(Handle, TVM_SETLINECOLOR, 0, LPARAM(Value));
    end;
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
    SendMessage(Handle, TVM_SETUNICODEFORMAT, WPARAM(Value), 0);
end;

type
  TMenuAccessProtected = class(TMenu);

procedure TJvTreeView.SetMenu(const Value: TMenu);
begin
  if FMenu <> Value then
  begin
    if (FMenu <> nil) then
    begin
      FMenu.RemoveFreeNotification(Self);
      if not (csDesigning in ComponentState) then
        TMenuAccessProtected(FMenu).OnChange := FOldMenuChange;
    end;
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

procedure TJvTreeView.PostCheckStateChanged(Node: TTreeNode);
begin
  if Node <> nil then
    PostMessage(Handle, WM_CHECKSTATECHANGED, Ord(TJvTreeNode(Node).Checked), LPARAM(Node.ItemId));
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
    if FPageControl <> nil then
      FPageControl.RemoveFreeNotification(Self);
    FPageControl := Value;
    if FPageControl <> nil then
      FPageControl.FreeNotification(Self);
  end;
end;


{ TJvIPAddressValues }

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

constructor TJvIPAddressValues.Create(AOwner: TJvIpAddress);
begin
  inherited Create;

  FOwner := AOwner; 
end;

function TJvIPAddressValues.GetAddress: Cardinal;
begin
  Result := FOwner.Address;
end;

function TJvIPAddressValues.GetValues(Index: Integer): Byte;
begin
  Result :=  FOwner.AddressValue[Index + 1];
end;

procedure TJvIPAddressValues.SetAddress(const AValue: Cardinal);
begin
  FOwner.Address := AValue;
end;

procedure TJvIPAddressValues.SetValues(Index: Integer; Value: Byte);
begin
  if Changing(Index, Value) then
  begin
    FOwner.AddressValue[Index + 1] := Value;
    Change;
  end;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.
