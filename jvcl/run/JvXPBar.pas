{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPBar.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s): dejoy
  //dejoy 2004-4-20
  --add GroupIndex,AutoCheck,Checked property in TJvXPBarItem.
  --add GetItemClass in TJvXPBarItems.
  --add GetBarItemsClass in TJvXPCustomWinXPBar.

Contributor(s): dierk schmid
  //dierk 2004-4-23
  --add property RoundedItemFrame in TJvXPCustomWinXPBar (Integer>0 is the edge radius)
  --add property ItemFrameColor in TJvXPBarColors
  //dejoy 2004-4-25
  -- splitt ItemFrameColor to CheckedFrameColor , FocusedFrameColor  in TJvXPBarColors.

Contributors(s): matej golob
  //matej 2004-5-3
  --add property BorderColor in TJvXPBarColors.
  --add property HeaderRounded
  --add property TopSpace

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvXPBar;

{$I jvcl.inc}

// sorry no theming as standalone

{$IFNDEF USEJVCL}
{$UNDEF JVCLThemesEnabled}
{$UNDEF JVCLThemesEnabledD56}
{$ENDIF USEJVCL}

interface

uses
  Windows, Classes, SysUtils, 
  Graphics, Controls, Forms, ExtCtrls, ImgList, ActnList, Messages,
  {$IFDEF VisualCLX}
  Qt, QTypes, JvQTypes,
  {$ENDIF VisualCLX}
  {$IFDEF USEJVCL}
  JvConsts,
  {$ENDIF USEJVCL}
  JvXPCore, JvXPCoreUtils;

type
  TJvXPBarRollDirection = (rdExpand, rdCollapse);

  TJvXPBarRollMode = (rmFixed, rmShrink); // rmFixed is default

  TJvXPBarHitTest =
   (
    htNone,      // mouse is inside non-supported rect
    htHeader,    // mouse is inside header
    htRollButton // mouse is inside rollbutton
   );

  TJvXPBarRollDelay = 1..200;
  TJvXPBarRollStep = 1..50;

const
  WM_XPBARAFTERCOLLAPSE = WM_USER + 303; // Ord('J') + Ord('V') + Ord('C') + Ord('L')
  WM_XPBARAFTEREXPAND = WM_XPBARAFTERCOLLAPSE + 1;

  { color constants.
  }

//        dxColor_CheckedColorXP :=  TColor($00c9b4e2);
//        dxColor_CheckedColorXP :=  TColor($00d9c1bb);
//        dxColor_CheckedColorXP :=  TColor($00e8ccae);

  dxColor_FocusedColorXP = TColor($00D8ACB0);
  dxColor_CheckedColorXP = TColor($00D9C1BB);
  {$IFDEF VisualCLX}
  clHotLight = clActiveHighlight;
  {$ENDIF VisualCLX}
  {$IFNDEF USEJVCL} // VisualCLX activates USEJVCL
  clHotLight = TColor(COLOR_HOTLIGHT or $80000000);
  {$ENDIF !USEJVCL}

  dxColor_FocusedFrameColorXP = clHotLight;
  dxColor_CheckedFrameColorXP = clHighlight;

type
  TJvXPBarItem = class;
  TJvXPBarItems = class;
  TJvXPCustomWinXPBar = class;

  TJvXPBarOnCanChangeEvent = procedure(Sender: TObject; Item: TJvXPBarItem;
    var AllowChange: Boolean) of object;

  TJvXPBarOnCollapsedChangeEvent = procedure(Sender: TObject;
    Collapsing: Boolean) of object;

  TJvXPBarOnDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    Rect: TRect; State: TJvXPDrawState; Item: TJvXPBarItem; Bitmap: TBitmap) of object;

  TJvXPBarOnItemClickEvent = procedure(Sender: TObject; Item: TJvXPBarItem) of object;

  TJvXPBarItemActionLink = class(TActionLink)
  private
    FClient: TJvXPBarItem;
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    {$IFDEF VCL}
    procedure SetCaption(const Value: string); override;
    procedure SetHint(const Value: string); override;
    function DoShowHint(var HintStr: string): Boolean; virtual;
    {$IFDEF COMPILER6_UP}
    function IsAutoCheckLinked: Boolean; virtual;
    procedure SetAutoCheck(Value: Boolean); override;
    {$ENDIF COMPILER6_UP}
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: WideString); override;
    function DoShowHint(var HintStr: WideString): Boolean; virtual;
    {$ENDIF VisualCLX}
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    property Client: TJvXPBarItem read FClient write FClient;
  end;

  TJvXPBarItemActionLinkClass = class of TJvXPBarItemActionLink;

  TJvXPBarItemClass = class of TJvXPBarItem;

  TJvXPBarItem = class(TCollectionItem)
  private
    FActionLink: TJvXPBarItemActionLink;
    FCollection: TJvXPBarItems;
    FCaption: TCaption;
    FData: Pointer;
    FDataObject: TObject;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: TImageIndex;
    FImageList: TCustomImageList;
    FName: string;
    FWinXPBar: TJvXPCustomWinXPBar;
    FTag: Integer;
    FVisible: Boolean;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FGroupIndex: Integer;
    FChecked: Boolean;
    {$IFDEF VCL}
    FAutoCheck: Boolean;
    {$IFDEF COMPILER6_UP}
    function IsAutoCheckStored: Boolean;
    {$ENDIF COMPILER6_UP}
    {$ENDIF VCL}
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsCheckedStored: Boolean;
    function GetImages: TCustomImageList;
    procedure DoActionChange(Sender: TObject);
    procedure SetAction(Value: TBasicAction);
    procedure SetCaption(Value: TCaption);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetName(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetChecked(const Value: Boolean);
    procedure TurnSiblingsOff;
  protected
    function GetActionLinkClass: TJvXPBarItemActionLinkClass; dynamic;
    function GetAction: TBasicAction; virtual;
    function GetDisplayName: string; override;
    procedure Notification(AComponent: TComponent); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;

    procedure DrawItem(AWinXPBar: TJvXPCustomWinXPBar; ACanvas: TCanvas;
      Rect: TRect; State: TJvXPDrawState; ShowItemFrame: Boolean; Bitmap: TBitmap); virtual;
    property ActionLink: TJvXPBarItemActionLink read FActionLink write FActionLink;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: TObject read FDataObject write FDataObject;
    property Images: TCustomImageList read GetImages;
    property WinXPBar: TJvXPCustomWinXPBar read FWinXPBar;
  published
    property Action: TBasicAction read GetAction write SetAction;
    {$IFDEF VCL}
    property AutoCheck: Boolean read FAutoCheck write FAutoCheck {$IFDEF COMPILER6_UP} stored IsAutoCheckStored {$ENDIF} default False;
    {$ENDIF VCL}
    property Caption: TCaption read FCaption write SetCaption stored IsCaptionStored;
    property Checked: Boolean read FChecked write SetChecked stored IsCheckedStored default False;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property Name: string read FName write SetName;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

  TJvXPBarItemsClass = class of TJvXPBarItems;

  TJvXPBarItems = class(TCollection)
  private
    FWinXPBar: TJvXPCustomWinXPBar;
    function GetItem(Index: Integer): TJvXPBarItem;
    procedure SetItem(Index: Integer; Value: TJvXPBarItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    class function GetItemClass: TJvXPBarItemClass; virtual;
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar);
    function Add: TJvXPBarItem; overload;
    function Add(Action: TBasicAction): TJvXPBarItem; overload;
    function Add(DataObject: TObject): TJvXPBarItem; overload;
    function Insert(Index: Integer): TJvXPBarItem; overload;
    function Insert(Index: Integer; Action: TBasicAction): TJvXPBarItem; overload;
    function Insert(Index: Integer; DataObject: TObject): TJvXPBarItem; overload;
    function Find(const AName: string): TJvXPBarItem; overload;
    function Find(const Action: TBasicAction): TJvXPBarItem; overload;
    function Find(const DataObject: TObject): TJvXPBarItem; overload;
    property Items[Index: Integer]: TJvXPBarItem read GetItem write SetItem; default;
  end;

  TJvXPBarVisibleItems = class(TPersistent)
  private
    FItems: TList;
    FWinXPBar: TJvXPCustomWinXPBar;
    function Exists(Item: TJvXPBarItem): Boolean;
    function GetItem(Index: Integer): TJvXPBarItem;
    procedure Add(Item: TJvXPBarItem);
    procedure Delete(Item: TJvXPBarItem);
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar);
    destructor Destroy; override;
    function Count: Integer;
    property Items[Index: Integer]: TJvXPBarItem read GetItem; default;
  end;

  TJvXPFadeThread = class(TThread)
  private
    FWinXPBar: TJvXPCustomWinXPBar;
    FRollDirection: TJvXPBarRollDirection;
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar; RollDirection: TJvXPBarRollDirection);
    procedure Execute; override;
  end;

  TJvXPBarColors = class(TPersistent)
  private
    FCheckedFrameColor: TColor;
    FFocusedFrameColor: TColor;
    FCheckedColor: TColor;
    FFocusedColor: TColor;
    FBodyColor: TColor;
    FGradientTo: TColor;
    FGradientFrom: TColor;
    FSeparatorColor: TColor;
    FBorderColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBodyColor(const Value: TColor);
    procedure SetGradientFrom(const Value: TColor);
    procedure SetGradientTo(const Value: TColor);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetCheckedColor(const Value: TColor);
    procedure SetFocusedColor(const Value: TColor);
    procedure SetCheckedFrameColor(const Value: TColor);
    procedure SetFocusedFrameColor(const Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor  default clWhite;
    property CheckedColor: TColor read FCheckedColor write SetCheckedColor default dxColor_CheckedColorXP;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor default dxColor_FocusedColorXP;
    property CheckedFrameColor: TColor read FCheckedFrameColor write SetCheckedFrameColor default dxColor_CheckedFrameColorXP;
    property FocusedFrameColor: TColor read FFocusedFrameColor write SetFocusedFrameColor default dxColor_FocusedFrameColorXP;
    property BodyColor: TColor read FBodyColor write SetBodyColor default TColor($00F7DFD6);
    property GradientFrom: TColor read FGradientFrom write SetGradientFrom default clWhite;
    property GradientTo: TColor read FGradientTo write SetGradientTo default TColor($00F7D7C6);
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default TColor($00F7D7C6);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvXPBarOptions = class(TPersistent)
  end;

  TJvXPCustomWinXPBar = class(TJvXPCustomControl)
  private
    FCollapsed: Boolean;
    FFadeThread: TJvXPFadeThread;
    FFont: TFont;
    FFontChanging: Boolean;
    FGradient: TBitmap;
    FGradientWidth: Integer;
    FHeaderFont: TFont;
    FHeaderRounded: Boolean;
    FHitTest: TJvXPBarHitTest;
    FHotTrack: Boolean;
    FHoverIndex: Integer;
    FIcon: TIcon;
    FImageList: TCustomImageList;
    FItemHeight: Integer;
    FItems: TJvXPBarItems;
    FRollDelay: TJvXPBarRollDelay;
    FRolling: Boolean;
    FRollMode: TJvXPBarRollMode;
    FRollOffset: Integer;
    FRollStep: TJvXPBarRollStep;
    FShowLinkCursor: Boolean;
    FShowRollButton: Boolean;
    FHotTrackColor: TColor;
    FVisibleItems: TJvXPBarVisibleItems;
    FAfterCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FBeforeCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FOnCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FOnCanChange: TJvXPBarOnCanChangeEvent;
    FOnDrawItem: TJvXPBarOnDrawItemEvent;
    FOnItemClick: TJvXPBarOnItemClickEvent;
    FColors: TJvXPBarColors;
    FRollImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FRollChangeLink: TChangeLink;
    FGrouped: Boolean;
    FHeaderHeight: Integer;
    FStoredHint: string;
    FShowItemFrame: Boolean;
    FRoundedItemFrame: Integer;  // DS
    FTopSpace: Integer;
    function IsFontStored: Boolean;
    procedure FontChange(Sender: TObject);
    procedure SetCollapsed(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TJvXPBarItems);
    procedure SetRollOffset(const Value: Integer);
    procedure SetShowRollButton(Value: Boolean);
    procedure ResizeToMaxHeight;
    procedure SetColors(const Value: TJvXPBarColors);
    procedure SetRollImages(const Value: TCustomImageList);
    procedure SetGrouped(const Value: Boolean);
    procedure GroupMessage;
    procedure SetHeaderHeight(const Value: Integer);
    function GetRollHeight: Integer;
    function GetRollWidth: Integer;
    procedure SetHeaderRounded(const Value: Boolean);
    procedure SetTopSpace(const Value: Integer);
  protected
    {$IFDEF VCL}
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    {$ENDIF VisualCLX}
    class function GetBarItemsClass: TJvXPBarItemsClass; virtual;
    function GetHitTestRect(const HitTest: TJvXPBarHitTest): TRect;
    function GetItemRect(Index: Integer): TRect; virtual;
    procedure ItemVisibilityChanged(Item: TJvXPBarItem); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HookMouseDown; override;
    procedure HookMouseEnter; override;
    procedure HookMouseLeave; override;
    procedure HookMouseMove(X: Integer = 0; Y: Integer = 0); override;
    procedure HookParentFontChanged; override;
    procedure HookResized; override;
    procedure SortVisibleItems(const Redraw: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure DoDrawItem(const Index: Integer; State: TJvXPDrawState); virtual;
    procedure Paint; override;
    procedure EndUpdate; override;
    procedure WMAfterXPBarCollapse(var Msg: TMessage); message WM_XPBARAFTERCOLLAPSE;
    procedure WMAfterXPBarExpand(var Msg: TMessage); message WM_XPBARAFTEREXPAND;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Colors: TJvXPBarColors read FColors write SetColors;
    property RollImages: TCustomImageList read FRollImages write SetRollImages;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont stored IsFontStored;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 28;
    property HeaderRounded: Boolean read FHeaderRounded write SetHeaderRounded default True;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default $00FF7C35;
    property Icon: TIcon read FIcon write SetIcon;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 20;
    property Items: TJvXPBarItems read FItems write SetItems;
    property RollDelay: TJvXPBarRollDelay read FRollDelay write FRollDelay default 25;
    property Rolling: Boolean read FRolling default False;
    property RollMode: TJvXPBarRollMode read FRollMode write FRollMode default rmShrink;
    property RollOffset: Integer read FRollOffset write SetRollOffset;
    property RollStep: TJvXPBarRollStep read FRollStep write FRollStep default 3;
    property ShowLinkCursor: Boolean read FShowLinkCursor write FShowLinkCursor default True;
    property ShowRollButton: Boolean read FShowRollButton write SetShowRollButton default True;
    property ShowItemFrame: Boolean read FShowItemFrame write FShowItemFrame;
    property RoundedItemFrame: Integer read FRoundedItemFrame write FRoundedItemFrame default 1; //DS
    property TopSpace: Integer read FTopSpace write SetTopSpace default 5;

    property AfterCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FAfterCollapsedChange write
      FAfterCollapsedChange;
    property BeforeCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FBeforeCollapsedChange write
      FBeforeCollapsedChange;
    property OnCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FOnCollapsedChange write FOnCollapsedChange;
    property OnCanChange: TJvXPBarOnCanChangeEvent read FOnCanChange write FOnCanChange;
    property OnDrawItem: TJvXPBarOnDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TJvXPBarOnItemClickEvent read FOnItemClick write FOnItemClick;
    procedure AdjustClientRect(var Rect: TRect); override;
    // show hints for individual items in the list
    function HintShow(var HintInfo: THintInfo): Boolean;
      {$IFDEF USEJVCL} override; {$ELSE} dynamic; {$ENDIF}
    {$IFNDEF USEJVCL}
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    {$ENDIF USEJVCL}
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHitTestAt(X, Y: Integer): TJvXPBarHitTest;
    procedure Click; override;
    property Height default 46;
    property VisibleItems: TJvXPBarVisibleItems read FVisibleItems;
    property Width default 153;
    procedure InitiateAction; override;
  end;

  TJvXPBar = class(TJvXPCustomWinXPBar)
  published
    property Caption;
    property Collapsed;
    property Colors;
    property Items;
    property RollImages;
    property Font;
    property Grouped;
    property HeaderFont;
    property HeaderHeight;
    property HeaderRounded;
    property HotTrack;
    property HotTrackColor;
    property Icon;
    property ImageList;
    property ItemHeight;
    property RollDelay;
    property RollMode;
    property RollStep;
    property ShowLinkCursor;
    property ShowRollButton;
    property ShowItemFrame;
    property RoundedItemFrame;
    property TopSpace;

    property AfterCollapsedChange;
    property BeforeCollapsedChange;
    property OnCollapsedChange;
    property OnCanChange;
    property OnDrawItem;
    property OnItemClick;

    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property Ctl3D;
    //property DockSite;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    //property TabOrder;
    //property TabStop;
    //property UseDockManager default True;
    property Align;
    property Anchors;
    //property AutoSize;
    property Constraints;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnCanResize;
    {$ENDIF VCL}
    property DragMode;
    //property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    //property OnStartDock;
    //property OnUnDock;
    property OnClick;
    property OnDblClick;
    property OnConstrainedResize;
    {$IFDEF COMPILER6_UP}
    property OnContextPopup;
    {$ENDIF COMPILER6_UP}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure RoundedFrame(Canvas: TCanvas; ARect: TRect; AColor: TColor; R: Integer);

implementation

uses
  {$IFDEF JVCLThemesEnabled}
  UxTheme,
  {$IFNDEF COMPILER7_UP}
  TmSchema,
  {$ENDIF COMPILER7_UP}
  JvThemes,
  {$ENDIF JVCLThemesEnabled}
  {$IFDEF USEJVCL}
  JvJCLUtils, JvResources,
  {$ENDIF USEJVCL}
  Menus;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvXPBar.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvXPBar.res}
{$ENDIF LINUX}

{$IFNDEF USEJVCL}
resourcestring
  RsUntitled = 'untitled';
{$ENDIF USEJVCL}

const
  FC_HEADER_MARGIN = 6;
  FC_ITEM_MARGIN = 8;

function SortByIndex(Item1, Item2: Pointer): Integer;
var
  Idx1, Idx2: Integer;
begin
  Idx1 := TCollectionItem(Item1).Index;
  Idx2 := TCollectionItem(Item2).Index;
  if Idx1 < Idx2 then
    Result := -1
  else
  if Idx1 = Idx2 then
    Result := 0
  else
    Result := 1;
end;

//=== { TJvXPBarItemActionLink } =============================================

procedure TJvXPBarItemActionLink.AssignClient(AClient: TObject);
begin
  Client := AClient as TJvXPBarItem;
end;

{$IFDEF VCL}
{$IFDEF COMPILER6_UP}
function TJvXPBarItemActionLink.IsAutoCheckLinked: Boolean;
begin
  Result := (Client.AutoCheck = (Action as TCustomAction).AutoCheck);
end;
{$ENDIF COMPILER6_UP}
{$ENDIF VCL}

function TJvXPBarItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (Client.Caption = (Action as TCustomAction).Caption);
end;

function TJvXPBarItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (Client.Checked = (Action as TCustomAction).Checked);
end;

function TJvXPBarItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (Client.Enabled = (Action as TCustomAction).Enabled);
end;

function TJvXPBarItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (Client.Hint = (Action as TCustomAction).Hint);
end;

function TJvXPBarItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (Client.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TJvXPBarItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (Client.Visible = (Action as TCustomAction).Visible);
end;

function TJvXPBarItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    JvXPMethodsEqual(TMethod(Client.OnClick), TMethod(Action.OnExecute));
end;

{$IFDEF VCL}
{$IFDEF COMPILER6_UP}
procedure TJvXPBarItemActionLink.SetAutoCheck(Value: Boolean);
begin
  if IsAutoCheckLinked then Client.AutoCheck := Value;
end;
{$ENDIF COMPILER6_UP}
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvXPBarItemActionLink.SetCaption(const Value: string);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvXPBarItemActionLink.SetCaption(const Value: TCaption);
{$ENDIF VisualCLX}
begin
  if IsCaptionLinked then
    Client.Caption := Value;
end;

procedure TJvXPBarItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    Client.Enabled := Value;
end;

procedure TJvXPBarItemActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    Client.Checked := Value;
end;

{$IFDEF VCL}
procedure TJvXPBarItemActionLink.SetHint(const Value: string);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvXPBarItemActionLink.SetHint(const Value: WideString);
{$ENDIF VisualCLX}
begin
  if IsHintLinked then
    Client.Hint := Value;
end;

procedure TJvXPBarItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    Client.ImageIndex := Value;
end;

procedure TJvXPBarItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    Client.Visible := Value;
end;

procedure TJvXPBarItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    Client.OnClick := Value;
end;

//===TJvXPBarItem ============================================================

constructor TJvXPBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := TJvXPBarItems(Collection);
  FCaption := '';
  FData := nil;
  FDataObject := nil;
  FEnabled := True;
  FImageIndex := -1;
  FImageList := nil;
  FHint := '';
  FName := '';
  FWinXPBar := FCollection.FWinXPBar;
  FTag := 0;
  FVisible := True;
  {$IFDEF VCL}
  FAutoCheck := False;
  {$ENDIF VCL}
  FChecked := False;
  FGroupIndex := 0;
  FWinXPBar.ItemVisibilityChanged(Self);
  FWinXPBar.ResizeToMaxHeight;
end;

destructor TJvXPBarItem.Destroy;
begin
  FVisible := False; // required to remove from visible list!
  FWinXPBar.ItemVisibilityChanged(Self);
  FActionLink.Free;
  FActionLink := nil;

  inherited Destroy;
  FWinXPBar.ResizeToMaxHeight;
end;

procedure TJvXPBarItem.Notification(AComponent: TComponent);
begin
  if AComponent = Action then
    Action := nil;
  if AComponent = FImageList then
    FImageList := nil;
end;

function TJvXPBarItem.GetDisplayName: string;
var
  DisplayName, ItemName: string;
begin
  DisplayName := FCaption;
  if DisplayName = '' then
    DisplayName := RsUntitled;
  ItemName := FName;
  if ItemName <> '' then
    DisplayName := DisplayName + ' [' + ItemName + ']';
  if not FVisible then
    DisplayName := DisplayName + '*';
  Result := DisplayName;
end;

function TJvXPBarItem.GetImages: TCustomImageList;
begin
  Result := nil;
  if Assigned(FImageList) then
    Result := FImageList
  else
  if Assigned(Action) and Assigned(TAction(Action).ActionList.Images) then
    Result := TAction(Action).ActionList.Images
  else
  if Assigned(FWinXPBar.FImageList) then
    Result := FWinXPBar.FImageList;
end;

procedure TJvXPBarItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      Update;
      {$IFDEF VCL}
      {$IFDEF COMPILER6_UP}
      if not CheckDefaults or (Self.AutoCheck = False) then
        Self.AutoCheck := AutoCheck;
      {$ENDIF COMPILER6_UP}
      {$ENDIF VCL}
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
      if not CheckDefaults or Self.Enabled then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or Self.Visible then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TJvXPBarItem.DrawItem(AWinXPBar: TJvXPCustomWinXPBar; ACanvas: TCanvas;
  Rect: TRect; State: TJvXPDrawState; ShowItemFrame: Boolean; Bitmap: TBitmap);
var
  ItemCaption: TCaption;
  HasImages: Boolean;
  lBar: TJvXPCustomWinXPBar;
begin
  lBar := (AWinXPBar as TJvXPCustomWinXPBar);
  HasImages := Self.Images <> nil;
  with ACanvas do
  begin
    Font.Assign(lBar.Font);
    Brush.Color := lBar.Colors.BodyColor;
    if not ShowItemFrame then
      FillRect(Rect);
    if not Self.Enabled then
      Font.Color := clGray
    else
    begin
      if dsFocused in State then
      begin
        if lBar.HotTrack then
        begin
          if lBar.FHotTrackColor <> clNone then
            Font.Color := lBar.FHotTrackColor;
          Font.Style := Font.Style + [fsUnderline];
        end;
        if ShowItemFrame then
        begin
          Brush.Color := lBar.Colors.FocusedColor;
          if lBar.RoundedItemFrame > 0 then
            RoundedFrame(ACanvas, Rect, lBar.Colors.FocusedFrameColor, lBar.RoundedItemFrame)
          else
          begin
            FillRect(Rect);
            JvXPFrame3D(ACanvas, Rect, lBar.Colors.FocusedFrameColor, lBar.Colors.FocusedFrameColor);
          end;
        end;
      end
      else
      if dsClicked in State then
      begin
        if ShowItemFrame then
        begin
          Brush.Color := lBar.Colors.CheckedColor;
          if lBar.RoundedItemFrame>0 then
            RoundedFrame(ACanvas, Rect, lBar.Colors.CheckedFrameColor, lBar.RoundedItemFrame)
          else
          begin
            FillRect(Rect);
            JvXPFrame3D(ACanvas, Rect, lBar.Colors.CheckedFrameColor, lBar.Colors.CheckedFrameColor);
          end;
        end;
      end
      else
        FillRect(Rect);
    end;
    if HasImages then
      Draw(Rect.Left, Rect.Top + (lBar.FItemHeight - Bitmap.Height) div 2, Bitmap);
    ItemCaption := Self.Caption;
    if (ItemCaption = '') and ((csDesigning in lBar.ComponentState) or (lBar.ControlCount = 0)) then
      ItemCaption := Format('(%s %d)', [RsUntitled, Index]);
    Inc(Rect.Left, 20);
    SetBkMode(Handle, Windows.TRANSPARENT);
    {$IFDEF USEJVCL}
    DrawText(ACanvas, ItemCaption, -1, Rect, DT_SINGLELINE or
      DT_VCENTER or DT_END_ELLIPSIS);
    {$ELSE}
    DrawText(ACanvas.Handle, PAnsiChar(ItemCaption), -1, Rect, DT_SINGLELINE or
      DT_VCENTER or DT_END_ELLIPSIS);
    {$ENDIF USEJVCL}
  end;
end;

function TJvXPBarItem.GetActionLinkClass: TJvXPBarItemActionLinkClass;
begin
  Result := TJvXPBarItemActionLink;
end;

procedure TJvXPBarItem.Assign(Source: TPersistent);
begin
  if Source is TJvXPBarItem then
    with TJvXPBarItem(Source) do
    begin
      Self.Action.Assign(Action);
      Self.Caption := Caption;
      Self.Data := Data;
      Self.DataObject := DataObject;
      Self.Enabled := Enabled;
      Self.Hint := Hint;
      Self.ImageList.Assign(ImageList);
      Self.ImageIndex := ImageIndex;
      Self.Name := Name;
      Self.Tag := Tag;
      Self.Visible := Visible;
      Self.OnClick := OnClick;
      {$IFDEF VCL}
      Self.AutoCheck := AutoCheck;
      {$ENDIF VCL}
      Self.Checked := Checked;
      Self.GroupIndex := GroupIndex;
      Self.OnDblClick := OnDblClick;
    end
  else
    inherited Assign(Source);
end;

{$IFDEF VCL}
{$IFDEF COMPILER6_UP}
function TJvXPBarItem.IsAutoCheckStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsAutoCheckLinked;
end;
{$ENDIF COMPILER6_UP}
{$ENDIF VCL}

function TJvXPBarItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TJvXPBarItem.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TJvXPBarItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TJvXPBarItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TJvXPBarItem.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

function TJvXPBarItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

function TJvXPBarItem.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCheckedLinked;
end;

procedure TJvXPBarItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

function TJvXPBarItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

procedure TJvXPBarItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
    FWinXPBar.InternalRedraw; // redraw image
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(FWinXPBar); // deligates notification to WinXPBar!
  end;
end;

procedure TJvXPBarItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetName(const Value: string);
begin
  if (Value <> FName) and (FCollection.Find(Value) = nil) then
    FName := Value;
end;

procedure TJvXPBarItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    FWinXPBar.ItemVisibilityChanged(Self);
    FWinXPBar.ResizeToMaxHeight;
  end;
end;

procedure TJvXPBarItem.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    if Checked then
      TurnSiblingsOff;
  end;
end;

procedure TJvXPBarItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
//    Change(False);
    if Value then
      TurnSiblingsOff;
  end;
end;

procedure TJvXPBarItem.TurnSiblingsOff;
var
  I: Integer;
  Item: TJvXPBarItem;
begin
  if (GroupIndex <> 0) and Assigned(FWinXPBar) then
  begin
    for I := 0 to FWinXPBar.Items.Count - 1 do
    begin
      Item := FWinXPBar.Items[I];
      if (Item <> Self) and (Item.GroupIndex = GroupIndex) then
        Item.Checked := False;
    end;
  end;
end;

//=== { TJvXPBarItems } ======================================================

constructor TJvXPBarItems.Create(WinXPBar: TJvXPCustomWinXPBar);
begin
  inherited Create(GetItemClass());
  FWinXPBar := WinXPBar;
end;

function TJvXPBarItems.Add: TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited Add);
end;

function TJvXPBarItems.Add(Action: TBasicAction): TJvXPBarItem;
begin
  Result := Add;
  Result.Action := Action;
end;

function TJvXPBarItems.Add(DataObject: TObject): TJvXPBarItem;
begin
  Result := Add;
  Result.DataObject := DataObject;
end;

function TJvXPBarItems.Insert(Index: Integer): TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited Insert(Index));
end;

function TJvXPBarItems.Insert(Index: Integer; Action: TBasicAction): TJvXPBarItem;
begin
  Result := Insert(Index);
  Result.Action := Action;
end;

function TJvXPBarItems.Insert(Index: Integer; DataObject: TObject): TJvXPBarItem;
begin
  Result := Insert(Index);
  Result.DataObject := DataObject;
end;

function TJvXPBarItems.GetOwner: TPersistent;
begin
  Result := FWinXPBar;
end;

class function TJvXPBarItems.GetItemClass: TJvXPBarItemClass;
begin
  Result := TJvXPBarItem;
end;

function TJvXPBarItems.GetItem(Index: Integer): TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited GetItem(Index));
end;

procedure TJvXPBarItems.SetItem(Index: Integer; Value: TJvXPBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvXPBarItems.Update(Item: TCollectionItem);
begin
  FWinXPBar.SortVisibleItems(True);
end;

function TJvXPBarItems.Find(const AName: string): TJvXPBarItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJvXPBarItems.Find(const Action: TBasicAction): TJvXPBarItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Action = Action then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJvXPBarItems.Find(const DataObject: TObject): TJvXPBarItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].DataObject = DataObject then
    begin
      Result := Items[I];
      Break;
    end;
end;

//=== { TJvXPBarVisibleItems } ===============================================

constructor TJvXPBarVisibleItems.Create(WinXPBar: TJvXPCustomWinXPBar);
begin
  inherited Create;
  FItems := TList.Create;
  FWinXPBar := WinXPBar;
end;

destructor TJvXPBarVisibleItems.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvXPBarVisibleItems.GetItem(Index: Integer): TJvXPBarItem;
begin
  Result := nil;
  if Index < FItems.Count then
    Result := FItems[Index];
end;

function TJvXPBarVisibleItems.Count: Integer;
begin
  Result := FItems.Count;
end;

function TJvXPBarVisibleItems.Exists(Item: TJvXPBarItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I] = Item then
    begin
      Result := True;
      Break;
    end;
end;

procedure TJvXPBarVisibleItems.Add(Item: TJvXPBarItem);
begin
  if not Exists(Item) then
  begin
    FItems.Add(Item);
    FWinXPBar.SortVisibleItems(False);
  end;
end;

procedure TJvXPBarVisibleItems.Delete(Item: TJvXPBarItem);
begin
  if Exists(Item) then
    FItems.Delete(FItems.IndexOf(Item));
end;

//=== { TJvXPFadeThread } ====================================================

constructor TJvXPFadeThread.Create(WinXPBar: TJvXPCustomWinXPBar;
  RollDirection: TJvXPBarRollDirection);
begin
  inherited Create(True);
  FWinXPBar := WinXPBar;
  FRollDirection := RollDirection;
  FreeOnTerminate := True;
  Suspended := False;
end;

procedure TJvXPFadeThread.Execute;
var
  NewOffset: Integer;
begin
  while not Terminated do
  try
    FWinXPBar.FRolling := True;

    { calculate new roll offset }
    if FRollDirection = rdCollapse then
      NewOffset := FWinXPBar.RollOffset - FWinXPBar.FRollStep
    else
      NewOffset := FWinXPBar.RollOffset + FWinXPBar.FRollStep;

    { validate offset ranges }
    if NewOffset < 0 then
      NewOffset := 0;
    if NewOffset > FWinXPBar.FItemHeight then
      NewOffset := FWinXPBar.FItemHeight;
    FWinXPBar.RollOffset := NewOffset;

    { terminate on 'out-of-range' }
    if ((FRollDirection = rdCollapse) and (NewOffset = 0)) or
      ((FRollDirection = rdExpand) and (NewOffset = FWinXPBar.FItemHeight)) then
      Terminate;
    {$IFDEF VisualCLX}
    WakeUpGUIThread;
    {$ENDIF VisualCLX}

    { idle process }
    Sleep(FWinXPBar.FRollDelay);
  finally
    FWinXPBar.FRolling := False;
  end;

  { redraw button state }
  FWinXPBar.FCollapsed := FRollDirection = rdCollapse;
  if FWinXPBar.FShowRollButton then
    FWinXPBar.InternalRedraw;

  { update inspector }
  if csDesigning in FWinXPBar.ComponentState then
    {$IFDEF VCL}
    TCustomForm(FWinXPBar.Owner).Designer.Modified
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    TCustomForm(FWinXPBar.Owner).DesignerHook.Modified
    {$ENDIF VisualCLX}
  else
    PostMessage(FWinXPBar.Handle, WM_XPBARAFTERCOLLAPSE,
      Ord(FRollDirection = rdCollapse), 0);
  {$IFDEF VisualCLX}
  WakeUpGUIThread;
  {$ENDIF VisualCLX}
end;

//=== { TJvXPBarColors } =====================================================

constructor TJvXPBarColors.Create;
{$IFDEF JVCLThemesEnabled}
var
  Details: TThemedElementDetails;
  AColor: COLORREF;
{$ENDIF JVCLThemesEnabled}
begin
  inherited Create;
  FBodyColor := TColor($00F7DFD6);
  FBorderColor := clWhite;
  FGradientFrom := clWhite;
  FGradientTo := TColor($00F7D7C6);
  FSeparatorColor := TColor($00F7D7C6);
  FCheckedColor := dxColor_CheckedColorXP;
  FFocusedColor := dxColor_FocusedColorXP;
  FCheckedFrameColor := dxColor_CheckedFrameColorXP;
  FFocusedFrameColor := dxColor_FocusedFrameColorXP;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tebHeaderBackgroundNormal);
    with Details do
    begin
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_FILLCOLOR, AColor) = 0 then
        FBodyColor := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_GRADIENTCOLOR1, AColor) = 0 then
        FGradientFrom := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_GRADIENTCOLOR2, AColor) = 0 then
        FGradientTo := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_EDGEFILLCOLOR, AColor) = 0 then
        FSeparatorColor := AColor;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvXPBarColors.Assign(Source: TPersistent);
begin
  if Source is TJvXPBarColors then
    with TJvXPBarColors(Source) do
    begin
      Self.CheckedColor := CheckedColor;
      Self.FocusedColor := FocusedColor;
      Self.CheckedFrameColor := CheckedFrameColor;
      Self.FocusedFrameColor := FocusedFrameColor;
      Self.BodyColor := BodyColor;
      Self.GradientTo := GradientTo;
      Self.GradientFrom := GradientFrom;
      Self.SeparatorColor := SeparatorColor;
    end
  else
    inherited Assign(Source);
end;

procedure TJvXPBarColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvXPBarColors.SetBodyColor(const Value: TColor);
begin
  if FBodyColor <> Value then
  begin
    FBodyColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetGradientFrom(const Value: TColor);
begin
  if FGradientFrom <> Value then
  begin
    FGradientFrom := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetGradientTo(const Value: TColor);
begin
  if FGradientTo <> Value then
  begin
    FGradientTo := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetSeparatorColor(const Value: TColor);
begin
  if FSeparatorColor <> Value then
  begin
    FSeparatorColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetCheckedColor(const Value: TColor);
begin
  if FCheckedColor <> Value then
  begin
    FCheckedColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetFocusedColor(const Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetCheckedFrameColor(const Value: TColor);
begin
  if FCheckedFrameColor <> Value then
  begin
    FCheckedFrameColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetFocusedFrameColor(const Value: TColor);
begin
  if FFocusedFrameColor <> Value then
  begin
    FFocusedFrameColor := Value;
    Change;
  end;
end;

//=== { TJvXPCustomWinXPBar } ================================================

constructor TJvXPCustomWinXPBar.Create(AOwner: TComponent);
const
  MouseEvents: TJvXPControlStyle = [csRedrawMouseEnter, csRedrawMouseLeave];
begin
  inherited Create(AOwner);
  FStoredHint := '|'; // no one in their right mind uses a pipe as the only character in a hint...
  ControlStyle := ControlStyle - [csDoubleClicks] + [csAcceptsControls, csActionClient];
  ExControlStyle := [csRedrawCaptionChanged];
  ExControlStyle := ExControlStyle + MouseEvents;
  Height := 46;
  HotTrack := True; // initialize mouse events
  Width := 153;
  FColors := TJvXPBarColors.Create;
  FColors.OnChange := DoColorsChange;
  FCollapsed := False;
  FFadeThread := nil;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := DoColorsChange;
  FRollChangeLink := TChangeLink.Create;
  FRollChangeLink.OnChange := DoColorsChange;
  FTopSpace := 5;

  FFont := TFont.Create;
  FFont.Color := $00840000;
  FFont.Size := 8;
  FFont.OnChange := FontChange;
  FGradient := TBitmap.Create;
  FHeaderHeight := 28;
  FHeaderRounded := True;
  FGradientWidth := 0;
  FHeaderFont := TFont.Create;
  FHeaderFont.Color := $00840000;
  FHeaderFont.Size := 8;
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := FontChange;

  FHitTest := htNone;

  FHotTrackColor := $00FF7C35;
  FHoverIndex := -1;
  FIcon := TIcon.Create;
  FItemHeight := 20;
  FItems := GetBarItemsClass.Create(Self);
  FRollDelay := 25;
  FRolling := False;
  FRollMode := rmShrink;
  FRollOffset := FItemHeight;
  FRollStep := 3;
  FShowLinkCursor := True;
  FShowRollButton := True;
  FVisibleItems := TJvXPBarVisibleItems.Create(Self);
end;

destructor TJvXPCustomWinXPBar.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  FGradient.Free;
  FIcon.Free;
  FItems.Free;
  FVisibleItems.Free;
  FColors.Free;
  FImageChangeLink.Free;
  FRollChangeLink.Free;
  inherited Destroy;
end;

procedure TJvXPCustomWinXPBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = FImageList then
      ImageList := nil;
    if AComponent = FRollImages then
      RollImages := nil;
    for I := 0 to FItems.Count - 1 do
      FItems[I].Notification(AComponent);
  end;
  inherited Notification(AComponent, Operation);
end;

function TJvXPCustomWinXPBar.IsFontStored: Boolean;
begin
  Result := not ParentFont {$IFDEF VCL} and not DesktopFont {$ENDIF};
end;

procedure TJvXPCustomWinXPBar.FontChange(Sender: TObject);
begin
  if (not FFontChanging) and not (csLoading in ComponentState) then
    ParentFont := False;
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.ResizeToMaxHeight;
var
  NewHeight: Integer;
begin
{ TODO: Check this!!! }
  if IsLocked then
    Exit;
  NewHeight := FC_HEADER_MARGIN + HeaderHeight + FVisibleItems.Count * FRollOffset + FC_ITEM_MARGIN + 1;
  { full collapsing }
  if ((FRolling and not FCollapsed) or (not FRolling and FCollapsed) or
    (FVisibleItems.Count = 0)) then
    Dec(NewHeight, FC_ITEM_MARGIN);
//  if Height <> NewHeight then
  Height := NewHeight - 5 + FTopSpace;
end;

function TJvXPCustomWinXPBar.GetHitTestAt(X, Y: Integer): TJvXPBarHitTest;
begin
  Result := htNone;
  if PtInRect(GetHitTestRect(htHeader), Point(X, Y)) then
    Result := htHeader;
  if PtInRect(GetHitTestRect(htRollButton), Point(X, Y)) then
    Result := htRollButton;
end;

function TJvXPCustomWinXPBar.GetItemRect(Index: Integer): TRect;
begin
  Result.Left := 3;
  Result.Right := Width - 3;
  if FRollMode = rmShrink then
    Result.Top := FC_HEADER_MARGIN + HeaderHeight + FC_ITEM_MARGIN div 2 + Index * FRollOffset - 4 + FTopSpace
  else
    Result.Top := FC_HEADER_MARGIN + HeaderHeight + FC_ITEM_MARGIN div 2 + Index * FItemHeight - 4 + FTopSpace;
  Result.Bottom := Result.Top + FItemHeight;
end;

function TJvXPCustomWinXPBar.GetRollHeight: Integer;
begin
  if Assigned(FRollImages) then
    Result := FRollImages.Height
  else
    Result := 18;
end;

function TJvXPCustomWinXPBar.GetRollWidth: Integer;
begin
  if Assigned(FRollImages) then
    Result := FRollImages.Width
  else
    Result := 18;
end;

function TJvXPCustomWinXPBar.GetHitTestRect(const HitTest: TJvXPBarHitTest): TRect;

begin
  case HitTest of
    htHeader:
      Result := Bounds(0, 5, Width, FHeaderHeight);
    htRollButton:
      Result := Bounds(Width - 24, (FHeaderHeight - GetRollHeight) div 2, GetRollWidth, GetRollHeight);
  end;
end;

procedure TJvXPCustomWinXPBar.SortVisibleItems(const Redraw: Boolean);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FVisibleItems.FItems.Sort(@SortByIndex);
  if Redraw then
    InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.ItemVisibilityChanged(Item: TJvXPBarItem);
begin
  // update visible-item list
  if Item.Visible then
    FVisibleItems.Add(Item)
  else
    FVisibleItems.Delete(Item);
end;

procedure TJvXPCustomWinXPBar.HookMouseDown;
var
  Rect: TRect;
begin
  inherited HookMouseDown; // update drawstate
  if FHitTest = htRollButton then
  begin
    Rect := GetHitTestRect(htRollButton);
    {$IFDEF VisualCLX} QWindows.{$ENDIF}InvalidateRect(Handle, @Rect, False);
  end;
end;

procedure TJvXPCustomWinXPBar.HookMouseEnter;
begin
  inherited HookMouseEnter;
  if (FHoverIndex <> -1) then
    DoDrawItem(FHoverIndex, [dsFocused]);
end;

procedure TJvXPCustomWinXPBar.HookMouseLeave;
begin
  inherited HookMouseLeave;
  if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex] <> nil) and (not FVisibleItems[FHoverIndex].Checked) then
    DoDrawItem(FHoverIndex, []);
end;

procedure TJvXPCustomWinXPBar.HookMouseMove(X, Y: Integer);
var
  Rect: TRect;
  OldHitTest: TJvXPBarHitTest;
  NewIndex, Header: Integer;
begin
  OldHitTest := FHitTest;
  FHitTest := GetHitTestAt(X, Y);
  if FHitTest <> OldHitTest then
  begin
    Rect := Bounds(0, 5, Width, FHeaderHeight); // header
    {$IFDEF VisualCLX} QWindows.{$ENDIF}InvalidateRect(Handle, @Rect, False);
    if FShowLinkCursor then
    begin
      if FHitTest <> htNone then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
    end;
  end;
  Header := FC_HEADER_MARGIN + HeaderHeight + FC_ITEM_MARGIN;
  if (Y < Header) or (Y > Height - FC_ITEM_MARGIN) then
    NewIndex := -1
  else
    NewIndex := (Y - Header) div ((Height - Header) div FVisibleItems.Count);
  if NewIndex <> -1 then
  begin
    if FStoredHint = '|' then
      FStoredHint := Hint;
    if Action is TCustomAction then
      inherited Hint := TCustomAction(Action).Hint
    else
      inherited Hint := VisibleItems[NewIndex].Hint;
  end
  else
  begin
    if FStoredHint <> '|' then
      inherited Hint := FStoredHint;
    FStoredHint := '|';
  end;

  if NewIndex <> FHoverIndex then
  begin
    if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex] <> nil) then
      if FVisibleItems[FHoverIndex].Checked then
        DoDrawItem(FHoverIndex, [dsClicked])
      else
        DoDrawItem(FHoverIndex, []);
    FHoverIndex := NewIndex;
    if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex] <> nil) and (FVisibleItems[FHoverIndex].Enabled) then
    begin
      DoDrawItem(FHoverIndex, [dsFocused]);
      if FShowLinkCursor then
        Cursor := crHandPoint;
    end
    else
    if FShowLinkCursor then
      Cursor := crDefault;
  end;

  inherited HookMouseMove(X, Y);
end;

procedure TJvXPCustomWinXPBar.HookParentFontChanged;
begin
  if ParentFont then
  begin
    FFontChanging := True;
    try
      FFont.Color := $00E75100;
      FFont.Name := inherited Font.Name;
      FFont.Size := 8;
      FFont.Style := inherited Font.Style;
      FHeaderFont.Color := $00E75100;
      FHeaderFont.Name := Font.Name;
      FHeaderFont.Size := 8;
      FHeaderFont.Style := [fsBold];
    finally
      FFontChanging := False;
    end;
    inherited HookParentFontChanged;
  end;
end;

procedure TJvXPCustomWinXPBar.HookResized;
begin
  // perform actions only on 'width'-change
  if FGradientWidth <> Width then
  begin
    FGradientWidth := Width;
    {$IFDEF VCL}
    // recreate gradient rect
    JvXPCreateGradientRect(Width, FHeaderHeight,
      clWhite, $00F7D7C6, 32, gsLeft, False, FGradient);
    {$ENDIF VCL}
  end;

  // resize to maximum height
  ResizeToMaxHeight;
  inherited HookResized;
end;

procedure TJvXPCustomWinXPBar.SetCollapsed(Value: Boolean);
begin
  if Value <> FCollapsed then
  begin
    if not (csLoading in ComponentState) then
    begin
      if Assigned(FBeforeCollapsedChange) then
        FBeforeCollapsedChange(Self, Value);
      if Value then
        FFadeThread := TJvXPFadeThread.Create(Self, rdCollapse)
      else
        FFadeThread := TJvXPFadeThread.Create(Self, rdExpand);
      if Assigned(FOnCollapsedChange) then
        FOnCollapsedChange(Self, Value);
    end
    else
    begin
      FCollapsed := Value;
      FRolling := True;
      if Value then
        RollOffset := 0
      else
        RollOffset := FItemHeight;
      FRolling := False;
      if Grouped and not Value then
        GroupMessage;
    end;
  end;
end;

procedure TJvXPCustomWinXPBar.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.SetHotTrack(Value: Boolean);
const
  MouseEvents: TJvXPControlStyle = [csRedrawMouseEnter, csRedrawMouseLeave];
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
//    if FHotTrack then
//      ExControlStyle := ExControlStyle + MouseEvents
//    else
//      ExControlStyle := ExControlStyle - MouseEvents;
    if not (csLoading in ComponentState) then
      InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetHotTrackColor(Value: TColor);
begin
  if Value <> FHotTrackColor then
  begin
    FHotTrackColor := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetIcon(Value: TIcon);
begin
  if Value <> FIcon then
  begin
    FIcon.Assign(Value);
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    if FImageList <> nil then
      FImageList.UnRegisterChanges(FImageChangeLink);
    FImageList := Value;
    if FImageList <> nil then
    begin
      FImageList.FreeNotification(Self);
      FImageList.RegisterChanges(FImageChangeLink);
    end;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetItemHeight(Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    if not FCollapsed then
      RollOffset := FItemHeight;
  end;
end;

procedure TJvXPCustomWinXPBar.SetItems(Value: TJvXPBarItems);
begin
  FItems.Assign(Value);
end;

procedure TJvXPCustomWinXPBar.SetRollOffset(const Value: Integer);
begin
  if Value <> FRollOffset then
  begin
    FRollOffset := Value;
    ResizeToMaxHeight;
  end;
end;

procedure TJvXPCustomWinXPBar.SetShowRollButton(Value: Boolean);
begin
  if Value <> FShowRollButton then
  begin
    FShowRollButton := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.EndUpdate;
begin
  inherited EndUpdate;
  ResizeToMaxHeight;
end;

procedure TJvXPCustomWinXPBar.Click;
var
  AllowChange, CallInherited: Boolean;
  {$IFDEF VCL}
  lItem: TJvXPBarItem;
  {$ENDIF VCL}
begin
  CallInherited := True;
  if (FShowRollButton) and (FHitTest <> htNone) then
    Collapsed := not Collapsed;
  if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex] <> nil) and (FVisibleItems[FHoverIndex].Enabled) then
  begin
    AllowChange := True;
    if Assigned(FOnCanChange) then
      FOnCanChange(Self, FVisibleItems[FHoverIndex], AllowChange);
    if not AllowChange then
      Exit;

    //dejoy add
    {$IFDEF VCL}
    lItem := FVisibleItems[FHoverIndex];
    with lItem do
    begin
      {$IFDEF COMPILER6_UP}
      if (not Assigned(ActionLink) and AutoCheck) or
        (Assigned(ActionLink) and not ActionLink.IsAutoCheckLinked and AutoCheck) then
      {$ELSE}
      if AutoCheck then
      {$ENDIF COMPILER6_UP}
        lItem.Checked := not lItem.Checked;
    end;
    {$ENDIF VCL}
    if FVisibleItems[FHoverIndex].Checked then
      DrawState := DrawState + [dsClicked]
    else
      DrawState := DrawState - [dsClicked];

    if Assigned(FOnItemClick) then
    begin
      FOnItemClick(Self, FVisibleItems[FHoverIndex]);
      CallInherited := False;
    end;
    if Assigned(FVisibleItems[FHoverIndex].FOnClick) then
    begin
      { set linked 'action' as Sender }
      if Assigned(FVisibleItems[FHoverIndex].Action) then
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex].Action)
      else
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex]);
      CallInherited := False;
    end;
    Collapsed := False;
    InternalRedraw; //dejoy
  end;
  if CallInherited then
    inherited Click;
end;

procedure TJvXPCustomWinXPBar.DoDrawItem(const Index: Integer; State: TJvXPDrawState);
var
  Bitmap: TBitmap;
  ItemRect: TRect;
  HasImages: Boolean;
begin
  Bitmap := TBitmap.Create;
  with Canvas do
  try
    {$IFDEF VisualCLX}
    Start;
    {$ENDIF VisualCLX}
    Bitmap.Assign(nil);
    ItemRect := GetItemRect(Index);
    HasImages := FVisibleItems[Index].Images <> nil;
    if HasImages then
      FVisibleItems[Index].Images.GetBitmap(FVisibleItems[Index].ImageIndex, Bitmap);
    Bitmap.Transparent := True;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Canvas, ItemRect, State, FVisibleItems[Index], Bitmap)
    else
      FVisibleItems[Index].DrawItem(Self, Canvas, ItemRect, State, ShowItemFrame, Bitmap);
  finally
    Bitmap.Free;
    {$IFDEF VisualCLX}
    Stop;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvXPCustomWinXPBar.Paint;
var
  Rect: TRect;
  Bitmap: TBitmap;
  Index, I: Integer;
  OwnColor: TColor;
begin
  with Canvas do
  begin
    { get client rect }
    Rect := GetClientRect;

    { fill non-client area }
    {$IFDEF VisualCLX}
    Brush.Color := TJvXPWinControl(Parent).Color;
    with Rect do
      FillRect(Bounds(Left, Top, Right - Left, FTopSpace));
    {$ENDIF VisualCLX}
    Inc(Rect.Top, FTopSpace + FHeaderHeight);
    Brush.Color := FColors.BodyColor; //$00F7DFD6;
    FillRect(Rect);
    Dec(Rect.Top, FHeaderHeight);

    { draw header }
    {$IFDEF VCL}
    JvXPCreateGradientRect(Width, FHeaderHeight, FColors.GradientFrom,
      FColors.GradientTo, 32, gsLeft, True, FGradient);
    Draw(0, Rect.Top, FGradient);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FillGradient(Handle, Bounds(0, Rect.Top, Width, FHeaderHeight),
      32, FColors.GradientFrom, FColors.GradientTo, gdHorizontal);
    {$ENDIF VisualCLX}

    { draw frame... }
    Brush.Color := FColors.FBorderColor;
    FrameRect({$IFDEF VisualCLX} Canvas, {$ENDIF} Rect);

    if FHeaderRounded then
    begin
      OwnColor := TJvXPWinControl(Parent).Color;
      Pixels[0, Rect.Top] := OwnColor;
      Pixels[0, Rect.Top + 1] := OwnColor;
      Pixels[1, Rect.Top] := OwnColor;
      Pixels[1, Rect.Top + 1] := FColors.FBorderColor;
      Pixels[Width - 1, Rect.Top] := OwnColor;
      Pixels[Width - 2, Rect.Top] := OwnColor;
      Pixels[Width - 1, Rect.Top + 1] := OwnColor;
      Pixels[Width - 2, Rect.Top + 1] := FColors.FBorderColor;
    end;

    { draw Button }
    if FShowRollButton and (Width >= 115) then
    begin
      Bitmap := TBitmap.Create;
      try
        if Assigned(FRollImages) then
        begin
          // format:
          // 0 - normal collapsed
          // 1 - normal expanded
          // 2 - hot collapsed
          // 3 - hot expanded
          // 4 - down collapsed
          // 5 - down expanded
          Index := 0; // normal
          if FHitTest = htRollButton then
          begin
            if dsHighlight in DrawState then
              Index := 2; // hot
            if (dsClicked in DrawState) and (dsHighlight in DrawState) then
              Index := 4; // down
          end;
          if not FCollapsed then
            Inc(Index);
          if Index >= FRollImages.Count then
            Index := Ord(not FCollapsed);
          FRollImages.GetBitmap(Index, Bitmap);
        end
        else
        begin
          Index := 0;
          if FHitTest = htRollButton then
          begin
            if dsHighlight in DrawState then
              Index := 1; // hot
            if (dsClicked in DrawState) and (dsHighlight in DrawState) then
              Index := 2; // down
          end;
          if FCollapsed then
            Bitmap.LoadFromResourceName(hInstance, 'XPEXPAND' + IntToStr(Index))
          else
            Bitmap.LoadFromResourceName(hInstance, 'XPCOLLAPSE' + IntToStr(Index));
        end;
        Bitmap.Transparent := True;
        Draw(Rect.Right - 24, Rect.Top + (HeaderHeight - GetRollHeight) div 2, Bitmap);
      finally
        Bitmap.Free;
      end;
      Dec(Rect.Right, 25);
    end;

    { draw seperator line }
    Pen.Color := FColors.SeparatorColor;
    JvXPDrawLine(Canvas, 1, Rect.Top + FHeaderHeight, Width - 1, Rect.Top + FHeaderHeight);

    { draw icon }
    Inc(Rect.Left, 22);
    if not FIcon.Empty then
    begin
      Draw(2, 0, FIcon);
      Inc(Rect.Left, 16);
    end;

    { draw caption }
    SetBkMode(Handle, TRANSPARENT);
    Font.Assign(FHeaderFont);
    if FHotTrack and (dsHighlight in DrawState) and (FHitTest <> htNone) and (FHotTrackColor <> clNone) then
      Font.Color := FHotTrackColor;
    Rect.Bottom := Rect.Top + FHeaderHeight;
    Dec(Rect.Right, 3);
    {$IFDEF USEJVCL}
    DrawText(Canvas, Caption, -1, Rect, DT_SINGLELINE or DT_VCENTER or
      DT_END_ELLIPSIS or DT_NOPREFIX);
    {$ELSE}
    DrawText(Canvas.Handle, PChar(Caption), -1, Rect, DT_SINGLELINE or DT_VCENTER or
      DT_END_ELLIPSIS or DT_NOPREFIX);
    {$ENDIF USEJVCL}
    { draw visible items }
    Brush.Color := FColors.BodyColor;
    if not FCollapsed or FRolling then
      for I := 0 to FVisibleItems.Count - 1 do
        if FVisibleItems[I].Checked then
          DoDrawItem(I, [dsClicked])
        else
          DoDrawItem(I, []);
  end;
end;

procedure TJvXPCustomWinXPBar.WMAfterXPBarCollapse(var Msg: TMessage);
begin
  if Assigned(FAfterCollapsedChange) then
    FAfterCollapsedChange(Self, Msg.WParam <> 0);
  if Grouped and not FCollapsed then
    GroupMessage;
end;

procedure TJvXPCustomWinXPBar.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvXPCustomWinXPBar.SetColors(const Value: TJvXPBarColors);
begin
  FColors.Assign(Value);
end;

procedure TJvXPCustomWinXPBar.SetRollImages(const Value: TCustomImageList);
begin
  if FRollImages <> Value then
  begin
    if FRollImages <> nil then
      FRollImages.UnRegisterChanges(FRollChangeLink);
    FRollImages := Value;
    if FRollImages <> nil then
    begin
      FRollImages.FreeNotification(Self);
      FRollImages.RegisterChanges(FRollChangeLink);
    end;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.GroupMessage;
var
  Msg: TMessage;
begin
  if Parent <> nil then
  begin
    Msg.Msg := WM_XPBARAFTEREXPAND;
    Msg.WParam := Integer(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvXPCustomWinXPBar.WMAfterXPBarExpand(var Msg: TMessage);
begin
  if Grouped and (TObject(Msg.WParam) <> Self) then
    Collapsed := True;
end;

procedure TJvXPCustomWinXPBar.SetGrouped(const Value: Boolean);
begin
  if FGrouped <> Value then
  begin
    FGrouped := Value;
    if FGrouped and not Collapsed then
      Collapsed := True;
  end;
end;

procedure TJvXPCustomWinXPBar.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if ControlCount > 0 then
  begin
    Inc(Rect.Top, FHeaderHeight + 4);
    InflateRect(Rect, -4, -4);
  end;
end;

procedure TJvXPCustomWinXPBar.SetHeaderHeight(const Value: Integer);
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    ResizeToMaxHeight;
//    InternalRedraw;
  end;
end;

function TJvXPCustomWinXPBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  // draw the item hint (if available)
  if (FHoverIndex > -1) and (FVisibleItems[FHoverIndex] <> nil) then
  begin
    HintInfo.CursorRect := GetItemRect(FHoverIndex);
    with VisibleItems[FHoverIndex] do
    begin
      if Action is TCustomAction then
        HintInfo.HintStr := TCustomAction(Action).Hint
      else
        HintInfo.HintStr := VisibleItems[FHoverIndex].Hint;
    end;
  end
  else
  if (VisibleItems.Count > 0) and not Collapsed then
    HintInfo.CursorRect := GetHitTestRect(htHeader);

  if HintInfo.HintStr = '' then
    HintInfo.HintStr := Hint;
  Result := False; // use default hint window
end;

{$IFNDEF USEJVCL}
procedure TJvXPCustomWinXPBar.CMHintShow(var Msg: TCMHintShow);
begin
  Msg.Result := Ord(HintShow(Msg.HintInfo^));
end;
{$ENDIF USEJVCL}

{$IFDEF VCL}
function TJvXPBarItemActionLink.DoShowHint(var HintStr: string): Boolean;
{$ENDIF VCL}
{$IFDEF VisualCLX}
function TJvXPBarItemActionLink.DoShowHint(var HintStr: WideString): Boolean;
{$ENDIF VisualCLX}
begin
  Result := True;
  if Action is TCustomAction then
  begin
    Result := TCustomAction(Action).DoHint(HintStr);
    if Result and Application.HintShortCuts and (TCustomAction(Action).ShortCut <> scNone) then
      if HintStr <> '' then
        HintStr := Format('%s (%s)', [HintStr, ShortCutToText(TCustomAction(Action).ShortCut)]);
  end;
end;

procedure TJvXPCustomWinXPBar.InitiateAction;
var
  I: Integer;
begin
  inherited InitiateAction;
  // go through each item and update
  for I := 0 to Items.Count - 1 do
    Items[I].ActionChange(Items[I].Action, csLoading in ComponentState);
end;

{$IFDEF VCL}
procedure TJvXPCustomWinXPBar.CMDialogChar(var Msg: TCMDialogChar);
var
  I: Integer;
begin
  if CanFocus then
  begin
    if IsAccel(Msg.CharCode, Caption) then
    begin
      Collapsed := not Collapsed;
      Msg.Result := 1;
    end
    else
    if not Collapsed then
      for I := 0 to VisibleItems.Count - 1 do
        if IsAccel(Msg.CharCode, VisibleItems[I].Caption) and VisibleItems[I].Enabled then
        begin
          Msg.Result := 1;
          FHitTest := htNone;
          FHoverIndex := I;
          Click;
          Exit;
        end;
  end;
  inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvXPCustomWinXPBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
var
  I: Integer;
begin
  if CanFocus then
  begin
    if IsAccel(Key, Caption) then
    begin
      Result := True;
      FHitTest := htHeader;
      FHoverIndex := -1;
      Click;
      Exit;
    end
    else
      for I := 0 to VisibleItems.Count - 1 do
        if IsAccel(Key, VisibleItems[I].Caption) then
        begin
          Result := True;
          FHitTest := htNone;
          FHoverIndex := I;
          Click;
          Exit;
        end;
  end;
  Result := inherited WantKey(Key, Shift, KeyText);
end;
{$ENDIF VisualCLX}

class function TJvXPCustomWinXPBar.GetBarItemsClass: TJvXPBarItemsClass;
begin
  Result := TJvXPBarItems;
end;

procedure TJvXPCustomWinXPBar.DblClick;
var
  lItem: TJvXPBarItem;
begin
  if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex] <> nil) and (FVisibleItems[FHoverIndex].Enabled) then
  begin
    lItem := FVisibleItems[FHoverIndex];
    if Assigned(lItem.FOnDblClick) then
      lItem.FOnDblClick(lItem);
  end;
  inherited DblClick;
end;

procedure RoundedFrame(Canvas: TCanvas; ARect: TRect; AColor: TColor; R: Integer);
begin
  // Draw Frame with round edges
  with Canvas, ARect do
  begin
    Pen.Color := AColor;
    Dec(Right);
    Dec(Bottom);
    Polygon([Point(Left + R, Top),
      Point(Right - R, Top),
      Point(Right, Top + R),
      Point(Right, Bottom - R),
      Point(Right - R, Bottom),
      Point(Left + R, Bottom),
      Point(Left, Bottom - R),
      Point(Left, Top + R),
      Point(Left + R, Top)]);
    Inc(Right);
    Inc(Bottom);
  end;
end;

procedure TJvXPCustomWinXPBar.SetHeaderRounded(const Value: Boolean);
begin
  if FHeaderRounded <> Value then
  begin
    FHeaderRounded := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetTopSpace(const Value: Integer);
begin
  if Value <> FTopSpace then
  begin
    FTopSpace := Value;
    if FTopSpace < 0 then
      FTopSpace := 0;
    ResizeToMaxHeight;
    InternalRedraw;
  end;
end;


end.

