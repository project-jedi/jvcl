{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNavigationPane.PAS, released on 2004-03-28.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvNavigationPane;

interface
uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Controls, Graphics, Menus, ExtCtrls, ImgList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QGraphics, QMenus, QExtCtrls, QImgList, Types, Qt,
  QTypes, QWindows,
  {$ENDIF VisualCLX}
  JvTypes, JvButton, JvPageList, JvComponent, JvExExtCtrls;

const
  CM_PARENTSTYLEMANAGERCHANGE = CM_BASE + 1;
  CM_PARENTSTYLEMANAGERCHANGED = CM_BASE + 2;
  
type
  TJvCustomNavigationPane = class;
  TJvNavIconButton = class;
  TJvNavStyleLink = class;
  TJvNavPaneStyleManager = class;
  TMsgStyleManagerChange = record
    Msg: Cardinal;
    Sender:TControl;
    StyleManager:TJvNavPaneStyleManager;
    Result: Longint;
  end;

  TJvNavPanelHeader = class(TJvCustomControl)
  private
    FColorFrom: TColor;
    FColorTo: TColor;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FChangeLink: TChangeLink;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FWordWrap: boolean;
    FAlignment: TAlignment;
    FParentStyleManager: boolean;
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: boolean);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg:TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    procedure CMControlChange(var Message:TMessage); message CM_CONTROLCHANGE;
    procedure SetParentStyleManager(const Value: boolean);
  protected

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TextChanged; override;
    procedure Paint; override;
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Anchors;
    property Caption;
    property Constraints;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property DragMode;
    property OnStartDock;
    property OnEndDock;
    property OnUnDock;
    {$ENDIF VCL}
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap default False;

    property ColorFrom: TColor read FColorFrom write SetColorFrom default $D68652;
    property ColorTo: TColor read FColorTo write SetColorTo default $944110;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property Height default 27;
    property Width default 225;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvNavPanelDivider = class(TJvExSplitter)
  private
    FColorFrom: TColor;
    FColorTo: TColor;
    FFrameColor: TColor;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FAlignment: TAlignment;
    FParentStyleManager: boolean;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure Paint; override;
    procedure TextChanged; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // NB! Color is published but not used
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Align default alNone;
    property Anchors;
    property AutoSnap default False;
    property Caption;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default $FFE7CE;
    property ColorTo: TColor read FColorTo write SetColorTo default $E7A67B;
    property Constraints;
    property Cursor default crSizeNS;
    property Enabled;
    property Font;
    property FrameColor: TColor read FFrameColor write SetFrameColor default $943000;
    property Height default 19;
    property ResizeStyle default rsUpdate;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property Width default 125;
  end;

  TJvOutlookSplitter = class(TJvExSplitter)
  private
    FColorTo: TColor;
    FColorFrom: TColor;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FParentStyleManager: boolean;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure Paint; override;
    procedure EnabledChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // NB! Color is published but not used
    property Align default alBottom;
    property AutoSnap default False;
    property ResizeStyle default rsUpdate;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default $D68652;
    property ColorTo: TColor read FColorTo write SetColorTo default $944110;
    property Height default 7;
    property Cursor default crSizeNS;
    property Enabled;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
  end;

  TJvNavPanelColors = class(TPersistent)
  private
    FButtonColorTo: TColor;
    FButtonColorFrom: TColor;
    FFrameColor: TColor;
    FButtonHotColorFrom: TColor;
    FButtonHotColorTo: TColor;
    FButtonSelectedColorFrom: TColor;
    FButtonSelectedColorTo: TColor;
    FOnChange: TNotifyEvent;
    FSplitterColorFrom: TColor;
    FSplitterColorTo: TColor;
    FDividerColorTo: TColor;
    FDividerColorFrom: TColor;
    FHeaderColorFrom: TColor;
    FHeaderColorTo: TColor;
    FButtonSeparatorColor: TColor;
    procedure SetButtonColorFrom(const Value: TColor);
    procedure SetButtonColorTo(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetButtonHotColorFrom(const Value: TColor);
    procedure SetButtonHotColorTo(const Value: TColor);
    procedure SetButtonSelectedColorFrom(const Value: TColor);
    procedure SetButtonSelectedColorTo(const Value: TColor);
    procedure SetSplitterColorFrom(const Value: TColor);
    procedure SetSplitterColorTo(const Value: TColor);
    procedure SetDividerColorFrom(const Value: TColor);
    procedure SetDividerColorTo(const Value: TColor);
    procedure SetHeaderColorFrom(const Value: TColor);
    procedure SetHeaderColorTo(const Value: TColor);
    procedure SetButtonSeparatorColor(const Value: TColor);
  protected
    procedure Change;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property SplitterColorFrom: TColor read FSplitterColorFrom write SetSplitterColorFrom;
    property SplitterColorTo: TColor read FSplitterColorTo write SetSplitterColorTo;
    property DividerColorFrom: TColor read FDividerColorFrom write SetDividerColorFrom;
    property DividerColorTo: TColor read FDividerColorTo write SetDividerColorTo;
    property HeaderColorFrom: TColor read FHeaderColorFrom write SetHeaderColorFrom;
    property HeaderColorTo: TColor read FHeaderColorTo write SetHeaderColorTo;
    property ButtonColorFrom: TColor read FButtonColorFrom write SetButtonColorFrom;
    property ButtonColorTo: TColor read FButtonColorTo write SetButtonColorTo;
    property ButtonHotColorFrom: TColor read FButtonHotColorFrom write SetButtonHotColorFrom;
    property ButtonHotColorTo: TColor read FButtonHotColorTo write SetButtonHotColorTo;
    property ButtonSelectedColorFrom: TColor read FButtonSelectedColorFrom write SetButtonSelectedColorFrom;
    property ButtonSelectedColorTo: TColor read FButtonSelectedColorTo write SetButtonSelectedColorTo;
    property ButtonSeparatorColor: TColor read FButtonSeparatorColor write SetButtonSeparatorColor default clGray;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvNavPanelFonts = class(TPersistent)
  private
    FHeaderFont: TFont;
    FNavPanelFont: TFont;
    FDividerFont: TFont;
    FOnChange: TNotifyEvent;
    FNavPanelHotTrackFont: TFont;
    FNavPanelHotTrackFontOptions: TJvTrackFontOptions;
    procedure SetDividerFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetNavPanelFont(const Value: TFont);
    procedure SetNavPanelHotTrackFont(const Value: TFont);
    procedure SetNavPanelHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure Change;
    procedure DoFontChange(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
  published
    property NavPanelFont: TFont read FNavPanelFont write SetNavPanelFont;
    property NavPanelHotTrackFont: TFont read FNavPanelHotTrackFont write SetNavPanelHotTrackFont;
    property NavPanelHotTrackFontOptions: TJvTrackFontOptions read FNavPanelHotTrackFontOptions write SetNavPanelHotTrackFontOptions default DefaultTrackFontOptions;

    property DividerFont: TFont read FDividerFont write SetDividerFont;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvIconPanel = class(TJvCustomControl)
  private
    FDropButton: TJvNavIconButton;
    FColors: TJvNavPanelColors;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FOnDropDownMenu: TContextPopupEvent;
    FParentStyleManager: boolean;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    function GetDropDownMenu: TPopupMenu;
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg:TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    procedure CMControlChange(var Message:TMessage); message CM_CONTROLCHANGE;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property OnDropDownMenu: TContextPopupEvent read FOnDropDownMenu write FOnDropDownMenu;
  end;

  TJvNavIconButtonType = (nibDropDown, nibImage, nibDropArrow, nibClose);

  TJvNavIconButton = class(TJvCustomGraphicButton)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FButtonType: TJvNavIconButtonType;
    FColors: TJvNavPanelColors;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FParentStyleManager: boolean;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure DoImagesChange(Sender: TObject);
    procedure SetButtonType(const Value: TJvNavIconButtonType);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;

    property OnDropDownMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    //    property Caption;
    property Constraints;
    property Down;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property DragMode;
    property DropDownMenu;
    property GroupIndex;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property ButtonType: TJvNavIconButtonType read FButtonType write SetButtonType;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property Width default 22;
    property Height default 22;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvNavPanelToolButton = class(TJvCustomGraphicButton)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FButtonType: TJvNavIconButtonType;
    procedure DoImagesChange(Sender: TObject);
    procedure SetButtonType(const Value: TJvNavIconButtonType);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property ButtonType: TJvNavIconButtonType read FButtonType write SetButtonType;
    property Caption;
  end;

  TJvNavPanelButton = class(TJvCustomGraphicButton)
  private
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FColors: TJvNavPanelColors;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FAlignment: TAlignment;
    FWordWrap: boolean;
    FParentStyleManager: boolean;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: boolean);
    {$IFDEF VCL}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF VCL}
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure Paint; override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF VisualCLX}
    function WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; override;
    {$ENDIF VisualCLX}
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property WordWrap: boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property Caption;
    property Constraints;
    property Down;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;

    property HotTrack default True;
    property HotTrackFont;
    property HotTrackFontOptions;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Width default 125;
    property Height default 28;

    property Colors: TJvNavPanelColors read FColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvNavPanelPage = class(TJvCustomPage)
  private
    FNavPanel: TJvNavPanelButton;
    FIconButton: TJvNavIconButton;
    FOnClick: TNotifyEvent;
    FIconPanel: TJvIconPanel;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FHeader: TJvNavPanelHeader;
    FImageIndex:TImageIndex;
    FParentStyleManager: boolean;
    procedure SetCaption(const Value: TCaption);
    procedure SetIconic(const Value: Boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    function GetCaption: TCaption;
    function GetIconic: Boolean;
    function GetImageIndex: TImageIndex;
    procedure DoButtonClick(Sender: TObject);
    function GetHint: string;
    procedure SetHint(const Value: string);

    procedure SetIconPanel(const Value: TJvIconPanel);
    function GetColors: TJvNavPanelColors;
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAutoHeader(const Value: boolean);
    function GetAutoHeader: boolean;
    function GetAlignment: TAlignment;
    function GetWordWrap: boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: boolean);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg:TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    procedure CMControlChange(var Message:TMessage); message CM_CONTROLCHANGE;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure UpdatePageList;
    procedure SetParent({$IFDEF VisualCLX}const {$ENDIF}AParent: TWinControl); override;
    procedure SetPageIndex(Value: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property NavPanel: TJvNavPanelButton read FNavPanel;
    property IconButton: TJvNavIconButton read FIconButton;
    property IconPanel: TJvIconPanel read FIconPanel write SetIconPanel;
    property Colors: TJvNavPanelColors read GetColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property Header: TJvNavPanelHeader read FHeader;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property WordWrap: boolean read GetWordWrap write SetWordWrap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoHeader: boolean read GetAutoHeader write SetAutoHeader;

  published
    property Color default clWindow;
    property ParentColor default False;
    property Caption: TCaption read GetCaption write SetCaption;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEndDock;
    {$ENDIF VCL}
    property DragMode;
    property Iconic: Boolean read GetIconic write SetIconic default False;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property Hint: string read GetHint write SetHint;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnContextPopup;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
  end;

  TJvNavPaneToolPanel = class;

  TJvNavPaneToolButton = class(TCollectionItem)
  private
    FImageIndex: TImageIndex;
    FEnabled: Boolean;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TJvNavPaneToolButtons = class(TOwnedCollection)
  private
    FPanel: TJvNavPaneToolPanel;
    function GetItem(Index: Integer): TJvNavPaneToolButton;
    procedure SetItem(Index: Integer; const Value: TJvNavPaneToolButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TJvNavPaneToolPanel);
    function Add: TJvNavPaneToolButton;
    property Items[Index: Integer]: TJvNavPaneToolButton read GetItem write SetItem; default;
  end;

  TJvNavPaneToolButtonClick = procedure(Sender: TObject; Index: Integer) of object;
  TJvToolPanelHitTestInfo = (phtNowhere, phtAbove, phtBelow, phtToLeft, phtToRight, phtGrabber, phtHeader, phtClient);
  TJvToolPanelHitTestInfos = set of TJvToolPanelHitTestInfo;
  TJvNavPaneToolPanel = class(TJvCustomControl)
  private
    FStyleLink: TJvNavStyleLink;
    FChangeLink: TChangeLink;
    FStyleManager: TJvNavPaneStyleManager;
    FColorFrom: TColor;
    FColorTo: TColor;
    FButtonWidth: Integer;
    FHeaderHeight: Integer;
    FEdgeRounding: Integer;
    FButtonHeight: Integer;
    FButtonColor: TColor;
    FImages: TCustomImageList;
    FButtons: TJvNavPaneToolButtons;
    FRealButtons: TList;
    FOnButtonClick: TJvNavPaneToolButtonClick;
    FDropDown: TJvNavPanelToolButton;
    FCloseButton: TJvNavPanelToolButton;
    FOnClose: TNotifyEvent;
    FShowGrabber: Boolean;
    FOnDropDownMenu: TContextPopupEvent;
    FParentStyleManager: boolean;
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetEdgeRounding(const Value: Integer);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtons(const Value: TJvNavPaneToolButtons);
    procedure DoStyleChange(Sender: TObject);
    procedure DoImagesChange(Sender: TObject);
    procedure ButtonsChanged;
    procedure InternalButtonClick(Sender: TObject);
    function GetCloseButton: Boolean;
    function GetDropDownMenu: TPopupMenu;
    procedure SetCloseButton(const Value: Boolean);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure DoCloseClick(Sender: TObject);
    procedure SetShowGrabber(const Value: Boolean);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg:TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    procedure CMControlChange(var Message:TMessage); message CM_CONTROLCHANGE;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
    property EdgeRounding: Integer read FEdgeRounding write SetEdgeRounding default 9;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHitTestInfoAt(X, Y: Integer): TJvToolPanelHitTestInfos;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Constraints;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF VCL}
    property DragMode;

    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 185;
    property Height default 41;

    property Buttons: TJvNavPaneToolButtons read FButtons write SetButtons;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default $A6A5A6;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 30;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 21;
    property Color default clWindow;
    property CloseButton: Boolean read GetCloseButton write SetCloseButton default True;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default $FFF7CE;
    property ColorTo: TColor read FColorTo write SetColorTo default $E7A67B;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 29;
    property Images: TCustomImageList read FImages write SetImages;
    property ParentColor default False;
    property ShowGrabber: Boolean read FShowGrabber write SetShowGrabber default True;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property OnButtonClick: TJvNavPaneToolButtonClick read FOnButtonClick write FOnButtonClick;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnDropDownMenu: TContextPopupEvent read FOnDropDownMenu write FOnDropDownMenu;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvCustomNavigationPane = class(TJvCustomPageList)
  private
    FIconPanel: TJvIconPanel;
    FSplitter: TJvOutlookSplitter;
    FLargeImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FColors: TJvNavPanelColors;
    FNavPanelFont: TFont;
    FResizable: Boolean;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FNavPanelHotTrackFont: TFont;
    FNavPanelHotTrackFontOptions: TJvTrackFontOptions;
    FAutoHeaders: boolean;
    FWordWrap: boolean;
    FAlignment: TAlignment;
    FOnDropDownMenu: TContextPopupEvent;
    FParentStyleManager: boolean;
    function GetDropDownMenu: TPopupMenu;
    function GetSmallImages: TCustomImageList;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetLargeImages(const Value: TCustomImageList);
    procedure SetSmallImages(const Value: TCustomImageList);
    function GetMaximizedCount: Integer;
    procedure SetMaximizedCount(Value: Integer);
    procedure HidePanel(Index: Integer);
    procedure ShowPanel(Index: Integer);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetResizable(const Value: Boolean);
    function GetNavPage(Index: Integer): TJvNavPanelPage;
    {$IFDEF VCL}
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    procedure DoSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure SetNavPanelFont(const Value: TFont);
    procedure SetNavPanelHotTrackFont(const Value: TFont);
    procedure SetNavPanelHotTrackFontOptions(const Value: TJvTrackFontOptions);

    procedure DoNavPanelFontChange(Sender: TObject);
    function IsNavPanelFontStored: Boolean;
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetSplitterHeight(const Value: Integer);
    function GetSplitterHeight: Integer;
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAutoHeaders(const Value: boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: boolean);
    procedure ParentStyleManagerChanged(var Msg:TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg:TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    procedure CMControlChange(var Message:TMessage); message CM_CONTROLCHANGE;
    procedure SetParentStyleManager(const Value: boolean);
  protected
    procedure UpdatePages; virtual;
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
    procedure SetActivePage(Page: TJvCustomPage); override;
    procedure InsertPage(APage: TJvCustomPage); override;
    procedure RemovePage(APage: TJvCustomPage); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    function InternalGetPageClass: TJvCustomPageClass; override;
    property NavPages[Index: Integer]: TJvNavPanelPage read GetNavPage;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePositions;
  protected
    {$IFDEF VCL}
    property BorderWidth default 1;
    {$ENDIF VCL}
    property AutoHeaders: boolean read FAutoHeaders write SetAutoHeaders;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 28;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 22;
    property NavPanelFont: TFont read FNavPanelFont write SetNavPanelFont stored IsNavPanelFontStored;
    property NavPanelHotTrackFont: TFont read FNavPanelHotTrackFont write SetNavPanelHotTrackFont;
    property NavPanelHotTrackFontOptions: TJvTrackFontOptions read FNavPanelHotTrackFontOptions write SetNavPanelHotTrackFontOptions default DefaultTrackFontOptions;

    property Color default clWindow;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager:boolean read FParentStyleManager write SetParentStyleManager default True;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property MaximizedCount: Integer read GetMaximizedCount write SetMaximizedCount;
    property ParentColor default False;
    property Resizable: Boolean read FResizable write SetResizable default True;
    property SmallImages: TCustomImageList read GetSmallImages write SetSmallImages;
    property SplitterHeight: Integer read GetSplitterHeight write SetSplitterHeight default 7;
    property WordWrap: boolean read FWordWrap write SetWordWrap default False;
    property OnDropDownMenu: TContextPopupEvent read FOnDropDownMenu write FOnDropDownMenu;
  end;

  TJvNavigationPane = class(TJvCustomNavigationPane)
  public
    property NavPages;
  published
    property ActivePage;
//    property Alignment;
    property Align;
    property Anchors;
    property AutoHeaders;
    {$IFDEF VCL}
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEndDock;
    {$ENDIF VCL}
    property ButtonHeight;
    property ButtonWidth;
    property Caption;
    property Color;
    property Colors;
    property StyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager;
    property Constraints;

    property DragMode;
    property DropDownMenu;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SplitterHeight;
    property Visible;

    property LargeImages;
    property MaximizedCount;
    property NavPanelFont;
    property NavPanelHotTrackFont;
    property NavPanelHotTrackFontOptions;

    property Resizable;
    property SmallImages;
//    property WordWrap;
    property OnChange;
    property OnChanging;
    property OnDropDownMenu;
    property OnClick;
    property OnContextPopup;
    {$IFDEF VCL}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    {$ENDIF VCL}
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

  TJvNavStyleLink = class(TObject)
  private
    FSender: TObject;
    FOnChange: TNotifyEvent;
  public
    destructor Destroy; override;
    procedure Change; dynamic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Sender: TObject read FSender write FSender;
  end;

  TJvNavPanelTheme = (nptStandard, nptXPBlue, nptXPSilver, nptXPOlive, nptCustom);
  TJvNavPaneStyleManager = class(TJvComponent)
  private
    FColors: TJvNavPanelColors;
    FTheme: TJvNavPanelTheme;
    FClients: TList;
    FFonts: TJvNavPanelFonts;
    FOnThemeChange: TNotifyEvent;
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetTheme(const Value: TJvNavPanelTheme);
    procedure DoThemeChange(Sender: TObject);
    procedure SetFonts(const Value: TJvNavPanelFonts);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RegisterChanges(Value: TJvNavStyleLink);
    procedure UnRegisterChanges(Value: TJvNavStyleLink);
  published
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property Fonts: TJvNavPanelFonts read FFonts write SetFonts;
    property Theme: TJvNavPanelTheme read FTheme write SetTheme; // no default
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

{$IFDEF COMPILER5}
type
  TCustomImageListEx = class(TCustomImageList)
  public
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      Enabled: Boolean); overload;
  end;
{$ELSE}
type
  TCustomImageListEx = TCustomImageList;
{$ENDIF COMPILER5}

implementation
uses
  {$IFDEF COMPILER5}
  CommCtrl,
  {$ENDIF COMPILER5}
  {$IFDEF VCL}
  Forms, ActnList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms, QActnList,
  {$ENDIF VisualCLX}
  JvJVCLUtils, JvJCLUtils;

const
  cNavPanelButtonGroupIndex = 113;

procedure InternalStyleManagerChanged(AControl:TWinControl; AStyleManager:TJvNavPaneStyleManager);
var
  Msg:TMsgStyleManagerChange;
  {$IFDEF VisualCLX}
  I:Integer;
  {$ENDIF}
begin
  Msg.Msg := CM_PARENTSTYLEMANAGERCHANGED;
  Msg.Sender := AControl;
  Msg.StyleManager := AStyleManager;
  Msg.Result := 0;
  AControl.Broadcast(Msg);
  {$IFDEF VisualCLX}
  with Msg do
    for i := 0 to AControl.ControlCount - 1 do
      if AControl.Controls[i].Perform(Msg, integer(Sender), integer(StyleManager)) <> 0 then Exit;
  {$ENDIF}
end;

{$IFDEF COMPILER5}
procedure TCustomImageListEx.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean);
const
  DrawingStyles: array[TDrawingStyle] of Longint = (ILD_FOCUS,
    ILD_SELECTED, ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint = (0, ILD_MASK);
begin
  if HandleAllocated then
    DoDraw(Index, Canvas, X, Y, DrawingStyles[ADrawingStyle] or
      Images[AImageType], Enabled);
end;
{$ENDIF COMPILER5}

type
  TObjectList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

{ TObjectList }

procedure TObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

{ TJvIconPanel }

constructor TJvIconPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  Height := 28;
  FDropButton := TJvNavIconButton.Create(Self);
  FDropButton.Visible := False;
  FDropButton.ButtonType := nibDropDown;
  FDropButton.GroupIndex := 0;
  FDropButton.Width := 22;
  FDropButton.Left := Width + 10;
  FDropButton.Align := alRight;
  FDropButton.Parent := Self;
  FDropButton.OnDropDownMenu := DoDropDownMenu;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  FParentStyleManager := True;
end;

destructor TJvIconPanel.Destroy;
begin
  FStyleLink.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure TJvIconPanel.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvIconPanel.DoStyleChange(Sender: TObject);
begin
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
  Font := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelFont;
end;

function TJvIconPanel.GetDropDownMenu: TPopupMenu;
begin
  Result := FDropButton.DropDownMenu;
end;

procedure TJvIconPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvIconPanel.Paint;
begin
  GradientFillRect(Canvas, ClientRect, Colors.ButtonColorFrom, Colors.ButtonColorTo, fdTopToBottom, 32);
  Canvas.Pen.Color := Colors.FrameColor;
  if Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width + 1, 0);
  end
  else
  begin
    Canvas.MoveTo(0, ClientHeight - 1);
    Canvas.LineTo(Width + 1, ClientHeight - 1);
  end;
end;

procedure TJvIconPanel.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
  FDropButton.Colors := Value;
end;

procedure TJvIconPanel.SetStyleManager(const Value: TJvNavPaneStyleManager);
//var
//  i: Integer;
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      Colors := FStyleManager.Colors;
    end;
  end;
//  FDropButton.StyleManager := Value;
  InternalStyleManagerChanged(Self, Value);
  // TODO: should this be removed?
//  for i := 0 to ControlCount - 1 do
//    if Controls[i] is TJvNavIconButton then
//      TJvNavIconButton(Controls[i]).StyleManager := Value;
end;

procedure TJvIconPanel.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropButton.DropDownMenu := Value;
  FDropButton.Visible := Value <> nil;
end;

{$IFDEF VCL}

procedure TJvIconPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvIconPanel.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF VisualCLX}

procedure TJvIconPanel.DoDropDownMenu(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
  if Assigned(FOnDropDownMenu) then
    FOnDropDownMenu(Self, MousePos, Handled);
end;

procedure TJvIconPanel.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
    InternalStyleManagerChanged(Self, Msg.StyleManager);
  end;
end;

procedure TJvIconPanel.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

procedure TJvIconPanel.CMControlChange(var Message: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvIconPanel.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStylemanagerChanged(Self, StyleManager);
end;

{ TJvCustomNavigationPane }

constructor TJvCustomNavigationPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FButtonHeight := 28;
  FButtonWidth := 22;
  {$IFDEF VCL}
  BorderWidth := 1;
  {$ENDIF VCL}
  ParentColor := False;
  Color := clWindow;
  ControlStyle := ControlStyle + [csOpaque];
  FResizable := True;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  FIconPanel := TJvIconPanel.Create(Self);
  FIconPanel.Parent := Self;
  FIconPanel.Align := alBottom;
  FIconPanel.OnDropDownMenu := DoDropDownMenu;
  FNavPanelFont := TFont.Create;
  FNavPanelHotTrackFont := TFont.Create;
  {$IFDEF VCL}
  FNavPanelFont.Assign(Screen.IconFont);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FNavPanelFont.Assign(Application.Font);
  {$ENDIF VisualCLX}
  FNavPanelFont.Style := [fsBold];
  FNavPanelFont.OnChange := DoNavPanelFontChange;
  FNavPanelHotTrackFont.Assign(FNavPanelFont);
  FNavPanelHotTrackFont.OnChange := DoNavPanelFontChange;
  FNavPanelHotTrackFontOptions := DefaultTrackFontOptions;
  FSplitter := TJvOutlookSplitter.Create(Self);
  with FSplitter do
  begin
    ResizeStyle := rsNone;
    MinSize := 1;
    OnCanResize := DoSplitterCanResize;
    Parent := Self;
  end;
  FParentStyleManager := True;
end;

destructor TJvCustomNavigationPane.Destroy;
begin
  FStyleLink.Free;
  FColors.Free;
  FNavPanelFont.Free;
  inherited Destroy;
end;

procedure TJvCustomNavigationPane.DoSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
var
  ACount: Integer;
begin
  ACount := MaximizedCount;
  if NewSize < ButtonHeight div 2 then
    MaximizedCount := ACount - 1
  else if NewSize > ButtonHeight + ButtonHeight div 2 then
    MaximizedCount := ACount + 1;
  NewSize := 0;
  Accept := False;
end;

function TJvCustomNavigationPane.GetDropDownMenu: TPopupMenu;
begin
  if FIconPanel <> nil then
    Result := FIconPanel.DropDownMenu
  else
    Result := nil;
end;

function TJvCustomNavigationPane.GetSmallImages: TCustomImageList;
begin
  Result := FSmallImages;
end;

function TJvCustomNavigationPane.GetMaximizedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to PageCount - 1 do
    if not NavPages[i].Iconic then
      Inc(Result);
end;

procedure TJvCustomNavigationPane.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = LargeImages then
      LargeImages := nil;
    if AComponent = SmallImages then
      SmallImages := nil;
    if (AComponent = StyleManager) then
      StyleManager := nil;
  end;
end;

procedure TJvCustomNavigationPane.SetDropDownMenu(const Value: TPopupMenu);
begin
  if FIconPanel <> nil then
    FIconPanel.DropDownMenu := Value;
end;

procedure TJvCustomNavigationPane.SetLargeImages(const Value: TCustomImageList);
begin
  if FLargeImages <> Value then
  begin
    FLargeImages := Value;
    UpdatePages;
  end;
end;

procedure TJvCustomNavigationPane.SetSmallImages(const Value: TCustomImageList);
begin
  if FSmallImages <> Value then
  begin
    FSmallImages := Value;
    UpdatePages;
  end;
end;

procedure TJvCustomNavigationPane.HidePanel(Index: Integer);
begin
  if (Index >= 0) and (Index < PageCount) then // don't hide the first panel
    NavPages[Index].Iconic := True;
end;

procedure TJvCustomNavigationPane.ShowPanel(Index: Integer);
begin
  if (Index >= 0) and (Index < PageCount) then
    NavPages[Index].Iconic := False;
end;

procedure TJvCustomNavigationPane.SetMaximizedCount(Value: Integer);
var
  i, ACount: Integer;
begin
  ACount := MaximizedCount;
  if (Value < 0) then Value := 0;
  if (Value > PageCount) then Value := PageCount;
  if Value = MaximizedCount then Exit;
  while ACount > Value do
  begin
    HidePanel(ACount - 1);
    Dec(ACount);
  end;
  if Value > ACount then
    for i := Value downto ACount do
      ShowPanel(i - 1);
  UpdatePositions;
end;

{$IFDEF VCL}

procedure TJvCustomNavigationPane.WMNCPaint(var Message: TWMNCPaint);
var
  AColor: TColor;
begin
  AColor := Color;
  try
    Color := Colors.FrameColor;
    inherited;
  finally
    Color := AColor;
  end;
end;
{$ENDIF VCL}

procedure TJvCustomNavigationPane.UpdatePositions;
var
  i, X, Y: Integer;
begin
  if (csDestroying in ComponentState) or (FIconPanel = nil) then Exit;
  DisableAlign;
  FIconPanel.DisableAlign;
  try
    Y := 0;
    X := 0;
    FSplitter.Top := Y;
    FIconPanel.FDropButton.Left := Width;
    FIconPanel.Top := Height - FIconPanel.Height;
    Inc(Y, FSplitter.Height);
    for i := 0 to PageCount - 1 do
    begin
      if NavPages[i].NavPanel = nil then Exit;
      if NavPages[i].Iconic then
      begin
        NavPages[i].IconButton.Left := X;
        Inc(X, NavPages[i].IconButton.Width);
      end
      else
      begin
        NavPages[i].NavPanel.Top := Y;
        Inc(Y, NavPages[i].NavPanel.Height);
      end;
      NavPages[i].Invalidate;
    end;
  finally
    EnableAlign;
    FIconPanel.EnableAlign;
  end;
  Invalidate;
end;

procedure TJvCustomNavigationPane.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvCustomNavigationPane.DoColorsChange(Sender: TObject);
begin
  if FIconPanel <> nil then
    TJvIconPanel(FIconPanel).Colors := Colors;
  UpdatePages;
  FSplitter.ColorFrom := Colors.SplitterColorFrom;
  FSplitter.ColorTo := Colors.SplitterColorTo;
  Invalidate;
end;

procedure TJvCustomNavigationPane.Loaded;
begin
  inherited;
  UpdatePositions;
end;

procedure TJvCustomNavigationPane.SetResizable(const Value: Boolean);
begin
  if FResizable <> Value then
  begin
    FResizable := Value;
    FSplitter.Enabled := FResizable;
  end;
end;

function TJvCustomNavigationPane.InternalGetPageClass: TJvCustomPageClass;
begin
  Result := TJvNavPanelPage;
end;

function TJvCustomNavigationPane.GetNavPage(Index: Integer): TJvNavPanelPage;
begin
  Result := TJvNavPanelPage(Pages[Index]);
end;

procedure TJvCustomNavigationPane.InsertPage(APage: TJvCustomPage);
begin
  inherited InsertPage(APage);
  if APage <> nil then
  begin
    TJvNavPanelPage(APage).Top := FIconPanel.Top;
    if ActivePage = nil then
      ActivePage := APage;
  end;
  UpdatePositions;
end;

procedure TJvCustomNavigationPane.SetActivePage(Page: TJvCustomPage);
begin
  inherited;
  if (ActivePage <> nil) then
  begin
    TJvNavPanelPage(ActivePage).NavPanel.Down := True;
    TJvNavPanelPage(ActivePage).IconButton.Down := True;
    TJvNavPanelPage(ActivePage).NavPanel.Invalidate;
    TJvNavPanelPage(ActivePage).IconButton.Invalidate;
    ActivePage.Invalidate;
  end;
end;

{$IFDEF VCL}

procedure TJvCustomNavigationPane.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if ActivePage = nil then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  Message.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvCustomNavigationPane.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF VisualCLX}

procedure TJvCustomNavigationPane.SetNavPanelFont(const Value: TFont);
begin
  FNavPanelFont.Assign(Value);
end;

procedure TJvCustomNavigationPane.SetNavPanelHotTrackFont(const Value: TFont);
begin
  FNavPanelHotTrackFont.Assign(Value);
end;

procedure TJvCustomNavigationPane.SetNavPanelHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FNavPanelHotTrackFontOptions <> Value then
  begin
    FNavPanelHotTrackFontOptions := Value;
    UpdatePages;
  end;
end;

function TJvCustomNavigationPane.IsNavPanelFontStored: Boolean;
var
  F: TFont;
begin
  {$IFDEF VCL}
  F := Screen.IconFont;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  F := Application.Font;
  {$ENDIF VisualCLX}
  with FNavPanelFont do
    Result := (Name <> F.Name) or (Size <> F.Size) or (Style <> [fsBold])
      or (Color <> F.Color) or (Pitch <> F.Pitch) or (Charset <> F.CharSet);
end;

procedure TJvCustomNavigationPane.DoNavPanelFontChange(Sender: TObject);
begin
  UpdatePages;
end;

procedure TJvCustomNavigationPane.RemovePage(APage: TJvCustomPage);
begin
  inherited RemovePage(APage);
  Invalidate;
end;

procedure TJvCustomNavigationPane.SetButtonHeight(const Value: Integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    UpdatePages;
    FIconPanel.Height := FButtonHeight;
  end;
end;

procedure TJvCustomNavigationPane.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    UpdatePages;
    FIconPanel.FDropButton.Width := FButtonWidth;
  end;
end;

procedure TJvCustomNavigationPane.SetSplitterHeight(const Value: Integer);
begin
  if FSplitter.Height <> Value then
    FSplitter.Height := Value;
end;

function TJvCustomNavigationPane.GetSplitterHeight: Integer;
begin
  Result := FSplitter.Height;
end;

procedure TJvCustomNavigationPane.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      Colors := FStyleManager.Colors;
    end;
//    FSplitter.StyleManager := Value;
    InternalStyleManagerChanged(Self, Value);
  end;
end;

procedure TJvCustomNavigationPane.DoStyleChange(Sender: TObject);
begin
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
  NavPanelFont := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelFont;
  NavPanelHotTrackFont := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFont;
  NavPanelHotTrackFontOptions := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFontOptions;
end;

procedure TJvCustomNavigationPane.SetAutoHeaders(const Value: boolean);
begin
  if FAutoHeaders <> Value then
  begin
    FAutoHeaders := Value;
    UpdatePages;
  end;
end;

procedure TJvCustomNavigationPane.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    UpdatePages;
  end;
end;

procedure TJvCustomNavigationPane.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    UpdatePages;
  end;
end;

procedure TJvCustomNavigationPane.UpdatePages;
var
  I: Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    NavPages[i].AutoHeader := AutoHeaders;
    NavPages[i].NavPanel.Height := ButtonHeight;
    NavPages[i].IconButton.Width := ButtonWidth;
    NavPages[i].NavPanel.Colors := Colors;
    NavPages[i].IconButton.Colors := Colors;
    NavPages[i].NavPanel.HotTrackFontOptions := NavPanelHotTrackFontOptions;
    NavPages[i].NavPanel.Font := FNavPanelFont;
    NavPages[i].NavPanel.HotTrackFont := FNavPanelHotTrackFont;

    NavPages[i].WordWrap := WordWrap;
    NavPages[i].Alignment := Alignment;
    NavPages[i].NavPanel.Images := LargeImages;
    if AutoHeaders then
      NavPages[i].Header.Images := LargeImages;
    NavPages[i].IconButton.Images := SmallImages;
//    NavPages[i].StyleManager := StyleManager;
  end;
end;

procedure TJvCustomNavigationPane.DoDropDownMenu(Sender: TObject;
  MousePos: TPoint; var Handled: boolean);
begin
  if Assigned(FOnDropDownMenu) then
    FOnDropDownMenu(Self, MousePos, Handled);
end;

procedure TJvCustomNavigationPane.ParentStyleManagerChanged(
  var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
    InternalStyleManagerChanged(Self, Msg.StyleManager);
  end;
end;

procedure TJvCustomNavigationPane.SetParentStyleManager(
  const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

procedure TJvCustomNavigationPane.CMControlChange(var Message: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvCustomNavigationPane.ParentStyleManagerChange(
  var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

{ TJvNavIconButton }

constructor TJvNavIconButton.Create(AOwner: TComponent);
begin
  inherited;
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  Width := 22;
  Height := 22;
  {$IFDEF VCL}
  Font := Screen.IconFont;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Font := Application.Font;
  {$ENDIF VisualCLX}
  Font.Style := [fsBold];
  FParentStyleManager := True;
end;

destructor TJvNavIconButton.Destroy;
begin
  FStyleLink.Free;
  FChangeLink.Free;
  FColors.Free;
  inherited;
end;

procedure TJvNavIconButton.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavIconButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavIconButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil
    else if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavIconButton.Paint;
var
  Rect: TRect;
  P: TPoint;
  i: Integer;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    Brush.Style := bsClear;
    InflateRect(Rect, 0, -1);
    if (bsMouseInside in MouseStates) then
    begin
      if (bsMouseDown in MouseStates) then
        GradientFillRect(Canvas, Rect, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32)
      else
        GradientFillRect(Canvas, Rect, Colors.ButtonHotColorFrom, Colors.ButtonHotColorTo, fdTopToBottom, 32)
    end
    else if Down then
      GradientFillRect(Canvas, Rect, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32);
    case ButtonType of
      nibDropDown:
        begin // area should be 7x12
          InflateRect(Rect, -((Rect.Right - Rect.Left) - 7) div 2, -((Rect.Bottom - Rect.Top) - 12) div 2);
          if bsMouseDown in MouseStates then
            OffsetRect(Rect, 1,1);
          Canvas.Pen.Color := clBlack;
          P.X := Rect.Left;
          P.Y := Rect.Top;
          // chevron, upper
          for i := 0 to 2 do
          begin
            Canvas.MoveTo(P.X, P.Y);
            Canvas.LineTo(P.X + 2, P.Y);

            Canvas.MoveTo(P.X + 4, P.Y);
            Canvas.LineTo(P.X + 6, P.Y);
            Inc(P.X);
            Inc(P.Y);
          end;
          // chevron, lower
          Dec(P.X);
          Dec(P.Y);
          for i := 0 to 2 do
          begin
            Canvas.MoveTo(P.X, P.Y);
            Canvas.LineTo(P.X + 2, P.Y);

            Canvas.MoveTo(P.X + 4, P.Y);
            Canvas.LineTo(P.X + 6, P.Y);
            Dec(P.X);
            Inc(P.Y);
          end;

          // drop arrow
          Inc(P.X, 1);
          Inc(P.Y, 3);
          for i := 0 to 3 do
          begin
            Canvas.MoveTo(P.X + i, P.Y + i);
            Canvas.LineTo(P.X + 7 - i, P.Y + i);
          end;
        end;
      nibImage:
        begin
          if (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
            // draw image only
            TCustomImageListEx(Images).Draw(Canvas,
              (Width - Images.Width) div 2 + Ord(bsMouseDown in MouseStates),
              (Height - Images.Height) div 2 + Ord(bsMouseDown in MouseStates),
              ImageIndex, {$IFDEF VisualCLX}itImage, {$ENDIF}Enabled);
        end;
      nibDropArrow:
        begin
          // area should be 9 x 5, centered
          P.X := Rect.Left + (RectWidth(Rect) - 9) div 2 + Ord(bsMouseDown in MouseStates);
          P.Y := Rect.Top + (RectHeight(Rect) - 5) div 2 + Ord(bsMouseDown in MouseStates);
          Canvas.Pen.Color := clBlack;
          for i := 0 to 4 do
          begin
            Canvas.MoveTo(P.X + i, P.Y + i);
            Canvas.LineTo(P.X + 9 - i, P.Y + i);
          end;
        end;
      nibClose:
        begin
          // area should be 8 x 8, centered
          P.X := (RectWidth(ClientRect) - 8) div 2 + Ord(bsMouseDown in MouseStates);
          P.Y := (RectHeight(ClientRect) - 8) div 2 + Ord(bsMouseDown in MouseStates);
          Canvas.Pen.Color := clBlack;
          for i := 0 to 7 do
          begin
            Canvas.MoveTo(P.X + I, P.Y + I);
            Canvas.LineTo(P.X + I + 2, P.Y + I);
          end;
          Inc(P.X, 7);
          for i := 0 to 7 do
          begin
            Canvas.MoveTo(P.X - I, P.Y + I);
            Canvas.LineTo(P.X - I + 2, P.Y + I);
          end;
        end;

    end;
    if csDesigning in ComponentState then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psDot;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(ClientRect);
    end;
  end;
end;

procedure TJvNavIconButton.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavIconButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavIconButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChangeLink);
    end;
    Invalidate;
  end;
end;

procedure TJvNavIconButton.SetButtonType(const Value: TJvNavIconButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Invalidate;
  end;
end;

procedure TJvNavIconButton.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      Colors := FStyleManager.Colors;
    end;
  end;
end;

procedure TJvNavIconButton.DoStyleChange(Sender: TObject);
begin
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
  Font := (Sender as TJvNavPaneStyleManager).Fonts.DividerFont;
end;

procedure TJvNavIconButton.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
  end;
end;

procedure TJvNavIconButton.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

{ TJvNavPanelButton }

procedure TJvNavPanelButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
        Self.Caption := Caption;
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

constructor TJvNavPanelButton.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taLeftJustify;

  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  ControlStyle := ControlStyle + [csOpaque, csDisplayDragImage];
  Flat := True;
  HotTrack := True;
  Height := 28;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  {$IFDEF VCL}
  Font := Screen.IconFont;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Font := Application.Font;
  {$ENDIF VisualCLX}
  Font.Style := [fsBold];
  HotTrackFont := Font;
  HotTrackFont.Style := [fsBold];
  Width := 125;
  Height := 28;
  FParentStyleManager := True;
end;

destructor TJvNavPanelButton.Destroy;
begin
  FStyleLink.Free;
  FColors.Free;
  inherited;
end;

procedure TJvNavPanelButton.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelButton.DoStyleChange(Sender: TObject);
begin
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
  Font := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelFont;
  HotTrackFont := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFont;
  HotTrackFontOptions := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFontOptions;
end;

procedure TJvNavPanelButton.FontChanged;
begin
  inherited;
  Invalidate;
end;

procedure TJvNavPanelButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavPanelButton.Paint;
//const
//  cAlignment:array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
//  cWordWrap: array[boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  R: TRect;
  X, Y: Integer;
  function IsValidImage: boolean;
  begin
    Result := Assigned(Images) and (ImageIndex >= 0);
  end;
begin
  R := ClientRect;
  if HotTrack and (bsMouseInside in MouseStates) then
  begin
    if (bsMouseDown in MouseStates) then
      GradientFillRect(Canvas, R, Colors.ButtonSelectedColorTo, Colors.ButtonSelectedColorFrom, fdTopToBottom, 32)
    else
      GradientFillRect(Canvas, R, Colors.ButtonHotColorFrom, Colors.ButtonHotColorTo, fdTopToBottom, 32);
  end
  else if Down then
    GradientFillRect(Canvas, R, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32)
  else if (bsMouseDown in MouseStates) then
    GradientFillRect(Canvas, R, Colors.ButtonSelectedColorTo, Colors.ButtonSelectedColorFrom, fdTopToBottom, 32)
  else
    GradientFillRect(Canvas, ClientRect, Colors.ButtonColorFrom, Colors.ButtonColorTo, fdTopToBottom, 32);
  InflateRect(R, -4, -4);
  if IsValidImage then
  begin
    Y := (Height - Images.Height) div 2;
    X := 4;
    Images.Draw(Canvas, X, Y, ImageIndex);
    Inc(R.Left, Images.Width + 4);
  end;
  if Caption <> '' then
  begin
    if HotTrack and (bsMouseInside in MouseStates) and not (bsMouseDown in MouseStates) then
      Canvas.Font := HotTrackFont
    else
      Canvas.Font := Font;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas, Caption, Length(Caption), R, DT_SINGLELINE or DT_VCENTER);
  end;
  if Colors.ButtonSeparatorColor <> clNone then
  begin
    Canvas.Pen.Color := Colors.ButtonSeparatorColor;
    if Align = alBottom then
    begin
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(Width + 1, 0);
    end
    else
    begin
      Canvas.MoveTo(0, ClientHeight - 1);
      Canvas.LineTo(Width + 1, ClientHeight - 1);
    end;
  end;
end;

procedure TJvNavPanelButton.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavPanelButton.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      Colors := FStyleManager.Colors;
    end;
  end;
end;

procedure TJvNavPanelButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TJvNavPanelButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

{$IFDEF VCL}

procedure TJvNavPanelButton.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) then
  begin
    Message.Result := 1;
    Click;
  end
  else
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvNavPanelButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and not (ssCtrl in Shift);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift, KeyText);
end;
{$ENDIF VisualCLX}

procedure TJvNavPanelButton.ParentStyleManagerChanged(
  var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
  end;
end;

procedure TJvNavPanelButton.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

{ TJvNavPanelColors }

procedure TJvNavPanelColors.Assign(Source: TPersistent);
begin
  if (Source is TJvNavPanelColors) and (Source <> Self) then
  begin
    FButtonColorFrom := TJvNavPanelColors(Source).ButtonColorFrom;
    FButtonColorTo := TJvNavPanelColors(Source).ButtonColorTo;
    FButtonHotColorFrom := TJvNavPanelColors(Source).ButtonHotColorFrom;
    FButtonHotColorTo := TJvNavPanelColors(Source).ButtonHotColorTo;
    FButtonSelectedColorFrom := TJvNavPanelColors(Source).ButtonSelectedColorFrom;
    FButtonSelectedColorTo := TJvNavPanelColors(Source).ButtonSelectedColorTo;
    FFrameColor := TJvNavPanelColors(Source).FrameColor;
    FHeaderColorFrom := TJvNavPanelColors(Source).HeaderColorFrom;
    FHeaderColorTo := TJvNavPanelColors(Source).HeaderColorTo;
    FDividerColorFrom := TJvNavPanelColors(Source).DividerColorFrom;
    FDividerColorTo := TJvNavPanelColors(Source).DividerColorTo;
    FSplitterColorFrom := TJvNavPanelColors(Source).SplitterColorFrom;
    FSplitterColorTo := TJvNavPanelColors(Source).SplitterColorTo;
    FButtonSeparatorColor := TJvNavPanelColors(Source).ButtonSeparatorColor;
    Change;
    Exit;
  end;
  inherited;
end;

procedure TJvNavPanelColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TJvNavPanelColors.Create;
begin
  inherited Create;
  FSplitterColorFrom := $D68652;
  FSplitterColorTo := $944110;
  FButtonColorFrom := $FFE7CE;
  FButtonColorTo := $E7A67B;
  FFrameColor := $943000;
  FButtonHotColorFrom := $8CE7FF;
  FButtonHotColorTo := $1096EF;
  FButtonSelectedColorFrom := $0053DDFF;
  FButtonSelectedColorTo := $000D7BC6;
  FDividerColorFrom := $FFE7CE;
  FDividerColorTo := $E7A67B;
  FHeaderColorFrom := $D68652;
  FHeaderColorTo := $944110;
  FButtonSeparatorColor := clGray;
end;

procedure TJvNavPanelColors.SetButtonColorFrom(const Value: TColor);
begin
  if FButtonColorFrom <> Value then
  begin
    FButtonColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonColorTo(const Value: TColor);
begin
  if FButtonColorTo <> Value then
  begin
    FButtonColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetDividerColorFrom(const Value: TColor);
begin
  if FDividerColorFrom <> Value then
  begin
    FDividerColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetDividerColorTo(const Value: TColor);
begin
  if FDividerColorTo <> Value then
  begin
    FDividerColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetHeaderColorFrom(const Value: TColor);
begin
  if FHeaderColorFrom <> Value then
  begin
    FHeaderColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetHeaderColorTo(const Value: TColor);
begin
  if FHeaderColorTo <> Value then
  begin
    FHeaderColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonHotColorFrom(const Value: TColor);
begin
  if FButtonHotColorFrom <> Value then
  begin
    FButtonHotColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonHotColorTo(const Value: TColor);
begin
  if FButtonHotColorTo <> Value then
  begin
    FButtonHotColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonSelectedColorFrom(const Value: TColor);
begin
  if FButtonSelectedColorFrom <> Value then
  begin
    FButtonSelectedColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonSelectedColorTo(const Value: TColor);
begin
  if FButtonSelectedColorTo <> Value then
  begin
    FButtonSelectedColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSplitterColorFrom(const Value: TColor);
begin
  if FSplitterColorFrom <> Value then
  begin
    FSplitterColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSplitterColorTo(const Value: TColor);
begin
  if FSplitterColorTo <> Value then
  begin
    FSplitterColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetButtonSeparatorColor(const Value: TColor);
begin
  if FButtonSeparatorColor <> Value then
  begin
    FButtonSeparatorColor := Value;
    Change;
  end;
end;

{ TJvNavPanelFonts }

procedure TJvNavPanelFonts.Assign(Source: TPersistent);
begin
  if (Source is TJvNavPanelFonts) and (Source <> Self) then
  begin
    NavPanelFont := TJvNavPanelFonts(Source).NavPanelFont;
    DividerFont := TJvNavPanelFonts(Source).DividerFont;
    HeaderFont := TJvNavPanelFonts(Source).HeaderFont;
  end;
end;

procedure TJvNavPanelFonts.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TJvNavPanelFonts.Create;
begin
  inherited Create;
  FDividerFont := TFont.Create;
  FNavPanelFont := TFont.Create;
  FNavPanelHotTrackFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FHeaderFont.Name := 'Arial';
  FHeaderFont.Size := 12;
  FHeaderFont.Style := [fsBold];
  FHeaderFont.Color := clWhite;
  FHeaderFont.OnChange := DoFontChange;

  {$IFDEF VCL}
  FDividerFont.Assign(Screen.IconFont);
  FNavPanelFont.Assign(Screen.IconFont);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FDividerFont.Assign(Application.Font);
  FNavPanelFont.Assign(Application.Font);
  {$ENDIF VisualCLX}
  FNavPanelFont.Style := [fsBold];
  FNavPanelHotTrackFont.Assign(FNavPanelFont);
  FNavPanelHotTrackFontOptions := DefaultTrackFontOptions;

  FDividerFont.OnChange := DoFontChange;
  FNavPanelFont.OnChange := DoFontChange;
  FNavPanelHotTrackFont.OnChange := DoFontChange;
end;

destructor TJvNavPanelFonts.Destroy;
begin
  FDividerFont.Free;
  FHeaderFont.Free;
  FNavPanelFont.Free;
  FNavPanelHotTrackFont.Free;
  inherited;
end;

procedure TJvNavPanelFonts.DoFontChange(Sender: TObject);
begin
  Change;
end;

procedure TJvNavPanelFonts.SetDividerFont(const Value: TFont);
begin
  FDividerFont.Assign(Value);
end;

procedure TJvNavPanelFonts.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TJvNavPanelFonts.SetNavPanelFont(const Value: TFont);
begin
  FNavPanelFont.Assign(Value);
end;

procedure TJvNavPanelFonts.SetNavPanelHotTrackFont(const Value: TFont);
begin
  FNavPanelHotTrackFont.Assign(Value);
end;

procedure TJvNavPanelFonts.SetNavPanelHotTrackFontOptions(
  const Value: TJvTrackFontOptions);
begin
  if FNavPanelHotTrackFontOptions <> Value then
  begin
    FNavPanelHotTrackFontOptions := Value;
    Change;
  end;
end;

{ TJvNavPanelPage }

constructor TJvNavPanelPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clWindow;
  ControlStyle := ControlStyle + [csDisplayDragImage];
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;

  FNavPanel := TJvNavPanelButton.Create(Self);
  FNavPanel.Visible := True;
  FNavPanel.Align := alBottom;
  FNavPanel.GroupIndex := cNavPanelButtonGroupIndex; // use a silly number that no one else is probable to use
  FNavPanel.AllowAllUp := False;

  FIconButton := TJvNavIconButton.Create(Self);
  FIconButton.ButtonType := nibImage;
  FIconButton.Visible := False;
  FIconButton.Align := alRight;
  FIconButton.Width := 0;
  FIconButton.GroupIndex := cNavPanelButtonGroupIndex;
  FIconButton.AllowAllUp := False;

  FNavPanel.OnClick := DoButtonClick;
  FIconButton.OnClick := DoButtonClick;

  ImageIndex := -1;
  FParentStyleManager := True;
end;

destructor TJvNavPanelPage.Destroy;
begin
  FStyleLink.Free;
  inherited;
end;

procedure TJvNavPanelPage.DoButtonClick(Sender: TObject);
begin
  if not NavPanel.Down then
  begin
    if Parent <> nil then
      TJvCustomNavigationPane(Parent).ActivePage := Self; // this sets "Down" as well
    if Assigned(FOnClick) then FOnClick(Self);
  end;
end;

procedure TJvNavPanelPage.DoStyleChange(Sender: TObject);
begin
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
  NavPanel.Font := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelFont;
  NavPanel.HotTrackFont := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFont;
  NavPanel.HotTrackFontOptions := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelHotTrackFontOptions;
end;

function TJvNavPanelPage.GetCaption: TCaption;
begin
  if NavPanel = nil then
    Result := ''
  else
    Result := NavPanel.Caption;
end;

function TJvNavPanelPage.GetColors: TJvNavPanelColors;
begin
  Result := NavPanel.Colors;
end;

function TJvNavPanelPage.GetHint: string;
begin
  if NavPanel = nil then
    Result := ''
  else
    Result := NavPanel.Hint;
end;

function TJvNavPanelPage.GetIconic: Boolean;
begin
  if NavPanel = nil then
    Result := False
  else
    Result := not NavPanel.Visible;
end;

function TJvNavPanelPage.GetImageIndex: TImageIndex;
begin
  if NavPanel = nil then
    Result := FImageIndex
  else
    Result := NavPanel.ImageIndex;
end;

procedure TJvNavPanelPage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (AComponent = IconPanel) then
      IconPanel := nil
    else if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavPanelPage.SetCaption(const Value: TCaption);
begin
  if NavPanel <> nil then
    NavPanel.Caption := Value;
  if AutoHeader then
    Header.Caption := StripHotKey(Value);
end;

procedure TJvNavPanelPage.SetColors(const Value: TJvNavPanelColors);
begin
  NavPanel.Colors := Value;
  IconButton.Colors := Value;
  if AutoHeader then
  begin
    Header.ColorFrom := Value.HeaderColorFrom;
    Header.ColorTo := Value.HeaderColorTo;
  end;
end;

procedure TJvNavPanelPage.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      Colors := FStyleManager.Colors;
    end;
  end;
//  FNavPanel.StyleManager := Value;
//  FIconButton.StyleManager := Value;
//  if AutoHeader then
//    Header.StyleManager := Value;
end;

procedure TJvNavPanelPage.SetHint(const Value: string);
begin
  NavPanel.Hint := Value;
  IconButton.Hint := Value;
end;

procedure TJvNavPanelPage.SetIconic(const Value: Boolean);
begin
  NavPanel.Visible := not Value;
  IconButton.Visible := Value;
  NavPanel.Height := TJvCustomNavigationPane(Parent).ButtonHeight * Ord(NavPanel.Visible);
  IconButton.Width := TJvCustomNavigationPane(Parent).ButtonWidth * Ord(IconButton.Visible);
  UpdatePageList;
end;

procedure TJvNavPanelPage.SetIconPanel(const Value: TJvIconPanel);
begin
  if (FIconPanel <> Value) and not (csDestroying in ComponentState) then
  begin
    FIconPanel := Value;
    if IconButton <> nil then
    begin
      if FIconPanel <> nil then
      begin
        IconButton.Parent := FIconPanel;
        FIconPanel.FreeNotification(Self);
      end
      else
        IconButton.Parent := nil;
    end;
  end;
end;

procedure TJvNavPanelPage.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  NavPanel.ImageIndex := Value;
  IconButton.ImageIndex := Value;
  if AutoHeader then
    Header.ImageIndex := Value;
end;

procedure TJvNavPanelPage.SetPageIndex(Value: Integer);
begin
  inherited SetPageIndex(Value);
  UpdatePageList;
end;

procedure TJvNavPanelPage.SetParent({$IFDEF VisualCLX}const {$ENDIF}AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (FNavPanel = nil) or (FIconButton = nil) or (csDestroying in ComponentState) then
    Exit;
  NavPanel.Parent := AParent;
  if AParent is TJvCustomNavigationPane then
  begin
    IconPanel := TJvCustomNavigationPane(AParent).FIconPanel;
//    StyleManager := TJvCustomNavigationPane(AParent).StyleManager;

    NavPanel.Colors := TJvCustomNavigationPane(AParent).Colors;
    NavPanel.StyleManager := StyleManager;
    NavPanel.Height := TJvCustomNavigationPane(AParent).ButtonHeight;
    NavPanel.Images := TJvCustomNavigationPane(AParent).LargeImages;
    NavPanel.Font := TJvCustomNavigationPane(AParent).NavPanelFont;
    NavPanel.HotTrackFont := TJvCustomNavigationPane(AParent).NavPanelHotTrackFont;
    NavPanel.HotTrackFontOptions := TJvCustomNavigationPane(AParent).NavPanelHotTrackFontOptions;

    IconButton.Images := TJvCustomNavigationPane(AParent).SmallImages;
    IconButton.Width := TJvCustomNavigationPane(AParent).ButtonWidth;
    AutoHeader := TJvCustomNavigationPane(AParent).AutoHeaders;
  end
  else
    IconButton.Parent := nil;
end;

procedure TJvNavPanelPage.UpdatePageList;
begin
  if PageList <> nil then
    TJvCustomNavigationPane(PageList).UpdatePositions;
end;

procedure TJvNavPanelPage.SetAutoHeader(const Value: boolean);
begin
  if AutoHeader <> Value then
  begin
    FreeAndNil(FHeader);
    if Value then
    begin
      FHeader := TJvNavPanelHeader.Create(nil);
      FHeader.ColorFrom := Colors.HeaderColorFrom;
      FHeader.ColorTo := Colors.HeaderColorTo;
      FHeader.Images := NavPanel.Images;
      FHeader.ImageIndex := ImageIndex;
      FHeader.Caption := StripHotkey(Caption);
      // make sure header is top-most
      FHeader.Top := -10;
      FHeader.Parent := Self;
      FHeader.Align := alTop;
      FHeader.Alignment := Alignment;
      FHeader.WordWrap := WordWrap;
    end;
  end;
end;

function TJvNavPanelPage.GetAutoHeader: boolean;
begin
  Result := FHeader <> nil;
end;

function TJvNavPanelPage.GetAlignment: TAlignment;
begin
  if NavPanel <> nil then
    Result := NavPanel.Alignment
  else
    Result := taLeftJustify;
end;

function TJvNavPanelPage.GetWordWrap: boolean;
begin
  if NavPanel <> nil then
    Result := NavPanel.WordWrap
  else
    Result := False;
end;

procedure TJvNavPanelPage.SetAlignment(const Value: TAlignment);
begin
  if NavPanel <> nil then
    NavPanel.Alignment := Value;
  if AutoHeader then
    Header.Alignment := Value;
end;

procedure TJvNavPanelPage.SetWordWrap(const Value: boolean);
begin
  if NavPanel <> nil then
    NavPanel.WordWrap := Value;
  if AutoHeader then
    Header.WordWrap := Value;
end;

procedure TJvNavPanelPage.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
    InternalStyleManagerChanged(Self, Msg.StyleManager);
  end;
end;

procedure TJvNavPanelPage.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

procedure TJvNavPanelPage.CMControlChange(var Message: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvNavPanelPage.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

{ TJvOutlookSplitter }

procedure TJvOutlookSplitter.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

constructor TJvOutlookSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FColorFrom := $D68652;
  FColorTo := $944110;
  Align := alBottom;
  AutoSnap := False;
  ResizeStyle := rsUpdate;
  Height := 7;
  Cursor := crSizeNS;
  FParentStyleManager := True;
end;

destructor TJvOutlookSplitter.Destroy;
begin
  FStyleLink.Free;
  FStyleLink := nil;
  inherited Destroy;
end;

procedure TJvOutlookSplitter.DoStyleChange(Sender: TObject);
begin
  with (Sender as TJvNavPaneStyleManager).Colors do
  begin
    FColorFrom := SplitterColorFrom;
    FColorTo := SplitterColorTo;
    Invalidate;
  end;
end;

procedure TJvOutlookSplitter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvOutlookSplitter.Paint;
var
  i: Integer;
  R: TRect;
begin
  R := ClientRect;
  if Align in [alTop, alBottom] then
  begin
    GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, R.Bottom - R.Top);
    Inc(R.Left, (R.Right - R.Left) div 2 - 20);
    Inc(R.Top, (R.Bottom - R.Top) div 2 - 1);
    R.Right := R.Left + 2;
    R.Bottom := R.Top + 2;
    if Enabled then
      for i := 0 to 9 do // draw the dots
      begin
        Canvas.Brush.Color := cl3DDkShadow;
        Canvas.FillRect(R);
        OffsetRect(R, 1, 1);
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(R);
        Canvas.Brush.Color := ColorFrom; // (p3) this is probably not the right color, but it's close enough for me...
        Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
        OffsetRect(R, 3, -1);
      end;
  end
  else
  begin
    GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdLeftToRight, R.Right - R.Left);
    Inc(R.Top, (R.Bottom - R.Top) div 2 - 20);
    Inc(R.Left, (R.Right - R.Left) div 2 - 1);
    R.Right := R.Left + 2;
    R.Bottom := R.Top + 2;
    if Enabled then
      for i := 0 to 9 do // draw the dots
      begin
        Canvas.Brush.Color := cl3DDkShadow;
        Canvas.FillRect(R);
        OffsetRect(R, 1, 1);
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(R);
        Canvas.Brush.Color := ColorFrom;
        Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
        OffsetRect(R, -1, 3);
      end;

  end;
end;

procedure TJvOutlookSplitter.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvOutlookSplitter.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      ColorFrom := FStyleManager.Colors.SplitterColorFrom;
      ColorTo := FStyleManager.Colors.SplitterColorTo;
    end;
  end;
end;

procedure TJvOutlookSplitter.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvOutlookSplitter.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
  end;
end;

procedure TJvOutlookSplitter.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

{ TJvNavPanelHeader }

constructor TJvNavPanelHeader.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  FColorFrom := $D68652;
  FColorTo := $944110;
  Font.Name := 'Arial';
  Font.Size := 12;
  Font.Style := [fsBold];
  Font.Color := clWhite;
  Height := 27;
  Width := 225;
  FParentStyleManager := True;
end;

destructor TJvNavPanelHeader.Destroy;
begin
  FChangeLink.Free;
  FStyleLink.Free;
  inherited Destroy;
end;

procedure TJvNavPanelHeader.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelHeader.DoStyleChange(Sender: TObject);
begin
  Font := (Sender as TJvNavPaneStyleManager).Fonts.HeaderFont;
  with (Sender as TJvNavPaneStyleManager).Colors do
  begin
    FColorFrom := HeaderColorFrom;
    FColorTo := HeaderColorTo;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil
    else if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavPanelHeader.Paint;
const
  cAlignment: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
  cWordWrap: array[boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);

var
  R, TempRect: TRect;
  X, Y, H: Integer;
  function IsValidImage: boolean;
  begin
    Result := (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count);
  end;
begin
  R := ClientRect;
  GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, 32);
  H := Canvas.TextHeight(Caption);
  if Caption <> '' then
  begin
    Canvas.Font := Font;
    InflateRect(R, -4, 0);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    TempRect := R;
    DrawText(Canvas, Caption, Length(Caption), TempRect,
      DT_CALCRECT or cAlignment[Alignment] or cWordWrap[WordWrap] or DT_VCENTER or DT_NOPREFIX or DT_END_ELLIPSIS);
    if WordWrap then
      OffsetRect(R, 0, (Height - H) div 2);
    if IsValidImage and (Alignment = taCenter) then
      OffsetRect(R, 0, -Images.Height div 2);
    DrawText(Canvas, Caption, Length(Caption), R,
      cAlignment[Alignment] or cWordWrap[WordWrap] or DT_VCENTER or DT_NOPREFIX or DT_END_ELLIPSIS);
  end;
  if IsValidImage then
  begin
    Y := (Height - Images.Height) div 2;
    case Alignment of
      taLeftJustify:
        X := R.Right - Images.Width;
      taRightJustify:
        X := R.Left + 4;
    else // taCenter
      begin
        if Caption <> '' then
        begin
          if WordWrap then
            Y := R.Top + H + 4
          else
            Y := (Height + Canvas.TextHeight('Wq')) div 2 + 4;
        end;
        X := (Width - Images.Width) div 2;
      end;
    end;
    if Y > Height - Images.Height - 4 then
      Y := Height - Images.Height - 4;
    TCustomImageListEx(Images).Draw(Canvas, X, Y, ImageIndex,
      {$IFDEF VisualCLX}itImage, {$ENDIF}True);
  end;
end;

procedure TJvNavPanelHeader.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      FColorFrom := FStyleManager.Colors.HeaderColorFrom;
      FColorTo := FStyleManager.Colors.HeaderColorTo;
      Invalidate;
    end;
    InternalStyleManagerChanged(Self, Value);
  end;
end;

procedure TJvNavPanelHeader.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.TextChanged;
begin
  inherited;
  Invalidate;
end;

{$IFDEF VCL}

procedure TJvNavPanelHeader.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvNavPanelHeader.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF VisualCLX}

procedure TJvNavPanelHeader.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
    InternalStyleManagerChanged(Self, Msg.StyleManager);
  end;
end;

procedure TJvNavPanelHeader.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvNavPanelHeader.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

procedure TJvNavPanelHeader.CMControlChange(var Message: TMessage);
begin
  // a control was inserted or removed
  InternalStyleManagerChanged(Self, StyleManager);
end;

{ TJvNavPanelDivider }

procedure TJvNavPanelDivider.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

constructor TJvNavPanelDivider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  Align := alNone;
  AutoSnap := False;
  ResizeStyle := rsUpdate;
  ControlStyle := ControlStyle + [csOpaque];
  FColorFrom := $FFE7CE;
  FColorTo := $E7A67B;
  FFrameColor := $943000;
  Cursor := crSizeNS;
  {$IFDEF VCL}
  Font := Screen.IconFont;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Font := Application.Font;
  {$ENDIF VisualCLX}
  Height := 19;
  Width := 125;
  FParentStyleManager := True;
end;

destructor TJvNavPanelDivider.Destroy;
begin
  FStyleLink.Free;
  FStyleLink := nil;
  inherited Destroy;
end;

procedure TJvNavPanelDivider.DoStyleChange(Sender: TObject);
begin
  Font := (Sender as TJvNavPaneStyleManager).Fonts.DividerFont;
  with (Sender as TJvNavPaneStyleManager).Colors do
  begin
    FColorFrom := DividerColorFrom;
    FColorTo := DividerColorTo;
    Self.FFrameColor := FrameColor;
    Invalidate;
  end;
end;

procedure TJvNavPanelDivider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavPanelDivider.Paint;
const
  cAlignment: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
begin
  R := ClientRect;
  GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, 32);
  if Caption <> '' then
  begin
    Canvas.Font := Font;
    case Alignment of
      taLeftJustify:
        Inc(R.Left, 7);
      taRightJustify:
        Dec(R.Right, 7);
    end;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas, Caption, Length(Caption), R,
      DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or DT_EDITCONTROL or cAlignment[Alignment]);
  end;
  Canvas.Pen.Color := FrameColor;
  Canvas.MoveTo(0, ClientHeight - 1);
  Canvas.LineTo(Width, ClientHeight - 1);
end;

procedure TJvNavPanelDivider.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelDivider.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      ColorFrom := FStyleManager.Colors.DividerColorFrom;
      ColorTo := FStyleManager.Colors.DividerColorTo;
    end;
  end;
end;

procedure TJvNavPanelDivider.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelDivider.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelDivider.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelDivider.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
  end;
end;

procedure TJvNavPanelDivider.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

{ TJvNavPaneStyleManager }

procedure TJvNavPaneStyleManager.Assign(Source: TPersistent);
var
  SourceColors: TJvNavPanelColors;
  SourceFonts: TJvNavPanelFonts;
begin
  SourceFonts := nil;
  if Source is TJvNavPaneStyleManager then
  begin
    Theme := TJvNavPaneStyleManager(Source).Theme;
    if Theme = nptCustom then
    begin
      SourceColors := TJvNavPaneStyleManager(Source).Colors;
      SourceFonts := TJvNavPaneStyleManager(Source).Fonts;
    end
    else
      Exit;
  end
  else if Source is TJvIconPanel then
    SourceColors := TJvIconPanel(Source).Colors
  else if Source is TJvNavIconButton then
    SourceColors := TJvNavIconButton(Source).Colors
  else if Source is TJvNavPanelButton then
    SourceColors := TJvNavPanelButton(Source).Colors
  else if Source is TJvNavPanelPage then
    SourceColors := TJvNavPanelPage(Source).Colors
  else if Source is TJvCustomNavigationPane then
    SourceColors := TJvCustomNavigationPane(Source).Colors
  else
  begin
    inherited Assign(Source);
    Exit;
  end;
  FColors.Assign(SourceColors);
  if SourceFonts <> nil then
    FFonts.Assign(SourceFonts);
end;

procedure TJvNavPaneStyleManager.AssignTo(Dest: TPersistent);
var
  DestColors: TJvNavPanelColors;
  DestFonts: TJvNavPanelFonts;
begin
  DestFonts := nil;
  if Dest is TJvNavPaneStyleManager then
  begin
    TJvNavPaneStyleManager(Dest).Theme := Theme;
    if Theme = nptCustom then
    begin
      DestColors := TJvNavPaneStyleManager(Dest).Colors;
      DestFonts := TJvNavPaneStyleManager(Dest).Fonts;
    end
    else
      Exit;
  end
  else if Dest is TJvIconPanel then
    DestColors := TJvIconPanel(Dest).Colors
  else if Dest is TJvNavIconButton then
    DestColors := TJvNavIconButton(Dest).Colors
  else if Dest is TJvNavPanelButton then
    DestColors := TJvNavPanelButton(Dest).Colors
  else if Dest is TJvNavPanelPage then
    DestColors := TJvNavPanelPage(Dest).Colors
  else if Dest is TJvCustomNavigationPane then
    DestColors := TJvCustomNavigationPane(Dest).Colors
  else
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  DestColors.Assign(Colors);
  if DestFonts <> nil then
    DestFonts.Assign(Fonts);
end;

procedure TJvNavPaneStyleManager.Change;
var
  I: Integer;
begin
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      TJvNavStyleLink(FClients[I]).Change;
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

constructor TJvNavPaneStyleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  FColors := TJvNavPanelColors.Create;
  FFonts := TJvNavPanelFonts.Create;
  Theme := nptStandard;
  FColors.OnChange := DoThemeChange;
  FFonts.OnChange := DoThemeChange;
end;

destructor TJvNavPaneStyleManager.Destroy;
begin
  while FClients.Count > 0 do
    UnRegisterChanges(TJvNavStyleLink(FClients.Last));
  FClients.Free;
  FClients := nil;
  FColors.Free;
  FFonts.Free;
  inherited Destroy;
end;

procedure TJvNavPaneStyleManager.DoThemeChange(Sender: TObject);
begin
  Theme := nptCustom;
  Change;
end;

procedure TJvNavPaneStyleManager.RegisterChanges(Value: TJvNavStyleLink);
begin
  Value.Sender := Self;
  if FClients <> nil then FClients.Add(Value);
end;

procedure TJvNavPaneStyleManager.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavPaneStyleManager.SetFonts(const Value: TJvNavPanelFonts);
begin
  FFonts.Assign(Value);
end;

procedure TJvNavPaneStyleManager.SetTheme(const Value: TJvNavPanelTheme);
begin
  // TODO: also set the fonts
  if (FTheme <> Value) then
  begin
    FColors.OnChange := nil;
    FFonts.OnChange := nil;
    try
      case Value of
        nptStandard:
          begin
            FColors.ButtonColorFrom := $FFFFFF;
            FColors.ButtonColorTo := $BDBEBD;
            FColors.ButtonSelectedColorFrom := $DECFCE;
            FColors.ButtonSelectedColorTo := $DECFCE;
            FColors.FrameColor := $848484;
            FColors.ButtonHotColorFrom := $C68284;
            FColors.ButtonHotColorTo := $C68284;
            FColors.DividerColorFrom := $EFF3EF;
            FColors.DividerColorTo := $C6C3C6;
            FColors.HeaderColorFrom := $848284;
            FColors.HeaderColorTo := $848284;
            FColors.SplitterColorFrom := $C6C3C6;
            FColors.SplitterColorTo := $8C8E8C;
            FColors.ButtonSeparatorColor := clGray;
            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindow;
            FFonts.DividerFont.Color := clWindow;
          end;
        nptXPBlue:
          begin
            FColors.ButtonColorFrom := $F7E2CD;
            FColors.ButtonColorTo := $F3A080;
            FColors.ButtonSelectedColorFrom := $BBE2EA;
            FColors.ButtonSelectedColorTo := $389FDD;
            FColors.FrameColor := $6F2F0C;
            FColors.ButtonHotColorFrom := $DBFBFF;
            FColors.ButtonHotColorTo := $5FC8FB;
            FColors.DividerColorFrom := $FFDBBC;
            FColors.DividerColorTo := $F2C0A4;
            FColors.HeaderColorFrom := $D0835C;
            FColors.HeaderColorTo := $903B09;
            FColors.SplitterColorFrom := $B78676;
            FColors.SplitterColorTo := $A03D09;
            FColors.ButtonSeparatorColor := clGray;
            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindowText;
            FFonts.DividerFont.Color := clWindowText;
          end;
        nptXPSilver:
          begin
            FColors.ButtonColorFrom := $F4E2E1;
            FColors.ButtonColorTo := $B09494;
            FColors.ButtonSelectedColorFrom := $BBE2EA;
            FColors.ButtonSelectedColorTo := $389FDD;
            FColors.FrameColor := $527D92;
            FColors.ButtonHotColorFrom := $DBFBFF;
            FColors.ButtonHotColorTo := $5FC8FB;
            FColors.DividerColorFrom := $F8F3F4;
            FColors.DividerColorTo := $EADADB;
            FColors.HeaderColorFrom := $BAA8BA;
            FColors.HeaderColorTo := $917275;
            FColors.SplitterColorFrom := $B8ABA9;
            FColors.SplitterColorTo := $81767E;
            FColors.ButtonSeparatorColor := clGray;
            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindowText;
            FFonts.DividerFont.Color := clWindowText;
          end;
        nptXPOlive:
          begin
            FColors.ButtonColorFrom := $D6F3E3;
            FColors.ButtonColorTo := $93BFB2;
            FColors.ButtonSelectedColorFrom := $BBE2EA;
            FColors.ButtonSelectedColorTo := $389FDD;
            FColors.FrameColor := $5A7972;
            FColors.ButtonHotColorFrom := $DBFBFF;
            FColors.ButtonHotColorTo := $5FC8FB;
            FColors.DividerColorFrom := $D2F4EE;
            FColors.DividerColorTo := $B5DFD8;
            FColors.HeaderColorFrom := $94BFB4;
            FColors.HeaderColorTo := $427665;
            FColors.SplitterColorFrom := $758D81;
            FColors.SplitterColorTo := $3A584D;
            FColors.ButtonSeparatorColor := clGray;
            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindowText;
            FFonts.DividerFont.Color := clWindowText;
          end;
        nptCustom:
          begin
          // do nothing
          end;
      end;
      FTheme := Value;
      Change;
    finally
      FColors.OnChange := DoThemeChange;
      FFonts.OnChange := DoThemeChange;
    end;
  end;
end;

procedure TJvNavPaneStyleManager.UnRegisterChanges(Value: TJvNavStyleLink);
var
  I: Integer;
begin
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      if FClients[I] = Value then
      begin
        Value.Sender := nil;
        FClients.Delete(I);
        Break;
      end;
end;

{ TJvNavStyleLink }

procedure TJvNavStyleLink.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Sender)
end;

destructor TJvNavStyleLink.Destroy;
begin
  if (Sender is TJvNavPaneStyleManager) then
    TJvNavPaneStyleManager(Sender).UnRegisterChanges(Self);
  inherited Destroy;
end;

{ TJvNavPaneToolPanel }

procedure TJvNavPaneToolPanel.ButtonsChanged;
var
  i: Integer;
  B: TJvNavPanelToolButton;
begin
  FRealButtons.Clear;
  for i := 0 to Buttons.Count - 1 do
  begin
    B := TJvNavPanelToolButton.Create(nil);
    B.Visible := False;
    B.SetBounds(0, 0, ButtonHeight - 1, ButtonHeight - 3);
    B.ButtonType := nibImage;
    B.Images := Images;
    B.ImageIndex := Buttons[i].ImageIndex;
    B.Enabled := Buttons[i].Enabled;
    B.Parent := Self;
//    B.Colors.ButtonSelectedColorFrom := clNone;
//    B.Colors.ButtonSelectedColorTo := clNone;
//    B.Colors.ButtonHotColorFrom := clNone;
//    B.Colors.ButtonHotColorTo := clNone;
    B.Tag := i;
    B.OnClick := InternalButtonClick;
    FRealButtons.Add(B);
  end;
  Invalidate;
end;

constructor TJvNavPaneToolPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, {$IFDEF VCL}csCaptureMouse, {$ENDIF VCL}csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable];
  ParentColor := False;
  Color := clWindow;

  FRealButtons := TObjectList.Create;
  FButtons := TJvNavPaneToolButtons.Create(Self);
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  FColorFrom := $FFF7CE;
  FColorTo := $E7A67B;
  FButtonColor := $A6A5A6;
  FButtonWidth := 30;
  FButtonHeight := 21;
  FHeaderHeight := 29;
  FEdgeRounding := 9;
  FShowGrabber := True;
  {$IFDEF VCL}
  Font := Screen.IconFont;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Font := Application.Font;
  {$ENDIF VisualCLX}
  Font.Style := [fsBold];

  FCloseButton := TJvNavPanelToolButton.Create(Self);
  FCloseButton.ButtonType := nibClose;
  FCloseButton.Parent := Self;
  FCloseButton.Visible := True;
  FCloseButton.OnClick := DoCloseClick;

  FDropDown := TJvNavPanelToolButton.Create(Self);
  FDropDown.Visible := False;
  FDropDown.ButtonType := nibDropArrow;
  FDropDown.OnDropDownMenu := DoDropDownMenu;
  FDropDown.Parent := Self;

  Width := 185;
  Height := 41;
  FParentStyleManager := True;
end;

destructor TJvNavPaneToolPanel.Destroy;
begin
  FStyleLink.Free;
  FChangeLink.Free;
  FButtons.Free;
  FRealButtons.Free;
  inherited Destroy;
end;

procedure TJvNavPaneToolPanel.DoCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvNavPaneToolPanel.DoDropDownMenu(Sender: TObject;
  MousePos: TPoint; var Handled: boolean);
begin
  if Assigned(FOnDropDownMenu) then
    FOnDropDownMenu(Self, MousePos, Handled);
end;

procedure TJvNavPaneToolPanel.DoImagesChange(Sender: TObject);
begin
  ButtonsChanged;
end;

procedure TJvNavPaneToolPanel.DoStyleChange(Sender: TObject);
begin
  Font := (Sender as TJvNavPaneStyleManager).Fonts.NavPanelFont;
  with (Sender as TJvNavPaneStyleManager).Colors do
  begin
    FColorFrom := ButtonColorFrom;
    FColorTo := ButtonColorTo;
    FButtonColor := SplitterColorTo;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

function TJvNavPaneToolPanel.GetCloseButton: Boolean;
begin
  Result := FCloseButton.Visible;
end;

function TJvNavPaneToolPanel.GetDropDownMenu: TPopupMenu;
begin
  Result := FDropDown.DropDownMenu;
end;

function TJvNavPaneToolPanel.GetHitTestInfoAt(X,
  Y: Integer): TJvToolPanelHitTestInfos;

  function InRange(Value, Min, Max: Integer): Boolean;
  begin
    Result := (Value >= Min) and (Value <= Max);
  end;

begin
  Result := [];
  if X < 0 then
    Include(Result, phtToLeft);
  if X > ClientWidth then
    Include(Result, phtToRight);
  if Y < 0 then
    Include(Result, phtAbove);
  if Y > ClientHeight then
    Include(Result, phtBelow);
  if InRange(Y, 0, HeaderHeight - EdgeRounding) then
  begin
    if InRange(X, 0, ClientWidth) then
    begin
      Include(Result, phtHeader);
      if (X <= 16) and ShowGrabber then
        Include(Result, phtGrabber);
    end;
  end;
  if InRange(X, 0, ClientWidth) and InRange(Y, HeaderHeight, ClientHeight) then
    Include(Result, phtClient);
end;

procedure TJvNavPaneToolPanel.InternalButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, TJvNavPanelToolButton(Sender).Tag);
end;

procedure TJvNavPaneToolPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (AComponent = Images) then
      Images := nil
    else if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TJvNavPaneToolPanel.Paint;
var
  R, R2: TRect;
  i, X, Y: Integer;
  B: TJvNavPanelToolButton;
begin
  // first, fill the background
  {$IFDEF VCL}
  Canvas.Lock;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Canvas.Start;
  {$ENDIF VisualCLX}
  try
    R := ClientRect;
    R.Bottom := HeaderHeight + EdgeRounding;
    Canvas.Brush.Color := ColorTo;
    Canvas.FillRect(R);
    R.Bottom := R.Top + HeaderHeight;
    GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, 32);
    // draw the drag dots
    R2 := Rect(R.Left, R.Top + HeaderHeight div 4, R.Left + 2, R.Top + HeaderHeight div 4 + 2);
    OffsetRect(R2, 6, 0);
    if ShowGrabber then
    begin
      for i := 0 to 3 do
      begin
        Canvas.Brush.Color := clWhite;
        OffsetRect(R2, 1, 1);
        Canvas.FillRect(R2);
        Canvas.Brush.Color := ButtonColor;
        OffsetRect(R2, -1, -1);
        Canvas.FillRect(R2);
        OffsetRect(R2, 0, 4);
      end;
      // draw the text
      Inc(R.Left, 16);
    end
    else
      Inc(R.Left, 12);
    Canvas.Font := Self.Font;
    if DropDownMenu = nil then
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas, Caption, Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    end;
    // draw the client areas top rounding
    R := ClientRect;
    Inc(R.Top, HeaderHeight);
//    Inc(R.Left);
    Inc(R.Right);
    Canvas.Brush.Color := Color;
    Canvas.Pen.Style := psClear;
    Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, EdgeRounding, EdgeRounding);
    // draw the button area
    Canvas.Brush.Color := ButtonColor;
    if Buttons.Count > 0 then
    begin
      R2 := Rect(R.Left, R.Top, R.Left + ButtonWidth * Buttons.Count - 1, R.Top + ButtonHeight);
      Canvas.RoundRect(R2.Left, R2.Top, R2.Right, R2.Bottom, EdgeRounding, EdgeRounding);
      // square two corners
      Canvas.FillRect(Rect(R2.Right - EdgeRounding, R2.Top, R2.Right - 1, R2.Top + EdgeRounding));
      Canvas.FillRect(Rect(R2.Left, R2.Bottom - EdgeRounding, R2.Left + EdgeRounding, R2.Bottom - 1));
      Canvas.Pen.Style := psSolid;
      Y := R2.Top + 1;
      // adjust the buttons and draw the dividers
      for i := 0 to Buttons.Count - 1 do
      begin
        X := R2.Left + ButtonWidth * i;
        B := TJvNavPanelToolButton(FRealButtons[i]);
        B.Left := X + (ButtonWidth - B.Width) div 2;
        B.Top := Y;
        B.Visible := True;
        if i > 0 then
        begin
          Canvas.Pen.Color := $E7EBEF;
          Canvas.MoveTo(X, R2.Top + 2);
          Canvas.LineTo(X, R2.Bottom - 3);
        end;
        if i < Buttons.Count - 1 then
        begin
          Canvas.Pen.Color := $CED3D6;
          Canvas.MoveTo(X + ButtonWidth - 1, R2.Top + 1);
          Canvas.LineTo(X + ButtonWidth - 1, R2.Bottom - 4);
        end;
      end;
    end;
      // fill the rest of the client area
    Canvas.Brush.Color := Color;
    Inc(R.Top, HeaderHeight + ButtonHeight + EdgeRounding);
    Dec(R.Right);
    Canvas.FillRect(R);
  finally
  {$IFDEF VCL}
  Canvas.Unlock;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Canvas.Stop;
  {$ENDIF VisualCLX}
  end;
end;

procedure TJvNavPaneToolPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
const
  cOffset = 14;
  cWidth = 18;
  cHeight = 18;
begin
  inherited;
  if Parent <> nil then
  begin
    FCloseButton.SetBounds(ClientWidth - cWidth - 2, (HeaderHeight - cHeight) div 2, cWidth, cHeight);
    if FCloseButton.Visible or (csDesigning in ComponentState) then
      FDropDown.SetBounds(cOffset, (HeaderHeight - cHeight) div 2, Width - cWidth - cOffset - 2, cHeight)
    else
      FDropDown.SetBounds(cOffset, (HeaderHeight - cHeight) div 2, Width - cOffset - 4, cHeight);
  end;
end;

procedure TJvNavPaneToolPanel.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetButtonHeight(const Value: Integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetButtons(const Value: TJvNavPaneToolButtons);
begin
  FButtons.Assign(Value);
end;

procedure TJvNavPaneToolPanel.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetCloseButton(const Value: Boolean);
begin
  if FCloseButton.Visible <> Value then
  begin
    FCloseButton.Visible := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TJvNavPaneToolPanel.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetDropDownMenu(const Value: TPopupMenu);
begin
  if FDropDown.DropDownMenu <> Value then
  begin
    FDropDown.DropDownMenu := Value;
    FDropDown.Visible := Value <> nil;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetEdgeRounding(const Value: Integer);
begin
  if FEdgeRounding <> Value then
  begin
    FEdgeRounding := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetHeaderHeight(const Value: Integer);
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetImages(const Value: TCustomImageList);
var
  i: integer;
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnregisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;
    for i := 0 to Buttons.Count - 1 do
      TJvNavPanelToolButton(FRealButtons[i]).Images := FImages;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetShowGrabber(const Value: Boolean);
begin
  if FShowGrabber <> Value then
  begin
    FShowGrabber := Value;
    Invalidate;
  end;
end;

procedure TJvNavPaneToolPanel.SetStyleManager(const Value: TJvNavPaneStyleManager);
begin
  if FStyleManager <> Value then
  begin
    ParentStyleManager := False;
    if FStyleManager <> nil then
      FStyleManager.UnRegisterChanges(FStyleLink);
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      FStyleManager.RegisterChanges(FStyleLink);
      FStyleManager.FreeNotification(Self);
      FColorFrom := FStyleManager.Colors.ButtonColorFrom;
      FColorTo := FStyleManager.Colors.ButtonColorTo;
      FButtonColor := FStyleManager.Colors.SplitterColorTo;
      Invalidate;
    end;
    InternalStyleManagerChanged(Self, StyleManager);
  end;
end;

procedure TJvNavPaneToolPanel.ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange);
begin
  if (Msg.Sender <> Self) and ParentStyleManager then
  begin
    StyleManager := Msg.StyleManager;
    ParentStyleManager := True;
    InternalStyleManagerChanged(Self, Msg.StyleManager);
  end;
end;

procedure TJvNavPaneToolPanel.TextChanged;
begin
  inherited TextChanged;
  FDropDown.Caption := Caption;
  Invalidate;
end;

{$IFDEF VisualCLX}

function TJvNavPaneToolPanel.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}

procedure TJvNavPaneToolPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TJvNavPaneToolPanel.WMNCPaint(var Message: TWMNCPaint);
var
  AColor: TColor;
begin
  AColor := Color;
  Color := ButtonColor;
  inherited;
  Color := AColor;
end;
{$ENDIF VCL}

procedure TJvNavPaneToolPanel.SetParentStyleManager(const Value: boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0,0);
  end;
end;

procedure TJvNavPaneToolPanel.CMControlChange(var Message: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvNavPaneToolPanel.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

{ TJvNavPaneToolButton }

constructor TJvNavPaneToolButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FEnabled := True;
end;

procedure TJvNavPaneToolButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TJvNavPaneToolButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

{ TJvNavPaneToolButtons }

function TJvNavPaneToolButtons.Add: TJvNavPaneToolButton;
begin
  Result := TJvNavPaneToolButton(inherited Add);
end;

constructor TJvNavPaneToolButtons.Create(AOwner: TJvNavPaneToolPanel);
begin
  inherited Create(AOwner, TJvNavPaneToolButton);
  FPanel := AOwner;
end;

function TJvNavPaneToolButtons.GetItem(Index: Integer): TJvNavPaneToolButton;
begin
  Result := TJvNavPaneToolButton(inherited Items[Index]);
end;

procedure TJvNavPaneToolButtons.SetItem(Index: Integer;
  const Value: TJvNavPaneToolButton);
begin
  inherited Items[Index] := Value;
end;

procedure TJvNavPaneToolButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FPanel <> nil then
    FPanel.ButtonsChanged;
end;

{ TJvNavPanelToolButton }

constructor TJvNavPanelToolButton.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
end;

destructor TJvNavPanelToolButton.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TJvNavPanelToolButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TJvNavPanelToolButton.Paint;
var
  R: TRect;
  I: Integer;
begin
//  inherited;
  if MouseStates <> [] then
  begin
    Canvas.Pen.Color := $6B2408;
    if (bsMouseInside in MouseStates) then
      Canvas.Brush.Color := $D6BEB5;
    if (bsMouseDown in MouseStates) or Down then
    begin
      if (ButtonType = nibDropArrow) and (DropDownMenu <> nil) then
      begin
        Canvas.Brush.Color := clWindow;
        Canvas.Pen.Color := cl3DDkShadow;
      end
      else
        Canvas.Brush.Color := $B59284;
    end;
    Canvas.Rectangle(ClientRect);
  end;
  case ButtonType of
    nibDropArrow: // dropdown arrow is 7x4, right-aligned
      begin
        R := ClientRect;
        if Caption <> '' then
        begin
          InflateRect(R, -2, -2);
          Canvas.Font := Font;
          SetBkMode(Canvas.Handle, TRANSPARENT);
          InflateRect(R, -2, 0);
          DrawText(Canvas, Caption, Length(Caption), R, DT_LEFT or DT_VCENTER or DT_NOPREFIX);
          InflateRect(R, 2, 0);
        end;
        R.Left := R.Right - 11;
        Dec(R.Right, 4);
        R.Top := (RectHeight(ClientRect) - 4) div 2;
        Canvas.Pen.Color := clWindowText;
        for I := 0 to 3 do
        begin
          Canvas.MoveTo(R.Left, R.Top);
          Canvas.LineTo(R.Right, R.Top);
          Dec(R.Right);
          Inc(R.Left);
          Inc(R.Top);
        end;
      end;
    nibClose:
      begin
      // close button is 8x8, centered
        if bsMouseDown in MouseStates then
          Canvas.Pen.Color := clHighlightText
        else
          Canvas.Pen.Color := clWindowText;
        R := ClientRect;
        InflateRect(R, -(RectWidth(R) div 2 - 4), -(RectHeight(R) div 2 - 4));
        if Odd(Height) or Odd(Width) then
        begin
          Inc(R.Right);
          Inc(R.Bottom);
        end;
        // (p3) this isn't exactly the same as MS's but good enough for me :)
        for I := 0 to 7 do
        begin
          Canvas.MoveTo(R.Left + I, R.Top + I);
          Canvas.LineTo(R.Left + I + 2, R.Top + I);
        end;
        for I := 0 to 7 do
        begin
          Canvas.MoveTo(R.Right - I, R.Top + I);
          Canvas.LineTo(R.Right - I - 2, R.Top + I);
        end;
      end;
    nibImage:
      if Assigned(Images) then
        TCustomImageListEx(Images).Draw(
          Canvas, (Width - Images.Width) div 2, (Height - Images.Height) div 2,
          ImageIndex, {$IFDEF VisualCLX}itImage, {$ENDIF}Enabled);
    else
      raise Exception.Create('ButtonType not supported');
  end;

end;

procedure TJvNavPanelToolButton.SetButtonType(const Value: TJvNavIconButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelToolButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelToolButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;

initialization
  RegisterClasses([TJvNavPanelPage]);

end.

