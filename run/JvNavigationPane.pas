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
  Windows, Messages,
  {$ENDIF VCL}
  Controls, Graphics, Menus, ExtCtrls, ImgList,
  {$IFDEF VisualCLX}
  Qt, QTypes, QWindows, QMessages,
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
    Sender: TControl;
    StyleManager: TJvNavPaneStyleManager;
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
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    FParentStyleManager: Boolean;
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: Boolean);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg: TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    {$IFDEF VCL}
    procedure CMControlChange(var Msg: TMessage); message CM_CONTROLCHANGE;
    {$ENDIF VCL}
    procedure SetParentStyleManager(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TextChanged; override;
    procedure Paint; override;
    {$IFDEF VisualCLX}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
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
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property ColorFrom: TColor read FColorFrom write SetColorFrom default TColor($D0835C);
    property ColorTo: TColor read FColorTo write SetColorTo default TColor($903B09);
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
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
    FParentStyleManager: Boolean;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: Boolean);
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
    property ColorFrom: TColor read FColorFrom write SetColorFrom default TColor($FFDBBC);
    property ColorTo: TColor read FColorTo write SetColorTo default TColor($F2C0A4);
    property Constraints;
    property Cursor default crSizeNS;
    property Enabled;
    property Font;
    property FrameColor: TColor read FFrameColor write SetFrameColor default TColor($6F2F0C);
    property Height default 19;
    property ResizeStyle default rsUpdate;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
    property Width default 125;
  end;

  TJvOutlookSplitter = class(TJvExSplitter)
  private
    FColorTo: TColor;
    FColorFrom: TColor;
    FStyleManager: TJvNavPaneStyleManager;
    FStyleLink: TJvNavStyleLink;
    FParentStyleManager: Boolean;
    {$IFDEF VCL}
    FDragZone: integer;
    FOldCursor: TCursor;
    {$ENDIF VCL}
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: Boolean);
    {$IFDEF VCL}
    procedure SetCursor(const Value: TCursor);
    function GetDragZoneRect: TRect;
    function MouseInDragZone(X, Y: Integer): boolean;
    {$ENDIF VCL}
  protected
    procedure Paint; override;
    procedure EnabledChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF VCL}
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // NB! Color is published but not used
    property Align default alBottom;
    property AutoSnap default False;
    property Cursor {$IFDEF VCL} write SetCursor {$ENDIF VCL} default crSizeNS;
    {$IFDEF VCL}
    // DragZone is the number of pixels in the center of the control that constitutes the draggable area.
    // For example, with a left/right aligned splitter and a DragZone of 100, 50 pixels above and 50 pixels below
    // the vertical midpoint can be clicked to start the sizing. Any clicks outside this area will not start a sizing operation
    // If DragZone <= 0, the entire control is a drag zone
    property DragZone: integer read FDragZone write FDragZone default 0;
    {$ENDIF VCL}
    property ResizeStyle default rsUpdate;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default TColor($B78676);
    property ColorTo: TColor read FColorTo write SetColorTo default TColor($A03D09);
    property Height default 7;
    property Enabled;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDblClick;
    property OnMouseUp;
    property OnMouseMove;
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
    FToolPanelColorFrom: TColor;
    FToolPanelColorTo: TColor;
    FToolPanelHeaderColorTo: TColor;
    FToolPanelHeaderColorFrom: TColor;
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
    procedure SetToolPanelColorFrom(const Value: TColor);
    procedure SetToolPanelColorTo(const Value: TColor);
    procedure SetToolPanelHeaderColorFrom(const Value: TColor);
    procedure SetToolPanelHeaderColorTo(const Value: TColor);
  protected
    procedure Change;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ButtonColorFrom: TColor read FButtonColorFrom write SetButtonColorFrom default TColor($F7E2CD);
    property ButtonColorTo: TColor read FButtonColorTo write SetButtonColorTo default TColor($F3A080);
    property ButtonHotColorFrom: TColor read FButtonHotColorFrom write SetButtonHotColorFrom default TColor($DBFBFF);
    property ButtonHotColorTo: TColor read FButtonHotColorTo write SetButtonHotColorTo default TColor($5FC8FB);
    property ButtonSelectedColorFrom: TColor read FButtonSelectedColorFrom write SetButtonSelectedColorFrom default TColor($BBE2EA);
    property ButtonSelectedColorTo: TColor read FButtonSelectedColorTo write SetButtonSelectedColorTo default TColor($389FDD);
    property ButtonSeparatorColor: TColor read FButtonSeparatorColor write SetButtonSeparatorColor default clGray;
    property SplitterColorFrom: TColor read FSplitterColorFrom write SetSplitterColorFrom default TColor($B78676);
    property SplitterColorTo: TColor read FSplitterColorTo write SetSplitterColorTo default TColor($A03D09);
    property DividerColorFrom: TColor read FDividerColorFrom write SetDividerColorFrom default TColor($FFDBBC);
    property DividerColorTo: TColor read FDividerColorTo write SetDividerColorTo default TColor($F2C0A4);
    property HeaderColorFrom: TColor read FHeaderColorFrom write SetHeaderColorFrom default TColor($D0835C);
    property HeaderColorTo: TColor read FHeaderColorTo write SetHeaderColorTo default TColor($903B09);
    property FrameColor: TColor read FFrameColor write SetFrameColor default TColor($6F2F0C);
    property ToolPanelColorFrom: TColor read FToolPanelColorFrom write SetToolPanelColorFrom default clWindow;
    property ToolPanelColorTo: TColor read FToolPanelColorTo write SetToolPanelColorTo default clWindow;
    property ToolPanelHeaderColorFrom: TColor read FToolPanelHeaderColorFrom write SetToolPanelHeaderColorFrom default TColor($F7E2CD);
    property ToolPanelHeaderColorTo: TColor read FToolPanelHeaderColorTo write SetToolPanelHeaderColorTo default TColor($F3A080);
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
    FParentStyleManager: Boolean;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    function GetDropDownMenu: TPopupMenu;
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg: TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    {$IFDEF VCL}
    procedure CMControlChange(var Msg: TMessage); message CM_CONTROLCHANGE;
    {$ENDIF VCL}
    procedure SetParentStyleManager(const Value: Boolean);
  protected
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF VisualCLX}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
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
    FParentStyleManager: Boolean;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure DoImagesChange(Sender: TObject);
    procedure SetButtonType(const Value: TJvNavIconButtonType);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: Boolean);
    function IsColorsStored: Boolean;
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
    property Colors: TJvNavPanelColors read FColors write SetColors stored IsColorsStored;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
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
    FDrawPartialMenuFrame: Boolean;
    FTransparentDown: Boolean;
    procedure DoImagesChange(Sender: TObject);
    procedure SetButtonType(const Value: TJvNavIconButtonType);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TransparentDown: Boolean read FTransparentDown write FTransparentDown default False;
    property DrawPartialMenuFrame: Boolean read FDrawPartialMenuFrame write FDrawPartialMenuFrame default False;
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
    FWordWrap: Boolean;
    FParentStyleManager: Boolean;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: Boolean);
    {$IFDEF VCL}
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF VCL}
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure SetParentStyleManager(const Value: Boolean);
    function IsColorsStored: Boolean;
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
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
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
    property DropDownMenu;
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

    property Colors: TJvNavPanelColors read FColors write SetColors stored IsColorsStored;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
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

  TJvNavPaneBackgroundImage = class(TPersistent)
  private
    FCenter: Boolean;
    FTile: Boolean;
    FTransparent: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FDrawing: Boolean;
    FPicture: TPicture;
    FOnChange: TNotifyEvent;
    procedure SetCenter(const Value: Boolean);
    procedure SetPicture(const Value: TPicture);
    procedure SetProportional(const Value: Boolean);
    procedure SetStretch(const Value: Boolean);
    procedure SetTile(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure PictureChanged(Sender: TObject);
  protected
    procedure DrawImage(Canvas: TCanvas; ARect: TRect); virtual;
    procedure Change; virtual;
    function CalcRect(ADestRect: TRect): TRect;
  public
    constructor Create;
    destructor Destroy; override;
    function HasImage: boolean;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Stretch: Boolean read FStretch write SetStretch;
    property Proportional: Boolean read FProportional write SetProportional;
    property Center: Boolean read FCenter write SetCenter;
    property Tile: Boolean read FTile write SetTile;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    FImageIndex: TImageIndex;
    FParentStyleManager: Boolean;
    FBackground: TJvNavPaneBackgroundImage;
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
    procedure SetAutoHeader(const Value: Boolean);
    function GetAutoHeader: Boolean;
    function GetAlignment: TAlignment;
    function GetWordWrap: Boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: Boolean);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg: TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    {$IFDEF VCL}
    procedure CMControlChange(var Msg: TMessage); message CM_CONTROLCHANGE;
    {$ENDIF VCL}
    procedure SetParentStyleManager(const Value: Boolean);
    procedure SetAction(const Value: TBasicAction);
    procedure SetBackground(const Value: TJvNavPaneBackgroundImage);
    procedure DoBackgroundChange(Sender: TObject);
  protected
    procedure UpdatePageList;
    function GetAction: TBasicAction;
      {$IFDEF VCL}{$IFDEF COMPILER6_UP} override; {$ENDIF}{$ENDIF}
    {$IFDEF VisualCLX}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
    {$ENDIF VisualCLX}
    procedure SetParent({$IFDEF VisualCLX} const {$ENDIF} AParent: TWinControl); override;
    procedure SetPageIndex(Value: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property NavPanel: TJvNavPanelButton read FNavPanel;
    property IconButton: TJvNavIconButton read FIconButton;
    property IconPanel: TJvIconPanel read FIconPanel write SetIconPanel;
    property Colors: TJvNavPanelColors read GetColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
    property Header: TJvNavPanelHeader read FHeader;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoHeader: Boolean read GetAutoHeader write SetAutoHeader;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Background: TJvNavPaneBackgroundImage read FBackground write SetBackground;

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
    FRealButton: TJvNavPanelToolButton;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetEnabled(const Value: Boolean);
    procedure SetAction(const Value: TBasicAction);
    procedure SetHint(const Value: string);
    function GetAction: TBasicAction;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetImageIndex: TImageIndex;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Button: TJvNavPanelToolButton read FRealButton;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Hint: string read GetHint write SetHint;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
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
    FButtonWidth: Integer;
    FHeaderHeight: Integer;
    FEdgeRounding: Integer;
    FButtonHeight: Integer;
    FImages: TCustomImageList;
    FButtons: TJvNavPaneToolButtons;
    FOnButtonClick: TJvNavPaneToolButtonClick;
    FDropDown: TJvNavPanelToolButton;
    FCloseButton: TJvNavPanelToolButton;
    FOnClose: TNotifyEvent;
    FShowGrabber: Boolean;
    FOnDropDownMenu: TContextPopupEvent;
    FParentStyleManager: Boolean;
    FBackground: TJvNavPaneBackgroundImage;
    FColors: TJvNavPanelColors;
    FHeaderVisible: Boolean;
    procedure DoStyleChange(Sender: TObject);
    procedure DoImagesChange(Sender: TObject);
    procedure ButtonsChanged;
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetEdgeRounding(const Value: Integer);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtons(const Value: TJvNavPaneToolButtons);
    procedure InternalButtonClick(Sender: TObject);
    function GetCloseButton: Boolean;
    function GetDropDownMenu: TPopupMenu;
    procedure SetCloseButton(const Value: Boolean);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure DoCloseClick(Sender: TObject);
    procedure SetShowGrabber(const Value: Boolean);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg: TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    {$IFDEF VCL}
    procedure CMControlChange(var Msg: TMessage); message CM_CONTROLCHANGE;
    {$ENDIF VCL}
    procedure SetParentStyleManager(const Value: Boolean);
    function GetDrawPartialMenuFrame: Boolean;
    procedure SetDrawPartialMenuFrame(const Value: Boolean);
    procedure SetBackground(const Value: TJvNavPaneBackgroundImage);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetHeaderVisible(const Value: Boolean);
    function IsColorsStored: Boolean;
  protected


    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
    property EdgeRounding: Integer read FEdgeRounding write SetEdgeRounding default 9;
    procedure AdjustClientRect(var Rect: TRect); override;
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
    property BorderWidth;
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
    property Background: TJvNavPaneBackgroundImage read FBackground write SetBackground;
    property DrawPartialMenuFrame: Boolean read GetDrawPartialMenuFrame write SetDrawPartialMenuFrame default False;
    property Buttons: TJvNavPaneToolButtons read FButtons write SetButtons;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 25;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property CloseButton: Boolean read GetCloseButton write SetCloseButton default True;
    property Colors: TJvNavPanelColors read FColors write SetColors stored IsColorsStored;

    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 29;
    property HeaderVisible: Boolean read FHeaderVisible write SetHeaderVisible default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ParentColor default False;
    property ShowGrabber: Boolean read FShowGrabber write SetShowGrabber default True;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
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
    FAutoHeaders: Boolean;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    FOnDropDownMenu: TContextPopupEvent;
    FParentStyleManager: Boolean;
    FBackground: TJvNavPaneBackgroundImage;
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
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    procedure DoSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure SetNavPanelFont(const Value: TFont);
    procedure SetNavPanelHotTrackFont(const Value: TFont);
    procedure SetNavPanelHotTrackFontOptions(const Value: TJvTrackFontOptions);

    procedure DoNavPanelFontChange(Sender: TObject);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetSplitterHeight(const Value: Integer);
    function GetSplitterHeight: Integer;
    procedure SetStyleManager(const Value: TJvNavPaneStyleManager);
    procedure DoStyleChange(Sender: TObject);
    procedure SetAutoHeaders(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWordWrap(const Value: Boolean);
    procedure ParentStyleManagerChanged(var Msg: TMsgStyleManagerChange); message CM_PARENTSTYLEMANAGERCHANGED;
    procedure ParentStyleManagerChange(var Msg: TMessage); message CM_PARENTSTYLEMANAGERCHANGE;
    {$IFDEF VCL}
    procedure CMControlChange(var Msg: TMessage); message CM_CONTROLCHANGE;
    {$ENDIF VCL}
    procedure SetParentStyleManager(const Value: Boolean);
    procedure SetBackground(const Value: TJvNavPaneBackgroundImage);
  protected
    function IsColorsStored: Boolean;
    function IsNavPanelFontStored: Boolean;
    function IsNavPanelFontHotTrackStored: Boolean;
    function IsNavPanelHotTrackFontOptionsStored: Boolean;
    procedure UpdatePages; virtual;
    {$IFDEF VisualCLX}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
    function WidgetFlags: Integer; override;
    procedure Paint; override;
    {$ENDIF VisualCLX}
    procedure SetActivePage(Page: TJvCustomPage); override;
    procedure InsertPage(APage: TJvCustomPage); override;
    procedure RemovePage(APage: TJvCustomPage); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    function InternalGetPageClass: TJvCustomPageClass; override;
    property NavPages[Index: Integer]: TJvNavPanelPage read GetNavPage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HidePage(Page: TJvCustomPage): TJvCustomPage; override;
    function ShowPage(Page: TJvCustomPage; PageIndex: integer = -1): TJvCustomPage; override;
    procedure UpdatePositions;
  protected
    {$IFDEF VCL}
    property BorderWidth default 1;
    {$ENDIF VCL}
    property AutoHeaders: Boolean read FAutoHeaders write SetAutoHeaders default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Background: TJvNavPaneBackgroundImage read FBackground write SetBackground;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 28;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 22;
    property NavPanelFont: TFont read FNavPanelFont write SetNavPanelFont;
    property NavPanelHotTrackFont: TFont read FNavPanelHotTrackFont write SetNavPanelHotTrackFont;
    property NavPanelHotTrackFontOptions: TJvTrackFontOptions read FNavPanelHotTrackFontOptions write SetNavPanelHotTrackFontOptions;

    property Color default clWindow;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property StyleManager: TJvNavPaneStyleManager read FStyleManager write SetStyleManager;
    // (p3) must be published after StyleManager
    property ParentStyleManager: Boolean read FParentStyleManager write SetParentStyleManager default True;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property MaximizedCount: Integer read GetMaximizedCount write SetMaximizedCount;
    property ParentColor default False;
    property Resizable: Boolean read FResizable write SetResizable default True;
    property SmallImages: TCustomImageList read GetSmallImages write SetSmallImages;
    property SplitterHeight: Integer read GetSplitterHeight write SetSplitterHeight default 7;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
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
    property Background;
    property ButtonHeight;
    property ButtonWidth;
    property Caption;
    property Color;
    property Colors stored IsColorsStored;
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
    property NavPanelFont stored IsNavPanelFontStored;
    property NavPanelHotTrackFont stored IsNavPanelFontHotTrackStored;
    property NavPanelHotTrackFontOptions stored IsNavPanelHotTrackFontOptionsStored;

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
    function IsColorsStored: Boolean;
    function IsFontsStored: Boolean;
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
    property Colors: TJvNavPanelColors read FColors write SetColors stored IsColorsStored;
    property Fonts: TJvNavPanelFonts read FFonts write SetFonts stored IsFontsStored;
    property Theme: TJvNavPanelTheme read FTheme write SetTheme nodefault;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

type
  {$IFDEF COMPILER5}
  TCustomImageListEx = class(TCustomImageList)
  public
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      Enabled: Boolean); overload;
  end;
  {$ELSE}
  TCustomImageListEx = TCustomImageList;
  {$ENDIF COMPILER5}

implementation

uses
  Math,
  {$IFDEF COMPILER5}
  CommCtrl,
  {$ENDIF COMPILER5}
  Forms, ActnList,
  JvJVCLUtils, JvJCLUtils, JvResources;

const
  cNavPanelButtonGroupIndex = 113;
  cToolButtonHeight = 18;
  cToolButtonOffset = 14;
  cToolButtonWidth = 18;

procedure InternalStyleManagerChanged(AControl: TWinControl; AStyleManager: TJvNavPaneStyleManager);
var
  Msg: TMsgStyleManagerChange;
  {$IFDEF VisualCLX}
  I: Integer;
  {$ENDIF VisualCLX}
begin
  Msg.Msg := CM_PARENTSTYLEMANAGERCHANGED;
  Msg.Sender := AControl;
  Msg.StyleManager := AStyleManager;
  Msg.Result := 0;
  AControl.Broadcast(Msg);
  {$IFDEF VisualCLX}
  with Msg do
    for I := 0 to AControl.ControlCount - 1 do
      if QMessages.Perform(AControl.Controls[I], Msg, Integer(Sender), Integer(StyleManager)) <> 0 then
        Exit;
  {$ENDIF VisualCLX}
end;

//=== TCustomImageListEx =====================================================

{$IFDEF COMPILER5}
procedure TCustomImageListEx.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean);
const
  DrawingStyles: array[TDrawingStyle] of Longint =
  (ILD_FOCUS, ILD_SELECTED, ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint =
  (0, ILD_MASK);
begin
  if HandleAllocated then
    DoDraw(Index, Canvas, X, Y, DrawingStyles[ADrawingStyle] or
      Images[AImageType], Enabled);
end;
{$ENDIF COMPILER5}

//=== TObjectList ============================================================

type
  TObjectList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

procedure TObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

//=== TJvIconPanel ===========================================================

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
  // Don't free FDropButton: it is freed automatically
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
//  I: Integer;
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
//  for I := 0 to ControlCount - 1 do
//    if Controls[I] is TJvNavIconButton then
//      TJvNavIconButton(Controls[I]).StyleManager := Value;
end;

procedure TJvIconPanel.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropButton.DropDownMenu := Value;
  FDropButton.Visible := Value <> nil;
end;

{$IFDEF VCL}
procedure TJvIconPanel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvIconPanel.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF VisualCLX}

procedure TJvIconPanel.DoDropDownMenu(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
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

procedure TJvIconPanel.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}
procedure TJvIconPanel.CMControlChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvIconPanel.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  inherited;
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VisualCLX}

procedure TJvIconPanel.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStylemanagerChanged(Self, StyleManager);
end;

//=== TJvCustomNavigationPane ================================================

constructor TJvCustomNavigationPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := TJvNavPaneBackgroundImage.Create;
  FBackground.OnChange := DoColorsChange;
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
  FBackground.Free;
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
  else
  if NewSize > ButtonHeight + ButtonHeight div 2 then
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
  I: Integer;
begin
  Result := 0;
  for I := 0 to PageCount - 1 do
    if not NavPages[I].Iconic then
      Inc(Result);
end;

function TJvCustomNavigationPane.HidePage(Page: TJvCustomPage): TJvCustomPage;
begin
  Result := inherited HidePage(Page);
  if Result <> nil then
    UpdatePositions;
end;

function TJvCustomNavigationPane.ShowPage(Page: TJvCustomPage; PageIndex: integer): TJvCustomPage;
begin
  Result := inherited ShowPage(Page, PageIndex);
  if Result <> nil then
    UpdatePositions;
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
    if AComponent = StyleManager then
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
  I, ACount: Integer;
begin
  ACount := MaximizedCount;
  if Value < 0 then
    Value := 0;
  if Value > PageCount then
    Value := PageCount;
  if Value = MaximizedCount then
    Exit;
  while ACount > Value do
  begin
    HidePanel(ACount - 1);
    Dec(ACount);
  end;
  if Value > ACount then
    for I := Value downto ACount do
      ShowPanel(I - 1);
  UpdatePositions;
end;

{$IFDEF VCL}
procedure TJvCustomNavigationPane.WMNCPaint(var Msg: TWMNCPaint);
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
  I, X, Y: Integer;
begin
  if (csDestroying in ComponentState) or (FIconPanel = nil) then
    Exit;
  DisableAlign;
  FIconPanel.DisableAlign;
  try
    Y := 0;
    X := 0;
    FSplitter.Top := Y;
    FIconPanel.FDropButton.Left := Width;
    FIconPanel.Top := Height - FIconPanel.Height;
    Inc(Y, FSplitter.Height);
    for I := 0 to PageCount - 1 do
    begin
      if (NavPages[I].NavPanel = nil) or (NavPages[I].IconButton = nil) then
        Exit;
      NavPages[I].IconButton.Left := X;
      Inc(X, NavPages[I].IconButton.Width);
      NavPages[I].NavPanel.Top := Y;
      Inc(Y, NavPages[I].NavPanel.Height);
      NavPages[I].Invalidate;
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
  inherited Loaded;
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
  inherited SetActivePage(Page);
  if ActivePage <> nil then
  begin
    TJvNavPanelPage(ActivePage).NavPanel.Down := True;
    TJvNavPanelPage(ActivePage).IconButton.Down := True;
    TJvNavPanelPage(ActivePage).NavPanel.Invalidate;
    TJvNavPanelPage(ActivePage).IconButton.Invalidate;
    ActivePage.Invalidate;
  end;
end;

{$IFDEF VCL}
procedure TJvCustomNavigationPane.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if ActivePage = nil then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    FBackground.DrawImage(Canvas, ClientRect);
  end;
  Msg.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvCustomNavigationPane.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;

procedure TJvCustomNavigationPane.Paint;
begin
  //  inherited;
  if ActivePage = nil then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    FBackground.DrawImage(Canvas, ClientRect);
  end;
end;

{$ENDIF VisualCLX}

procedure TJvCustomNavigationPane.SetBackground(const Value: TJvNavPaneBackgroundImage);
begin
  FBackground.Assign(Value);
end;

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
    Result := ((StyleManager = nil) or (StyleManager.Theme = nptCustom)) and ((Name <> F.Name) or (Size <> F.Size) or (Style <> [fsBold]) or
      (Color <> F.Color) or (Pitch <> F.Pitch) or (Charset <> F.CharSet));
end;

function TJvCustomNavigationPane.IsNavPanelFontHotTrackStored: Boolean;
begin
  Result := IsNavPanelHotTrackFontOptionsStored or IsNavPanelFontStored;
end;

function TJvCustomNavigationPane.IsNavPanelHotTrackFontOptionsStored: Boolean;
begin
  Result := not (hoFollowFont in NavPanelHotTrackFontOptions);
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

procedure TJvCustomNavigationPane.SetAutoHeaders(const Value: Boolean);
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

procedure TJvCustomNavigationPane.SetWordWrap(const Value: Boolean);
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
  for I := 0 to PageCount - 1 do
  begin
    NavPages[I].AutoHeader := AutoHeaders;
    NavPages[I].NavPanel.Height := ButtonHeight;
    NavPages[I].IconButton.Width := ButtonWidth;
    NavPages[I].NavPanel.Colors := Colors;
    NavPages[I].IconButton.Colors := Colors;
    NavPages[I].NavPanel.HotTrackFontOptions := NavPanelHotTrackFontOptions;
    NavPages[I].NavPanel.Font := FNavPanelFont;
    NavPages[I].NavPanel.HotTrackFont := FNavPanelHotTrackFont;

    NavPages[I].WordWrap := WordWrap;
    NavPages[I].Alignment := Alignment;
    NavPages[I].NavPanel.Images := LargeImages;
    if AutoHeaders then
      NavPages[I].Header.Images := LargeImages;
    NavPages[I].IconButton.Images := SmallImages;
    //    NavPages[I].StyleManager := StyleManager;
  end;
end;

procedure TJvCustomNavigationPane.DoDropDownMenu(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
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

procedure TJvCustomNavigationPane.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}
procedure TJvCustomNavigationPane.CMControlChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomNavigationPane.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  inherited ControlsListChanged(Control, Inserting);
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VisualCLX}

procedure TJvCustomNavigationPane.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

function TJvCustomNavigationPane.IsColorsStored: Boolean;
begin
  Result := (StyleManager = nil) or (StyleManager.Theme = nptCustom);
end;

//=== TJvNavIconButton ======================================================

constructor TJvNavIconButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  inherited Destroy;
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
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
    else
    if AComponent = StyleManager then
      StyleManager := nil;
end;

procedure TJvNavIconButton.Paint;
var
  Rect: TRect;
  P: TPoint;
  I: Integer;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    Brush.Style := bsClear;
    InflateRect(Rect, 0, -1);
    if bsMouseInside in MouseStates then
    begin
      if bsMouseDown in MouseStates then
        GradientFillRect(Canvas, Rect, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32)
      else
        GradientFillRect(Canvas, Rect, Colors.ButtonHotColorFrom, Colors.ButtonHotColorTo, fdTopToBottom, 32)
    end
    else
    if Down then
      GradientFillRect(Canvas, Rect, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32);
    case ButtonType of
      nibDropDown:
        begin // area should be 7x12
          InflateRect(Rect, -((Rect.Right - Rect.Left) - 7) div 2, -((Rect.Bottom - Rect.Top) - 12) div 2);
          if bsMouseDown in MouseStates then
            OffsetRect(Rect, 1, 1);
          Canvas.Pen.Color := clBlack;
          P.X := Rect.Left;
          P.Y := Rect.Top;
          // chevron, upper
          for I := 0 to 2 do
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
          for I := 0 to 2 do
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
          for I := 0 to 3 do
          begin
            Canvas.MoveTo(P.X + I, P.Y + I);
            Canvas.LineTo(P.X + 7 - I, P.Y + I);
          end;
        end;
      nibImage:
        begin
          if (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
            // draw image only
            TCustomImageListEx(Images).Draw(Canvas,
              (Width - Images.Width) div 2 + Ord(bsMouseDown in MouseStates),
              (Height - Images.Height) div 2 + Ord(bsMouseDown in MouseStates),
              ImageIndex, {$IFDEF VisualCLX} itImage, {$ENDIF} Enabled);
        end;
      nibDropArrow:
        begin
          // area should be 9 x 5, centered
          P.X := Rect.Left + (RectWidth(Rect) - 9) div 2 + Ord(bsMouseDown in MouseStates);
          P.Y := Rect.Top + (RectHeight(Rect) - 5) div 2 + Ord(bsMouseDown in MouseStates);
          Canvas.Pen.Color := clBlack;
          for I := 0 to 4 do
          begin
            Canvas.MoveTo(P.X + I, P.Y + I);
            Canvas.LineTo(P.X + 9 - I, P.Y + I);
          end;
        end;
      nibClose:
        begin
          // area should be 8 x 8, centered
          P.X := (RectWidth(ClientRect) - 8) div 2 + Ord(bsMouseDown in MouseStates);
          P.Y := (RectHeight(ClientRect) - 8) div 2 + Ord(bsMouseDown in MouseStates);
          Canvas.Pen.Color := clBlack;
          for I := 0 to 7 do
          begin
            Canvas.MoveTo(P.X + I, P.Y + I);
            Canvas.LineTo(P.X + I + 2, P.Y + I);
          end;
          Inc(P.X, 7);
          for I := 0 to 7 do
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

procedure TJvNavIconButton.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

function TJvNavIconButton.IsColorsStored: Boolean;
begin
  Result := (StyleManager = nil) or (StyleManager.Theme = nptCustom);
end;

//=== TJvNavPanelButton ======================================================

constructor TJvNavPanelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  inherited Destroy;
end;

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
  inherited FontChanged;
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
//  cAlignment: array [TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
//  cWordWrap: array [Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  R: TRect;
  X, Y: Integer;

  function IsValidImage: Boolean;
  begin
    Result := Assigned(Images) and (ImageIndex >= 0);
  end;

begin
  R := ClientRect;
  if HotTrack and (bsMouseInside in MouseStates) then
  begin
    if bsMouseDown in MouseStates then
      GradientFillRect(Canvas, R, Colors.ButtonSelectedColorTo, Colors.ButtonSelectedColorFrom, fdTopToBottom, 32)
    else
      GradientFillRect(Canvas, R, Colors.ButtonHotColorFrom, Colors.ButtonHotColorTo, fdTopToBottom, 32);
  end
  else
  if Down then
    GradientFillRect(Canvas, R, Colors.ButtonSelectedColorFrom, Colors.ButtonSelectedColorTo, fdTopToBottom, 32)
  else
  if bsMouseDown in MouseStates then
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

procedure TJvNavPanelButton.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

{$IFDEF VCL}
procedure TJvNavPanelButton.CMDialogChar(var Msg: TCMDialogChar);
begin
  if IsAccel(Msg.CharCode, Caption) then
  begin
    Msg.Result := 1;
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

procedure TJvNavPanelButton.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

function TJvNavPanelButton.IsColorsStored: Boolean;
begin
  Result := (StyleManager = nil) or (StyleManager.Theme = nptCustom);
end;

//=== TJvNavPanelColors ======================================================

constructor TJvNavPanelColors.Create;
begin
  inherited Create;
  // use XPBlue as standard
  FButtonColorFrom := TColor($F7E2CD);
  FButtonColorTo := TColor($F3A080);
  FButtonSelectedColorFrom := TColor($BBE2EA);
  FButtonSelectedColorTo := TColor($389FDD);
  FFrameColor := TColor($6F2F0C);
  FButtonHotColorFrom := TColor($DBFBFF);
  FButtonHotColorTo := TColor($5FC8FB);
  FDividerColorFrom := TColor($FFDBBC);
  FDividerColorTo := TColor($F2C0A4);
  FHeaderColorFrom := TColor($D0835C);
  FHeaderColorTo := TColor($903B09);
  FSplitterColorFrom := TColor($B78676);
  FSplitterColorTo := TColor($A03D09);
  FButtonSeparatorColor := clGray;
  FToolPanelColorFrom := clWindow;
  FToolPanelColorTo := clWindow;
  FToolPanelHeaderColorFrom := TColor($F7E2CD);
  FToolPanelHeaderColorTo := TColor($F3A080);
end;

procedure TJvNavPanelColors.Assign(Source: TPersistent);
begin
  if (Source is TJvNavPanelColors) then
  begin
    if (Source <> Self) then
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
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvNavPanelColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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

procedure TJvNavPanelColors.SetToolPanelColorFrom(const Value: TColor);
begin
  if FToolPanelColorFrom <> Value then
  begin
    FToolPanelColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetToolPanelColorTo(const Value: TColor);
begin
  if FToolPanelColorTo <> Value then
  begin
    FToolPanelColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetToolPanelHeaderColorFrom(
  const Value: TColor);
begin
  if FToolPanelHeaderColorFrom <> Value then
  begin
    FToolPanelHeaderColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetToolPanelHeaderColorTo(const Value: TColor);
begin
  if FToolPanelHeaderColorTo <> Value then
  begin
    FToolPanelHeaderColorTo := Value;
    Change;
  end;
end;

//=== TJvNavPanelFonts =======================================================

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
  inherited Destroy;
end;

procedure TJvNavPanelFonts.Assign(Source: TPersistent);
begin
  if Source is TJvNavPanelFonts then
  begin
    if Source <> Self then
    begin
      NavPanelFont := TJvNavPanelFonts(Source).NavPanelFont;
      DividerFont := TJvNavPanelFonts(Source).DividerFont;
      HeaderFont := TJvNavPanelFonts(Source).HeaderFont;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvNavPanelFonts.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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

procedure TJvNavPanelFonts.SetNavPanelHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FNavPanelHotTrackFontOptions <> Value then
  begin
    FNavPanelHotTrackFontOptions := Value;
    Change;
  end;
end;

//=== TJvNavPanelPage ========================================================

constructor TJvNavPanelPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := TJvNavPaneBackgroundImage.Create;
  FBackground.OnChange := DoBackgroundChange;
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
  FBackground.Free;
  inherited Destroy;
end;

procedure TJvNavPanelPage.DoButtonClick(Sender: TObject);
begin
  if not NavPanel.Down then
  begin
    if Parent <> nil then
      TJvCustomNavigationPane(Parent).ActivePage := Self; // this sets "Down" as well
    if Assigned(FOnClick) then
      FOnClick(Self);
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
    if AComponent = IconPanel then
      IconPanel := nil
    else
    if AComponent = StyleManager then
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

procedure TJvNavPanelPage.SetParent({$IFDEF VisualCLX} const {$ENDIF} AParent: TWinControl);
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

procedure TJvNavPanelPage.SetAutoHeader(const Value: Boolean);
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

function TJvNavPanelPage.GetAutoHeader: Boolean;
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

function TJvNavPanelPage.GetWordWrap: Boolean;
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

procedure TJvNavPanelPage.SetWordWrap(const Value: Boolean);
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

procedure TJvNavPanelPage.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}
procedure TJvNavPanelPage.CMControlChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvNavPanelPage.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  inherited ControlsListChanged(Control, Inserting);
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VisualCLX}


procedure TJvNavPanelPage.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

procedure TJvNavPanelPage.SetBackground(const Value: TJvNavPaneBackgroundImage);
begin
  FBackground.Assign(Value);
end;

procedure TJvNavPanelPage.DoBackgroundChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelPage.Paint;
begin
  inherited;
  if FBackground.HasImage then
    FBackground.DrawImage(Canvas, ClientRect)
  else
  if (Parent is TJvCustomNavigationPane) and TJvCustomNavigationPane(Parent).Background.HasImage then
    TJvCustomNavigationPane(Parent).Background.DrawImage(Canvas, ClientRect);
end;

function TJvNavPanelPage.GetAction: TBasicAction;
begin
  {$IFDEF COMPILER6_UP}
  Result := inherited GetAction;
  {$ELSE}
  Result := inherited Action;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvNavPanelPage.SetAction(const Value: TBasicAction);
begin
  inherited Action := Value;
  FNavPanel.Action := Value;
  FIconButton.Action := Value;
end;

procedure TJvNavPanelPage.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
        Self.Caption := Caption;
      if not CheckDefaults or Self.Enabled then
        Self.Enabled := Enabled; // NB! This disables resizing if the top-most button is disabled (due to TSplitter.FindControl)
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

//=== TJvOutlookSplitter =====================================================

constructor TJvOutlookSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FColorFrom := TColor($B78676);
  FColorTo := TColor($A03D09);
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

procedure TJvOutlookSplitter.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
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
    if AComponent = StyleManager then
      StyleManager := nil;
end;

procedure TJvOutlookSplitter.Paint;
var
  I: Integer;
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
      for I := 0 to 9 do // draw the dots
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
      for I := 0 to 9 do // draw the dots
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

procedure TJvOutlookSplitter.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}

function TJvOutlookSplitter.GetDragZoneRect: TRect;
begin
  Result := ClientRect;
  if DragZone <> 0 then
  begin
    if Align in [alLeft, alRight] then
    begin
      if DragZone < RectHeight(Result) then
      begin
        Result.Top := (RectHeight(Result) - DragZone) div 2;
        Result.Bottom := Result.Top + DragZone;
      end;
    end
    else
    if Align in [alTop, alBottom] then
    begin
      if DragZone < RectWidth(Result) then
      begin
        Result.Left := (RectWidth(Result) - DragZone) div 2;
        Result.Right := Result.Left + DragZone;
      end;
    end;
  end;
end;

function TJvOutlookSplitter.MouseInDragZone(X, Y: Integer): boolean;
begin
  Result := (DragZone <= 0) or PtInRect(GetDragZoneRect, Point(X, Y));
end;

procedure TJvOutlookSplitter.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if MouseInDragZone(Message.XPos, Message.YPos) then
    inherited;
end;

procedure TJvOutlookSplitter.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if MouseInDragZone(Message.XPos, Message.YPos) then
  begin
    if Cursor <> FOldCursor then
      inherited Cursor := FOldCursor;
  end
  else
  begin
    if Cursor <> crDefault then
    begin
      FOldCursor := Cursor;
      inherited Cursor := crDefault;
    end;
  end;
end;

procedure TJvOutlookSplitter.SetCursor(const Value: TCursor);
begin
  inherited Cursor := Value;
  FOldCursor := Value;
end;

{$ENDIF VCL}

//=== TJvNavPanelHeader ======================================================

constructor TJvNavPanelHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  FColorFrom := TColor($D0835C);
  FColorTo := TColor($903B09);
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
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
    else
    if AComponent = StyleManager then
      StyleManager := nil;
end;

procedure TJvNavPanelHeader.Paint;
const
  cAlignment: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
  cWordWrap: array[Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  R, TempRect: TRect;
  X, Y, H: Integer;

  function IsValidImage: Boolean;
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
      {$IFDEF VisualCLX} itImage, {$ENDIF} True);
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
  inherited TextChanged;
  Invalidate;
end;

{$IFDEF VCL}
procedure TJvNavPanelHeader.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
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

procedure TJvNavPanelHeader.SetWordWrap(const Value: Boolean);
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

procedure TJvNavPanelHeader.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}
procedure TJvNavPanelHeader.CMControlChange(var Msg: TMessage);
begin
  // a control was inserted or removed
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvNavPanelHeader.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  inherited ControlsListChanged(Control, Inserting);
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VisualCLX}


//=== TJvNavPanelDivider =====================================================

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
  FColorFrom := TColor($FFDBBC);
  FColorTo := TColor($F2C0A4);
  FFrameColor := TColor($6F2F0C);
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

procedure TJvNavPanelDivider.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
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

procedure TJvNavPanelDivider.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

//=== TJvNavPaneStyleManager =================================================

constructor TJvNavPaneStyleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  FColors := TJvNavPanelColors.Create;
  FFonts := TJvNavPanelFonts.Create;
  FColors.OnChange := DoThemeChange;
  FFonts.OnChange := DoThemeChange;
  FTheme := nptCustom; // (p3) required to trigger the change method
  Theme := nptStandard;
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
  else
  if Source is TJvIconPanel then
    SourceColors := TJvIconPanel(Source).Colors
  else
  if Source is TJvNavIconButton then
    SourceColors := TJvNavIconButton(Source).Colors
  else
  if Source is TJvNavPanelButton then
    SourceColors := TJvNavPanelButton(Source).Colors
  else
  if Source is TJvNavPanelPage then
    SourceColors := TJvNavPanelPage(Source).Colors
  else
  if Source is TJvCustomNavigationPane then
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
  else
  if Dest is TJvIconPanel then
    DestColors := TJvIconPanel(Dest).Colors
  else
  if Dest is TJvNavIconButton then
    DestColors := TJvNavIconButton(Dest).Colors
  else
  if Dest is TJvNavPanelButton then
    DestColors := TJvNavPanelButton(Dest).Colors
  else
  if Dest is TJvNavPanelPage then
    DestColors := TJvNavPanelPage(Dest).Colors
  else
  if Dest is TJvCustomNavigationPane then
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

procedure TJvNavPaneStyleManager.DoThemeChange(Sender: TObject);
begin
  Theme := nptCustom;
  Change;
end;

procedure TJvNavPaneStyleManager.RegisterChanges(Value: TJvNavStyleLink);
begin
  Value.Sender := Self;
  if FClients <> nil then
    FClients.Add(Value);
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
  if FTheme <> Value then
  begin
    FColors.OnChange := nil;
    FFonts.OnChange := nil;
    try
      case Value of
        nptStandard:
          begin
            FColors.ButtonColorFrom := TColor($FFFFFF);
            FColors.ButtonColorTo := TColor($BDBEBD);
            FColors.ButtonSelectedColorFrom := TColor($DECFCE);
            FColors.ButtonSelectedColorTo := TColor($DECFCE);
            FColors.FrameColor := TColor($848484);
            FColors.ButtonHotColorFrom := TColor($C68284);
            FColors.ButtonHotColorTo := TColor($C68284);
            FColors.DividerColorFrom := TColor($EFF3EF);
            FColors.DividerColorTo := TColor($C6C3C6);
            FColors.HeaderColorFrom := TColor($848284);
            FColors.HeaderColorTo := TColor($848284);
            FColors.SplitterColorFrom := TColor($C6C3C6);
            FColors.SplitterColorTo := TColor($8C8E8C);
            FColors.ButtonSeparatorColor := clGray;
            FColors.FToolPanelColorFrom := clWindow;
            FColors.FToolPanelColorTo := clWindow;
            FColors.FToolPanelHeaderColorFrom := TColor($FFFFFF);
            FColors.FToolPanelHeaderColorTo := TColor($BDBEBD);

            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindow;
            FFonts.DividerFont.Color := clWindow;
          end;
        nptXPBlue:
          begin
            FColors.ButtonColorFrom := TColor($F7E2CD);
            FColors.ButtonColorTo := TColor($F3A080);
            FColors.ButtonSelectedColorFrom := TColor($BBE2EA);
            FColors.ButtonSelectedColorTo := TColor($389FDD);
            FColors.FrameColor := TColor($6F2F0C);
            FColors.ButtonHotColorFrom := TColor($DBFBFF);
            FColors.ButtonHotColorTo := TColor($5FC8FB);
            FColors.DividerColorFrom := TColor($FFDBBC);
            FColors.DividerColorTo := TColor($F2C0A4);
            FColors.HeaderColorFrom := TColor($D0835C);
            FColors.HeaderColorTo := TColor($903B09);
            FColors.SplitterColorFrom := TColor($B78676);
            FColors.SplitterColorTo := TColor($A03D09);
            FColors.ButtonSeparatorColor := clGray;

            FColors.FToolPanelColorFrom := clWindow;
            FColors.FToolPanelColorTo := clWindow;
            FColors.FToolPanelHeaderColorFrom := TColor($F7E2CD);
            FColors.FToolPanelHeaderColorTo := TColor($F3A080);

            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindowText;
            FFonts.DividerFont.Color := clWindowText;
          end;
        nptXPSilver:
          begin
            FColors.ButtonColorFrom := TColor($F4E2E1);
            FColors.ButtonColorTo := TColor($B09494);
            FColors.ButtonSelectedColorFrom := TColor($BBE2EA);
            FColors.ButtonSelectedColorTo := TColor($389FDD);
            FColors.FrameColor := TColor($527D92);
            FColors.ButtonHotColorFrom := TColor($DBFBFF);
            FColors.ButtonHotColorTo := TColor($5FC8FB);
            FColors.DividerColorFrom := TColor($F8F3F4);
            FColors.DividerColorTo := TColor($EADADB);
            FColors.HeaderColorFrom := TColor($BAA8BA);
            FColors.HeaderColorTo := TColor($917275);
            FColors.SplitterColorFrom := TColor($B8ABA9);
            FColors.SplitterColorTo := TColor($81767E);
            FColors.ButtonSeparatorColor := clGray;
            FColors.FToolPanelColorFrom := clWindow;
            FColors.FToolPanelColorTo := clWindow;
            FColors.FToolPanelHeaderColorFrom := TColor($F4E2E1);
            FColors.FToolPanelHeaderColorTo := TColor($B09494);

            FFonts.HeaderFont.Color := clWindow;
            FFonts.NavPanelFont.Color := clWindowText;
            FFonts.NavPanelHotTrackFont.Color := clWindowText;
            FFonts.DividerFont.Color := clWindowText;
          end;
        nptXPOlive:
          begin
            FColors.ButtonColorFrom := TColor($D6F3E3);
            FColors.ButtonColorTo := TColor($93BFB2);
            FColors.ButtonSelectedColorFrom := TColor($BBE2EA);
            FColors.ButtonSelectedColorTo := TColor($389FDD);
            FColors.FrameColor := TColor($5A7972);
            FColors.ButtonHotColorFrom := TColor($DBFBFF);
            FColors.ButtonHotColorTo := TColor($5FC8FB);
            FColors.DividerColorFrom := TColor($D2F4EE);
            FColors.DividerColorTo := TColor($B5DFD8);
            FColors.HeaderColorFrom := TColor($94BFB4);
            FColors.HeaderColorTo := TColor($427665);
            FColors.SplitterColorFrom := TColor($758D81);
            FColors.SplitterColorTo := TColor($3A584D);
            FColors.ButtonSeparatorColor := clGray;
            FColors.ToolPanelColorFrom := clWindow;
            FColors.ToolPanelColorTo := clWindow;
            FColors.ToolPanelHeaderColorFrom := TColor($D6F3E3);
            FColors.ToolPanelHeaderColorTo := TColor($93BFB2);

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

function TJvNavPaneStyleManager.IsColorsStored: Boolean;
begin
  Result := Theme = nptCustom;
end;

function TJvNavPaneStyleManager.IsFontsStored: Boolean;
begin
  Result := Theme = nptCustom;
end;

//=== TJvNavStyleLink ========================================================

destructor TJvNavStyleLink.Destroy;
begin
  if Sender is TJvNavPaneStyleManager then
    TJvNavPaneStyleManager(Sender).UnRegisterChanges(Self);
  inherited Destroy;
end;

procedure TJvNavStyleLink.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

//=== TJvNavPaneToolPanel ====================================================

constructor TJvNavPaneToolPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaderVisible := True;
  ParentColor := False;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoImagesChange;
  FBackground := TJvNavPaneBackgroundImage.Create;
  FBackground.OnChange := DoImagesChange;

  ControlStyle := [csAcceptsControls, {$IFDEF VCL} csCaptureMouse, {$ENDIF VCL} csClickEvents,
  csOpaque, csDoubleClicks, csReplicatable];

  FButtons := TJvNavPaneToolButtons.Create(Self);
  FStyleLink := TJvNavStyleLink.Create;
  FStyleLink.OnChange := DoStyleChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  {
  FColorFrom := TColor($F7E2CD);
  FColorTo := TColor($F3A080);
  FButtonColor := TColor($A03D09);
  }
  FButtonWidth := 25;
  FButtonHeight := 22;
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
  FBackground.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure TJvNavPaneToolPanel.ButtonsChanged;
var
  I: Integer;
  B: TJvNavPanelToolButton;
begin
  if HeaderVisible then
    for I := 0 to Buttons.Count - 1 do
    begin
      B := Buttons[I].Button;
      B.Visible := False;
      B.SetBounds(0, 0, ButtonWidth - 3, ButtonHeight - 2);
      B.Images := Images;
      if B.Action = nil then
        B.OnClick := InternalButtonClick;
      B.Tag := I;
      B.Parent := Self;
    end;
  Invalidate;
end;

procedure TJvNavPaneToolPanel.DoCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvNavPaneToolPanel.DoDropDownMenu(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
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
  Colors := (Sender as TJvNavPaneStyleManager).Colors;
end;

procedure TJvNavPaneToolPanel.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

function TJvNavPaneToolPanel.GetCloseButton: Boolean;
begin
  Result := FCloseButton.Visible; // and HeaderVisible;
end;

function TJvNavPaneToolPanel.GetDropDownMenu: TPopupMenu;
begin
  Result := FDropDown.DropDownMenu;
end;

function TJvNavPaneToolPanel.GetHitTestInfoAt(X, Y: Integer): TJvToolPanelHitTestInfos;

  function InRange(Value, Min, Max: Integer): Boolean;
  begin
    Result := (Value >= Min) and (Value <= Max);
  end;

begin
  if not Visible then
  begin
    Result := [phtNowhere];
    Exit;
  end;
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
    if InRange(X, 0, ClientWidth) then
    begin
      Include(Result, phtHeader);
      if (X <= 16) and ShowGrabber then
        Include(Result, phtGrabber);
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
    if AComponent = Images then
      Images := nil
    else
    if AComponent = StyleManager then
      StyleManager := nil
    else
    if AComponent = DropDownMenu then
      DropDownMenu := nil;
  end;
end;

procedure TJvNavPaneToolPanel.Paint;
var
  R, R2: TRect;
  I, X, Y: Integer;
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
    if HeaderVisible then
      Inc(R.Top, HeaderHeight);
    GradientFillRect(Canvas, R, Colors.ToolPanelColorFrom, Colors.ToolPanelColorTo, fdTopToBottom, 255);
    FBackground.DrawImage(Canvas, R);
    R := ClientRect;
    if HeaderVisible then
    begin
      R.Bottom := HeaderHeight - EdgeRounding;
      R.Bottom := R.Top + HeaderHeight;
      GradientFillRect(Canvas, R, Colors.ToolPanelHeaderColorFrom, Colors.ToolPanelHeaderColorTo, fdTopToBottom, 255);
      // draw the drag dots
      R2 := Rect(R.Left, R.Top + (HeaderHeight - cToolButtonHeight) div 2 + 2, R.Left + 2, R.Top + (HeaderHeight - cToolButtonHeight) div 2 + 4);
      OffsetRect(R2, 6, 0);
      if ShowGrabber then
      begin
        for I := 0 to 3 do
        begin
          Canvas.Brush.Color := clWhite;
          OffsetRect(R2, 1, 1);
          Canvas.FillRect(R2);
          Canvas.Brush.Color := Colors.FrameColor;
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
      if (DropDownMenu = nil) and not (csDesigning in ComponentState) then
      begin
        OffsetRect(R, 2, -1); // line up with where button caption should have been
        SetBkMode(Canvas.Handle, TRANSPARENT);
        DrawText(Canvas, Caption, Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
      end;

      // draw the client areas top rounding, set pixels directly to avoid messing up any background image

      // just a simple "arrow" in each corner in the same color as the gradient
      // left corner
      Y := HeaderHeight;
      X := 0;
      for I := 0 to 3 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Inc(X);
      for I := 0 to 2 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Inc(X);
      for I := 0 to 1 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Inc(X);
      Canvas.Pixels[X, Y] := Colors.ToolPanelHeaderColorTo;
      //    Inc(X);
      //    Canvas.Pixels[X, Y] := Colors.HeaderColorTo;

      // right corner
      Y := HeaderHeight;
      X := ClientWidth - 1;
      for I := 0 to 4 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Dec(X);
      for I := 0 to 2 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Dec(X);
      for I := 0 to 1 do
        Canvas.Pixels[X, Y + I] := Colors.ToolPanelHeaderColorTo;
      Dec(X);
      Canvas.Pixels[X, Y] := Colors.ToolPanelHeaderColorTo;
      Dec(X);
      Canvas.Pixels[X, Y] := Colors.HeaderColorTo;

      // draw the button area
      R := ClientRect;
      Inc(R.Top, HeaderHeight);
      Inc(R.Right);
      Canvas.Brush.Color := Colors.FrameColor;
      Canvas.Pen.Style := psClear;
      if Buttons.Count > 0 then
      begin
        R2 := Rect(R.Left, R.Top, R.Left + ButtonWidth * Buttons.Count - 1, R.Top + ButtonHeight);
        Canvas.RoundRect(R2.Left, R2.Top, R2.Right, R2.Bottom, EdgeRounding, EdgeRounding);
        // square two corners
        Canvas.FillRect(Rect(R2.Right - EdgeRounding, R2.Top, R2.Right - 1, R2.Top + EdgeRounding));
        Canvas.FillRect(Rect(R2.Left, R2.Bottom - EdgeRounding, R2.Left + EdgeRounding, R2.Bottom - 1));
        Canvas.Pen.Style := psSolid;
        Y := R2.Top;
        // adjust the buttons and draw the dividers
        for I := 0 to Buttons.Count - 1 do
        begin
          X := R2.Left + ButtonWidth * I;
          B := Buttons[I].Button;
          B.SetBounds(X + 3, Y + 2, ButtonWidth - 6, ButtonHeight - 4);
          B.Visible := True;
          if I > 0 then
          begin
            Canvas.Pen.Color := TColor($E7EBEF);
            Canvas.MoveTo(X, R2.Top + 2);
            Canvas.LineTo(X, R2.Bottom - 3);
          end;
          if I < Buttons.Count - 1 then
          begin
            Canvas.Pen.Color := TColor($CED3D6);
            Canvas.MoveTo(X + ButtonWidth - 1, R2.Top + 1);
            Canvas.LineTo(X + ButtonWidth - 1, R2.Bottom - 4);
          end;
        end;
      end;
    end;
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
var
  AOffset: Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HeaderVisible and ShowGrabber then
    AOffset := cToolButtonOffset
  else
    AOffset := 4;
  if (Parent <> nil) and (HeaderHeight > cToolButtonHeight) then
  begin
    FCloseButton.SetBounds(ClientWidth - cToolButtonWidth - 2, (HeaderHeight - cToolButtonHeight) div 2, cToolButtonWidth, cToolButtonHeight);
    if FCloseButton.Visible or (csDesigning in ComponentState) then
      FDropDown.SetBounds(AOffset, (HeaderHeight - cToolButtonHeight) div 2, ClientWidth - cToolButtonWidth - AOffset - 2, cToolButtonHeight)
    else
      FDropDown.SetBounds(AOffset, (HeaderHeight - cToolButtonHeight) div 2, ClientWidth - AOffset - 4, cToolButtonHeight);
  end
  else
  begin
    FCloseButton.SetBounds(0, 0, 0, 0);
    FDropDown.SetBounds(0, 0, 0, 0);
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

procedure TJvNavPaneToolPanel.SetDropDownMenu(const Value: TPopupMenu);
begin
  if FDropDown.DropDownMenu <> Value then
  begin
    FDropDown.DropDownMenu := Value;
    FDropDown.Visible := (Value <> nil); // and HeaderVisible;
    SetBounds(Left, Top, Width, Height);
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
  I: Integer;
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
    for I := 0 to Buttons.Count - 1 do
      Buttons[I].Button.Images := FImages;
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
      Colors := FStyleManager.Colors;
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

procedure TJvNavPaneToolPanel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvNavPaneToolPanel.WMNCPaint(var Msg: TWMNCPaint);
var
  AColor: TColor;
begin
  AColor := Color;
  Color := Colors.FrameColor;
  inherited;
  Color := AColor;
end;

{$ENDIF VCL}

procedure TJvNavPaneToolPanel.SetParentStyleManager(const Value: Boolean);
begin
  if FParentStyleManager <> Value then
  begin
    FParentStyleManager := Value;
    if FParentStyleManager and (Parent <> nil) then
      {$IFDEF VCL}
      Parent.Perform(CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      QMessages.Perform(Parent, CM_PARENTSTYLEMANAGERCHANGE, 0, 0);
      {$ENDIF VisualCLX}
  end;
end;

{$IFDEF VCL}
procedure TJvNavPaneToolPanel.CMControlChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvNavPaneToolPanel.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  inherited;
  InternalStyleManagerChanged(Self, StyleManager);
end;
{$ENDIF VisualCLX}


procedure TJvNavPaneToolPanel.ParentStyleManagerChange(var Msg: TMessage);
begin
  InternalStyleManagerChanged(Self, StyleManager);
end;

function TJvNavPaneToolPanel.GetDrawPartialMenuFrame: Boolean;
begin
  if FDropDown <> nil then
    Result := FDropDown.DrawPartialMenuFrame
  else
    Result := False;
end;

procedure TJvNavPaneToolPanel.SetDrawPartialMenuFrame(const Value: Boolean);
begin
  if FDropDown <> nil then
    FDropDown.DrawPartialMenuFrame := Value;
end;

procedure TJvNavPaneToolPanel.SetBackground(const Value: TJvNavPaneBackgroundImage);
begin
  FBackground.Assign(Value);
end;

procedure TJvNavPaneToolPanel.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavPaneToolPanel.SetHeaderVisible(const Value: Boolean);
begin
  if FHeaderVisible <> Value then
  begin
    FHeaderVisible := Value;
    FCloseButton.Visible := CloseButton;
    FDropDown.Visible := (FDropDown.DropDownMenu <> nil); //  and Value;
    ButtonsChanged;
  end;
end;

function TJvNavPaneToolPanel.IsColorsStored: Boolean;
begin
  Result := (StyleManager = nil) or (StyleManager.Theme = nptCustom);
end;


//=== TJvNavPaneToolButton ===================================================

constructor TJvNavPaneToolButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRealButton := TJvNavPanelToolButton.Create(nil);
  FRealButton.ButtonType := nibImage;
end;

destructor TJvNavPaneToolButton.Destroy;
begin
  FRealButton.Free;
  inherited;
end;

function TJvNavPaneToolButton.GetAction: TBasicAction;
begin
  Result := FRealButton.Action;
end;

function TJvNavPaneToolButton.GetEnabled: Boolean;
begin
  Result := FRealButton.Enabled;
end;

function TJvNavPaneToolButton.GetHint: string;
begin
  Result := FRealButton.Hint;
end;

function TJvNavPaneToolButton.GetImageIndex: TImageIndex;
begin
  Result := FRealButton.ImageIndex;
end;

procedure TJvNavPaneToolButton.SetAction(const Value: TBasicAction);
begin
  FRealButton.Action := Value;
end;

procedure TJvNavPaneToolButton.SetEnabled(const Value: Boolean);
begin
  FRealButton.Enabled := Value;
end;

procedure TJvNavPaneToolButton.SetHint(const Value: string);
begin
  FRealButton.Hint := Value;
end;

procedure TJvNavPaneToolButton.SetImageIndex(const Value: TImageIndex);
begin
  FRealButton.ImageIndex := Value;
end;

//=== TJvNavPaneToolButtons ==================================================

constructor TJvNavPaneToolButtons.Create(AOwner: TJvNavPaneToolPanel);
begin
  inherited Create(AOwner, TJvNavPaneToolButton);
  FPanel := AOwner;
end;

function TJvNavPaneToolButtons.Add: TJvNavPaneToolButton;
begin
  Result := TJvNavPaneToolButton(inherited Add);
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

//=== TJvNavPanelToolButton ==================================================

constructor TJvNavPanelToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  DrawPartialMenuFrame := False;
  TransparentDown := False;
end;

destructor TJvNavPanelToolButton.Destroy;
begin
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvNavPanelToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
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

procedure TJvNavPanelToolButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TJvNavPanelToolButton.Paint;
label
  DrawButton;
var
  R: TRect;
  I: Integer;
begin
  //  inherited Paint;
  if MouseStates <> [] then
  begin
    Canvas.Pen.Color := TColor($6B2408);
    if bsMouseInside in MouseStates then
      Canvas.Brush.Color := TColor($D6BEB5);
    if (bsMouseDown in MouseStates) or Down then
    begin
      if TransparentDown then
        Canvas.Brush.Style := bsClear; // (p3) don't draw background - looks better IMO
      if (ButtonType = nibDropArrow) and (DropDownMenu <> nil) then
      begin
        Canvas.Brush.Color := clWindow;
        Canvas.Pen.Color := cl3DDkShadow;
        if DrawPartialMenuFrame then
        begin
          Canvas.FillRect(ClientRect); // if Brush.Style = bsClear, this does nothing
          Canvas.MoveTo(0, Height);
          Canvas.LineTo(0, 0);
          Canvas.LineTo(Width - 1, 0);
          Canvas.LineTo(Width - 1, Height);
          // (p3) yucky! first goto in JVCL?!!!
          goto DrawButton;
        end;
      end
      else
        Canvas.Brush.Color := TColor($B59284);
    end;
    Canvas.Rectangle(ClientRect);
  end;
DrawButton:
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
          ImageIndex, {$IFDEF VisualCLX} itImage, {$ENDIF} Enabled);
  else
    raise EJVCLException.CreateRes(@RsEUnsupportedButtonType);
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

//=== TJvNavPaneBackgroundImage ==============================================

constructor TJvNavPaneBackgroundImage.Create;
begin
  inherited Create;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
end;

destructor TJvNavPaneBackgroundImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TJvNavPaneBackgroundImage.CalcRect(ADestRect: TRect): TRect;
var
  w, h, cw, ch: Integer;
  XYAspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ADestRect.Right - ADestRect.Left;
  ch := ADestRect.Bottom - ADestRect.Top;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      XYAspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / XYAspect);
        if h > ch then // woops, too big
        begin
          h := ch;
          w := Trunc(ch * XYAspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * XYAspect);
        if w > cw then // woops, too big
        begin
          w := cw;
          h := Trunc(cw / XYAspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := ADestRect.Left;
    Top := ADestRect.Top;
    Right := ADestRect.Left + w;
    Bottom := ADestRect.Top + h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

procedure TJvNavPaneBackgroundImage.Change;
begin
  FDrawing := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
  FDrawing := False;
end;

procedure TJvNavPaneBackgroundImage.DrawImage(Canvas: TCanvas; ARect: TRect);

  procedure TileImage;
  var
    X, Y: integer;
    G: TGraphic;
  begin
    G := Picture.Graphic;
    X := ARect.Left;
    Y := ARect.Top;
    while Y < ARect.Bottom do
    begin
      Canvas.Draw(X, Y, G); // this doesn't clip on the right or bottom sides of ARect :(
      Inc(X, G.Width);
      if X > ARect.Right then
      begin
        X := ARect.Left;
        Inc(Y, G.Height);
      end;
    end;
  end;

begin
  if (Picture.Graphic = nil) or (Picture.Width = 0) or (Picture.Height = 0) then
    Exit;
  if Tile then
    TileImage
  else
    with Canvas do
      StretchDraw(CalcRect(ARect), Picture.Graphic);
end;

function TJvNavPaneBackgroundImage.HasImage: boolean;
begin
  with Picture do
    Result := (Graphic <> nil) and (Width <> 0) and (Height <> 0);
end;

procedure TJvNavPaneBackgroundImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
begin
  G := Picture.Graphic;
  if G <> nil then
    if not ({$IFDEF VCL} (G is TMetaFile) or {$ENDIF} (G is TIcon)) then
      G.Transparent := FTransparent;
  if not FDrawing then
    Change;
end;

procedure TJvNavPaneBackgroundImage.SetCenter(const Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    PictureChanged(Self);
  end;
end;

procedure TJvNavPaneBackgroundImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvNavPaneBackgroundImage.SetProportional(const Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;
end;

procedure TJvNavPaneBackgroundImage.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;
end;

procedure TJvNavPaneBackgroundImage.SetTile(const Value: Boolean);
begin
  if FTile <> Value then
  begin
    FTile := Value;
    PictureChanged(Self);
  end;
end;

procedure TJvNavPaneBackgroundImage.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    PictureChanged(Self)
  end;
end;



procedure TJvNavPaneToolPanel.AdjustClientRect(var Rect: TRect);
begin
  if HeaderVisible then
  begin
    Rect.Top := Rect.Top + HeaderHeight + EdgeRounding;
    if Buttons.Count > 0 then
      Rect.Top := Rect.Top + ButtonHeight - EdgeRounding;
  end;
  InflateRect(Rect, -2, -2);
  inherited AdjustClientRect(Rect);
end;

initialization
  RegisterClasses([TJvNavPanelPage]);

end.

