{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMenus.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributors: Olivier Sannier [obones att altern dott org]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMenus;

{$I jvcl.inc}
{$I vclonly.inc}

interface

uses
  Windows, Messages, SysUtils, Contnrs, Graphics, Controls, Forms, Classes,
  ExtCtrls, ImgList, Menus,
  JvTypes, JvWndProcHook, JVCLVer, JvFinalize;

const
  // custom painter constants
  DefaultImageBackgroundColor = clBtnFace;
  DefaultMarginColor: TColor = clBlue;

  // xp painter constants
  DefaultXPImageBackgroundColor = TColor($D1D8D8);
  DefaultXPSeparatorColor = TColor($A6A6A6);
  DefaultXPSFBrushColor = TColor($D2BDB6);
  DefaultXPSFPenColor = TColor($6A240A);
  DefaultXPShadowColor = TColor($9D8D88);
  DefaultXPCheckedImageBackColorSelected = TColor($B59285);
  DefaultXPCheckedImageBackColor = TColor($D8D5D4);

type
  // early declarations
  TJvMainMenu = class;
  TJvPopupMenu = class;
  TJvCustomMenuItemPainter = class;

  { Generic types }

  // size of an image
  TJvMenuImageSize = class(TPersistent)
  private
    FHeight: Integer;
    FWidth: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
  end;

  // margins around an image
  TJvImageMargin = class(TPersistent)
  private
    FTop: Integer;
    FLeft: Integer;
    FRight: Integer;
    FBottom: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
  end;

  // the vertical aligment
  TJvVerticalAlignment = (vaTop, vaMiddle, vaBottom);

  { TJvMenuChangeLink}

  // This class should be used by any class that wishes to be notified
  // when the content of the menu has changed. Pass an instance of
  // TJvMenuChangeLink to a TJvMainMenu through RegisterChanges and
  // the OnChange event of your object will be fired whenever it is
  // required. This is done on the same principle as the TImageList.
  // In the JVCL, TJvToolbar uses this principle to automatically
  // adjust its content (and size if autosize is true) when the
  // content of the menu it is linked to has changed.

  // This next type is the event triggered when the menu has changed
  // If Rebuild is true, the menu as had to be rebuilt because of a
  // change in its layout, not in the properties of one of its item.
  // Unfortunately, for a reason yet to be discovered, Rebuild is
  // always false, even when adding or removing items in the menu.
  // As a result any class using this feature should compute its
  // own value for Rebuild and decide upon it, rather than on the
  // original value of Rebuild
  TOnJvMenuChange = procedure(Sender: TJvMainMenu; Source: TMenuItem; Rebuild: Boolean) of object;

  TJvMenuChangeLink = class(TObject)
  private
    FOnChange: TOnJvMenuChange;
  protected
    // triggers the OnChange event.
    // this is protected as it cannot be accessed by any other class
    // except the TJvMainMenu which is located in the same unit
    // (scope only applies outside the unit)
    procedure Change(Sender: TJvMainMenu; Source: TMenuItem; Rebuild: Boolean); dynamic;
  public
    property OnChange: TOnJvMenuChange read FOnChange write FOnChange;
  end;

  { TJvMainMenu }

  // the different styles a menu can get
  TJvMenuStyle = (msStandard, // standard (no raising frames around images)
    msOwnerDraw, // drawn by owner
    msBtnLowered, // drawn as a lowered button
    msBtnRaised, // drawn as a raised button
    msOffice, // drawn as in MSOffice (raising frames around selected images)
    msXP, // drawn as in WinXP (white background, shadow below selected images)
    msItemPainter // drawn by the painter in ItemPainter property
    );

  // the state a menu item can get
  TMenuOwnerDrawState = set of (mdSelected, mdGrayed, mdDisabled, mdChecked,
    mdFocused, mdDefault, mdHotlight, mdInactive);

  // The event trigerred when an item is to be drawn by its owner
  TDrawMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; Rect: TRect;
    State: TMenuOwnerDrawState) of object;

  // The event trigerred when the size of an item is required
  TMeasureMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; var Width,
    Height: Integer) of object;

  // event trigerred when about to draw the menu item and a
  // glyph for it is required. If no handler is provided, the
  // image list will be asked and if not available, no image
  // will be drawn
  TItemParamsEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
    var Graphic: TGraphic; var NumGlyphs: Integer) of object;

  // event triggerred when asking for an image index
  // if no handler is provided, the value in the menu item will
  // be used
  TItemImageEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; var ImageIndex: Integer) of object;

  // the main menu class
  TJvMainMenu = class(TMainMenu)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCursor: TCursor;
    FDisabledImages: TImageList;
    FHotImages: TImageList;
    FImageMargin: TJvImageMargin;
    FImages: TImageList;
    FImageSize: TJvMenuImageSize;
    FShowCheckMarks: Boolean;
    FStyle: TJvMenuStyle;
    FTextMargin: Integer;
    FTextVAlignment: TJvVerticalAlignment;

    FOnDrawItem: TDrawMenuItemEvent;
    FOnMeasureItem: TMeasureMenuItemEvent;
    FOnGetItemParams: TItemParamsEvent;

    FImageChangeLink: TChangeLink;
    FOnGetImageIndex: TItemImageEvent;

    FDisabledImageChangeLink: TChangeLink;
    FOnGetDisabledImageIndex: TItemImageEvent;

    FHotImageChangeLink: TChangeLink;
    FOnGetHotImageIndex: TItemImageEvent;

    FChangeLinks: TObjectList;
    FCanvas: TControlCanvas;

    // This is one is used if Style is not msItemPainter
    FStyleItemPainter: TJvCustomMenuItemPainter;

    // This one is for the ItemPainter property
    FItemPainter: TJvCustomMenuItemPainter;
    function GetCanvas: TCanvas;
    procedure SetItemPainter(const Value: TJvCustomMenuItemPainter);
    function GetActiveItemPainter: TJvCustomMenuItemPainter;
    procedure SetStyle(Value: TJvMenuStyle);
    procedure SetDisabledImages(Value: TImageList);
    procedure SetImages(Value: TImageList);
    procedure SetHotImages(Value: TImageList);
  protected
    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure HotImageListChange(Sender: TObject);
    function FindForm: TWinControl;
    function NewWndProc(var Msg: TMessage): Boolean;
    procedure CMMenuChanged(var Msg: TMessage); message CM_MENUCHANGED;
    procedure WMDrawItem(var Msg: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Msg: TWMMeasureItem); message WM_MEASUREITEM;
    procedure WMMenuSelect(var Msg: TWMMenuSelect); message WM_MENUSELECT;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState; var ImageIndex: Integer); dynamic;
    procedure DrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState); virtual;
    procedure GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor; var Graphic: TGraphic;
      var NumGlyphs: Integer); dynamic;
    procedure MeasureItem(Item: TMenuItem; var Width, Height: Integer); dynamic;
    procedure RefreshMenu(AOwnerDraw: Boolean); virtual;
    function IsOwnerDrawMenu: Boolean;

    // called when the menu has changed. If Rebuild is true, the menu
    // as had to be rebuilt because of a change in its layout, not in
    // the properties of one of its item. Unfortunately, for a reason
    // yet to be discovered, Rebuild is always false, even when adding
    // or removing items in the menu.
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure DefaultDrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);

    // change registering methods
    procedure RegisterChanges(ChangeLink: TJvMenuChangeLink);
    procedure UnregisterChanges(ChangeLink: TJvMenuChangeLink);

    // get the canvas of the menu
    property Canvas: TCanvas read GetCanvas;
    // get the currently used painter
    property ActiveItemPainter: TJvCustomMenuItemPainter read GetActiveItemPainter;
  published
    // Style MUST BE before ItemPainter for the properties of the
    // painter to be correctly read from the DFM file.
    property Style: TJvMenuStyle read FStyle write SetStyle default msStandard;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property HotImages: TImageList read FHotImages write SetHotImages;
    property Images: TImageList read FImages write SetImages;
    property ImageMargin: TJvImageMargin read FImageMargin write FImageMargin;
    property ImageSize: TJvMenuImageSize read FImageSize write FImageSize;
    property ItemPainter: TJvCustomMenuItemPainter read FItemPainter write SetItemPainter;
    property OwnerDraw stored False;
    property ShowCheckMarks: Boolean read FShowCheckMarks write FShowCheckMarks default False;
    property TextMargin: Integer read FTextMargin write FTextMargin default 0;
    property TextVAlignment: TJvVerticalAlignment read FTextVAlignment write FTextVAlignment default vaMiddle;

    property OnGetImageIndex: TItemImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetDisabledImageIndex: TItemImageEvent read FOnGetDisabledImageIndex write FOnGetDisabledImageIndex;
    property OnGetHotImageIndex: TItemImageEvent read FOnGetHotImageIndex write FOnGetHotImageIndex;
    property OnDrawItem: TDrawMenuItemEvent read FOnDrawItem write FOnDrawItem;
    property OnGetItemParams: TItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
    property OnMeasureItem: TMeasureMenuItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

  { TJvPopupMenu }

  // The Popup counterpart of TJvMainMenu
  // does basically the same thing, but in a popup menu
  TJvPopupMenu = class(TPopupMenu)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCursor: TCursor;
    FDisabledImages: TImageList;
    FHotImages: TImageList;
    FImageMargin: TJvImageMargin;
    FImages: TImageList;
    FImageSize: TJvMenuImageSize;
    FShowCheckMarks: Boolean;
    FStyle: TJvMenuStyle;
    FTextMargin: Integer;
    FTextVAlignment: TJvVerticalAlignment;

    FOnDrawItem: TDrawMenuItemEvent;
    FOnMeasureItem: TMeasureMenuItemEvent;
    FOnGetItemParams: TItemParamsEvent;

    FImageChangeLink: TChangeLink;
    FOnGetImageIndex: TItemImageEvent;

    FDisabledImageChangeLink: TChangeLink;
    FOnGetDisabledImageIndex: TItemImageEvent;

    FHotImageChangeLink: TChangeLink;
    FOnGetHotImageIndex: TItemImageEvent;

    FPopupPoint: TPoint;
    FParentBiDiMode: Boolean;
    FCanvas: TControlCanvas;

    // This is one is used if Style is not msItemPainter
    FStyleItemPainter: TJvCustomMenuItemPainter;

    // This one is for the ItemPainter property
    FItemPainter: TJvCustomMenuItemPainter;
    function GetCanvas: TCanvas;
    procedure SetItemPainter(const Value: TJvCustomMenuItemPainter);
    function GetActiveItemPainter: TJvCustomMenuItemPainter;
    procedure SetDisabledImages(Value: TImageList);
    procedure SetImages(Value: TImageList);
    procedure SetHotImages(Value: TImageList);
    procedure SetStyle(Value: TJvMenuStyle);
  protected
    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure HotImageListChange(Sender: TObject);
    procedure WndMessage(Sender: TObject; var AMsg: TMessage;
      var Handled: Boolean);
    procedure WMDrawItem(var Msg: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Msg: TWMMeasureItem); message WM_MEASUREITEM;
    procedure SetBiDiModeFromPopupControl;

    procedure WriteState(Writer: TWriter); override;
    procedure ReadState(Reader: TReader); override;

    procedure Loaded; override;
    function UseRightToLeftAlignment: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
      var ImageIndex: Integer); dynamic;
    procedure DrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState); virtual;
    procedure GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
      AFont: TFont; var Color: TColor; var Graphic: TGraphic;
      var NumGlyphs: Integer); dynamic;
    procedure MeasureItem(Item: TMenuItem; var Width, Height: Integer); dynamic;
    procedure RefreshMenu(AOwnerDraw: Boolean); virtual;
    function IsOwnerDrawMenu: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Refresh;
    procedure Popup(X, Y: Integer); override;
    procedure DefaultDrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);

    property Canvas: TCanvas read GetCanvas;
    // get the currently used painter
    property ActiveItemPainter: TJvCustomMenuItemPainter read GetActiveItemPainter;
  published
    // Style MUST BE before ItemPainter for the properties of the
    // painter to be correctly read from the DFM file.
    property Style: TJvMenuStyle read FStyle write SetStyle default msStandard;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property HotImages: TImageList read FHotImages write SetHotImages;
    property ImageMargin: TJvImageMargin read FImageMargin write FImageMargin;
    property Images: TImageList read FImages write SetImages;
    property ImageSize: TJvMenuImageSize read FImageSize write FImageSize;
    property ItemPainter: TJvCustomMenuItemPainter read FItemPainter write SetItemPainter;
    property OwnerDraw stored False;
    property ShowCheckMarks: Boolean read FShowCheckMarks write FShowCheckMarks default False;
    property TextMargin: Integer read FTextMargin write FTextMargin default 0;
    property TextVAlignment: TJvVerticalAlignment read FTextVAlignment write FTextVAlignment default vaMiddle;

    property OnGetImageIndex: TItemImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetDisabledImageIndex: TItemImageEvent read FOnGetDisabledImageIndex write FOnGetDisabledImageIndex;
    property OnGetHotImageIndex: TItemImageEvent read FOnGetHotImageIndex write FOnGetHotImageIndex;
    property OnDrawItem: TDrawMenuItemEvent read FOnDrawItem write FOnDrawItem;
    property OnGetItemParams: TItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
    property OnMeasureItem: TMeasureMenuItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

  // the event trigerred when the margin of a menu must be drawn
  TJvDrawLeftMarginEvent = procedure(Sender: TMenu; Rect: TRect) of object;

  { TJvCustomMenuItemPainter }

  // This class is the base class for all the menu item painters.
  // Each instance of TJvMainMenu and TJvPopupMenu will contain one
  // instance of one of the descendent which will be be in charge
  // of the painting of menu items. There is one descendent per
  // style in the TJvMenuStyle enumeration
  TJvCustomMenuItemPainter = class(TComponent)
  private
    // property fields
    FImageBackgroundColor: TColor;
    FLeftMargin: Cardinal;
    FOnDrawLeftMargin: TJvDrawLeftMarginEvent;

    // other usage fields
    FMainMenu: TJvMainMenu;
    FPopupMenu: TJvPopupMenu;
    FOnDrawItem: TDrawMenuItemEvent;
    FImageMargin: TJvImageMargin;
    FImageSize: TJvMenuImageSize;

    FItem: TMenuItem;
    FState: TMenuOwnerDrawState;

    FImageIndex: Integer;
    FGlyph: TGraphic;
    FNumGlyphs: Integer;
    FParentMenu: TMenu;
    procedure SetLeftMargin(const Value: Cardinal);
    procedure SetImageBackgroundColor(const Value: TColor);
    function GetMenu: TMenu;
    procedure SetMenu(const Value: TMenu);
    function GetCanvas: TCanvas;

    procedure EmptyDrawItem(Sender: TObject;ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
  protected
    function GetTextWidth(Item: TMenuItem): Integer;
    function GetCheckMarkHeight: Integer; virtual;
    function GetCheckMarkWidth: Integer; virtual;
    function GetComponentState: TComponentState;
    function GetDisabledImages: TImageList;
    function GetDrawHighlight: Boolean; virtual;
    function GetGrayColor: TColor; virtual;
    function GetHotImages: TImageList;
    function GetImageHeight: Integer; virtual;
    function GetImageWidth: Integer; virtual;
    function GetImages: TImageList;
    function GetIsPopup: Boolean;
    function GetIsRightToLeft: Boolean;
    function GetShowCheckMarks: Boolean;
    function GetTextMargin: Integer; virtual;
    function GetTextVAlignment: TJvVerticalAlignment;

    function UseImages: Boolean;
    function UseHotImages: Boolean;
    function UseDisabledImages: Boolean;

    // This procedure will update the fields that are
    // instances of objects derived from TPersistent. This
    // allows for modification in the painter without any impact
    // on the values in the user's object (in his menu)
    procedure UpdateFieldsFromMenu; virtual;

    // draws the background required for a checked item
    // doesn't draw the mark, simply the grey matrix that
    // is shown behind the mark or image
    procedure DrawGlyphCheck(ARect: TRect); virtual;

    // prepare the paint by assigning various fields
    procedure PreparePaint(Item: TMenuItem; ItemRect: TRect;
      State: TMenuOwnerDrawState; Measure: Boolean); virtual;

    // draws the item background
    // does nothing by default
    procedure DrawItemBackground(ARect: TRect); virtual;

    // draws the check mark background
    // does nothing by default
    procedure DrawCheckMarkBackground(ARect: TRect); virtual;

    // draws the image background
    // does nothing by default
    procedure DrawImageBackground(ARect: TRect); virtual;

    // draws the background of the text
    // does nothing by default
    procedure DrawTextBackground(ARect: TRect); virtual;

    // draws a frame for the menu item.
    // will only be called if the menu item is selected (mdSelected in State)
    // and does nothing by default
    procedure DrawSelectedFrame(ARect: TRect); virtual;

    // Draws a disabled bitmap at the given coordinates.
    // The disabled bitmap will be created from the given bitmap.
    // This is only called when the glyph property of the item index
    // is not empty or when the graphic set in the OnItemParams event
    // was a TBitmap or when no image is available for a checked item
    procedure DrawDisabledBitmap(X, Y: Integer; Bitmap: TBitmap); virtual;

    // Draws the menu bitmap at the given coordinates.
    // This is only called when the glyph property of the item index
    // is not empty or when the graphic set in the OnItemParams event
    // was a TBitmap or when no image is available for a checked item
    procedure DrawMenuBitmap(X, Y: Integer; Bitmap: TBitmap); virtual;

    // Draws a disabled image. This is called when the ImageList property
    // is not empty
    procedure DrawDisabledImage(X, Y: Integer); virtual;

    // Draws an enabled image. This is called when the ImageList property
    // is not empty
    procedure DrawEnabledImage(X, Y: Integer); virtual;

    // Draws a check image for the menu item
    // will only be called if the menu item is checked, the menu item is
    // a popup at the time of showing (being a popup meaning not being
    // a top level menu item in a main menu) and the parent menu asks
    // to show check marks or there are no image for the item
    procedure DrawCheckImage(ARect: TRect); virtual;

    // draws the back of an image for a checked menu item.
    // by default, does nothing
    procedure DrawCheckedImageBack(ARect: TRect); virtual;

    // draws the back of an image for a menu item.
    // by default, does nothing
    procedure DrawNotCheckedImageBack(ARect: TRect); virtual;

    // draws a separator
    procedure DrawSeparator(ARect: TRect); virtual;

    // draws the text at the given place
    procedure DrawText(ARect: TRect; const Text: string; Flags: Longint); virtual;

    procedure DrawLeftMargin(ARect: TRect); virtual;
    procedure DefaultDrawLeftMargin(ARect: TRect; StartColor, EndColor: TColor);

    // NEVER STORE Canvas, this value is not to be trusted from the menu
    // it MUST be read everytime it is needed
    property Canvas: TCanvas read GetCanvas;

    // properties read or calculated from the properties of the
    // menu to which the painter is linked
    property CheckMarkHeight: Integer read GetCheckMarkHeight;
    property CheckMarkWidth: Integer read GetCheckMarkWidth;
    property ComponentState: TComponentState read GetComponentState;
    property DisabledImages: TImageList read GetDisabledImages;
    property DrawHighlight: Boolean read GetDrawHighlight;
    property GrayColor: TColor read GetGrayColor;
    property HotImages: TImageList read GetHotImages;
    property Images: TImageList read GetImages;
    property ImageHeight: Integer read GetImageHeight;
    property ImageMargin: TJvImageMargin read FImageMargin;
    property ImageSize: TJvMenuImageSize read FImageSize;
    property ImageWidth: Integer read GetImageWidth;
    property IsPopup: Boolean read GetIsPopup;
    property IsRightToLeft: Boolean read GetIsRightToLeft;
    property ShowCheckMarks: Boolean read GetShowCheckMarks;
    property TextMargin: Integer read GetTextMargin;
    property TextVAlignment: TJvVerticalAlignment read GetTextVAlignment;

    // Left margin properties and events
    property LeftMargin: Cardinal read FLeftMargin write SetLeftMargin default 0;
    property OnDrawLeftMargin: TJvDrawLeftMarginEvent read FOnDrawLeftMargin write FOnDrawLeftMargin;
    property ImageBackgroundColor: TColor read FImageBackgroundColor write SetImageBackgroundColor default DefaultImageBackgroundColor;
  public
    // constructor, will create the objects derived from TPersistent
    // which are stored here (see UpdateFieldsFromMenu)
    constructor Create(AOwner: TComponent); override;

    // This is the menu to which the painter is linked. It MUST be
    // set BEFORE calling any painting function, but no check is made
    // to ensure that this is the case
    property Menu: TMenu read GetMenu write SetMenu;

    // destroys the objects created in create
    destructor Destroy; override;

    // indicates in Width and Height the size of the given menu item
    // if it was painted with this painter
    procedure Measure(Item: TMenuItem; var Width, Height: Integer); virtual;

    // will paint the given item in the given rectangle
    // will call the various virtual functions depending on the
    // state of the menu item
    procedure Paint(Item: TMenuItem; ItemRect: TRect;
      State: TMenuOwnerDrawState); virtual;
  end;

  { TJvOfficeMenuItemPainter }

  // This painter draws an item using the office style
  TJvOfficeMenuItemPainter = class(TJvCustomMenuItemPainter)
  protected
    procedure CleanupGlyph(BtnRect: TRect);
    procedure DrawFrame(BtnRect: TRect);
    function GetDrawHighlight: Boolean; override;
    procedure DrawSelectedFrame(ARect: TRect); override;
    procedure DrawCheckedImageBack(ARect: TRect); override;
    procedure DrawNotCheckedImageBack(ARect: TRect); override;
    procedure UpdateFieldsFromMenu; override;
    function GetTextMargin: Integer; override;
    procedure DrawCheckImage(ARect: TRect); override;
    procedure DrawText(ARect: TRect; const Text: string; Flags: Longint); override;
    procedure DrawItemBackground(ARect: TRect); override;
  public
    procedure Paint(Item: TMenuItem; ItemRect: TRect; State: TMenuOwnerDrawState); override;
  published
    property LeftMargin;
    property OnDrawLeftMargin;
  end;

  // this painter draws an item as a lowered or raised button
  TJvBtnMenuItemPainter = class(TJvCustomMenuItemPainter)
  private
    FLowered: Boolean;
  protected
    procedure DrawSelectedFrame(ARect: TRect); override;
    function GetDrawHighlight: Boolean; override;
    function GetGrayColor: TColor; override;
    procedure UpdateFieldsFromMenu; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; Lowered: Boolean); reintroduce; overload;
  published
    property Lowered: Boolean read FLowered write FLowered;
    property LeftMargin;
    property OnDrawLeftMargin;
  end;

  // this painter is the standard one and as such doesn't do anything
  // more than the ancestor class except publishing properties
  TJvStandardMenuItemPainter = class(TJvCustomMenuItemPainter)
  protected
    procedure DrawCheckedImageBack(ARect: TRect); override;
    procedure UpdateFieldsFromMenu; override;
    function GetTextMargin: Integer; override;
    function GetImageWidth: Integer; override;
  public
    procedure Paint(Item: TMenuItem; ItemRect: TRect; State: TMenuOwnerDrawState); override;
  published
    property LeftMargin;
    property OnDrawLeftMargin;
  end;

  // this painter calls the user supplied events to render the item
  TJvOwnerDrawMenuItemPainter = class(TJvCustomMenuItemPainter)
  public
    procedure Measure(Item: TMenuItem; var Width, Height: Integer); override;
    procedure Paint(Item: TMenuItem; ItemRect: TRect; State: TMenuOwnerDrawState); override;
  end;

  // this painter draws an item using the XP style (white menus,
  // shadows below images...)
  TJvXPMenuItemPainter = class(TJvCustomMenuItemPainter)
  private
    // property fields
    FSelectionFrameBrush: TBrush;
    FSelectionFramePen: TPen;
    FShadowColor: TColor;
    FSeparatorColor: TColor;
    FCheckedImageBackColorSelected: TColor;
    FCheckedImageBackColor: TColor;
    // other usage fields
    FSelRect: TRect;
    FCheckedPoint: TPoint;
  protected
    procedure DrawBitmapShadow(X, Y: Integer; B: TBitmap);
    procedure DrawImageBackground(ARect: TRect); override;
    procedure DrawCheckMarkBackground(ARect: TRect); override;
    procedure PreparePaint(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState; Measure: Boolean); override;
    procedure DrawCheckedImageBack(ARect: TRect); override;
    procedure DrawEnabledImage(X, Y: Integer); override;
    procedure DrawItemBackground(ARect: TRect); override;
    procedure DrawMenuBitmap(X, Y: Integer; Bitmap: TBitmap); override;
    procedure DrawDisabledImage(X, Y: Integer); override;
    procedure DrawSelectedFrame(ARect: TRect); override;
    procedure DrawSeparator(ARect: TRect); override;
    procedure DrawText(ARect: TRect; const Text: string; Flags: Longint); override;
    function GetDrawHighlight: Boolean; override;
    procedure UpdateFieldsFromMenu; override;
    function GetTextMargin: Integer; override;
    procedure DrawCheckImage(ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Measure(Item: TMenuItem; var Width, Height: Integer); override;
    procedure Paint(Item: TMenuItem; ItemRect: TRect;
      State: TMenuOwnerDrawState); override;
  published
    property ImageBackgroundColor default DefaultXPImageBackgroundColor;
    property SelectionFrameBrush: TBrush read FSelectionFrameBrush;
    property SelectionFramePen: TPen read FSelectionFramePen;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor default DefaultXPSeparatorColor;
    property ShadowColor: TColor read FShadowColor write FShadowColor default DefaultXPShadowColor;
    property CheckedImageBackColor: TColor read FCheckedImageBackColor write FCheckedImageBackColor default DefaultXPCheckedImageBackColor;
    property CheckedImageBackColorSelected: TColor read FCheckedImageBackColorSelected write FCheckedImageBackColorSelected default DefaultXPCheckedImageBackColorSelected;
  end;

{ Utility routines }

procedure SetDefaultMenuFont(AFont: TFont);
function UseFlatMenubars: Boolean;

implementation

uses
  CommCtrl, Consts, Math,
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvJCLUtils, JvJVCLUtils;

const
  sUnitName = 'JvMenus';

const
  Separator = '-';

  // The space between a menu item text and its shortcut
  ShortcutSpacing = '        ';

function CreateMenuItemPainterFromStyle(Style: TJvMenuStyle; Menu: TMenu): TJvCustomMenuItemPainter;
begin
  case Style of
    msOwnerDraw:
      Result := TJvOwnerDrawMenuItemPainter.Create(Menu);
    msBtnLowered:
      Result := TJvBtnMenuItemPainter.Create(Menu, True);
    msBtnRaised:
      Result := TJvBtnMenuItemPainter.Create(Menu, False);
    msOffice:
      Result := TJvOfficeMenuItemPainter.Create(Menu);
    msXP:
      Result := TJvXPMenuItemPainter.Create(Menu);
  else
    Result := TJvStandardMenuItemPainter.Create(Menu);
  end;
  Result.Menu := Menu;
end;

function IsItemPopup(Item: TMenuItem): Boolean;
begin
  Result := (Item.Parent = nil) or (Item.Parent.Parent <> nil) or
    not (Item.Parent.Owner is TMainMenu);
end;

function IsWinXP_UP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    (Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
end;

function UseFlatMenubars: Boolean;
const
  SPI_GETFLATMENU = $1022;
var
  B: BOOL;
begin
  Result := IsWinXP_UP and SystemParametersInfo(SPI_GETFLATMENU, 0, @B, 0) and B;
end;

procedure MenuWndMessage(Menu: TMenu; var AMsg: TMessage; var Handled: Boolean);
var
  Mesg: TMessage;
  Item: Pointer;
begin
  with AMsg do
    case Msg of
      WM_MEASUREITEM:
        if (TWMMeasureItem(AMsg).MeasureItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMMeasureItem(AMsg).MeasureItemStruct^.itemID, fkCommand);
          if Item <> nil then
          begin
            Mesg := AMsg;
            TWMMeasureItem(Mesg).MeasureItemStruct^.itemData := Longint(Item);
            Menu.Dispatch(Mesg);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_DRAWITEM:
        if (TWMDrawItem(AMsg).DrawItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMDrawItem(AMsg).DrawItemStruct^.itemID, fkCommand);
          if Item <> nil then
          begin
            Mesg := AMsg;
            TWMDrawItem(Mesg).DrawItemStruct^.itemData := Longint(Item);
            Menu.Dispatch(Msg);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_MENUSELECT:
        Menu.Dispatch(AMsg);
      CM_MENUCHANGED:
        Menu.Dispatch(AMsg);
      WM_MENUCHAR:
        begin
          Menu.ProcessMenuChar(TWMMenuChar(AMsg));
        end;
    end;
end;

procedure SetDefaultMenuFont(AFont: TFont);
var
  NCMetrics: TNonCLientMetrics;
begin
  if NewStyleControls then
  begin
    NCMetrics.cbSize := SizeOf(TNonCLientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCMetrics, 0) then
    begin
      AFont.Handle := CreateFontIndirect(NCMetrics.lfMenuFont);
      Exit;
    end;
  end;
  with AFont do
  begin
    if NewStyleControls then
      Name := 'MS Sans Serif'
    else
      Name := 'System';
    Size := 8;
    Color := clMenuText;
    Style := [];
  end;
  AFont.Color := clMenuText;
end;

procedure MenuLine(Canvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
begin
  with Canvas do
  begin
    Pen.Color := C;
    Pen.Style := psSolid;
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

//=== { TJvMenuChangeLink } ==================================================

procedure TJvMenuChangeLink.Change(Sender: TJvMainMenu; Source: TMenuItem; Rebuild: Boolean);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender, Source, Rebuild);
end;

//=== { TJvMainMenu } ========================================================

constructor TJvMainMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited OwnerDraw := True;
  
  RegisterWndProcHook(FindForm, NewWndProc, hoAfterMsg);
  FStyle := msStandard;
  FStyleItemPainter := CreateMenuItemPainterFromStyle(FStyle, Self);
  FChangeLinks := TObjectList.Create(False);
  FImageMargin := TJvImageMargin.Create;
  FImageSize := TJvMenuImageSize.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;

  // set default values that are not 0
  FTextVAlignment := vaMiddle;
end;

destructor TJvMainMenu.Destroy;
begin
  FImageChangeLink.Free;
  FHotImageChangeLink.Free;
  FDisabledImageChangeLink.Free;
  FStyleItemPainter.Free;
  FChangeLinks.Free;
  FImageMargin.Free;
  FImageSize.Free;
  UnregisterWndProcHook(FindForm, NewWndProc, hoAfterMsg);
  inherited Destroy;
end;

procedure TJvMainMenu.Loaded;
begin
  inherited Loaded;
  if IsOwnerDrawMenu then
    RefreshMenu(True);
end;

function TJvMainMenu.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvMainMenu.IsOwnerDrawMenu: Boolean;
begin
  Result := True; //(FStyle <> msStandard) or (Assigned(FImages) and (FImages.Count > 0));
end;

procedure TJvMainMenu.MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
var
  I: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  for I := 0 to FChangeLinks.Count - 1 do
    TJvMenuChangeLink(FChangeLinks[I]).Change(Self, Source, Rebuild);
  inherited MenuChanged(Sender, Source, Rebuild);
end;

procedure TJvMainMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      SetImages(nil);
    if AComponent = FDisabledImages then
      SetDisabledImages(nil);
    if AComponent = FHotImages then
      SetHotImages(nil);
    if AComponent = FItemPainter then
      ItemPainter := nil;
  end;
end;

procedure TJvMainMenu.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FImages <> nil then
    FImages.UnregisterChanges(FImageChangeLink);
  FImages := Value;
  if Value <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
  // to be used in a standard (non JV) toolbar
  inherited Images := Value;
end;

procedure TJvMainMenu.DisabledImageListChange(Sender: TObject);
begin
  if Sender = FDisabledImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetDisabledImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FDisabledImages <> nil then
    FDisabledImages.UnregisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if Value <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
end;

procedure TJvMainMenu.HotImageListChange(Sender: TObject);
begin
  if Sender = FHotImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetHotImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FHotImages <> nil then
    FHotImages.UnregisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if Value <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
end;

procedure TJvMainMenu.SetStyle(Value: TJvMenuStyle);
begin
  if FStyle <> Value then
  begin
    // store the new style
    FStyle := Value;
    // delete the old painter and create a new internal painter
    // according to the style, but only if the style is not
    // msItemPainter
    if (Style <> msItemPainter) or (ItemPainter = nil) then
    begin
      ItemPainter := nil;
      FStyleItemPainter.Free;
      FStyleItemPainter := CreateMenuItemPainterFromStyle(Value, Self);
    end;
    // refresh
    RefreshMenu(IsOwnerDrawMenu);
  end;
end;

function TJvMainMenu.FindForm: TWinControl;
begin
  Result := FindControl(WindowHandle);
  if (Result = nil) and (Owner is TWinControl) then
    Result := TWinControl(Owner);
end;

procedure TJvMainMenu.Refresh;
begin
  RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.RefreshMenu(AOwnerDraw: Boolean);
begin
  Self.OwnerDraw := AOwnerDraw and not (csDesigning in ComponentState);
end;

procedure TJvMainMenu.DefaultDrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
begin
  if Canvas.Handle <> 0 then
  begin
    GetActiveItemPainter.Menu := Self;
    GetActiveItemPainter.Paint(Item, Rect, State);
  end;
end;

procedure TJvMainMenu.DrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
begin
  if Canvas.Handle <> 0 then
  begin
    GetActiveItemPainter.Menu := Self;
    GetActiveItemPainter.Paint(Item, Rect, State);
  end;
end;

procedure TJvMainMenu.RegisterChanges(ChangeLink: TJvMenuChangeLink);
begin
  FChangeLinks.Add(ChangeLink);
end;

procedure TJvMainMenu.UnregisterChanges(ChangeLink: TJvMenuChangeLink);
begin
  FChangeLinks.Remove(ChangeLink);
end;

procedure TJvMainMenu.MeasureItem(Item: TMenuItem; var Width, Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Item, Width, Height)
end;

{procedure TJvMainMenu.WndMessage(Sender: TObject; var AMsg: TMessage;
  var Handled: Boolean);
begin
  if IsOwnerDrawMenu then
    MenuWndMessage(Self, AMsg, Handled);
end;}

function TJvMainMenu.NewWndProc(var Msg: TMessage): Boolean;
var
  Handled: Boolean;
begin
  if IsOwnerDrawMenu then
    MenuWndMessage(Self, Msg, Handled);
  // let others listen in too...
  Result := False; //handled;
end;

procedure TJvMainMenu.GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
  AFont: TFont; var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Item, State, AFont, Color, Graphic, NumGlyphs);
  if (Item <> nil) and (Item.Caption = Separator) then
    Graphic := nil;
end;

procedure TJvMainMenu.GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
  var ImageIndex: Integer);
begin
  if Assigned(FImages) and (Item <> nil) and (Item.Caption <> Separator) and
    Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item, State, ImageIndex);
end;

procedure TJvMainMenu.CMMenuChanged(var Msg: TMessage);
begin
  inherited;
end;

procedure TJvMainMenu.WMDrawItem(var Msg: TWMDrawItem);
var
  State: TMenuOwnerDrawState;
  SaveIndex: Integer;
  Item: TMenuItem;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TMenuOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {if (mdDisabled in State) then
      State := State - [mdSelected];}
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and
      (FindItem(Item.Command, fkCommand) = Item) then
    begin
      FCanvas := TControlCanvas.Create;
      try
        SaveIndex := SaveDC(hDC);
        try
          Canvas.Handle := hDC;
          SetDefaultMenuFont(Canvas.Font);
          Canvas.Font.Color := clMenuText;
          Canvas.Brush.Color := clMenu;
          if mdDefault in State then
            Canvas.Font.Style := Canvas.Font.Style + [fsBold];
          if (mdSelected in State) and not
            (Style in [msBtnLowered, msBtnRaised]) then
          begin
            Canvas.Brush.Color := clHighlight;
            Canvas.Font.Color := clHighlightText;
          end;
          with rcItem do
            IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
          DrawItem(Item, rcItem, State);
          Canvas.Handle := 0;
        finally
          RestoreDC(hDC, SaveIndex);
        end;
      finally
        Canvas.Free;
      end;
    end;
  end;
end;

procedure TJvMainMenu.WMMeasureItem(var Msg: TWMMeasureItem);
var
  Item: TMenuItem;
  SaveIndex: Integer;
  DC: HDC;
begin
  with Msg.MeasureItemStruct^ do
  begin
    Item := FindItem(itemID, fkCommand);
    if Assigned(Item) then
    begin
      DC := GetWindowDC(0);
      try
        FCanvas := TControlCanvas.Create;
        try
          SaveIndex := SaveDC(DC);
          try
            FCanvas.Handle := DC;
            FCanvas.Font := Screen.MenuFont;
            if Item.Default then
              Canvas.Font.Style := Canvas.Font.Style + [fsBold];
            GetActiveItemPainter.Menu := Self;
            GetActiveItemPainter.Measure(Item, Integer(itemWidth), Integer(itemHeight));
            //MeasureItem(Item, Integer(itemWidth), Integer(itemHeight));
          finally
            FCanvas.Handle := 0;
            RestoreDC(DC, SaveIndex);
          end;
        finally
          Canvas.Free;
        end;
      finally
        ReleaseDC(DC, 0);
      end;
    end;
  end;
end;

procedure TJvMainMenu.WMMenuSelect(var Msg: TWMMenuSelect);
var
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  MenuID: Integer;
begin
  if FCursor <> crDefault then
    with Msg do
    begin
      FindKind := fkCommand;
      if MenuFlag and MF_POPUP <> 0 then
      begin
        FindKind := fkHandle;
        MenuID := GetSubMenu(Menu, IDItem);
      end
      else
        MenuID := IDItem;
      MenuItem := TMenuItem(FindItem(MenuID, FindKind));
      if (MenuItem <> nil) and (IsItemPopup(MenuItem) or (MenuItem.Count = 0))
        and (MenuFlag and MF_HILITE <> 0) then
        SetCursor(Screen.Cursors[FCursor])
      else
        SetCursor(Screen.Cursors[crDefault]);
    end;
end;

procedure TJvMainMenu.SetItemPainter(const Value: TJvCustomMenuItemPainter);
begin
  if Value <> FItemPainter then
  begin
    // Remove menu from current item painter
    if FItemPainter <> nil then
      FItemPainter.Menu := nil;

    // set value and if not nil, setup the painter correctly
    FItemPainter := Value;
    if FItemPainter <> nil then
    begin
      Style := msItemPainter;
      FItemPainter.FreeNotification(Self);
      FItemPainter.Menu := Self;
    end
    else
      Style := msStandard;
    Refresh;
  end;
end;

function TJvMainMenu.GetActiveItemPainter: TJvCustomMenuItemPainter;
begin
  if (Style = msItemPainter) and (ItemPainter <> nil) then
    Result := ItemPainter
  else
    Result := FStyleItemPainter;
end;

//=== { TJvPopupList } =======================================================

type
  TJvPopupList = class(TList)
  private
    procedure WndProc(var Message: TMessage);
  public
    Window: HWND;
    procedure Add(Popup: TPopupMenu);
    procedure Remove(Popup: TPopupMenu);
  end;

var
  PopupList: TJvPopupList = nil;

procedure TJvPopupList.WndProc(var Message: TMessage);
var
  I: Integer;
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  ContextID: Integer;
  Handled: Boolean;
begin
  try
    case Message.Msg of
      WM_MEASUREITEM, WM_DRAWITEM:
        for I := 0 to Count - 1 do
        begin
          Handled := False;
          TJvPopupMenu(Items[I]).WndMessage(nil, Message, Handled);
          if Handled then
            Exit;
        end;
      WM_COMMAND:
        for I := 0 to Count - 1 do
          if TJvPopupMenu(Items[I]).DispatchCommand(Message.WParam) then
            Exit;
      WM_INITMENUPOPUP:
        for I := 0 to Count - 1 do
          with TWMInitMenuPopup(Message) do
            if TJvPopupMenu(Items[I]).DispatchPopup(MenuPopup) then
              Exit;
      WM_MENUSELECT:
        with TWMMenuSelect(Message) do
        begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then
          begin
            FindKind := fkHandle;
            ContextID := GetSubMenu(Menu, IDItem);
          end
          else
            ContextID := IDItem;
          for I := 0 to Count - 1 do
          begin
            MenuItem := TJvPopupMenu(Items[I]).FindItem(ContextID, FindKind);
            if MenuItem <> nil then
            begin
              Application.Hint := MenuItem.Hint;
              with TJvPopupMenu(Items[I]) do
                if FCursor <> crDefault then
                  if (MenuFlag and MF_HILITE <> 0) then
                    SetCursor(Screen.Cursors[FCursor])
                  else
                    SetCursor(Screen.Cursors[crDefault]);
              Exit;
            end;
          end;
          Application.Hint := '';
        end;
      WM_MENUCHAR:
        for I := 0 to Count - 1 do
          with TJvPopupMenu(Items[I]) do
            if (Handle = HMenu(Message.LParam)) or
              (FindItem(Message.LParam, fkHandle) <> nil) then
            begin
              ProcessMenuChar(TWMMenuChar(Message));
              Exit;
            end;
      WM_HELP:
        with PHelpInfo(Message.LParam)^ do
        begin
          for I := 0 to Count - 1 do
            if TJvPopupMenu(Items[I]).Handle = hItemHandle then
            begin
              ContextID := TMenu(Items[I]).GetHelpContext(iCtrlID, True);
              if ContextID = 0 then
                ContextID := TMenu(Items[I]).GetHelpContext(hItemHandle, False);
              if Screen.ActiveForm = nil then
                Exit;
              if (biHelp in Screen.ActiveForm.BorderIcons) then
                Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
              else
                Application.HelpContext(ContextID);
              Exit;
            end;
        end;
    end;
    with Message do
      Result := DefWindowProc(Window, Msg, WParam, LParam);
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvPopupList.Add(Popup: TPopupMenu);
begin
  if Count = 0 then
    Window := AllocateHWnd(WndProc);
  inherited Add(Popup);
end;

procedure TJvPopupList.Remove(Popup: TPopupMenu);
begin
  inherited Remove(Popup);
  if Count = 0 then
    DeallocateHWnd(Window);
end;

//=== { TJvPopupMenu } =======================================================

constructor TJvPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if PopupList = nil then
  begin
    PopupList := TJvPopupList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(PopupList));
  end;
  FStyle := msStandard;
  FStyleItemPainter := CreateMenuItemPainterFromStyle(FStyle, Self);
  FCursor := crDefault;
  FImageMargin := TJvImageMargin.Create;
  FImageSize := TJvMenuImageSize.Create;
  PopupList.Add(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;
  FPopupPoint := Point(-1, -1);

  // Set default values that are not 0
  FTextVAlignment := vaMiddle;
end;

destructor TJvPopupMenu.Destroy;
begin
  FImageChangeLink.Free;
  FDisabledImageChangeLink.Free;
  FHotImageChangeLink.Free;
  FImageMargin.Free;
  FImageSize.Free;
  FStyleItemPainter.Free;
  PopupList.Remove(Self);
  inherited Destroy;
end;

procedure TJvPopupMenu.Loaded;
begin
  inherited Loaded;
  if IsOwnerDrawMenu then
    RefreshMenu(True);
end;

function TJvPopupMenu.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvPopupMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      SetImages(nil);
    if AComponent = FDisabledImages then
      SetDisabledImages(nil);
    if AComponent = FHotImages then
      SetHotImages(nil);
    if AComponent = FItemPainter then
      ItemPainter := nil;
  end;
end;

procedure TJvPopupMenu.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FImages <> nil then
    FImages.UnregisterChanges(FImageChangeLink);
  FImages := Value;
  if Value <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
end;

procedure TJvPopupMenu.DisabledImageListChange(Sender: TObject);
begin
  if Sender = FDisabledImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetDisabledImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FDisabledImages <> nil then
    FDisabledImages.UnregisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if Value <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
end;

procedure TJvPopupMenu.HotImageListChange(Sender: TObject);
begin
  if Sender = FHotImages then
    RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetHotImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FHotImages <> nil then
    FImages.UnregisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if Value <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then
    RefreshMenu(not OldOwnerDraw);
end;

function FindPopupControl(const Pos: TPoint): TControl;
var
  Window: TWinControl;
begin
  Result := nil;
  Window := FindVCLWindow(Pos);
  if Window <> nil then
  begin
    Result := Window.ControlAtPos(Pos, False);
    if Result = nil then
      Result := Window;
  end;
end;

procedure TJvPopupMenu.SetBiDiModeFromPopupControl;
var
  AControl: TControl;
begin
  if not SysLocale.MiddleEast then
    Exit;
  if FParentBiDiMode then
  begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      BiDiMode := AControl.BiDiMode
    else
      BiDiMode := Application.BiDiMode;
  end;
end;

function TJvPopupMenu.UseRightToLeftAlignment: Boolean;
var
  AControl: TControl;
begin
  Result := False;
  if not SysLocale.MiddleEast then
    Exit;
  if FParentBiDiMode then
  begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      Result := AControl.UseRightToLeftAlignment
    else
      Result := Application.UseRightToLeftAlignment;
  end
  else
    Result := (BiDiMode = bdRightToLeft);
end;

procedure TJvPopupMenu.Popup(X, Y: Integer);
const
  Flags: array[Boolean, TPopupAlignment] of Word =
  ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
    (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word =
  (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
var
  FOnPopup: TNotifyEvent;
begin
  FPopupPoint := Point(X, Y);
  FParentBiDiMode := ParentBiDiMode;
  try
    SetBiDiModeFromPopupControl;
    FOnPopup := OnPopup;
    if Assigned(FOnPopup) then
      FOnPopup(Self);
    if IsOwnerDrawMenu then
      RefreshMenu(True);
    AdjustBiDiBehavior;
    TrackPopupMenu(Items.Handle,
      Flags[UseRightToLeftAlignment, Alignment] or Buttons[TrackButton], X, Y,
      0 { reserved }, PopupList.Window, nil);
  finally
    ParentBiDiMode := FParentBiDiMode;
  end;
end;

procedure TJvPopupMenu.Refresh;
begin
  RefreshMenu(IsOwnerDrawMenu);
end;

function TJvPopupMenu.IsOwnerDrawMenu: Boolean;
begin
  Result := (FStyle <> msStandard) or (Assigned(FImages) and (FImages.Count > 0));
end;

procedure TJvPopupMenu.RefreshMenu(AOwnerDraw: Boolean);
begin
  Self.OwnerDraw := AOwnerDraw and not (csDesigning in ComponentState);
end;

procedure TJvPopupMenu.SetStyle(Value: TJvMenuStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;

    // delete the old painter and create a new internal painter
    // according to the style, but only if the style is not
    // msItemPainter
    if Style <> msItemPainter then
    begin
      ItemPainter := nil;
      FStyleItemPainter.Free;
      FStyleItemPainter := CreateMenuItemPainterFromStyle(Value, Self);
    end;

    RefreshMenu(IsOwnerDrawMenu);
  end;
end;

procedure TJvPopupMenu.DefaultDrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
begin
  if Canvas.Handle <> 0 then
  begin
    GetActiveItemPainter.Menu := Self;
    GetActiveItemPainter.Paint(Item, Rect, State);
  end;
end;

procedure TJvPopupMenu.DrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
begin
  if Canvas.Handle <> 0 then
  begin
    GetActiveItemPainter.Menu := Self;
    GetActiveItemPainter.Paint(Item, Rect, State);
  end;
end;

procedure TJvPopupMenu.MeasureItem(Item: TMenuItem; var Width, Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Item, Width, Height)
end;

procedure TJvPopupMenu.WndMessage(Sender: TObject; var AMsg: TMessage;
  var Handled: Boolean);
begin
  if IsOwnerDrawMenu then
    MenuWndMessage(Self, AMsg, Handled);
end;

procedure TJvPopupMenu.GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
  AFont: TFont; var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Item, State, AFont, Color, Graphic, NumGlyphs);
  if (Item <> nil) and (Item.Caption = Separator) then
    Graphic := nil;
end;

procedure TJvPopupMenu.GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
  var ImageIndex: Integer);
begin
  if Assigned(FImages) and (Item <> nil) and (Item.Caption <> Separator) and
    Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item, State, ImageIndex);
end;

procedure TJvPopupMenu.WMDrawItem(var Msg: TWMDrawItem);
var
  State: TMenuOwnerDrawState;
  SaveIndex: Integer;
  Item: TMenuItem;
  //  MarginRect: TRect;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TMenuOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and
      (FindItem(Item.Command, fkCommand) = Item) then
    begin
      FCanvas := TControlCanvas.Create;
      try
        SaveIndex := SaveDC(hDC);
        try
          Canvas.Handle := hDC;
          SetDefaultMenuFont(Canvas.Font);
          Canvas.Font.Color := clMenuText;
          Canvas.Brush.Color := clMenu;
          if mdDefault in State then
            Canvas.Font.Style := Canvas.Font.Style + [fsBold];
          if (mdSelected in State) and
            not (Style in [msBtnLowered, msBtnRaised]) then
          begin
            Canvas.Brush.Color := clHighlight;
            Canvas.Font.Color := clHighlightText;
          end;
          with rcItem do
            IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
          DrawItem(Item, rcItem, State);
          Canvas.Handle := 0;
        finally
          RestoreDC(hDC, SaveIndex);
        end;
      finally
        Canvas.Free;
      end;
    end;
  end;
end;

procedure TJvPopupMenu.WMMeasureItem(var Msg: TWMMeasureItem);
var
  Item: TMenuItem;
  SaveIndex: Integer;
  DC: HDC;
begin
  with Msg.MeasureItemStruct^ do
  begin
    Item := FindItem(itemID, fkCommand);
    if Assigned(Item) then
    begin
      DC := GetWindowDC(0);
      try
        FCanvas := TControlCanvas.Create;
        try
          SaveIndex := SaveDC(DC);
          try
            FCanvas.Handle := DC;
            FCanvas.Font := Screen.MenuFont;
            if Item.Default then
              Canvas.Font.Style := Canvas.Font.Style + [fsBold];
            GetActiveItemPainter.Menu := Self;
            GetActiveItemPainter.Measure(Item, Integer(itemWidth), Integer(itemHeight));
            //MeasureItem(Item, Integer(itemWidth), Integer(itemHeight));
          finally
            FCanvas.Handle := 0;
            RestoreDC(DC, SaveIndex);
          end;
        finally
          Canvas.Free;
        end;
      finally
        ReleaseDC(DC, 0);
      end;
    end;
  end;
end;

procedure TJvPopupMenu.Assign(Source: TPersistent);
begin
  if Source is TJvPopupMenu then
  begin
    AutoHotkeys := TJvPopupMenu(Source).AutoHotkeys;
    AutoLineReduction := TJvPopupMenu(Source).AutoLineReduction;
    BiDiMode := TJvPopupMenu(Source).BiDiMode;
    Cursor := TJvPopupMenu(Source).Cursor;
    DisabledImages := TJvPopupMenu(Source).DisabledImages;
    HotImages := TJvPopupMenu(Source).HotImages;
    ImageMargin.Assign(TJvPopupMenu(Source).ImageMargin);
    Images := TJvPopupMenu(Source).Images;
    ImageSize.Assign(TJvPopupMenu(Source).ImageSize);
    ParentBiDiMode := TJvPopupMenu(Source).ParentBiDiMode;
    ShowCheckMarks := TJvPopupMenu(Source).ShowCheckMarks;
    Style := TJvPopupMenu(Source).Style;
    Tag := TJvPopupMenu(Source).Tag;
    TextMargin := TJvPopupMenu(Source).TextMargin;
    TextVAlignment := TJvPopupMenu(Source).TextVAlignment;
  end
  else
  if Source is TJvMainMenu then
  begin
    AutoHotkeys := TJvMainMenu(Source).AutoHotkeys;
    AutoLineReduction := TJvMainMenu(Source).AutoLineReduction;
    BiDiMode := TJvMainMenu(Source).BiDiMode;
    Cursor := TJvMainMenu(Source).Cursor;
    DisabledImages := TJvMainMenu(Source).DisabledImages;
    HotImages := TJvMainMenu(Source).HotImages;
    ImageMargin.Assign(TJvMainMenu(Source).ImageMargin);
    Images := TJvMainMenu(Source).Images;
    ImageSize.Assign(TJvMainMenu(Source).ImageSize);
    ParentBiDiMode := TJvMainMenu(Source).ParentBiDiMode;
    ShowCheckMarks := TJvMainMenu(Source).ShowCheckMarks;
    Style := TJvMainMenu(Source).Style;
    Tag := TJvMainMenu(Source).Tag;
    TextMargin := TJvMainMenu(Source).TextMargin;
    TextVAlignment := TJvMainMenu(Source).TextVAlignment;
  end
  else
    inherited Assign(Source);
end;

procedure TJvPopupMenu.ReadState(Reader: TReader);
begin
  //  Reader.ReadComponent(FJvMenuItemPainter);
  inherited ReadState(Reader);
end;

procedure TJvPopupMenu.WriteState(Writer: TWriter);
begin
  inherited WriteState(Writer);
  //  Writer.WriteComponent(FJvMenuItemPainter);
end;

procedure TJvPopupMenu.SetItemPainter(const Value: TJvCustomMenuItemPainter);
begin
  if Value <> FItemPainter then
  begin
    // Remove menu from current item painter
    if FItemPainter <> nil then
      FItemPainter.Menu := nil;

    // set value and if not nil, setup the painter correctly
    FItemPainter := Value;
    if FItemPainter <> nil then
    begin
      Style := msItemPainter;
      FItemPainter.FreeNotification(Self);
      FItemPainter.Menu := Self;
    end;
    Refresh;
  end;
end;

function TJvPopupMenu.GetActiveItemPainter: TJvCustomMenuItemPainter;
begin
  if Style = msItemPainter then
    Result := ItemPainter
  else
    Result := FStyleItemPainter;
end;

//=== { TJvCustomMenuItemPainter } ===========================================

constructor TJvCustomMenuItemPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // affect default values that are not 0
  FImageBackgroundColor := DefaultImageBackgroundColor;

  FImageMargin := TJvImageMargin.Create;
  FImageSize := TJvMenuImageSize.Create;
end;

destructor TJvCustomMenuItemPainter.Destroy;
begin
  FImageSize.Free;
  FImageMargin.Free;
  inherited Destroy;
end;

procedure TJvCustomMenuItemPainter.DrawDisabledBitmap(X, Y: Integer; Bitmap: TBitmap);
var
  Bmp: TBitmap;
  GrayColor, SaveColor: TColor;
  IsHighlight: Boolean;
begin
  if (mdSelected in FState) then
    GrayColor := clGrayText
  else
    GrayColor := clBtnShadow;
  IsHighlight := NewStyleControls and ((not (mdSelected in FState)) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) =
    GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight))));
  if Bitmap.Monochrome then
  begin
    SaveColor := Canvas.Brush.Color;
    try
      if IsHighlight then
      begin
        Canvas.Brush.Color := clBtnHighlight;
        SetTextColor(Canvas.Handle, clWhite);
        SetBkColor(Canvas.Handle, clBlack);
        BitBlt(Canvas.Handle, X + 1, Y + 1, Bitmap.Width, Bitmap.Height,
          Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
      Canvas.Brush.Color := GrayColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
    finally
      Canvas.Brush.Color := SaveColor;
    end;
  end
  else
  begin
    Bmp := CreateDisabledBitmapEx(Bitmap, clBlack, clMenu,
      clBtnHighlight, GrayColor, IsHighlight);
    try
      DrawBitmapTransparent(Canvas, X, Y, Bmp, clMenu);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TJvCustomMenuItemPainter.DrawMenuBitmap(X, Y: Integer; Bitmap: TBitmap);
begin
  if mdDisabled in FState then
    DrawDisabledBitmap(X, Y, Bitmap)
  else
  begin
    if Bitmap.Monochrome and (not FItem.Checked or ShowCheckMarks) then
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else
      DrawBitmapTransparent(Canvas, X, Y, Bitmap, Bitmap.TransparentColor and not PaletteMask);
  end;
end;

procedure TJvCustomMenuItemPainter.DrawCheckImage(ARect: TRect);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    with Bmp do
    begin
      Width := GetSystemMetrics(SM_CXMENUCHECK);
      Height := GetSystemMetrics(SM_CYMENUCHECK);
    end;
    if FItem.RadioItem then
      with Bmp do
      begin
        DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
          DFC_MENU, DFCS_MENUBULLET);
        Monochrome := True;
        Inc(ARect.Top); // the bullet must be shifted one pixel towards the bottom
      end
    else
      with Bmp do
      begin
        DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
          DFC_MENU, DFCS_MENUCHECK);
        Monochrome := True;
      end;
    case TextVAlignment of
      vaMiddle:
        Inc(ARect.Top, ((ARect.Bottom - ARect.Top + 1) - Bmp.Height) div 2);
      vaBottom:
        ARect.Top := ARect.Bottom - Bmp.Height;
    end;
    // draw the check mark bitmap, always centered horizontally
    DrawMenuBitmap(ARect.Left + (ARect.Right - ARect.Left + 1 - Bmp.Width) div 2,
      ARect.Top, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvCustomMenuItemPainter.DrawGlyphCheck(ARect: TRect);
var
  SaveColor: TColor;
  Bmp: TBitmap;
begin
  InflateRect(ARect, -1, -1);
  SaveColor := Canvas.Brush.Color;
  try
    if not (mdSelected in FState) then
      Bmp := AllocPatternBitmap(clMenu, clBtnHighlight)
    else
      Bmp := nil;
    try
      if Bmp <> nil then
        Canvas.Brush.Bitmap := Bmp
      else
        Canvas.Brush.Color := clMenu;
      Canvas.FillRect(ARect);
    finally
      Canvas.Brush.Bitmap := nil;
    end;
  finally
    Canvas.Brush.Color := SaveColor;
  end;
  Frame3D(Canvas, ARect, GrayColor, clBtnHighlight, 1);
end;

function TJvCustomMenuItemPainter.GetDisabledImages: TImageList;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.DisabledImages
  else
    Result := FPopupMenu.DisabledImages;
end;

function TJvCustomMenuItemPainter.GetHotImages: TImageList;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.HotImages
  else
    Result := FPopupMenu.HotImages;
end;

function TJvCustomMenuItemPainter.GetImages: TImageList;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.Images
  else
    Result := FPopupMenu.Images;
end;

function TJvCustomMenuItemPainter.GetShowCheckMarks: Boolean;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.ShowCheckMarks
  else
    Result := FPopupMenu.ShowCheckMarks;
end;

function TJvCustomMenuItemPainter.UseImages: Boolean;
begin
  Result := Assigned(Images) and (FImageIndex >= 0) and
    (FImageIndex < Images.Count) and Images.HandleAllocated;
end;

function TJvCustomMenuItemPainter.UseHotImages: Boolean;
begin
  Result := Assigned(HotImages) and (FImageIndex >= 0) and
    (FImageIndex < HotImages.Count) and HotImages.HandleAllocated;
end;

function TJvCustomMenuItemPainter.UseDisabledImages: Boolean;
begin
  Result := Assigned(DisabledImages) and (FImageIndex >= 0) and
    (FImageIndex < DisabledImages.Count) and DisabledImages.HandleAllocated;
end;

procedure TJvCustomMenuItemPainter.DrawText(ARect: TRect; const Text: string; Flags: Longint);
begin
  if Length(Text) = 0 then
    Exit;
  if (FParentMenu <> nil) and (FParentMenu.IsRightToLeft) then
  begin
    if Flags and DT_LEFT = DT_LEFT then
      Flags := Flags and (not DT_LEFT) or DT_RIGHT
    else
    if Flags and DT_RIGHT = DT_RIGHT then
      Flags := Flags and (not DT_RIGHT) or DT_LEFT;
    Flags := Flags or DT_RTLREADING;
  end;

  case TextVAlignment of
    vaMiddle:
      Inc(ARect.Top, ((ARect.Bottom - ARect.Top + 1) - Canvas.TextHeight(StripHotkey(Text))) div 2);
    vaBottom:
      ARect.Top := ARect.Bottom - Canvas.TextHeight(StripHotkey(Text));
  end;

  // if a top level menu item then draw text centered horizontally
  if not IsPopup then
    ARect.Left := ARect.Left + ((ARect.Right - ARect.Left) - Canvas.TextWidth(StripHotkey(Text))) div 2;

  if mdDisabled in FState then
  begin
    if DrawHighlight then
    begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(ARect, 1, 1);
      Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
      OffsetRect(ARect, -1, -1);
    end;
    Canvas.Font.Color := GrayColor;
  end;
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags)
end;

procedure TJvCustomMenuItemPainter.PreparePaint(Item: TMenuItem;
  ItemRect: TRect; State: TMenuOwnerDrawState; Measure: Boolean);
var
  BackColor: TColor;
begin
  UpdateFieldsFromMenu;
  FItem := Item;
  FState := State;

  FGlyph := nil;
  BackColor := Canvas.Brush.Color;
  FNumGlyphs := 1;
  if Assigned(FMainMenu) then
    FMainMenu.GetItemParams(FItem, FState, Canvas.Font, BackColor, FGlyph, FNumGlyphs)
  else
    FPopupMenu.GetItemParams(FItem, FState, Canvas.Font, BackColor, FGlyph, FNumGlyphs);

  if not Measure and (BackColor <> clNone) then
  begin
    Canvas.Brush.Color := BackColor;
    Canvas.FillRect(ItemRect);
  end;
  FImageIndex := FItem.ImageIndex;

  if Assigned(FMainMenu) then
    FMainMenu.GetImageIndex(FItem, FState, FImageIndex)
  else
    FPopupMenu.GetImageIndex(FItem, FState, FImageIndex);
end;

procedure TJvCustomMenuItemPainter.Paint(Item: TMenuItem; ItemRect: TRect;
  State: TMenuOwnerDrawState);
var
  MaxWidth, I: Integer;
  Bmp: TBitmap;

  // the rect that will contain the size of the menu item caption
  CaptionRect: TRect;

  // The rect in which to draw the check mark for the item
  CheckMarkRect: TRect;

  // The rect in which to draw the image, with or without the image margins
  ImageRect: TRect;
  ImageAndMarginRect: TRect;

  // The rect in which to draw the text, with or without the margins
  TextRect: TRect;
  TextAndMarginRect: TRect;

  // The rect where the Left margin has to be drawn (its height is the height of the entire menu, not just the item)
  LeftMarginRect: TRect;

  // The item rect, whithout the left margin
  ItemRectNoLeftMargin: TRect;

  CanvasWindow: HWND;
  CanvasRect: TRect;
begin
  // We must do this to prevent the code in Menus.pas from drawing
  // the item before us, thus trigerring rendering glitches, especially
  // when a top menuitem that has an image index not equal to -1
  Item.OnDrawItem := EmptyDrawItem;

  // prepare the painting
  PreparePaint(Item, ItemRect, State, False);

  // calculate areas for the different parts of the item to be drawn
  if IsPopup then
  begin
    // as the margin is to be drawn for the entire height of the menu,
    // we need to retrieve this height. The only way to do that is to
    // get the rectangle for the canvas in which we are to draw.
    CanvasWindow := WindowFromDC(Canvas.Handle);
    GetWindowRect(CanvasWindow, CanvasRect);

    // different values depending on the reading convention
    if IsRightToLeft then
    begin
      CheckMarkRect := Rect(ItemRect.Right - CheckMarkWidth + 1, ItemRect.Top, ItemRect.Right, ItemRect.Bottom);
      ImageAndMarginRect := Rect(CheckMarkRect.Left - 1 - ImageMargin.Left - ImageWidth - ImageMargin.Right, ItemRect.Top, CheckMarkRect.Left - 1, ItemRect.Bottom);
      TextAndMarginRect := Rect(ItemRect.Left, ItemRect.Top, ImageAndMarginRect.Left - 1, ItemRect.Bottom);
      ItemRectNoLeftMargin := Rect(ItemRect.Left, ItemRect.Top, Cardinal(ItemRect.Right)-LeftMargin, ItemRect.Bottom);
      OffsetRect(CheckMarkRect, -LeftMargin, 0);
      OffsetRect(ImageAndMarginRect, -LeftMargin, 0);
      OffsetRect(TextAndMarginRect, -LeftMargin, 0);

      LeftMarginRect := Rect(ItemRect.Right, 0, Cardinal(ItemRect.Right) - LeftMargin, CanvasRect.Bottom - CanvasRect.Top);
    end
    else
    begin
      CheckMarkRect := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + CheckMarkWidth - 1, ItemRect.Bottom);
      ImageAndMarginRect := Rect(CheckMarkRect.Right + 1, ItemRect.Top, CheckMarkRect.Right + 1 + ImageMargin.Left + ImageWidth + ImageMargin.Right - 1, ItemRect.Bottom);
      TextAndMarginRect := Rect(ImageAndMarginRect.Right + 1, ItemRect.Top, ItemRect.Right, ItemRect.Bottom);
      ItemRectNoLeftMargin := Rect(Cardinal(ItemRect.Left)+LeftMargin, ItemRect.Top, ItemRect.Right, ItemRect.Bottom);
      OffsetRect(CheckMarkRect, LeftMargin, 0);
      OffsetRect(ImageAndMarginRect, LeftMargin, 0);
      OffsetRect(TextAndMarginRect, LeftMargin, 0);

      LeftMarginRect := Rect(ItemRect.Left, 0, Cardinal(ItemRect.Left) + LeftMargin, CanvasRect.Bottom - CanvasRect.Top);
    end;
    ImageRect := Rect(ImageAndMarginRect.Left + ImageMargin.Left, ImageAndMarginRect.Top + ImageMargin.Top, ImageAndMarginRect.Right - ImageMargin.Right, ImageAndMarginRect.Bottom - ImageMargin.Bottom);
    TextRect := Rect(TextAndMarginRect.Left + TextMargin, TextAndMarginRect.Top, TextAndMarginRect.Right, TextAndMarginRect.Bottom);
  end
  else
  begin
    ItemRectNoLeftMargin := ItemRect;
    TextAndMarginRect := ItemRect;
    TextRect := ItemRect;
  end;

  // first, draw the background of the entire item
  DrawItemBackground(ItemRect);

  // draw the margin, if any
  if LeftMargin > 0 then
    DrawLeftMargin(LeftMarginRect);

  // draw the background of each separate part
  if IsPopup then
  begin
    if ShowCheckMarks then
      DrawCheckMarkBackground(CheckMarkRect);
    DrawImageBackground(ImageAndMarginRect);
  end;
  DrawTextBackground(TextAndMarginRect);

  // if the item is selected, then draw the frame to represent that
  if mdSelected in State then
    DrawSelectedFrame(ItemRectNoLeftMargin);

  if Assigned(Item) then
  begin
    FParentMenu := Item.GetParentMenu;

    // if item is checked and if we show check marks and if
    // the item is a popup (ie, not a top item), then we draw
    // the check image
    if Item.Checked and ShowCheckMarks and IsPopup then
      DrawCheckImage(CheckMarkRect);

    // It is now time to draw the image. The image will not be
    // drawn for root menu items (non popup).
    if IsPopup then
    begin
      // if we have a valid image from the list to use for this item
      if UseImages then
      begin
        // Draw the corresponding back of an item
        // if the item is to be drawn checked or not
        if Item.Checked and not ShowCheckMarks then
          DrawCheckedImageBack(ImageAndMarginRect)
        else
          DrawNotCheckedImageBack(ImageAndMarginRect);

        // then, draw the correct image, according to the state
        // of the item
        if (mdDisabled in State) then
          DrawDisabledImage(ImageRect.Left, ImageRect.Top)
        else
          DrawEnabledImage(ImageRect.Left, ImageRect.Top)
      end
        // else, we may have a valid glyph, but we won't use it if
        // the item is a separator
      else
      if Assigned(FGlyph) and not FGlyph.Empty and
        (Item.Caption <> Separator) then
      begin
        // Draw the corresponding back of an item
        // if the item is to be drawn checked or not
        if Item.Checked and not ShowCheckMarks then
          DrawCheckedImageBack(ImageAndMarginRect)
        else
          DrawNotCheckedImageBack(ImageAndMarginRect);

        if FGlyph is TBitmap then
        begin
          // in the case of a bitmap, we may have more than one glyph
          // in the graphic. If so, we draw only the one that corresponds
          // to the current state of the item
          // if not, we simply draw the bitmap
          if FNumGlyphs in [2..5] then
          begin
            I := 0;
            if mdDisabled in State then
              I := 1
            else
            if mdChecked in State then
              I := 3
            else
            if mdSelected in State then
              I := 2;
            if I > FNumGlyphs - 1 then
              I := 0;
            Bmp := TBitmap.Create;
            try
              AssignBitmapCell(FGlyph, Bmp, FNumGlyphs, 1, I);
              DrawMenuBitmap(ImageRect.Left, ImageRect.Top, Bmp);
            finally
              Bmp.Free;
            end;
          end
          else
            DrawMenuBitmap(ImageRect.Left, ImageRect.Top, TBitmap(FGlyph));
        end
        else
        begin
          Canvas.Draw(ImageRect.Left, ImageRect.Top, FGlyph);
        end;
      end
      // at last, if there is no image given by the user, there may
      // be a check mark to draw instead
      else
      if Item.Checked and not ShowCheckMarks then
      begin
        DrawCheckedImageBack(ImageAndMarginRect);
        DrawCheckImage(ImageRect);
      end;
    end;

    // now that the image and check mark are drawn, we can
    // draw the text of the item (or a separator)

    if Item.Caption = Separator then
      DrawSeparator(ItemRectNoLeftMargin)
    else
    begin
      // find the largest text element
      Windows.DrawText(Canvas.Handle,
                       PChar(Item.Caption),
                       Length(Item.Caption),
                       CaptionRect,
                       DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
      MaxWidth := CaptionRect.Right - CaptionRect.Left;
      if (Item.Parent <> nil) and (Item.ShortCut <> scNone) then
      begin
        for I := 0 to Item.Parent.Count - 1 do
        begin
          Windows.DrawText(Canvas.Handle,
                           PChar(Item.Parent.Items[I].Caption+ShortcutSpacing),
                           Length(Item.Parent.Items[I].Caption+ShortcutSpacing),
                           CaptionRect,
                           DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
          MaxWidth := Max(CaptionRect.Right - CaptionRect.Left, MaxWidth);
        end;
      end;

      // draw the text
      Canvas.Brush.Style := bsClear;
      DrawText(TextRect, Item.Caption, DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
      if (Item.ShortCut <> scNone) and (Item.Count = 0) and IsPopup then
      begin
        // draw the shortcut
        DrawText(Rect(TextRect.Left + MaxWidth, TextRect.Top, TextRect.Right, TextRect.Bottom),
          ShortCutToText(Item.ShortCut), DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
      end;
    end;
  end
  else
  begin
    MessageBox('!!! asked to draw nil item !!!'#13#10 +
      'Please report this to the JVCL team, ' +
      'detailing the precise conditions in ' +
      'which this error occured.'#13#10 +
      'Thank you for your cooperation.',
      'error in menu painter',
      MB_ICONERROR);
  end;
end;

procedure TJvCustomMenuItemPainter.DrawSelectedFrame;
begin
  // Do nothing by default
end;

procedure TJvCustomMenuItemPainter.DrawCheckedImageBack(ARect: TRect);
begin
  // do nothing by default
end;

procedure TJvCustomMenuItemPainter.DrawNotCheckedImageBack(ARect: TRect);
begin
  // do nothing by default
end;

function TJvCustomMenuItemPainter.GetDrawHighlight: Boolean;
begin
  Result := NewStyleControls and
    (not (mdSelected in FState) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) = GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight)))
    );
end;

function TJvCustomMenuItemPainter.GetGrayColor: TColor;
begin
  if mdSelected in FState then
    Result := clGrayText
  else
    Result := clBtnShadow;
end;

function TJvCustomMenuItemPainter.GetIsPopup: Boolean;
begin
  Result := (FItem.Parent = nil) or (FItem.Parent.Parent <> nil) or
    not (FItem.Parent.Owner is TMainMenu);
end;

function TJvCustomMenuItemPainter.GetTextWidth(Item: TMenuItem): Integer;
var
  I: Integer;
  MaxWidth: Integer;
  tmpWidth: Integer;
  ShortcutWidth: Integer;
  OneItemHasChildren: Boolean;
  CaptionRect: TRect;
begin
  if IsPopup then
  begin
    // The width of the text is splitted in three parts:
    // Text Shortcut SubMenuArrow.
    // with the two last ones being not compulsory

    Windows.DrawText(Canvas.Handle,
      PChar(Item.Caption),
      Length(Item.Caption),
      CaptionRect,
      DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
    MaxWidth := CaptionRect.Right - CaptionRect.Left;

    ShortcutWidth := 0;
    OneItemHasChildren := False;
    // Find the widest item in the menu being displayed
    if Item.Parent <> nil then

      // If the current item is the first one and it's not
      // alone, then discard its width because for some reason
      // the canvas is never correct.
      if Item = Item.Parent.Items[0] then
      begin
        if Item.Parent.Count > 1 then
          Result := 0
        else
          Result := MaxWidth;
        Exit;
      end;

      for I := 0 to Item.Parent.Count - 1 do
      begin
        Windows.DrawText(Canvas.Handle,
          PChar(Item.Parent.Items[I].Caption),
          Length(Item.Parent.Items[I].Caption),
          CaptionRect,
          DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
        tmpWidth := CaptionRect.Right - CaptionRect.Left;
        if tmpWidth > MaxWidth then
          MaxWidth := tmpWidth;

        // if the item has childs, then add the required
        // width for an arrow. It is considered to be the width of
        // two spaces.
        if Item.Parent.Items[I].Count > 0 then
          OneItemHasChildren := True;

        if Item.Parent.Items[I].ShortCut <> scNone then
        begin
          Windows.DrawText(Canvas.Handle,
            PChar(ShortCutToText(Item.Parent.Items[I].ShortCut)),
            Length(ShortCutToText(Item.Parent.Items[I].ShortCut)),
            CaptionRect,
            DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
          tmpWidth := CaptionRect.Right - CaptionRect.Left;
          if tmpWidth > ShortcutWidth then
            ShortcutWidth := tmpWidth;
        end;
      end;
    Result := MaxWidth;

    // If there was a shortcut in any of the items,
    if ShortcutWidth <> 0 then
    begin
      // add its width to the current width, plus the spacing
      Inc(Result, ShortcutWidth);
      Inc(Result, Canvas.TextWidth(ShortcutSpacing));
    end
    else
    if OneItemHasChildren then
      Inc(Result, Canvas.TextWidth('  '));
  end
  else
    Result := Canvas.TextWidth(StripHotkey(Item.Caption));
end;

procedure TJvCustomMenuItemPainter.Measure(Item: TMenuItem;
  var Width, Height: Integer);
begin
  PreparePaint(Item, Rect(0, 0, 0, 0), [], True);

  if IsPopup then
  begin
    Width := LeftMargin + Cardinal(CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right + TextMargin + GetTextWidth(Item));

    if Item.Caption = Separator then
      Height := Max(Canvas.TextHeight(Separator) div 2, 9)
    else
    begin
      Height := Max(GetSystemMetrics(SM_CYMENU), Canvas.TextHeight(Item.Caption));
      Height := Max(Height, CheckMarkHeight);
      Height := Max(Height, ImageMargin.Top + ImageHeight + ImageMargin.Bottom);
    end;
  end
  else
  begin
    Width := TextMargin + GetTextWidth(Item);
    Height := Max(GetSystemMetrics(SM_CYMENU), Canvas.TextHeight(Item.Caption));
  end;
end;

procedure TJvCustomMenuItemPainter.DrawItemBackground(ARect: TRect);
begin
  // do nothing
end;

procedure TJvCustomMenuItemPainter.DrawDisabledImage(X, Y: Integer);
begin
  if UseDisabledImages then
    ImageList_Draw(DisabledImages.Handle, FImageIndex, Canvas.Handle,
      X, Y, ILD_NORMAL)
  else
    ImageListDrawDisabled(Images, Canvas, X, Y, FImageIndex, clBtnHighlight,
      GrayColor, DrawHighlight)
end;

procedure TJvCustomMenuItemPainter.DrawEnabledImage(X, Y: Integer);
begin
  if UseHotImages and (mdSelected in FState) then
    ImageList_Draw(HotImages.Handle, FImageIndex, Canvas.Handle,
      X, Y, ILD_NORMAL)
  else
    ImageList_Draw(Images.Handle, FImageIndex, Canvas.Handle,
      X, Y, ILD_NORMAL);
end;

{function TJvCustomMenuItemPainter.GetShadowColor: TColor;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.ShadowColor
  else
    Result := FPopupMenu.ShadowColor;
end;}

procedure TJvCustomMenuItemPainter.DrawSeparator(ARect: TRect);
var
  LineTop: Integer;
begin
  LineTop := (ARect.Top + ARect.Bottom) div 2 - 1;
  if NewStyleControls then
  begin
    Canvas.Pen.Width := 1;
    MenuLine(Canvas, clBtnShadow, ARect.Left - 1, LineTop, ARect.Right, LineTop);
    MenuLine(Canvas, clBtnHighlight, ARect.Left, LineTop + 1, ARect.Right, LineTop + 1);
  end
  else
  begin
    Canvas.Pen.Width := 2;
    MenuLine(Canvas, clMenuText, ARect.Left, LineTop + 1, ARect.Right, LineTop + 1);
  end;
end;

procedure TJvCustomMenuItemPainter.DrawImageBackground(ARect: TRect);
begin
  // do nothing by default
end;

function TJvCustomMenuItemPainter.GetIsRightToLeft: Boolean;
begin
  Result := (FItem.GetParentMenu <> nil) and
    (FItem.GetParentMenu.BiDiMode = bdRightToLeft);
end;

function TJvCustomMenuItemPainter.GetCheckMarkHeight: Integer;
begin
  if ShowCheckMarks then
    Result := GetSystemMetrics(SM_CYMENUCHECK)
  else
    Result := 0;
end;

function TJvCustomMenuItemPainter.GetCheckMarkWidth: Integer;
begin
  if ShowCheckMarks then
    Result := GetSystemMetrics(SM_CXMENUCHECK)
  else
    Result := 0;
end;

function TJvCustomMenuItemPainter.GetImageHeight: Integer;
begin
  if Assigned(Images) then
    Result := Images.Height
  else
  if Assigned(FGlyph) then
    Result := FGlyph.Height
  else
    Result := ImageSize.Height;
end;

function TJvCustomMenuItemPainter.GetImageWidth: Integer;
begin
  if Assigned(Images) then
    Result := Images.Width
  else
  if Assigned(FGlyph) then
    Result := FGlyph.Width
  else
    Result := ImageSize.Width;
end;

function TJvCustomMenuItemPainter.GetTextMargin: Integer;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.TextMargin
  else
    Result := FPopupMenu.TextMargin;
end;

procedure TJvCustomMenuItemPainter.DrawCheckMarkBackground(ARect: TRect);
begin
  // do nothing by default
end;

procedure TJvCustomMenuItemPainter.DrawTextBackground(ARect: TRect);
begin
  // do nothing by default
end;

function TJvCustomMenuItemPainter.GetTextVAlignment: TJvVerticalAlignment;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.TextVAlignment
  else
    Result := FPopupMenu.TextVAlignment;
end;

procedure TJvCustomMenuItemPainter.UpdateFieldsFromMenu;
begin
  if Assigned(FMainMenu) then
  begin
    FOnDrawItem := FMainMenu.OnDrawItem;
    FImageMargin.Assign(FMainMenu.ImageMargin);
    FImageSize.Assign(FMainMenu.ImageSize);
  end
  else
  begin
    FOnDrawItem := FPopupMenu.OnDrawItem;
    FImageMargin.Assign(FPopupMenu.ImageMargin);
    FImageSize.Assign(FPopupMenu.ImageSize);
  end;
end;

function TJvCustomMenuItemPainter.GetComponentState: TComponentState;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.ComponentState
  else
    Result := FPopupMenu.ComponentState;
end;

procedure TJvCustomMenuItemPainter.DefaultDrawLeftMargin(ARect: TRect;
  StartColor, EndColor: TColor);
var
  R: Integer;
begin
  with ARect do
  begin
    R := Right - 3;

    // Draw the gradient
    GradientFillRect(Canvas, Rect(Left, Top, R, Bottom), StartColor,
      EndColor, fdTopToBottom, 32);

    // Draw the separating line
    MenuLine(Canvas, clBtnFace, Right - 3, Top, Right - 3, Bottom);
    MenuLine(Canvas, clBtnShadow, Right - 2, Top, Right - 2, Bottom);
    MenuLine(Canvas, clBtnHighlight, Right - 1, Top, Right - 1, Bottom);
  end;
end;

procedure TJvCustomMenuItemPainter.DrawLeftMargin(ARect: TRect);
begin
  if Assigned(FOnDrawLeftMargin) then
    FOnDrawLeftMargin(Self.FParentMenu, ARect)
  else
  begin
    DefaultDrawLeftMargin(ARect, DefaultMarginColor, RGB(
      GetRValue(DefaultMarginColor) div 4,
      GetGValue(DefaultMarginColor) div 4,
      GetBValue(DefaultMarginColor) div 4));
  end;
end;

procedure TJvCustomMenuItemPainter.SetLeftMargin(const Value: Cardinal);
begin
  FLeftMargin := Value;
end;

procedure TJvCustomMenuItemPainter.SetImageBackgroundColor(const Value: TColor);
begin
  FImageBackgroundColor := Value;
end;

function TJvCustomMenuItemPainter.GetMenu: TMenu;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu
  else
    Result := FPopupMenu;
end;

procedure TJvCustomMenuItemPainter.SetMenu(const Value: TMenu);
begin
  // Note: One may be tempted to store the value of the Canvas
  // property. This is not a good idea as the Canvas may only be
  // created when the menu is about to be displayed, thus being
  // nil right now.

  if Value is TJvMainMenu then
  begin
    FMainMenu := TJvMainMenu(Value);
    FPopupMenu := nil;
  end
  else
  if Value is TJvPopupMenu then
  begin
    FMainMenu := nil;
    FPopupMenu := TJvPopupMenu(Value);
  end
  else
  begin
    FMainMenu := nil;
    FPopupMenu := nil;
  end;
end;

function TJvCustomMenuItemPainter.GetCanvas: TCanvas;
begin
  if Assigned(FMainMenu) then
    Result := FMainMenu.Canvas
  else
  if Assigned(FPopupMenu) then
    Result := FPopupMenu.Canvas
  else
    Result := nil;
end;

procedure TJvCustomMenuItemPainter.EmptyDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
begin
// Do nothing, on purpose
end;

//=== { TJvBtnMenuItemPainter } ==============================================

constructor TJvBtnMenuItemPainter.Create(AOwner: TComponent; Lowered: Boolean);
begin
  inherited Create(AOwner);
  FLowered := Lowered;
end;

constructor TJvBtnMenuItemPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLowered := True;
end;

procedure TJvBtnMenuItemPainter.DrawSelectedFrame(ARect: TRect);
begin
  if FLowered then
    Frame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1)
  else
    Frame3D(Canvas, ARect, clBtnHighlight, clBtnShadow, 1);
end;

function TJvBtnMenuItemPainter.GetDrawHighlight: Boolean;
begin
  Result := NewStyleControls;
end;

function TJvBtnMenuItemPainter.GetGrayColor: TColor;
begin
  Result := clBtnShadow;
end;

procedure TJvBtnMenuItemPainter.UpdateFieldsFromMenu;
begin
  inherited UpdateFieldsFromMenu;
  FImageMargin.Top := FImageMargin.Top + 1;
  FImageMargin.Bottom := FImageMargin.Bottom + 1;
end;

//=== { TJvOfficeMenuItemPainter } ===========================================

procedure TJvOfficeMenuItemPainter.Paint(Item: TMenuItem; ItemRect: TRect; State: TMenuOwnerDrawState);
begin
  inherited Paint(Item, ItemRect, State);
end;

procedure TJvOfficeMenuItemPainter.CleanupGlyph(BtnRect: TRect);
var
  SaveBrush: TBrush; // to save brush
begin
  SaveBrush := Canvas.Brush;
  Canvas.Brush.Color := ImageBackgroundColor;
  Inc(BtnRect.Right);
  Dec(BtnRect.Left);
  Canvas.FillRect(BtnRect);
  Canvas.Brush := SaveBrush;
end;

procedure TJvOfficeMenuItemPainter.DrawFrame(BtnRect: TRect);
begin
  CleanupGlyph(BtnRect);
  Frame3D(Canvas, BtnRect, clBtnHighlight, clBtnShadow, 1);
end;

procedure TJvOfficeMenuItemPainter.DrawSelectedFrame(ARect: TRect);
begin
  if not IsPopup then
  begin
    CleanupGlyph(ARect);
    Frame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1);
  end;
end;

procedure TJvOfficeMenuItemPainter.DrawCheckedImageBack(ARect: TRect);
begin
  CleanupGlyph(ARect);
  DrawGlyphCheck(ARect);
end;

procedure TJvOfficeMenuItemPainter.DrawNotCheckedImageBack(ARect: TRect);
begin
  if (mdSelected in FState) and IsPopup then
    DrawFrame(ARect);
end;

function TJvOfficeMenuItemPainter.GetDrawHighlight: Boolean;
begin
  Result := NewStyleControls and
    (not (mdSelected in FState) or (not IsPopup) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) = GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight)))
    );
end;

procedure TJvOfficeMenuItemPainter.UpdateFieldsFromMenu;
begin
  inherited UpdateFieldsFromMenu;
  FImageMargin.Left := FImageMargin.Left + 2;
  FImageMargin.Top := FImageMargin.Top + 2;
  FImageMargin.Right := FImageMargin.Right + 3;
  FImageMargin.Bottom := FImageMargin.Bottom + 2;
end;

function TJvOfficeMenuItemPainter.GetTextMargin: Integer;
begin
  Result := inherited GetTextMargin + 3;
end;

procedure TJvOfficeMenuItemPainter.DrawCheckImage(ARect: TRect);
begin
  inherited DrawCheckImage(Rect(ARect.Left + 2, ARect.Top, ARect.Right, ARect.Bottom - 1));
end;

procedure TJvOfficeMenuItemPainter.DrawText(ARect: TRect;
  const Text: string; Flags: Integer);
begin
  if not IsPopup then
    Canvas.Font.Color := clMenuText;
  inherited DrawText(Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom - 1), Text, Flags);
end;

procedure TJvOfficeMenuItemPainter.DrawItemBackground(ARect: TRect);
begin
  inherited DrawItemBackground(ARect);
  if not IsPopup and (mdHotlight in FState) then
    DrawFrame(ARect);
end;

//=== { TJvXPMenuItemPainter } ===============================================

constructor TJvXPMenuItemPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectionFrameBrush := TBrush.Create;
  FSelectionFramePen := TPen.Create;

  FSelRect := Rect(0, 0, 0, 0);
  FCheckedPoint := Point(0, 0);

  // affect default values that are not 0
  FShadowColor := DefaultXPShadowColor;
  FImageBackgroundColor := DefaultXPImageBackgroundColor;
  FSelectionFrameBrush.Color := DefaultXPSFBrushColor;
  FSelectionFrameBrush.Style := bsSolid;
  FSelectionFramePen.Color := DefaultXPSFPenColor;
  FSelectionFramePen.Style := psSolid;
  FSeparatorColor := DefaultXPSeparatorColor;
  FCheckedImageBackColor := DefaultXPCheckedImageBackColor;
  FCheckedImageBackColorSelected := DefaultXPCheckedImageBackColorSelected;
end;

destructor TJvXPMenuItemPainter.Destroy;
begin
  FSelectionFrameBrush.Free;
  FSelectionFramePen.Free;
  inherited Destroy;
end;

procedure TJvXPMenuItemPainter.DrawCheckedImageBack(ARect: TRect);
begin
  with Canvas do
  begin
    Pen.Assign(SelectionFramePen);
    Brush.Style := bsSolid;
    if mdSelected in FState then
      Brush.Color := CheckedImageBackColorSelected //SRGB(133,146,181)
    else
      Brush.Color := CheckedImageBackColor; //RGB(212,213,216);
    Rectangle(ARect.Left, ARect.Top + 1, ARect.Right - 3, ARect.Bottom - 2);
  end;
end;

procedure TJvXPMenuItemPainter.DrawBitmapShadow(X, Y: Integer; B: TBitmap);
var
  BX, BY: Integer;
  TransparentColor: TColor;
begin
  TransparentColor := B.Canvas.Pixels[0, B.Height - 1];
  for BY := 0 to B.Height - 1 do
    for BX := 0 to B.Width - 1 do
      if B.Canvas.Pixels[BX, BY] <> TransparentColor then
        Canvas.Pixels[X + BX, Y + BY] := ShadowColor;
end;

procedure TJvXPMenuItemPainter.DrawDisabledImage(X, Y: Integer);
begin
  // to take the margin into account
  if IsRightToLeft then
    Inc(X, 3)
  else
    Dec(X, 3);

  if UseDisabledImages then
    ImageList_Draw(DisabledImages.Handle, FImageIndex, Canvas.Handle,
      X, Y, ILD_NORMAL)
  else
    //TODO: Change to draw greyscale image
    ImageListDrawDisabled(Images, Canvas, X, Y, FImageIndex, clBtnHighlight, GrayColor,
      DrawHighlight);
end;

procedure TJvXPMenuItemPainter.DrawEnabledImage(X, Y: Integer);
var
  TmpBitmap: TBitmap;
begin
  // to take the margin into account
  if IsRightToLeft then
    Inc(X, 3)
  else
    Dec(X, 3);

  if (mdSelected in FState) then
  begin
    // draw shadow for selected and enbled item
    // first, create a bitmap from the correct image
    TmpBitmap := TBitmap.Create;
    if UseHotImages then
    begin
      TmpBitmap.Width := HotImages.Width;
      TmpBitmap.Height := HotImages.Height;
      TmpBitmap.Canvas.Brush.Color := Canvas.Brush.Color;
      TmpBitmap.Canvas.FillRect(Rect(0, 0, TmpBitmap.Width, TmpBitmap.Height));
      ImageList_DrawEx(HotImages.Handle, FImageIndex, TmpBitmap.Canvas.Handle,
        0, 0, 0, 0, clNone, clNone, ILD_TRANSPARENT);
    end
    else
    begin
      TmpBitmap.Width := Images.Width;
      TmpBitmap.Height := Images.Height;
      TmpBitmap.Canvas.Brush.Color := Canvas.Brush.Color;
      TmpBitmap.Canvas.FillRect(Rect(0, 0, TmpBitmap.Width, TmpBitmap.Height));
      ImageList_DrawEx(Images.Handle, FImageIndex, TmpBitmap.Canvas.Handle,
        0, 0, 0, 0, clNone, clNone, ILD_TRANSPARENT);
    end;

    // then effectively draw the shadow
    DrawBitmapShadow(X + 1, Y + 1, TmpBitmap);

    TmpBitmap.Free;

    // shift the image to the top and left
    Dec(X);
    Dec(Y);
  end;

  // and call inherited to draw the image
  inherited DrawEnabledImage(X, Y);
end;

procedure TJvXPMenuItemPainter.DrawItemBackground(ARect: TRect);
const
  COLOR_MENUBAR = 30;
begin
  with Canvas do
  begin
    if IsPopup then
    begin
      // popup items, always white background
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      FillRect(ARect);
    end
    else
    begin
      // top level items, depends on the Hotlight status
      if mdHotlight in FState then
      begin
        Brush.Assign(SelectionFrameBrush);
        Pen.Assign(SelectionFramePen);
        Rectangle(ARect);
      end
      else
        if UseFlatMenubars then
        begin
          Brush.Color := GetSysColor(COLOR_MENUBAR);
          Brush.Style := bsSolid;
          Pen.Style := psSolid;
          Pen.Color := Brush.Color;
          FillRect(ARect);
        end
        else
        begin
          Brush.Color := clBtnFace;
          Brush.Style := bsSolid;
          Pen.Style := psSolid;
          Pen.Color := Brush.Color;
          Rectangle(ARect);
        end;
    end;
  end;
end;

procedure TJvXPMenuItemPainter.DrawMenuBitmap(X, Y: Integer; Bitmap: TBitmap);
begin
  if mdDisabled in FState then
    DrawDisabledBitmap(X, Y, Bitmap)
  else
  begin
    // if selected, then draw shadow and shift real image towards
    // top and left, but only if draw bitmap was called because
    // of a user supplied glyph
    if (mdSelected in FState) and Assigned(FGlyph) then
    begin
      DrawBitmapShadow(X + 1, Y + 1, Bitmap);
      Dec(X);
      Dec(Y);
    end;

    if Bitmap.Monochrome and (not FItem.Checked or ShowCheckMarks) then
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else
      DrawBitmapTransparent(Canvas, X, Y, Bitmap, Bitmap.TransparentColor and not PaletteMask);
  end;
end;

procedure TJvXPMenuItemPainter.DrawSelectedFrame(ARect: TRect);
begin
  with Canvas do
  begin
    Font.Color := clMenuText;
    if IsPopup then
    begin
      Brush.Assign(SelectionFrameBrush);
      Pen.Style := psClear;
      Rectangle(0, ARect.Top, ARect.Right + 4, ARect.Bottom - 1);
      Pen.Assign(SelectionFramePen);
      Brush.Style := bsClear;
      MoveTo(0, ARect.Top);
      LineTo(ARect.Right + 4, ARect.Top);
      MoveTo(0, ARect.Bottom - 2);
      LineTo(ARect.Right + 4, ARect.Bottom - 2);
    end
    else
    begin
      Brush.Color := clSilver;
      Brush.Style := bsSolid;
      Pen.Color := clGray;
      Pen.Style := psSolid;
      Rectangle(ARect);
    end;
  end;
end;

procedure TJvXPMenuItemPainter.Measure(Item: TMenuItem;
  var Width, Height: Integer);
begin
  inherited Measure(Item, Width, Height);
  if Item.Caption = Separator then
    Height := 3
  else
    Inc(Height, 2);
end;

procedure TJvXPMenuItemPainter.Paint(Item: TMenuItem; ItemRect: TRect;
  State: TMenuOwnerDrawState);
var
  DesktopCanvas: TJvDesktopCanvas;
  CanvasWindow: HWND;
  WRect: TRect;
begin
  FItem := Item;

  // draw the contour of the window
  if IsPopup and not (csDesigning in ComponentState) then
  begin
    CanvasWindow := WindowFromDC(Canvas.Handle);

    if not (Assigned(FMainMenu) and
      (FMainMenu.GetOwner <> nil) and
      (FMainMenu.GetOwner is TForm) and
      (TForm(FMainMenu.GetOwner).Handle = CanvasWindow)) then
    begin
      DesktopCanvas := TJvDesktopCanvas.Create;
      GetWindowRect(CanvasWindow, WRect);

      with DesktopCanvas do
      begin
        Brush.Style := bsClear;
        Pen.Color := RGB(102, 102, 102);
        Pen.Style := psSolid;

        // dark contour
        Rectangle(WRect);

        // two white lines above bottom
        Pen.Color := clWhite;
        MoveTo(WRect.Left + 1, WRect.Bottom - 2);
        LineTo(WRect.Right - 1, WRect.Bottom - 2);
        MoveTo(WRect.Left + 1, WRect.Bottom - 3);
        LineTo(WRect.Right - 1, WRect.Bottom - 3);

        // two white lines below top
        MoveTo(WRect.Left + 1, WRect.Top + 1);
        LineTo(WRect.Right - 1, WRect.Top + 1);
        MoveTo(WRect.Left + 1, WRect.Top + 2);
        LineTo(WRect.Right - 1, WRect.Top + 2);

        // three lines before right
        if IsRightToLeft then
          Pen.Color := ImageBackgroundColor
        else
          Pen.Color := clWhite;
        MoveTo(WRect.Right - 2, WRect.Top + ItemRect.Top + 3);
        LineTo(WRect.Right - 2, WRect.Top + ItemRect.Bottom + 3);
        MoveTo(WRect.Right - 3, WRect.Top + ItemRect.Top + 3);
        LineTo(WRect.Right - 3, WRect.Top + ItemRect.Bottom + 3);

        // two lines after left
        if IsRightToLeft then
          Pen.Color := clWhite
        else
          Pen.Color := ImageBackgroundColor;
        MoveTo(WRect.Left + 1, WRect.Top + ItemRect.Top + 3);
        LineTo(WRect.Left + 1, WRect.Top + ItemRect.Bottom + 3);
        MoveTo(WRect.Left + 2, WRect.Top + ItemRect.Top + 3);
        LineTo(WRect.Left + 2, WRect.Top + ItemRect.Bottom + 3);

        // and one more line on one side, depending on the bidi mode
        // the line will only be drawn if the drawing rectangle is smaller
        // than the borders
        // also add a line of image background at the top and bottom
        // of the column to look even more like the real XP menus.
        Pen.Style := psSolid;
        Pen.Color := ImageBackgroundColor;
        if not IsRightToLeft then
        begin
          if ItemRect.Left > 0 then
          begin
            MoveTo(WRect.Left + 3, WRect.Top + ItemRect.Top + 3);
            LineTo(WRect.Left + 3, WRect.Top + ItemRect.Bottom + 3);
            MoveTo(WRect.Left + 1, WRect.Top + 2);
            LineTo(WRect.Left + 1 + CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right + 1, WRect.Top + 2);
            MoveTo(WRect.Left + 1, WRect.Bottom - 3);
            LineTo(WRect.Left + 1 + CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right + 1, WRect.Bottom - 3);
          end
          else
          begin
            MoveTo(WRect.Left + 1, WRect.Top + 2);
            LineTo(WRect.Left + 1 + CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right, WRect.Top + 2);
            MoveTo(WRect.Left + 1, WRect.Bottom - 3);
            LineTo(WRect.Left + 1 + CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right, WRect.Bottom - 3);
          end;
        end
        else
        begin
          if ItemRect.Right < WRect.Right - WRect.Left + 1 then
          begin
            MoveTo(WRect.Right - 4, WRect.Top + ItemRect.Top + 3);
            LineTo(WRect.Right - 4, WRect.Top + ItemRect.Bottom + 3);
            MoveTo(WRect.Right - 2, WRect.Top + 2);
            LineTo(WRect.Right - 2 - CheckMarkWidth - ImageMargin.Left - ImageWidth - ImageMargin.Right - 1, WRect.Top + 2);
            MoveTo(WRect.Right - 2, WRect.Bottom - 3);
            LineTo(WRect.Right - 2 - CheckMarkWidth - ImageMargin.Left - ImageWidth - ImageMargin.Right - 1, WRect.Bottom - 3);
          end
          else
          begin
            MoveTo(WRect.Right - 2, WRect.Top + 2);
            LineTo(WRect.Right - 2 - CheckMarkWidth - ImageMargin.Left - ImageWidth - ImageMargin.Right, WRect.Top + 2);
            MoveTo(WRect.Right - 2, WRect.Bottom - 3);
            LineTo(WRect.Right - 2 - CheckMarkWidth - ImageMargin.Left - ImageWidth - ImageMargin.Right, WRect.Bottom - 3);
          end;
        end;

        // if the item is selected, close the selection frame by drawing
        // two lines at the end of the item, 1 pixel from the sides
        // of the window rectangle
        if mdSelected in State then
        begin
          Brush.Style := bsClear;
          Pen.Assign(SelectionFramePen);
          MoveTo(WRect.Left + 2, WRect.Top + ItemRect.Top + 3);
          LineTo(WRect.Left + 2, WRect.Top + ItemRect.Bottom + 2);
          MoveTo(WRect.Right - 3, WRect.Top + ItemRect.Top + 3);
          LineTo(WRect.Right - 3, WRect.Top + ItemRect.Bottom + 2);

          // change the pen for the next instructions to draw in
          // the correct color for a selected item.
          Pen.Style := psSolid;
          Pen.Color := SelectionFrameBrush.Color;

          if IsRightToLeft then
          begin
            MoveTo(WRect.Right - 4, WRect.Top + ItemRect.Top + 3);
            LineTo(WRect.Right - 4, WRect.Top + ItemRect.Bottom + 1);
            Pixels[WRect.Right - 4, WRect.Top + ItemRect.Top + 3] := SelectionFramePen.Color;
            Pixels[WRect.Right - 4, WRect.Top + ItemRect.Bottom + 1] := SelectionFramePen.Color;
          end
          else
          begin
            MoveTo(WRect.Left + 3, WRect.Top + ItemRect.Top + 3);
            LineTo(WRect.Left + 3, WRect.Top + ItemRect.Bottom + 1);
            Pixels[WRect.Left + 3, WRect.Top + ItemRect.Top + 3] := SelectionFramePen.Color;
            Pixels[WRect.Left + 3, WRect.Top + ItemRect.Bottom + 1] := SelectionFramePen.Color;
          end;
        end;
      end;
      DesktopCanvas.Free;
    end;
  end;

  // then draw the items
  inherited Paint(Item, ItemRect, State);
end;

procedure TJvXPMenuItemPainter.PreparePaint(Item: TMenuItem;
  Rect: TRect; State: TMenuOwnerDrawState; Measure: Boolean);
begin
  // to prevent erasing when the item is selected
  Canvas.Brush.Color := clNone;
  inherited PreparePaint(Item, Rect, State, Measure);
end;

procedure TJvXPMenuItemPainter.DrawSeparator(ARect: TRect);
begin
  // draw the separating line
  if IsRightToLeft then
    MenuLine(Canvas, SeparatorColor, ARect.Left, ARect.Top + 1, ARect.Right - CheckMarkWidth - ImageMargin.Left - ImageWidth - ImageMargin.Right - TextMargin, ARect.Top + 1)
  else
    MenuLine(Canvas, SeparatorColor, ARect.Left + CheckMarkWidth + ImageMargin.Left + ImageWidth + ImageMargin.Right + TextMargin, ARect.Top + 1, ARect.Right, ARect.Top + 1);
end;

procedure TJvXPMenuItemPainter.DrawImageBackground(ARect: TRect);
begin
  with Canvas do
  begin
    // draw the gray background in the area
    Brush.Color := ImageBackgroundColor;
    Brush.Style := bsSolid;
    Pen.Style := psClear;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom + 1);
  end;
end;

procedure TJvXPMenuItemPainter.DrawCheckMarkBackground(ARect: TRect);
begin
  DrawImageBackground(ARect);
end;

function TJvXPMenuItemPainter.GetDrawHighlight: Boolean;
begin
  Result := NewStyleControls and
    (not (mdSelected in FState) or (not IsPopup) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) = GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight)))
    );
end;

procedure TJvXPMenuItemPainter.UpdateFieldsFromMenu;
begin
  inherited UpdateFieldsFromMenu;
  FImageMargin.Left := FImageMargin.Left + 6;
  FImageMargin.Top := FImageMargin.Top + 4;
  FImageMargin.Right := FImageMargin.Right + 4;
  FImageMargin.Bottom := FImageMargin.Bottom + 4;
end;

procedure TJvXPMenuItemPainter.DrawText(ARect: TRect; const Text: string;
  Flags: Integer);
begin
  inherited DrawText(Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom - 1), Text, Flags);
end;

function TJvXPMenuItemPainter.GetTextMargin: Integer;
begin
  Result := inherited GetTextMargin + 2;
end;

procedure TJvXPMenuItemPainter.DrawCheckImage(ARect: TRect);
begin
  inherited DrawCheckImage(Rect(ARect.Left - 2, ARect.Top, ARect.Right - 2, ARect.Bottom - 1));
end;

//=== { TJvStandardMenuItemPainter } ========================================

procedure TJvStandardMenuItemPainter.DrawCheckedImageBack(ARect: TRect);
begin
  inherited;
end;

procedure TJvStandardMenuItemPainter.UpdateFieldsFromMenu;
begin
  inherited;
end;

function TJvStandardMenuItemPainter.GetTextMargin: Integer;
begin
  Result := inherited GetTextMargin + 2;
end;

function TJvStandardMenuItemPainter.GetImageWidth: Integer;
var
  I: Integer;
begin
  Result := inherited GetImageWidth;

  // If any of the items has a checkmark then we need to
  // ensure the width of the "image" is enough to display a check
  // mark, and this for all items
  if (FItem.Parent <> nil) then
  begin
    for I := 0 to FItem.Parent.Count - 1 do
    begin
      if FItem.Parent.Items[I].Checked then
      begin
        Result := Max(Result, GetSystemMetrics(SM_CXMENUCHECK));
        Exit;
      end;
    end;
  end;
end;

procedure TJvStandardMenuItemPainter.Paint(Item: TMenuItem;
  ItemRect: TRect; State: TMenuOwnerDrawState);
begin
  inherited;
end;

//=== { TJvOwnerDrawMenuItemPainter } ========================================

procedure TJvOwnerDrawMenuItemPainter.Measure(Item: TMenuItem;
  var Width, Height: Integer);
begin
  if Assigned(FMainMenu) then
  begin
    if Assigned(FMainMenu.OnMeasureItem) then
      FMainMenu.OnMeasureItem(FMainMenu, Item, Width, Height);
  end
  else
  begin
    if Assigned(FPopupMenu.OnMeasureItem) then
      FPopupMenu.OnMeasureItem(FPopupMenu, Item, Width, Height);
  end;
end;

procedure TJvOwnerDrawMenuItemPainter.Paint(Item: TMenuItem; ItemRect: TRect;
  State: TMenuOwnerDrawState);
begin
  if Assigned(FMainMenu) then
  begin
    if Assigned(FMainMenu.OnDrawItem) then
      FMainMenu.OnDrawItem(FMainMenu, Item, ItemRect, State);
  end
  else
  begin
    if Assigned(FPopupMenu.OnDrawItem) then
      FPopupMenu.OnDrawItem(FPopupMenu, Item, ItemRect, State);
  end;
end;

//=== { TJvImageMargin } =====================================================

procedure TJvImageMargin.Assign(Source: TPersistent);
begin
  if Source is TJvImageMargin then
  begin
    Left := TJvImageMargin(Source).Left;
    Right := TJvImageMargin(Source).Right;
    Top := TJvImageMargin(Source).Top;
    Bottom := TJvImageMargin(Source).Bottom;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvMenuImageSize } ===================================================

procedure TJvMenuImageSize.Assign(Source: TPersistent);
begin
  if Source is TJvMenuImageSize then
  begin
    Height := TJvMenuImageSize(Source).Height;
    Width := TJvMenuImageSize(Source).Width;
  end
  else
    inherited Assign(Source);
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

