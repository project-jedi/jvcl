{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxCtrls.pas, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Peter Thornqvist [peter3 att users dott sourceforge dott net]

Last Modified: 2003-09-30

Changes:
2003-09-13:
  * Turned TJvCustomLabel into a consumer.
    Notes: * angled labels will simply use the current item's Text to render and ignore any provider
             specified rendering implementations.
           * D5 users: when changing a property that might clear out the provider (Caption,
             ImageIndex and Image) you can run into Access Violations if the Provider property is
             collapsed. This is due to a limitation in D5 property editors and can not be solved.
2003-08-17:
  * All implementation moved from TJvLabel to TJvCustomLabel. TJvLabel now only publishes
    properties and events.
  * Added Images and ImageIndex support to TJvCustomLabel. Currently, images are always displayed to the left
   of the text. Spacing between image and text is set with Spacing.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* Images are only displayed in TJvCustomLabel if Angle = 0.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvxCtrls;

interface

uses
  Windows, Registry, ShellAPI,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF}
  Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls, Forms,
  Buttons, Menus, IniFiles, ImgList,
  JvTimer, JvConsts, JvPlacemnt, JvComponent, JVCLVer,
  JvTypes;

type
  TPositiveInt = 1..MaxInt;

  TJvTextListBox = class(TCustomListBox)
  private
    FMaxWidth: Integer;
    procedure ResetHorizontalExtent;
    procedure SetHorizontalExtent;
    function GetItemWidth(Index: Integer): Integer;
  protected
    procedure WndProc(var Msg: TMessage); override;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
  end;

  TGetItemWidthEvent = procedure(Control: TWinControl; Index: Integer;
    var Width: Integer) of object;

  TJvxCustomListBox = class(TJvWinControl)
  private
    FItems: TStrings;
    FBorderStyle: TBorderStyle;
    FCanvas: TCanvas;
    FColumns: Integer;
    FItemHeight: Integer;
    FStyle: TListBoxStyle;
    FIntegralHeight: Boolean;
    FMultiSelect: Boolean;
    FSorted: Boolean;
    FExtendedSelect: Boolean;
    FTabWidth: Integer;
    FSaveItems: TStringList;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FAutoScroll: Boolean;
    FGraySelection: Boolean;
    FMaxItemWidth: Integer;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnGetItemWidth: TGetItemWidthEvent;
    procedure ResetHorizontalExtent;
    procedure SetHorizontalExtent;
    function GetAutoScroll: Boolean;
    function GetItemHeight: Integer; virtual;
    function GetItemIndex: Integer;
    function GetSelCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetTopIndex: Integer;
    procedure SetAutoScroll(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnWidth;
    procedure SetColumns(Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure SetGraySelection(Value: Boolean);
    procedure SetOnDrawItem(Value: TDrawItemEvent);
    procedure SetOnGetItemWidth(Value: TGetItemWidthEvent);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function CreateItemList: TStrings; virtual;
    function GetItemWidth(Index: Integer): Integer; virtual;
    procedure WndProc(var Msg: TMessage); override;
    procedure DragCanceled; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    function GetItemData(Index: Integer): Longint; dynamic;
    procedure SetItemData(Index: Integer; AData: Longint); dynamic;
    procedure SetItems(Value: TStrings); virtual;
    procedure ResetContent; dynamic;
    procedure DeleteString(Index: Integer); dynamic;
    property AutoScroll: Boolean read GetAutoScroll write SetAutoScroll default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: Integer read FColumns write SetColumns default 0;
    property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect default True;
    property GraySelection: Boolean read FGraySelection write SetGraySelection default False;
    property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ParentColor default False;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write SetOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnGetItemWidth: TGetItemWidthEvent read FOnGetItemWidth write SetOnGetItemWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DefaultDrawText(X, Y: Integer; const S: string);
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    property Canvas: TCanvas read FCanvas;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelCount: Integer read GetSelCount;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  published
    property TabStop default True;
  end;

  TCheckKind = (ckCheckBoxes, ckRadioButtons, ckCheckMarks);
  TChangeStateEvent = procedure(Sender: TObject; Index: Integer) of object;

  TJvxCheckListBox = class(TJvxCustomListBox)
  private
    FAllowGrayed: Boolean;
    FCheckKind: TCheckKind;
    FSaveStates: TList;
    FDrawBitmap: TBitmap;
    FCheckWidth, FCheckHeight: Integer;
    FReserved: Integer;
    FInUpdateStates: Boolean;
    FIniLink: TJvIniLink;
    FOnClickCheck: TNotifyEvent;
    FOnStateChange: TChangeStateEvent;
    procedure ResetItemHeight;
    function GetItemHeight: Integer; override;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; Enabled: Boolean);
    procedure SetCheckKind(Value: TCheckKind);
    procedure SetChecked(Index: Integer; AChecked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    function GetAllowGrayed: Boolean;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure InvalidateItem(Index: Integer);
    function CreateCheckObject(Index: Integer): TObject;
    function FindCheckObject(Index: Integer): TObject;
    function GetCheckObject(Index: Integer): TObject;
    function IsCheckObject(Index: Integer): Boolean;
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
    procedure ReadCheckData(Reader: TReader);
    procedure WriteCheckData(Writer: TWriter);
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure UpdateCheckStates;
    function GetCheckedIndex: Integer;
    procedure SetCheckedIndex(Value: Integer);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
  protected
    function CreateItemList: TStrings; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetItemWidth(Index: Integer): Integer; override;
    function GetItemData(Index: Integer): Longint; override;
    procedure SetItemData(Index: Integer; AData: Longint); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ResetContent; override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck; dynamic;
    procedure ChangeItemState(Index: Integer); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    function GetCheckWidth: Integer;
    procedure SetItems(Value: TStrings); override;
    procedure InternalLoad(const Section: string);
    procedure InternalSave(const Section: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure LoadFromAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
    //procedure SaveToAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
    procedure Load;
    procedure Save;
    procedure ApplyState(AState: TCheckBoxState; EnabledOnly: Boolean);
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property EnabledItem[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
  published
    property AllowGrayed: Boolean read GetAllowGrayed write FAllowGrayed default False;
    property CheckKind: TCheckKind read FCheckKind write SetCheckKind default ckCheckBoxes;
    property CheckedIndex: Integer read GetCheckedIndex write SetCheckedIndex default -1;
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property Align;
    property AutoScroll default True;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property GraySelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items stored False;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabWidth;
    property Visible;
    property OnStateChange: TChangeStateEvent read FOnStateChange write FOnStateChange;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetItemWidth;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
  end;

const
  clbDefaultState = cbUnchecked;
  clbDefaultEnabled = True;

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
  TJvCustomLabel = class(TJvGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FShadowColor: TColor;
    FShadowSize: Byte;
    FShadowPos: TShadowPosition;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FShowFocus: Boolean;
    FFocused: Boolean;
    FMouseInControl: Boolean;
    FDragging: Boolean;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FChangeLink:TChangeLink;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHintColor: TColor;
    FHintSaved: TColor;
    FAutoOpenURL: boolean;
    FURL: string;
    FAngle: TJvLabelRotateAngle;
    FSpacing: integer;
    FHotTrackFontOptions: TJvTrackFontOptions;
    //FConsumerSvc: TJvDataConsumer;
    function GetTransparent: Boolean;
    procedure UpdateTracking;
    procedure SetAlignment(Value: TAlignment);
    {$IFNDEF COMPILER6_UP} // Polaris
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF}
    procedure SetFocusControl(Value: TWinControl);
    procedure SetLayout(Value: TTextLayout);
    procedure SetLeftMargin(Value: Integer);
    procedure SetRightMargin(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowSize(Value: Byte);
    procedure SetShadowPos(Value: TShadowPosition);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender:TObject);
    procedure DrawAngleText(Flags: Word);
    procedure SetAngle(const Value: TJvLabelRotateAngle);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetSpacing(const Value: integer);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure DoDrawCaption(var Rect: TRect; Flags: Word); virtual;
    procedure DoDrawText(var Rect: TRect; Flags: Word); virtual;
    procedure AdjustBounds;
    {$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF}
    function GetDefaultFontColor: TColor; virtual;
    function GetLabelCaption: string; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseEnter; reintroduce;
    procedure MouseLeave; reintroduce;
    function GetImageWidth:integer;virtual;
    function GetImageHeight:integer;virtual;
    //procedure SetConsumerService(Value: TJvDataConsumer);
    //function ProviderActive: Boolean;
    //procedure ConsumerServiceChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    //procedure NonProviderChange;
    property Angle: TJvLabelRotateAngle read FAngle write SetAngle default 0;
    property AutoOpenURL: boolean read FAutoOpenURL write FAutoOpenURL;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions:TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Images:TCustomImageList read FImages write SetImages;
    property ImageIndex:TImageIndex read FImageIndex write SetImageIndex;
    // specifies the offset between the right edge of the image and the left edge of the text (in pixels)
    property Spacing:integer read FSpacing write SetSpacing default 4;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 0;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 0;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnHighlight;
    property ShadowSize: Byte read FShadowSize write SetShadowSize default 0;
    property ShadowPos: TShadowPosition read FShadowPos write SetShadowPos default spLeftTop;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property URL: string read FURL write FURL;
    //property Provider: TJvDataConsumer read FConsumerSvc write SetConsumerService;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvxLabel = class(TJvCustomLabel)
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowSize;
    property ShadowPos;
    property ShowAccelChar;
    property ShowFocus;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

  TGlyphLayout = (glGlyphLeft, glGlyphRight, glGlyphTop, glGlyphBottom);
  TScrollDirection = (sdVertical, sdHorizontal);
  TPanelDrawEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Rect: TRect) of object;

  TJvSecretPanel = class(TJvCustomPanel)
  private
    FActive: Boolean;
    FAlignment: TAlignment;
    FLines: TStrings;
    FCycled: Boolean;
    FScrollCnt: Integer;
    FMaxScroll: Integer;
    FTxtDivider: Byte;
    FFirstLine: Integer;
    FTimer: TJvTimer;
    FTxtRect: TRect;
    FPaintRect: TRect;
    FGlyphOrigin: TPoint;
    FMemoryImage: TBitmap;
    FGlyph: TBitmap;
    FHiddenList: TList;
    FTextStyle: TPanelBevel;
    FDirection: TScrollDirection;
    FGlyphLayout: TGlyphLayout;
    FOnPaintClient: TPanelDrawEvent;
    FOnStartPlay: TNotifyEvent;
    FOnStopPlay: TNotifyEvent;
    FAsyncDrawing: Boolean;
    procedure SetAsyncDrawing(Value: Boolean);
    function GetInflateWidth: Integer;
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLines(Value: TStrings);
    procedure SetActive(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetGlyphLayout(Value: TGlyphLayout);
    procedure SetTextStyle(Value: TPanelBevel);
    procedure SetDirection(Value: TScrollDirection);
    procedure RecalcDrawRect;
    procedure PaintGlyph;
    procedure PaintText;
    procedure UpdateMemoryImage;
    procedure GlyphChanged(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Paint; override;
    procedure PaintClient(Canvas: TCanvas; Rect: TRect); virtual;
    procedure TimerExpired(Sender: TObject); virtual;
    procedure StartPlay; dynamic;
    procedure StopPlay; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    property Canvas;
  published
    property AsyncDrawing: Boolean read FAsyncDrawing write SetAsyncDrawing default True;
    property Active: Boolean read FActive write SetActive default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Cycled: Boolean read FCycled write FCycled default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphLayout: TGlyphLayout read FGlyphLayout write SetGlyphLayout
      default glGlyphLeft;
    property Interval: Cardinal read GetInterval write SetInterval default 30;
    property Lines: TStrings read FLines write SetLines;
    property ScrollDirection: TScrollDirection read FDirection write SetDirection
      default sdVertical;
    property TextStyle: TPanelBevel read FTextStyle write SetTextStyle default bvNone;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Align;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnPaintClient: TPanelDrawEvent read FOnPaintClient write FOnPaintClient;
    property OnStartPlay: TNotifyEvent read FOnStartPlay write FOnStartPlay;
    property OnStopPlay: TNotifyEvent read FOnStopPlay write FOnStopPlay;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    property OnResize;
  end;

  TJvNumGlyphs = 1..5;
  TJvDropDownMenuPos = (dmpBottom, dmpRight);
  TJvButtonState = (rbsUp, rbsDisabled, rbsDown, rbsExclusive, rbsInactive);

  TJvSpeedButton = class(TJvGraphicControl)
  private
    FAllowAllUp: Boolean;
    FAllowTimer, FOver: Boolean;
    FDown: Boolean;
    FDragging: Boolean;
    FDrawImage: TBitmap;
    FDropDownMenu: TPopupMenu;
    FFlat: Boolean;
    FFontSave: TFont;
    FGlyph: Pointer;
    FGroupIndex: Integer;
    FHintColor: TColor;
    FHotGlyph: TBitmap;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FInactiveGrayed: Boolean;
    FInitRepeatPause: Word;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FMarkDropDown: Boolean;
    FMenuPosition: TJvDropDownMenuPos;
    FMenuTracking: Boolean;
    FModalResult: TModalResult;
    FMouseInControl: Boolean;
    FOldGlyph: TBitmap;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FRepeatPause: Word;
    FRepeatTimer: TTimer;
    FSaved:TColor;
    FSpacing: Integer;
    FStyle: TButtonStyle;
    FTransparent: Boolean;
    function GetAlignment: TAlignment;
    function GetGlyph: TBitmap;
    function GetGrayNewStyle: Boolean;
    function GetNumGlyphs: TJvNumGlyphs;
    function GetWordWrap: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetAllowTimer(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetDropDownMenu(Value: TPopupMenu);
    procedure SetFlat(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGrayNewStyle(const Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetInactiveGrayed(Value: Boolean);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetMarkDropDown(Value: Boolean);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    procedure SetSpacing(Value: Integer);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);

    function CheckMenuDropDown(const Pos: TSmallPoint; Manual: Boolean): Boolean;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GlyphChanged(Sender: TObject);
    procedure TimerExpired(Sender: TObject);
    procedure UpdateExclusive;

    procedure CMButtonPressed(var Msg: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Msg: TMessage); message WM_MOUSEMOVE;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    FState: TJvButtonState;
    //Polaris
    FFlatStandard: Boolean;
    procedure SetFlatStandard(Value: Boolean);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetDropDownMenuPos: TPoint;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure PaintGlyph(Canvas: TCanvas; ARect: TRect; AState: TJvButtonState;
      DrawMark: Boolean); virtual;
    procedure MouseEnter; reintroduce;
    procedure MouseLeave; reintroduce;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property ButtonGlyph: Pointer read FGlyph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick;
    function CheckBtnMenuDropDown: Boolean;
    procedure Click; override;
    procedure UpdateTracking;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property Action;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property AllowTimer: Boolean read FAllowTimer write SetAllowTimer default False;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    { Ensure group index is declared before Down }
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatStandard: Boolean read FFlatStandard write SetFlatStandard default False;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GrayedInactive: Boolean read FInactiveGrayed write SetInactiveGrayed default True;
    property GrayNewStyle: Boolean read GetGrayNewStyle write SetGrayNewStyle default True;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotGlyph: TBitmap read FHotGlyph write SetGlyph;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions:TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property InitPause: Word read FInitRepeatPause write FInitRepeatPause default 500;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property MarkDropDown: Boolean read FMarkDropDown write SetMarkDropDown default True;
    property MenuPosition: TJvDropDownMenuPos read FMenuPosition write FMenuPosition default dmpBottom;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint default False;
    property RepeatInterval: Word read FRepeatPause write FRepeatPause default 100;
    property ShowHint default True;
    property Spacing: Integer read FSpacing write SetSpacing default 1;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvButtonImage = class(TObject)
  private
    FGlyph: TObject;
    FButtonSize: TPoint;
    FCaption: TCaption;
    function GetNumGlyphs: TJvNumGlyphs;
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    procedure DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
      Layout: TButtonLayout; AFont: TFont; Images: TImageList;
      ImageIndex: Integer; Flags: Word);
    procedure Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
      Layout: TButtonLayout; AFont: TFont; Flags: Word);
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read FCaption write FCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonSize: TPoint read FButtonSize write FButtonSize;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

  TJvxButtonGlyph = class
  private
    FAlignment: TAlignment;
    FGlyphList: TImageList;
    FGrayNewStyle: Boolean;
    FIndexs: array[TJvButtonState] of Integer;
    FNumGlyphs: TJvNumGlyphs;
    FOnChange: TNotifyEvent;
    FOriginal: TBitmap;
    FTransparentColor: TColor;
    FWordWrap: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGrayNewStyle(const Value: Boolean);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function MapColor(Color: TColor): TColor;
  protected
    procedure MinimizeCaption(Canvas: TCanvas; const Caption: string;
      Buffer: PChar; MaxLen, Width: Integer);
    function CreateButtonGlyph(State: TJvButtonState): Integer;
    function CreateImageGlyph(State: TJvButtonState; Images: TImageList;
      Index: Integer): Integer;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect;
      Flags: Word; Images: TImageList; ImageIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    function DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TJvButtonState): TPoint;
    function DrawButtonImage(Canvas: TCanvas; X, Y: Integer; Images: TImageList;
      ImageIndex: Integer; State: TJvButtonState): TPoint;
    function DrawEx(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; PopupMark: Boolean;
      Images: TImageList; ImageIndex: Integer; State: TJvButtonState;
      Flags: Word): TRect;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TJvButtonState; Flags: Word);
    procedure DrawPopupMark(Canvas: TCanvas; X, Y: Integer;
      State: TJvButtonState);
    function Draw(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; PopupMark: Boolean;
      State: TJvButtonState; Flags: Word): TRect;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property GrayNewStyle: Boolean read FGrayNewStyle write SetGrayNewStyle;
    property NumGlyphs: TJvNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;

function CheckBitmap: TBitmap;

implementation

{$R JvxCtrls.res}

uses
  SysUtils, Consts, Math, ActnList, CommCtrl,
  JvThemes, JvJVCLUtils, JvJCLUtils, JvFunctions;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

function HeightOf(const R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function WidthOf(const R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

//=== TJvTextListBox =========================================================

procedure TJvTextListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

function TJvTextListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: Longint;
  S: string;
begin
  S := Items[Index] + 'x';
  if TabWidth > 0 then
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
      1, ATabWidth));
  end
  else
    Result := Canvas.TextWidth(S);
end;

procedure TJvTextListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;


procedure TJvTextListBox.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        inherited WndProc(Msg);
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Msg.Result));
        SetHorizontalExtent;
      end;
    LB_DELETESTRING:
      begin
        if GetItemWidth(Msg.WParam) >= FMaxWidth then
        begin
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Msg);
          ResetHorizontalExtent;
        end
        else
          inherited WndProc(Msg);
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHorizontalExtent;
        Perform(WM_HSCROLL, SB_TOP, 0);
        inherited WndProc(Msg);
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Msg);
        Canvas.Font.Assign(Self.Font);
        ResetHorizontalExtent;
        Exit;
      end;
  else
    inherited WndProc(Msg);
  end;
end;

//=== TJvListBoxStrings ======================================================

type
  TJvListBoxStrings = class(TStrings)
  private
    ListBox: TJvxCustomListBox;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

function TJvListBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_GETCOUNT, 0, 0);
end;

function TJvListBoxStrings.Get(Index: Integer): string;
var
  Len: Integer;
  Text: array[0..4095] of Char;
begin
  Len := SendMessage(ListBox.Handle, LB_GETTEXT, Index,
    Longint(@Text));
  if Len < 0 then
    Error(SListIndexError, Index);
  SetString(Result, Text, Len);
end;

function TJvListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(ListBox.GetItemData(Index));
  if Longint(Result) = LB_ERR then
    Error(SListIndexError, Index);
end;

procedure TJvListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  ListBox.SetItemData(Index, Longint(AObject));
end;

function TJvListBoxStrings.Add(const S: string): Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TJvListBoxStrings.Insert(Index: Integer; const S: string);
begin
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TJvListBoxStrings.Delete(Index: Integer);
begin
  ListBox.DeleteString(Index);
end;

procedure TJvListBoxStrings.Clear;
begin
  ListBox.ResetContent;
end;

procedure TJvListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(ListBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
    ListBox.Refresh;
end;

//=== TJvxCustomListBox ======================================================

{ TJvxCustomListBox implementation copied from STDCTRLS.PAS and modified }

procedure ListIndexError(Index: Integer);

function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
begin
  raise EStringListError.CreateFmt(SListIndexError, [Index])at ReturnAddr;
end;

constructor TJvxCustomListBox.Create(AOwner: TComponent);
const
  ListBoxStyle = [csSetCaption, csDoubleClicks];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := ListBoxStyle
  else
    ControlStyle := ListBoxStyle + [csFramed];
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FItems := CreateItemList;
  TJvListBoxStrings(FItems).ListBox := Self;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FBorderStyle := bsSingle;
  FExtendedSelect := True;
end;

destructor TJvxCustomListBox.Destroy;
begin
  FCanvas.Free;
  FItems.Free;
  FSaveItems.Free;
  // (rom) inherited moved to end
  inherited Destroy;
end;

function TJvxCustomListBox.CreateItemList: TStrings;
begin
  Result := TJvListBoxStrings.Create;
end;

function TJvxCustomListBox.GetItemData(Index: Integer): Longint;
begin
  Result := SendMessage(Handle, LB_GETITEMDATA, Index, 0);
end;

procedure TJvxCustomListBox.SetItemData(Index: Integer; AData: Longint);
begin
  SendMessage(Handle, LB_SETITEMDATA, Index, AData);
end;

procedure TJvxCustomListBox.DeleteString(Index: Integer);
begin
  SendMessage(Handle, LB_DELETESTRING, Index, 0);
end;

procedure TJvxCustomListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxItemWidth, 0);
end;

function TJvxCustomListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: Longint;
  S: string;
begin
  if (Style <> lbStandard) and Assigned(FOnGetItemWidth) and
    Assigned(FOnDrawItem) then
  begin
    Result := 0;
    FOnGetItemWidth(Self, Index, Result);
  end
  else
  begin
    S := Items[Index] + 'x';
    if TabWidth > 0 then
    begin
      {if (FTabChar > #0) then
        for I := 1 to Length(S) do
          if S[I] = FTabChar then S[I] := #9;}
      ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
      Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
        1, ATabWidth));
    end
    else
      Result := Canvas.TextWidth(S);
  end;
end;

procedure TJvxCustomListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxItemWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxItemWidth := Max(FMaxItemWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TJvxCustomListBox.ResetContent;
begin
  SendMessage(Handle, LB_RESETCONTENT, 0, 0);
end;

procedure TJvxCustomListBox.Clear;
begin
  FItems.Clear;
end;

procedure TJvxCustomListBox.SetColumnWidth;
begin
  if FColumns > 0 then
    SendMessage(Handle, LB_SETCOLUMNWIDTH, (Width + FColumns - 3) div FColumns, 0);
end;

procedure TJvxCustomListBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
    if (FColumns = 0) or (Value = 0) then
    begin
      FColumns := Value;
      RecreateWnd;
    end
    else
    begin
      FColumns := Value;
      if HandleAllocated then
        SetColumnWidth;
    end;
end;

function TJvxCustomListBox.GetItemIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
end;

function TJvxCustomListBox.GetSelCount: Integer;
begin
  Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
end;

procedure TJvxCustomListBox.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
    SendMessage(Handle, LB_SETCURSEL, Value, 0);
end;

procedure TJvxCustomListBox.SetExtendedSelect(Value: Boolean);
begin
  if Value <> FExtendedSelect then
  begin
    FExtendedSelect := Value;
    RecreateWnd;
  end;
end;

procedure TJvxCustomListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then
  begin
    FIntegralHeight := Value;
    RecreateWnd;
  end;
end;

function TJvxCustomListBox.GetAutoScroll: Boolean;
begin
  Result := FAutoScroll and (Columns = 0);
end;

procedure TJvxCustomListBox.SetOnDrawItem(Value: TDrawItemEvent);
begin
  if Assigned(FOnDrawItem) <> Assigned(Value) then
  begin
    FOnDrawItem := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  end
  else
    FOnDrawItem := Value;
end;

procedure TJvxCustomListBox.SetOnGetItemWidth(Value: TGetItemWidthEvent);
begin
  if Assigned(FOnGetItemWidth) <> Assigned(Value) then
  begin
    FOnGetItemWidth := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  end
  else
    FOnGetItemWidth := Value;
end;

procedure TJvxCustomListBox.SetAutoScroll(Value: Boolean);
begin
  if AutoScroll <> Value then
  begin
    FAutoScroll := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
    begin
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    end;
  end;
end;

function TJvxCustomListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and (FStyle = lbStandard) then
  begin
    Perform(LB_GETITEMRECT, 0, Longint(@R));
    Result := R.Bottom - R.Top;
  end;
end;

procedure TJvxCustomListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

procedure TJvxCustomListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    RecreateWnd;
  end;
end;

procedure TJvxCustomListBox.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

function TJvxCustomListBox.GetSelected(Index: Integer): Boolean;
var
  R: Longint;
begin
  R := SendMessage(Handle, LB_GETSEL, Index, 0);
  if R = LB_ERR then
    ListIndexError(Index);
  Result := LongBool(R);
end;

procedure TJvxCustomListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  if MultiSelect then
  begin
    if SendMessage(Handle, LB_SETSEL, Ord(Value), Index) = LB_ERR then
      ListIndexError(Index);
  end
  else
  begin
    if Value then
      SetItemIndex(Index)
    else if ItemIndex = Index then
      SetItemIndex(-1);
  end;
end;

procedure TJvxCustomListBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TJvxCustomListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

function TJvxCustomListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETTOPINDEX, 0, 0);
end;

procedure TJvxCustomListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvxCustomListBox.SetTopIndex(Value: Integer);
begin
  if GetTopIndex <> Value then
    SendMessage(Handle, LB_SETTOPINDEX, Value, 0);
end;

procedure TJvxCustomListBox.SetGraySelection(Value: Boolean);
begin
  if FGraySelection <> Value then
  begin
    FGraySelection := Value;
    if not Focused then
      Invalidate;
  end;
end;

procedure TJvxCustomListBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvxCustomListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
  if PtInRect(ClientRect, Pos) then
  begin
    Result := TopIndex;
    Count := Items.Count;
    while Result < Count do
    begin
      Perform(LB_GETITEMRECT, Result, Longint(@ItemRect));
      if PtInRect(ItemRect, Pos) then
        Exit;
      Inc(Result);
    end;
    if not Existing then
      Exit;
  end;
  Result := -1;
end;

function TJvxCustomListBox.ItemRect(Index: Integer): TRect;
var
  Count: Integer;
begin
  Count := Items.Count;
  if (Index = 0) or (Index < Count) then
    Perform(LB_GETITEMRECT, Index, Longint(@Result))
  else if Index = Count then
  begin
    Perform(LB_GETITEMRECT, Index - 1, Longint(@Result));
    OffsetRect(Result, 0, Result.Bottom - Result.Top);
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TJvxCustomListBox.CreateParams(var Params: TCreateParams);
type
  PSelects = ^TSelects;
  TSelects = array[Boolean] of Longword;
const
  BorderStyles: array[TBorderStyle] of Longword = (0, WS_BORDER);
  Styles: array[TListBoxStyle] of Longword =
  (0, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWVARIABLE
    {$IFDEF COMPILER6_UP}, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWFIXED{$ENDIF});
  Sorteds: TSelects = (0, LBS_SORT);
  MultiSelects: TSelects = (0, LBS_MULTIPLESEL);
  ExtendSelects: TSelects = (0, LBS_EXTENDEDSEL);
  IntegralHeights: TSelects = (LBS_NOINTEGRALHEIGHT, 0);
  MultiColumns: TSelects = (0, LBS_MULTICOLUMN);
  TabStops: TSelects = (0, LBS_USETABSTOPS);
var
  Selects: PSelects;
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'LISTBOX');
  with Params do
  begin
    Selects := @MultiSelects;
    if FExtendedSelect then
      Selects := @ExtendSelects;
    Style := Style or (WS_HSCROLL or WS_VSCROLL or LBS_HASSTRINGS or
      LBS_NOTIFY) or Styles[FStyle] or Sorteds[FSorted] or
      Selects^[FMultiSelect] or IntegralHeights[FIntegralHeight] or
      MultiColumns[FColumns <> 0] or BorderStyles[FBorderStyle] or
      TabStops[FTabWidth <> 0];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TJvxCustomListBox.CreateWnd;
var
  W, H: Integer;
begin
  W := Width;
  H := Height;
  inherited CreateWnd;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  if FTabWidth <> 0 then
    SendMessage(Handle, LB_SETTABSTOPS, 1, Longint(@FTabWidth));
  SetColumnWidth;
  if FSaveItems <> nil then
  begin
    FItems.Assign(FSaveItems);
    SetTopIndex(FSaveTopIndex);
    SetItemIndex(FSaveItemIndex);
    FSaveItems.Free;
    FSaveItems := nil;
  end;
end;

procedure TJvxCustomListBox.DestroyWnd;
begin
  if FItems.Count > 0 then
  begin
    FSaveItems := TStringList.Create;
    FSaveItems.Assign(FItems);
    FSaveTopIndex := GetTopIndex;
    FSaveItemIndex := GetItemIndex;
  end;
  inherited DestroyWnd;
end;

procedure TJvxCustomListBox.WndProc(var Msg: TMessage);
begin
  if AutoScroll then
  begin
    case Msg.Msg of
      LB_ADDSTRING, LB_INSERTSTRING:
        begin
          inherited WndProc(Msg);
          FMaxItemWidth := Max(FMaxItemWidth, GetItemWidth(Msg.Result));
          SetHorizontalExtent;
          Exit;
        end;
      LB_DELETESTRING:
        begin
          if GetItemWidth(Msg.WParam) >= FMaxItemWidth then
          begin
            Perform(WM_HSCROLL, SB_TOP, 0);
            inherited WndProc(Msg);
            ResetHorizontalExtent;
          end
          else
            inherited WndProc(Msg);
          Exit;
        end;
      LB_RESETCONTENT:
        begin
          FMaxItemWidth := 0;
          SetHorizontalExtent;
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Msg);
          Exit;
        end;
      WM_SETFONT:
        begin
          inherited WndProc(Msg);
          Canvas.Font.Assign(Self.Font);
          ResetHorizontalExtent;
          Exit;
        end;
    end;
  end;
  {for auto drag mode, let listbox handle itself, instead of TControl}
  if not (csDesigning in ComponentState) and ((Msg.Msg = WM_LBUTTONDOWN) or
    (Msg.Msg = WM_LBUTTONDBLCLK)) and not Dragging then
  begin
    if DragMode = dmAutomatic then
    begin
      if IsControlMouseMsg(TWMMouse(Msg)) then
        Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Msg); {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  inherited WndProc(Msg);
end;

procedure TJvxCustomListBox.WMLButtonDown(var Msg: TWMLButtonDown);
var
  ItemNo: Integer;
  ShiftState: TShiftState;
begin
  ShiftState := KeysToShiftState(Msg.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then
  begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then
    begin
      ItemNo := ItemAtPos(SmallPointToPoint(Msg.Pos), True);
      if (ItemNo >= 0) and (Selected[ItemNo]) then
      begin
        BeginDrag(False);
        Exit;
      end;
    end;
  end;
  inherited;
  if (DragMode = dmAutomatic) and not (FMultiSelect and
    ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then
    BeginDrag(False);
end;

procedure TJvxCustomListBox.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if csDesigning in ComponentState then
    DefaultHandler(Msg)
  else
    inherited;
end;

procedure TJvxCustomListBox.CNCommand(var Msg: TWMCommand);
begin
  case Msg.NotifyCode of
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
    LBN_DBLCLK: DblClick;
  end;
end;

procedure TJvxCustomListBox.WMPaint(var Msg: TWMPaint);

  procedure PaintListBox;
  var
    DrawItemMsg: TWMDrawItem;
    MeasureItemMsg: TWMMeasureItem;
    DrawItemStruct: TDrawItemStruct;
    MeasureItemStruct: TMeasureItemStruct;
    R: TRect;
    Y, I, H, W: Integer;
  begin
    { Initialize drawing records }
    DrawItemMsg.Msg := CN_DRAWITEM;
    DrawItemMsg.DrawItemStruct := @DrawItemStruct;
    DrawItemMsg.Ctl := Handle;
    DrawItemStruct.CtlType := ODT_LISTBOX;
    DrawItemStruct.itemAction := ODA_DRAWENTIRE;
    DrawItemStruct.itemState := 0;
    DrawItemStruct.HDC := Msg.DC;
    DrawItemStruct.CtlID := Handle;
    DrawItemStruct.hwndItem := Handle;
    { Intialize measure records }
    MeasureItemMsg.Msg := CN_MEASUREITEM;
    MeasureItemMsg.IDCtl := Handle;
    MeasureItemMsg.MeasureItemStruct := @MeasureItemStruct;
    MeasureItemStruct.CtlType := ODT_LISTBOX;
    MeasureItemStruct.CtlID := Handle;
    { Draw the listbox }
    Y := 0;
    I := TopIndex;
    GetClipBox(Msg.DC, R);
    H := Height;
    W := Width;
    while Y < H do
    begin
      MeasureItemStruct.itemID := I;
      if I < Items.Count then
        MeasureItemStruct.itemData := Longint(Pointer(Items.Objects[I]));
      MeasureItemStruct.itemWidth := W;
      MeasureItemStruct.itemHeight := FItemHeight;
      DrawItemStruct.itemData := MeasureItemStruct.itemData;
      DrawItemStruct.itemID := I;
      Dispatch(MeasureItemMsg);
      DrawItemStruct.rcItem := Rect(0, Y, MeasureItemStruct.itemWidth,
        Y + Integer(MeasureItemStruct.itemHeight));
      Dispatch(DrawItemMsg);
      Inc(Y, MeasureItemStruct.itemHeight);
      Inc(I);
      if I >= Items.Count then
        Break;
    end;
  end;

begin
  if Msg.DC <> 0 then
    PaintListBox
  else
    inherited;
end;

procedure TJvxCustomListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  SetColumnWidth;
end;

procedure TJvxCustomListBox.DragCanceled;
var
  M: TWMMouse;
  MousePos: TPoint;
begin
  with M do
  begin
    Msg := WM_LBUTTONDOWN;
    GetCursorPos(MousePos);
    Pos := PointToSmallPoint(ScreenToClient(MousePos));
    Keys := 0;
    Result := 0;
  end;
  DefaultHandler(M);
  M.Msg := WM_LBUTTONUP;
  DefaultHandler(M);
end;

procedure TJvxCustomListBox.DefaultDrawText(X, Y: Integer; const S: string);
var
  ATabWidth: Longint;
begin
  TControlCanvas(FCanvas).UpdateTextFlags;
  if FTabWidth = 0 then
    FCanvas.TextOut(X, Y, S)
  else
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    TabbedTextOut(FCanvas.Handle, X, Y, @S[1], Length(S), 1, ATabWidth, X);
  end;
end;

procedure TJvxCustomListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect, State)
  else
  begin
    FCanvas.FillRect(Rect);
    if Index < Items.Count then
    begin
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      DefaultDrawText(Rect.Left, Max(Rect.Top, (Rect.Bottom +
        Rect.Top - Canvas.TextHeight('Wy')) div 2), Items[Index]);
    end;
  end;
end;

procedure TJvxCustomListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Index, Height)
end;

procedure TJvxCustomListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    FCanvas.Handle := HDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      with FCanvas do
        if not (csDesigning in ComponentState) and FGraySelection and
          not Focused then
        begin
          Brush.Color := clBtnFace;
          if ColorToRGB(Font.Color) = ColorToRGB(clBtnFace) then
            Font.Color := clBtnText;
        end
        else
        begin
          Brush.Color := clHighlight;
          Font.Color := clHighlightText
        end;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    if odFocused in State then
      DrawFocusRect(HDC, rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TJvxCustomListBox.CNMeasureItem(var Msg: TWMMeasureItem);
begin
  with Msg.MeasureItemStruct^ do
  begin
    itemHeight := FItemHeight;
    if FStyle = lbOwnerDrawVariable then
      MeasureItem(itemID, Integer(itemHeight));
  end;
end;

procedure TJvxCustomListBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if FGraySelection and MultiSelect and (SelCount > 1) then
    Invalidate;
end;

procedure TJvxCustomListBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if FGraySelection and MultiSelect and (SelCount > 1) then
    Invalidate;
end;

procedure TJvxCustomListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

//=== TJvCheckListBoxItem ====================================================

type
  TJvCheckListBoxItem = class
  private
    FData: Longint;
    FState: TCheckBoxState;
    FEnabled: Boolean;
    function GetChecked: Boolean;
  public
    constructor Create;
    property Checked: Boolean read GetChecked;
    property Enabled: Boolean read FEnabled write FEnabled;
    property State: TCheckBoxState read FState write FState;
  end;

constructor TJvCheckListBoxItem.Create;
begin
  inherited Create;
  FState := clbDefaultState;
  FEnabled := clbDefaultEnabled;
end;

function TJvCheckListBoxItem.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

//=== TJvCheckListBoxStrings =================================================

type
  TJvCheckListBoxStrings = class(TJvListBoxStrings)
  public
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

procedure TJvCheckListBoxStrings.Exchange(Index1, Index2: Integer);
var
  TempEnabled1, TempEnabled2: Boolean;
  TempState1, TempState2: TCheckBoxState;
begin
  with TJvxCheckListBox(ListBox) do
  begin
    TempState1 := State[Index1];
    TempEnabled1 := EnabledItem[Index1];
    TempState2 := State[Index2];
    TempEnabled2 := EnabledItem[Index2];
    inherited Exchange(Index1, Index2);
    State[Index1] := TempState2;
    EnabledItem[Index1] := TempEnabled2;
    State[Index2] := TempState1;
    EnabledItem[Index2] := TempEnabled1;
  end;
end;

procedure TJvCheckListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  TempEnabled: Boolean;
  TempState: TCheckBoxState;
begin
  with TJvxCheckListBox(ListBox) do
  begin
    TempState := State[CurIndex];
    TempEnabled := EnabledItem[CurIndex];
    inherited Move(CurIndex, NewIndex);
    State[NewIndex] := TempState;
    EnabledItem[NewIndex] := TempEnabled;
  end;
end;

//=== TJvxCheckListBox =======================================================

// (rom) changed to var
var
  GCheckBitmap: TBitmap = nil;

function CheckBitmap: TBitmap;
begin
  if GCheckBitmap = nil then
  begin
    GCheckBitmap := TBitmap.Create;
    GCheckBitmap.Handle := LoadBitmap(HInstance, 'JV_CHECK_IMAGES');
  end;
  Result := GCheckBitmap;
end;

procedure DestroyLocals;
begin
  if GCheckBitmap <> nil then
  begin
    GCheckBitmap.Free;
    GCheckBitmap := nil;
  end;
end;

const
  InternalVersion = 202; { for backward compatibility only }

constructor TJvxCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoScroll := True;
  with CheckBitmap do
  begin
    FCheckWidth := Width div 6;
    FCheckHeight := Height div 3;
  end;
  FDrawBitmap := TBitmap.Create;
  with FDrawBitmap do
  begin
    Width := FCheckWidth;
    Height := FCheckHeight;
  end;
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
end;

destructor TJvxCheckListBox.Destroy;
begin
  FSaveStates.Free;
  FSaveStates := nil;
  FDrawBitmap.Free;
  FDrawBitmap := nil;
  FIniLink.Free;
  inherited Destroy;
end;

procedure TJvxCheckListBox.Loaded;
begin
  inherited Loaded;
  UpdateCheckStates;
end;

function TJvxCheckListBox.CreateItemList: TStrings;
begin
  Result := TJvCheckListBoxStrings.Create;
end;

const
  sCount = 'Count';
  sItem = 'Item';

{procedure TJvxCheckListBox.LoadFromAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
var
  I: Integer;
  ACount: Integer;
begin
  ACount := Min(AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sCount]), 0), Items.Count);
  for I := 0 to ACount - 1 do
  begin
    State[I] := TCheckBoxState(AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sItem + IntToStr(I)]),
      Integer(clbDefaultState)));
    if (State[I] = cbChecked) and (FCheckKind = ckRadioButtons) then
      Exit;
  end;
end;

procedure TJvxCheckListBox.SaveToAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
var
  I: Integer;
begin
  AppStorage.DeleteSubTree(Path);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sCount]), Items.Count);
  for I := 0 to Items.Count - 1 do
    AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sItem + IntToStr(I)]), Integer(State[I]));
end;
}

procedure TJvxCheckListBox.Load;
begin
  IniLoad(nil);
end;

procedure TJvxCheckListBox.Save;
begin
  IniSave(nil);
end;

function TJvxCheckListBox.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvxCheckListBox.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TJvxCheckListBox.IniSave(Sender: TObject);
begin
{  if (Name <> '') and (IniStorage.IsActive) then
    InternalSave(GetDefaultSection(Self));}
end;

procedure TJvxCheckListBox.IniLoad(Sender: TObject);
begin
{  if (Name <> '') and (IniStorage.IsActive) then
    InternalLoad(GetDefaultSection(Self));}
end;

procedure TJvxCheckListBox.ReadCheckData(Reader: TReader);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    Reader.ReadListBegin;
    Clear;
    while not Reader.EndOfList do
    begin
      I := Items.Add(Reader.ReadString);
      if FReserved >= InternalVersion then
      begin
        State[I] := TCheckBoxState(Reader.ReadInteger);
        EnabledItem[I] := Reader.ReadBoolean;
      end
      else
      begin { for backward compatibility only }
        Checked[I] := Reader.ReadBoolean;
        EnabledItem[I] := Reader.ReadBoolean;
        if FReserved > 0 then
          State[I] := TCheckBoxState(Reader.ReadInteger);
      end;
    end;
    Reader.ReadListEnd;
    UpdateCheckStates;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvxCheckListBox.WriteCheckData(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to Items.Count - 1 do
    begin
      WriteString(Items[I]);
      WriteInteger(Integer(Self.State[I]));
      WriteBoolean(EnabledItem[I]);
    end;
    WriteListEnd;
  end;
end;

procedure TJvxCheckListBox.ReadVersion(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TJvxCheckListBox.WriteVersion(Writer: TWriter);
begin
  Writer.WriteInteger(InternalVersion);
end;

procedure TJvxCheckListBox.DefineProperties(Filer: TFiler);

function DoWrite: Boolean;
  var
    I: Integer;
    Ancestor: TJvxCheckListBox;
  begin
    Result := False;
    Ancestor := TJvxCheckListBox(Filer.Ancestor);
    if (Ancestor <> nil) and (Ancestor.Items.Count = Items.Count) and
      (Ancestor.Items.Count > 0) then
      for I := 1 to Items.Count - 1 do
      begin
        Result := (CompareText(Items[I], Ancestor.Items[I]) <> 0) or
          (State[I] <> Ancestor.State[I]) or
          (EnabledItem[I] <> Ancestor.EnabledItem[I]);
        if Result then
          Break;
      end
    else
      Result := Items.Count > 0;
  end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('InternalVersion', ReadVersion, WriteVersion, Filer.Ancestor = nil);
  Filer.DefineProperty('Strings', ReadCheckData, WriteCheckData, DoWrite);
end;

procedure TJvxCheckListBox.CreateWnd;
begin
  inherited CreateWnd;
  if FSaveStates <> nil then
  begin
    FSaveStates.Free;
    FSaveStates := nil;
  end;
  ResetItemHeight;
end;

procedure TJvxCheckListBox.DestroyWnd;
begin
  inherited DestroyWnd;
end;

procedure TJvxCheckListBox.WMDestroy(var Msg: TWMDestroy);
var
  I: Integer;
begin
  if Items.Count > 0 then
  begin
    if FSaveStates <> nil then
      FSaveStates.Clear
    else
      FSaveStates := TList.Create;
    for I := 0 to Items.Count - 1 do
    begin
      FSaveStates.Add(TObject(MakeLong(Ord(EnabledItem[I]), Word(State[I]))));
      FindCheckObject(I).Free;
    end;
  end;
  inherited;
end;

procedure TJvxCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

procedure TJvxCheckListBox.SetItems(Value: TStrings);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    inherited SetItems(Value);
    if (Value <> nil) and (Value is TJvListBoxStrings) and
      (TJvListBoxStrings(Value).ListBox <> nil) and
      (TJvListBoxStrings(Value).ListBox is TJvxCheckListBox) then
    begin
      for I := 0 to Items.Count - 1 do
        if I < Value.Count then
        begin
          Self.State[I] := TJvxCheckListBox(TJvListBoxStrings(Value).ListBox).State[I];
          EnabledItem[I] :=
            TJvxCheckListBox(TJvListBoxStrings(Value).ListBox).EnabledItem[I];
        end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvxCheckListBox.InternalLoad(const Section: string);
begin
  {if IniStorage.IsActive then
    with IniStorage do
      LoadFromAppStore(AppStorage, AppStorage.ConcatPaths([AppStoragePath, Section]));}
end;

procedure TJvxCheckListBox.InternalSave(const Section: string);
begin
{  if IniStorage.IsActive then
    with IniStorage do
      SaveToAppStore(AppStorage, AppStorage.ConcatPaths([AppStoragePath, Section]));}
end;

function TJvxCheckListBox.GetItemWidth(Index: Integer): Integer;
begin
  Result := inherited GetItemWidth(Index) + GetCheckWidth;
end;

function TJvxCheckListBox.GetCheckWidth: Integer;
begin
  Result := FCheckWidth + 2;
end;

function TJvxCheckListBox.GetAllowGrayed: Boolean;
begin
  Result := FAllowGrayed and (FCheckKind in [ckCheckBoxes, ckCheckMarks]);
end;

procedure TJvxCheckListBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

function TJvxCheckListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and ((FStyle = lbStandard) or
    ((FStyle = lbOwnerDrawFixed) and not Assigned(FOnDrawItem))) then
  begin
    Perform(LB_GETITEMRECT, 0, Longint(@R));
    Result := R.Bottom - R.Top;
  end;
end;

procedure TJvxCheckListBox.ResetItemHeight;
var
  H: Integer;
begin
  if (Style = lbStandard) or ((Style = lbOwnerDrawFixed) and
    not Assigned(FOnDrawItem)) then
  begin
    Canvas.Font := Font;
    H := Max(Canvas.TextHeight('Wg'), FCheckHeight);
    if Style = lbOwnerDrawFixed then
      H := Max(H, FItemHeight);
    Perform(LB_SETITEMHEIGHT, 0, H);
    if (H * Items.Count) <= ClientHeight then
      SetScrollRange(Handle, SB_VERT, 0, 0, True);
  end;
end;

procedure TJvxCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  SaveEvent: TDrawItemEvent;
begin
  if Index < Items.Count then
  begin
    R := Rect;
    if not UseRightToLeftAlignment then
    begin
      R.Right := Rect.Left;
      R.Left := R.Right - GetCheckWidth;
    end
    else
    begin
      R.Left := Rect.Right;
      R.Right := R.Left + GetCheckWidth;
    end;
    DrawCheck(R, GetState(Index), EnabledItem[Index]);
    if not EnabledItem[Index] then
      if odSelected in State then
        Canvas.Font.Color := clInactiveCaptionText
      else
        Canvas.Font.Color := clGrayText;
  end;
  if (Style = lbStandard) and Assigned(FOnDrawItem) then
  begin
    SaveEvent := OnDrawItem;
    OnDrawItem := nil;
    try
      inherited DrawItem(Index, Rect, State);
    finally
      OnDrawItem := SaveEvent;
    end;
  end
  else
    inherited DrawItem(Index, Rect, State);
end;

procedure TJvxCheckListBox.CNDrawItem(var Msg: TWMDrawItem);
begin
  with Msg.DrawItemStruct^ do
    if not UseRightToLeftAlignment then
      rcItem.Left := rcItem.Left + GetCheckWidth
    else
      rcItem.Right := rcItem.Right - GetCheckWidth;
  inherited;
end;

procedure TJvxCheckListBox.DrawCheck(R: TRect; AState: TCheckBoxState;
  Enabled: Boolean);
const
  CheckImages: array[TCheckBoxState, TCheckKind, Boolean] of Integer =
  (((3, 0), (9, 6), (15, 12)), { unchecked }
    ((4, 1), (10, 7), (16, 13)), { checked   }
    ((5, 2), (11, 8), (17, 14))); { grayed    }
var
  DrawRect: TRect;
  SaveColor: TColor;
begin
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckHeight) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
  SaveColor := Canvas.Brush.Color;
  AssignBitmapCell(CheckBitmap, FDrawBitmap, 6, 3,
    CheckImages[AState, FCheckKind, Enabled]);
  Canvas.Brush.Color := Self.Color;
  try
    Canvas.BrushCopy(DrawRect, FDrawBitmap, Bounds(0, 0, FCheckWidth,
      FCheckHeight), CheckBitmap.TransparentColor and not PaletteMask);
  finally
    Canvas.Brush.Color := SaveColor;
  end;
end;

procedure TJvxCheckListBox.ApplyState(AState: TCheckBoxState;
  EnabledOnly: Boolean);
var
  I: Integer;
begin
  if FCheckKind in [ckCheckBoxes, ckCheckMarks] then
    for I := 0 to Items.Count - 1 do
      if not EnabledOnly or EnabledItem[I] then
        State[I] := AState;
end;

function TJvxCheckListBox.GetCheckedIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  if FCheckKind = ckRadioButtons then
    for I := 0 to Items.Count - 1 do
      if State[I] = cbChecked then
      begin
        Result := I;
        Exit;
      end;
end;

procedure TJvxCheckListBox.SetCheckedIndex(Value: Integer);
begin
  if (FCheckKind = ckRadioButtons) and (Items.Count > 0) then
    SetState(Max(Value, 0), cbChecked);
end;

procedure TJvxCheckListBox.UpdateCheckStates;
begin
  if (FCheckKind = ckRadioButtons) and (Items.Count > 0) then
  begin
    FInUpdateStates := True;
    try
      SetState(Max(GetCheckedIndex, 0), cbChecked);
    finally
      FInUpdateStates := False;
    end;
  end;
end;

procedure TJvxCheckListBox.SetCheckKind(Value: TCheckKind);
begin
  if FCheckKind <> Value then
  begin
    FCheckKind := Value;
    UpdateCheckStates;
    Invalidate;
  end;
end;

procedure TJvxCheckListBox.SetChecked(Index: Integer; AChecked: Boolean);
const
  CheckStates: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
begin
  SetState(Index, CheckStates[AChecked]);
end;

procedure TJvxCheckListBox.SetState(Index: Integer; AState: TCheckBoxState);
var
  I: Integer;
begin
  if (AState <> GetState(Index)) or FInUpdateStates then
  begin
    if (FCheckKind = ckRadioButtons) and (AState = cbUnchecked) and
      (GetCheckedIndex = Index) then
      Exit;
    TJvCheckListBoxItem(GetCheckObject(Index)).State := AState;
    if (FCheckKind = ckRadioButtons) and (AState = cbChecked) then
      for I := Items.Count - 1 downto 0 do
      begin
        if (I <> Index) and (GetState(I) = cbChecked) then
        begin
          TJvCheckListBoxItem(GetCheckObject(I)).State := cbUnchecked;
          InvalidateCheck(I);
        end;
      end;
    InvalidateCheck(Index);
    if not (csReading in ComponentState) then
      ChangeItemState(Index);
  end;
end;

procedure TJvxCheckListBox.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then
  begin
    TJvCheckListBoxItem(GetCheckObject(Index)).Enabled := Value;
    InvalidateItem(Index);
  end;
end;

procedure TJvxCheckListBox.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  if not UseRightToLeftAlignment then
    R.Right := R.Left + GetCheckWidth
  else
    R.Left := R.Right - GetCheckWidth;
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  UpdateWindow(Handle);
end;

procedure TJvxCheckListBox.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  UpdateWindow(Handle);
end;

function TJvxCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  if IsCheckObject(Index) then
    Result := TJvCheckListBoxItem(GetCheckObject(Index)).GetChecked
  else
    Result := False;
end;

function TJvxCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if IsCheckObject(Index) then
    Result := TJvCheckListBoxItem(GetCheckObject(Index)).State
  else
    Result := clbDefaultState;
  if (FCheckKind = ckRadioButtons) and (Result <> cbChecked) then
    Result := cbUnchecked;
end;

function TJvxCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  if IsCheckObject(Index) then
    Result := TJvCheckListBoxItem(GetCheckObject(Index)).Enabled
  else
    Result := clbDefaultEnabled;
end;

procedure TJvxCheckListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ' ':
      begin
        ToggleClickCheck(ItemIndex);
        Key := #0;
      end;
    '+':
      begin
        ApplyState(cbChecked, True);
        ClickCheck;
      end;
    '-':
      begin
        ApplyState(cbUnchecked, True);
        ClickCheck;
      end;
  end;
end;

procedure TJvxCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    Index := ItemAtPos(Point(X, Y), True);
    if Index <> -1 then
    begin
      if not UseRightToLeftAlignment then
      begin
        if X - ItemRect(Index).Left < GetCheckWidth then
          ToggleClickCheck(Index);
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          ToggleClickCheck(Index);
      end;
    end;
  end;
end;

procedure TJvxCheckListBox.ToggleClickCheck(Index: Integer);
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and EnabledItem[Index] then
  begin
    State := Self.State[Index];
    case State of
      cbUnchecked:
        if AllowGrayed then
          State := cbGrayed
        else
          State := cbChecked;
      cbChecked:
        State := cbUnchecked;
      cbGrayed:
        State := cbChecked;
    end;
    Self.State[Index] := State;
    ClickCheck;
  end;
end;

procedure TJvxCheckListBox.ChangeItemState(Index: Integer);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, Index);
end;

procedure TJvxCheckListBox.ClickCheck;
begin
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Self);
end;

function TJvxCheckListBox.GetItemData(Index: Integer): Longint;
var
  Item: TJvCheckListBoxItem;
begin
  Result := 0;
  if IsCheckObject(Index) then
  begin
    Item := TJvCheckListBoxItem(GetCheckObject(Index));
    if Item <> nil then
      Result := Item.FData;
  end;
end;

function TJvxCheckListBox.GetCheckObject(Index: Integer): TObject;
begin
  Result := FindCheckObject(Index);
  if Result = nil then
    Result := CreateCheckObject(Index);
end;

function TJvxCheckListBox.FindCheckObject(Index: Integer): TObject;
var
  ItemData: Longint;
begin
  Result := nil;
  ItemData := inherited GetItemData(Index);
  if ItemData = LB_ERR then
    ListIndexError(Index)
  else
  begin
    Result := TJvCheckListBoxItem(ItemData);
    if not (Result is TJvCheckListBoxItem) then
      Result := nil;
  end;
end;

function TJvxCheckListBox.CreateCheckObject(Index: Integer): TObject;
begin
  Result := TJvCheckListBoxItem.Create;
  inherited SetItemData(Index, Longint(Result));
end;

function TJvxCheckListBox.IsCheckObject(Index: Integer): Boolean;
begin
  Result := FindCheckObject(Index) <> nil;
end;

procedure TJvxCheckListBox.SetItemData(Index: Integer; AData: Longint);
var
  Item: TJvCheckListBoxItem;
  L: Longint;
begin
  Item := TJvCheckListBoxItem(GetCheckObject(Index));
  Item.FData := AData;
  if (FSaveStates <> nil) and (FSaveStates.Count > 0) then
  begin
    L := Longint(Pointer(FSaveStates[0]));
    Item.FState := TCheckBoxState(LongRec(L).Hi);
    Item.FEnabled := LongRec(L).Lo <> 0;
    FSaveStates.Delete(0);
  end;
end;

procedure TJvxCheckListBox.ResetContent;
var
  I: Integer;
begin
  for I := Items.Count - 1 downto 0 do
  begin
    if IsCheckObject(I) then
      GetCheckObject(I).Free;
    inherited SetItemData(I, 0);
  end;
  inherited ResetContent;
end;

procedure TJvxCheckListBox.DeleteString(Index: Integer);
begin
  if IsCheckObject(Index) then
    GetCheckObject(Index).Free;
  inherited SetItemData(Index, 0);
  inherited DeleteString(Index);
end;

//=== TJvCustomLabel =========================================================

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;
var
  RText, RShadow: TRect;
  Color: TColorRef;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(DC, ShadowColor);
  case ShadowPos of
    spLeftTop:
      OffsetRect(RShadow, -ShadowSize, -ShadowSize);
    spRightBottom:
      OffsetRect(RShadow, ShadowSize, ShadowSize);
    spLeftBottom:
      begin
        {OffsetRect(RText, ShadowSize, 0);}
        OffsetRect(RShadow, -ShadowSize, ShadowSize);
      end;
    spRightTop:
      begin
        {OffsetRect(RText, 0, ShadowSize);}
        OffsetRect(RShadow, ShadowSize, -ShadowSize);
      end;
  end;
  Result := DrawText(DC, Str, Count, RShadow, Format);
  if Result > 0 then
    Inc(Result, ShadowSize);
  SetTextColor(DC, Color);
  DrawText(DC, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

constructor TJvCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RendersSingleItem]);
  //FConsumerSvc.OnChanged := ConsumerServiceChanged;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
{$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle - [csOpaque];
{$ENDIF}
  FHotTrack := False;
  // (rom) needs better font handling
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FSpacing := 4;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 0;
  FShadowPos := spLeftTop;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvCustomLabel.Destroy;
begin
  FChangeLink.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  //FreeAndNil(FConsumerSvc);
  inherited;
end;

function TJvCustomLabel.GetLabelCaption: string;
{var
  ItemText: IJvDataItemText;}
begin
{  if ProviderActive then
  begin
    Provider.Enter;
    try
      if Supports((Provider as IJvDataConsumerItemSelect).GetItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := Caption;
    finally
      Provider.Leave;
    end;
  end
  else}
    Result := Caption;
end;

function TJvCustomLabel.GetDefaultFontColor: TColor;
begin
  Result := Font.Color;
end;

procedure TJvCustomLabel.DoDrawCaption(var Rect: TRect; Flags: Word);
var
  Text: string;
  PosShadow: TShadowPosition;
  SizeShadow: Byte;
  ColorShadow: TColor;
begin
  Text := GetLabelCaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + ' ';
  if not FShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  Canvas.Font.Color := GetDefaultFontColor;
  PosShadow := FShadowPos;
  SizeShadow := FShadowSize;
  ColorShadow := FShadowColor;
  if not Enabled then
  begin
    if (FShadowSize = 0) and NewStyleControls then
    begin
      PosShadow := spRightBottom;
      SizeShadow := 1;
    end;
    Canvas.Font.Color := clGrayText;
    ColorShadow := clBtnHighlight;
  end;
  if Images <> nil then
  begin
    Inc(Rect.Left, GetImageWidth + Spacing);
    if Flags and DT_CALCRECT = 0 then
      Images.Draw(Canvas, 0,0,ImageIndex);
  end;
  DrawShadowText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
end;

procedure TJvCustomLabel.DoDrawText(var Rect: TRect; Flags: Word);
{var
  Tmp: TSize;
  TmpItem: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  DrawState: TProviderDrawStates;}
begin
{  if ProviderActive then
  begin
    Provider.Enter;
    try
      if not Enabled then
        DrawState := [pdsDisabled]
      else
        DrawState := [];
      TmpItem := (Provider as IJvDataConsumerItemSelect).GetItem;
      if (TmpItem <> nil) and (Supports(TmpItem.GetItems, IJvDataItemsRenderer, ItemsRenderer) or
        Supports(TmpItem, IJvDataItemRenderer, ItemRenderer)) then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font := Font;
        if (Flags and DT_CALCRECT <> 0) then
        begin
          if ItemsRenderer <> nil then
            Tmp := ItemsRenderer.MeasureItem(Canvas, TmpItem)
          else
            Tmp := ItemRenderer.Measure(Canvas);
          Rect.Right := Tmp.cx;
          Rect.Bottom := Tmp.cy;
        end
        else
        begin
          if ItemsRenderer <> nil then
            ItemsRenderer.DrawItem(Canvas, Rect, TmpItem, DrawState)
          else
            ItemRenderer.Draw(Canvas, Rect, DrawState);
        end;
      end
      else
        DoDrawCaption(Rect, Flags);
    finally
      Provider.Leave;
    end;
  end
  else}
    DoDrawCaption(Rect, Flags);
end;

procedure TJvCustomLabel.DrawAngleText(Flags: Word);
var
  Text: array[0..4096] of Char;
  LogFont, NewLogFont: TLogFont;
  NewFont: HFont;
  MRect: TRect;
  TextX, TextY: Integer;
  Phi: Real;
  Angle10: Integer;
begin
  Angle10 := Angle * 10;
  StrLCopy(@Text, PChar(GetLabelCaption), SizeOf(Text) - 1);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  Canvas.Font := Font;
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    RaiseLastOSError;
  NewLogFont := LogFont;
  MRect := ClientRect;
  NewLogFont.lfEscapement := Angle10;
  NewLogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  NewFont := CreateFontIndirect(NewLogFont);
  {
    (p3) unnecessary
    OldFont := SelectObject(Canvas.Font.Handle, NewFont);
    DeleteObject(OldFont);
    ...this does the same thing:
  }
  Canvas.Font.Handle := NewFont;
  Phi := Angle10 * Pi / 1800;
  if not AutoSize then
  begin
    TextX := Trunc(0.5 * ClientWidth - 0.5 * Canvas.TextWidth(Text) * Cos(Phi) - 0.5 * Canvas.TextHeight(Text) *
      Sin(Phi));
    TextY := Trunc(0.5 * ClientHeight - 0.5 * Canvas.TextHeight(Text) * Cos(Phi) + 0.5 * Canvas.TextWidth(Text) *
      Sin(Phi));
  end
  else
  begin
    ClientWidth := 4 + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)) + Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    ClientHeight := 4 + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)) + Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    TextX := 2;
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextX := TextX + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)));
    if Angle10 > 1800 then
      TextX := TextX + Trunc(Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    TextY := 2;
    if Angle10 < 1800 then
      TextY := TextY + Trunc(Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextY := TextY + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)));
  end;
  if not Enabled then
  begin
    Canvas.Font.Color := clBtnHighlight;
    Canvas.TextOut(TextX+1, TextY+1, Text);
    Canvas.Font.Color := clBtnShadow;
    Canvas.TextOut(TextX, TextY, Text);
  end
  else
    Canvas.TextOut(TextX, TextY, Text);
end;

procedure TJvCustomLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  DrawStyle: Integer;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  with Canvas do
  begin
    if not Transparent then
     // only FillRect mode because Transparent is always True on JVCLThemesEnabled
      DrawThemedBackground(Self, Canvas, ClientRect, Self.Color);
    Brush.Style := bsClear;
    if Angle <> 0 then
      DrawAngleText(DT_EXPANDTABS or DT_WORDBREAK or Alignments[Alignment])
    else
    begin
      Rect := ClientRect;
      Inc(Rect.Left, FLeftMargin);
      Dec(Rect.Right, FRightMargin);
      InflateRect(Rect, -1, 0);
      DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
      { Calculate vertical layout }
      if FLayout <> tlTop then
      begin
        DoDrawText(Rect, DrawStyle or DT_CALCRECT);
        Rect.Left := ClientRect.Left + FLeftMargin;
        Rect.Right := ClientRect.Right - FRightMargin;
//        if Images <> nil then
//          Inc(Rect.Left,GetImageWidth + 4);
        if FLayout = tlBottom then
          OffsetRect(Rect, 0, Height - Rect.Bottom)
        else
          OffsetRect(Rect, 0, (Height - Rect.Bottom) div 2);
      end;
      DoDrawText(Rect, DrawStyle);
    end;
    if FShowFocus and Assigned(FFocusControl) and FFocused and
      not (csDesigning in ComponentState) then
    begin
      InflateRect(Rect, 1, 0);
      Brush.Color := Self.Color;
      DrawFocusRect(Rect);
    end;
  end;
end;

procedure TJvCustomLabel.Loaded;
begin
  inherited Loaded;
  //Provider.Loaded;
end;

procedure TJvCustomLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if AutoSize then
  begin
    Rect := ClientRect;
    Inc(Rect.Left, FLeftMargin);
    Dec(Rect.Right, FRightMargin);
    InflateRect(Rect, -1, 0);
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Dec(Rect.Left, FLeftMargin);
    Inc(Rect.Right, FRightMargin);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    InflateRect(Rect, 1, 0);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    Rect.Bottom := Max(Rect.Bottom, Rect.Top + GetImageHeight);
    if (AAlignment = taRightJustify) and (Images = nil) then
      Inc(X, Width - Rect.Right);
    if Images <> nil then
      Dec(Rect.Left,GetImageWidth + Spacing);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvCustomLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetAutoSize(Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
  inherited SetAutoSize(Value);
  {$ENDIF}
  FAutoSize := Value;
  AdjustBounds;
end;

procedure TJvCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Max(Value, 0);
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetRightMargin(Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Max(Value, 0);
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowSize(Value: Byte);
begin
  if Value <> FShadowSize then
  begin
    FShadowSize := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowPos(Value: TShadowPosition);
begin
  if Value <> FShadowPos then
  begin
    FShadowPos := Value;
    Invalidate;
  end;
end;

function TJvCustomLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TJvCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if FShowFocus then
    Invalidate;
end;

procedure TJvCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
  {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
     Value := True; // themes aware Label are always transparent transparent
  {$ENDIF}
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
  end;
end;

procedure TJvCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FFocusControl) then
      FocusControl := nil;
    if (AComponent = Images) then
      Images := nil;
  end;
end;

procedure TJvCustomLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
    FDragging := True;
end;

procedure TJvCustomLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging and (Button = mbLeft) then
    FDragging := False;
  UpdateTracking;
end;

procedure TJvCustomLabel.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomLabel.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomLabel.UpdateTracking;
var
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if FMouseInControl <> OldValue then
    if FMouseInControl then
      MouseEnter
    else
      MouseLeave;
end;

procedure TJvCustomLabel.CMFocusChanged(var Msg: TCMFocusChanged);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (Msg.Sender = FFocusControl);
  if FFocused <> Active then
  begin
    FFocused := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited;
end;

procedure TJvCustomLabel.CMTextChanged(var Msg: TMessage);
begin
  //NonProviderChange;
  Invalidate;
  AdjustBounds;
end;

procedure TJvCustomLabel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
  UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
end;

procedure TJvCustomLabel.CMDialogChar(var Msg: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Msg.CharCode, GetLabelCaption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Msg.Result := 1;
      end;
end;

procedure TJvCustomLabel.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Visible then
    UpdateTracking;
end;

procedure TJvCustomLabel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FMouseInControl and Enabled and IsForegroundTask then
  begin
    FHintSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if HotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FMouseInControl := True;
    MouseEnter;
  end;
end;

procedure TJvCustomLabel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FMouseInControl and Enabled and not FDragging then
  begin
    Application.HintColor := FHintSaved;
    if HotTrack then
      Font.Assign(FFontSave);
    FMouseInControl := False;
    MouseLeave;
  end;
end;

procedure TJvCustomLabel.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    {if Images <> nil then
      NonProviderChange;}
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    //NonProviderChange;
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(self);
      FImages.UnRegisterChanges(FChangeLink);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(self);
      FImages.RegisterChanges(FChangeLink);
    end;
    if AutoSize then AdjustBounds else Invalidate;
  end;
end;

function TJvCustomLabel.GetImageHeight: integer;
begin
  Result := 0;
  if {not ProviderActive and} (Images <> nil) then
    Result := Images.Height;
end;

{procedure TJvCustomLabel.SetConsumerService(Value: TJvDataConsumer);
begin
end;

function TJvCustomLabel.ProviderActive: Boolean;
begin
  Result := (Provider <> nil) and (Provider.ProviderIntf <> nil);
end;

procedure TJvCustomLabel.ConsumerServiceChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  if ProviderActive or (Reason = ccrProviderSelected) then
    AdjustBounds;
end;

procedure TJvCustomLabel.NonProviderChange;
begin
  if Provider <> nil then
    Provider.Provider := nil;
end;}

function TJvCustomLabel.GetImageWidth: integer;
begin
  Result := 0;
  if {not ProviderActive and }(Images <> nil) then
    Result := Images.Width;
end;

procedure TJvCustomLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomLabel.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCustomLabel.Click;
var
  HasBeenHandled: Boolean;
  {TmpItem: IJvDataItem;
  ItemHandler: IJvDataItemBasicAction;}
begin
  HasBeenHandled := False;
{  if ProviderActive then
  begin
    Provider.Enter;
    try
      TmpItem := (Provider as IJvDataConsumerItemSelect).GetItem;
      if (TmpItem <> nil) and Supports(TmpItem, IJvDataItemBasicAction, ItemHandler) then
        HasBeenHandled := ItemHandler.Execute(Self);
    finally
      Provider.Leave;
    end;
  end;}
  if not HasBeenHandled then
  begin
    inherited Click;
    if AutoOpenURL and (URL <> '') then
      ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TJvCustomLabel.SetAngle(const Value: TJvLabelRotateAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomLabel.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    if AutoSize then AdjustBounds else Invalidate;
  end;
end;

procedure TJvCustomLabel.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;


//=== TJvSecretPanel =========================================================

constructor TJvSecretPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollCnt := 0;
  FAlignment := taCenter;
  FActive := False;
  FTxtDivider := 1;
  FGlyphLayout := glGlyphLeft;
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvLowered;
  FTextStyle := bvNone;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FHiddenList := TList.Create;
  FTimer := TJvTimer.Create(Self);
  with FTimer do
  begin
    Enabled := False;
    OnTimer := TimerExpired;
    Interval := 30;
    SyncEvent := False;
    FAsyncDrawing := True;
  end;
end;

destructor TJvSecretPanel.Destroy;
begin
  SetActive(False);
  FGlyph.OnChange := nil;
  FGlyph.Free;
  TStringList(FLines).OnChange := nil;
  FLines.Free;
  FHiddenList.Free;
  inherited Destroy;
end;

procedure TJvSecretPanel.GlyphChanged(Sender: TObject);
begin
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.LinesChanged(Sender: TObject);
begin
  if Active then
  begin
    FScrollCnt := 0;
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if Active then
    UpdateMemoryImage;
end;

procedure TJvSecretPanel.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  if Active then
    UpdateMemoryImage;
end;

procedure TJvSecretPanel.WMSize(var Msg: TMessage);
begin
  inherited;
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.SetAsyncDrawing(Value: Boolean);
begin
  if FAsyncDrawing <> Value then
  begin
    FTimer.SyncEvent := not Value;
    FAsyncDrawing := Value;
  end;
end;

procedure TJvSecretPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if (AControl = nil) and Active then
    UpdateMemoryImage;
end;

function TJvSecretPanel.GetInflateWidth: Integer;
begin
  Result := BorderWidth;
  if BevelOuter <> bvNone then
    Inc(Result, BevelWidth);
  if BevelInner <> bvNone then
    Inc(Result, BevelWidth);
end;

procedure TJvSecretPanel.RecalcDrawRect;
const
  MinOffset = 3;
var
  InflateWidth: Integer;
  LastLine: Integer;
begin
  FTxtRect := GetClientRect;
  FPaintRect := FTxtRect;
  InflateWidth := GetInflateWidth;
  InflateRect(FPaintRect, -InflateWidth, -InflateWidth);
  Inc(InflateWidth, MinOffset);
  InflateRect(FTxtRect, -InflateWidth, -InflateWidth);
  with FGlyphOrigin do
  begin
    case FGlyphLayout of
      glGlyphLeft:
        begin
          X := FTxtRect.Left;
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then
            Y := FTxtRect.Top;
          if Glyph.Width > 0 then
          begin
            Inc(X, MinOffset);
            FTxtRect.Left := X + Glyph.Width + InflateWidth;
          end;
        end;
      glGlyphRight:
        begin
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then
            Y := FTxtRect.Top;
          X := FTxtRect.Right - Glyph.Width;
          if Glyph.Width > 0 then
          begin
            Dec(X, MinOffset);
            if X < FTxtRect.Left then
              X := FTxtRect.Left;
            FTxtRect.Right := X - InflateWidth;
          end;
        end;
      glGlyphTop:
        begin
          Y := FTxtRect.Top;
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then
            X := FTxtRect.Left;
          if Glyph.Height > 0 then
          begin
            Inc(Y, MinOffset);
            FTxtRect.Top := Y + Glyph.Height + (InflateWidth + MinOffset);
          end;
        end;
      glGlyphBottom:
        begin
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then
            X := FTxtRect.Left;
          Y := FTxtRect.Bottom - Glyph.Height;
          if Glyph.Height > 0 then
          begin
            Dec(Y, MinOffset);
            if Y < FTxtRect.Top then
              Y := FTxtRect.Top;
            FTxtRect.Bottom := Y - (InflateWidth + MinOffset);
          end;
        end;
    end;
  end;
  if FDirection = sdHorizontal then
  begin
    LastLine := FLines.Count - 1;
    while (LastLine >= 0) and (Trim(FLines[LastLine]) = '') do
      Dec(LastLine);
    InflateWidth := HeightOf(FTxtRect) -
      (LastLine + 1 - FFirstLine) * FTxtDivider;
    if InflateWidth > 0 then
      InflateRect(FTxtRect, 0, -InflateWidth div 2);
  end;
  with FTxtRect do
    if (Left >= Right) or (Top >= Bottom) then
      FTxtRect := Rect(0, 0, 0, 0);
end;

procedure TJvSecretPanel.PaintGlyph;
begin
  if not FGlyph.Empty then
  begin
    RecalcDrawRect;
    DrawBitmapTransparent(Canvas, FGlyphOrigin.X, FGlyphOrigin.Y,
      FGlyph, FGlyph.TransparentColor and not PaletteMask);
  end;
end;

procedure TJvSecretPanel.PaintText;
var
  STmp: array[0..255] of Char;
  R: TRect;
  I: Integer;
  Flags: Longint;
begin
  if (FLines.Count = 0) or IsRectEmpty(FTxtRect) or not HandleAllocated then
    Exit;
  FMemoryImage.Canvas.Lock;
  try
    with FMemoryImage.Canvas do
    begin
      I := SaveDC(Handle);
      try
        with FTxtRect do
          MoveWindowOrg(Handle, -Left, -Top);
        Brush.Color := Self.Color;
        PaintClient(FMemoryImage.Canvas, FPaintRect);
      finally
        RestoreDC(Handle, I);
        SetBkMode(Handle, Transparent);
      end;
    end;
    R := Bounds(0, 0, WidthOf(FTxtRect), HeightOf(FTxtRect));
    if FDirection = sdHorizontal then
    begin
      if IsRightToLeft then
      begin
        R.Right := R.Left + FScrollCnt;
        R.Left := R.Right - (FMaxScroll - WidthOf(FTxtRect));
      end
      else
      begin
        R.Left := R.Right - FScrollCnt;
        R.Right := R.Left + (FMaxScroll - WidthOf(FTxtRect));
      end;
    end
    else
    begin { sdVertical }
      R.Top := R.Bottom - FScrollCnt;
    end;
    R.Bottom := R.Top + FTxtDivider;
    Flags := DT_EXPANDTABS or Alignments[FAlignment] or DT_SINGLELINE or
      DT_NOCLIP or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    for I := FFirstLine to FLines.Count do
    begin
      if I = FLines.Count then
        StrCopy(STmp, ' ')
      else
        StrPLCopy(STmp, FLines[I], SizeOf(STmp) - 1);
      if R.Top >= HeightOf(FTxtRect) then
        Break
      else if R.Bottom > 0 then
      begin
        if FTextStyle <> bvNone then
        begin
          FMemoryImage.Canvas.Font.Color := clBtnHighlight;
          case FTextStyle of
            bvLowered:
              begin
                OffsetRect(R, 1, 1);
                DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
                OffsetRect(R, -1, -1);
              end;
            bvRaised:
              begin
                OffsetRect(R, -1, -1);
                DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
                OffsetRect(R, 1, 1);
              end;
          end;
          FMemoryImage.Canvas.Font.Color := Self.Font.Color;
          SetBkMode(FMemoryImage.Canvas.Handle, Transparent);
        end;
        DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
      end;
      OffsetRect(R, 0, FTxtDivider);
    end;
    Canvas.Lock;
    try
      BitBlt(Canvas.Handle, FTxtRect.Left, FTxtRect.Top, FMemoryImage.Width,
        FMemoryImage.Height, FMemoryImage.Canvas.Handle, 0, 0, SRCCOPY);
      ValidateRect(Handle, @FTxtRect);
    finally
      Canvas.Unlock;
    end;
  finally
    FMemoryImage.Canvas.Unlock;
  end;
end;

procedure TJvSecretPanel.PaintClient(Canvas: TCanvas; Rect: TRect);
begin
  if Assigned(FOnPaintClient) then
    FOnPaintClient(Self, Canvas, Rect)
  else
    Canvas.FillRect(Rect);
end;

procedure TJvSecretPanel.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  SaveIndex: Integer;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    Canvas.Brush.Color := Self.Color;
    PaintClient(Canvas, Rect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
  if Active then
  begin
    PaintGlyph;
    {PaintText;}
  end;
end;

procedure TJvSecretPanel.StartPlay;
begin
  if Assigned(FOnStartPlay) then
    FOnStartPlay(Self);
end;

procedure TJvSecretPanel.StopPlay;
begin
  if Assigned(FOnStopPlay) then
    FOnStopPlay(Self);
end;

procedure TJvSecretPanel.TimerExpired(Sender: TObject);
begin
  if FScrollCnt < FMaxScroll then
  begin
    Inc(FScrollCnt);
    if Assigned(FMemoryImage) then
      PaintText;
  end
  else if Cycled then
  begin
    FScrollCnt := 0;
    if Assigned(FMemoryImage) then
      PaintText;
  end
  else
  begin
    FTimer.Synchronize(Stop);
  end;
end;

procedure TJvSecretPanel.UpdateMemoryImage;
var
  Metrics: TTextMetric;
  I: Integer;
begin
  if FMemoryImage = nil then
    FMemoryImage := TBitmap.Create;
  FMemoryImage.Canvas.Lock;
  try
    FFirstLine := 0;
    while (FFirstLine < FLines.Count) and (Trim(FLines[FFirstLine]) = '') do
      Inc(FFirstLine);
    Canvas.Font := Self.Font;
    GetTextMetrics(Canvas.Handle, Metrics);
    FTxtDivider := Metrics.tmHeight + Metrics.tmExternalLeading;
    if FTextStyle <> bvNone then
      Inc(FTxtDivider);
    RecalcDrawRect;
    if FDirection = sdHorizontal then
    begin
      FMaxScroll := 0;
      for I := FFirstLine to FLines.Count - 1 do
        FMaxScroll := Max(FMaxScroll, Canvas.TextWidth(FLines[I]));
      Inc(FMaxScroll, WidthOf(FTxtRect));
    end
    else
    begin { sdVertical }
      FMaxScroll := ((FLines.Count - FFirstLine) * FTxtDivider) +
        HeightOf(FTxtRect);
    end;
    FMemoryImage.Width := WidthOf(FTxtRect);
    FMemoryImage.Height := HeightOf(FTxtRect);
    with FMemoryImage.Canvas do
    begin
      Font := Self.Font;
      Brush.Color := Self.Color;
      SetBkMode(Handle, Transparent);
    end;
  finally
    FMemoryImage.Canvas.Unlock;
  end;
end;

function TJvSecretPanel.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvSecretPanel.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TJvSecretPanel.Play;
begin
  SetActive(True);
end;

procedure TJvSecretPanel.Stop;
begin
  SetActive(False);
end;

procedure TJvSecretPanel.SetActive(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      try
        FTimer.Enabled := True;
        StartPlay;
      except
        FActive := False;
        FTimer.Enabled := False;
        raise;
      end;
    end
    else
    begin
      FMemoryImage.Canvas.Lock;
      { ensure that canvas is locked before timer is disabled }
      FTimer.Enabled := False;
      FScrollCnt := 0;
      FMemoryImage.Free;
      FMemoryImage := nil;
      StopPlay;
      if (csDesigning in ComponentState) and
        not (csDestroying in ComponentState) then
        ValidParentForm(Self).Designer.Modified;
    end;
    if not (csDestroying in ComponentState) then
      for I := 0 to Pred(ControlCount) do
      begin
        if FActive then
        begin
          if Controls[I].Visible then
            FHiddenList.Add(Controls[I]);
          if not (csDesigning in ComponentState) then
            Controls[I].Visible := False
        end
        else if FHiddenList.IndexOf(Controls[I]) >= 0 then
        begin
          Controls[I].Visible := True;
          Controls[I].Invalidate;
          if csDesigning in ComponentState then
            Controls[I].Update;
        end;
      end;
    if not FActive then
      FHiddenList.Clear;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Active then
      Invalidate;
  end;
end;

procedure TJvSecretPanel.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TJvSecretPanel.SetDirection(Value: TScrollDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TJvSecretPanel.SetTextStyle(Value: TPanelBevel);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TJvSecretPanel.SetGlyphLayout(Value: TGlyphLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TJvSecretPanel.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

//=== TJvGlyphList ===========================================================

type
  TJvGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): Integer;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

constructor TJvGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TJvGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TJvGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

function TJvGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

function TJvGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TJvGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

//=== TJvGlyphCache ==========================================================

type
  TJvGlyphCache = class(TObject)
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TJvGlyphList;
    procedure ReturnList(List: TJvGlyphList);
    function Empty: Boolean;
  end;

constructor TJvGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TJvGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TJvGlyphCache.GetList(AWidth, AHeight: Integer): TJvGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then
        Exit;
  end;
  Result := TJvGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TJvGlyphCache.ReturnList(List: TJvGlyphList);
begin
  if List = nil then
    Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TJvGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

//=== TJvxButtonGlyph =========================================================

// (rom) changed to var
var
  GlyphCache: TJvGlyphCache = nil;

procedure TJvxButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect; Flags: Word;
  Images: TImageList; ImageIndex: Integer);
var
  TextPos: TPoint;
  MaxSize, ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  CString: array[0..255] of Char;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);
  if Assigned(Images) and (Images.Width > 0) and (ImageIndex >= 0) and
    (ImageIndex < Images.Count) then
    GlyphSize := Point(Images.Width, Images.Height)
  else if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    MaxSize.X := ClientSize.X - GlyphSize.X;
    if Margin <> -1 then
      Dec(MaxSize.X, Margin);
    if Spacing <> -1 then
      Dec(MaxSize.X, Spacing);
    if PopupMark then
      Dec(MaxSize.X, 9);
    MaxSize.Y := ClientSize.Y;
  end
  else { blGlyphTop, blGlyphBottom }
  begin
    MaxSize.X := ClientSize.X;
    MaxSize.Y := ClientSize.Y - GlyphSize.Y;
    if Margin <> -1 then
      Dec(MaxSize.Y, Margin);
    if Spacing <> -1 then
      Dec(MaxSize.Y, Spacing);
  end;
  MaxSize.X := Max(0, MaxSize.X);
  MaxSize.Y := Max(0, MaxSize.Y);
  MinimizeCaption(Canvas, Caption, CString, SizeOf(CString) - 1, MaxSize.X);
  Caption := StrPas(CString);
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, MaxSize.X, 0);
    DrawText(Canvas.Handle, CString, -1, TextBounds, DT_CALCRECT or DT_CENTER or
      DT_VCENTER or WordWraps[FWordWrap] or Flags);
  end
  else
    TextBounds := Rect(0, 0, 0, 0);
  TextBounds.Bottom := Max(TextBounds.Top, TextBounds.Top +
    Min(MaxSize.Y, HeightOf(TextBounds)));
  TextBounds.Right := Max(TextBounds.Left, TextBounds.Left +
    Min(MaxSize.X, WidthOf(TextBounds)));
  TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
    TextBounds.Top);
  if PopupMark then
    if ((GlyphSize.X = 0) or (GlyphSize.Y = 0)) or (Layout = blGlyphLeft) then
      Inc(TextSize.X, 9)
    else if GlyphSize.X > 0 then
      Inc(GlyphSize.X, 6);
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y div 2) - (GlyphSize.Y div 2);
    TextPos.Y := (ClientSize.Y div 2) - (TextSize.Y div 2);
  end
  else
  begin
    GlyphPos.X := (ClientSize.X div 2) - (GlyphSize.X div 2);
    TextPos.X := (ClientSize.X div 2) - (TextSize.X div 2);
  end;
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X div 2) - (TotalSize.X div 2)
      else
        Margin := (ClientSize.Y div 2) - (TotalSize.Y div 2);
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X div 2) - (TextSize.X div 2)
      else
        Spacing := (TotalSize.Y div 2) - (TextSize.Y div 2);
    end;
  end;
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

constructor TJvxButtonGlyph.Create;
var
  I: TJvButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clFuchsia;
  FAlignment := taCenter;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then
    GlyphCache := TJvGlyphCache.Create;
end;

function TJvxButtonGlyph.CreateButtonGlyph(State: TJvButtonState): Integer;
var
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight, X, Y: Integer;
  IRect, ORect: TRect;
  I: TJvButtonState;
begin
  if (State = rbsDown) and (NumGlyphs < 3) then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then
    Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    I := State;
    if Ord(I) >= NumGlyphs then
      I := rbsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end;
      rbsDisabled:
        if NumGlyphs > 1 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end
        else
        begin
          if FGrayNewStyle then
          begin
            MonoBmp := CreateDisabledBitmap_NewStyle(FOriginal, FTransparentColor);
            try
              FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(MonoBmp,
                FTransparentColor);
            finally
              MonoBmp.Free;
            end;
          end
          else
          begin
            MonoBmp := CreateDisabledBitmap(FOriginal, clBlack);
            try
              FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(MonoBmp,
                ColorToRGB(clBtnFace));
            finally
              MonoBmp.Free;
            end;
          end;
        end;
      rbsInactive:
        if NumGlyphs > 4 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end
        else
        begin
          with TmpImage do
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(FOriginal.Canvas.Pixels[X, Y]);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

function TJvxButtonGlyph.CreateImageGlyph(State: TJvButtonState;
  Images: TImageList; Index: Integer): Integer;
var
  TmpImage, Mask: TBitmap;
  IWidth, IHeight, X, Y: Integer;
begin
  if State = rbsDown then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (Images.Width = 0) or (Images.Height = 0) or
    (Images.Count = 0) then
    Exit;
  IWidth := Images.Width;
  IHeight := Images.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          with TmpImage.Canvas do
          begin
            FillRect(Rect(0, 0, IWidth, IHeight));
            ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_NORMAL);
          end;
          Mask := TBitmap.Create;
          try
            with Mask do
            begin
              Monochrome := True;
              Height := IHeight;
              Width := IWidth;
            end;
            with Mask.Canvas do
            begin
              FillRect(Rect(0, 0, IWidth, IHeight));
              ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
            end;
            FIndexs[State] := TJvGlyphList(FGlyphList).Add(TmpImage, Mask);
          finally
            Mask.Free;
          end;
        end;
      rbsDisabled:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageListDrawDisabled(Images, TmpImage.Canvas, 0, 0, Index,
            clBtnHighlight, clBtnShadow, True);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
      rbsInactive:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageList_Draw(Images.Handle, Index, TmpImage.Canvas.Handle, 0, 0,
            ILD_NORMAL);
          with TmpImage do
          begin
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(Canvas.Pixels[X, Y]);
          end;
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
end;

destructor TJvxButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

function TJvxButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; State: TJvButtonState; Flags: Word): TRect;
begin
  Result := DrawEx(Canvas, Client, Caption, Layout, Margin, Spacing,
    PopupMark, nil, -1, State, Flags);
end;

function TJvxButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TJvButtonState): TPoint;
var
  Index: Integer;
begin
  Result := Point(0, 0);
  if (FOriginal = nil) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then
    Exit;
  Index := CreateButtonGlyph(State);
  if Index >= 0 then
  begin
    ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
    Result := Point(FGlyphList.Width, FGlyphList.Height);
  end;
end;

function TJvxButtonGlyph.DrawButtonImage(Canvas: TCanvas; X, Y: Integer;
  Images: TImageList; ImageIndex: Integer; State: TJvButtonState): TPoint;
var
  Index: Integer;
begin
  Result := Point(0, 0);
  if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
    Exit;
  if State = rbsDisabled then
  begin
    ImageListDrawDisabled(Images, Canvas, X, Y, ImageIndex, clBtnHighlight,
      clBtnShadow, True);
  end
  else if State = rbsInactive then
  begin
    Index := CreateImageGlyph(State, Images, ImageIndex);
    if Index >= 0 then
      ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
  end
  else
    ImageList_Draw(Images.Handle, ImageIndex, Canvas.Handle, X, Y, ILD_NORMAL);
  Result := Point(Images.Width, Images.Height);
end;

procedure TJvxButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TJvButtonState; Flags: Word);
var
  CString: array[0..255] of Char;
begin
  Canvas.Brush.Style := bsClear;
  StrPLCopy(CString, Caption, SizeOf(CString) - 1);
  Flags := DT_VCENTER or WordWraps[FWordWrap] or Flags;
  if State = rbsDisabled then
  begin
    with Canvas do
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, CString, Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, CString, Length(Caption), TextBounds, Flags);
    end;
  end
  else
    DrawText(Canvas.Handle, CString, -1, TextBounds, Flags);
end;

function TJvxButtonGlyph.DrawEx(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; Images: TImageList; ImageIndex: Integer;
  State: TJvButtonState; Flags: Word): TRect;
var
  UseImages: Boolean;
  GlyphPos, PopupPos: TPoint;
  TextBounds: TRect;
  CaptionText: string;
begin
  CaptionText := Caption;
  CalcButtonLayout(Canvas, Client, CaptionText, Layout, Margin, Spacing,
    PopupMark, GlyphPos, TextBounds, Flags, Images,
    ImageIndex);
  UseImages := False;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) and
    (Images.Width > 0) then
  begin
    UseImages := True;
    PopupPos := DrawButtonImage(Canvas, GlyphPos.X, GlyphPos.Y, Images,
      ImageIndex, State);
  end
  else
    PopupPos := DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State);
  DrawButtonText(Canvas, CaptionText, TextBounds, State, Flags);
  if PopupMark then
    if (Layout <> blGlyphLeft) and (((FOriginal <> nil) and
      (FOriginal.Width > 0)) or UseImages) then
    begin
      PopupPos.X := GlyphPos.X + PopupPos.X + 1;
      PopupPos.Y := GlyphPos.Y + PopupPos.Y div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end
    else
    begin
      if CaptionText <> '' then
        PopupPos.X := TextBounds.Right + 3
      else
        PopupPos.X := (Client.Left + Client.Right - 7) div 2;
      PopupPos.Y := TextBounds.Top + HeightOf(TextBounds) div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end;
  Result := TextBounds;
end;

procedure TJvxButtonGlyph.DrawPopupMark(Canvas: TCanvas; X, Y: Integer;
  State: TJvButtonState);
var
  AColor: TColor;

  procedure DrawMark;
  var
    I: Integer;
  begin
    with Canvas do
    begin
      for I := 0 to 6 do
      begin
        Pixels[X + I, Y - 1] := AColor;
        if (I > 0) and (I < 6) then
        begin
          Pixels[X + I, Y] := AColor;
          if (I > 1) and (I < 5) then
            Pixels[X + I, Y + 1] := AColor;
        end;
      end;
      Pixels[X + 3, Y + 2] := AColor;
    end;
  end;

begin
  if State = rbsDisabled then
  begin
    AColor := clBtnHighlight;
    Inc(X, 1);
    Inc(Y, 1);
    DrawMark;
    Dec(X, 1);
    Dec(Y, 1);
    AColor := clBtnShadow;
  end
  else
    AColor := clBtnText;
  DrawMark;
end;

procedure TJvxButtonGlyph.GlyphChanged(Sender: TObject);
var
  Glyphs: Integer;
begin
  if Sender = FOriginal then
  begin
    Invalidate;
    if (FOriginal <> nil) and (FOriginal.Height > 0) then
    begin
      FTransparentColor := FOriginal.TransparentColor and not PaletteMask;
      if FOriginal.Width mod FOriginal.Height = 0 then
      begin
        Glyphs := FOriginal.Width div FOriginal.Height;
        if Glyphs > (Ord(High(TJvButtonState)) + 1) then
          Glyphs := 1;
        SetNumGlyphs(Glyphs);
      end;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TJvxButtonGlyph.Invalidate;
var
  I: TJvButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if Assigned(FGlyphList) then
      if FIndexs[I] <> -1 then
        TJvGlyphList(FGlyphList).Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(TJvGlyphList(FGlyphList));
  FGlyphList := nil;
end;

function TJvxButtonGlyph.MapColor(Color: TColor): TColor;
var
  Index: Byte;
begin
  if (Color = FTransparentColor) or (ColorToRGB(Color) = ColorToRGB(clBtnFace)) then
    Result := Color
  else
  begin
    Color := ColorToRGB(Color);
    Index := Byte(Longint(Word(GetRValue(Color)) * 77 +
      Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) shr 8);
    Result := RGB(Index, Index, Index);
  end;
end;

procedure TJvxButtonGlyph.MinimizeCaption(Canvas: TCanvas; const Caption: string;
  Buffer: PChar; MaxLen, Width: Integer);
var
  I: Integer;
  Lines: TStrings;
begin
  StrPLCopy(Buffer, Caption, MaxLen);
  if FWordWrap then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := Caption;
    for I := 0 to Lines.Count - 1 do
      Lines[I] := MinimizeName(Lines[I], Canvas, Width);
    StrPLCopy(Buffer, TrimRight(Lines.Text), MaxLen);
  finally
    Lines.Free;
  end;
end;

procedure TJvxButtonGlyph.SetGlyph(Value: TBitmap);
begin
  Invalidate;
  FOriginal.Assign(Value);
end;

procedure TJvxButtonGlyph.SetGrayNewStyle(const Value: Boolean);
begin
  if Value <> FGrayNewStyle then
  begin
    Invalidate;
    FGrayNewStyle := Value;
  end;
end;

procedure TJvxButtonGlyph.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

//=== TJvButtonImage =========================================================

// (rom) changed to var
var
  ButtonCount: Integer = 0;

  { DrawButtonFrame - returns the remaining usable area inside the Client rect }

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean; Style: TButtonStyle): TRect;
var
  NewStyle: Boolean;
begin
  Result := Client;
  NewStyle := (Style = bsNew) or (NewStyleControls and (Style = bsAutoDetect));
  if IsDown then
  begin
    if NewStyle then
    begin
      //Polaris
      //Frame3D(Canvas, Result,clBtnShadow{ clWindowFrame}, clBtnHighlight, 1);
      //if not IsFlat then
      //  Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      if not IsFlat then
      begin
        Frame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1);
        Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      end
      else
        Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1);
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1)
          // Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.PolyLine([Point(Result.Left, Result.Bottom - 1),
          Point(Result.Left, Result.Top), Point(Result.Right, Result.Top)]);
      end;
    end;
  end
  else
  begin
    if NewStyle then
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1)
      else
      begin
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnFace, clBtnShadow, 1);
      end;
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1);
      end;
    end;
  end;
  InflateRect(Result, -1, -1);
end;

constructor TJvButtonImage.Create;
begin
  FGlyph := TJvxButtonGlyph.Create;
  NumGlyphs := 1;
  FButtonSize := Point(24, 23);
end;

destructor TJvButtonImage.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvButtonImage.Invalidate;
begin
  TJvxButtonGlyph(FGlyph).Invalidate;
end;

function TJvButtonImage.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := TJvxButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TJvButtonImage.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  TJvxButtonGlyph(FGlyph).NumGlyphs := Value;
end;

function TJvButtonImage.GetWordWrap: Boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).WordWrap;
end;

procedure TJvButtonImage.SetWordWrap(Value: Boolean);
begin
  TJvxButtonGlyph(FGlyph).WordWrap := Value;
end;

function TJvButtonImage.GetGlyph: TBitmap;
begin
  Result := TJvxButtonGlyph(FGlyph).Glyph;
end;

procedure TJvButtonImage.SetGlyph(Value: TBitmap);
begin
  TJvxButtonGlyph(FGlyph).Glyph := Value;
end;

function TJvButtonImage.GetAlignment: TAlignment;
begin
  Result := TJvxButtonGlyph(FGlyph).Alignment;
end;

procedure TJvButtonImage.SetAlignment(Value: TAlignment);
begin
  TJvxButtonGlyph(FGlyph).Alignment := Value;
end;

procedure TJvButtonImage.Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Flags: Word);
begin
  DrawEx(Canvas, X, Y, Margin, Spacing, Layout, AFont, nil, -1, Flags);
end;

procedure TJvButtonImage.DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Images: TImageList; ImageIndex: Integer;
  Flags: Word);
var
  Target: TRect;
  SaveColor: Integer;
  SaveFont: TFont;
begin
  SaveColor := Canvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(Canvas.Font);
  try
    Target := Bounds(X, Y, FButtonSize.X, FButtonSize.Y);
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Target);
    Frame3D(Canvas, Target, clBtnShadow, clWindowFrame, 1);
    Frame3D(Canvas, Target, clBtnHighlight, clBtnShadow, 1);
    if AFont <> nil then
      Canvas.Font := AFont;
    TJvxButtonGlyph(FGlyph).DrawEx(Canvas, Target, Caption, Layout, Margin,
      Spacing, False, Images, ImageIndex, rbsUp, Flags);
  finally
    Canvas.Font.Assign(SaveFont);
    SaveFont.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

//=== TJvSpeedButton ========================================================

procedure TJvSpeedButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
      TransparentColor := clFuchsia;
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (not CheckDefaults or (Self.Down = False)) and (FGroupIndex <> 0) then
        Self.Down := Checked;
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure TJvSpeedButton.ButtonClick;
var
  FirstTickCount, Now: Longint;
begin
  if FMenuTracking or (not Enabled) or (Assigned(FDropDownMenu) and
    DropDownMenu.AutoPopup) then
    Exit;
  if not FDown then
  begin
    FState := rbsDown;
    Repaint;
  end;
  try
    FirstTickCount := GetTickCount;
    repeat
      Now := GetTickCount;
    until (Now - FirstTickCount >= 20) or (Now < FirstTickCount);
    if FGroupIndex = 0 then
      Click;
  finally
    FState := rbsUp;
    if FGroupIndex = 0 then
      Repaint
    else
    begin
      SetDown(not FDown);
      Click;
    end;
  end;
end;

function TJvSpeedButton.CheckBtnMenuDropDown: Boolean;
begin
  Result := CheckMenuDropDown(PointToSmallPoint(GetDropDownMenuPos), True);
end;

function TJvSpeedButton.CheckMenuDropDown(const Pos: TSmallPoint;
  Manual: Boolean): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FDropDownMenu) and (DropDownMenu.AutoPopup or Manual) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.SendCancelMode(nil);
    DropDownMenu.PopupComponent := Self;
    with ClientToScreen(SmallPointToPoint(Pos)) do
      DropDownMenu.Popup(X, Y);
    Result := True;
  end;
end;

procedure TJvSpeedButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TJvSpeedButton.CMButtonPressed(var Msg: TMessage);
var
  Sender: TControl;
begin
  if (Msg.WParam = FGroupIndex) and Parent.HandleAllocated then
  begin
    Sender := TControl(Msg.LParam);
    if (Sender <> nil) and (Sender is TJvSpeedButton) then
      if Sender <> Self then
      begin
        if TJvSpeedButton(Sender).Down and FDown then
        begin
          FDown := False;
          FState := rbsUp;
          Repaint;
        end;
        FAllowAllUp := TJvSpeedButton(Sender).AllowAllUp;
      end;
  end;
end;

procedure TJvSpeedButton.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvSpeedButton.CMEnabledChanged(var Msg: TMessage);
var
  State: TJvButtonState;
begin
  inherited;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TJvxButtonGlyph(FGlyph).CreateButtonGlyph(State);
  UpdateTracking;
  Repaint;
end;

procedure TJvSpeedButton.CMFontChanged(var Msg: TMessage);
begin
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
  Invalidate;
end;

procedure TJvSpeedButton.CMMouseEnter(var Msg: TMessage);
{$IFDEF JVCLThemesEnabled}
var
  NeedRepaint: Boolean;
{$ENDIF}
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if not FHotGlyph.Empty then
    begin
      FOldGlyph.Assign(Glyph);
      Glyph.Assign(FHotGlyph);
    end;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := True;
  end;

  {$IFDEF JVCLThemesEnabled}
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and not FMouseInControl and Enabled and
    (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint or ThemeServices.ThemesEnabled) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    if Enabled then
      Repaint;
  end;
  {$ELSE}
  if not FMouseInControl and Enabled and IsForegroundTask then
  begin
    FMouseInControl := True;
    if FFlat then
      Repaint;
    MouseEnter;
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvSpeedButton.CMMouseLeave(var Msg: TMessage);
{$IFDEF JVCLThemesEnabled}
var
  NeedRepaint: Boolean;
{$ENDIF}
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if not FOldGlyph.Empty then
      Glyph.Assign(FOldGlyph);
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  {$IFDEF JVCLThemesEnabled}
  NeedRepaint := FFlat and FMouseInControl and Enabled and not FDragging;
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint or ThemeServices.ThemesEnabled then
  begin
    FMouseInControl := False;
    if Enabled then
      Repaint;
  end;
  {$ELSE}
  if FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    if FFlat then
      Invalidate;
    MouseLeave;
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvSpeedButton.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvSpeedButton.CMSysColorChange(var Msg: TMessage);
begin
  TJvxButtonGlyph(FGlyph).Invalidate;
  Invalidate;
end;

procedure TJvSpeedButton.CMTextChanged(var Msg: TMessage);
begin
  Invalidate;
end;

procedure TJvSpeedButton.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Visible then
    UpdateTracking;
end;

constructor TJvSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := False;
  FHotGlyph := TBitmap.Create;
  FOldGlyph := TBitmap.Create;
  FFlatStandard := False;
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  FInactiveGrayed := True;
  FDrawImage := TBitmap.Create;
  FGlyph := TJvxButtonGlyph.Create;
  TJvxButtonGlyph(FGlyph).OnChange := GlyphChanged;
  TJvxButtonGlyph(FGlyph).GrayNewStyle := True;
  ParentFont := True;
  ParentShowHint := False;
  ShowHint := True;
  FSpacing := 1;
  FMargin := -1;
  FInitRepeatPause := 500;
  FRepeatPause := 100;
  FStyle := bsAutoDetect;
  FLayout := blGlyphTop;
  FMarkDropDown := True;
  Inc(ButtonCount);
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvSpeedButton.Destroy;
begin
  TJvxButtonGlyph(FGlyph).Free;
  Dec(ButtonCount);
  FDrawImage.Free;
  FDrawImage := nil;
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  FHotGlyph.Free;
  FOldGlyph.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TJvSpeedButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  if FDragging and (Button = mbLeft) then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      FState := rbsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [rbsExclusive, rbsDown]) then
        Repaint
      else
        Invalidate;
    end
    else if DoClick then
    begin
      SetDown(not FDown);
      if FDown then
        Repaint;
    end
    else
    begin
      if FDown then
        FState := rbsExclusive;
      Repaint;
    end;
    if DoClick and not FMenuTracking then
      Click;
  end;
  UpdateTracking;
end;

function TJvSpeedButton.GetAlignment: TAlignment;
begin
  Result := TJvxButtonGlyph(FGlyph).Alignment;
end;

function TJvSpeedButton.GetDropDownMenuPos: TPoint;
begin
  if Assigned(FDropDownMenu) then
  begin
    if MenuPosition = dmpBottom then
    begin
      case FDropDownMenu.Alignment of
        paLeft:
          Result := Point(-1, Height);
        paRight:
          Result := Point(Width + 1, Height);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end
    else { dmpRight }
    begin
      case FDropDownMenu.Alignment of
        paLeft:
          Result := Point(Width, -1);
        paRight:
          Result := Point(-1, -1);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end;
  end
  else
    Result := Point(0, 0);
end;

function TJvSpeedButton.GetGlyph: TBitmap;
begin
  Result := TJvxButtonGlyph(FGlyph).Glyph;
end;

function TJvSpeedButton.GetGrayNewStyle: Boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).GrayNewStyle;
end;

function TJvSpeedButton.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := TJvxButtonGlyph(FGlyph).NumGlyphs;
end;

function TJvSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TJvSpeedButton.GetWordWrap: Boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).WordWrap;
end;

procedure TJvSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvSpeedButton.Loaded;
var
  State: TJvButtonState;
begin
  inherited Loaded;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TJvxButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TJvSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Msg: TMsg;
begin
  if FMenuTracking then
    Exit;
  inherited MouseDown(Button, Shift, X, Y);
  if (not FMouseInControl) and Enabled then
  begin
    FMouseInControl := True;
    Repaint;
  end;
  if (Button = mbLeft) and Enabled {and not (ssDouble in Shift)} then
  begin
    if not FDown then
    begin
      FState := rbsDown;
      Repaint;
    end;
    FDragging := True;
    FMenuTracking := True;
    try
      P := GetDropDownMenuPos;
      if CheckMenuDropDown(PointToSmallPoint(P), False) then
        DoMouseUp(Button, Shift, X, Y);
      if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
      begin
        if (Msg.Message = WM_LBUTTONDOWN) or (Msg.Message = WM_LBUTTONDBLCLK) then
        begin
          P := ScreenToClient(Msg.Pt);
          if (P.X >= 0) and (P.X < ClientWidth) and (P.Y >= 0)
            and (P.Y <= ClientHeight) then
            KillMessage(HWND_DESKTOP, Msg.Message);
        end;
      end;
    finally
      FMenuTracking := False;
    end;
    if FAllowTimer then
    begin
      if FRepeatTimer = nil then
        FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.Interval := InitPause;
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Enabled := True;
    end;
  end;
end;

procedure TJvSpeedButton.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvSpeedButton.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TJvButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then
      NewState := rbsUp
    else
      NewState := rbsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := rbsExclusive
      else
        NewState := rbsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;

procedure TJvSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
end;

procedure TJvSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DropDownMenu) and (Operation = opRemove) then
    DropDownMenu := nil;
end;

procedure TJvSpeedButton.Paint;
var
  PaintRect: TRect;
  AState: TJvButtonState;
  {$IFDEF JVCLThemesEnabled}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if not Enabled {and not (csDesigning in ComponentState)} then
  begin
    FState := rbsDisabled;
    FDragging := False;
  end
  else if FState = rbsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := rbsExclusive
    else
      FState := rbsUp;
  AState := FState;

  if FFlat and not FMouseInControl and not (csDesigning in ComponentState) then
    AState := rbsInactive;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    PerformEraseBackground(Self, Canvas.Handle);

    if not Enabled then
      Button := tbPushButtonDisabled
    else if AState in [rbsDown, rbsExclusive] then
      Button := tbPushButtonPressed
    else if FMouseInControl then
      Button := tbPushButtonHot
    else
      Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if FFlat then
    begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      InflateRect(PaintRect, 1, 1);
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(ToolButton);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end;

    if Button = tbPushButtonPressed then
    begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
      if ToolButton <> ttbToolbarDontCare then
        Canvas.Font.Color := clHighlightText;
      OffsetRect(PaintRect, 1, 0);
    end;
    PaintGlyph({FDrawImage.} Canvas, PaintRect, AState, FMarkDropDown and
      Assigned(FDropDownMenu));
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    PaintRect := Rect(0, 0, Width, Height);
    FDrawImage.Width := Self.Width;
    FDrawImage.Height := Self.Height;
    with FDrawImage.Canvas do
    begin
      Font := Self.Font;
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(PaintRect);
      if FTransparent then
        CopyParentImage(Self, FDrawImage.Canvas);
      if (AState <> rbsInactive) or (FState = rbsExclusive) then
        PaintRect := DrawButtonFrame(FDrawImage.Canvas, PaintRect,
          FState in [rbsDown, rbsExclusive], FFlat, FStyle)
      else if FFlat then
        InflateRect(PaintRect, -2, -2);
    end;
    if (FState = rbsExclusive) and not Transparent and
      (not FFlat or (AState = rbsInactive)) then
    begin
      FDrawImage.Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      InflateRect(PaintRect, 1, 1);
      FDrawImage.Canvas.FillRect(PaintRect);
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [rbsDown, rbsExclusive] then
      OffsetRect(PaintRect, 1, 1);
    if (FState = rbsDisabled) or not FInactiveGrayed then
      AState := FState;
    PaintGlyph(FDrawImage.Canvas, PaintRect, AState, FMarkDropDown and
      Assigned(FDropDownMenu));
    Canvas.Draw(0, 0, FDrawImage);
  end;
end;

procedure TJvSpeedButton.PaintGlyph(Canvas: TCanvas; ARect: TRect;
  AState: TJvButtonState; DrawMark: Boolean);
begin
  if FFlatStandard and (AState = rbsInactive) then
    AState := rbsExclusive; // Polaris
  TJvxButtonGlyph(FGlyph).Draw(Canvas, ARect, Caption, FLayout,
    FMargin, FSpacing, DrawMark, AState, DrawTextBiDiModeFlags(Alignments[Alignment]));
end;

procedure TJvSpeedButton.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    TJvxButtonGlyph(FGlyph).Alignment := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJvSpeedButton.SetAllowTimer(Value: Boolean);
begin
  FAllowTimer := Value;
  if not FAllowTimer and (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Enabled := False;
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TJvSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then
    Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then
      Exit;
    FDown := Value;
    if Value then
    begin
      if FState = rbsUp then
        Invalidate;
      FState := rbsExclusive;
    end
    else
    begin
      FState := rbsUp;
    end;
    Repaint;
    if Value then
      UpdateExclusive;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetDropDownMenu(Value: TPopupMenu);
begin
  FDropDownMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if FMarkDropDown then
    Invalidate;
end;

procedure TJvSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetFlatStandard(Value: Boolean);
begin
  { Polaris }
  if FFlatStandard <> Value then
  begin
    FFlatStandard := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetGlyph(Value: TBitmap);
begin
  TJvxButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

procedure TJvSpeedButton.SetGrayNewStyle(const Value: Boolean);
begin
  if GrayNewStyle <> Value then
  begin
    TJvxButtonGlyph(FGlyph).GrayNewStyle := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvSpeedButton.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvSpeedButton.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font,FHotTrackFontOptions);
  end;
end;

procedure TJvSpeedButton.SetInactiveGrayed(Value: Boolean);
begin
  if Value <> FInactiveGrayed then
  begin
    FInactiveGrayed := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetMarkDropDown(Value: Boolean);
begin
  if Value <> FMarkDropDown then
  begin
    FMarkDropDown := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > Ord(High(TJvButtonState)) + 1 then
    Value := Ord(High(TJvButtonState)) + 1;
  if Value <> TJvxButtonGlyph(FGlyph).NumGlyphs then
  begin
    TJvxButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetStyle(Value: TButtonStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetWordWrap(Value: Boolean);
begin
  if Value <> WordWrap then
  begin
    TJvxButtonGlyph(FGlyph).WordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatInterval;
  if (FState = rbsDown) and MouseCapture then
  try
    Click;
  except
    FRepeatTimer.Enabled := False;
    raise;
  end;
end;

procedure TJvSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_JVBUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvSpeedButton.UpdateTracking;
var
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if FMouseInControl <> OldValue then
    if FMouseInControl then
    begin
      if Flat then
        Repaint;
      MouseEnter;
    end
    else
    begin
      if Flat then
        Invalidate;
      MouseLeave;
    end;
end;

procedure TJvSpeedButton.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  if not FMenuTracking then
  begin
    inherited;
    if FDown then
      DblClick;
  end;
end;

procedure TJvSpeedButton.WMMouseMove(var Msg: TMessage);
begin
  inherited;
end;

procedure TJvSpeedButton.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvSpeedButton.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

initialization
  GCheckBitmap := nil;

finalization
  DestroyLocals;

end.

