{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpeedButton.pas, released on 2003-10-19.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Peter Thornqvist [peter3 at sourceforge dot net]
  Dejoy Den
  Andreas Hausladen

Changes:
2003-10-19:
  * Moved TJvSpeedButton from JvxCtrls to this unit
2005-05-20:(dejoy)
  * TJvSpeedButton implemented interface of IJvHotTrack.
2005-06-04:(dejoy)
  * fixed bug: memory leak in TJvCustomSpeedButton.Paint;

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSpeedButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  CommCtrl, Types, SysUtils, Classes, Windows, Messages,
  Controls, Graphics, Forms, ExtCtrls, Buttons, Menus, ImgList, ActnList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvExControls, JvComponent, JvConsts, JvTypes, JvHotTrackPersistent,
  JvThemes;

type
  TJvNumGlyphs = 1..5;
  TJvDropDownMenuPos = (dmpBottom, dmpRight);
  TJvButtonState = (rbsUp, rbsDisabled, rbsDown, rbsExclusive, rbsInactive);
  TJvSpeedButtonHotTrackOptions = TJvHotTrackOptions;

  TJvxButtonGlyph = class;

  TJvCustomSpeedButton = class(TJvGraphicControl, IJvHotTrack)
  private
    FAllowAllUp: Boolean;
    FAllowTimer: Boolean;
    FDown: Boolean;
    FDragging: Boolean;
    FDropDownMenu: TPopupMenu;
    FFlat: Boolean;
    FFontSave: TFont;
    FGlyph: TJvxButtonGlyph;
    FGroupIndex: Integer;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FHotTrackOptions: TJvHotTrackOptions;
    FInactiveGrayed: Boolean;
    FInitRepeatPause: Word;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FMarkDropDown: Boolean;
    FMenuPosition: TJvDropDownMenuPos;
    FMenuTracking: Boolean;
    FModalResult: TModalResult;
    FRepeatPause: Word;
    FRepeatTimer: TTimer;
    FSpacing: Integer;
    FStyle: TButtonStyle;
    FTransparent: Boolean;
    FDoubleBuffered: Boolean;
    function GetAlignment: TAlignment;
    function GetGrayNewStyle: Boolean;
    function GetWordWrap: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetAllowTimer(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetFlat(Value: Boolean);
    procedure SetGrayNewStyle(const Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetInactiveGrayed(Value: Boolean);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetMarkDropDown(Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);

    {IJvHotTrack}   //added by dejoy 2005-04-20
    function GetHotTrack:Boolean;
    function GetHotTrackFont:TFont;
    function GetHotTrackFontOptions:TJvTrackFontOptions;
    function GetHotTrackOptions:TJvHotTrackOptions;
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvHotTrackOptions);
    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;

    function CheckMenuDropDown(const Pos: TSmallPoint; Manual: Boolean): Boolean;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerExpired(Sender: TObject);
    procedure UpdateExclusive;
    procedure CMButtonPressed(var Msg: TCMButtonPressed); message CM_JVBUTTONPRESSED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    function IsHotTrackFontStored: Boolean;
  protected
    FState: TJvButtonState;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure VisibleChanged; override;
    function GetDropDownMenuPos: TPoint;
    procedure Loaded; override;
    procedure Paint; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TJvButtonState; DrawMark, PaintOnGlass: Boolean); virtual; abstract;
    property ButtonGlyph: TJvxButtonGlyph read FGlyph;
    property IsDragging: Boolean read FDragging;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick;
    function CheckBtnMenuDropDown: Boolean;
    procedure Click; override;
    procedure UpdateTracking;
    procedure Repaint; override;
  protected
    property Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property AllowTimer: Boolean read FAllowTimer write SetAllowTimer default False;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered default True;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropdownMenu;
    property Flat: Boolean read FFlat write SetFlat default False;
    { If True, Image is grayed when not mouse is in control (Only for flat buttons) }
    property GrayedInactive: Boolean read FInactiveGrayed write SetInactiveGrayed default True;
    { If True, Image is grayed (when enables=False) like the imagelist does, otherwise like the speedbutton does }
    property GrayNewStyle: Boolean read GetGrayNewStyle write SetGrayNewStyle default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HotTrackOptions: TJvHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;
    property InitPause: Word read FInitRepeatPause write FInitRepeatPause default 500;
    { (rb) Weird default }
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property MarkDropDown: Boolean read FMarkDropDown write SetMarkDropDown default True;
    property MenuPosition: TJvDropDownMenuPos read FMenuPosition write FMenuPosition default dmpBottom;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property RepeatInterval: Word read FRepeatPause write FRepeatPause default 100;
    { (rb) Weird default }
    property Spacing: Integer read FSpacing write SetSpacing default 1;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property ParentColor default False;
    property Color default clBtnFace;

    property OnMouseEnter;
    property OnMouseLeave;
  end;

  TJvImageSpeedButton = class;
  TJvSpeedButton = class;

  TJvImageSpeedButtonActionLink = class(TControlActionLink)
  protected
    FClient: TJvImageSpeedButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TJvSpeedButtonActionLink = class(TControlActionLink)
  protected
    FClient: TJvSpeedButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TJvImageSpeedButton = class(TJvCustomSpeedButton)
  private
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FHotTrackImageIndex: TImageIndex;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetHotTrackImageIndex(const Value: TImageIndex);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure InvalidateImage;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TJvButtonState; DrawMark, PaintOnGlass: Boolean); override;
    function IsImageVisible: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Alignment;
    property AllowAllUp;
    property AllowTimer;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property Caption;
    property Constraints;
    property Color;
    property ParentColor;
    { Ensure group index is declared before Down }
    property GroupIndex;
    property DoubleBuffered;
    property Down;
    property DragMode;
    property DropDownMenu;
    property Enabled;
    property Flat;
    property Font;
    property GrayedInactive;
    property GrayNewStyle;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackImageIndex: TImageIndex read FHotTrackImageIndex write SetHotTrackImageIndex;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property InitPause;
    property Layout;
    property Margin;
    property MarkDropDown;
    property MenuPosition;
    property ModalResult;
    property ParentFont;
    property ParentShowHint default False;
    property RepeatInterval;
    property ShowHint default True;
    property Spacing;
    property Style;
    property Transparent;
    property Visible;
    property WordWrap;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnStartDrag;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSpeedButton = class(TJvCustomSpeedButton)
  private
    FHotTrackGlyph: TJvxButtonGlyph;
    FGlyphFromAction: Boolean;
    function GetGlyph: TBitmap;
    function GetHotTrackGlyph: TBitmap;
    function GetNumGlyphs: TJvNumGlyphs;
    procedure GlyphChanged(Sender: TObject);
    procedure HotTrackGlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetHotTrackGlyph(const Value: TBitmap);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function IsGlyphStored: Boolean;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPalette: HPALETTE; override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TJvButtonState; DrawMark, PaintOnGlass: Boolean); override;
    procedure SyncHotGlyph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Alignment;
    property AllowAllUp;
    property AllowTimer;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property Caption;
    property Constraints;
    property Color;
    property ParentColor;
    { Ensure group index is declared before Down }
    property GroupIndex;
    property DoubleBuffered;
    property Down;
    property DragMode;
    property DropDownMenu;
    property Enabled;
    property Flat;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property GrayedInactive;
    property GrayNewStyle;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    {Inserted by (ag) 2004-09-04}
    property HotTrackOptions;
    {Insert End}
    property HotTrackGlyph: TBitmap read GetHotTrackGlyph write SetHotTrackGlyph;
    property InitPause;
    property Layout;
    property Margin;
    property MarkDropDown;
    property MenuPosition;
    property ModalResult;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint default False;
    property RepeatInterval;
    property ShowHint default True;
    property Spacing;
    property Style;
    {$IFDEF HAS_PROPERTY_STYLEELEMENTS}
    property StyleElements;
    {$ENDIF HAS_PROPERTY_STYLEELEMENTS}
    property Transparent;
    property Visible;
    property WordWrap;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
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
    FGlyph: TJvxButtonGlyph;
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
      Layout: TButtonLayout; AFont: TFont; Images: TCustomImageList;
      ImageIndex: Integer; Flags: Word; PaintOnGlass: Boolean);
    procedure Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
      Layout: TButtonLayout; AFont: TFont; Flags: Word; PaintOnGlass: Boolean);
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read FCaption write FCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonSize: TPoint read FButtonSize write FButtonSize;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

  { (rb) Similar class in JvButtons.pas }
  TJvxButtonGlyph = class(TObject)
  private
    FAlignment: TAlignment;
    FGlyphList: TCustomImageList;
    FGrayNewStyle: Boolean;
    FIndexs: array [TJvButtonState] of Integer;
    FNumGlyphs: TJvNumGlyphs;
    FOnChange: TNotifyEvent;
    FOriginal: TBitmap;
    FTransparentColor: TColor;
    FWordWrap: Boolean;
    FPaintOnGlass: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGrayNewStyle(const Value: Boolean);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function MapColor(Color: TColor): TColor;
  protected
    procedure MinimizeCaption(Canvas: TCanvas; var Caption: string; Width: Integer);
    function CreateButtonGlyph(State: TJvButtonState): Integer;
    function CreateImageGlyph(State: TJvButtonState; Images: TCustomImageList;
      Index: Integer): Integer;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect; const AOffset: TPoint;
      var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect;
      Flags: Word; Images: TCustomImageList; ImageIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    function DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TJvButtonState): TPoint;
    function DrawButtonImage(Canvas: TCanvas; X, Y: Integer; Images: TCustomImageList;
      ImageIndex: Integer; State: TJvButtonState): TPoint;
    function DrawEx(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      PopupMark: Boolean; Images: TCustomImageList; ImageIndex: Integer;
      State: TJvButtonState; Flags: Word; PaintOnGlass: Boolean): TRect;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TJvButtonState; Flags: Word);
    procedure DrawPopupMark(Canvas: TCanvas; X, Y: Integer;
      State: TJvButtonState);
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      PopupMark: Boolean; State: TJvButtonState; Flags: Word; PaintOnGlass: Boolean): TRect;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property GrayNewStyle: Boolean read FGrayNewStyle write SetGrayNewStyle;
    property NumGlyphs: TJvNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ DrawButtonFrame - returns the remaining usable area inside the Client rect }

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean; Style: TButtonStyle; AColor: TColor): TRect;

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
  Math, JvJCLUtils, JvJVCLUtils;

type
  TJvGlyphList = class;

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

  TJvGlyphList = class(TCustomImageList)
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

  TWinControlAccess = class(TWinControl);

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);

var
  // (rom) changed to var
  // (rb) used for?
  ButtonCount: Integer;
  GlyphCache: TJvGlyphCache;
  TempBrushBitmap: TBitmap = nil;
  SaveColor1, SaveColor2: TColor;

//=== Local procedures =======================================================

{ DrawButtonFrame - returns the remaining usable area inside the Client rect }

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean; Style: TButtonStyle; AColor: TColor): TRect;

var
  NewStyle: Boolean;
  ShadowColor, HighlightColor: TColor;

  // Honeymic
  function GetHighlightColor(BaseColor: TColor): TColor;
  begin
   Result := RGB(
      Min(GetRValue(ColorToRGB(BaseColor)) + 64, 255),
      Min(GetGValue(ColorToRGB(BaseColor)) + 64, 255),
      Min(GetBValue(ColorToRGB(BaseColor)) + 64, 255));
  end;

  // Honeymic
  function GetShadowColor(BaseColor: TColor): TColor;
  begin
   Result := RGB(
      Max(GetRValue(ColorToRGB(BaseColor)) - 64, 0),
      Max(GetGValue(ColorToRGB(BaseColor)) - 64, 0),
      Max(GetBValue(ColorToRGB(BaseColor)) - 64, 0));
  end;

begin
  Result := Client;
  NewStyle := (Style = bsNew) or (Style = bsAutoDetect);
  ShadowColor := GetShadowColor(AColor);     // Honeymic
  HighlightColor := GetHighlightColor(AColor);  // Honeymic

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
        // Honeymic
        Frame3D(Canvas, Result, clWindowFrame, HighlightColor, 1);
        Frame3D(Canvas, Result, ShadowColor, AColor, 1);
      end
      else
        // Honeymic
        Frame3D(Canvas, Result, ShadowColor, HighlightColor, 1);
    end
    else
    begin
      if IsFlat then
      begin
        // Honeymic
        Frame3D(Canvas, Result, clWindowFrame, HighlightColor, 1);
      end
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        // Honeymic
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
        // Honeymic
        Frame3D(Canvas, Result, HighlightColor, ShadowColor, 1)
      else
      begin
        // Honeymic
        Frame3D(Canvas, Result, HighlightColor, clWindowFrame, 1);
        Frame3D(Canvas, Result, AColor, ShadowColor, 1);
      end;
    end
    else
    begin
      if IsFlat then
        // Honeymic
        Frame3D(Canvas, Result, HighlightColor, clWindowFrame, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        // Honeymic
        Frame3D(Canvas, Result, HighlightColor, ShadowColor, 1);
      end;
    end;
  end;
  InflateRect(Result, -1, -1);
end;

function GetBrushPattern(Color1, Color2: TColor): TBitmap;
begin
  if TempBrushBitmap = nil then
    TempBrushBitmap := CreateTwoColorsBrushPattern(Color1, Color2)
  else
  begin
    if (Color1 <> SaveColor1) or (Color2 <> SaveColor2) then
    begin
      FreeAndNil(TempBrushBitmap);
      TempBrushBitmap := CreateTwoColorsBrushPattern(Color1, Color2);
    end;
  end;
  SaveColor1 := Color1;
  SaveColor2 := Color2;

  Result := TempBrushBitmap;
end;

//=== { TJvButtonImage } =====================================================

constructor TJvButtonImage.Create;
begin
  inherited Create;
  FGlyph := TJvxButtonGlyph.Create;
  NumGlyphs := 1;
  FButtonSize := Point(24, 23);
end;

destructor TJvButtonImage.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvButtonImage.Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Flags: Word; PaintOnGlass: Boolean);
begin
  DrawEx(Canvas, X, Y, Margin, Spacing, Layout, AFont, nil, -1, Flags, PaintOnGlass);
end;

procedure TJvButtonImage.DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Images: TCustomImageList; ImageIndex: Integer;
  Flags: Word; PaintOnGlass: Boolean);
var
  Target: TRect;
  SaveColor: Integer;
  SaveFont: TFont;
  Offset: TPoint;
begin
  SaveColor := Canvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(Canvas.Font);
  try
    Target := Bounds(X, Y, FButtonSize.X, FButtonSize.Y);
    Offset := Point(0, 0);
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Target);
    Frame3D(Canvas, Target, clBtnShadow, clWindowFrame, 1);
    Frame3D(Canvas, Target, clBtnHighlight, clBtnShadow, 1);
    if AFont <> nil then
      Canvas.Font := AFont;
    FGlyph.DrawEx(Canvas, Target, Offset, Caption, Layout, Margin,
      Spacing, False, Images, ImageIndex, rbsUp, Flags, PaintOnGlass);
  finally
    Canvas.Font.Assign(SaveFont);
    SaveFont.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

function TJvButtonImage.GetAlignment: TAlignment;
begin
  Result := FGlyph.Alignment;
end;

function TJvButtonImage.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end;

function TJvButtonImage.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := FGlyph.NumGlyphs;
end;

function TJvButtonImage.GetWordWrap: Boolean;
begin
  Result := FGlyph.WordWrap;
end;

procedure TJvButtonImage.Invalidate;
begin
  FGlyph.Invalidate;
end;

procedure TJvButtonImage.SetAlignment(Value: TAlignment);
begin
  FGlyph.Alignment := Value;
end;

procedure TJvButtonImage.SetGlyph(Value: TBitmap);
begin
  FGlyph.Glyph := Value;
end;

procedure TJvButtonImage.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  FGlyph.NumGlyphs := Value;
end;

procedure TJvButtonImage.SetWordWrap(Value: Boolean);
begin
  FGlyph.WordWrap := Value;
end;

//=== { TJvCustomSpeedButton } ===============================================

constructor TJvCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clBtnFace;
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FHotTrackOptions := TJvSpeedButtonHotTrackOptions.Create(Self);
  FFontSave := TFont.Create;
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  FInactiveGrayed := True;
  FGlyph := TJvxButtonGlyph.Create;
  FGlyph.GrayNewStyle := True;
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
  FDoubleBuffered := True;
  Inc(ButtonCount);
end;

destructor TJvCustomSpeedButton.Destroy;
begin
  FHotTrackOptions.Free;
  FGlyph.Free;
  Dec(ButtonCount);
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TJvCustomSpeedButton.ButtonClick;
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
    Sleep(20); // (ahuser) why?
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

function TJvCustomSpeedButton.CheckBtnMenuDropDown: Boolean;
begin
  Result := CheckMenuDropDown(PointToSmallPoint(GetDropDownMenuPos), True);
end;

function TJvCustomSpeedButton.CheckMenuDropDown(const Pos: TSmallPoint;
  Manual: Boolean): Boolean;
var
  Form: TCustomForm;
  Pt: TPoint;
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
    Pt := ClientToScreen(SmallPointToPoint(Pos));
    DropDownMenu.Popup(Pt.X, Pt.Y);
    Result := True;
  end;
end;

procedure TJvCustomSpeedButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TJvCustomSpeedButton.CMButtonPressed(var Msg: TCMButtonPressed);
var
  Sender: TControl;
begin
  if (Msg.Index = FGroupIndex) and Parent.HandleAllocated then
  begin
    Sender := Msg.Control;
    if (Sender <> nil) and (Sender is TJvCustomSpeedButton) then
      if Sender <> Self then
      begin
        if TJvCustomSpeedButton(Sender).Down and FDown then
        begin
          FDown := False;
          FState := rbsUp;
          Repaint;
        end;
        FAllowAllUp := TJvCustomSpeedButton(Sender).AllowAllUp;
      end;
  end;
end;

function TJvCustomSpeedButton.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and (ssAlt in Shift);
  if Result then
    Click
  else
    inherited WantKey(Key, Shift);
end;

procedure TJvCustomSpeedButton.EnabledChanged;
var
  State: TJvButtonState;
begin
  inherited EnabledChanged;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  FGlyph.CreateButtonGlyph(State);
  { Resync MouseOver }
  UpdateTracking;
  Repaint;
end;

procedure TJvCustomSpeedButton.FontChanged;
begin
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
  Invalidate;
end;

procedure TJvCustomSpeedButton.MouseEnter(Control: TControl);
var
  NeedRepaint: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver and Enabled  then
  begin
    { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
      be used as a dock client. }
    NeedRepaint :=
      {$IFDEF JVCLThemesEnabled}
      StyleServices.Enabled or
      {$ENDIF JVCLThemesEnabled}
      FHotTrack or (FFlat and Enabled and (DragMode <> dmAutomatic) and (GetCapture = NullHandle));

    NeedRepaint := NeedRepaint and not Mouse.IsDragging;

    inherited MouseEnter(Control); // set MouseOver
    { Windows XP introduced hot states also for non-flat buttons. }
    if NeedRepaint then
      Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.MouseLeave(Control: TControl);
var
  NeedRepaint: Boolean;
begin
  if MouseOver and Enabled  then
  begin
    NeedRepaint :=
      {$IFDEF JVCLThemesEnabled}
      { Windows XP introduced hot states also for non-flat buttons. }
      StyleServices.Enabled or
      {$ENDIF JVCLThemesEnabled}
      HotTrack or (FFlat and Enabled and not FDragging and (GetCapture = NullHandle));

    NeedRepaint := NeedRepaint and not Mouse.IsDragging;

    inherited MouseLeave(Control); // set MouseOver
    if NeedRepaint then
      Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.CMSysColorChange(var Msg: TMessage);
begin
  FGlyph.Invalidate;
  Invalidate;
end;

procedure TJvCustomSpeedButton.TextChanged;
begin
  Invalidate;
end;

procedure TJvCustomSpeedButton.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible then
    UpdateTracking;
end;

procedure TJvCustomSpeedButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
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
      { Calling Click might open a new window or something which will remove
        the focus; if the new window is modal then UpdateTracking won't be
        called until the window is closed, thus: }
      MouseLeave(Self);
      Perform(CM_MOUSELEAVE, 0, 0);
      { Even if the mouse is not in the control (DoClick=False) we must redraw
        the image, because it must change from hot -> normal }
      //if not DoClick then
        Invalidate;
    end
    else
    if DoClick then
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
    begin
      Click;
    end;
  end;
  { After a Click call a lot can happen thus check whether we're hot or not: }
  UpdateTracking;
end;

function TJvCustomSpeedButton.GetAlignment: TAlignment;
begin
  Result := FGlyph.Alignment;
end;

function TJvCustomSpeedButton.GetDropDownMenuPos: TPoint;
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

function TJvCustomSpeedButton.GetGrayNewStyle: Boolean;
begin
  Result := FGlyph.GrayNewStyle;
end;

function TJvCustomSpeedButton.GetWordWrap: Boolean;
begin
  Result := FGlyph.WordWrap;
end;

procedure TJvCustomSpeedButton.Loaded;
var
  LState: TJvButtonState;
begin
  inherited Loaded;

  if Enabled then
  begin
    if Flat then
      LState := rbsInactive
    else
      LState := rbsUp;
  end
  else
    LState := rbsDisabled;
  FGlyph.CreateButtonGlyph(LState);
end;

procedure TJvCustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Msg: TMsg;
begin
  try
    if FMenuTracking then
      Exit;
    inherited MouseDown(Button, Shift, X, Y);
    if not MouseOver and Enabled then
    begin
      MouseOver := True;
      Invalidate {Repaint};
    end;
    if (Button = mbLeft) and Enabled {and not (ssDouble in Shift)} then
    begin
      if not FDown then
      begin
        FState := rbsDown;
        Invalidate {Repaint};
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
            if (P.X >= 0) and (P.X < ClientWidth) and (P.Y >= 0) and (P.Y <= ClientHeight) then
              KillMessage(Windows.HWND_DESKTOP, Msg.Message);
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
  finally
  end;
end;

procedure TJvCustomSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TJvCustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
end;

procedure TJvCustomSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DropDownMenu) and (Operation = opRemove) then
    DropDownMenu := nil;
end;

procedure TJvCustomSpeedButton.Paint;
var
  PaintRect: TRect;
  State: TJvButtonState;
  OldPenColor:TColor;
  Offset: TPoint;
  {$IFDEF JVCLThemesEnabled}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  if not Enabled {and not (csDesigning in ComponentState)} then
  begin
    FState := rbsDisabled;
    FDragging := False;
  end
  else
  if FState = rbsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := rbsExclusive
    else
      FState := rbsUp;

  if FFlat and not MouseOver and not (csDesigning in ComponentState) then
    { rbsInactive : flat and not 'mouse in control', thus
        - picture might be painted gray
        - no border, unless Button is exclusive
    }
    State := rbsInactive
  else
    State := FState;

  PaintRect := Rect(0, 0, Width, Height);

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if ControlInGlassPaint(Self) then
      FillRect(Canvas.Handle, ClientRect, GetStockObject(BLACK_BRUSH))
    else
    begin
      if FTransparent then
        CopyParentImage(Self, Canvas)
      else
      begin
        if not DoubleBuffered then
          PerformEraseBackground(Self, Canvas.Handle) // uses Control.Left/Top as Offset
        else
          PerformEraseBackground(Self, Canvas.Handle, Point(0, 0)); // we are drawing into a bitmap
      end;
    end;

    if (MouseOver or FDragging) and HotTrack then
      Canvas.Font := Self.HotTrackFont
    else
      Canvas.Font := Self.Font;

    if not Enabled then
      Button := tbPushButtonDisabled
    else
    if FState in [rbsDown, rbsExclusive] then
      Button := tbPushButtonPressed
    else
    if MouseOver or FDragging then
      Button := tbPushButtonHot
    else
      Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if FFlat then
    begin
      case Button of
        tbPushButtonDisabled:
          ToolButton := ttbButtonDisabled;
        tbPushButtonPressed:
          ToolButton := ttbButtonPressed;
        tbPushButtonHot:
          ToolButton := ttbButtonHot;
        tbPushButtonNormal:
          ToolButton := ttbButtonNormal;
      end;
    end;

    if ToolButton = ttbToolbarDontCare then
    begin
      Details := StyleServices.GetElementDetails(Button);
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
    end
    else
    begin
      Details := StyleServices.GetElementDetails(ToolButton);
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
    end;

    if (Button = tbPushButtonPressed) and Flat then
      // A pressed speed Button has a white text. This applies however only to flat buttons.
      //if ToolButton <> ttbToolbarDontCare then
      //  Canvas.Font.Color := clHighlightText;
      Offset := Point(1, 0)
    else
      Offset := Point(0, 0);

    { Check whether the image need to be painted gray.. }
    if (FState = rbsDisabled) or not FInactiveGrayed then
      { .. do not paint gray image }
      State := FState;

    PaintImage(Canvas, PaintRect, Offset, State,
      FMarkDropDown and Assigned(FDropDownMenu), ControlInGlassPaint(Self));
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    if FTransparent then
      CopyParentImage(Self, Canvas)
    else
    begin
      if Flat then
        Canvas.Brush.Color := TWinControlAccess(Parent).Color
      else
        Canvas.Brush.Color := Self.Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(PaintRect);
    end;

    if (State <> rbsInactive) or (FState = rbsExclusive) then
      PaintRect := DrawButtonFrame(Canvas, PaintRect,
        FState in [rbsDown, rbsExclusive], FFlat, FStyle, Color)
    else
    if FFlat then
      InflateRect(PaintRect, -2, -2);

    if (FState = rbsExclusive) and not Transparent and
      (not FFlat or (State = rbsInactive)) then
    begin
      Canvas.Brush.Bitmap := AllocPatternBitmap(Self.Color, clBtnHighlight);
      InflateRect(PaintRect, 1, 1);
      Canvas.FillRect(PaintRect);
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [rbsDown, rbsExclusive] then
      Offset := Point(1, 1)
    else
      Offset := Point(0, 0);

    { Check whether the image need to be painted gray.. }
    if (FState = rbsDisabled) or not FInactiveGrayed then
      { .. do not paint gray image }
      State := FState;

    if ((HotTrackOptions.Enabled and Down) or (MouseOver or FDragging)) and HotTrack then
    begin
      Canvas.Font := Self.HotTrackFont;
      {Inserted by (ag) 2004-09-04}
      if HotTrackOptions.Enabled then
        begin
          if Down then  //fixed bug: memory leak
            Canvas.Brush.Bitmap := GetBrushPattern(HotTrackOptions.Color, clWindow)
          else
          begin
            Canvas.Brush.Color := HotTrackOptions.Color;
            Canvas.Brush.Style := bsSolid;
          end;
          {Inserted by (dejoy) 2005-05-20}
          if HotTrackOptions.FrameVisible then
          begin
            OldPenColor := Canvas.Pen.Color;
            Canvas.Pen.Color := HotTrackOptions.FrameColor;
            Canvas.Rectangle(0, 0, Width, Height);
            Canvas.Pen.Color := OldPenColor;
          end
          else
          begin
            PaintRect := ClientRect;
            if Flat then
              InflateRect(PaintRect,-1,-1)
            else
              InflateRect(PaintRect,-1,-2);
            Canvas.FillRect(PaintRect);
          end;
         {Insert End by (dejoy)}
          if Down then
            Canvas.Brush.Bitmap := nil; // release bitmap
        end;
      {Insert End}
    end else
      Canvas.Font := Self.Font;
    PaintImage(Canvas, PaintRect, Offset, State,
      FMarkDropDown and Assigned(FDropDownMenu), ControlInGlassPaint(Self));
  end;
end;

procedure TJvCustomSpeedButton.Repaint;
begin
  if GetGlassPaintFlag(Self) then
    Invalidate
  else
    inherited;
end;

procedure TJvCustomSpeedButton.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FGlyph.Alignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomSpeedButton.SetAllowTimer(Value: Boolean);
begin
  FAllowTimer := Value;
  if not FAllowTimer and (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Enabled := False;
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TJvCustomSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then
    Value := False;
  if Value <> FDown then
  begin
    if FDown and not FAllowAllUp then
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

procedure TJvCustomSpeedButton.SetDropdownMenu(Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FDropDownMenu));
  if FMarkDropDown then
    Invalidate;
end;

procedure TJvCustomSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetGrayNewStyle(const Value: Boolean);
begin
  if GrayNewStyle <> Value then
  begin
    FGlyph.GrayNewStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomSpeedButton.SetHotTrackFont(Value: TFont);
begin
  if (FHotTrackFont<>Value) and (Value <> nil) then
    FHotTrackFont.Assign(Value);
end;

procedure TJvCustomSpeedButton.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvCustomSpeedButton.SetHotTrackOptions(Value: TJvHotTrackOptions);
begin
  if (FHotTrackOptions <> Value) and (Value <> nil) then
    FHotTrackOptions.Assign(Value);
end;

procedure TJvCustomSpeedButton.IJvHotTrack_Assign(
  Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    HotTrack := Source.HotTrack;
    HotTrackFont :=Source.HotTrackFont;
    HotTrackFontOptions := Source.HotTrackFontOptions;
    HotTrackOptions := Source.HotTrackOptions;
  end;
end;

function TJvCustomSpeedButton.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCustomSpeedButton.SetInactiveGrayed(Value: Boolean);
begin
  if Value <> FInactiveGrayed then
  begin
    FInactiveGrayed := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetMarkDropDown(Value: Boolean);
begin
  if Value <> FMarkDropDown then
  begin
    FMarkDropDown := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetStyle(Value: TButtonStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpeedButton.SetWordWrap(Value: Boolean);
begin
  if Value <> WordWrap then
  begin
    FGlyph.WordWrap := Value;
    Invalidate;
  end;
end;

function TJvCustomSpeedButton.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomSpeedButton.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJvCustomSpeedButton.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := FHotTrackFontOptions;
end;

function TJvCustomSpeedButton.GetHotTrackOptions: TJvHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

procedure TJvCustomSpeedButton.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
  end;
end;

procedure TJvCustomSpeedButton.TimerExpired(Sender: TObject);
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

procedure TJvCustomSpeedButton.UpdateExclusive;
var
  Msg: TCMButtonPressed;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_JVBUTTONPRESSED;
    Msg.Index := FGroupIndex;
    Msg.Control := Self;
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomSpeedButton.UpdateTracking;
var
  P: TPoint;
  NewValue: Boolean;
begin
  GetCursorPos(P);
  NewValue := Enabled and (FindDragTarget(P, True) = Self) and IsForegroundTask;
  if MouseOver <> NewValue then
    if NewValue then
      Perform(CM_MOUSEENTER, 0, 0)
    else
      Perform(CM_MOUSELEAVE, 0, 0);
end;

procedure TJvCustomSpeedButton.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  if not FMenuTracking then
  begin
    inherited;
    if FDown then
      DblClick;
  end;
end;

procedure TJvCustomSpeedButton.WMPaint(var Msg: TWMPaint);
var
  MemBitmap: HBitmap;
  SaveBitmap: HBitmap;
  MemDC: HDC;
  DC: HDC;
begin
  if not DoubleBuffered or ControlInGlassPaint(Self) then
    inherited
  else
  if Msg.DC <> 0 then
  begin
    MemBitmap := CreateCompatibleBitmap(Msg.DC, Width, Height);
    MemDC := CreateCompatibleDC(Msg.DC);
    SaveBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := Msg.DC;
      Msg.DC := MemDC;
      inherited;
      Msg.DC := DC;
      BitBlt(Msg.DC, 0, 0, Width, Height, MemDC, 0, 0, SRCCOPY);
    finally
      SelectObject(MemDC, SaveBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure TJvCustomSpeedButton.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomSpeedButton.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

//=== { TJvGlyphCache } ======================================================

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

function TJvGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

function TJvGlyphCache.GetList(AWidth, AHeight: Integer): TJvGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := TJvGlyphList(FGlyphLists[I]);
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

//=== { TJvGlyphList } =======================================================

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

constructor TJvGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

procedure TJvGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

destructor TJvGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

//=== { TJvImageSpeedButton } ================================================

procedure TJvImageSpeedButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (not CheckDefaults or (Self.Images = nil)) and (ActionList <> nil) then
        Self.Images := TCustomImageList(ActionList.Images);
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

constructor TJvImageSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
end;

destructor TJvImageSpeedButton.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

function TJvImageSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvImageSpeedButtonActionLink;
end;

procedure TJvImageSpeedButton.ImageListChange(Sender: TObject);
begin
  InvalidateImage;
end;

procedure TJvImageSpeedButton.InvalidateImage;
begin
  Invalidate;
end;

function TJvImageSpeedButton.IsImageVisible: Boolean;
begin
  Result := {FImageVisible and} Assigned(FImages) and (FImageIndex >= 0)
end;

procedure TJvImageSpeedButton.PaintImage(Canvas: TCanvas; ARect: TRect;
  const Offset: TPoint; AState: TJvButtonState; DrawMark: Boolean; PaintOnGlass: Boolean);
var
  LImageIndex: TImageIndex;
begin
  if (MouseOver or FDragging) and HotTrack and (HotTrackImageIndex <> -1) then
    LImageIndex := HotTrackImageIndex
  else
    LImageIndex := ImageIndex;
  FGlyph.DrawEx(Canvas, ARect, Offset, Caption, FLayout,
    FMargin, FSpacing, DrawMark, Images, LImageIndex, AState, DrawTextBiDiModeFlags(Alignments[Alignment]),
    PaintOnGlass);
end;

procedure TJvImageSpeedButton.SetHotTrackImageIndex(
  const Value: TImageIndex);
begin
  if FHotTrackImageIndex <> Value then
  begin
    FHotTrackImageIndex := Value;
    { Only invalidate when hot }
    if (MouseOver or FDragging) and HotTrack then
      InvalidateImage;
  end;
end;

procedure TJvImageSpeedButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    { Only invalidate when not hot }
    if not (MouseOver or FDragging) or not HotTrack then
      InvalidateImage;
  end;
end;

procedure TJvImageSpeedButton.SetImages(const Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FImages, FImageChangeLink);
  if FImages = nil then
    SetImageIndex(-1);
  InvalidateImage;
end;

//=== { TJvImageSpeedButtonActionLink } ======================================

procedure TJvImageSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvImageSpeedButton;
end;

function TJvImageSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

function TJvImageSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  { (rb) This will fail in D7 due to a bug in TCustomAction.SetGroupIndex }
  Result := (FClient is TJvCustomSpeedButton) and
    (FClient.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TJvImageSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    FClient.GroupIndex := Value;
end;

function TJvImageSpeedButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TJvImageSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    FClient.Down := Value;
end;

procedure TJvImageSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

//=== { TJvSpeedButton } =====================================================

procedure TJvSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

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
      FGlyphFromAction := True;
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      { Copy image from action's imagelist }
      if (FGlyphFromAction or (Glyph.Empty)) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(TCustomImageList(ActionList.Images), ImageIndex);
    end;
end;

constructor TJvSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph.OnChange := GlyphChanged;
  FHotTrackGlyph := TJvxButtonGlyph.Create;
  FHotTrackGlyph.OnChange := HotTrackGlyphChanged;
end;

destructor TJvSpeedButton.Destroy;
begin
  FHotTrackGlyph.Free;
  inherited Destroy;
end;

function TJvSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvSpeedButtonActionLink;
end;

function TJvSpeedButton.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end;

function TJvSpeedButton.GetHotTrackGlyph: TBitmap;
begin
  Result := FHotTrackGlyph.Glyph;
end;

function TJvSpeedButton.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := FGlyph.NumGlyphs;
end;

function TJvSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TJvSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvSpeedButton.HotTrackGlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

function TJvSpeedButton.IsGlyphStored: Boolean;
begin
  Result := not FGlyphFromAction;
end;

procedure TJvSpeedButton.PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
  AState: TJvButtonState; DrawMark, PaintOnGlass: Boolean);
begin
  if (MouseOver or FDragging) and HotTrack and not HotTrackGlyph.Empty then
  begin
    SyncHotGlyph;
    FHotTrackGlyph.Draw(Canvas, ARect, Offset, Caption, FLayout,
      FMargin, FSpacing, DrawMark, AState, DrawTextBiDiModeFlags(Alignments[Alignment]),
      PaintOnGlass);
  end
  else
    FGlyph.Draw(Canvas, ARect, Offset, Caption, FLayout,
      FMargin, FSpacing, DrawMark, AState, DrawTextBiDiModeFlags(Alignments[Alignment]),
      PaintOnGlass);
end;

procedure TJvSpeedButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Glyph := Value;
  FGlyphFromAction := False;
  Invalidate;
end;

procedure TJvSpeedButton.SetHotTrackGlyph(const Value: TBitmap);
begin
  FHotTrackGlyph.Glyph := Value;
  Invalidate;
end;

procedure TJvSpeedButton.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else
  if Value > Ord(High(TJvButtonState)) + 1 then
    Value := Ord(High(TJvButtonState)) + 1;
  if Value <> FGlyph.NumGlyphs then
  begin
    FGlyph.NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SyncHotGlyph;
begin
  with FHotTrackGlyph do
  begin
    OnChange := nil;
    try
      Alignment := FGlyph.Alignment;
      GrayNewStyle := FGlyph.GrayNewStyle;
      NumGlyphs := FGlyph.NumGlyphs;
      WordWrap := FGlyph.WordWrap;
    finally
      OnChange := HotTrackGlyphChanged;
    end;
  end;
end;

//=== { TJvSpeedButtonActionLink } ===========================================

procedure TJvSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvSpeedButton;
end;

function TJvSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

function TJvSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TJvSpeedButton) and
    (TJvSpeedButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TJvSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TJvSpeedButton(FClient).GroupIndex := Value;
end;

procedure TJvSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TJvSpeedButton(FClient).Down := Value;
end;

//=== { TJvxButtonGlyph } ====================================================

procedure TJvxButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect; const AOffset: TPoint;
  var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect;
  Flags: Word; Images: TCustomImageList; ImageIndex: Integer);
var
  TextPos: TPoint;
  MaxSize, ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);
  if Assigned(Images) and (Images.Width > 0) and (ImageIndex >= 0) and
    (ImageIndex < Images.Count) then
    GlyphSize := Point(Images.Width, Images.Height)
  else
  if FOriginal <> nil then
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
  MinimizeCaption(Canvas, Caption, MaxSize.X);
  if Caption <> '' then
  begin
    TextBounds := Rect(0, 0, MaxSize.X, 0);
    DrawGlassableText(Canvas.Handle, Caption, TextBounds, DT_CALCRECT or DT_VCENTER or
      WordWraps[FWordWrap] or Flags);
  end
  else
    TextBounds := Rect(0, 0, 0, 0);
  TextBounds.Bottom := Max(TextBounds.Top, TextBounds.Top + Min(MaxSize.Y, RectHeight(TextBounds)));
  TextBounds.Right := Max(TextBounds.Left, TextBounds.Left + Min(MaxSize.X, RectWidth(TextBounds)));
  TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  if PopupMark then
    if ((GlyphSize.X = 0) or (GlyphSize.Y = 0)) or (Layout = blGlyphLeft) then
      Inc(TextSize.X, 9)
    else
    if GlyphSize.X > 0 then
      Inc(GlyphSize.X, 6);

  { If the Layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X) div 2;
    TextPos.X := (ClientSize.X - TextSize.X) div 2;
  end;
  if Flags and DT_CENTER = 0 then
  begin
    if Flags and DT_RIGHT <> 0 then
    begin
      if Layout in [blGlyphLeft, blGlyphRight] then
        GlyphPos.X := ClientSize.X - GlyphSize.X - TextSize.X;
      TextPos.X := ClientSize.X - TextSize.X;
    end
    else
    begin
      if Layout in [blGlyphLeft, blGlyphRight] then
      begin
        GlyphPos.X := 0;
        TextPos.X := GlyphSize.X;
      end
      else
        TextPos.X := 0;
    end;
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
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if (Layout in [blGlyphLeft, blGlyphRight]) then
      begin
        if Flags and DT_CENTER <> 0 then
          Margin := (ClientSize.X div 2) - (TotalSize.X div 2)
        else
        begin
          if Layout = blGlyphRight then
          begin
            Margin := 0;
            if Flags and DT_RIGHT = 0 then
              Margin := ClientSize.X - TextSize.X - GlyphSize.X
          end
          else
            Margin := GlyphPos.X;
        end;
      end
      else
        Margin := (ClientSize.Y div 2) - (TotalSize.Y div 2);
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));
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
  Inc(GlyphPos.X, Client.Left + AOffset.X);
  Inc(GlyphPos.Y, Client.Top + AOffset.Y);

  OffsetRect(TextBounds, TextPos.X + Client.Left + AOffset.X, TextPos.Y + Client.Top + AOffset.Y);
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
  iWidth, iHeight, X, Y: Integer;
  IRect, ORect: TRect;
  I: TJvButtonState;
begin
  if (State = rbsDown) and (NumGlyphs < 3) then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then
    Exit;
  iWidth := FOriginal.Width div FNumGlyphs;
  iHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(iWidth, iHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := iWidth;
    TmpImage.Height := iHeight;
    IRect := Rect(0, 0, iWidth, iHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    I := State;
    if Ord(I) >= NumGlyphs then
      I := rbsUp;
    ORect := Rect(Ord(I) * iWidth, 0, (Ord(I) + 1) * iWidth, iHeight);
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
  Images: TCustomImageList; Index: Integer): Integer;
var
  TmpImage, Mask: TBitmap;
  iWidth, iHeight, X, Y: Integer;
begin
  if State = rbsDown then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (Images.Width = 0) or (Images.Height = 0) or
    (Images.Count = 0) then
    Exit;
  iWidth := Images.Width;
  iHeight := Images.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(iWidth, iHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := iWidth;
    TmpImage.Height := iHeight;
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          with TmpImage.Canvas do
          begin
            FillRect(Rect(0, 0, iWidth, iHeight));
            ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_NORMAL);
          end;
          Mask := TBitmap.Create;
          try
            with Mask do
            begin
              Monochrome := True;
              Height := iHeight;
              Width := iWidth;
            end;
            with Mask.Canvas do
            begin
              FillRect(Rect(0, 0, iWidth, iHeight));
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
          TmpImage.Canvas.FillRect(Rect(0, 0, iWidth, iHeight));
          ImageListDrawDisabled(Images, TmpImage.Canvas, 0, 0, Index,
            clBtnHighlight, clBtnShadow, True);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
      rbsInactive:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, iWidth, iHeight));
          ImageList_Draw(Images.Handle, Index, TmpImage.Canvas.Handle, 0, 0, ILD_NORMAL);
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
  const Offset: TPoint;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; State: TJvButtonState; Flags: Word; PaintOnGlass: Boolean): TRect;
begin
  Result := DrawEx(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    PopupMark, nil, -1, State, Flags, PaintOnGlass);
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
    DrawGlassableImageList(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL, FPaintOnGlass);
    Result := Point(FGlyphList.Width, FGlyphList.Height);
  end;
end;

function TJvxButtonGlyph.DrawButtonImage(Canvas: TCanvas; X, Y: Integer;
  Images: TCustomImageList; ImageIndex: Integer; State: TJvButtonState): TPoint;
var
  Index: Integer;
begin
  Result := Point(0, 0);
  if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
    Exit;

  if State = rbsDisabled then
  begin
    if GrayNewStyle then
      Images.Draw(Canvas, X, Y, ImageIndex, False)
    else
      ImageListDrawDisabled(Images, Canvas, X, Y, ImageIndex, clBtnHighlight,
        clBtnShadow, True);
  end
  else
  if State = rbsInactive then
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
  DC: HDC;
begin
  Canvas.Brush.Style := bsClear;
  Flags := DT_VCENTER or WordWraps[FWordWrap] or Flags;
  DC := Canvas.Handle;
  if State = rbsDisabled then
  begin
    OffsetRect(TextBounds, 1, 1);
    SetTextColor(DC, ColorToRGB(clBtnHighlight));
    DrawGlassableText(DC, Caption, TextBounds, Flags, FPaintOnGlass);
    OffsetRect(TextBounds, -1, -1);
    SetTextColor(DC, ColorToRGB(clBtnShadow));
    DrawGlassableText(DC, Caption, TextBounds, Flags, FPaintOnGlass);
  end
  else
    DrawGlassableText(DC, Caption, TextBounds, Flags, FPaintOnGlass);
end;

function TJvxButtonGlyph.DrawEx(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; Images: TCustomImageList; ImageIndex: Integer;
  State: TJvButtonState; Flags: Word; PaintOnGlass: Boolean): TRect;
var
  UseImages: Boolean;
  GlyphPos, PopupPos: TPoint;
  TextBounds: TRect;
  LCaption: string;
begin
  FPaintOnGlass := PaintOnGlass;
  { MinimizeCaption might change the caption }
  LCaption := Caption;
  CalcButtonLayout(Canvas, Client, Offset, LCaption, Layout, Margin, Spacing,
    PopupMark, GlyphPos, TextBounds, Flags, Images, ImageIndex);
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
  DrawButtonText(Canvas, LCaption, TextBounds, State, Flags);
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
      if LCaption <> '' then
        PopupPos.X := TextBounds.Right + 3
      else
        PopupPos.X := (Client.Left + Client.Right - 7) div 2;
      PopupPos.Y := TextBounds.Top + RectHeight(TextBounds) div 2;
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

  { Fix for Mantis #4864: JvSpeedButton AV }
  if GlyphCache <> nil then
    GlyphCache.ReturnList(TJvGlyphList(FGlyphList))
  else
    FGlyphList.Free;
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

procedure TJvxButtonGlyph.MinimizeCaption(Canvas: TCanvas; var Caption: string;
  Width: Integer);
var
  I: Integer;
  Lines: TStringList;
begin
  if FWordWrap then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := Caption;
    for I := 0 to Lines.Count - 1 do
      Lines[I] := MinimizeText(Lines[I], Canvas, Width);
    Caption := TrimRight(Lines.Text);
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

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(GlyphCache);
  FreeAndNil(TempBrushBitmap);

end.

