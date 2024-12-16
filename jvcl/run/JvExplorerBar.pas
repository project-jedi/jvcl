{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExplorerBar.pas, released on 2010-11-28.

The Initial Developers of the Original Code is: Max Evans
Copyright (c) 2009 Max Events
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Contributor(s):
  Andreas Hausladen (bugfixing, additional features)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExplorerBar;

// ahuser: TODO: All the "Repaint" must be changed to Invalidate()

interface

{$I jvcl.inc}

uses
  Windows, Messages, Types,
  SysUtils, Classes, Contnrs, Math,
  Graphics, Controls, ExtCtrls, Forms, ImgList,
  JclBase;

const
  JvExplorerConstDefaultWidth = 218;
  JvExplorerConstDefaultHeight = 300;
  JvExplorerConstDefaultItemHeight = 23;
  JvExplorerConstYOffset = 8;
  JvExplorerConstLineYOffset = 3;
  JvExplorerConstIconSide = 32;
  JvExplorerConstXOffset = 12;
  JvExplorerConstIconOffset = 4;
  JvExplorerConstUnknownValue = -1;
  JvExplorerConstUnknownRect: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
  JvExplorerConstAnimationSpeed = 30; // Timer.Interval
  JvExplorerConstAnimatinoCount = 10;
  JvExplorerConstScollbarWidth = 17;
  JvExplorerConstSkinLeftPartWidth = 5;
  JvExplorerConstSkinRightPartWidth = 25;
  JvExplorerConstXGap = 3;

type
  TJvExplorerHotStyle = set of (hsUnderline, hsBold, hsItalic, hsStrikeOut);
  //TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
  TJvExplorerIconSource = (isImageList, isBitmap, isNone);
  TJvExplorerGroupState = (gsOpen, gsClosed, gsOpening, gsClosing);
  TJvExplorerBarItemStates = (bisNormal, bisSelected, bisHot, bisChecked, bisPushed);
  TJvExplorerBarItemState = set of TJvExplorerBarItemStates;
  TJvExplorerMouseHotTrackPosition = (mhtTitle, mhtBody, mhtNone, mhtItem);

  TJvExplorerThemeElementRectType = (
    terTitleLeft,
    terTitleCenter,
    terTitleRightUpOff,
    terTitleRightUpOn,
    terTitleRightDownOff,
    terTitleRightDownOn,
    terSpecialTitleLeft,
    terSpecialTitleCenter,
    terSpecialTitleRightUpOff,
    terSpecialTitleRightUpOn,
    terSpecialTitleRightDownOff,
    terSpecialTitleRightDownOn
  );

  TJvExplorerTheme = (
    etBlue,
    etSilver,
    etOlive,
    etOrange,
    etBlueFlat
  );

const
  JvExplorerThemeNames: array[TJvExplorerTheme] of string = (
    'Blue',
    'Silver',
    'Olive',
    'Orange',
    'Blue Flat'
  );

type
  { flickerfree paint box OnPaint event type }
  TJvExplorerFFPaintEvent = procedure(Sender: TObject; Buffer: TBitmap) of object;

  { a flickerfree paint box }
  TJvExplorerPaintBox = class(TCustomControl)
  private
    FOnFFPaint: TJvExplorerFFPaintEvent;
    FBuffer: TBitmap; // buffered bitmap (filled when Repaint method is called)
    FDrawing: Boolean;
    FNeedRepaint: Boolean;
    FDeformHorz: Extended;
    FDeformVert: Extended;
    FDeformAlpha: Extended;
    FApplyDeform: Boolean;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Repaint; override;
    procedure ApplyDeform(Horz, Vert, Alpha: Extended);
    procedure NoDeform;

    property OnPaint: TJvExplorerFFPaintEvent read FOnFFPaint write FOnFFPaint;
  end;

  TJvExplorerBar = class;
  TJvExplorerBarGroupItem = class;
  TJvExplorerMenuItemClickEvent = procedure(Sender: TObject; Id: Integer) of object;
  TJvExplorerItemClickEvent = procedure(Sender: TObject; Item: TJvExplorerBarGroupItem; ItemIndex, GroupIndex: Integer) of object;
  TJvExplorerCustomDrawBarItem = procedure(Sender: TObject; BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer) of object;
  TJvExplorerCustomMeasureBarItem = procedure(Sender: TObject; BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer) of object;

  TJvCustomBarItemViewerClass = class of TJvExplorerCustomBarItemViewer;

  TJvExplorerCustomBarItemViewer = class(TObject)
  private
    FItem: TJvExplorerBarGroupItem;
    FShowClientArea: Boolean;
    function GetExplorerBar: TJvExplorerBar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  protected
    procedure DrawCaption(Bitmap: TBitmap; var X, Y: Integer; var aRect: TRect; dwFlags: Integer); virtual;
    procedure DrawIcon(Bitmap: TBitmap; var X, Y, Width: Integer); virtual;

    procedure HandleCustomDrawBarItem(BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer);
    procedure HandleCustomMeasureBarItem(BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer);
  public
    constructor Create(AItem: TJvExplorerBarGroupItem); virtual;
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); virtual;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); virtual;

    property Item: TJvExplorerBarGroupItem read FItem;
    property ExplorerBar: TJvExplorerBar read GetExplorerBar;
    property ShowClientArea: Boolean read FShowClientArea write FShowClientArea;
  end;

  TJvDefaultBarItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerBarGroup = class;

  { an item - generic class for all items }
  TJvExplorerBarGroupItem = class(TComponent)
  private
    FHeight: Integer;
    FIcon: TBitmap;
    FIconIndex: Integer;
    FIdentifier: Integer;
    FState: TJvExplorerBarItemState;
    FHint: string;
    FWidth: Integer;
    FCaption: string;
    FEnabled: Boolean;
    FExplorerGroup: TJvExplorerBarGroup;
    FIndex: Integer;
    FHotTracking: Boolean;
    FMouseInControl: Boolean;
    FWordWrap: Boolean;
    FFontStyle: TFontStyles;
    FFontColor: TColor;
    FClientAreaRectangle: TRect;
    FItemViewer: TJvExplorerCustomBarItemViewer;
    FLoadedExplorerBar: TJvExplorerBar;
    FLoadedGroupIndex: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetIcon(const Value: TBitmap);
    procedure SetIconIndex(const Value: Integer);
    procedure SetState(const Value: TJvExplorerBarItemState); virtual;
    procedure SetCaption(const Value: string);
    procedure SetMouseInControl(const Value: Boolean); virtual;
    procedure SetFontColor(const Value: TColor);
    procedure SetWordWrap(const Value: Boolean);
    function GetItemRectangle: TRect;

    function GetBarItemViewer: TJvExplorerCustomBarItemViewer;
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure MouseDown(Sender: TObject; X, Y: Integer); virtual;

    function IsEnabled: Boolean; virtual;
    procedure HandleItemClick;

    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; virtual; abstract; // ahuser: AItem parameter not needed, always Self ???
    procedure NotifyPaint;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ItemClick; virtual;
    property MouseInControl: Boolean read FMouseInControl write SetMouseInControl;
    property ExplorerGroup: TJvExplorerBarGroup read FExplorerGroup;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write FWidth;
    property Icon: TBitmap read FIcon write SetIcon;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property Identifier: Integer read FIdentifier write FIdentifier;
    property State: TJvExplorerBarItemState read FState write SetState;
    property Hint: string read FHint write FHint;
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Index: Integer read FIndex write FIndex;
    property HotTracking: Boolean read FHotTracking write FHotTracking;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property FontColor: TColor read FFontColor write SetFontColor;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property ItemRectangle: TRect read GetItemRectangle;

    property ClientAreaRectangle: TRect read FClientAreaRectangle write FClientAreaRectangle; // written by ExplorerBar.Items
  end;

  TJvExplorerBarGroupItems = class(TComponentList)
  private
    FGroup: TJvExplorerBarGroup;
    function GetItem(Index: TJclListSize): TJvExplorerBarGroupItem;
  public
    constructor Create(AGroup: TJvExplorerBarGroup);
    function Add(AItem: TJvExplorerBarGroupItem): TJvExplorerBarGroupItem;
    function Remove(AItem: TJvExplorerBarGroupItem): TJclListSize;
    function IndexOf(AItem: TJvExplorerBarGroupItem): TJclListSize;

    property Items[Index: TJclListSize]: TJvExplorerBarGroupItem read GetItem; default;
    property Group: TJvExplorerBarGroup read FGroup;
  end;

  { a group of items - clickable }
  TJvExplorerBarGroup = class(TCollectionItem)
  private
    FAnimStep: Integer;
    FNbSteps: Integer;
    FDestHeight: Integer;
    FShownHeight: Integer;
    FBackgroundImage: TBitmap;
    FGroupIconIndex: Integer;
    FItems: TJvExplorerBarGroupItems;
    FSpecialGroup: Boolean;
    FState: TJvExplorerGroupState;
    FTimer: TTimer;
    FTitle: string;
    FTitleRect: TRect;
    FBodyRect: TRect;
    FHeight: Integer;
    FWidth: Integer;
    procedure Draw(Bitmap: TBitmap; var X, Y: Integer);
    procedure Measure(Bitmap: TBitmap; var X, Y: Integer); overload;
    procedure Measure; overload;
    procedure SetBackgroundImage(Value: TBitmap);
    procedure SetGroupIconIndex(Value: Integer);
    procedure SetSpecialGroup(Value: Boolean);
    procedure SetTitle(Value: string);
    procedure Timer(Sender: TObject);
    function GetExplorerBar: TJvExplorerBar;
  protected
    procedure NotifyPaint;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure OpenClose;
    procedure Collapse;
    procedure Expand;
    function FindItem(const ATitle, AClassName: string): TJvExplorerBarGroupItem;
    property Items: TJvExplorerBarGroupItems read FItems;
    property ExplorerBar: TJvExplorerBar read GetExplorerBar;
    property Width: Integer read FWidth;
  published
    property BackgroundImage: TBitmap read FBackgroundImage write SetBackgroundImage;
    property SpecialGroup: Boolean read FSpecialGroup write SetSpecialGroup;
    property State: TJvExplorerGroupState read FState write FState;
    property Title: string read FTitle write SetTitle;
    property GroupIconIndex: Integer read FGroupIconIndex write SetGroupIconIndex;
  end;

  { a list of groups }
  TJvExplorerBarGroups = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvExplorerBarGroup;
    function GetExplorerBar: TJvExplorerBar;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    function AddTitle(const ATitle: string; ASpecialGroup: Boolean = False): TJvExplorerBarGroup;
    function Add: TJvExplorerBarGroup;

    property ExplorerBar: TJvExplorerBar read GetExplorerBar;
    property Items[Index: Integer]: TJvExplorerBarGroup read GetItem; default;
  end;

  TJvExplorerBarColourSet = class(TObject)
  private
    FText: TColor;
    FTextHot: TColor;
    FBar: TJvExplorerBar;
    FGroupBackground: TColor;
    FBackground: TColor;
    FSpecialText: TColor;
    FSpecialTextHot: TColor;
    FBorder: TColor;
  public
    constructor Create(ABar: TJvExplorerBar);
    procedure SaveToFile(const AFilename: string);
    procedure LoadFromFile(const AFilename, AThemeName: string);

    property TextHot: TColor read FTextHot write FTextHot;
    property Text: TColor read FText write FText;
    property Border: TColor read FBorder write FBorder;
    property Background: TColor read FBackground write FBackground;
    property GroupBackground: TColor read FGroupBackground write FGroupBackground;
    property SpecialText: TColor read FSpecialText write FSpecialText;
    property SpecialTextHot: TColor read FSpecialTextHot write FSpecialTextHot;
    property Bar: TJvExplorerBar read FBar;
  end;

  TJvExplorerBarThemeElements = class(TObject)
  private
    FBar: TJvExplorerBar;
    FTheme: TJvExplorerTheme;

    FBlueElements: TBitmap;
    FOrangeElements: TBitmap;
    FOliveElements: TBitmap;
    FSilverElements: TBitmap;
    FBlueFlatElements: TBitmap;

    FArrowDown: TBitmap;
    FArrowUp: TBitmap;
    FCheckBoxWidth: Integer;
    FCheckBoxHeight: Integer;

    function GetHeight: Integer;
    function GetThemeBitmap: TBitmap;
    procedure SetTheme(Value: TJvExplorerTheme);
    procedure ThemeChanged;
  protected
    procedure UpdateCheckBoxDimensions;
    procedure DrawMask(Dest: TBitmap; X, Y: Integer; Src: TBitmap);
    function ElementRect(dwType: TJvExplorerThemeElementRectType): TRect;

    property BlueElements: TBitmap read FBlueElements;
    property SilverElements: TBitmap read FSilverElements;
    property OliveElements: TBitmap read FOliveElements;
    property OrangeElements: TBitmap read FOrangeElements;
    property BlueFlatElements: TBitmap read FBlueFlatElements;
    property ArrowUp: TBitmap read FArrowUp;
    property ArrowDown: TBitmap read FArrowDown;
  public
    constructor Create(ABar: TJvExplorerBar);
    destructor Destroy; override;

    procedure DrawThemedHeader(Bitmap, Skin: TBitmap; X, Y, Width: Integer;
      SpecialGroup, Closed, MouseOver: Boolean);
    procedure DrawHeader(Bitmap: TBitmap; X, Y, Width: Integer; SpecialGroup, Closed: Boolean;
      TextColor: TColor);
    procedure BlendFillRect(Dest: TBitmap; aRect: TRect; Alpha: Extended = 1);

    property Bar: TJvExplorerBar read FBar;
    property CheckBoxWidth: Integer read FCheckBoxWidth;
    property CheckBoxHeight: Integer read FCheckBoxHeight;
    property Height: Integer read GetHeight;
    property ThemeBitmap: TBitmap read GetThemeBitmap;
    property Theme: TJvExplorerTheme read FTheme write SetTheme;
  end;

  { a complete bar }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvExplorerBar = class(TScrollingWinControl)
  private
    FAnimStep: Integer;
    FUpdateLock: Integer;
    FAnimate: Boolean;
    FGroups: TJvExplorerBarGroups;
    FGroupIcons: TCustomImageList;
    FGroupIconsChangeLink: TChangeLink;
    FHotArea: TObject;
    FItemIcons: TCustomImageList;
    FItemIconsChangeLink: TChangeLink;
    FNeedResize: Boolean;
    FOnClick: TJvExplorerMenuItemClickEvent;
    FPaintBox: TJvExplorerPaintBox;
    FTimer: TTimer;
    FOldWidth: Integer;
    FGroupsHeight: Integer;
    FHoverGroup: TJvExplorerBarGroup;
    FColourSet: TJvExplorerBarColourSet;
    FMousePosition: TJvExplorerMouseHotTrackPosition;
    FOnItemClick: TJvExplorerItemClickEvent;
    FThemeElements: TJvExplorerBarThemeElements;
    FOnBeforeGroupPaint: TNotifyEvent;
    FOnBeforePaint: TNotifyEvent;
    FOnAfterDrawGroup: TNotifyEvent;
    FOnAfterDraw: TNotifyEvent;
    FOnCustomMeasureBarItem: TJvExplorerCustomMeasureBarItem;
    FOnCustomDrawBarItem: TJvExplorerCustomDrawBarItem;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    function GetHotItem(X, Y: Integer): TObject;
    procedure PaintBuffer(Sender: TObject; Buffer: TBitmap);
    procedure PbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetGroupIcons(Value: TCustomImageList);
    procedure Measure;
    procedure SetItemIcons(Value: TCustomImageList);
    procedure Timer(Sender: TObject);
    function GetThem: TJvExplorerTheme;
    procedure SetTheme(const Value: TJvExplorerTheme);
    procedure SetGroups(const Value: TJvExplorerBarGroups);
  protected
    procedure Resize; override;
    procedure Loaded; override;

    procedure DoItemClick(Item: TJvExplorerBarGroupItem; ItemIndex, GroupIndex: Integer);
    procedure DoCustomDrawBarItem(BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer);
    procedure DoCustomMeasureBarItem(BarItem: TObject; Bitmap: TBitmap; var X, Y, Width: Integer);

    procedure GroupIconsChange(Sender: TObject);
    procedure ItemIconsChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Repaint; override;
    procedure ShowAnimate;
    procedure FocusToGroup(dwIndex: Integer);
    procedure CloseAllGroups;
    function IsUpdating: Boolean;

    property HoverGroup: TJvExplorerBarGroup read FHoverGroup;
    property ColourSet: TJvExplorerBarColourSet read FColourSet;
    property MousePosition: TJvExplorerMouseHotTrackPosition read FMousePosition;
    property HotArea: TObject read FHotArea write FHotArea; // written by ExplorerBar.Items
    property ThemeElements: TJvExplorerBarThemeElements read FThemeElements;
  published
    property Animate: Boolean read FAnimate write FAnimate;
    property GroupIcons: TCustomImageList read FGroupIcons write SetGroupIcons;
    property ItemIcons: TCustomImageList read FItemIcons write SetItemIcons;
    property OnMenuItemClick: TJvExplorerMenuItemClickEvent read FOnClick write FOnClick;
    property OnItemClick: TJvExplorerItemClickEvent read FOnItemClick write FOnItemClick;
    property OnBeforeDrawGroup: TNotifyEvent read FOnBeforeGroupPaint write FOnBeforeGroupPaint;
    property OnBeforeDraw: TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
    property OnAfterDraw: TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnAfterDrawGroup: TNotifyEvent read FOnAfterDrawGroup write FOnAfterDrawGroup;
    property OnCustomDrawBarItem: TJvExplorerCustomDrawBarItem read FOnCustomDrawBarItem write FOnCustomDrawBarItem;
    property OnCustomMeasureBarItem: TJvExplorerCustomMeasureBarItem read FOnCustomMeasureBarItem write FOnCustomMeasureBarItem;
    property Groups: TJvExplorerBarGroups read FGroups write SetGroups;
    property Theme: TJvExplorerTheme read GetThem write SetTheme default etBlue;
    property Align;
    property Anchors;
    {$IFDEF COMPILER14_UP}
    property DoubleBuffered;
    property ParentDoubleBuffered;
    {$ENDIF COMPILER14_UP}
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property HorzScrollBar;
    property VertScrollBar;
  end;

type
  TJvExplorerBarRenderer = class(TObject)
  public
    class procedure DrawAlpha(Dest: TBitmap; BgColor: TColor; X, Y: Integer;
      SrcRect: TRect; Src: TBitmap; Alpha: Extended = 1); {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
  end;

  TJvExplorerBarText = class(TObject)
  public
    class function Parse(Canvas: TCanvas; var Line: string; MaxWidth: Integer): string; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function ClipLine(Canvas: TCanvas; const Line: string; MaxWidth: Integer): string; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function HeightOf(Canvas: TCanvas; const Line: string; MaxWidth: Integer): Integer; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
  end;

implementation

uses
  IniFiles,
  JvJVCLUtils;

{$R JvExplorerBar.res}

{ resize a bitmap }

procedure ResizeBmp(var Src, Dst: TBitmap);
var
  OldStreatchMode: Integer;
begin
  OldStreatchMode := SetStretchBltMode(Dst.Canvas.Handle, HALFTONE);
  Dst.Canvas.CopyRect(Dst.Canvas.ClipRect, Src.Canvas, Src.Canvas.ClipRect);
  SetStretchBltMode(Dst.Canvas.Handle, OldStreatchMode);
end;

{ nb of steps to change height from H1 to H2 }

function CalcNbSteps(H1, H2, Incr: Integer): Integer;
var
  NbSteps: Integer;
  Buf: Integer;
begin
  NbSteps := 1;
  if H1 > H2 then
  begin
    Buf := H1;
    H1 := H2;
    H2 := Buf;
  end;
  while H1 < H2 do
  begin
    Inc(H1, NbSteps);
    Inc(NbSteps, Incr);
  end;
  Result := NbSteps;
end;

{ TJvExplorerBarRenderer }

class procedure TJvExplorerBarRenderer.DrawAlpha(Dest: TBitmap; BgColor: TColor;
  X, Y: Integer; SrcRect: TRect; Src: TBitmap; Alpha: Extended = 1);
var
  I, J: Integer;
  RTmp, OutRect: TRect;
  PSrc, PDest: PByteArray;
  R, G, B: Byte;
begin
  if Alpha >= 1 then // if no alpha blending
    Dest.Canvas.CopyRect(Rect(X, Y, X + SrcRect.Right - SrcRect.Left,
      Y + SrcRect.Bottom - SrcRect.Top), Src.Canvas, SrcRect)
  else
  begin
    Src.PixelFormat := pf24bit;
    Dest.PixelFormat := pf24bit;
    RTmp := Rect(Max(0, X), Max(0, Y), Min(Dest.Width - 1, X + SrcRect.Right - SrcRect.Left - 1),
      Min(Dest.Height - 1, Y + SrcRect.Bottom - SrcRect.Top - 1));
    if IntersectRect(OutRect, RTmp, Dest.Canvas.ClipRect) then
    begin
      R := GetRValue(BgColor);
      G := GetGValue(BgColor);
      B := GetBValue(BgColor);
      for J := OutRect.Top to OutRect.Bottom do
      begin
        PSrc := Src.ScanLine[J - Y + SrcRect.Top];
        PDest := Dest.ScanLine[J];
        for I := OutRect.Left to OutRect.Right do
        begin // blending
          PDest[I * 3] := Round(((1 - Alpha) * B + Alpha * PSrc[(I - X + SrcRect.Left) * 3]));
          PDest[I * 3 + 1] := Round(((1 - Alpha) * G + Alpha * PSrc[(I - X + SrcRect.Left) * 3 + 1]));
          PDest[I * 3 + 2] := Round(((1 - Alpha) * R + Alpha * PSrc[(I - X + SrcRect.Left) * 3 + 2]));
        end;
      end;
    end;
  end;
end;

{ TJvExplorerBarText }

class function TJvExplorerBarText.HeightOf(Canvas: TCanvas; const Line: string; MaxWidth: Integer): Integer;
var
  R: TRect;
begin
  SetRect(R, 0, 0, MaxWidth, MaxWidth);
  DrawText(Canvas.Handle, PChar(Line), Length(Line), R,
    DT_CALCRECT or DT_WORDBREAK or DT_EDITCONTROL or DT_TOP or DT_WORD_ELLIPSIS or DT_MODIFYSTRING);
  Result := R.Bottom; // + Canvas.TextHeight('A');
end;

class function TJvExplorerBarText.Parse(Canvas: TCanvas; var Line: string; MaxWidth: Integer): string;
var //  parse a line to fit in a specified width
  I, Len: Integer;
  Word: string;
  Stop: Boolean;
begin
  Result := '';
  Stop := False;
  while not Stop and (Line <> '') do
  begin
    I := 1;
    Len := Length(Line);
    while (I <= Len) and (Line[I] <> ' ') do
      Inc(I);
    Word := Copy(Line, 1, I);
    if Canvas.TextWidth(Result + Word) < MaxWidth then
    begin
      Line := Copy(Line, I + 1, Length(Line));
      Result := Result + Word;
    end
    else
    begin
      if Result = '' then
      begin
        Line := Copy(Line, I + 1, Length(Line));
        repeat
          Delete(Word, Length(Word), 1);
        until (Canvas.TextWidth(Word + '...') < MaxWidth) or (Length(Word) < 2);
        Result := Word + '...';
      end;
      Stop := True;
    end;
  end;
end;

class function TJvExplorerBarText.ClipLine(Canvas: TCanvas; const Line: string; MaxWidth: Integer): string;
begin
  Result := Line;
  if Canvas.TextWidth(Line) > MaxWidth then
  begin
    repeat
      System.Delete(Result, Length(Result), 1);
    until (Canvas.TextWidth(Result + '...') <= MaxWidth) or
      (Length(Result) < 2);
    Result := Result + '...';
  end;
end;

{ TFlickerFreePaintBox }

constructor TJvExplorerPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  FBuffer := TBitmap.Create;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FBuffer.PixelFormat := pf24bit;
  FBuffer.Canvas.Brush.Color := Color;
  FNeedRepaint := True;
  FDrawing := False;
end;

procedure TJvExplorerPaintBox.Paint;
var
  Bmp: TBitmap;
begin
  if FNeedRepaint then
    Repaint
  else if FApplyDeform then
  begin
    FDrawing := True;
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := FBuffer.Width;
      Bmp.Height := FBuffer.Height;
      TJvExplorerBarRenderer.DrawAlpha(Bmp, Color, 0, 0, FBuffer.Canvas.ClipRect, FBuffer, FDeformAlpha);
      Canvas.CopyRect(Rect(0, 0, Round(FBuffer.Width * FDeformHorz),
        Round(FBuffer.Height * FDeformVert)), Bmp.Canvas, Bmp.Canvas.ClipRect);
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(Round(FBuffer.Width * FDeformHorz), 0, Width - 1, Height - 1));
      Canvas.FillRect(Rect(0, Round(FBuffer.Height * FDeformVert), Width - 1, Height - 1));
    finally
      Bmp.Free;
      FDrawing := False;
    end;
  end
  else
  begin
    FDrawing := True;
    try
      Canvas.Draw(0, 0, FBuffer);
    finally
      FDrawing := False;
    end;
  end;
end;

procedure TJvExplorerPaintBox.Resize;
begin
  if FDrawing then
    Exit;
  FBuffer.Free;
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FBuffer.Canvas.Brush.Color := Color;
  FNeedRepaint := True;
  inherited Resize;
end;

procedure TJvExplorerPaintBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvExplorerPaintBox.Repaint;
begin
  FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);
  if Assigned(FOnFFPaint) then
    FOnFFPaint(Self, FBuffer);
  FNeedRepaint := False;
  Paint;
end;

procedure TJvExplorerPaintBox.ApplyDeform(Horz, Vert, Alpha: Extended);
begin
  FApplyDeform := True;
  FDeformHorz := Horz;
  FDeformVert := Vert;
  FDeformAlpha := Alpha;
  Paint;
end;

procedure TJvExplorerPaintBox.NoDeform;
begin
  FApplyDeform := False;
  Paint;
end;

{ TJvExplorerBarGroupItem }

constructor TJvExplorerBarGroupItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := nil;
  FIconIndex := JvExplorerConstUnknownValue;
  FIdentifier := JvExplorerConstUnknownValue;
  ClientAreaRectangle := JvExplorerConstUnknownRect;
  FEnabled := True;
  FHeight := JvExplorerConstDefaultItemHeight;
  FFontColor := clWindowText;
  FFontStyle := [];
end;

procedure TJvExplorerBarGroupItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // ahuser: we need some information to reconstruct the explorer bar in Loaded()
  //Filer.DefineProperty('ExplorerBar', ReadExplorerBar, WriteExplorerbar, True);
  //Filer.DefineProperty('GroupIndex', ReadGroupIndex, WriteGroupIndex, True);
end;

destructor TJvExplorerBarGroupItem.Destroy;
begin
  FIcon.Free;
  FItemViewer.Free;
  inherited Destroy;
end;

procedure TJvExplorerBarGroupItem.MouseDown(Sender: TObject; X, Y: Integer);
begin
end;

procedure TJvExplorerBarGroupItem.HandleItemClick;
begin
  ExplorerGroup.ExplorerBar.DoItemClick(Self, Index, ExplorerGroup.Index);
end;

procedure TJvExplorerBarGroupItem.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  GetBarItemViewer().Draw(Bitmap, X, Y, Width);
end;

function TJvExplorerBarGroupItem.GetBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  if FItemViewer = nil then
    FItemViewer := CreateBarItemViewer();
  Result := FItemViewer;
end;

function TJvExplorerBarGroupItem.GetItemRectangle: TRect;
begin
  Result := ClientAreaRectangle;
end;

procedure TJvExplorerBarGroupItem.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  GetBarItemViewer().Measure(Bitmap, X, Y, Width);
end;

procedure TJvExplorerBarGroupItem.NotifyPaint;
begin
  if ExplorerGroup <> nil then
    ExplorerGroup.NotifyPaint;
end;

function TJvExplorerBarGroupItem.IsEnabled: Boolean;
begin
  Result := Enabled;
end;

procedure TJvExplorerBarGroupItem.ItemClick;
begin
  HandleItemClick;
end;

procedure TJvExplorerBarGroupItem.Loaded;
begin
  inherited Loaded;
  // ahuser: maybe I should implement them completely different with an own DFM stream
  if FLoadedExplorerBar <> nil then
  begin
    if (FLoadedGroupIndex >= 0) and (FLoadedGroupIndex < FLoadedExplorerBar.Groups.Count) then
      FLoadedExplorerBar.Groups[FLoadedGroupIndex].Items.Add(Self);
    FLoadedGroupIndex := -1;
    FLoadedExplorerBar := nil;
  end;
end;

procedure TJvExplorerBarGroupItem.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetFontColor(const Value: TColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetFontStyle(const Value: TFontStyles);
begin
  if Value <> FFontStyle then
  begin
    FFontStyle := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetHeight(const Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetIcon(const Value: TBitmap);
begin
  if Value <> FIcon then
  begin
    if Value = nil then
    begin
      if FIcon <> nil then
      begin
        FIcon.Free;
        FIcon := nil;
      end;
    end
    else
    begin
      if FIcon = nil then
        FIcon := TBitmap.Create;
      FIcon.Assign(Value);
    end;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetIconIndex(const Value: Integer);
begin
  if Value <> FIconIndex then
  begin
    FIconIndex := Value;
    if ExplorerGroup.ExplorerBar.ItemIcons <> nil then
      NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetMouseInControl(const Value: Boolean);
begin
  FMouseInControl := Value;
end;

procedure TJvExplorerBarGroupItem.SetState(const Value: TJvExplorerBarItemState);
begin
  if Value <> FState then
  begin
    FState := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerBarGroupItem.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    NotifyPaint;
  end;
end;

{ TJvExplorerBarGroup }

constructor TJvExplorerBarGroup.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FItems := TJvExplorerBarGroupItems.Create(Self);
  FState := gsOpen;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := JvExplorerConstAnimationSpeed;
  FTimer.OnTimer := Timer;
  FHeight := 25;
  FDestHeight := FHeight;
  FShownHeight := FHeight;
  FWidth := ExplorerBar.ClientWidth - 2 * JvExplorerConstXOffset;
  FGroupIconIndex := JvExplorerConstUnknownValue;
  FBackgroundImage := nil;
end;

destructor TJvExplorerBarGroup.Destroy;
begin
  if ExplorerBar.FHotArea = Self then
    ExplorerBar.FHotArea := nil;
  FBackgroundImage.Free;
  FItems.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvExplorerBarGroup.Draw(Bitmap: TBitmap; var X, Y: Integer);
var
  I: Integer;
  BodyRgn: HRGN;
  TitleRect: TRect;
begin
  if ExplorerBar.IsUpdating then
    Exit;
  BodyRgn := CreateRectRgn(X, Y, X + FWidth, Y + JvExplorerConstIconSide);
  SelectClipRgn(Bitmap.Canvas.Handle, BodyRgn);
  ExplorerBar.ThemeElements.DrawThemedHeader(Bitmap, ExplorerBar.ThemeElements.ThemeBitmap, X, Y, FWidth,
    FSpecialGroup, FState in [gsClosed, gsClosing], Self = ExplorerBar.HotArea);
  // else
  // ExplorerBar.ThemeElements.DrawHeader(Bitmap, X, Y, FWidth, FSpecialGroup, FState in [gsClosed, gsClosing], IfThen(FSpecialGroup, IfThen(Self <> ExplorerBar.FHotArea, ExplorerBar.ColourSet.SpecialText, ExplorerBar.ColourSet.SpecialTextHot), IfThen(Self <> ExplorerBar.FHotArea, ExplorerBar.ColourSet.Text, ExplorerBar.ColourSet.TextHot)));
  // draw icon and write title
  with Bitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Style := [fsBold];
    Font.Color := IfThen(FSpecialGroup, IfThen(Self <> ExplorerBar.FHotArea,
      ExplorerBar.ColourSet.SpecialText, ExplorerBar.ColourSet.SpecialTextHot),
      IfThen(Self <> ExplorerBar.FHotArea, ExplorerBar.ColourSet.Text,
      ExplorerBar.ColourSet.TextHot));
    if (FGroupIconIndex <> JvExplorerConstUnknownValue) and (ExplorerBar.FGroupIcons <> nil) then
    begin
      ExplorerBar.FGroupIcons.Draw(Bitmap.Canvas, X, Y, FGroupIconIndex);
      TitleRect := Rect(X + 2 * JvExplorerConstIconOffset + JvExplorerConstIconSide,
        Y + JvExplorerConstYOffset + (JvExplorerConstIconSide - JvExplorerConstYOffset - TextHeight('A')) div 2,
        X + FWidth - JvExplorerConstSkinRightPartWidth, Y + JvExplorerConstIconSide);
      DrawText(Handle, PChar(Title), -1, TitleRect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or
        DT_END_ELLIPSIS);
      // TextRect(Rect(X, Y, X + FWidth - SKIN_RIGHT_PART_WIDTH, Y + ICON_SIDE), X + 2 * ICON_OFFSET + ICON_SIDE, Y + Y_OFFSET + (ICON_SIDE - Y_OFFSET - TextHeight('A')) div 2, FTitle);
    end
    else
    begin
      TitleRect := Rect(X + 2 * JvExplorerConstIconOffset,
        Y + JvExplorerConstYOffset + (JvExplorerConstIconSide - JvExplorerConstYOffset - TextHeight('A')) div 2,
        X + FWidth - JvExplorerConstSkinRightPartWidth, Y + JvExplorerConstIconSide);
      DrawText(Handle, PChar(Title), -1, TitleRect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or
        DT_END_ELLIPSIS);
      // TextRect(Rect(X, Y, X + FWidth - SKIN_RIGHT_PART_WIDTH, Y + ICON_SIDE), X + 2 * ICON_OFFSET, Y + Y_OFFSET + (ICON_SIDE - Y_OFFSET - TextHeight('A')) div 2, FTitle);
    end;

  end;
  DeleteObject(BodyRgn);
  FTitleRect := Rect(X, Y + JvExplorerConstIconSide - ExplorerBar.ThemeElements.Height,
    X + FWidth, Y + JvExplorerConstIconSide);
  Inc(Y, JvExplorerConstIconSide);
  FBodyRect.TopLeft := Point(X, Y);
  FBodyRect.Right := X + FWidth;
  if FState <> gsClosed then
  begin
    BodyRgn := CreateRectRgn(X, Y, X + FWidth, Y + FShownHeight);
    try
      SelectClipRgn(Bitmap.Canvas.Handle, BodyRgn);
      // draw body background
      with Bitmap.Canvas do
      begin
        Brush.Color := ExplorerBar.ColourSet.GroupBackground;
        Brush.Style := bsSolid;
        FillRect(Bitmap.Canvas.ClipRect);
        // draw BgImage
        if FBackgroundImage <> nil then
          Draw(X + FWidth - FBackgroundImage.Width, Y + FShownHeight - FBackgroundImage.Height, FBackgroundImage);
        Pen.Color := ExplorerBar.ColourSet.Border;
        Pen.Style := psSolid;
        MoveTo(X, Y);
        LineTo(X, Y + FShownHeight - 1);
        LineTo(X + FWidth - 1, Y + FShownHeight - 1);
        LineTo(X + FWidth - 1, Y);
      end;
      // draw items
      if FState in [gsOpening, gsClosing] then
        Y := Y - FHeight + FShownHeight;
      Inc(Y, JvExplorerConstYOffset);
      for I := 0 to FItems.Count - 1 do
        try
          (FItems[I] as TJvExplorerBarGroupItem).Draw(Bitmap, X, Y, FWidth);
        except
        end;
      Inc(Y, JvExplorerConstYOffset);

      if FState in [gsOpening, gsClosing] then
      begin
        Bitmap.Canvas.Brush.Color := ExplorerBar.ColourSet.Background;
        ExplorerBar.ThemeElements.BlendFillRect(Bitmap, Bitmap.Canvas.ClipRect,
          1 - FShownHeight / FHeight);
      end;
    finally
      DeleteObject(BodyRgn);
    end;
  end;
  FBodyRect.Bottom := Y;
  Inc(Y, JvExplorerConstYOffset);
end;

procedure TJvExplorerBarGroup.Expand;
begin
  State := gsClosed;
  OpenClose;
end;

function TJvExplorerBarGroup.FindItem(const ATitle, AClassName: string): TJvExplorerBarGroupItem;
var
  I: Integer;
  Item: TJvExplorerBarGroupItem;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    Item := Items.GetItem(I);
    if (CompareText(Item.ClassName, AClassName) = 0) and
      (CompareText(Item.Caption, ATitle) = 0) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TJvExplorerBarGroup.GetExplorerBar: TJvExplorerBar;
begin
  Result := TJvExplorerBarGroups(Collection).ExplorerBar;
end;

procedure TJvExplorerBarGroup.Measure(Bitmap: TBitmap; var X, Y: Integer);
var
  I, Bottom: Integer;
begin
  if ExplorerBar.IsUpdating then
    Exit;
  Inc(Y, JvExplorerConstIconSide);
  Bottom := Y;
  Inc(Bottom, JvExplorerConstYOffset);
  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Measure(Bitmap, X, Bottom, FWidth);
  end;

  Inc(Bottom, JvExplorerConstYOffset);
  FHeight := Bottom - Y;
  if (FShownHeight > FHeight) or (FState = gsOpen) then
    FShownHeight := FHeight;
  if FState in [gsOpen, gsOpening] then
    Y := Bottom;
  Inc(Y, JvExplorerConstYOffset);
end;

procedure TJvExplorerBarGroup.Measure;
var
  X, Y: Integer;
begin
  X := 0;
  Y := 0;
  Measure(ExplorerBar.FPaintBox.FBuffer, X, Y);
end;

procedure TJvExplorerBarGroup.NotifyPaint;
begin
  if TJvExplorerBarGroups(Collection).UpdateCount = 0 then
    ExplorerBar.Repaint;
end;

procedure TJvExplorerBarGroup.SetBackgroundImage(Value: TBitmap);
begin
  if Value <> nil then
  begin
    if FBackgroundImage = nil then
      FBackgroundImage := TBitmap.Create;
    FBackgroundImage.Assign(Value);
  end
  else if FBackgroundImage <> nil then
  begin
    FBackgroundImage.Free;
    FBackgroundImage := nil;
  end;
end;

procedure TJvExplorerBarGroup.SetGroupIconIndex(Value: Integer);
begin
  FGroupIconIndex := Value;
  if not ExplorerBar.IsUpdating then
    ExplorerBar.Repaint;
end;

procedure TJvExplorerBarGroup.SetSpecialGroup(Value: Boolean);
begin
  FSpecialGroup := Value;
  if not ExplorerBar.IsUpdating then
    ExplorerBar.Repaint;
end;

procedure TJvExplorerBarGroup.SetTitle(Value: string);
begin
  FTitle := Value;
  if not ExplorerBar.IsUpdating then
    ExplorerBar.Repaint;
end;

procedure TJvExplorerBarGroup.Timer(Sender: TObject);
begin
  case FState of
    gsOpening:
      begin
        FShownHeight := FShownHeight + FAnimStep;
        if FShownHeight > FHeight then
          FShownHeight := FHeight;
        Dec(FAnimStep, 1 + FHeight div 150);
      end;
    gsClosing:
      begin
        FShownHeight := FShownHeight - FAnimStep;
        if FShownHeight < 0 then
          FShownHeight := 0;
        Inc(FAnimStep, 1 + FHeight div 150);
      end;
  end;
  if (FAnimStep >= FNbSteps) or (FAnimStep < 0) then
  begin
    FShownHeight := FDestHeight;
    FTimer.Enabled := False;
    if FShownHeight = 0 then
      FState := gsClosed
    else
      FState := gsOpen;
  end;
  ExplorerBar.Measure;
  ExplorerBar.Repaint;
end;

procedure TJvExplorerBarGroup.Assign(Source: TPersistent);
var
  Src: TJvExplorerBarGroup;
begin
  if Source is TJvExplorerBarGroup then
  begin
    Collection.BeginUpdate;
    try
      Src := TJvExplorerBarGroup(Source);
      SetBackgroundImage(Src.BackgroundImage);
      SpecialGroup := Src.SpecialGroup;
      State := Src.State;
      Title := Src.Title;
      GroupIconIndex := Src.GroupIconIndex;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvExplorerBarGroup.Collapse;
begin
  Self.State := gsOpen;
  Self.OpenClose;
end;

procedure TJvExplorerBarGroup.OpenClose;
var
  DoCalc: Boolean;
begin
  if not ExplorerBar.Animate then
  begin
    case FState of
      gsOpen:
        FState := gsClosed;
      gsClosed:
        FState := gsOpen;
    end;
  end
  else
  begin
    DoCalc := False;
    case FState of
      gsOpen:
        begin
          DoCalc := True;
          FDestHeight := 0;
          FShownHeight := FHeight;
          FState := gsClosing;
          FTimer.Enabled := True;
        end;
      gsClosed:
        begin
          DoCalc := True;
          FDestHeight := FHeight;
          FShownHeight := 0;
          FState := gsOpening;
          FTimer.Enabled := True;
        end;
      gsOpening:
        begin
          FDestHeight := 0;
          FState := gsClosing;
        end;
      gsClosing:
        begin
          FDestHeight := FHeight;
          FState := gsOpening;
        end;
    end;
    if FShownHeight > FHeight then
      FShownHeight := FHeight;
    if DoCalc then
    begin
      FNbSteps := CalcNbSteps(0, FHeight, 1 + FHeight div 150);
      if FState = gsOpening then
        FAnimStep := FNbSteps - 1
      else
        FAnimStep := 0;
    end;
  end;
  ExplorerBar.Measure;
  ExplorerBar.Repaint;
end;

{ TExplorerBarGroups }

constructor TJvExplorerBarGroups.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TJvExplorerBarGroup);
end;

function TJvExplorerBarGroups.Add: TJvExplorerBarGroup;
begin
  Result := TJvExplorerBarGroup(inherited Add);
end;

function TJvExplorerBarGroups.AddTitle(const ATitle: string; ASpecialGroup: Boolean): TJvExplorerBarGroup;
begin
  Result := Add();
  Result.Title := ATitle;
  Result.SpecialGroup := ASpecialGroup;
  ExplorerBar.FNeedResize := True;
end;

function TJvExplorerBarGroups.GetItem(Index: Integer): TJvExplorerBarGroup;
begin
  Result := TJvExplorerBarGroup(inherited GetItem(Index));
end;

function TJvExplorerBarGroups.GetExplorerBar: TJvExplorerBar;
begin
  Result := TJvExplorerBar(inherited Owner);
end;

procedure TJvExplorerBarGroups.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  ExplorerBar.FNeedResize := True;
end;

procedure TJvExplorerBarGroups.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  ExplorerBar.FNeedResize := True;
  ExplorerBar.Repaint;
end;

{ TJvExplorerBar }

constructor TJvExplorerBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColourSet := TJvExplorerBarColourSet.Create(Self);
  FThemeElements := TJvExplorerBarThemeElements.Create(Self);
  FAnimate := True;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks,
    csActionClient];
  Width := JvExplorerConstDefaultWidth;
  FOldWidth := JvExplorerConstDefaultWidth;
  Height := JvExplorerConstDefaultHeight;
  ParentColor := False;
  VertScrollBar.Smooth := True;
  VertScrollBar.Tracking := True;
  VertScrollBar.ButtonSize := JvExplorerConstScollbarWidth;
  VertScrollBar.Visible := True;
  HorzScrollBar.Visible := True;
  HorzScrollBar.Tracking := True;
  HorzScrollBar.Smooth := True;
  AutoScroll := True;
  FPaintBox := TJvExplorerPaintBox.Create(Self);
  FGroups := TJvExplorerBarGroups.Create(Self);
  with FPaintBox do
  begin
    Parent := Self;
    SetBounds(0, 0, Self.Width, 1);
    Resize;
    OnMouseMove := PbMouseMove;
    OnMouseUp := PbMouseUp;
    OnMouseDown := PbMouseDown;
    OnPaint := PaintBuffer;
  end;
  FGroupIcons := nil;
  FItemIcons := nil;
  FNeedResize := True;
  FHotArea := nil;

  FGroupIconsChangeLink := TChangeLink.Create;
  FGroupIconsChangeLink.OnChange := GroupIconsChange;
  FItemIconsChangeLink := TChangeLink.Create;
  FItemIconsChangeLink.OnChange := ItemIconsChange;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := Timer;
  FTimer.Interval := JvExplorerConstAnimationSpeed;
end;

destructor TJvExplorerBar.Destroy;
begin
  SetGroupIcons(nil);
  SetItemIcons(nil);
  FItemIconsChangeLink.Free;
  FGroupIconsChangeLink.Free;

  FPaintBox.Free;
  FGroups.Free;
  FTimer.Free;
  FColourSet.Free;
  FThemeElements.Free;
  inherited Destroy;
end;

procedure TJvExplorerBar.GroupIconsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvExplorerBar.ItemIconsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvExplorerBar.Loaded;
begin
  inherited Loaded;
  FNeedResize := True;
  Repaint;
end;

procedure TJvExplorerBar.CMMouseLeave(var Msg: TMessage);
var
  OldHotArea: TObject;
  DrawX, DrawY: Integer;
begin
  if (HotArea <> nil) then
  begin
    OldHotArea := HotArea;
    FHotArea := nil;
    if (OldHotArea <> nil) and (OldHotArea is TJvExplorerBarGroupItem) then
      OldHotArea := (OldHotArea as TJvExplorerBarGroupItem).ExplorerGroup;
    if OldHotArea <> nil then
      try
        with OldHotArea as TJvExplorerBarGroup do
        begin
          DrawX := FTitleRect.Left;
          DrawY := FTitleRect.Top - JvExplorerConstIconSide + ThemeElements.Height;
          Draw(FPaintBox.FBuffer, DrawX, DrawY);
        end;
      except
      end;
    FPaintBox.Paint;
  end;
end;

function TJvExplorerBar.GetHotItem(X, Y: Integer): TObject;
var
  I, J: Integer;
  Found: Boolean;
begin
  Result := nil;
  FMousePosition := mhtNone;
  Found := False;
  I := 0;
  while not Found and (I < Groups.Count) do
    with Groups[I] do
    begin
      if PtInRect(FTitleRect, Point(X, Y)) then
      begin
        FMousePosition := mhtTitle;
        Result := Groups[I];
        Found := True;
      end
      else
      if PtInRect(FBodyRect, Point(X, Y)) then
      begin
        J := 0;
        FMousePosition := mhtBody;
        while not Found and (J < Items.Count) do
        begin
          if (PtInRect((Items[J]).ClientAreaRectangle, Point(X, Y))) and (Items[J].HotTracking) and
            (Items[J].Enabled) then
          begin
            FMousePosition := mhtItem;
            Result := Items[J];
            Items[J].State := [bisHot];
            Items[J].MouseInControl := True;
            Found := True;
          end;
          Inc(J);
        end;
      end;
      Inc(I);
    end;
end;

procedure TJvExplorerBar.PbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewHotArea: TObject;
  OldHotArea: TObject;
  DrawX, DrawY: Integer;
begin
  if Shift <> [] then
    Exit;
  if not ((HotArea <> nil) and
         ((FHotArea is TJvExplorerBarGroupItem) and PtInRect(TJvExplorerBarGroupItem(FHotArea).ClientAreaRectangle, Point(X, Y))) or
         ((FHotArea is TJvExplorerBarGroup) and PtInRect(TJvExplorerBarGroup(FHotArea).FTitleRect, Point(X, Y)))
     ) then
  begin
    NewHotArea := GetHotItem(X, Y);
    if NewHotArea <> HotArea then
    begin
      if (HotArea is TJvExplorerBarGroupItem) then
      begin
        TJvExplorerBarGroupItem(HotArea).State := [];
        TJvExplorerBarGroupItem(HotArea).MouseInControl := False;
      end;

      OldHotArea := HotArea;
      FHotArea := NewHotArea;
      if (MousePosition = mhtItem) then
      begin
        if (HotArea as TJvExplorerBarGroupItem).Hint <> '' then
        begin
          ShowHint := True;
          Hint := (FHotArea as TJvExplorerBarGroupItem).Hint;
        end;
      end
      else
        ShowHint := False;

      if (OldHotArea <> nil) and (OldHotArea is TJvExplorerBarGroupItem) then
        OldHotArea := (OldHotArea as TJvExplorerBarGroupItem).ExplorerGroup;
      if (NewHotArea <> nil) and (NewHotArea is TJvExplorerBarGroupItem) then
        NewHotArea := (NewHotArea as TJvExplorerBarGroupItem).ExplorerGroup;
      if OldHotArea <> nil then
        with OldHotArea as TJvExplorerBarGroup do
        begin
          DrawX := FTitleRect.Left;
          DrawY := FTitleRect.Top - JvExplorerConstIconSide + ThemeElements.Height;
          Draw(FPaintBox.FBuffer, DrawX, DrawY);
        end;
      if NewHotArea <> nil then
        with NewHotArea as TJvExplorerBarGroup do
        begin
          DrawX := FTitleRect.Left;
          DrawY := FTitleRect.Top - JvExplorerConstIconSide + ThemeElements.Height;
          Draw(FPaintBox.FBuffer, DrawX, DrawY);
        end;
      if FHotArea = nil then
        (Sender as TJvExplorerPaintBox).Cursor := crDefault
      else
        (Sender as TJvExplorerPaintBox).Cursor := crHandPoint;
      FPaintBox.Paint;
    end;
  end;
end;

procedure TJvExplorerBar.PbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NewHotArea: TObject;
begin
  NewHotArea := GetHotItem(X, Y);
  if (NewHotArea = HotArea) and (NewHotArea <> nil) then
  begin
    // if (NewHotArea is TExplorerBarGroupItem) then
    // begin
    // (NewHotArea as TExplorerBarGroupItem).OnItemClick(Self);
    // if Assigned(FOnClick) then fOnClick(Self, (NewHotArea as TExplorerBarGroupItem).Identifier);

    if NewHotArea is TJvExplorerBarGroup then
      TJvExplorerBarGroup(NewHotArea).OpenClose;
  end;

  if HotArea = nil then
    (Sender as TJvExplorerPaintBox).Cursor := crDefault
  else
    (Sender as TJvExplorerPaintBox).Cursor := crHandPoint;
  Repaint;
end;

procedure TJvExplorerBar.PbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NewHotArea: TObject;
begin
  NewHotArea := GetHotItem(X, Y);
  if (NewHotArea = HotArea) and (NewHotArea <> nil) then
    if (NewHotArea is TJvExplorerBarGroupItem) then
    begin
      try
        if TJvExplorerBarGroupItem(NewHotArea).IsEnabled then
        begin
          TJvExplorerBarGroupItem(NewHotArea).MouseDown(Sender, X, Y);
          TJvExplorerBarGroupItem(NewHotArea).ItemClick;
          if Assigned(FOnClick) then
            FOnClick(Self, TJvExplorerBarGroupItem(NewHotArea).Identifier);
        end;
      except
      end;
    end;

  if HotArea = nil then
    (Sender as TJvExplorerPaintBox).Cursor := crDefault
  else
    (Sender as TJvExplorerPaintBox).Cursor := crHandPoint;
  Repaint;
end;

procedure TJvExplorerBar.PaintBuffer(Sender: TObject; Buffer: TBitmap);
var
  I: Integer;
  X, Y: Integer;
begin
  if IsUpdating then
    Exit;
  Buffer.Canvas.Brush.Color := ColourSet.Background;
  Buffer.Canvas.Font.Name := 'Tahoma';
  Buffer.Canvas.FillRect(Buffer.Canvas.ClipRect);
  X := JvExplorerConstXOffset;
  Y := JvExplorerConstYOffset;
  for I := 0 to FGroups.Count - 1 do
  begin
    if Assigned(OnBeforeDrawGroup) then
      OnBeforeDrawGroup(FGroups[I]);
    FGroups[I].Draw(Buffer, X, Y);
    if Assigned(OnAfterDrawGroup) then
      OnAfterDrawGroup(FGroups[I]);
  end;
end;

procedure TJvExplorerBar.SetGroupIcons(Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FGroupIcons, FGroupIconsChangeLink);
end;

procedure TJvExplorerBar.SetGroups(const Value: TJvExplorerBarGroups);
begin
  if Value <> FGroups then
    FGroups.Assign(Value);
end;

procedure TJvExplorerBar.SetItemIcons(Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FItemIcons, FItemIconsChangeLink);
end;

function TJvExplorerBar.GetThem: TJvExplorerTheme;
begin
  Result := ThemeElements.Theme;
end;

procedure TJvExplorerBar.SetTheme(const Value: TJvExplorerTheme);
begin
  if Value <> ThemeElements.Theme then
  begin
    ThemeElements.Theme := Value;
    Repaint;
  end;
end;

procedure TJvExplorerBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = GroupIcons then
      GroupIcons := nil;
    if AComponent = ItemIcons then
      ItemIcons := nil;
  end;
end;

procedure TJvExplorerBar.Measure;
var
  I: Integer;
  X, Y: Integer;
  Closing: Boolean;
  NewPbHeight: Integer;
begin
  // resize FpaintBox & FpaintBox.Buffer
  Y := JvExplorerConstYOffset;
  X := JvExplorerConstXOffset;
  Closing := False;
  for I := 0 to FGroups.Count - 1 do
    with FGroups[I] do
    begin
      Measure(FPaintBox.FBuffer, X, Y);
      if State = gsClosing then
        Closing := True;
    end;
  if Closing and (Y < ClientHeight - 1) then
    NewPbHeight := ClientHeight - 1
  else
    NewPbHeight := Y;
  if NewPbHeight <> FPaintBox.Height then
  begin
    FPaintBox.Height := NewPbHeight;
    FPaintBox.Resize;
  end;
  FGroupsHeight := Y;
  Resize;
end;

procedure TJvExplorerBar.Timer(Sender: TObject);
begin
  Inc(FAnimStep);
  if FAnimStep >= JvExplorerConstAnimatinoCount then
  begin
    FTimer.Enabled := False;
    FPaintBox.NoDeform;
  end
  else
    FPaintBox.ApplyDeform(1, FAnimStep / JvExplorerConstAnimatinoCount,
      (FAnimStep / JvExplorerConstAnimatinoCount) * (FAnimStep / JvExplorerConstAnimatinoCount));
  FPaintBox.Paint;
end;

procedure TJvExplorerBar.Resize;
var
  I: Integer;
  NeedRefresh: Boolean;
begin
  inherited Resize;
  NeedRefresh := False;
  if (ClientHeight >= FGroupsHeight) and VertScrollBar.Visible then
  begin
    VertScrollBar.Visible := False;
    NeedRefresh := True;
  end
  else if (ClientHeight < FGroupsHeight) and not VertScrollBar.Visible then
  begin
    VertScrollBar.Visible := True;
    NeedRefresh := True;
  end;
  if Width <> FOldWidth then
  begin
    FOldWidth := Width;
    FPaintBox.Width := Width;
    FPaintBox.Resize;
    NeedRefresh := True;
  end;
  if NeedRefresh then
  begin
    for I := 0 to FGroups.Count - 1 do
    begin
      FGroups[I].FWidth := ClientWidth - 2 * JvExplorerConstXOffset;
      FGroups[I].Measure;
    end;
    Measure;
    Repaint;
  end;
end;

procedure TJvExplorerBar.DoCustomDrawBarItem(BarItem: TObject; Bitmap: TBitmap;
  var X, Y, Width: Integer);
begin
  if Assigned(FOnCustomDrawBarItem) then
    FOnCustomDrawBarItem(Self, BarItem, Bitmap, X, Y, Width);
end;

procedure TJvExplorerBar.DoCustomMeasureBarItem(BarItem: TObject; Bitmap: TBitmap;
  var X, Y, Width: Integer);
begin
  if Assigned(FOnCustomMeasureBarItem) then
    FOnCustomMeasureBarItem(Self, BarItem, Bitmap, X, Y, Width);
end;

procedure TJvExplorerBar.DoItemClick(Item: TJvExplorerBarGroupItem; ItemIndex, GroupIndex: Integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item, ItemIndex, GroupIndex);
end;

procedure TJvExplorerBar.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TJvExplorerBar.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
    Measure;
end;

function TJvExplorerBar.IsUpdating: Boolean;
begin
  Result := FUpdateLock > 0;
end;

procedure TJvExplorerBar.Repaint;
begin
  if IsUpdating or ([csDestroying, csLoading] * ComponentState <> []) or not HandleAllocated or not Visible then
    Exit;
  inherited Repaint;
  if Assigned(OnBeforeDraw) then
    OnBeforeDraw(Self);
  if FNeedResize then
  begin
    Measure;
    FNeedResize := False;
  end;
  FPaintBox.Repaint;
  if Assigned(OnAfterDraw) then
    OnAfterDraw(Self);
end;

procedure TJvExplorerBar.ShowAnimate;
begin
  if IsUpdating or not Animate then
    Exit;
  FAnimStep := 0;
  if FNeedResize then
  begin
    Measure;
    FNeedResize := False;
  end;
  FTimer.Enabled := True;
  Timer(FTimer);
end;

procedure TJvExplorerBar.FocusToGroup(dwIndex: Integer);
begin
  CloseAllGroups;
  BeginUpdate;
  Groups[dwIndex].OpenClose;
  EndUpdate;
end;

procedure TJvExplorerBar.CloseAllGroups;
var
  I: Integer;
begin
  BeginUpdate;
  for I := 0 to Groups.Count - 1 do
  begin
    if Groups[I].State = gsOpen then
      Groups[I].OpenClose;
  end;
  EndUpdate;
end;

{ TExplorerBarGroupItems }

function TJvExplorerBarGroupItems.Add(AItem: TJvExplorerBarGroupItem): TJvExplorerBarGroupItem;
var
  Index: Integer;
begin
  Index := inherited Add(AItem);
  AItem.FExplorerGroup := Group;
  AItem.Index := Index;
  Result := AItem;
  Group.ExplorerBar.FNeedResize := True;
end;

constructor TJvExplorerBarGroupItems.Create(AGroup: TJvExplorerBarGroup);
begin
  inherited Create(True);
  FGroup := AGroup;
end;

function TJvExplorerBarGroupItems.GetItem(Index: TJclListSize): TJvExplorerBarGroupItem;
begin
  Result := TJvExplorerBarGroupItem(inherited Items[Index]);
end;

function TJvExplorerBarGroupItems.IndexOf(AItem: TJvExplorerBarGroupItem): TJclListSize;
begin
  Result := inherited IndexOf(AItem);
end;

function TJvExplorerBarGroupItems.Remove(AItem: TJvExplorerBarGroupItem): TJclListSize;
begin
  Result := inherited Remove(AItem);
end;

{ TDefaultBarItemView }

procedure TJvDefaultBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
end;

procedure TJvDefaultBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
end;

{ TCustomBarItemViewer }

constructor TJvExplorerCustomBarItemViewer.Create(AItem: TJvExplorerBarGroupItem);
begin
  inherited Create;
  FItem := AItem;
  // FShowClientArea:= True;
end;

procedure TJvExplorerCustomBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  if ShowClientArea then
  begin
    Bitmap.Canvas.Brush.Color := Item.ExplorerGroup.ExplorerBar.ColourSet.TextHot;
    Bitmap.Canvas.FrameRect(Item.ClientAreaRectangle);
    Bitmap.Canvas.Brush.Style := bsClear;
  end;
  Inc(Y, JvExplorerConstYOffset div 2);
end;

procedure TJvExplorerCustomBarItemViewer.DrawCaption(Bitmap: TBitmap; var X, Y: Integer; var aRect: TRect;
  dwFlags: Integer);
var
  DrawFlags: Integer;
begin
  DrawFlags := DT_LEFT;
  if Item.WordWrap then
    DrawFlags := DrawFlags or DT_WORDBREAK or DT_WORD_ELLIPSIS or DT_TOP or DT_EDITCONTROL
  else
    DrawFlags := DrawFlags or DT_SINGLELINE or DT_END_ELLIPSIS;

  // DrawText(Bitmap.Canvas.Handle,PChar(Item.Caption),Length(Item.Caption),aRect,DrawFlags);
  // DrawFlags:= DrawFlags and not DT_CALCRECT;
  DrawText(Bitmap.Canvas.Handle, PChar(Item.Caption), Length(Item.Caption), aRect, DrawFlags);
  Y := aRect.Bottom + (JvExplorerConstYOffset div 2);
  Item.ClientAreaRectangle := aRect;
end;

procedure TJvExplorerCustomBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
//var
//  DrawFlags: Integer;
//  R: TRect;
begin
  Bitmap.Canvas.Font.Style := Item.FontStyle;
  //DrawFlags := DT_LEFT;
  if Item.WordWrap then
  begin
    // SetRect(aRect,0,0,Width,Width);
    // DrawFlags:= DrawFlags or DT_WORDBREAK or DT_WORD_ELLIPSIS or DT_TOP or DT_EDITCONTROL or DT_CALCRECT;
    // DrawText(Bitmap.Canvas.Handle,PChar(Item.Caption),Length(Item.Caption),aRect,DrawFlags);
    // Inc(Y,aRect.Bottom);
    Inc(Y, TJvExplorerBarText.HeightOf(Bitmap.Canvas, Item.Caption, Width) +
      JvExplorerConstIconOffset)
  end
  else
    Inc(Y, Bitmap.Canvas.TextHeight('Yy') + JvExplorerConstIconOffset);
  Inc(Y, JvExplorerConstYOffset div 2);
end;

procedure TJvExplorerCustomBarItemViewer.DrawIcon(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  if (Item.IconIndex <> JvExplorerConstUnknownValue) and
    Assigned(Item.ExplorerGroup.ExplorerBar.ItemIcons) then
  begin
    Item.ExplorerGroup.ExplorerBar.ItemIcons.Draw(Bitmap.Canvas, X + JvExplorerConstXOffset, Y,
      Item.IconIndex);
    X := Item.ExplorerGroup.ExplorerBar.ItemIcons.Width + (JvExplorerConstXOffset * 2) +
      JvExplorerConstIconOffset;
  end
  else
    X := X + (JvExplorerConstXOffset);
end;

function TJvExplorerCustomBarItemViewer.GetExplorerBar: TJvExplorerBar;
begin
  Result := Item.ExplorerGroup.ExplorerBar;
end;

procedure TJvExplorerCustomBarItemViewer.HandleCustomDrawBarItem(BarItem: TObject; Bitmap: TBitmap;
  var X, Y, Width: Integer);
begin
  Item.ExplorerGroup.ExplorerBar.DoCustomDrawBarItem(BarItem, Bitmap, X, Y, Width);
end;

procedure TJvExplorerCustomBarItemViewer.HandleCustomMeasureBarItem(BarItem: TObject; Bitmap: TBitmap;
  var X, Y, Width: Integer);
begin
  Item.ExplorerGroup.ExplorerBar.DoCustomMeasureBarItem(BarItem, Bitmap, X, Y, Width);
end;

{ TExplorerBarColourSet }

constructor TJvExplorerBarColourSet.Create(ABar: TJvExplorerBar);
begin
  inherited Create;
  FBar := ABar;
end;

procedure TJvExplorerBarColourSet.LoadFromFile(const AFilename, AThemeName: string);
{
[Blue]
Name=Blue
Text Hot=16748098
Text=12999969
Border=16777215
Background=15114362
Group Background=16773869
Special Text=16777215
Special Text Hot=16748098

[Olive]
Name=Olive
Text Hot=1938034
Text=2975318
Border=16777215
Background=11262154
Group Background=15529718
Special Text=16777215
Special Text Hot=12117984

[Silver]
Name=Silver
Text Hot=8158334
Text=4013375
Border=16777215
Background=13879235
Group Background=16118256
Special Text=16777215
Special Text Hot=15132390
}
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(AFilename);
  try
    if IniFile.SectionExists(AThemeName) then
    begin
      IniFile.ReadString(AThemeName, 'Name', JvExplorerThemeNames[Bar.ThemeElements.Theme]);
      IniFile.ReadInteger(AThemeName, 'Text Hot', TColor(TextHot));
      IniFile.ReadInteger(AThemeName, 'Text', TColor(Text));
      IniFile.ReadInteger(AThemeName, 'Border', TColor(Border));
      IniFile.ReadInteger(AThemeName, 'Background', TColor(Background));
      IniFile.ReadInteger(AThemeName, 'Group Background', TColor(GroupBackground));
      IniFile.ReadInteger(AThemeName, 'Special Text', TColor(SpecialText));
      IniFile.ReadInteger(AThemeName, 'Special Text Hot', TColor(SpecialTextHot));
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TJvExplorerBarColourSet.SaveToFile(const AFilename: string);
var
  IniFile: TMemIniFile;
  ThemeName: string;
begin
  ThemeName := JvExplorerThemeNames[Bar.ThemeElements.Theme];
  IniFile := TMemIniFile.Create(AFilename);
  try
    IniFile.EraseSection(ThemeName);
    IniFile.WriteString(ThemeName, 'Name', ThemeName);
    IniFile.WriteInteger(ThemeName, 'Text Hot', Integer(TextHot));
    IniFile.WriteInteger(ThemeName, 'Text', Integer(Text));
    IniFile.WriteInteger(ThemeName, 'Border', Integer(Border));
    IniFile.WriteInteger(ThemeName, 'Background', Integer(Background));
    IniFile.WriteInteger(ThemeName, 'Group Background', Integer(GroupBackground));
    IniFile.WriteInteger(ThemeName, 'Special Text', Integer(SpecialText));
    IniFile.WriteInteger(ThemeName, 'Special Text Hot', Integer(SpecialTextHot));
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

{ TExplorerBarThemeElements }

constructor TJvExplorerBarThemeElements.Create(ABar: TJvExplorerBar);
begin
  inherited Create;
  FBar := ABar;

  FTheme := etBlue;

  FBlueElements := TBitmap.Create;
  FOliveElements := TBitmap.Create;
  FSilverElements := TBitmap.Create;
  FOrangeElements := TBitmap.Create;
  FBlueFlatElements := TBitmap.Create;

  FBlueElements.LoadFromResourceName(hInstance, 'THEME_BLUE');
  FOliveElements.LoadFromResourceName(hInstance, 'THEME_OLIVE');
  FSilverElements.LoadFromResourceName(hInstance, 'THEME_SILVER');
  FOrangeElements.LoadFromResourceName(hInstance, 'THEME_ORANGE');
  FBlueFlatElements.LoadFromResourceName(hInstance, 'THEME_BLUE_FLAT');

  FArrowUp := TBitmap.Create;
  FArrowUp.LoadFromResourceName(hInstance, 'ARROW_UP');
  FArrowUp.PixelFormat := pf8bit;

  FArrowDown := TBitmap.Create;
  FArrowDown.LoadFromResourceName(hInstance, 'ARROW_DOWN');
  FArrowDown.PixelFormat := pf8bit;

  ThemeChanged;
  UpdateCheckBoxDimensions;
end;

destructor TJvExplorerBarThemeElements.Destroy;
begin
  BlueElements.Free;
  OliveElements.Free;
  SilverElements.Free;
  OrangeElements.Free;
  BlueFlatElements.Free;
  ArrowDown.Free;
  ArrowUp.Free;
  inherited Destroy;
end;

procedure TJvExplorerBarThemeElements.BlendFillRect(Dest: TBitmap; aRect: TRect; Alpha: Extended);
var
  I, J: Integer;
  OutRect: TRect;
  PDest: PByteArray;
  R, G, B: Byte;
begin
  if Alpha >= 1 then
    Dest.Canvas.FillRect(aRect)
  else if Alpha > 0 then
  begin
    if Dest.PixelFormat <> pf24bit then
      Dest.PixelFormat := pf24bit;
    if IntersectRect(OutRect, aRect, Dest.Canvas.ClipRect) then
    begin
      R := GetRValue(Dest.Canvas.Brush.Color);
      G := GetGValue(Dest.Canvas.Brush.Color);
      B := GetBValue(Dest.Canvas.Brush.Color);
      for J := OutRect.Top to OutRect.Bottom - 1 do
      begin
        PDest := Dest.ScanLine[J];
        for I := OutRect.Left to OutRect.Right - 1 do
        begin // blending
          PDest[I * 3] := Round(((1 - Alpha) * PDest[I * 3] + Alpha * B));
          PDest[I * 3 + 1] := Round(((1 - Alpha) * PDest[I * 3 + 1] + Alpha * G));
          PDest[I * 3 + 2] := Round(((1 - Alpha) * PDest[I * 3 + 2] + Alpha * R));
        end;
      end;
    end;
  end;
end;

procedure TJvExplorerBarThemeElements.DrawHeader(Bitmap: TBitmap; X, Y, Width: Integer;
  SpecialGroup, Closed: Boolean; TextColor: TColor);
begin
  with Bitmap.Canvas do
  begin
    Brush.Color := IfThen(SpecialGroup, clActiveCaption, clBtnFace);
    FillRect(Rect(X, Y + JvExplorerConstIconSide - Height, X + Width, Y + JvExplorerConstIconSide));
    Brush.Color := TextColor;
    if Closed then
      DrawMask(Bitmap, Width - JvExplorerConstXOffset, Y + 2 * JvExplorerConstYOffset, ArrowDown)
    else
      DrawMask(Bitmap, Width - JvExplorerConstXOffset, Y + 2 * JvExplorerConstYOffset, ArrowUp);
  end;
end;

procedure TJvExplorerBarThemeElements.DrawMask(Dest: TBitmap; X, Y: Integer; Src: TBitmap);
var
  I, J: Integer;
  X2, Y2: Integer;
  PSrc: PByteArray;
begin
  if (Y >= Dest.Height) or (X >= Dest.Width) then
    Exit;
  Y2 := Min(Src.Height, Dest.Height - Y) - 1;
  X2 := Min(Src.Width, Dest.Width - X) - 1;
  for J := 0 to Y2 do
  begin
    PSrc := Src.ScanLine[J];
    for I := 0 to X2 do
      if PSrc[I] = 0 then
        Dest.Canvas.Pixels[X + I, Y + J] := Dest.Canvas.Brush.Color;
  end;
end;

procedure TJvExplorerBarThemeElements.DrawThemedHeader(Bitmap, Skin: TBitmap; X, Y, Width: Integer;
  SpecialGroup, Closed, MouseOver: Boolean);
begin
  SetStretchBltMode(Bitmap.Canvas.Handle, COLORONCOLOR);
  if SpecialGroup then
  begin
    BitBlt(Bitmap.Canvas.Handle, X, Y + JvExplorerConstIconSide - Height,
      JvExplorerConstSkinLeftPartWidth, Height, Skin.Canvas.Handle,
      ElementRect(terSpecialTitleLeft).Left, ElementRect(terSpecialTitleLeft).Top, SRCCOPY);
    StretchBlt(Bitmap.Canvas.Handle,
      X + JvExplorerConstSkinLeftPartWidth,
      Y + JvExplorerConstIconSide - Height,
      Width - JvExplorerConstSkinLeftPartWidth - JvExplorerConstSkinRightPartWidth,
      Height,
      Skin.Canvas.Handle,
      ElementRect(terSpecialTitleCenter).Left, ElementRect(terSpecialTitleCenter).Top,
      ElementRect(terSpecialTitleCenter).Right - ElementRect(terSpecialTitleCenter).Left,
      ElementRect(terSpecialTitleCenter).Bottom - ElementRect(terSpecialTitleCenter).Top, SRCCOPY);
    BitBlt(Bitmap.Canvas.Handle,
      X + Width - JvExplorerConstSkinRightPartWidth,
      Y + JvExplorerConstIconSide - Height, JvExplorerConstSkinRightPartWidth, Height,
      Skin.Canvas.Handle, IfThen(Closed, IfThen(MouseOver, ElementRect(terSpecialTitleRightDownOn).Left,
      ElementRect(terSpecialTitleRightDownOff).Left),
      IfThen(MouseOver, ElementRect(terSpecialTitleRightUpOn).Left, ElementRect(terSpecialTitleRightUpOff).Left)),
      IfThen(Closed, IfThen(MouseOver, ElementRect(terSpecialTitleRightDownOn).Top, ElementRect(terSpecialTitleRightDownOff).Top),
      IfThen(MouseOver, ElementRect(terSpecialTitleRightUpOn).Top, ElementRect(terSpecialTitleRightUpOff).Top)),
      SRCCOPY);
  end
  else
  begin
    BitBlt(Bitmap.Canvas.Handle, X, Y + JvExplorerConstIconSide - Height,
      JvExplorerConstSkinLeftPartWidth, Height, Skin.Canvas.Handle, ElementRect(terTitleLeft).Left,
      ElementRect(terTitleLeft).Top, SRCCOPY);
    StretchBlt(Bitmap.Canvas.Handle,
      X + JvExplorerConstSkinLeftPartWidth,
      Y + JvExplorerConstIconSide - Height,
      Width - JvExplorerConstSkinLeftPartWidth - JvExplorerConstSkinRightPartWidth,
      Height,
      Skin.Canvas.Handle,
      ElementRect(terTitleCenter).Left, ElementRect(terTitleCenter).Top,
      ElementRect(terTitleCenter).Right - ElementRect(terTitleCenter).Left,
      ElementRect(terTitleCenter).Bottom - ElementRect(terTitleCenter).Top, SRCCOPY);
    BitBlt(Bitmap.Canvas.Handle,
      X + Width - JvExplorerConstSkinRightPartWidth,
      Y + JvExplorerConstIconSide - Height, JvExplorerConstSkinRightPartWidth, Height,
      Skin.Canvas.Handle,
      IfThen(Closed, IfThen(MouseOver, ElementRect(terTitleRightDownOn).Left, ElementRect(terTitleRightDownOff).Left),
      IfThen(MouseOver, ElementRect(terTitleRightUpOn).Left, ElementRect(terTitleRightUpOff).Left)),
      IfThen(Closed, IfThen(MouseOver, ElementRect(terTitleRightDownOn).Top, ElementRect(terTitleRightDownOff).Top),
      IfThen(MouseOver, ElementRect(terTitleRightUpOn).Top, ElementRect(terTitleRightUpOff).Top)),
      SRCCOPY);
  end;
end;

function TJvExplorerBarThemeElements.ElementRect(dwType: TJvExplorerThemeElementRectType): TRect;
begin
  case dwType of
    terTitleLeft:
      Result := Rect(7, 1, 12, 24);
    terTitleCenter:
      Result := Rect(13, 1, 163, 24);
    terTitleRightUpOff:
      Result := Rect(164, 1, 189, 24);
    terTitleRightUpOn:
      Result := Rect(190, 1, 215, 24);
    terTitleRightDownOff:
      Result := Rect(216, 1, 241, 24);
    terTitleRightDownOn:
      Result := Rect(242, 1, 267, 24);
    terSpecialTitleLeft:
      Result := Rect(7, 25, 12, 48);
    terSpecialTitleCenter:
      Result := Rect(13, 25, 163, 48);
    terSpecialTitleRightUpOff:
      Result := Rect(164, 25, 189, 48);
    terSpecialTitleRightUpOn:
      Result := Rect(190, 25, 215, 48);
    terSpecialTitleRightDownOff:
      Result := Rect(216, 25, 241, 48);
    terSpecialTitleRightDownOn:
      Result := Rect(242, 25, 267, 48);
  end;
end;

function TJvExplorerBarThemeElements.GetHeight: Integer;
begin
  Result := 23;
end;

function TJvExplorerBarThemeElements.GetThemeBitmap: TBitmap;
begin
  case Theme of
    etBlue:
      Result := BlueElements;
    etSilver:
      Result := SilverElements;
    etOlive:
      Result := OliveElements;
    etOrange:
      Result := OrangeElements;
    etBlueFlat:
      Result := BlueFlatElements;
  else
    Result := BlueElements;
  end;
end;

procedure TJvExplorerBarThemeElements.SetTheme(Value: TJvExplorerTheme);
begin
  if Value <> FTheme then
  begin
    FTheme := Value;
    ThemeChanged;
  end;
end;

procedure TJvExplorerBarThemeElements.ThemeChanged;
begin
  Bar.ColourSet.Background := ThemeBitmap.Canvas.Pixels[1, 1];
  Bar.ColourSet.GroupBackground := ThemeBitmap.Canvas.Pixels[1, 7];
  Bar.ColourSet.Border := ThemeBitmap.Canvas.Pixels[1, 13];
  Bar.ColourSet.Text := ThemeBitmap.Canvas.Pixels[1, 19];
  Bar.ColourSet.TextHot := ThemeBitmap.Canvas.Pixels[1, 25];
  Bar.ColourSet.SpecialText := ThemeBitmap.Canvas.Pixels[1, 31];
  Bar.ColourSet.SpecialTextHot := ThemeBitmap.Canvas.Pixels[1, 37];
  Bar.Color := Bar.ColourSet.Background;
  Bar.Repaint;
  // Bar.ColourSet.Background:= clWhite; //clWindow;
  // Bar.ColourSet.GroupBackground:= clWindow;
  // Bar.ColourSet.Border:= clBtnFace;
  // Bar.ColourSet.Text:= clBtnText;
  // Bar.ColourSet.TextHot:= clBtnText;
  // Bar.ColourSet.SpecialText:= clCaptionText;
  // Bar.ColourSet.SpecialTextHot:= clCaptionText;
  // Bar.Color := clWhite; //clWindow;
end;

procedure TJvExplorerBarThemeElements.UpdateCheckBoxDimensions;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(32759));
      FCheckBoxWidth := Width div 4;
      FCheckBoxHeight := Height div 3;
    finally
      Free;
    end;
end;

end.
