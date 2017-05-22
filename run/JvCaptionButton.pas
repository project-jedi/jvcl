{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionButton.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.

This unit is a merging of the original TJvCaptionButton, TJvaCaptionButton.
Merging done 2003-06-12 by Remko Bonte [remkobonte at myrealbox dot com]

All Rights Reserved.

Contributor(s):
  Andrei Prygounkov <a dot prygounkov at gmx dot de>, author of TJvaCaptionButton.
  Remko Bonte [remkobonte at myrealbox dot com], theme support, actions
  Olivier Sannier [obones att altern dott org], caption hints.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Modified 2003-06-13 (p3):
- Fixed MouseUp X,Y inconsistentcy (did not report the same values as MouseDown)
- Added MouseMove handler
- Added ShowHint, ParentShowHint
- Fixed drawing of disabled MinimizeToTray icon as well as incorrect Font.Color in text drawing
- Added Assign
- Tested on W2k
- Demo (examples\CaptionBtn) updated and extended

Known Issues:

  * Msimg32.dll code should be moved to seperate import unit. Code is partly
    copied from JwaWinGDI.pas.
  * Button can disappear at design-time when switching themes.
  * With more buttons, button can appear hot while mouse is over another caption
    button.
  * Still some flicker while resizing due to wrong FButtonRect, see comment
    at HandleNCPaintBefore.
  * Buttons on small caption (BorderStyle in [bsSizeToolWin, bsToolWin]) looks
    ugly.

-----------------------------------------------------------------------------}
// $Id$

unit JvCaptionButton;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, Types,
  ActnList, ImgList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvComponentBase, JvTypes;

type
  TJvStandardButton = (tsbNone, tsbClose, tsbHelp, tsbMax, tsbMin, tsbRestore,
    tsbMinimizeToTray); // a la e-Mule
  TJvCaptionButtonLayout = (cbImageLeft, cbImageRight);
  TJvRedrawKind = (rkDirect, rkIndirect, rkTotalCaptionBar);

  TJvCaptionButton = class;

  TJvCaptionButtonActionLink = class(TActionLink)
  protected
    FClient: TJvCaptionButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;

    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TJvCaptionButtonActionLinkClass = class of TJvCaptionButtonActionLink;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvCaptionButton = class(TJvComponent)
  private
    { Properties }
    FAlignment: TAlignment;
    FHeight: Integer;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FCaption: string;
    FDown: Boolean;
    FEnabled: Boolean;
    FFont: TFont;
    FHint: string;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FLayout: TJvCaptionButtonLayout;
    FMargin: Integer;
    FPosition: Integer;
    FSpacing: Integer;
    FStandard: TJvStandardButton;
    FToggle: Boolean;
    FVisible: Boolean;
    FOnClick: TNotifyEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;

    FDefaultButtonLeft: Integer;
    FDefaultButtonTop: Integer;
    FDefaultButtonWidth: Integer;
    FDefaultButtonHeight: Integer;

    FActionLink: TJvCaptionButtonActionLink;
    FBuffer: TBitmap;
    FButtonRect: TRect;
    FCaptionHeight: Integer;
    FClickRect: TRect; // Clickable area is a bit bigger than the button
    FHasCaption: Boolean; // True, if the form has a caption
    FHasSmallCaption: Boolean; // True, if the form has BorderStyle bsToolWindow, bsSizeToolWin
    FImageChangeLink: TChangeLink;
    FMouseButtonDown: Boolean;
    FMouseInControl: Boolean;
    FNeedRecalculate: Boolean;
    FRgnChanged: Boolean;
    FSaveRgn: HRGN;
    FShowHint: Boolean;
    FParentShowHint: Boolean;
    {tool tip specific}
    FToolTipHandle: THandle;
    {tool tip specific end}
    FCurrentWindowState: TWindowState;

    {$IFDEF JVCLThemesEnabled}
    FCaptionActive: Boolean;
    FForceDrawSimple: Boolean;
    FForceRedraw: Boolean;
    function GetIsThemed: Boolean;
    procedure SetForceDrawSimple(const Value: Boolean);
    {$ENDIF JVCLThemesEnabled}
    function GetAction: TBasicAction;
    function GetIsImageVisible: Boolean;
    function GetParentForm: TCustomForm;
    function GetParentFormHandle: THandle;
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    procedure SetAction(const Value: TBasicAction);
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(Value: string);
    procedure SetDown(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeight(Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetLayout(const Value: TJvCaptionButtonLayout);
    procedure SetLeft(Value: Integer);
    procedure SetMargin(const Value: Integer);
    procedure SetMouseInControl(const Value: Boolean);
    procedure SetPosition(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetStandard(Value: TJvStandardButton);
    procedure SetToggle(const Value: Boolean);
    procedure SetTop(Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(Value: Integer);

    {tool tip handling}
    procedure CreateToolTip(Wnd: THandle);
    procedure DestroyToolTip;
    procedure HideToolTip;
    procedure ForwardToToolTip(Msg: TMessage);

    procedure Hook;
    procedure UnHook;
    function WndProcAfter(var Msg: TMessage): Boolean;
    function WndProcBefore(var Msg: TMessage): Boolean;

    procedure DrawButton(DC: HDC);
    {$IFDEF JVCLThemesEnabled}
    procedure DrawButtonBackground(ACanvas: TCanvas);
    {$ENDIF JVCLThemesEnabled}
    procedure DrawStandardButton(ACanvas: TCanvas);
    procedure DrawNonStandardButton(ACanvas: TCanvas);
    procedure DrawButtonImage(ACanvas: TCanvas; ImageBounds: TRect);
    procedure DrawButtonText(ACanvas: TCanvas; TextBounds: TRect);

    procedure Redraw(const AKind: TJvRedrawKind);
    procedure CalcDefaultButtonRect(Wnd: THandle);

    {Paint related messages}
    procedure HandleNCActivate(var Msg: TWMNCActivate);
    procedure HandleNCPaintAfter(Wnd: THandle; var Msg: TWMNCPaint);
    procedure HandleNCPaintBefore(Wnd: THandle; var Msg: TWMNCPaint);
    {Mouse down-related messages}
    function HandleButtonDown(var Msg: TWMNCHitMessage): Boolean;
    function HandleButtonUp(var Msg: TWMNCHitMessage): Boolean;
    function HandleHitTest(var Msg: TWMNCHitTest): Boolean;
    function HandleMouseMove(var Msg: TWMNCHitMessage): Boolean;
    procedure HandleNCMouseMove(var Msg: TWMNCHitMessage);
    {Other}
    function HandleNotify(var Msg: TWMNotify): Boolean;

    procedure ImageListChange(Sender: TObject);
    procedure DoActionChange(Sender: TObject);

    function MouseOnButton(X, Y: Integer; const TranslateToScreenCoord: Boolean): Boolean;
    procedure SetParentShowHint(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure CalcButtonParts(ACanvas: TCanvas; ButtonRect: TRect; var RectText, RectImage: TRect);
    function GetActionLinkClass: TJvCaptionButtonActionLinkClass; dynamic;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateButtonRect(Wnd: THandle);

    property ActionLink: TJvCaptionButtonActionLink read FActionLink write FActionLink;
    property IsImageVisible: Boolean read GetIsImageVisible;
    {$IFDEF JVCLThemesEnabled}
    // The value of IsThemed stays the same until a WM_THEMECHANGED is received.
    property IsThemed: Boolean read GetIsThemed;
    {$ENDIF JVCLThemesEnabled}
    property MouseInControl: Boolean read FMouseInControl;
    property ParentFormHandle: THandle read GetParentFormHandle;
    property ParentForm: TCustomForm read GetParentForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitiateAction; virtual;
    procedure ResetButton;
    procedure Click; dynamic;

    property DefaultButtonWidth: Integer read FDefaultButtonWidth;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonHeight: Integer read FHeight write SetHeight default 0;
    property ButtonLeft: Integer read FLeft write SetLeft default 0;
    property ButtonTop: Integer read FTop write SetTop default 0;
    property ButtonWidth: Integer read FWidth write SetWidth default 0;
    property Caption: string read FCaption write SetCaption stored IsCaptionStored;
    property Down: Boolean read FDown write SetDown default False;
    {$IFDEF JVCLThemesEnabled}
    property ForceDrawSimple: Boolean read FForceDrawSimple write SetForceDrawSimple default False;
    {$ENDIF JVCLThemesEnabled}
    property ShowHint: Boolean read FShowHint write SetShowHint default False;
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored default True;
    property Font: TFont read FFont write SetFont;
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Layout: TJvCaptionButtonLayout read FLayout write SetLayout default cbImageLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Position: Integer read FPosition write SetPosition default 0;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Standard: TJvStandardButton read FStandard write SetStandard default tsbNone;
    property Toggle: Boolean read FToggle write SetToggle default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

function TransparentBlt(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, hHeightDest: Integer;
  hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
  crTransparent: UINT): BOOL; stdcall;
function AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
  nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
  nHeightSrc: Integer; BlendFunction: BLENDFUNCTION): BOOL; stdcall;

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
  CommCtrl, Buttons, SysUtils,
  JvThemes,
  {$IFDEF JVCLThemesEnabled}
  UxTheme,
  {$IFNDEF COMPILER7_UP}
  TmSchema,
  {$ENDIF !COMPILER7_UP}
  {$ENDIF JVCLThemesEnabled}
  JvDsgnIntf, JvJCLUtils, JvResources, JvWndProcHook, JvJVCLUtils;

const
  { Msimg32.dll is included in Windows 98 and later }
  Msimg32DLLName = 'Msimg32.dll';

  TransparentBltName = 'TransparentBlt';
  AlphaBlendName = 'AlphaBlend';

  htCaptionButton = HTSIZELAST + 1;
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  GMsimg32Handle: THandle = 0;
  GTriedLoadMsimg32Dll: Boolean = False;

  _AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
    nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
    nHeightSrc: Integer; BlendFunction: BLENDFUNCTION): BOOL; stdcall;
  _TransparentBlt: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, hHeightDest: Integer;
    hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
    crTransparent: UINT): BOOL; stdcall;

{$IFDEF JVCLThemesEnabled}

type
  { (rb) I couldn't get the alpha channel to work with the normal TBitmap so
         introduced TAlphaBitmap. TBitmapAdapter hides the implementation details
         of the TBitmap/TAlphaBitmap }

  TAlphaBitmap = class(TObject)
  private
    FHandle: HDC;
    FBitmapInfo: TBitmapInfo;
    FDIBHandle: HBitmap;
    FOldBitmap: HBitmap;
    FBitsMem: Pointer;
    FBitCount: Byte;
    FHasAlphaChannel: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    procedure CreateHandle(AWidth, AHeight: Integer);
    function CreateDIB(ADC: HDC; AWidth, AHeight: Integer): HBitmap;
    procedure Duplicate(Src: HBitmap);
    procedure FreeHandle;
    procedure InitAlpha;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);

    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);

    property Handle: HDC read FHandle;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Data: Pointer read FBitsMem;
    property BitCount: Byte read FBitCount;
    property HasAlphaChannel: Boolean read FHasAlphaChannel;
  end;

  TBitmapAdapter = class(TObject)
  private
    FBitmap: TObject;
    FMargins: TMargins;
    FTransparentColor: TColorRef;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetIsValid: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent);

    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);

    function Draw(ACanvas: TCanvas; const Rect: TRect; AMargins: PMargins): Boolean;
    function DrawFixed(ACanvas: TCanvas; const X, Y: Integer): Boolean;
    function DrawFixedPart(ACanvas: TCanvas; const DestRect: TRect; const SrcX, SrcY: Integer): Boolean;
    function DrawPart(ACanvas: TCanvas; const SrcRect, DestRect: TRect; AMargins: PMargins): Boolean;

    property Margins: TMargins read FMargins write FMargins;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property IsValid: Boolean read GetIsValid;
    property TransparentColor: TColorRef read FTransparentColor write FTransparentColor;
  end;

  TGlobalXPData = class(TObject)
  private
    FCaptionButtonHeight: Integer;
    FCaptionButtonCount: Integer;
    FCaptionButtons: TBitmapAdapter;

    FIsThemed: Boolean;
    FBitmapValid: Boolean;
    FClientCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddClient;
    procedure RemoveClient;

    procedure Update;
    procedure DrawSimple(ACanvas: TCanvas; State: Integer; const DrawRect: TRect);
    function Draw(ACanvas: TCanvas; State: Integer; const DrawRect: TRect): Boolean;
    property IsThemed: Boolean read FIsThemed;
  end;

var
  GGlobalXPData: TGlobalXPData;

//=== Local procedures =======================================================

function IsVistaOrNewer: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and CheckWin32Version(6, 0);
end;

function GlobalXPData: TGlobalXPData;
begin
  if not Assigned(GGlobalXPData) then
    GGlobalXPData := TGlobalXPData.Create;

  Result := GGlobalXPData;
end;

function TranslateBitmapFileName(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    case S[I] of
      'A'..'Z', '0'..'9':
        {do nothing};
      'a'..'z':
        Result[I] := UpCase(S[I]);
    else
      Result[I] := '_';
    end;
end;

procedure DupBits(Src, Dst: HBitmap; Size: TPoint);
var
  MemDC: HDC;
  DesktopDC: HDC;
  OldBitmap: HBitmap;
begin
  OldBitmap := 0;
  DesktopDC := GetDC(GetDesktopWindow);
  MemDC := CreateCompatibleDC(DesktopDC);
  try
    OldBitmap := SelectObject(MemDC, Src);
    BitBlt(Dst, 0, 0, Size.X, Size.Y, MemDC, 0, 0, SRCCOPY);
  finally
    SelectObject(MemDC, OldBitmap);
    ReleaseDC(GetDesktopWindow, DesktopDC);
    DeleteDC(MemDC);
  end;
end;

function GetHasAlphaChannel(Data: PRGBQuad; Count: Integer): Boolean;
begin
  Result := False;

  while Count > 0 do
  begin
    Result := Data.rgbReserved <> 0;
    if Result then
      Break;
    Inc(Data);
    Dec(Count);
  end;
end;

procedure PreMultiplyAlphaChannel(Data: PRGBQuad; Count: Integer);
begin
  while Count > 0 do
  begin
    with Data^ do
    begin
      rgbBlue := (rgbBlue * rgbReserved + 128) div 255;
      rgbGreen := (rgbGreen * rgbReserved + 128) div 255;
      rgbRed := (rgbRed * rgbReserved + 128) div 255;
    end;
    Inc(Data);
    Dec(Count);
  end;
end;

{$ENDIF JVCLThemesEnabled}

procedure UnloadMsimg32Dll;
begin
  @_TransparentBlt := nil;
  @_AlphaBlend := nil;
  if GMsimg32Handle > 0 then
    FreeLibrary(GMsimg32Handle);
  GMsimg32Handle := 0;
end;

procedure LoadMsimg32Dll;
begin
  GTriedLoadMsimg32Dll := True;
  GMsimg32Handle := SafeLoadLibrary(Msimg32DLLName);
  if GMsimg32Handle <> 0 then
  begin
    @_TransparentBlt := GetProcAddress(GMsimg32Handle, TransparentBltName);
    @_AlphaBlend := GetProcAddress(GMsimg32Handle, AlphaBlendName);
  end;
end;

{$IFDEF JVCLThemesEnabled}

function TransparentBltStretch(DestDC: HDC; const DestRect: TRect;
  SourceDC: HDC; const SourceRect: TRect; const SizingMargins: TMargins;
  const TransparentColor: TColor): Boolean;
var
  ESourceWidth, ESourceHeight: Integer;
  EDestWidth, EDestHeight: Integer;
  LastOriginSource: TPoint;
  LastOriginDest: TPoint;
begin
  {                Source                           Dest

               |--------------|             |--------------------|
               | A |   B  | C |             | A |      B     | C |
               |-- |------|---|             |-- |------------|---|
               |   |      |   |             |   |            |   |
               | D |   E  | F |             |   |            |   |
               |   |      |   |     =>      | D |      E     | F |
               |---|------|---|             |   |            |   |
               | G |   H  | I |             |   |            |   |
               |--------------|             |--------------------|
                                            | G |      H     | I |
                                            |-- |------------|---|
  }
  ESourceWidth := SourceRect.Right - SourceRect.Left - SizingMargins.cxLeftWidth - SizingMargins.cxRightWidth;
  ESourceHeight := SourceRect.Bottom - SourceRect.Top - SizingMargins.cyTopHeight - SizingMargins.cyBottomHeight;
  EDestWidth := DestRect.Right - DestRect.Left - SizingMargins.cxLeftWidth - SizingMargins.cxRightWidth;
  EDestHeight := DestRect.Bottom - DestRect.Top - SizingMargins.cyTopHeight - SizingMargins.cyBottomHeight;

  GetWindowOrgEx(SourceDC, LastOriginSource);
  SetWindowOrgEx(SourceDC, LastOriginSource.X - SourceRect.Left, LastOriginSource.Y - SourceRect.Top, nil);
  GetWindowOrgEx(DestDC, LastOriginDest);
  SetWindowOrgEx(DestDC, LastOriginDest.X - DestRect.Left, LastOriginDest.Y - DestRect.Top, nil);

  Result :=
    { A }
  TransparentBlt(
    DestDC,
    0, 0, SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight,
    SourceDC,
    0, 0, SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight,
    TransparentColor
    ) and

  { B }
  TransparentBlt(
    DestDC,
    SizingMargins.cxLeftWidth, 0, EDestWidth, SizingMargins.cyTopHeight,
    SourceDC,
    SizingMargins.cxLeftWidth, 0, ESourceWidth, SizingMargins.cyTopHeight,
    TransparentColor
    ) and

  { C }
  TransparentBlt(
    DestDC,
    EDestWidth + SizingMargins.cxLeftWidth, 0, SizingMargins.cxRightWidth, SizingMargins.cyTopHeight,
    SourceDC,
    ESourceWidth + SizingMargins.cxLeftWidth, 0, SizingMargins.cxRightWidth, SizingMargins.cyTopHeight,
    TransparentColor
    ) and

  { D }
  TransparentBlt(
    DestDC,
    0, SizingMargins.cyTopHeight, SizingMargins.cxLeftWidth, EDestHeight,
    SourceDC,
    0, SizingMargins.cyTopHeight, SizingMargins.cxLeftWidth, ESourceHeight,
    TransparentColor
    ) and

  { E }
  TransparentBlt(
    DestDC,
    SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight, EDestWidth, EDestHeight,
    SourceDC,
    SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight, ESourceWidth, ESourceHeight,
    TransparentColor
    ) and

  { F }
  TransparentBlt(
    DestDC,
    EDestWidth + SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight, SizingMargins.cxRightWidth, EDestHeight,
    SourceDC,
    ESourceWidth + SizingMargins.cxLeftWidth, SizingMargins.cyTopHeight, SizingMargins.cxRightWidth, ESourceHeight,
    TransparentColor
    ) and

  { G }
  TransparentBlt(
    DestDC,
    0, EDestHeight + SizingMargins.cyTopHeight, SizingMargins.cxLeftWidth, SizingMargins.cyBottomHeight,
    SourceDC,
    0, ESourceHeight + SizingMargins.cyTopHeight, SizingMargins.cxLeftWidth, SizingMargins.cyBottomHeight,
    TransparentColor
    ) and

  { H }
  TransparentBlt(
    DestDC,
    SizingMargins.cxLeftWidth, EDestHeight + SizingMargins.cyTopHeight, EDestWidth, SizingMargins.cyBottomHeight,
    SourceDC,
    SizingMargins.cxLeftWidth, ESourceHeight + SizingMargins.cyTopHeight, ESourceWidth, SizingMargins.cyBottomHeight,
    TransparentColor
    ) and

  { I }
  TransparentBlt(
    DestDC,
    EDestWidth + SizingMargins.cxLeftWidth, EDestHeight + SizingMargins.cyTopHeight,
    SizingMargins.cxRightWidth, SizingMargins.cyBottomHeight,
    SourceDC,
    ESourceWidth + SizingMargins.cxLeftWidth, ESourceHeight + SizingMargins.cyTopHeight,
    SizingMargins.cxRightWidth, SizingMargins.cyBottomHeight,
    TransparentColor
    );

  SetWindowOrgEx(SourceDC, LastOriginSource.X, LastOriginSource.Y, nil);
  SetWindowOrgEx(DestDC, LastOriginDest.X, LastOriginDest.Y, nil);
end;

function GetXPCaptionButtonBitmap(ABitmap: TBitmapAdapter; out BitmapCount: Integer): Boolean;
var
  Handle: THandle;
  ThemeFileNameW, BitmapFileNameW: array [0..MAX_PATH] of WideChar;
  Details: TThemedElementDetails;
  Margins: TMargins;
begin
  ThemeFileNameW[MAX_PATH] := #0;
  BitmapFileNameW[MAX_PATH] := #0;

  Result := UxTheme.GetCurrentThemeName(ThemeFileNameW, MAX_PATH, nil, 0, nil, 0) = S_OK;
  if not Result then
    Exit;

  Details := ThemeServices.GetElementDetails(twMinButtonNormal);
  with Details do
    Result := GetThemeFilename(ThemeServices.Theme[Element], Part, State,
      TMT_IMAGEFILE, BitmapFileNameW, MAX_PATH) = S_OK;
  if not Result then
    Exit;

  with Details do
    Result := GetThemeInt(ThemeServices.Theme[Element], Part, State,
      TMT_IMAGECOUNT, BitmapCount) = S_OK;
  if not Result then
    Exit;

  Result := BitmapCount > 0;
  if not Result then
    Exit;

  with Details do
    if GetThemeMargins(ThemeServices.Theme[Element], 0, Part, State,
      TMT_SIZINGMARGINS, nil, Margins) <> S_OK then
      FillChar(Margins, SizeOf(Margins), 0);
  ABitmap.Margins := Margins;

  Handle := SafeLoadLibrary(ThemeFileNameW, SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  if Handle <> 0 then
  try
    ABitmap.Assign(nil); // fixes GDI resource leak
    ABitmap.LoadFromResourceName(Handle, TranslateBitmapFileName(BitmapFileNameW));
    { (rb) can't determine actual transparent color? }
    ABitmap.TransparentColor := clFuchsia;

    Result := (ABitmap.Width > 0) and (ABitmap.Height > 0);
  finally
    FreeLibrary(Handle);
  end;
end;

{$ENDIF JVCLThemesEnabled}

//=== Global procedures ======================================================

function AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
  nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
  nHeightSrc: Integer; BlendFunction: BLENDFUNCTION): BOOL; stdcall;
begin
  if not GTriedLoadMsimg32Dll then
    LoadMsimg32Dll;
  Result := Assigned(_AlphaBlend) and
    _AlphaBlend(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc,
      nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, BlendFunction);
end;

function TransparentBlt(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, hHeightDest: Integer;
  hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
  crTransparent: UINT): BOOL; stdcall;
begin
  if not GTriedLoadMsimg32Dll then
    LoadMsimg32Dll;
  Result := Assigned(_TransparentBlt) and
    _TransparentBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, hHeightDest, hdcSrc,
      nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, crTransparent);
end;

{$IFDEF JVCLThemesEnabled}

//=== { TAlphaBitmap } =======================================================

destructor TAlphaBitmap.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TAlphaBitmap.Assign(Source: TPersistent);
begin
  // What to do here when Source is not nil???
  if not Assigned(Source) then
    FreeHandle
  else
    ;
end;

function TAlphaBitmap.CreateDIB(ADC: HDC; AWidth, AHeight: Integer): HBitmap;
begin
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(FBitmapInfo.bmiHeader);
    biWidth := AWidth;
    biHeight := AHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := AWidth * AHeight * 4;
  end;
  // Create the DIB
  Result := CreateDIBSection(ADC, FBitmapInfo, DIB_RGB_COLORS, FBitsMem, 0, 0);
end;

procedure TAlphaBitmap.CreateHandle(AWidth, AHeight: Integer);
var
  H: HBitmap;
begin
  FreeHandle;

  H := CreateScreenCompatibleDC;
  FDIBHandle := CreateDIB(H, AWidth, AHeight);
  if FDIBHandle <> 0 then
    FOldBitmap := SelectObject(H, FDIBHandle)
  else
    FOldBitmap := 0;
  FHandle := H;
end;

procedure TAlphaBitmap.Duplicate(Src: HBitmap);
var
  Bitmap: Windows.TBitmap;
begin
  GetObject(Src, SizeOf(Bitmap), @Bitmap);
  CreateHandle(Bitmap.bmWidth, Bitmap.bmHeight);

  DupBits(Src, FHandle, Point(Bitmap.bmWidth, Bitmap.bmHeight));
end;

procedure TAlphaBitmap.FreeHandle;
begin
  if FHandle <> 0 then
  begin
    if FDIBHandle <> 0 then
    begin
      if FOldBitmap <> 0 then
        SelectObject(FHandle, FOldBitmap);
      DeleteObject(FDIBHandle);
    end;
    DeleteDC(FHandle);
  end;
end;

function TAlphaBitmap.GetHeight: Integer;
begin
  Result := FBitmapInfo.bmiHeader.biHeight;
end;

function TAlphaBitmap.GetWidth: Integer;
begin
  Result := FBitmapInfo.bmiHeader.biWidth;
end;

procedure TAlphaBitmap.InitAlpha;
var
  Count: Integer;
begin
  Count := Width * Height;
  if BitCount < 32 then
    FHasAlphaChannel := False
  else
  begin
    FHasAlphaChannel := GetHasAlphaChannel(Data, Count);

    if HasAlphaChannel then
      PreMultiplyAlphaChannel(Data, Count);
  end;
end;

procedure TAlphaBitmap.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Stream: TCustomMemoryStream;
  BitmapInfoHeader: TBitmapInfoHeader;
  BitmapHandle: HBitmap;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_BITMAP);
  try
    Stream.Read(BitmapInfoHeader, SizeOf(TBitmapInfoHeader));
    FBitCount := BitmapInfoHeader.biBitCount;
  finally
    Stream.Free;
  end;

  if FBitCount = 32 then
  begin
    BitmapHandle := LoadImage(Instance, PChar(ResID), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
    if BitmapHandle = 0 then
      Exit;

    Duplicate(BitmapHandle);
    DeleteObject(BitmapHandle);

    InitAlpha;
  end;
end;

procedure TAlphaBitmap.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
  BitmapInfoHeader: TBitmapInfoHeader;
  BitmapHandle: HBitmap;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_BITMAP);
  try
    Stream.Read(BitmapInfoHeader, SizeOf(TBitmapInfoHeader));
    FBitCount := BitmapInfoHeader.biBitCount;
  finally
    Stream.Free;
  end;

  if FBitCount = 32 then
  begin
    BitmapHandle := LoadImage(Instance, PChar(ResName), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
    if BitmapHandle = 0 then
      Exit;

    Duplicate(BitmapHandle);
    DeleteObject(BitmapHandle);

    InitAlpha;
  end;
end;

//=== { TBitmapAdapter } =====================================================

constructor TBitmapAdapter.Create;
begin
  inherited Create;
  FTransparentColor := clFuchsia;
end;

destructor TBitmapAdapter.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBitmapAdapter.Assign(Source: TPersistent);
begin
  if FBitmap is TBitmap then
    (FBitmap as TBitmap).Assign(Source)
  else
  if FBitmap is TAlphaBitmap then
    (FBitmap as TAlphaBitmap).Assign(Source);
end;

procedure TBitmapAdapter.Clear;
begin
  FreeAndNil(FBitmap);
end;

function TBitmapAdapter.Draw(ACanvas: TCanvas; const Rect: TRect;
  AMargins: PMargins): Boolean;
begin
  if (Rect.Right - Rect.Left = Width) and (Rect.Bottom - Rect.Top = Height) then
    Result := DrawFixedPart(ACanvas, Rect, 0, 0)
  else
  begin
    if AMargins = nil then
      AMargins := @FMargins;

    if FBitmap is TAlphaBitmap then
      with TAlphaBitmap(FBitmap) do
        Result := TransparentBltStretch(ACanvas.Handle, Rect, Handle,
          Bounds(0, 0, Width, Height), AMargins^, FTransparentColor)
    else
    if FBitmap is TBitmap then
      with TBitmap(FBitmap) do
        Result := TransparentBltStretch(ACanvas.Handle, Rect, Canvas.Handle,
          Bounds(0, 0, Width, Height), AMargins^, FTransparentColor)
    else
      Result := False;
  end;
end;

function TBitmapAdapter.DrawFixed(ACanvas: TCanvas; const X, Y: Integer): Boolean;
begin
  Result := DrawFixedPart(ACanvas, Bounds(X, Y, Width, Height), 0, 0);
end;

function TBitmapAdapter.DrawFixedPart(ACanvas: TCanvas;
  const DestRect: TRect; const SrcX, SrcY: Integer): Boolean;
var
  BlendFunction: TBlendFunction;
  W, H: Integer;
begin
  W := DestRect.Right - DestRect.Left;
  H := DestRect.Bottom - DestRect.Top;

  if FBitmap is TAlphaBitmap then
  begin
    with TAlphaBitmap(FBitmap) do
    begin
      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.SourceConstantAlpha := $FF;
      BlendFunction.AlphaFormat := AC_SRC_ALPHA;

      Result := AlphaBlend(ACanvas.Handle, DestRect.Left, DestRect.Top, W, H,
        Handle, SrcX, SrcY, W, H, BlendFunction);
    end;
  end
  else
  if FBitmap is TBitmap then
    with TBitmap(FBitmap) do
      Result := TransparentBlt(ACanvas.Handle, DestRect.Left, DestRect.Top, W, H,
        Canvas.Handle, SrcX, SrcY, W, H, Self.TransparentColor)
  else
    Result := False;
end;

function TBitmapAdapter.DrawPart(ACanvas: TCanvas; const SrcRect,
  DestRect: TRect; AMargins: PMargins): Boolean;
begin
  // Same width/height?
  if (SrcRect.Right - SrcRect.Left = DestRect.Right - DestRect.Left) and
    (SrcRect.Bottom - SrcRect.Top = DestRect.Bottom - DestRect.Top) then
    Result := DrawFixedPart(ACanvas, DestRect, SrcRect.Left, SrcRect.Top)
  else
  begin
    if AMargins = nil then
      AMargins := @FMargins;

    if FBitmap is TAlphaBitmap then
      with TAlphaBitmap(FBitmap) do
        Result := TransparentBltStretch(ACanvas.Handle, DestRect, Handle, SrcRect,
          AMargins^, Self.TransparentColor)
    else
    if FBitmap is TBitmap then
      with TBitmap(FBitmap) do
        Result := TransparentBltStretch(ACanvas.Handle, DestRect, Canvas.Handle, SrcRect,
          AMargins^, Self.TransparentColor)
    else
      Result := False;
  end;
end;

function TBitmapAdapter.GetHeight: Integer;
begin
  if FBitmap is TAlphaBitmap then
    Result := TAlphaBitmap(FBitmap).Height
  else
  if FBitmap is TBitmap then
    Result := TBitmap(FBitmap).Height
  else
    Result := 0;
end;

function TBitmapAdapter.GetIsValid: Boolean;
begin
  Result := Assigned(FBitmap);
end;

function TBitmapAdapter.GetWidth: Integer;
begin
  if FBitmap is TAlphaBitmap then
    Result := TAlphaBitmap(FBitmap).Width
  else
  if FBitmap is TBitmap then
    Result := TBitmap(FBitmap).Width
  else
    Result := 0;
end;

procedure TBitmapAdapter.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  AlphaBitmap: TAlphaBitmap;
begin
  Clear;

  AlphaBitmap := TAlphaBitmap.Create;
  try
    AlphaBitmap.LoadFromResourceID(Instance, ResID);
    if AlphaBitmap.BitCount < 32 then
    begin
      FBitmap := TBitmap.Create;
      TBitmap(FBitmap).LoadFromResourceID(Instance, ResID);
    end
    else
    begin
      FBitmap := AlphaBitmap;
      AlphaBitmap := nil;
    end;
  finally
    AlphaBitmap.Free;
  end;
end;

procedure TBitmapAdapter.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  AlphaBitmap: TAlphaBitmap;
begin
  Clear;

  AlphaBitmap := TAlphaBitmap.Create;
  try
    AlphaBitmap.LoadFromResourceName(Instance, ResName);
    if AlphaBitmap.BitCount < 32 then
    begin
      FBitmap := TBitmap.Create;
      TBitmap(FBitmap).LoadFromResourceName(Instance, ResName);
    end
    else
    begin
      FBitmap := AlphaBitmap;
      AlphaBitmap := nil;
    end;
  finally
    AlphaBitmap.Free;
  end;
end;

//=== { TGlobalXPData } ======================================================

constructor TGlobalXPData.Create;
begin
  inherited Create;
  Update;
end;

destructor TGlobalXPData.Destroy;
begin
  FCaptionButtons.Free;
  inherited Destroy;
end;

procedure TGlobalXPData.AddClient;
begin
  Inc(FClientCount);
end;

function TGlobalXPData.Draw(ACanvas: TCanvas; State: Integer;
  const DrawRect: TRect): Boolean;
var
  SrcRect: TRect;
begin
  Result := FBitmapValid;
  if not Result then
    Exit;

  { State is 1-based }
  if (State >= FCaptionButtonCount) and (State > 4) then
    State := ((State - 1) mod 4) + 1;
  if State > FCaptionButtonCount then
    State := FCaptionButtonCount;

  SrcRect := Bounds(0, FCaptionButtonHeight * (State - 1),
    FCaptionButtons.Width, FCaptionButtonHeight);

  Result := FCaptionButtons.DrawPart(ACanvas, SrcRect, DrawRect, nil);
end;

procedure TGlobalXPData.DrawSimple(ACanvas: TCanvas; State: Integer;
  const DrawRect: TRect);
const
  // Normal, Hot, Pushed, Disabled,
  cCaptionButton: array [0..3] of TThemedWindow =
    (twMinButtonNormal, twMinButtonHot, twMinButtonPushed, twMinButtonDisabled);
  cNormalButton: array [0..3] of TThemedButton =
    (tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled);
var
  Details: TThemedElementDetails;
  DrawRgn: HRGN;
begin
  { Draw the button in 2 pieces, draw the edge of a caption button, and the
    inner of a normal button, because drawing a normal button looks ugly }

  // State = 1..8 -> State = 0..3
  State := (State - 1) mod 4;

  { 1a. Draw the outer bit as a caption button }
  Details := ThemeServices.GetElementDetails(cCaptionButton[State]);
  ThemeServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  { 1b. Draw the inner bit as a normal button }
  DrawRgn := CreateRectRgn(DrawRect.Left + 1, DrawRect.Top + 1, DrawRect.Right - 1, DrawRect.Bottom - 1);
  try
    Details := ThemeServices.GetElementDetails(cNormalButton[State]);
    SelectClipRgn(ACanvas.Handle, DrawRgn);
    ThemeServices.DrawElement(ACanvas.Handle, Details, DrawRect);
    SelectClipRgn(ACanvas.Handle, 0);
  finally
    DeleteObject(DrawRgn);
  end;
end;

procedure TGlobalXPData.RemoveClient;
begin
  Dec(FClientCount);
  if FClientCount = 0 then
  begin
    if Self = GGlobalXPData then
      GGlobalXPData := nil;
    Self.Free;
  end;
end;

procedure TGlobalXPData.Update;
begin
  FIsThemed := StyleServices.Available and IsThemeActive and IsAppThemed;
  if not FIsThemed then
    Exit;

  if FCaptionButtons = nil then
    FCaptionButtons := TBitmapAdapter.Create;

  FBitmapValid := GetXPCaptionButtonBitmap(FCaptionButtons, FCaptionButtonCount);
  if FBitmapValid then
    FCaptionButtonHeight := FCaptionButtons.Height div FCaptionButtonCount
  else
    FreeAndNil(FCaptionButtons);
end;

{$ENDIF JVCLThemesEnabled}

//=== { TJvCaptionButton } ===================================================

constructor TJvCaptionButton.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    raise EJVCLException.CreateRes(@RsEOwnerMustBeTCustomForm);

  inherited Create(AOwner);

  { Defaults }
  FAlignment := taLeftJustify;
  FHeight := 0;
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FEnabled := True;
  FImageIndex := -1;
  FLayout := cbImageLeft;
  FMargin := -1;
  FPosition := 0;
  FSpacing := 4;
  FStandard := tsbNone;
  FToggle := False;
  FVisible := True;

  FNeedRecalculate := True;
  FCaption := '';
  FDown := False;
  FToolTipHandle := 0;

  FFont := TFont.Create;
  FBuffer := TBitmap.Create;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FParentShowHint := True;

  FCurrentWindowState := TCustomForm(AOwner).WindowState;

  {$IFDEF JVCLThemesEnabled}
  GlobalXPData.AddClient;
  {$ENDIF JVCLThemesEnabled}

  {$IFDEF JVCLThemesEnabled}
  if IsVistaOrNewer and IsThemed then // Windows Vista
    FForceRedraw := True;
  {$ENDIF JVCLThemesEnabled}

  Hook;
end;

destructor TJvCaptionButton.Destroy;
begin
  DestroyToolTip;

  UnHook;
  Redraw(rkTotalCaptionBar);

  FFont.Free;
  FBuffer.Free;

  FreeAndNil(FActionLink);
  FreeAndNil(FImageChangeLink);

  {$IFDEF JVCLThemesEnabled}
  GlobalXPData.RemoveClient;
  {$ENDIF JVCLThemesEnabled}

  inherited Destroy;
end;

procedure TJvCaptionButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or not Assigned(Self.Images) then
        Self.Images := ActionList.Images;
      if not CheckDefaults or (Self.Caption = '') then
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

procedure TJvCaptionButton.Assign(Source: TPersistent);
begin
  if Source is TJvCaptionButton then
  begin
    Alignment := TJvCaptionButton(Source).Alignment;
    ButtonHeight := TJvCaptionButton(Source).ButtonHeight;
    ButtonLeft := TJvCaptionButton(Source).ButtonLeft;
    ButtonTop := TJvCaptionButton(Source).ButtonTop;
    ButtonWidth := TJvCaptionButton(Source).ButtonWidth;
    Caption := TJvCaptionButton(Source).Caption;
    ShowHint := TJvCaptionButton(Source).ShowHint;
    ParentShowHint := TJvCaptionButton(Source).ParentShowHint;
    Enabled := TJvCaptionButton(Source).Enabled;
    Font := TJvCaptionButton(Source).Font;
    Hint := TJvCaptionButton(Source).Hint;
    ImageIndex := TJvCaptionButton(Source).ImageIndex;
    Images := TJvCaptionButton(Source).Images;
    Layout := TJvCaptionButton(Source).Layout;
    Margin := TJvCaptionButton(Source).Margin;
    Position := TJvCaptionButton(Source).Position;
    Spacing := TJvCaptionButton(Source).Spacing;
    Standard := TJvCaptionButton(Source).Standard;
    // set toggle before down
    Toggle := TJvCaptionButton(Source).Toggle;
    Down := TJvCaptionButton(Source).Down;
    Visible := TJvCaptionButton(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCaptionButton.CalcButtonParts(ACanvas: TCanvas;
  ButtonRect: TRect; var RectText, RectImage: TRect);
// copied from TJvCustomImageButton
const
  CDefaultMargin = 4;
var
  BlockWidth, ButtonWidth, ButtonHeight, BlockMargin, InternalSpacing: Integer;
  LMargin: Integer;
  OldFont: TFont;
begin
  SetRect(RectText, 0, 0, 0, 0);
  OldFont := ACanvas.Font;
  ACanvas.Font := Font;
  DrawText(ACanvas, Caption, -1, RectText, DT_CALCRECT or Alignments[FAlignment]);
  ACanvas.Font := OldFont;
  if IsImageVisible then
  begin
    with Images do
      SetRect(RectImage, 0, 0, Width - 1, Height - 1);
    InternalSpacing := Spacing;
  end
  else
  begin
    SetRect(RectImage, 0, 0, 0, 0);
    InternalSpacing := 0;
  end;
  BlockWidth := RectImage.Right + InternalSpacing + RectText.Right;
  ButtonWidth := ButtonRect.Right - ButtonRect.Left;
  if Margin = -1 then
    LMargin := CDefaultMargin
  else
    LMargin := Margin;

  case Alignment of
    taLeftJustify:
      BlockMargin := LMargin;
    taRightJustify:
      BlockMargin := ButtonWidth - BlockWidth - LMargin - 1;
  else {taCenter}
    BlockMargin := (ButtonWidth - BlockWidth) div 2
  end;
  case Layout of
    cbImageLeft:
      begin
        OffsetRect(RectImage, BlockMargin, 0);
        OffsetRect(RectText, RectImage.Right + InternalSpacing, 0);
      end;
    cbImageRight:
      begin
        OffsetRect(RectText, BlockMargin, 0);
        OffsetRect(RectImage, RectText.Right + InternalSpacing, 0);
      end;
  end;
  ButtonHeight := ButtonRect.Bottom - ButtonRect.Top;
  OffsetRect(RectImage, ButtonRect.Left, (ButtonHeight - RectImage.Bottom) div 2 + ButtonRect.Top);
  OffsetRect(RectText, ButtonRect.Left, (ButtonHeight - RectText.Bottom) div 2 + ButtonRect.Top);
end;

procedure TJvCaptionButton.CalcDefaultButtonRect(Wnd: THandle);
const
  CSpaceBetweenButtons = 2;
var
  Style: DWORD;
  ExStyle: DWORD;
  FrameSize: TSize;
  Placement: WindowPlacement;
begin
  if Wnd = 0 then
    Exit;

  { 0. Init some local vars }
  FNeedRecalculate := False;
  Style := GetWindowLong(Wnd, GWL_STYLE);
  FHasCaption := Style and WS_CAPTION = WS_CAPTION;
  if not FHasCaption then
    Exit;

  Placement.length := SizeOf(WindowPlacement);
  GetWindowPlacement(Wnd, @Placement);
  ExStyle := GetWindowLong(Wnd, GWL_EXSTYLE);
  FHasSmallCaption := ExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
  {$IFDEF JVCLThemesEnabled}
  if not IsThemed and (Placement.showCmd = SW_SHOWMINIMIZED) then
    FHasSmallCaption := False;
  FCaptionActive := (GetActiveWindow = Wnd) and IsForegroundTask;
  {$ELSE}
  if Placement.showCmd = SW_SHOWMINIMIZED then
    FHasSmallCaption := False;
  {$ENDIF JVCLThemesEnabled}

  if (Style and WS_THICKFRAME = WS_THICKFRAME) and (Placement.showCmd <> SW_SHOWMINIMIZED) then
  begin
    FrameSize.cx := GetSystemMetrics(SM_CXSIZEFRAME);
    FrameSize.cy := GetSystemMetrics(SM_CYSIZEFRAME);
  end
  else
  begin
    FrameSize.cx := GetSystemMetrics(SM_CXFIXEDFRAME);
    FrameSize.cy := GetSystemMetrics(SM_CYFIXEDFRAME);
  end;

  { 1. Calc FDefaultButtonTop }
  FDefaultButtonTop := FrameSize.cy + 2;

  { 2. Calc FDefaultButtonHeight }
  if FHasSmallCaption then
    FCaptionHeight := GetSystemMetrics(SM_CYSMCAPTION)
  else
    FCaptionHeight := GetSystemMetrics(SM_CYCAPTION);
  FDefaultButtonHeight := FCaptionHeight - 5;

  { 3. Calc FDefaultButtonWidth }
  {$IFDEF JVCLThemesEnabled}
  if IsThemed then
  begin
    if IsVistaOrNewer then
    begin
      if FHasSmallCaption then
      begin
        FDefaultButtonWidth := GetSystemMetrics(SM_CXSMSIZE) - 4
      end
      else
      begin
        // This is not exactly correct but WM_GETTITLEBARINFOEX returns the coordinates
        // for the "Glass" style. But because we paint into the NC area, out window uses
        // the "Basic" style.
        FDefaultButtonWidth := GetSystemMetrics(SM_CXSIZE) - 4;
      end;

      // Adjust position
      FDefaultButtonTop := FDefaultButtonTop - 2;
      FDefaultButtonHeight := FCaptionHeight - 3;
    end
    else
      FDefaultButtonWidth := FDefaultButtonHeight;
  end
  else
  {$ENDIF JVCLThemesEnabled}
  if FHasSmallCaption then
    FDefaultButtonWidth := GetSystemMetrics(SM_CXSMSIZE) - CSpaceBetweenButtons
  else
    FDefaultButtonWidth := GetSystemMetrics(SM_CXSIZE) - CSpaceBetweenButtons;

  { 4. Calc FDefaultButtonLeft }
  FDefaultButtonLeft := FrameSize.cx;
  Inc(FDefaultButtonLeft, FDefaultButtonWidth + CSpaceBetweenButtons);

  if Style and WS_SYSMENU = WS_SYSMENU then
  begin
    { 4a. Avoid close button }
    Inc(FDefaultButtonLeft, FDefaultButtonWidth + CSpaceBetweenButtons);

    if not FHasSmallCaption then
    begin
      if (Style and WS_MAXIMIZEBOX = WS_MAXIMIZEBOX) or
        (Style and WS_MINIMIZEBOX = WS_MINIMIZEBOX) then
      begin
        {$IFDEF JVCLThemesEnabled}
        if IsThemed then
          { 4b. If it have Max or Min button, both are visible. When themed
                the CONTEXTHELP button is then never visible }
          Inc(FDefaultButtonLeft, 2 * (FDefaultButtonWidth + CSpaceBetweenButtons))
        else
        {$ENDIF JVCLThemesEnabled}
        begin
          { 4b. If it have Max or Min button, both are visible. }
          Inc(FDefaultButtonLeft, 2 * FDefaultButtonWidth + CSpaceBetweenButtons);

          { 4c. If it have CONTEXTHELP button, avoid it. }
          if ((Style and WS_MAXIMIZEBOX = 0) or (Style and WS_MINIMIZEBOX = 0)) and
            (ExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP) then
            Inc(FDefaultButtonLeft, FDefaultButtonWidth + 2 * CSpaceBetweenButtons);
        end;
      end
      else
      { 4c. If it have CONTEXTHELP button, avoid it. }
      if ExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP then
        Inc(FDefaultButtonLeft, FDefaultButtonWidth + CSpaceBetweenButtons);
    end;
  end;
end;

procedure TJvCaptionButton.Click;
begin
  if csDesigning in ComponentState then
    DesignerSelectComponent(Self)
  else
  if Enabled then
  begin
    { Call OnClick if assigned and not equal to associated action's OnExecute.
      If associated action's OnExecute assigned then call it, otherwise, call
      OnClick. }
    if Assigned(FOnClick) and (Action <> nil) and (@FOnClick <> @Action.OnExecute) then
      FOnClick(Self)
    else
    if {not (csDesigning in ComponentState) and} Assigned(ActionLink) then
      FActionLink.Execute(Self)
    else
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

procedure TJvCaptionButton.CreateToolTip(Wnd: THandle);
var
  ToolInfo: TToolInfo;
begin
  if csDesigning in ComponentState then
    Exit;

  DestroyToolTip;

  if Wnd = 0 then
    Exit;

  FToolTipHandle := CreateWindowEx(0, TOOLTIPS_CLASS, nil, TTS_ALWAYSTIP,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
    ParentForm.Handle, // Thus automatically destroyed if ParentForm handle is destroyed.
    0, HInstance, nil);

  if FToolTipHandle = 0 then
    Exit;

  // initialize tooltip info
  ToolInfo.cbSize := SizeOf(TToolInfo);
  ToolInfo.uFlags := TTF_IDISHWND; { Thus ignores rect param }
  ToolInfo.hwnd := Wnd;
  ToolInfo.uId := Wnd;
  ToolInfo.lpszText := LPSTR_TEXTCALLBACK;

  // register button with tooltip
  SendMessage(FToolTipHandle, TTM_ADDTOOL, 0, LPARAM(@ToolInfo));
end;

procedure TJvCaptionButton.DestroyToolTip;
begin
  if FToolTipHandle <> 0 then
  begin
    DestroyWindow(FToolTipHandle);
    FToolTipHandle := 0;
  end;
end;

procedure TJvCaptionButton.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

procedure TJvCaptionButton.DrawButton(DC: HDC);
var
  Canvas: TControlCanvas;
begin
  if not Visible or not FHasCaption or (csDestroying in ComponentState) then
    Exit;

  Canvas := TControlCanvas.Create;
  try
    Canvas.Handle := DC;

    UpdateButtonRect(ParentFormHandle);

    FBuffer.Width := FButtonRect.Right - FButtonRect.Left;
    FBuffer.Height := FButtonRect.Bottom - FButtonRect.Top;

    {$IFDEF JVCLThemesEnabled}
    DrawButtonBackground(FBuffer.Canvas);
    {$ENDIF JVCLThemesEnabled}

    { We do a buffered drawing, otherwise you get flickering on XP, and you
      have to hassle with the clipping rects. }
    if FStandard <> tsbNone then
      DrawStandardButton(FBuffer.Canvas)
    else
      DrawNonStandardButton(FBuffer.Canvas);

    Canvas.Draw(FButtonRect.Left, FButtonRect.Top, FBuffer);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvCaptionButton.DrawButtonBackground(ACanvas: TCanvas);
const
  CCaption: array [Boolean, Boolean] of TThemedWindow =
   (
    (twCaptionInactive, twCaptionActive),
    (twSmallCaptionInactive, twSmallCaptionActive)
   );
var
  Details: TThemedElementDetails;
  ClipRect: TRect;
  CaptionRect: TRect;
begin
  if not IsThemed or (csDestroying in ComponentState) then
    Exit;

  { We basically draw the background to display 4 pixels - the corners of the
    rect - correct <g>. Don't know a better way to do this. }

  { Determine the rect of the caption }
  GetWindowRect(ParentFormHandle, CaptionRect);
  OffsetRect(CaptionRect, -CaptionRect.Left, -CaptionRect.Top);
  CaptionRect.Bottom := FCaptionHeight + 4;
  { Offset it so the place where the button is, is at (0, 0) }
  OffsetRect(CaptionRect, -FButtonRect.Left, -FButtonRect.Top);
  ClipRect := Rect(0, 0, FButtonRect.Right - FButtonRect.Left, FButtonRect.Bottom - FButtonRect.Top);

  Details := ThemeServices.GetElementDetails(CCaption[FHasSmallCaption, FCaptionActive]);
  ThemeServices.DrawElement(ACanvas.Handle, Details, CaptionRect, @ClipRect);
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCaptionButton.DrawButtonImage(ACanvas: TCanvas; ImageBounds: TRect);
begin
  if csDestroying in ComponentState then
    Exit;
  if IsImageVisible then
    Images.Draw(ACanvas, ImageBounds.Left, ImageBounds.Top, ImageIndex, Enabled);
end;

procedure TJvCaptionButton.DrawButtonText(ACanvas: TCanvas; TextBounds: TRect);
var
  Flags: DWORD;
  OldFont: TFont;
begin
  Flags := DT_VCENTER or Alignments[FAlignment];
  with ACanvas do
  begin
    Brush.Style := bsClear;
    if not Enabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      OldFont := Font;
      Font := Self.Font;
      Font.Color := clBtnHighlight;
      DrawText(ACanvas, Caption, Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(ACanvas, Caption, Length(Caption), TextBounds, Flags);
      Font := OldFont;
    end
    else
    begin
      OldFont := Font;
      Font := Self.Font;
      DrawText(ACanvas, Caption, Length(Caption), TextBounds, Flags);
      Font := OldFont;
    end;
  end;
end;

procedure TJvCaptionButton.DrawNonStandardButton(ACanvas: TCanvas);
{$IFDEF JVCLThemesEnabled}
const
  cState_Normal = 1;
  cState_Hot = 2;
  cState_Pushed = 3;
  cState_Disabled = 4;
{$ENDIF JVCLThemesEnabled}
var
  DrawRect: TRect;
  RectText, RectImage: TRect;
  {$IFDEF JVCLThemesEnabled}
  State: Integer;
  DrawRgn: HRGN;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  DrawRect := Rect(0, 0, FButtonRect.Right - FButtonRect.Left, FButtonRect.Bottom - FButtonRect.Top);

  {$IFDEF JVCLThemesEnabled}
  // Satisfy the compiler
  DrawRgn := 0;

  { 1. Draw the button }
  if IsThemed then
  begin
    if not Enabled then
      State := 4
    else
    if FDown then
      State := 3
    else
    if FMouseInControl then
      State := 2
    else
      State := 1;

    { Special state for buttons drawn on a not active caption }
    if not FCaptionActive then
      Inc(State, 4);

    if ForceDrawSimple or not GlobalXPData.Draw(ACanvas, State, DrawRect) then
      GlobalXPData.DrawSimple(ACanvas, State, DrawRect)
  end
  else
  {$ENDIF JVCLThemesEnabled}
    DrawButtonFace(ACanvas, DrawRect, 1, bsAutoDetect, False, FDown, False);

  { 2. Draw the text & picture }
  {$IFDEF JVCLThemesEnabled}
  if IsThemed then
  begin
    { 2a. If themed, only draw in the inner bit of the button using a clip region }
    DrawRgn := CreateRectRgn(DrawRect.Left + 2, DrawRect.Top + 2, DrawRect.Right - 2, DrawRect.Bottom - 2);

    SelectClipRgn(ACanvas.Handle, DrawRgn);
  end;
  {$ENDIF JVCLThemesEnabled}

  if FDown then
  begin
    {$IFDEF JVCLThemesEnabled}
    if IsThemed then
      OffsetRect(DrawRect, 1, 0)
    else
    {$ENDIF JVCLThemesEnabled}
      OffsetRect(DrawRect, 1, 1);
  end;
  { 2b. Calc position and Draw the picture & text }
  CalcButtonParts(ACanvas, DrawRect, RectText, RectImage);
  DrawButtonText(ACanvas, RectText);
  DrawButtonImage(ACanvas, RectImage);
  { 2c. Clean up }
  {$IFDEF JVCLThemesEnabled}
  if IsThemed then
  begin
    SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(DrawRgn);
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvCaptionButton.DrawStandardButton(ACanvas: TCanvas);
const
  {$IFDEF JVCLThemesEnabled}
  CElements: array [TJvStandardButton] of TThemedWindow =
   (twWindowDontCare, twCloseButtonNormal, twHelpButtonNormal, twMaxButtonNormal,
    twMinButtonNormal, twRestoreButtonNormal, twMinButtonNormal);
  {$ENDIF JVCLThemesEnabled}
  CDrawFlags: array [TJvStandardButton] of Word =
   (0, DFCS_CAPTIONCLOSE, DFCS_CAPTIONHELP, DFCS_CAPTIONMAX, DFCS_CAPTIONMIN,
    DFCS_CAPTIONRESTORE, 0);
  CDown: array [Boolean] of Word = (0, DFCS_PUSHED);
  CEnabled: array [Boolean] of Word = (DFCS_INACTIVE, 0);
var
  DrawRect: TRect;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  CaptionButton: TThemedWindow;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  DrawRect := Rect(0, 0, FButtonRect.Right - FButtonRect.Left, FButtonRect.Bottom - FButtonRect.Top);

  {$IFDEF JVCLThemesEnabled}
  if IsThemed then
  begin
    CaptionButton := CElements[FStandard];
    { Note : There is only a small close button (??) }
    if FHasSmallCaption and (FStandard = tsbClose) then
      CaptionButton := twSmallCloseButtonNormal;

    if not Enabled then
      Inc(CaptionButton, 3)
    else
    if FDown then
      { If Down and inactive, draw inactive border }
      Inc(CaptionButton, 2)
    else
    if FMouseInControl then
      Inc(CaptionButton);

    Details := ThemeServices.GetElementDetails(CaptionButton);
    { Special state for buttons drawn on a not active caption }
    if not FCaptionActive and (Details.State = 1) then
      Details.State := 5;
    ThemeServices.DrawElement(ACanvas.Handle, Details, DrawRect)
  end
  else
  {$ENDIF JVCLThemesEnabled}
  if Standard = tsbMinimizeToTray then
  begin
    DrawButtonFace(ACanvas, DrawRect, 1, bsAutoDetect, False, FDown, False);
    if Enabled then
      ACanvas.Brush.Color := clWindowText
    else
    begin
      ACanvas.Brush.Color := clBtnHighlight;
      ACanvas.FillRect(Rect(DrawRect.Right - 6, DrawRect.Bottom - 4, DrawRect.Right - 3, DrawRect.Bottom - 2));
      ACanvas.Brush.Color := clBtnShadow;
    end;
    ACanvas.FillRect(Rect(DrawRect.Right - 7, DrawRect.Bottom - 5, DrawRect.Right - 4, DrawRect.Bottom - 3));
  end
  else
    DrawFrameControl(ACanvas.Handle, DrawRect, DFC_CAPTION, {DFCS_ADJUSTRECT or}
      CDrawFlags[Standard] or CDown[Down] or CEnabled[Enabled]);
end;

procedure TJvCaptionButton.ForwardToToolTip(Msg: TMessage);
var
  ForwardMsg: TMsg;
begin
  if FToolTipHandle = 0 then
    Exit;

  // forward to tool tip
  ForwardMsg.lParam := Msg.LParam;
  ForwardMsg.wParam := Msg.WParam;
  ForwardMsg.message := Msg.Msg;
  ForwardMsg.hwnd := ParentFormHandle;
  SendMessage(FToolTipHandle, TTM_RELAYEVENT, 0, LPARAM(@ForwardMsg));
end;

function TJvCaptionButton.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TJvCaptionButton.GetActionLinkClass: TJvCaptionButtonActionLinkClass;
begin
  Result := TJvCaptionButtonActionLink;
end;

function TJvCaptionButton.GetIsImageVisible: Boolean;
begin
  Result := Assigned(Images) and (ImageIndex > -1) and (ImageIndex < Images.Count);
end;

{$IFDEF JVCLThemesEnabled}
function TJvCaptionButton.GetIsThemed: Boolean;
begin
  Result := GlobalXPData.IsThemed;
end;
{$ENDIF JVCLThemesEnabled}

function TJvCaptionButton.GetParentForm: TCustomForm;
begin
  if Owner is TControl then
    Result := Forms.GetParentForm(TControl(Owner))
  else
    Result := nil;
end;

function TJvCaptionButton.GetParentFormHandle: THandle;
var
  P: TCustomForm;
begin
  P := GetParentForm;
  if Assigned(P) and P.HandleAllocated then
    Result := P.Handle
  else
    Result := 0;
end;

function TJvCaptionButton.HandleButtonDown(var Msg: TWMNCHitMessage): Boolean;
begin
  Result := Visible and Enabled and (Msg.HitTest = htCaptionButton) and
    MouseOnButton(Msg.XCursor, Msg.YCursor, False);

  if Result then
  begin
    FMouseButtonDown := True;
    if Toggle then
      FDown := not FDown
    else
      FDown := True;
    with TWMMouse(Msg) do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    {if not Toggle then}
    SetCapture(ParentFormHandle);
    Redraw(rkIndirect);

    { Note: If Toggle = False -> click event is fired in HandleButtonUp }
    if Toggle then
      Click;
  end
  else
  if FDown and not Toggle then
  begin
    FMouseButtonDown := False;
    FDown := False;
    Redraw(rkIndirect);
  end;
end;

function TJvCaptionButton.HandleButtonUp(var Msg: TWMNCHitMessage): Boolean;
var
  DoClick: Boolean;
  P: TPoint;
begin
  Result := False;

  if not FMouseButtonDown then
    Exit;

  Result := FDown and MouseOnButton(Msg.XCursor, Msg.YCursor, Msg.Msg = WM_LBUTTONUP);
  { Note: If Toggle = True -> click event is fired in HandleButtonDown }
  DoClick := Result and not Toggle;

  FMouseButtonDown := False;
  ReleaseCapture;

  if not Toggle then
  begin
    FDown := False;
    Redraw(rkIndirect);
  end;

  if DoClick then
    Click;

  //(p3) we need to convert MouseUp message because they are in client coordinates (MouseDown are already in screen coords, so no need to change)
  with TWMMouse(Msg) do
  begin
    P := Point(XPos, YPos);
    Assert(ParentForm <> nil, '');
    P := ParentForm.ClientToScreen(P);
    MouseUp(mbLeft, KeysToShiftState(Keys), P.X, P.Y);
  end;
end;

function TJvCaptionButton.HandleHitTest(var Msg: TWMNCHitTest): Boolean;
var
  CurPos: TPoint;
begin
  Result := Visible and MouseOnButton(Msg.XPos, Msg.YPos, False);
  if Result then
    Msg.Result := htCaptionButton;

  if not Result and Visible and MouseInControl then
  begin
    // We can get weird hittest values (probably from the hint window) so
    // double check that the mouse is not on the button.
    // Actually we wrongfully assumed that Msg represents the current mouse
    // position so we have to double check.
    GetCursorPos(CurPos);
    if not MouseOnButton(CurPos.X, CurPos.Y, False) then
    begin
      SetMouseInControl(False);
      Redraw(rkIndirect);
    end;
  end;

  //Result := False;
end;

function TJvCaptionButton.HandleMouseMove(var Msg: TWMNCHitMessage): Boolean;
var
  DoRedraw: Boolean;
  MouseWasInControl: Boolean;
begin
  Result := FMouseButtonDown;

  if Result then
  begin
    MouseWasInControl := FMouseInControl;
    SetMouseInControl(MouseOnButton(Msg.XCursor, Msg.YCursor, Msg.Msg = WM_MOUSEMOVE));
    DoRedraw := (FMouseInControl <> MouseWasInControl) or
      // User presses mouse button, but left the caption button
      (FDown and not Toggle and not FMouseInControl) or
      // User presses mouse button, and enters the caption button
      (not FDown and not Toggle and FMouseInControl);

    FDown := (FDown and Toggle) or
      (FMouseButtonDown and not Toggle and FMouseInControl);
    if DoRedraw then
      Redraw(rkIndirect);
  end;
  // (p3) don't handle mouse move here: it is triggered even if the mouse is outside the button
  //  with TWMMouseMove(Msg) do
  //    MouseMove(KeysToShiftState(Keys), XPos, YPos);
end;

procedure TJvCaptionButton.HandleNCActivate(var Msg: TWMNCActivate);
begin
  {$IFDEF JVCLThemesEnabled}
  FCaptionActive := Msg.Active;
  {$ENDIF JVCLThemesEnabled}
  SetMouseInControl(MouseInControl and Msg.Active);

  Redraw(rkDirect);
end;

procedure TJvCaptionButton.HandleNCMouseMove(var Msg: TWMNCHitMessage);
var
  IsOnButton: Boolean;
begin
  IsOnButton := MouseOnButton(Msg.XCursor, Msg.YCursor, False);
  if Visible then
  begin
    if (IsOnButton <> FMouseInControl) then
    begin
      SetMouseInControl(not FMouseInControl);
      if not Down then
        Redraw(rkIndirect);
    end;
   // (p3) only handle mouse move if we are inside the button or it will be triggered for the entire NC area
    if IsOnButton then
      with TWMMouseMove(Msg) do
        MouseMove(KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TJvCaptionButton.HandleNCPaintAfter(Wnd: THandle; var Msg: TWMNCPaint);
begin
  if FRgnChanged then
  begin
    DeleteObject(Msg.RGN);
    Msg.RGN := FSaveRgn;
    FRgnChanged := False;
  end;

  Redraw(rkDirect);
end;

procedure TJvCaptionButton.HandleNCPaintBefore(Wnd: THandle; var Msg: TWMNCPaint);
var
  WindowRect: TRect;
  DrawRgn: HRGN;
  LButtonRect: TRect;
begin
  { Note: There is one problem with this reduce flickering method: This
          function is executed before windows handles the WM_NCPAINT and
          HandleNCPaintAfter is executed after windows handles WM_NCPAINT.

          When you resize a form, the value returned by API GetWindowRect is
          adjusted when windows handles the WM_NCPAINT.

          Thus return value of GetWindowRect in HandleNCPaintBefore differs
          from return value of GetWindowRect in HandleNCPaintAfter.
        ->
          Thus value of FButtonRect in HandleNCPaintBefore differs
          from return value of FButtonRect in HandleNCPaintAfter.

          (Diff is typically 1 pixel)

          This causes a light flickering at the edge of the button when
          you resize the form.

          To see it, put Sleep(1000) or so, before and after the DrawButton call
          in HandleNCPaintAfter and resize the screen horizontally
  }
  if Wnd = 0 then
    Exit;

  FSaveRgn := Msg.RGN;
  FRgnChanged := False;
  { Calculate button rect in screen coordinates, put it in LButtonRect }
  UpdateButtonRect(Wnd);
  LButtonRect := FButtonRect;
  GetWindowRect(Wnd, WindowRect);
  OffsetRect(LButtonRect, WindowRect.Left, WindowRect.Top);
  { Check if button rect is in the to be updated region.. }
  if RectInRegion(FSaveRgn, LButtonRect)
    {$IFDEF JVCLThemesEnabled}
    or FForceRedraw
    {$ENDIF JVCLThemesEnabled}
    then
  begin
    {$IFDEF JVCLThemesEnabled}
    FForceRedraw := False;
    {$ENDIF JVCLThemesEnabled}
    { ..If so remove the button rectangle from the region (otherwise the caption
      background would be drawn over the button, which causes flicker) }
    DrawRgn := CreateRectRgn(LButtonRect.Left, LButtonRect.Top, LButtonRect.Right, LButtonRect.Bottom);
    try
      Msg.RGN := CreateRectRgn(0, 0, 1, 1);
      FRgnChanged := True;
      CombineRgn(Msg.RGN, FSaveRgn, DrawRgn, RGN_DIFF);
    finally
      DeleteObject(DrawRgn);
    end;
  end;
end;

function TJvCaptionButton.HandleNotify(var Msg: TWMNotify): Boolean;
var
  CurPos: TPoint;
  LButtonRect, WindowRect: TRect;
begin
  // if we receive a TTN_GETDISPINFO notification
  // and it is from the tooltip
  Result := (Msg.NMHdr.code = TTN_NEEDTEXT) and (Msg.NMHdr.hwndFrom = FToolTipHandle);

  if Result and (ShowHint or (ParentShowHint and ParentForm.ShowHint)) then
  begin
    // get cursor position
    GetCursorPos(CurPos);
    GetWindowRect(ParentFormHandle, WindowRect);
    LButtonRect := FButtonRect;
    OffsetRect(LButtonRect, WindowRect.Left, WindowRect.Top);

    // if the mouse is in the area of the button
    if PtInRect(LButtonRect, CurPos) then
      {$IFDEF SUPPORTS_UNICODE}
      if Msg.NMHdr.code = TTN_NEEDTEXTW then
      begin
        with PNMTTDispInfoW(Msg.NMHdr)^ do
        begin
          // then we return the hint
          lpszText := PChar(FHint);  // we do loose text here, but unicode should have kicked in anyway
          hinst := 0;
          uFlags := TTF_IDISHWND;
          hdr.idFrom := ParentFormHandle;
        end;
      end
      else
      {$ENDIF SUPPORTS_UNICODE}
      if Msg.NMHdr.code = TTN_NEEDTEXTA then
      begin
        with PNMTTDispInfoA(Msg.NMHdr)^ do
        begin
          // then we return the hint
          lpszText := PAnsiChar(AnsiString(FHint));  // we do loose text here, but unicode should have kicked in anyway
          hinst := 0;
          uFlags := TTF_IDISHWND;
          hdr.idFrom := ParentFormHandle;
        end;
      end
      else
        with PNMTTDispInfoW(Msg.NMHdr)^ do
        begin
          // then we return the hint
          lpszText := PWideChar(WideString(FHint));
          hinst := 0;
          uFlags := TTF_IDISHWND;
          hdr.idFrom := ParentFormHandle;
        end
    else
      //else we hide the tooltip
      HideToolTip;
  end;
end;

procedure TJvCaptionButton.HideToolTip;
begin
  if FToolTipHandle <> 0 then
    SendMessage(FToolTipHandle, TTM_POP, 0, 0);
end;

procedure TJvCaptionButton.Hook;
var
  P: TCustomForm;
begin
  //if not Visible or not FHasCaption then
  // Exit;

  P := ParentForm;
  if Assigned(P) then
  begin
    RegisterWndProcHook(P, WndProcAfter, hoAfterMsg);
    RegisterWndProcHook(P, WndProcBefore, hoBeforeMsg);

    if P.HandleAllocated then
      CreateToolTip(P.Handle);
  end;
end;

procedure TJvCaptionButton.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Redraw(rkIndirect);
end;

procedure TJvCaptionButton.InitiateAction;
begin
  if FActionLink <> nil then
    FActionLink.Update;
end;

function TJvCaptionButton.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TJvCaptionButton.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TJvCaptionButton.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TJvCaptionButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

procedure TJvCaptionButton.Loaded;
begin
  inherited Loaded;

  CreateToolTip(ParentFormHandle);
  Redraw(rkTotalCaptionBar);
end;

procedure TJvCaptionButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvCaptionButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

function TJvCaptionButton.MouseOnButton(X, Y: Integer;
  const TranslateToScreenCoord: Boolean): Boolean;
var
  WindowRect: TRect;
  Wnd: THandle;
  P: TPoint;
begin
  Wnd := ParentFormHandle;
  Result := Wnd <> 0;
  if not Result then
    Exit;

  P := Point(X, Y);
  if TranslateToScreenCoord then
    Windows.ClientToScreen(Wnd, P);

  GetWindowRect(Wnd, WindowRect);
  Result := PtInRect(FClickRect, Point(P.X - WindowRect.Left, P.Y - WindowRect.Top));
end;

procedure TJvCaptionButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TJvCaptionButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then
    Images := nil;
end;

procedure TJvCaptionButton.Redraw(const AKind: TJvRedrawKind);
var
  Wnd: THandle;
  DC: HDC;
begin
  if csLoading in ComponentState then
    Exit;

  Wnd := ParentFormHandle;
  if Wnd = 0 then
    Exit;

  case AKind of
    rkDirect:
      begin
        DC := GetWindowDC(Wnd);
        try
          DrawButton(DC);
        finally
          ReleaseDC(Wnd, DC);
        end;
      end;
    rkIndirect:
      begin
        UpdateButtonRect(Wnd);
        RedrawWindow(Wnd, @FButtonRect, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
      end;
    rkTotalCaptionBar:
      begin
        UpdateButtonRect(Wnd);
        RedrawWindow(Wnd, PRect(0), 0, RDW_FRAME or RDW_NOINTERNALPAINT or RDW_INVALIDATE);
      end;
  end;
end;

procedure TJvCaptionButton.ResetButton;
begin
  UnHook;
  Hook;
  Redraw(rkTotalCaptionBar);
end;

procedure TJvCaptionButton.SetAction(const Value: TBasicAction);
begin
  if (FActionLink <> nil) and (FActionLink.Action <> nil) then
    FActionLink.Action.RemoveFreeNotification(Self);
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;

procedure TJvCaptionButton.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetCaption(Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetDown(const Value: Boolean);
begin
  if (FDown <> Value) and Toggle then
  begin
    FDown := Value;
    Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetFont(Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvCaptionButton.SetForceDrawSimple(const Value: Boolean);
begin
  if FForceDrawSimple <> Value then
  begin
    FForceDrawSimple := Value;
    if IsThemed then
      Redraw(rkDirect);
  end;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCaptionButton.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (Value >= 0) then
  begin
    FHeight := Value;
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.SetImageIndex(const Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetImages(const Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FImages, FImageChangeLink);
  if Standard = tsbNone then
    Redraw(rkIndirect);
end;

procedure TJvCaptionButton.SetLayout(const Value: TJvCaptionButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if (csDesigning in ComponentState) and (FAlignment <> taCenter) then
      case FLayout of
        cbImageLeft: FAlignment := taLeftJustify;
        cbImageRight: FAlignment := taRightJustify;
      end;
    if (Standard = tsbNone) and IsImageVisible then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.SetMargin(const Value: Integer);
begin
  if (FMargin <> Value) and (Value >= -1) then
  begin
    FMargin := Value;
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetMouseInControl(const Value: Boolean);
begin
  if FMouseInControl <> Value then
  begin
    if not Value then
      HideToolTip;

    FMouseInControl := Value;
  end;
end;

procedure TJvCaptionButton.SetParentShowHint(const Value: Boolean);
begin
  if FParentShowHint <> Value then
  begin
    FParentShowHint := Value;
    if FParentShowHint then
      FShowHint := ParentForm.ShowHint;
  end;
end;

procedure TJvCaptionButton.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    FParentShowHint := False;
  end;
end;

procedure TJvCaptionButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    if Standard = tsbNone then
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetStandard(Value: TJvStandardButton);
{$IFDEF JVCLThemesEnabled}
var
  ButtonSizeChanged: Boolean;
{$ENDIF JVCLThemesEnabled}
begin
  if FStandard <> Value then
  begin
    {$IFDEF JVCLThemesEnabled}
    ButtonSizeChanged := IsThemed and
      ((Value = tsbMinimizeToTray) or (FStandard = tsbMinimizeToTray));
    {$ENDIF JVCLThemesEnabled}
    FStandard := Value;

    {$IFDEF JVCLThemesEnabled}
    if ButtonSizeChanged then
      UpdateButtonRect(ParentFormHandle);

    if ButtonSizeChanged and (FStandard = tsbMinimizeToTray) then
      Redraw(rkTotalCaptionBar)
    else
    {$ENDIF JVCLThemesEnabled}
      Redraw(rkIndirect);
  end;
end;

procedure TJvCaptionButton.SetToggle(const Value: Boolean);
begin
  if FToggle <> Value then
  begin
    FToggle := Value;
    if FDown and not FToggle and not (FMouseInControl and FMouseButtonDown) then
    begin
      FDown := False;
      Redraw(rkIndirect);
    end;
  end;
end;

procedure TJvCaptionButton.SetTop(Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    { Caption, RedrawButton doesn't draw the caption itself }
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    FWidth := Value;
    Redraw(rkTotalCaptionBar);
  end;
end;

procedure TJvCaptionButton.UnHook;
var
  P: TCustomForm;
begin
  P := ParentForm;
  if Assigned(P) then
  begin
    DestroyToolTip;
    UnRegisterWndProcHook(P, WndProcAfter, hoAfterMsg);
    UnRegisterWndProcHook(P, WndProcBefore, hoBeforeMsg);
  end;
end;

procedure TJvCaptionButton.UpdateButtonRect(Wnd: THandle);
var
  WindowRect: TRect;
  LButtonWidth: Integer;
  LButtonHeight: Integer;
begin
  if Wnd = 0 then
    Exit;

  if FNeedRecalculate then
    CalcDefaultButtonRect(Wnd);

  GetWindowRect(Wnd, WindowRect);

  if ButtonWidth <> 0 then
    LButtonWidth := ButtonWidth
  else
    LButtonWidth := FDefaultButtonWidth;

  if ButtonHeight <> 0 then
    LButtonHeight := ButtonHeight
  else
    LButtonHeight := FDefaultButtonHeight;

  FButtonRect := Bounds(
    WindowRect.Right - WindowRect.Left - FDefaultButtonLeft + ButtonLeft
    + FDefaultButtonWidth - LButtonWidth,
    FDefaultButtonTop + ButtonTop, LButtonWidth, LButtonHeight);

  OffsetRect(FButtonRect, -FPosition * (FDefaultButtonWidth + 2), 0);

  { Special }
  {$IFDEF JVCLThemesEnabled}
  if (FStandard = tsbMinimizeToTray) and IsThemed then
    Inc(FButtonRect.Top, 2);
  {$ENDIF JVCLThemesEnabled}

  { Click rect is a bit bigger }
  FClickRect := Rect(FButtonRect.Left - 2, FButtonRect.Top - 2, FButtonRect.Right + 1, FButtonRect.Bottom + 2);
end;

function TJvCaptionButton.WndProcAfter(var Msg: TMessage): Boolean;
begin
  { let others listen in too }
  Result := False;

  case Msg.Msg of
    {$IFDEF JVCLThemesEnabled}
    WM_THEMECHANGED,
    {$ENDIF JVCLThemesEnabled}
    CM_SYSCOLORCHANGE:
      begin
        FNeedRecalculate := True;
        {$IFDEF JVCLThemesEnabled}
        { force theme data refresh, needed when

          * Switching from 'windows classic' style to 'windows XP' style
            ( delphi 7 bug) }
        ThemeServices.UpdateThemes;
        GlobalXPData.Update;
        {$ENDIF JVCLThemesEnabled}
      end;
    CM_SYSFONTCHANGED:
      begin
        FNeedRecalculate := True;
        {$IFDEF JVCLThemesEnabled}
        { force theme data refresh, needed when

          * Non-themed application and switching system font size }
        if not StyleServices.Enabled then
          ThemeServices.UpdateThemes;
        {$ENDIF JVCLThemesEnabled}
      end;
    WM_SETTEXT:
      { Caption text may overwrite the button, so redraw }
      Redraw(rkIndirect);
    WM_NCPAINT:
      HandleNCPaintAfter(ParentFormHandle, TWMNCPaint(Msg));
    WM_NCACTIVATE:
      HandleNCActivate(TWMNCActivate(Msg));
    CM_RECREATEWND:
      begin
        CreateToolTip(ParentFormHandle);
        FNeedRecalculate := True;
        //CalcDefaultButtonRect(ParentFormHandle);
        Redraw(rkTotalCaptionBar);
      end;
    WM_LBUTTONDOWN, WM_NCLBUTTONUP, WM_LBUTTONUP, WM_NCMOUSEMOVE, WM_NCRBUTTONUP,
    WM_RBUTTONUP, WM_NCRBUTTONDOWN, WM_RBUTTONDOWN, WM_NCLBUTTONDOWN:
      ForwardToToolTip(Msg);
  end;
end;

function TJvCaptionButton.WndProcBefore(var Msg: TMessage): Boolean;
begin
  case Msg.Msg of
    CM_SHOWHINTCHANGED:
      begin
        if ParentShowHint then
          FShowHint := ParentForm.ShowHint;
        Result := False;
      end;
    CM_MOUSELEAVE, CM_MOUSEENTER:
      begin
        if FMouseInControl then
        begin
          SetMouseInControl(False);
          Redraw(rkIndirect);
        end;
        Result := False;
      end;
    WM_NCLBUTTONDOWN: //, WM_LBUTTONDOWN:
      begin
        Result := HandleButtonDown(TWMNCHitMessage(Msg));
        if Result then
          ForwardToToolTip(Msg);
      end;
    WM_NCLBUTTONUP, WM_LBUTTONUP:
      begin
        Result := HandleButtonUp(TWMNCHitMessage(Msg));
        if Result then
          ForwardToToolTip(Msg);
      end;
    WM_MOUSEMOVE:
      begin
        Result := HandleMouseMove(TWMNCHitMessage(Msg));
        if Result then
          ForwardToToolTip(Msg);
      end;
    WM_NCMOUSEMOVE:
      begin
        Result := False;
        HandleNCMouseMove(TWMNCHitMessage(Msg));
      end;
    WM_NCHITTEST:
      Result := HandleHitTest(TWMNCHitTest(Msg));
    WM_NCPAINT:
      begin
        Result := False;
        HandleNCPaintBefore(ParentFormHandle, TWMNCPaint(Msg));
      end;
    WM_DESTROY:
      begin
        Result := False;
        {DestroyToolTip;}
        // FToolTipHandle is automatically destroyed when ParentForm handle is destroyed.
        FToolTipHandle := 0;
      end;
    WM_NOTIFY:
      Result := HandleNotify(TWMNotify(Msg));
    WM_SIZE:
      begin
        if Assigned(ParentForm) and (FCurrentWindowState <> ParentForm.WindowState) then
        begin
          FNeedRecalculate := True;
          FCurrentWindowState := ParentForm.WindowState;
          RedrawWindow(ParentFormHandle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
        end;
        Result := False;
      end;
  else
    Result := False;
  end;
end;

//=== { TJvCaptionButtonActionLink } =========================================

procedure TJvCaptionButtonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TJvCaptionButton;
end;

function TJvCaptionButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    AnsiSameText(FClient.Caption, (Action as TCustomAction).Caption);
end;

function TJvCaptionButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TJvCaptionButtonActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TJvCaptionButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TJvCaptionButtonActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnClick = @Action.OnExecute);
end;

function TJvCaptionButtonActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TJvCaptionButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

procedure TJvCaptionButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TJvCaptionButtonActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;

procedure TJvCaptionButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

procedure TJvCaptionButtonActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;

procedure TJvCaptionButtonActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  UnloadMsimg32Dll;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
