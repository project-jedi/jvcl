{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionButton.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.

This unit is a merging of the original TJvCaptionButton, TJvaCaptionButton.
Merging done 2003-06-12 by Remko Bonte [remkobonte at myrealbox dot com]

All Rights Reserved.

Contributor(s):
  Andrei Prygounkov <a dot prygounkov at gmx dot de>, author of TJvaCaptionButton.
  Remko Bonte [remkobonte at myrealbox dot com], theme support, actions
  Olivier Sannier [obones at meloo dot com], caption hints.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Modified 2003-06-13 (p3):
- Fixed MouseUp X,Y inconsistentcy (did not report the same values as MouseDown)
- Added MouseMove handler
- Added ShowHint, ParentShowHint
- Fixed drawing of disabled MinimizeToTray icon as well as incorrect Font.Color in text drawing
- Added Assign
- Tested on W2k
- Demo (examples\CaptionBtn) updated and extended

Known Issues:

  * Button can disappear at design-time when switching themes.
  * With more buttons, button can appear hot while mouse is over another caption
    button.
  * Still some flicker while resizing due to wrong FButtonRect, see comment
    at HandleNCPaintBefore.
  * Buttons on small caption (BorderStyle in [bsSizeToolWin, bsToolWin]) looks
    ugly.
  * Only tested on XP.


-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvCaptionButton;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  ActnList, ImgList,
  JvComponent, JvWndProcHook;

type
  {$IFNDEF COMPILER6_UP}
  TWMNCPaint = packed record
    Msg: Cardinal;
    RGN: HRGN;
    Unused: Longint;
    Result: Longint;
  end;
  {$ENDIF COMPILER6_UP}

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
    FSaveRgn: HRgn;
    FShowHint: Boolean;
    FParentShowHint: Boolean;

    {tool tip specific}
    FToolTipHandle: THandle;
    {tool tip specific end}

    {$IFDEF JVCLThemesEnabled}
    FCaptionActive: Boolean;
    FForceDrawSimple: Boolean;
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

implementation

uses
  CommCtrl, Buttons, SysUtils,
  JvThemes,
  {$IFDEF JVCLThemesEnabled}
  UxTheme,
  JvJVCLUtils, JvJCLUtils,
  {$ENDIF JVCLThemesEnabled}
  JvDsgnIntf, JvTypes, JvResources;

{$IFDEF JVCLThemesEnabled}

type
  TGlobalXPData = class
  private
    FButtons: TBitmap;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FIsThemed: Boolean;
    FBitmapValid: Boolean;
    FClientCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddClient;
    procedure RemoveClient;

    procedure Update;
    procedure DrawSimple(HDC: HDC; State: Integer; const DrawRect: TRect);
    procedure Draw(HDC: HDC; State: Integer; const DrawRect: TRect);
    property IsThemed: Boolean read FIsThemed;
  end;

{$ENDIF JVCLThemesEnabled}

const
  htCaptionButton = HTSIZELAST + 1;

  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

{$IFDEF JVCLThemesEnabled}

var
  GGlobalXPData: TGlobalXPData;

//=== Local procedures =======================================================

function GlobalXPData: TGlobalXPData;
begin
  if not Assigned(GGlobalXPData) then
    GGlobalXPData := TGlobalXPData.Create;

  Result := GGlobalXPData;
end;

function StrArrayIndex(const S: string; const SS: array of string): Integer;
begin
  Result := Low(SS);
  while (Result <= High(SS)) and not SameText(S, SS[Result]) do
    Inc(Result);
  if Result > High(SS) then
    Result := -1;
end;

function GetXPCaptionButtonBitmap(ABitmap: TBitmap): Boolean;
type
  TXPStyle = (xpBlue, xpMetallic, xpGreen);
const
  cDefaultFileName = 'Luna.msstyles';
  cStyleNames: array [TXPStyle] of string =
    ('NormalColor', 'Metallic', 'HomeStead');
  cResName: array [TXPStyle] of string =
    ('BLUE_CAPTIONBUTTON_BMP', 'METALLIC_CAPTIONBUTTON_BMP', 'HOMESTEAD_CAPTIONBUTTON_BMP');
var
  Handle: THandle;
  ThemeFileNameW, ColorBuffW: array [0..MAX_PATH] of WideChar;
  Style: Integer;
  OldError: Longint;
begin
  ThemeFileNameW[MAX_PATH] := #0;
  ColorBuffW[MAX_PATH] := #0;

  Result := UxTheme.GetCurrentThemeName(ThemeFileNameW, MAX_PATH, ColorBuffW, MAX_PATH, nil, 0) = S_OK;
  if not Result then
    Exit;

  Result := SameText(ExtractFileName(string(ThemeFileNameW)), cDefaultFileName);
  if not Result then
    Exit;

  Style := StrArrayIndex(string(ColorBuffW), cStyleNames);
  Result := Style >= 0;
  if not Result then
    Exit;

  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    Handle := LoadLibraryW(ThemeFileNameW);
    Result := Handle <> 0;
    if Result then
    try
      ABitmap.LoadFromResourceName(Handle, cResName[TXPStyle(Style)]);
      Result := (ABitmap.Width > 0) and (ABitmap.Height > 0);
    finally
      FreeLibrary(Handle);
    end;
  finally
    SetErrorMode(OldError);
  end;
end;

//=== TGlobalXPData ==========================================================

procedure TGlobalXPData.AddClient;
begin
  Inc(FClientCount);
end;

constructor TGlobalXPData.Create;
begin
  inherited Create;
  FButtons := TBitmap.Create;
  Update;
end;

destructor TGlobalXPData.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

procedure TGlobalXPData.Draw(HDC: HDC; State: Integer;
  const DrawRect: TRect);
begin
  if FBitmapValid then
    // We could call TransparentBlt() for windows systems >= 98
    StretchBltTransparent(HDC, DrawRect.Left, DrawRect.Top, DrawRect.Right - DrawRect.Left, DrawRect.Bottom -
      DrawRect.Top,
      FButtons.Canvas.Handle, 0, FButtonHeight * (State - 1),
      FButtonWidth, FButtonHeight, FButtons.Palette, clFuchsia)
  else
    DrawSimple(HDC, State, DrawRect);
end;

procedure TGlobalXPData.DrawSimple(HDC: HDC; State: Integer;
  const DrawRect: TRect);
const
  // Normal, Hot, Pushed, Disabled,
  cCaptionButton: array [0..3] of TThemedWindow = (twMinButtonNormal, twMinButtonHot,
    twMinButtonPushed, twMinButtonDisabled);
  cNormalButton: array [0..3] of TThemedButton = (tbPushButtonNormal, tbPushButtonHot,
    tbPushButtonPressed, tbPushButtonDisabled);
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
  ThemeServices.DrawElement(HDC, Details, DrawRect);

  { 1b. Draw the inner bit as a normal button }
  with DrawRect do
    DrawRgn := CreateRectRgn(Left + 1, Top + 1, Right - 1, Bottom - 1);
  try
    Details := ThemeServices.GetElementDetails(cNormalButton[State]);
    SelectClipRgn(HDC, DrawRgn);
    ThemeServices.DrawElement(HDC, Details, DrawRect);
    SelectClipRgn(HDC, 0);
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
  FIsThemed := ThemeServices.ThemesAvailable and IsThemeActive and IsAppThemed;
  if not FIsThemed then
    Exit;

  FBitmapValid := GetXPCaptionButtonBitmap(FButtons);

  if FBitmapValid then
  begin
    FButtonWidth := FButtons.Width;
    FButtonHeight := FButtons.Height div 8;
  end;
end;

{$ENDIF JVCLThemesEnabled}

//=== TJvCaptionButton =======================================================

constructor TJvCaptionButton.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    raise EJVCLException.Create(RsEOwnerMustBeTCustomForm);

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

  {$IFDEF JVCLThemesEnabled}
  GlobalXPData.AddClient;
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
    Alignment      := TJvCaptionButton(Source).Alignment;
    ButtonHeight   := TJvCaptionButton(Source).ButtonHeight;
    ButtonLeft     := TJvCaptionButton(Source).ButtonLeft;
    ButtonTop      := TJvCaptionButton(Source).ButtonTop;
    ButtonWidth    := TJvCaptionButton(Source).ButtonWidth;
    Caption        := TJvCaptionButton(Source).Caption;
    ShowHint       := TJvCaptionButton(Source).ShowHint;
    ParentShowHint := TJvCaptionButton(Source).ParentShowHint;
    Enabled        := TJvCaptionButton(Source).Enabled;
    Font           := TJvCaptionButton(Source).Font;
    Hint           := TJvCaptionButton(Source).Hint;
    ImageIndex     := TJvCaptionButton(Source).ImageIndex;
    Images         := TJvCaptionButton(Source).Images;
    Layout         := TJvCaptionButton(Source).Layout;
    Margin         := TJvCaptionButton(Source).Margin;
    Position       := TJvCaptionButton(Source).Position;
    Spacing        := TJvCaptionButton(Source).Spacing;
    Standard       := TJvCaptionButton(Source).Standard;
    // set toggle before down
    Toggle         := TJvCaptionButton(Source).Toggle;
    Down           := TJvCaptionButton(Source).Down;
    Visible        := TJvCaptionButton(Source).Visible;
    Exit;
  end;
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
  DrawText(ACanvas.Handle, PChar(Caption), -1, RectText, DT_CALCRECT or
    Alignments[FAlignment]);
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
begin
  if Wnd = 0 then
    Exit;

  { 0. Init some local vars }
  FNeedRecalculate := False;
  Style := GetWindowLong(Wnd, GWL_STYLE);
  FHasCaption := Style and WS_CAPTION = WS_CAPTION;
  if not FHasCaption then
    Exit;

  ExStyle := GetWindowLong(Wnd, GWL_EXSTYLE);
  FHasSmallCaption := ExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
  {$IFDEF JVCLThemesEnabled}
  FCaptionActive := (GetActiveWindow = Wnd) and IsForegroundTask;
  {$ENDIF JVCLThemesEnabled}

  if Style and WS_THICKFRAME = WS_THICKFRAME then
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
    FDefaultButtonWidth := FDefaultButtonHeight
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
          if ((Style and WS_MAXIMIZEBOX = 0) or (Style and WS_MINIMIZEBOX = 0))
            and (ExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP) then
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
      FActionLink.Execute{$IFDEF COMPILER6_UP}(Self){$ENDIF}
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

  if FToolTipHandle <> 0 then
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
  SendMessage(FToolTipHandle, TTM_ADDTOOL, 0, Integer(@ToolInfo));
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

    with FButtonRect do
    begin
      FBuffer.Width := Right - Left;
      FBuffer.Height := Bottom - Top;
    end;

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
  with FButtonRect do
    ClipRect := Rect(0, 0, Right - Left, Bottom - Top);

  Details := ThemeServices.GetElementDetails(CCaption[FHasSmallCaption, FCaptionActive]);
  ThemeServices.DrawElement(ACanvas.Handle, Details, CaptionRect, @ClipRect);
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCaptionButton.DrawButtonImage(ACanvas: TCanvas; ImageBounds: TRect);
begin
  if csDestroying in ComponentState then
    Exit;
  with ImageBounds do
    if IsImageVisible then
      Images.Draw(ACanvas, Left, Top, ImageIndex, Enabled);
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
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      Font := OldFont;
    end
    else
    begin
      OldFont := Font;
      Font := Self.Font;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
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
  with FButtonRect do
    DrawRect := Rect(0, 0, Right - Left, Bottom - Top);

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

    if ForceDrawSimple then
      GlobalXPData.DrawSimple(ACanvas.Handle, State, DrawRect)
    else
      GlobalXPData.Draw(ACanvas.Handle, State, DrawRect);
  end
  else
  {$ENDIF JVCLThemesEnabled}
    DrawButtonFace(ACanvas, DrawRect, 1, bsAutoDetect, False, FDown, False);

  { 2. Draw the text & picture }
  {$IFDEF JVCLThemesEnabled}
  if IsThemed then
  begin
    { 2a. If themed, only draw in the inner bit of the button using a clip region }
    with DrawRect do
      DrawRgn := CreateRectRgn(Left + 2, Top + 2, Right - 2, Bottom - 2);

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
  with FButtonRect do
    DrawRect := Rect(0, 0, Right - Left, Bottom - Top);

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
    begin
      ACanvas.Brush.Color := clWindowText;
      with DrawRect do
        ACanvas.FillRect(Rect(Right - 7, Bottom - 5, Right - 4, Bottom - 3));
    end
    else
    begin
      ACanvas.Brush.Color := clBtnHighlight;
      with DrawRect do
        ACanvas.FillRect(Rect(Right - 6, Bottom - 4, Right - 3, Bottom - 2));
      ACanvas.Brush.Color := clBtnShadow;
      with DrawRect do
        ACanvas.FillRect(Rect(Right - 7, Bottom - 5, Right - 4, Bottom - 3));
    end;
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
  SendMessage(FToolTipHandle, TTM_RELAYEVENT, 0, Integer(@ForwardMsg));
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
  //  with TWmMouseMove(Msg) do
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
  if RectInRegion(FSaveRgn, LButtonRect) then
  begin
    { ..If so remove the button rectangle from the region (otherwise the caption
      background would be drawn over the button, which causes flicker) }
    with LButtonRect do
      DrawRgn := CreateRectRgn(Left, Top, Right, Bottom);
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
  Result := (Msg.NMHdr.Code = TTN_NEEDTEXT) and (Msg.NMHdr.hWndFrom = FToolTipHandle);

  if Result and (ShowHint or (ParentShowHint and ParentForm.ShowHint)) then
  begin
    // get cursor position
    GetCursorPos(CurPos);
    GetWindowRect(ParentFormHandle, WindowRect);
    LButtonRect := FButtonRect;
    OffsetRect(LButtonRect, WindowRect.Left, WindowRect.Top);

    // if the mouse is in the area of the button
    if PtInRect(LButtonRect, CurPos) then
      if Msg.NMHdr.code = TTN_NEEDTEXTA then
      begin
        with PNMTTDispInfoA(Msg.NMHdr)^ do
        begin
          // then we return the hint
          lpszText := PChar(FHint);
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
    SendMessage(FTooltipHandle, TTM_POP, 0, 0);
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
  if FHeight <> Value then
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
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
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
  if FWidth <> Value then
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
    UnregisterWndProcHook(P, WndProcAfter, hoAfterMsg);
    UnregisterWndProcHook(P, WndProcBefore, hoBeforeMsg);
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
  with FButtonRect do
    FClickRect := Rect(Left - 2, Top - 2, Right + 1, Bottom + 2);
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
        ThemeServices.ApplyThemeChange;
        GlobalXPData.Update;
        {$ENDIF JVCLThemesEnabled}
      end;
    CM_SYSFONTCHANGED:
      begin
        FNeedRecalculate := True;
        {$IFDEF JVCLThemesEnabled}
        { force theme data refresh, needed when

          * Non-themed application and switching system font size }
        if not ThemeServices.ThemesEnabled then
          ThemeServices.ApplyThemeChange;
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
  else
    Result := False;
  end;
end;

{$IFDEF JVCLThemesEnabled}
function TJvCaptionButton.GetIsThemed: Boolean;
begin
  Result := GlobalXPData.IsThemed;
end;
{$ENDIF JVCLThemesEnabled}

//=== TJvCaptionButtonActionLink =============================================

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

end.

