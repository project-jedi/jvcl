{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBalloonHint.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Remko Bonte <remkobonte@myrealbox.com>
Portions created by Remko Bonte are Copyright (C) 2002 Remko Bonte.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-12-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * Only dropdown shadow for windows xp systems.
  * Only custom animation for windows xp systems, because of use of window region.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBalloonHint;

interface

uses
  Windows, Controls, Classes, Graphics, Messages, Forms, ImgList,
  JvComponent;

type
  TJvStemSize = (ssSmall, ssNormal, ssLarge);
  TJvIconKind = (ikCustom, ikNone, ikApplication, ikError, ikInformation, ikQuestion, ikWarning);
  TJvBalloonOption = (boUseDefaultHeader, boUseDefaultIcon, boUseDefaultImageIndex,
    boShowCloseBtn, boCustomAnimation, boPlaySound);
  TJvBalloonOptions = set of TJvBalloonOption;
  TJvApplicationHintOption = (ahShowHeaderInHint, ahShowIconInHint, ahUseBalloonAsHint,
    ahPlaySound);
  TJvApplicationHintOptions = set of TJvApplicationHintOption;
  TJvBalloonPosition = (bpAuto, bpLeftDown, bpRightDown, bpLeftUp, bpRightUp);
  TJvAnimationStyle = (atNone, atSlide, atRoll, atRollHorNeg, atRollHorPos, atRollVerNeg,
    atRollVerPos, atSlideHorNeg, atSlideHorPos, atSlideVerNeg, atSlideVerPos, atCenter, atBlend);

  TJvBalloonHint = class;

  PHintData = ^THintData;
  THintData = record
    RAnchorWindow: TCustomForm;
    { Position of the top-left edge of the window balloon inside the client
      rect of the anchor window (Used to move the balloon window if the
      anchor window moves): }
    RAnchorPosition: TPoint;
    { Position of the stem point inside the client rect of the balloon window
      (Used the check on resize of the anchor window whether the stem point is
      still inside the balloon window): }
    RStemPointPosition: TPoint;
    RHeader: string;
    RHint: string;
    RIconKind: TJvIconKind;
    RImageIndex: TImageIndex;
    RVisibleTime: Integer;
    RShowCloseBtn: Boolean;
    RAnimationStyle: TJvAnimationStyle;
    RAnimationTime: Cardinal;
    { If the position of the balloon needs to be changed - for example if
      DefaultBalloonPosition = bpAuto - RSwitchHeight indicates how much we
      change the vertical position; if the balloon is an application hint,
      RSwitchHeight is the height of the cursor; if the balloon is attached to
      a control, RSwitchHeight is the height of that control }
    RSwitchHeight: Integer;
  end;

  TJvBalloonWindow = class(THintWindow)
  private
    FCurrentPosition: TJvBalloonPosition;
    FDeltaY: Integer;
    FSwitchHeight: Integer;

    FShowIcon: Boolean;
    FShowHeader: Boolean;

    FMsg: string;
    FHeader: string;
    FMessageTop: Integer;
    FTipHeight: Integer;
    FTipWidth: Integer;
    FTipDelta: Integer;
    FImageSize: TSize;
    function GetStemPointPosition: TPoint;
    function GetStemPointPositionInRect(const ARect: TRect): TPoint;
  protected
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$IFNDEF COMPILER6_UP}
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF COMPILER6_UP}
    procedure NCPaint(DC: HDC); override;
    {$ENDIF}
    procedure Paint; override;

    function CreateRegion: HRGN;
    procedure UpdateRegion;
    procedure CalcAutoPosition(var ARect: TRect);
    procedure CheckPosition(var ARect: TRect);

    function CalcOffset(const ARect: TRect): TPoint;
    function CalcHeaderRect(MaxWidth: Integer): TRect; virtual;
    function CalcMsgRect(MaxWidth: Integer): TRect; virtual;
    procedure Init(AData: Pointer); virtual;
  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    property StemPointPosition: TPoint read GetStemPointPosition;
  end;

  TJvBalloonWindowEx = class(TJvBalloonWindow)
  private
    FCtrl: TJvBalloonHint;
    FCloseBtnRect: TRect;
    FCloseState: Cardinal;
    FImageIndex: TImageIndex;
    FIconKind: TJvIconKind;
    FAnimationTime: Cardinal;
    FAnimationStyle: TJvAnimationStyle;

    FShowCloseBtn: Boolean;
    FIsAnchored: Boolean;
  protected
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMActivateApp(var Msg: TWMActivateApp); message WM_ACTIVATEAPP;

    procedure Paint; override;

    procedure InternalActivateHint(var Rect: TRect; const AHint: string);
    procedure MoveWindow(NewPos: TPoint);
    procedure ChangeCloseState(const AState: Cardinal);

    function CalcHeaderRect(MaxWidth: Integer): TRect; override;
    procedure Init(AData: Pointer); override;
  end;

  TJvBalloonHint = class(TJvComponent)
  private
    FHint: TJvBalloonWindowEx;
    FActive: Boolean;
    FTimerHandle: Word;
    FOptions: TJvBalloonOptions;
    FImages: TCustomImageList;
    FDefaultHeader: string;
    FDefaultIcon: TJvIconKind;
    FDefaultImageIndex: TImageIndex;
    FData: THintData;
    FApplicationHintOptions: TJvApplicationHintOptions;
    FDefaultBalloonPosition: TJvBalloonPosition;
    FCustomAnimationTime: Cardinal;
    FCustomAnimationStyle: TJvAnimationStyle;
    FOldHintWindowClass: THintWindowClass;
    procedure SetOptions(const Value: TJvBalloonOptions);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetApplicationHintOptions(const Value: TJvApplicationHintOptions);
  protected
    function HookProc(var Msg: TMessage): Boolean;
    procedure Hook;
    procedure UnHook;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure StartHintTimer(Value: Integer);
    procedure StopHintTimer;

    procedure InternalActivateHintPos;
    procedure InternalActivateHint(ACtrl: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ActivateHint(ACtrl: TControl; const AHint: string; const AHeader: string = '';
      const VisibleTime: Integer = 5000); overload;
    procedure ActivateHint(ACtrl: TControl; const AHint: string; const AImageIndex: TImageIndex;
      const AHeader: string = ''; const VisibleTime: Integer = 5000); overload;
    procedure ActivateHint(ACtrl: TControl; const AHint: string; const AIconKind: TJvIconKind;
      const AHeader: string = ''; const VisibleTime: Integer = 5000); overload;
    procedure ActivateHintPos(AAnchorWindow: TCustomForm; AAnchorPosition: TPoint;
      const AHeader, AHint: string; const VisibleTime: Integer = 5000;
      const AIconKind: TJvIconKind = ikInformation; const AImageIndex: TImageIndex = -1);
    procedure ActivateHintRect(ARect: TRect; const AHeader, AHint: string;
      const VisibleTime: Integer = 5000; const AIconKind: TJvIconKind = ikInformation;
      const AImageIndex: TImageIndex = -1);
    procedure CancelHint;
  published
    property CustomAnimationStyle: TJvAnimationStyle read FCustomAnimationStyle write
      FCustomAnimationStyle default atBlend;
    property CustomAnimationTime: Cardinal read FCustomAnimationTime write FCustomAnimationTime
      default 100;
    property DefaultBalloonPosition: TJvBalloonPosition read FDefaultBalloonPosition write
      FDefaultBalloonPosition default bpAuto;
    property DefaultImageIndex: TImageIndex read FDefaultImageIndex write FDefaultImageIndex
      default -1;
    property DefaultHeader: string read FDefaultHeader write FDefaultHeader;
    property DefaultIcon: TJvIconKind read FDefaultIcon write FDefaultIcon default ikInformation;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TJvBalloonOptions read FOptions write SetOptions default [boShowCloseBtn];
    property ApplicationHintOptions: TJvApplicationHintOptions read FApplicationHintOptions write
      SetApplicationHintOptions default [ahShowHeaderInHint, ahShowIconInHint];
  end;

implementation

uses
  SysUtils, CommCtrl, Registry, Math, MMSystem, // needed for sndPlaySound
  ComCtrls, // needed for GetComCtlVersion
  {$IFDEF JVCLThemesEnabled}
  Themes,
  {$ENDIF}
  JvWndProcHook;

const
  { TJvStemSize = (ssSmall, ssNormal, ssLarge);
    ssLarge isn't used (yet)
  }
  CTipHeight: array [TJvStemSize] of Integer = (8, 16, 24);
  CTipWidth: array [TJvStemSize] of Integer = (8, 16, 24);
  CTipDelta: array [TJvStemSize] of Integer = (16, 15, 17);

type
  TGlobalCtrl = class(TComponent)
  private
    FMainCtrl: TJvBalloonHint;
    FDefaultImages: TImageList;
    FSounds: array [TJvIconKind] of string;
    FNeedUpdateBkColor: Boolean;
    FBkColor: TColor;
    function GetMainCtrl: TJvBalloonHint;
    procedure GetDefaultSounds;
    procedure GetDefaultImages;
    procedure SetMainCtrl(ACtrl: TJvBalloonHint);
    procedure SetBkColor(const Value: TColor);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Instance: TGlobalCtrl;
    function HintImageSize: TSize; overload;
    function HintImageSize(const AIconKind: TJvIconKind;
      const AImageIndex: TImageIndex): TSize; overload;
    procedure DrawHintImage(Canvas: TCanvas; X, Y: Integer; const ABkColor: TColor); overload;
    procedure DrawHintImage(Canvas: TCanvas; X, Y: Integer; const AIconKind: TJvIconKind;
      const AImageIndex: TImageIndex; const ABkColor: TColor); overload;
    procedure PlaySound(const AIconKind: TJvIconKind);

    property MainCtrl: TJvBalloonHint read GetMainCtrl write SetMainCtrl;
    property BkColor: TColor read FBkColor write SetBkColor;
  end;

var
  GGlobalCtrl: TGlobalCtrl = nil;

procedure HintTimerProc(Wnd: HWND; Msg, TimerID, SysTime: Longint); stdcall;
begin
  try
    TGlobalCtrl.Instance.MainCtrl.CancelHint;
  except
    Application.HandleException(Application);
  end;
end;

function WorkAreaRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

{$IFNDEF COMPILER6_UP}

resourcestring
  SParentRequired = 'Control ''%s'' has no parent window';
  SParentGivenNotAParent = 'Parent given is not a parent of ''%s''';

const
  SPI_GETTOOLTIPANIMATION = $1016;
  {$EXTERNALSYM SPI_GETTOOLTIPANIMATION}
  SPI_GETTOOLTIPFADE = $1018;
  {$EXTERNALSYM SPI_GETTOOLTIPFADE}

type
  TAnimateWindowProc = function(HWND: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall;

var
  AnimateWindowProc: TAnimateWindowProc = nil;

{$ENDIF}

function IsWinXP_UP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    (Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
end;

{$IFDEF COMPILER6_UP}

function InternalClientToParent(AControl: TControl; const Point: TPoint;
  AParent: TWinControl): TPoint;
begin
  Result := AControl.ClientToParent(Point, AParent);
end;

{$ELSE}

function InternalClientToParent(AControl: TControl; const Point: TPoint;
  AParent: TWinControl): TPoint;
var
  LParent: TWinControl;
begin
  if AParent = nil then
    AParent := AControl.Parent;
  if AParent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [AControl.Name]);
  Result := Point;
  Inc(Result.X, AControl.Left);
  Inc(Result.Y, AControl.Top);
  LParent := AControl.Parent;
  while LParent <> nil do
  begin
    if LParent.Parent <> nil then
    begin
      Inc(Result.X, LParent.Left);
      Inc(Result.Y, LParent.Top);
    end;
    if LParent = AParent then
      Break
    else
      LParent := LParent.Parent;
  end;
  if LParent = nil then
    raise EInvalidOperation.CreateFmt(SParentGivenNotAParent, [AControl.Name]);
end;

{$ENDIF COMPILER6_UP}

//=== TJvBalloonWindow =======================================================

procedure TJvBalloonWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    ShowWindow(Handle, SW_HIDE);

  CheckPosition(Rect);

  Inc(Rect.Bottom, 4);
  UpdateBoundsRect(Rect);
  Dec(Rect.Bottom, 4);

  UpdateRegion;

  with TGlobalCtrl.Instance do
    if ahPlaySound in MainCtrl.ApplicationHintOptions then
      PlaySound(MainCtrl.DefaultIcon);
  inherited;
end;

procedure TJvBalloonWindow.CalcAutoPosition(var ARect: TRect);
var
  NewPosition: TJvBalloonPosition;
  ScreenRect: TRect;
  LStemPointPosition: TPoint;
begin

  { bpAuto returns the same value as bpLeftDown; bpLeftDown is choosen
    arbitrary }
  FCurrentPosition := bpLeftDown;
  ScreenRect := WorkAreaRect;

  { Note: 2*(Left + Width div 2) = 2*(Left + (Right-Left) div 2) ~=
          2*Left + (Right-Left) = Left + Right;

          Thus multiply everything with 2

         Monitor:
     |---------------|
     |       |       |
     |   1   |   2   |
     |       |       |
     |---------------|
     |       |       |
     |   3   |   4   |
     |       |       |
     |---------------|

  }
  with GetStemPointPositionInRect(ARect) do
    LStemPointPosition := Point(X * 2, Y * 2);

  if LStemPointPosition.Y < ScreenRect.Top + ScreenRect.Bottom then
  begin
    if LStemPointPosition.X < ScreenRect.Left + ScreenRect.Right then
      { 1 }
      NewPosition := bpLeftUp
    else
      { 2 }
      NewPosition := bpRightUp;
  end
  else
  begin
    if LStemPointPosition.X < ScreenRect.Left + ScreenRect.Right then
      { 3 }
      NewPosition := bpLeftDown
    else
      { 4 }
      NewPosition := bpRightDown;
  end;

  if NewPosition <> FCurrentPosition then
  begin
    { Reset the offset.. }
    with CalcOffset(ARect) do
      OffsetRect(ARect, -X, -Y);

    FCurrentPosition := NewPosition;

    { ..and set the offset }
    with CalcOffset(ARect) do
      OffsetRect(ARect, X, Y);
  end;

  case FCurrentPosition of
    bpLeftDown, bpRightDown: FDeltaY := FTipHeight;
    bpLeftUp, bpRightUp: FDeltaY := 0;
  end;
end;

function TJvBalloonWindow.CalcHeaderRect(MaxWidth: Integer): TRect;
begin
  if FShowHeader then
  begin
    Result := Rect(0, 0, MaxWidth, 0);
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    DrawText(Canvas.Handle, PChar(FHeader), -1, Result, DT_CALCRECT or DT_LEFT or
      DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);

    { Other }
    Inc(Result.Right, 13);
    Inc(Result.Bottom, 11);

    if FShowIcon then
      with FImageSize do
      begin
        { Include image }
        Inc(Result.Right, cx + 8);
        Result.Bottom := Max(Result.Bottom, cy + 11);
      end;
  end
  else
  if FShowIcon then
    with FImageSize do
      Result := Rect(0, 0, cx + 11, cy + 11)
  else
    SetRectEmpty(Result);
end;

function TJvBalloonWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  MsgRect, HeaderRect: TRect;
  StemSize: TJvStemSize;
begin
  Init(AData);

  FMsg := AHint;

  { Calc HintRect }
  MsgRect := CalcMsgRect(MaxWidth);

  { Calc HeaderRect }
  HeaderRect := CalcHeaderRect(MaxWidth);
  if IsRectEmpty(HeaderRect) then
  begin
    HeaderRect.Bottom := 9;
    FMessageTop := 7;
    StemSize := ssSmall;
  end
  else
  begin
    Inc(HeaderRect.Right, 12);
    FMessageTop := HeaderRect.Bottom + 1;
    StemSize := ssNormal;
  end;

  FTipHeight := CTipHeight[StemSize];
  FTipWidth := CTipWidth[StemSize];
  FTipDelta := CTipDelta[StemSize];

  { Combine }
  Result := Rect(0, 0, Max(MsgRect.Right, HeaderRect.Right),
    HeaderRect.Bottom + MsgRect.Bottom + FTipHeight + 13);

  with CalcOffset(Result) do
    OffsetRect(Result, X, Y);
  { bpAuto returns the same value as bpLeftDown; bpLeftDown is choosen
    arbitrary }
  case FCurrentPosition of
    bpAuto, bpLeftDown, bpRightDown:
      FDeltaY := FTipHeight;
    bpLeftUp, bpRightUp:
      FDeltaY := 0;
  end;
end;

function TJvBalloonWindow.CalcMsgRect(MaxWidth: Integer): TRect;
begin
  if FMsg > '' then
  begin
    Result := Rect(0, 0, MaxWidth, 0);
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    DrawText(Canvas.Handle, PChar(FMsg), -1, Result, DT_CALCRECT or DT_LEFT or
      DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);

    { Other }
    Inc(Result.Right, 27);
  end
  else
    SetRectEmpty(Result);
end;

function TJvBalloonWindow.CalcOffset(const ARect: TRect): TPoint;
begin
  with ARect do
    case FCurrentPosition of
      { bpAuto returns the same value as bpLeftDown; bpLeftDown is choosen
        arbitrary }
      bpAuto, bpLeftDown:
        Result := Point(Left - Right + FTipDelta, 0);
      bpRightDown:
        Result := Point(-FTipDelta, 0);
      bpLeftUp:
        Result := Point(Left - Right + FTipDelta, Top - Bottom - FSwitchHeight);
      bpRightUp:
        Result := Point(-FTipDelta, Top - Bottom - FSwitchHeight);
    end;
end;

procedure TJvBalloonWindow.CheckPosition(var ARect: TRect);
var
  NewPosition: TJvBalloonPosition;
  ScreenRect: TRect;
begin
  if FCurrentPosition = bpAuto then
    CalcAutoPosition(ARect);

  NewPosition := FCurrentPosition;
  ScreenRect := WorkAreaRect;

  if ARect.Bottom > ScreenRect.Bottom - ScreenRect.Top then
  begin
    if NewPosition = bpLeftDown then
      NewPosition := bpLeftUp
    else
    if NewPosition = bpRightDown then
      NewPosition := bpRightUp;
  end;
  if ARect.Right > ScreenRect.Right - ScreenRect.Left then
  begin
    if NewPosition = bpRightDown then
      NewPosition := bpLeftDown
    else
    if NewPosition = bpRightUp then
      NewPosition := bpLeftUp;
  end;
  if ARect.Left < ScreenRect.Left then
  begin
    if NewPosition = bpLeftDown then
      NewPosition := bpRightDown
    else
    if NewPosition = bpLeftUp then
      NewPosition := bpRightUp;
  end;
  if ARect.Top < ScreenRect.Top then
  begin
    if NewPosition = bpLeftUp then
      NewPosition := bpLeftDown
    else
    if NewPosition = bpRightUp then
      NewPosition := bpRightDown;
  end;

  if NewPosition <> FCurrentPosition then
  begin
    { Reset the offset.. }
    with CalcOffset(ARect) do
      OffsetRect(ARect, -X, -Y);
    FCurrentPosition := NewPosition;

    { ..and set the offset }
    with CalcOffset(ARect) do
      OffsetRect(ARect, X, Y);
  end;

  case FCurrentPosition of
    bpLeftDown, bpRightDown: FDeltaY := FTipHeight;
    bpLeftUp, bpRightUp: FDeltaY := 0;
  end;
end;

procedure TJvBalloonWindow.CMShowingChanged(var Msg: TMessage);
begin
  { In response of RecreateWnd, SetParentWindow calls, only respond when visible }
  { Actually only necessairy for TJvBalloonWindow not for TJvBalloonWindowEx }
  if Showing then
    UpdateRegion;
  inherited;
end;

procedure TJvBalloonWindow.CMTextChanged(var Msg: TMessage);
begin
  {inherited;}
end;

procedure TJvBalloonWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);

  { Drop shadow in combination with custom animation may cause blurry effect,
    no solution.
  }
  if IsWinXP_UP then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

function TJvBalloonWindow.CreateRegion: HRGN;
var
  Rect: TRect;
  RegionRound, RegionTip: HRGN;
  ptTail: array [0..2] of TPoint;
begin
  SetRect(Rect, 0, 0, Width, Height);

  case FCurrentPosition of
    bpLeftDown:
      begin
        {         0
                / |
               /  |
              /   |
             2----1
        }

        ptTail[0] := Point(Rect.Right - (FTipDelta + 1), 0);
        ptTail[1] := Point(Rect.Right - (FTipDelta + 1), FTipHeight + 1);
        ptTail[2] := Point(Rect.Right - (FTipDelta + FTipWidth + 2), FTipHeight + 1);
      end;
    bpRightDown:
      begin
        {    0
             | \
             |  \
             |   \
             1----2
        }
        ptTail[0] := Point(FTipDelta + 1, 0);
        ptTail[1] := Point(FTipDelta + 1, FTipHeight + 1);
        ptTail[2] := Point(FTipDelta + FTipWidth + 2, FTipHeight + 1);
      end;
    bpLeftUp:
      begin
        {    2----1
              \   |
               \  |
                \ |
                  0
        }

        ptTail[0] := Point(Rect.Right - (FTipDelta + 1), Rect.Bottom + 1);
        ptTail[1] := Point(Rect.Right - (FTipDelta + 1), Rect.Bottom - (FTipHeight + 1));
        ptTail[2] := Point(Rect.Right - (FTipDelta + FTipWidth + 2), Rect.Bottom - (FTipHeight + 1));
      end;
    bpRightUp:
      begin
        {    1----2
             |   /
             |  /
             | /
             0
        }

        ptTail[0] := Point(FTipDelta + 1, Rect.Bottom);
        ptTail[1] := Point(FTipDelta + 1, Rect.Bottom - (FTipHeight + 1));
        ptTail[2] := Point(FTipDelta + FTipWidth + 2, Rect.Bottom - (FTipHeight + 1));
      end;
  end;

  RegionTip := CreatePolygonRgn(ptTail, 3, WINDING);
  case FCurrentPosition of
    bpLeftDown, bpRightDown:
      RegionRound := CreateRoundRectRgn(1, FTipHeight + 1, Width, Height - 3, 11, 11);
  else {bpLeftUp, bpRightUp:}
    RegionRound := CreateRoundRectRgn(1, 1, Rect.Right, Rect.Bottom - FTipHeight, 11, 11);
  end;
  Result := CreateRectRgn(0, 0, 1, 1);

  CombineRgn(Result, RegionTip, RegionRound, RGN_OR);
  DeleteObject(RegionTip);
  DeleteObject(RegionRound);
end;

function TJvBalloonWindow.GetStemPointPosition: TPoint;
begin
  Result := GetStemPointPositionInRect(BoundsRect);
end;

function TJvBalloonWindow.GetStemPointPositionInRect(
  const ARect: TRect): TPoint;
begin
  { bpAuto returns the same value as bpLeftDown; bpLeftDown is choosen
    arbitrary }
  with ARect do
    case FCurrentPosition of
      bpAuto, bpLeftDown:
        Result := Point(Right - FTipDelta, Top);
      bpRightDown:
        Result := Point(Left + FTipDelta, Top);
      bpLeftUp:
        Result := Point(Right - FTipDelta, Bottom);
      bpRightUp:
        Result := Point(Left + FTipDelta, Bottom);
    end;
end;

procedure TJvBalloonWindow.Init(AData: Pointer);
begin
  with TGlobalCtrl.Instance.MainCtrl do
  begin
    FShowIcon := (ahShowIconInHint in ApplicationHintOptions) and
      (DefaultIcon <> ikNone) and
      ((DefaultIcon <> ikCustom) or (DefaultImageIndex > -1));
    FShowHeader := (ahShowHeaderInHint in ApplicationHintOptions) and (DefaultHeader > '');
    FHeader := DefaultHeader;
    FCurrentPosition := DefaultBalloonPosition;
  end;

  FImageSize := TGlobalCtrl.Instance.HintImageSize;
  FSwitchHeight := GetSystemMetrics(SM_CYCURSOR);
end;

{$IFDEF COMPILER6_UP}
procedure TJvBalloonWindow.NCPaint(DC: HDC);
begin
  { Do nothing, thus prevent TJvHintWindow from drawing }
end;
{$ENDIF}

procedure TJvBalloonWindow.Paint;
var
  HintRect: TRect;
  HeaderRect: TRect;
begin
  if FShowIcon then
    TGlobalCtrl.Instance.DrawHintImage(Canvas, 12, FDeltaY + 8, Color);

  if FMsg > '' then
  begin
    HintRect := ClientRect;
    Inc(HintRect.Left, 12);
    Inc(HintRect.Top, FDeltaY + FMessageTop);
    Canvas.Font.Color := Screen.HintFont.Color;
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    DrawText(Canvas.Handle, PChar(FMsg), -1, HintRect, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;

  if FShowHeader then
  begin
    HeaderRect := ClientRect;
    Inc(HeaderRect.Left, 12);
    if FShowIcon then
      Inc(HeaderRect.Left, FImageSize.cx + 8);
    Inc(HeaderRect.Top, FDeltaY + 8);
    Canvas.Font.Color := Screen.HintFont.Color;
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    DrawText(Canvas.Handle, PChar(FHeader), -1, HeaderRect, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;
end;

procedure TJvBalloonWindow.UpdateRegion;
var
  Region: HRGN;
begin
  Region := CreateRegion;
  if SetWindowRgn(Handle, Region, False) = 0 then
    DeleteObject(Region);
  { MSDN: After a successful call to SetWindowRgn, the system owns the region
    specified by the region handle hRgn. The system does not make a copy of
    the region. Thus, you should not make any further function calls with
    this region handle. In particular, do not delete this region handle. The
    system deletes the region handle when it no longer needed. }
end;

procedure TJvBalloonWindow.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Brush, BrushBlack: HBRUSH;
  Region: HRGN;
begin
  Brush := CreateSolidBrush(ColorToRGB(Color));
  BrushBlack := CreateSolidBrush(0);
  try
    Region := CreateRegion;
    OffsetRgn(Region, -1, -1);
    FillRgn(Msg.DC, Region, Brush);
    FrameRgn(Msg.DC, Region, BrushBlack, 1, 1);
    DeleteObject(Region);
  finally
    DeleteObject(Brush);
    DeleteObject(BrushBlack);
  end;
  Msg.Result := 1;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvBalloonWindow.WMNCPaint(var Msg: TMessage);
begin
  { Do nothing, thus prevent TJvHintWindow from drawing }
end;
{$ENDIF}

//=== TJvBalloonHint =========================================================

constructor TJvBalloonHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FHint := TJvBalloonWindowEx.Create(Self);
  FHint.FCtrl := Self;
  FHint.Visible := False;
  FOptions := [boShowCloseBtn];
  FApplicationHintOptions := [ahShowHeaderInHint, ahShowIconInHint];
  FDefaultIcon := ikInformation;
  FDefaultBalloonPosition := bpAuto;
  FDefaultImageIndex := -1;
  FCustomAnimationTime := 100;
  FCustomAnimationStyle := atBlend;
  TGlobalCtrl.Instance.MainCtrl := Self;
end;

destructor TJvBalloonHint.Destroy;
begin
  CancelHint;
  StopHintTimer;
  { This will implicitly reset the global HintWindowClass var.: }
  ApplicationHintOptions := [];
  inherited Destroy;
end;

procedure TJvBalloonHint.ActivateHint(ACtrl: TControl; const AHint,
  AHeader: string; const VisibleTime: Integer);
begin
  if not Assigned(ACtrl) then
    Exit;

  CancelHint;

  with FData do
  begin
    RHint := AHint;
    RHeader := AHeader;
    RVisibleTime := VisibleTime;
    RIconKind := ikNone;
  end;

  InternalActivateHint(ACtrl);
end;

procedure TJvBalloonHint.ActivateHint(ACtrl: TControl; const AHint: string;
  const AImageIndex: TImageIndex; const AHeader: string;
  const VisibleTime: Integer);
begin
  if not Assigned(ACtrl) then
    Exit;

  CancelHint;

  with FData do
  begin
    RHint := AHint;
    RIconKind := ikCustom;
    RImageIndex := AImageIndex;
    RHeader := AHeader;
    RVisibleTime := VisibleTime;
  end;

  InternalActivateHint(ACtrl);
end;

procedure TJvBalloonHint.ActivateHint(ACtrl: TControl; const AHint: string;
  const AIconKind: TJvIconKind; const AHeader: string;
  const VisibleTime: Integer);
begin
  if not Assigned(ACtrl) then
    Exit;

  CancelHint;

  with FData do
  begin
    RHint := AHint;
    RIconKind := AIconKind;
    RImageIndex := -1;
    RHeader := AHeader;
    RVisibleTime := VisibleTime;
  end;

  InternalActivateHint(ACtrl);
end;

procedure TJvBalloonHint.ActivateHintPos(AAnchorWindow: TCustomForm; AAnchorPosition: TPoint;
  const AHeader, AHint: string; const VisibleTime: Integer; const AIconKind: TJvIconKind;
  const AImageIndex: TImageIndex);
begin
  CancelHint;

  with FData do
  begin
    RAnchorWindow := AAnchorWindow;
    RAnchorPosition := AAnchorPosition;
    RHeader := AHeader;
    RHint := AHint;
    RVisibleTime := VisibleTime;
    RIconKind := AIconKind;
    RImageIndex := AImageIndex;
    RSwitchHeight := 0;
  end;

  InternalActivateHintPos;
end;

procedure TJvBalloonHint.ActivateHintRect(ARect: TRect; const AHeader,
  AHint: string; const VisibleTime: Integer; const AIconKind: TJvIconKind;
  const AImageIndex: TImageIndex);
begin
  CancelHint;

  with FData do
  begin
    RAnchorWindow := nil;
    RAnchorPosition := Point((ARect.Left + ARect.Right) div 2, ARect.Bottom);
    RHeader := AHeader;
    RHint := AHint;
    RVisibleTime := VisibleTime;
    RIconKind := AIconKind;
    RImageIndex := AImageIndex;
    RSwitchHeight := ARect.Bottom - ARect.Top;
  end;

  InternalActivateHintPos;
end;

procedure TJvBalloonHint.CancelHint;
begin
  if not FActive then
    Exit;

  FActive := False;
  StopHintTimer;
  UnHook;

  if GetCapture = FHint.Handle then
    ReleaseCapture;
  { Ensure property Visible is set to False: }
  FHint.Hide;
  { If ParentWindow = 0, calling Hide won't trigger the CM_SHOWINGCHANGED message
    thus ShowWindow/SetWindowPos isn't called. We do it ourselfs: }
  if FHint.ParentWindow = 0 then
    ShowWindow(FHint.Handle, SW_HIDE);
end;

procedure TJvBalloonHint.Hook;
begin
  if Assigned(FData.RAnchorWindow) then
    RegisterWndProcHook(FData.RAnchorWindow, HookProc, hoBeforeMsg);
end;

function TJvBalloonHint.HookProc(var Msg: TMessage): Boolean;
begin
  Result := False;
  case Msg.Msg of
    WM_MOVE:
      with FData do
        FHint.MoveWindow(RAnchorWindow.ClientToScreen(RAnchorPosition));
    WM_SIZE:
      with FData do
        if not PtInRect(RAnchorWindow.ClientRect, RStemPointPosition) then
          CancelHint;
    WM_SHOWWINDOW:
      CancelHint;
    WM_CLOSE:
      CancelHint;
  end;
end;

procedure TJvBalloonHint.InternalActivateHint(ACtrl: TControl);
var
  LParentForm: TCustomForm;
begin
  if not Assigned(ACtrl) then
    Exit;

  LParentForm := GetParentForm(ACtrl);
  with ACtrl, FData do
  begin
    RAnchorWindow := LParentForm;
    if LParentForm = ACtrl then
      RAnchorPosition := Point(Width div 2, ClientHeight)
    else
      RAnchorPosition := InternalClientToParent(ACtrl, Point(Width div 2, Height), LParentForm);

    RSwitchHeight := ACtrl.Height;
  end;

  InternalActivateHintPos;
end;

procedure TJvBalloonHint.InternalActivateHintPos;
var
  Rect: TRect;
  Animate: BOOL;
begin
  with FData do
  begin
    { Use defaults if necessairy: }
    if boUseDefaultHeader in Options then
      RHeader := DefaultHeader;
    if boUseDefaultIcon in Options then
      RIconKind := DefaultIcon;
    if boUseDefaultImageIndex in Options then
      RImageIndex := DefaultImageIndex;
    RShowCloseBtn := boShowCloseBtn in Options;

    { Determine animation style }
    if not IsWinXP_UP then
      RAnimationStyle := atNone
    else
    if boCustomAnimation in Options then
    begin
      RAnimationStyle := FCustomAnimationStyle;
      RAnimationTime := FCustomAnimationTime;
    end
    else
    begin
      SystemParametersInfo(SPI_GETTOOLTIPANIMATION, 0, @Animate, 0);
      if Animate then
      begin
        SystemParametersInfo(SPI_GETTOOLTIPFADE, 0, @Animate, 0);
        if Animate then
          RAnimationStyle := atBlend
        else
          RAnimationStyle := atSlide;
      end
      else
        RAnimationStyle := atNone;
      RAnimationTime := 100;
    end;

    { Hook the anchor window }
    FActive := True;
    Hook;

    { Determine the size of the balloon rect, the stem point will be on
      position (0, 0) }
    Rect := FHint.CalcHintRect(Screen.Width, RHint, @FData);

    { Offset the rectangle to the anchor position }
    if Assigned(RAnchorWindow) then
      with RAnchorWindow.ClientToScreen(RAnchorPosition) do
        OffsetRect(Rect, X, Y)
    else
      with RAnchorPosition do
        OffsetRect(Rect, X, Y);

    if boPlaySound in Options then
      TGlobalCtrl.Instance.PlaySound(RIconKind);

    FHint.InternalActivateHint(Rect, RHint);

    { Now we can determine the actual anchor & stempoint position: }
    if Assigned(RAnchorWindow) then
    begin
      RAnchorPosition := RAnchorWindow.ScreenToClient(Rect.TopLeft);
      RStemPointPosition := RAnchorWindow.ScreenToClient(FHint.StemPointPosition);
    end
    else
    begin
      RAnchorPosition := Rect.TopLeft;
      RStemPointPosition := FHint.StemPointPosition;
    end;

    { Last call because of possible CancelHint call in StartHintTimer }
    if RVisibleTime > 0 then
      StartHintTimer(RVisibleTime);
    if GetCapture = 0 then
      SetCapture(FHint.Handle);
    ReleaseCapture;
  end;
end;

procedure TJvBalloonHint.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TJvBalloonHint.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  if Images <> nil then
    Images.FreeNotification(Self);
end;

procedure TJvBalloonHint.SetApplicationHintOptions(
  const Value: TJvApplicationHintOptions);
var
  ChangedOptions: TJvApplicationHintOptions;
  IsMainCtrl: Boolean;
begin
  if Value = FApplicationHintOptions then
    Exit;

  ChangedOptions := FApplicationHintOptions + Value - (FApplicationHintOptions * Value);
  IsMainCtrl := TGlobalCtrl.Instance.MainCtrl = Self;

  FApplicationHintOptions := Value;

  // boShowHeaderInHint, boShowIconInHint, boUseBalloonAsHint
  if (ahUseBalloonAsHint in ChangedOptions) and not (csDesigning in ComponentState) then
  begin
    { Safety check, this is always true unless you use more than 1 TJvBalloonHint
      control in a project, which is not a good idea }
    if not IsMainCtrl then
      { Flag can't be on if the control isn't the 'main' control }
      Exclude(FApplicationHintOptions, ahUseBalloonAsHint)
    else
    if ahUseBalloonAsHint in FApplicationHintOptions then
    begin
      { If flag is turned on then.. }
      FOldHintWindowClass := HintWindowClass;
      HintWindowClass := TJvBalloonWindow;
    end
    else
      { If flag is turned off then.. }
      HintWindowClass := FOldHintWindowClass;
  end;
end;

procedure TJvBalloonHint.SetOptions(const Value: TJvBalloonOptions);
begin
  if Value = FOptions then
    Exit;

  FOptions := Value;
end;

procedure TJvBalloonHint.StartHintTimer(Value: Integer);
begin
  StopHintTimer;
  FTimerHandle := SetTimer(0, 0, Value, @HintTimerProc);
  if FTimerHandle = 0 then
    CancelHint;
end;

procedure TJvBalloonHint.StopHintTimer;
begin
  if FTimerHandle <> 0 then
  begin
    KillTimer(0, FTimerHandle);
    FTimerHandle := 0;
  end;
end;

procedure TJvBalloonHint.UnHook;
begin
  if Assigned(FData.RAnchorWindow) then
    UnRegisterWndProcHook(FData.RAnchorWindow, HookProc, hoBeforeMsg);
end;

//=== TGlobalCtrl ============================================================

constructor TGlobalCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if IsWinXP_UP then
  begin
    FDefaultImages := TImageList.Create(nil);
    { According to MSDN flag ILC_COLOR32 needs to be included (?) }
    FDefaultImages.Handle := ImageList_Create(16, 16, ILC_COLOR32 or ILC_MASK, 4, 4);
  end
  else
    FDefaultImages := TImageList.CreateSize(16, 16);

  { Only need to update the background color in XP when using pre v6.0 ComCtl32.dll
    image lists }
  FNeedUpdateBkColor := IsWinXP_UP and (GetComCtlVersion < $00060000);

  if FNeedUpdateBkColor then
    FDefaultImages.BkColor := Application.HintColor
  else
    FDefaultImages.BkColor := clNone;

  FBkColor := Application.HintColor;

  GetDefaultImages;
  GetDefaultSounds;
end;

destructor TGlobalCtrl.Destroy;
begin
  FDefaultImages.Free;
  inherited Destroy;
end;

procedure TGlobalCtrl.DrawHintImage(Canvas: TCanvas; X, Y: Integer; const ABkColor: TColor);
begin
  DrawHintImage(Canvas, X, Y, MainCtrl.DefaultIcon, MainCtrl.DefaultImageIndex, ABkColor);
end;

procedure TGlobalCtrl.DrawHintImage(Canvas: TCanvas; X, Y: Integer;
  const AIconKind: TJvIconKind; const AImageIndex: TImageIndex; const ABkColor: TColor);
const
  CDefaultImages: array[TJvIconKind] of Integer = (-1, -1, 0, 1, 2, 3, 4);
begin
  case AIconKind of
    ikCustom:
      with MainCtrl do
        if not Assigned(Images) or (AImageIndex < 0) or (AImageIndex >= Images.Count) then
        begin
          BkColor := ABkColor;
          FDefaultImages.Draw(Canvas, X, Y, CDefaultImages[ikInformation]);
        end
        else
          Images.Draw(Canvas, X, Y, AImageIndex);
    ikNone:
      ;
  else
    begin
      BkColor := ABkColor;
      FDefaultImages.Draw(Canvas, X, Y, CDefaultImages[AIconKind]);
    end;
  end;
end;

procedure TGlobalCtrl.GetDefaultImages;
type
  TPictureType = (ptXP, ptNormal, ptSimple);
const
  { Get the images:

    For        From          ID   TJvIconKind    Spec
    ---------------------------------------------------------------------------
    Windows XP User32.dll   100   ikApplication  16x16 32x32 48x48 1,4,8,32 bpp
                            101   ikWarning
                            102   ikQuestion
                            103   ikError
                            104   ikInformation
                            105   ikApplication
    All (?)    comctl32.dll 20480 ikError        16x16 32x32 4 bpp
                            20481 ikInformation
                            20482 ikWarning
  }

  { ikApplication, ikError, ikInformation, ikQuestion, ikWarning }
  CIcons: array [TPictureType, ikApplication..ikWarning] of Integer = (
    (100, 103, 104, 102, 101),                             // XP
    (OIC_SAMPLE, 20480, 20481, OIC_QUES, 20482),           // Normal
    (OIC_SAMPLE, OIC_HAND, OIC_NOTE, OIC_QUES, OIC_BANG)   // Paranoid
    );
  CFlags: array [Boolean] of UINT = (0, LR_SHARED);
var
  IconKind: TJvIconKind;
  PictureType: TPictureType;
  IconHandle: THandle;
  Shared: Boolean;
  Modules: array [Boolean] of HMODULE;
begin
  PictureType := ptNormal;
  Modules[True] := 0;

  if IsWinXP_UP then
  begin
    Modules[False] := GetModuleHandle('user32.dll');
    if Modules[False] <> 0 then
      PictureType := ptXP
  end;

  if PictureType = ptNormal then
  begin
    Modules[False] := GetModuleHandle('comctl32.dll');
    if Modules[False] = 0 then
      PictureType := ptSimple;
  end;

  { Now   PictureType = ptXP     -> Modules = (user32.dll handle, 0)
          PictureType = ptNormal -> Modules = (comctl32.dll handle, 0)
          PictureType = ptSimple -> Modules = (0, 0)
  }

  for IconKind := Low(CIcons[PictureType]) to High(CIcons[PictureType]) do
  begin
    Shared := (PictureType = ptSimple) or
      (PictureType = ptNormal) and (IconKind in [ikApplication, ikQuestion]);
    IconHandle :=
      LoadImage(Modules[Shared], MakeIntResource(CIcons[PictureType, IconKind]),
      IMAGE_ICON, 16, 16, CFlags[Shared]);
    ImageList_AddIcon(FDefaultImages.Handle, IconHandle);
    { MSDN: Do not use DestroyIcon to destroy a shared icon. A shared icon is
      valid as long as the module from which it was loaded remains in memory }
    if not Shared then
      DestroyIcon(IconHandle);
  end;
end;

procedure TGlobalCtrl.GetDefaultSounds;
{ Taken from ActnMenus.pas }
var
  Registry: TRegistry;

  function ReadSoundSetting(KeyStr: string): string;
  var
    S: string;
  begin
    Registry.RootKey := HKEY_CURRENT_USER;
    Result := '';
    if Registry.OpenKeyReadOnly('\AppEvents\Schemes\Apps\.Default\' + KeyStr) then
    try
      S := Registry.ReadString('');
      SetLength(Result, 4096);
      SetLength(Result, ExpandEnvironmentStrings(PChar(S), PChar(Result), 4096) - 1);
    finally
      Registry.CloseKey;
    end;
  end;

begin
  Registry := TRegistry.Create;
  try
    FSounds[ikCustom] := ReadSoundSetting('SystemNotification\.Current');
    FSounds[ikNone] := FSounds[ikCustom];
    FSounds[ikApplication] := FSounds[ikCustom];
    FSounds[ikError] := ReadSoundSetting('SystemHand\.Current');
    FSounds[ikInformation] := ReadSoundSetting('SystemAsterisk\.Current');
    FSounds[ikQuestion] := ReadSoundSetting('SystemQuestion\.Current');
    FSounds[ikWarning] := ReadSoundSetting('SystemExclamation\.Current');
  finally
    Registry.Free;
  end;
end;

function TGlobalCtrl.GetMainCtrl: TJvBalloonHint;
begin
  if not Assigned(FMainCtrl) then
    FMainCtrl := TJvBalloonHint.Create(Self);
  Result := FMainCtrl;
end;

function TGlobalCtrl.HintImageSize: TSize;
begin
  Result := HintImageSize(MainCtrl.DefaultIcon, MainCtrl.DefaultImageIndex);
end;

function TGlobalCtrl.HintImageSize(const AIconKind: TJvIconKind;
  const AImageIndex: TImageIndex): TSize;
begin
  case AIconKind of
    ikCustom:
      with MainCtrl do
        if not Assigned(Images) or (AImageIndex < 0) or (AImageIndex >= Images.Count) then
        begin
          Result.cx := 16;
          Result.cy := 16;
        end
        else
        begin
          Result.cx := Images.Width;
          Result.cy := Images.Height;
        end;
    ikNone:
      begin
        Result.cx := 0;
        Result.cy := 0;
      end;
  else
    begin
      Result.cx := 16;
      Result.cy := 16;
    end;
  end;
end;

class function TGlobalCtrl.Instance: TGlobalCtrl;
begin
  if not Assigned(GGlobalCtrl) then
    GGlobalCtrl := TGlobalCtrl.Create(nil);
  Result := GGlobalCtrl;
end;

procedure TGlobalCtrl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FMainCtrl) and (Operation = opRemove) then
    MainCtrl := nil;
end;

procedure TGlobalCtrl.PlaySound(const AIconKind: TJvIconKind);
begin
  if Length(FSounds[AIconKind]) > 0 then
    sndPlaySound(PChar(FSounds[AIconKind]), SND_NOSTOP or SND_ASYNC);
end;

procedure TGlobalCtrl.SetBkColor(const Value: TColor);
begin
  if FNeedUpdateBkColor and (FBkColor <> Value) then
  begin
    { Icons in windows XP use an alpha channel to 'blend' with the background.
      If the background color changes, then the images must be redrawn,
      when using pre v6.0 ComCtl32.dll image lists
    }
    FBkColor := Value;
    FDefaultImages.Clear;
    FDefaultImages.BkColor := FBkColor;
    GetDefaultImages;
  end;
end;

procedure TGlobalCtrl.SetMainCtrl(ACtrl: TJvBalloonHint);
begin
  if FMainCtrl = ACtrl then
    Exit;

  if Assigned(FMainCtrl) then
  begin
    FMainCtrl.CancelHint;
    FMainCtrl.RemoveFreeNotification(Self);
    FMainCtrl.ApplicationHintOptions := FMainCtrl.ApplicationHintOptions - [ahUseBalloonAsHint];
  end;

  FMainCtrl := ACtrl;

  if Assigned(FMainCtrl) then
    FMainCtrl.FreeNotification(Self)
end;

//=== TJvBalloonWindowEx =====================================================

function TJvBalloonWindowEx.CalcHeaderRect(MaxWidth: Integer): TRect;
begin
  Result := inherited CalcHeaderRect(MaxWidth);
  if FShowCloseBtn then
  begin
    Inc(Result.Right, 20);
    if Result.Bottom < 20 then
      Result.Bottom := 20;
  end;
end;

procedure TJvBalloonWindowEx.ChangeCloseState(const AState: Cardinal);
{$IFDEF JVCLThemesEnabled}
var
  Details: TThemedElementDetails;
  Button: TThemedToolTip;
{$ENDIF}
begin
  if AState <> FCloseState then
  begin
    FCloseState := AState;
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if (AState and DFCS_PUSHED > 0) and (AState and DFCS_HOT = 0) then
        Button := tttCloseNormal
      else
      if AState and DFCS_PUSHED > 0 then
        Button := tttClosePressed
      else
      if AState and DFCS_HOT > 0 then
        Button := tttCloseHot
      else
        Button := tttCloseNormal;

      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(Canvas.Handle, Details, FCloseBtnRect);
    end
    else
    {$ENDIF JVCLThemesEnabled}
      DrawFrameControl(Canvas.Handle, FCloseBtnRect, DFC_CAPTION, DFCS_TRANSPARENT or
        DFCS_CAPTIONCLOSE or FCloseState);
  end;
end;

procedure TJvBalloonWindowEx.Init(AData: Pointer);
begin
  Canvas.Font := Screen.HintFont;
  Color := Application.HintColor;

  with PHintData(AData)^ do
  begin
    FImageIndex := RImageIndex;
    FIconKind := RIconKind;
    FHeader := RHeader;

    FShowHeader := FHeader > '';
    FShowIcon := (FIconKind <> ikNone) and
      ((FIconKind <> ikCustom) or (FImageIndex <> -1));
    FShowCloseBtn := RShowCloseBtn;

    FAnimationTime := RAnimationTime;
    FAnimationStyle := RAnimationStyle;

    FSwitchHeight := RSwitchHeight;
    FIsAnchored := Assigned(RAnchorWindow);
  end;

  FImageSize := TGlobalCtrl.Instance.HintImageSize(FIconKind, FImageIndex);
  FCurrentPosition := FCtrl.DefaultBalloonPosition;
end;

procedure TJvBalloonWindowEx.InternalActivateHint(var Rect: TRect;
  const AHint: string);
const
  {TJvAnimationStyle = (atNone, atSlide, atRoll, atRollHorNeg, atRollHorPos, atRollVerNeg,
    atRollVerPos, atSlideHorNeg, atSlideHorPos, atSlideVerNeg, atSlideVerPos, atCenter, atBlend);}
  CAnimationStyle: array [TJvAnimationStyle] of Integer = (0, AW_SLIDE, 0, AW_HOR_NEGATIVE,
    AW_HOR_POSITIVE, AW_VER_NEGATIVE, AW_VER_POSITIVE, AW_HOR_NEGATIVE or AW_SLIDE,
    AW_HOR_POSITIVE or AW_SLIDE, AW_VER_NEGATIVE or AW_SLIDE, AW_VER_POSITIVE or AW_SLIDE,
    AW_CENTER, AW_BLEND);
var
  AutoValue: Integer;
begin
  CheckPosition(Rect);

  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    Hide;
    if ParentWindow = 0 then
      ShowWindow(Handle, SW_HIDE);
  end;

  { This will prevent focusing/unfocusing of the application button on the
    taskbar when clicking on the balloon window }
  if FIsAnchored then
    ParentWindow := Application.Handle
  else
    ParentWindow := 0;

  UpdateBoundsRect(Rect);
  UpdateRegion;
  UpdateBoundsRect(Rect);

  if Rect.Top + Height > Screen.DesktopHeight then
    Rect.Top := Screen.DesktopHeight - Height;
  if Rect.Left + Width > Screen.DesktopWidth then
    Rect.Left := Screen.DesktopWidth - Width;
  if Rect.Left < Screen.DesktopLeft then
    Rect.Left := Screen.DesktopLeft;
  if Rect.Bottom < Screen.DesktopTop then
    Rect.Bottom := Screen.DesktopTop;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
    SWP_NOACTIVATE);
  if (FAnimationStyle <> atNone) and IsWinXP_UP and Assigned(AnimateWindowProc) then
  begin
    if FAnimationStyle in [atSlide, atRoll] then
      case FCurrentPosition of
        bpLeftDown, bpRightDown:
          AutoValue := AW_VER_POSITIVE;
      else {bpLeftUp, bpRightUp:}
        AutoValue := AW_VER_NEGATIVE;
      end
    else
      AutoValue := 0;
    { This function will fail on systems other than Windows XP,
      because of use of the window region: }
    AnimateWindowProc(Handle, FAnimationTime, CAnimationStyle[FAnimationStyle] or AutoValue);
  end;

  { Ensure property Visible is set to True: }
  Show;
  { If ParentWindow = 0, calling Show won't trigger the CM_SHOWINGCHANGED message
    thus ShowWindow/SetWindowPos isn't called. We do it ourselfs: }
  if ParentWindow = 0 then
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
  //Invalidate;
end;

procedure TJvBalloonWindowEx.MoveWindow(NewPos: TPoint);
begin
  BoundsRect := Rect(NewPos.X, NewPos.Y, NewPos.X + Width, NewPos.Y + Height);
end;

procedure TJvBalloonWindowEx.Paint;
var
  HintRect: TRect;
  HeaderRect: TRect;
begin
  HintRect := ClientRect;

  if FShowIcon then
    TGlobalCtrl.Instance.DrawHintImage(Canvas, 12, FDeltaY + 7, FIconKind, FImageIndex, Color);

  FCloseState := 0;
  if FShowCloseBtn then
  begin
    FCloseBtnRect := Rect(HintRect.Right - 22, FDeltaY + 5, HintRect.Right - 6, FDeltaY + 21);
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      Dec(FCloseBtnRect.Left);
      Dec(FCloseBtnRect.Top);
    end;
    {$ENDIF}
    ChangeCloseState(DFCS_FLAT);
  end;

  if FMsg > '' then
  begin
    Inc(HintRect.Left, 12);
    Inc(HintRect.Top, FDeltaY + FMessageTop);
    Canvas.Font.Color := Screen.HintFont.Color;
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    DrawText(Canvas.Handle, PChar(FMsg), -1, HintRect, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;

  if FShowHeader then
  begin
    HeaderRect := ClientRect;
    Inc(HeaderRect.Left, 12);
    if FShowIcon then
      Inc(HeaderRect.Left, FImageSize.cx + 8);
    Inc(HeaderRect.Top, FDeltaY + 8);
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    DrawText(Canvas.Handle, PChar(FHeader), -1, HeaderRect, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;
end;

procedure TJvBalloonWindowEx.WMActivateApp(var Msg: TWMActivateApp);
begin
  inherited;
  if FIsAnchored and (Msg.Active = False) then
    FCtrl.CancelHint;
end;

procedure TJvBalloonWindowEx.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  inherited;
  if FShowCloseBtn then
  begin
    if PtInRect(FCloseBtnRect, SmallPointToPoint(Msg.Pos)) then
    begin
      SetCapture(Handle);
      ChangeCloseState(FCloseState or DFCS_PUSHED);
    end;
  end;
end;

procedure TJvBalloonWindowEx.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  inherited;
  if FShowCloseBtn then
  begin
    if FCloseState and DFCS_PUSHED > 0 then
    begin
      ReleaseCapture;
      ChangeCloseState(FCloseState and not DFCS_PUSHED);
      if PtInRect(FCloseBtnRect, SmallPointToPoint(Msg.Pos)) then
        FCtrl.CancelHint;
    end;
  end;
end;

procedure TJvBalloonWindowEx.WMMouseMove(var Msg: TWMMouseMove);
var
  State: Cardinal;
begin
  inherited;
  if FShowCloseBtn then
  begin
    State := DFCS_FLAT;

    if PtInRect(FCloseBtnRect, SmallPointToPoint(Msg.Pos)) then
    begin
      { Note: DFCS_HOT is not supported in windows 95 systems }
      State := State or DFCS_HOT;
      if FCloseState and DFCS_PUSHED > 0 then
        State := State or DFCS_PUSHED;
    end;
    ChangeCloseState(State);
  end;
end;

procedure TJvBalloonWindowEx.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if FShowCloseBtn then
    Msg.Result := HTCLIENT
  else
    inherited;
end;

procedure InitD5Controls;
var
  UserHandle: HMODULE;
begin
  UserHandle := GetModuleHandle('USER32');
  if UserHandle <> 0 then
    @AnimateWindowProc := GetProcAddress(UserHandle, 'AnimateWindow');
end;

initialization
  {$IFNDEF COMPILER6_UP}
  InitD5Controls;
  {$ENDIF}
finalization
  FreeAndNil(GGlobalCtrl);
end.

