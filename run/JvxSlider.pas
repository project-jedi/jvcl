{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxSlider.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvxSlider;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Forms, Controls, ExtCtrls, Graphics, Messages, Menus,
  JvComponent, JvExControls;

type
  TNumThumbStates = 1..2;
  TSliderOrientation = (soHorizontal, soVertical);
  TSliderOption = (soShowFocus, soShowPoints, soSmooth,
    soRulerOpaque, soThumbOpaque);
  TSliderOptions = set of TSliderOption;
  TSliderImage = (siHThumb, siHRuler, siVThumb, siVRuler);
  TSliderImages = set of TSliderImage;
  TSliderImageArray = array [TSliderImage] of TBitmap;
  TJumpMode = (jmNone, jmHome, jmEnd, jmNext, jmPrior);

  TJvCustomSlider = class(TJvCustomControl)
  private
    FUserImages: TSliderImages;
    FImages: TSliderImageArray;
    FEdgeSize: Integer;
    FRuler: TBitmap;
    FPaintBuffered: Boolean;
    FRulerOrg: TPoint;
    FThumbRect: TRect;
    FThumbDown: Boolean;
    FNumThumbStates: TNumThumbStates;
    FPointsRect: TRect;
    FOrientation: TSliderOrientation;
    FOptions: TSliderOptions;
    FBevelStyle: TPanelBevel;
    FBevelWidth: Integer;
    FMinValue: Longint;
    FMaxValue: Longint;
    FIncrement: Longint;
    FValue: Longint;
    FHit: Integer;
    FFocused: Boolean;
    FSliding: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FMousePos: TPoint;
    FStartJump: TJumpMode;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnDrawPoints: TNotifyEvent;
    function GetImage(Index: Integer): TBitmap;
    procedure SetImage(Index: Integer; Value: TBitmap);
    procedure SliderImageChanged(Sender: TObject);
    procedure SetEdgeSize(Value: Integer);
    function GetNumThumbStates: TNumThumbStates;
    procedure SetNumThumbStates(Value: TNumThumbStates);
    procedure SetBevelStyle(Value: TPanelBevel);
    procedure SetOrientation(Value: TSliderOrientation);
    procedure SetOptions(Value: TSliderOptions);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    procedure SetIncrement(Value: Longint);
    procedure SetReadOnly(Value: Boolean);
    function GetThumbOffset: Integer;
    procedure SetThumbOffset(Value: Integer);
    procedure SetValue(Value: Longint);
    procedure ThumbJump(Jump: TJumpMode);
    function GetThumbPosition(var Offset: Integer): TPoint;
    function JumpTo(X, Y: Integer): TJumpMode;
    procedure InvalidateThumb;
    procedure StopTracking;
    procedure TimerTrack;
    function StoreImage(Index: Integer): Boolean;
    procedure CreateElements;
    procedure BuildRuler(R: TRect);
    procedure AdjustElements;
    procedure ReadUserImages(Stream: TStream);
    procedure WriteUserImages(Stream: TStream);
    procedure InternalDrawPoints(ACanvas: TCanvas; PointsStep, PointsHeight,
      ExtremePointsHeight: Longint);
    procedure DrawThumb(Canvas: TCanvas; Origin: TPoint; Highlight: Boolean);
    function GetValueByOffset(Offset: Integer): Longint;
    function GetOffsetByValue(Value: Longint): Integer;
    function GetRulerLength: Integer;
    {$IFDEF VCL}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    { TODO -cVisuaCLX : implement message handler substitutions }
    {$ENDIF VisualCLX}
  protected
    procedure DoBoundsChanged; override;
    procedure DoFocusChanged(Control: TWinControl); override;
    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure EnabledChanged; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    function CanModify: Boolean; virtual;
    function GetSliderRect: TRect; virtual;
    function GetSliderValue: Longint; virtual;
    procedure Change; dynamic;
    procedure Changed; dynamic;
    procedure Sized; virtual;
    procedure RangeChanged; virtual;
    procedure SetRange(Min, Max: Longint);
    procedure ThumbMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure ThumbMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ThumbMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    property ThumbOffset: Integer read GetThumbOffset write SetThumbOffset;
    property SliderRect: TRect read GetSliderRect;
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle
      default bvNone;
    property ImageHThumb: TBitmap index Ord(siHThumb) read GetImage
      write SetImage stored StoreImage;
    property ImageHRuler: TBitmap index Ord(siHRuler) read GetImage
      write SetImage stored StoreImage;
    property ImageVThumb: TBitmap index Ord(siVThumb) read GetImage
      write SetImage stored StoreImage;
    property ImageVRuler: TBitmap index Ord(siVRuler) read GetImage
      write SetImage stored StoreImage;
    property NumThumbStates: TNumThumbStates read GetNumThumbStates
      write SetNumThumbStates default 2;
    property Orientation: TSliderOrientation read FOrientation
      write SetOrientation default soHorizontal;
    property EdgeSize: Integer read FEdgeSize write SetEdgeSize default 2;
    property Options: TSliderOptions read FOptions write SetOptions
      default [soShowFocus, soShowPoints, soSmooth];
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnDrawPoints: TNotifyEvent read FOnDrawPoints write FOnDrawPoints;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawPoints(PointsStep, PointsHeight,
      ExtremePointsHeight: Longint); virtual;
    property Canvas;
    property Increment: Longint read FIncrement write SetIncrement default 10;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 100;
    property Value: Longint read FValue write SetValue default 0;
  end;

  TJvxSlider = class(TJvCustomSlider)
  published
    property Align;
    property BevelStyle;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property ImageHThumb;
    property ImageHRuler;
    property ImageVThumb;
    property ImageVRuler;
    property Increment;
    property MinValue;
    property MaxValue;
    property NumThumbStates;
    property Orientation;
    { ensure Orientation is published before EdgeSize }
    property EdgeSize;
    property Options;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Value;
    property Visible;
    property Anchors;
    property Constraints;
    property DragKind;
    property OnChange;
    property OnChanged;
    property OnDrawPoints;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
  end;

  TJvSliderImages = class;

  TJvCustomTrackBar = class(TJvCustomSlider)
  private
    FImages: TJvSliderImages;
  protected
    property Images: TJvSliderImages read FImages write FImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvSliderImages = class(TPersistent)
  private
    FSlider: TJvCustomSlider;
    function GetNumThumbStates: TNumThumbStates;
    procedure SetNumThumbStates(Value: TNumThumbStates);
    function GetEdgeSize: Integer;
    procedure SetEdgeSize(Value: Integer);
    function GetImage(Index: Integer): TBitmap;
    procedure SetImage(Index: Integer; Value: TBitmap);
    function StoreImage(Index: Integer): Boolean;
  published
    property HorzThumb: TBitmap index Ord(siHThumb) read GetImage
      write SetImage stored StoreImage;
    property HorzRuler: TBitmap index Ord(siHRuler) read GetImage
      write SetImage stored StoreImage;
    property VertThumb: TBitmap index Ord(siVThumb) read GetImage
      write SetImage stored StoreImage;
    property VertRuler: TBitmap index Ord(siVRuler) read GetImage
      write SetImage stored StoreImage;
    property NumThumbStates: TNumThumbStates read GetNumThumbStates
      write SetNumThumbStates default 2;
    property EdgeSize: Integer read GetEdgeSize write SetEdgeSize default 2;
  end;

implementation

uses
  Consts,
  Math,
  JvJVCLUtils, JvJCLUtils, JvConsts, JvTypes, JvThemes;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvxSlider.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvxSlider.res}
{$ENDIF LINUX}

//=== { TJvCustomSlider } ====================================================

const
  ImagesResNames: array [TSliderImage] of PChar =
    ('JV_W95_HTB', 'JV_W95_HRL', 'JV_W95_VTB', 'JV_W95_VRL');
  Indent = 6;
  JumpInterval = 400;

constructor TJvCustomSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  ControlStyle := [csClickEvents, csCaptureMouse,
    csDoubleClicks, csOpaque];  // csAcceptsControls
  IncludeThemeStyle(Self, [csParentBackground]);
  Width := 150;
  Height := 40;
  FNumThumbStates := 2;
  FBevelWidth := 1;
  FOrientation := soHorizontal;
  FOptions := [soShowFocus, soShowPoints, soSmooth];
  FEdgeSize := 2;
  FMinValue := 0;
  FMaxValue := 100;
  FIncrement := 10;
  TabStop := True;
  CreateElements;
  ControlState := ControlState - [csCreating];
end;

destructor TJvCustomSlider.Destroy;
var
  I: TSliderImage;
begin
  FOnChange := nil;
  FOnChanged := nil;
  FOnDrawPoints := nil;
  FRuler.Free;
  for I := Low(FImages) to High(FImages) do
  begin
    FImages[I].OnChange := nil;
    FImages[I].Free;
  end;
  inherited Destroy;
end;

procedure TJvCustomSlider.Loaded;
var
  I: TSliderImage;
begin
  inherited Loaded;
  for I := Low(FImages) to High(FImages) do
    if I in FUserImages then
      SetImage(Ord(I), FImages[I]);
end;

procedure TJvCustomSlider.AlignControls(AControl: TControl; var Rect: TRect);
var
  BevelSize: Integer;
begin
  BevelSize := 0;
  if BevelStyle <> bvNone then
    Inc(BevelSize, FBevelWidth);
  InflateRect(Rect, -BevelSize, -BevelSize);
  inherited AlignControls(AControl, Rect);
end;

{$IFDEF VCL}
procedure TJvCustomSlider.WMPaint(var Msg: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if FPaintBuffered then
    inherited
  else
  begin
    Canvas.Lock;
    try
      MemDC := GetDC(0);
      MemBitmap := CreateCompatibleBitmap(MemDC, ClientWidth, ClientHeight);
      ReleaseDC(0, MemDC);
      MemDC := CreateCompatibleDC(0);
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        DC := Msg.DC;
        Perform(WM_ERASEBKGND, MemDC, MemDC);
        FPaintBuffered := True;
        Msg.DC := MemDC;
        try
          WMPaint(Msg);
        finally
          Msg.DC := DC;
          FPaintBuffered := False;
        end;
        if DC = 0 then
          DC := BeginPaint(Handle, PS);
        BitBlt(DC, 0, 0, ClientWidth, ClientHeight, MemDC, 0, 0, SRCCOPY);
        if Msg.DC = 0 then
          EndPaint(Handle, PS);
      finally
        SelectObject(MemDC, OldBitmap);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TJvCustomSlider.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if not (csDesigning in ComponentState) and PtInRect(FThumbRect,
    ScreenToClient(P)) then
  begin
    Windows.SetCursor(Screen.Cursors[crHand]);
  end
  else
    inherited;
end;

procedure TJvCustomSlider.WMTimer(var Msg: TMessage);
begin
  TimerTrack;
end;

{$ENDIF VCL}
procedure TJvCustomSlider.Paint;
var
  R: TRect;
  TopColor, BottomColor, TransColor: TColor;
  HighlightThumb: Boolean;
  P: TPoint;
  Offset: Integer;
begin
  if csPaintCopy in ControlState then
  begin
    Offset := GetOffsetByValue(GetSliderValue);
    P := GetThumbPosition(Offset);
  end
  else
    P := Point(FThumbRect.Left, FThumbRect.Top);
  R := GetClientRect;
  if BevelStyle <> bvNone then
  begin
    TopColor := clBtnHighlight;
    if BevelStyle = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if BevelStyle = bvLowered then
      BottomColor := clBtnHighlight;
    Frame3D(Canvas, R, TopColor, BottomColor, FBevelWidth);
  end;
  if (csOpaque in ControlStyle) then
    DrawThemedBackground(Self, Canvas, R, Self.Color);
  if FRuler.Width > 0 then
  begin
    if soRulerOpaque in Options then
      TransColor := clNone
    else
      TransColor := FRuler.TransparentColor;
    DrawBitmapTransparent(Canvas, FRulerOrg.X, FRulerOrg.Y, FRuler,
      TransColor);
  end;
  if (soShowFocus in Options) and FFocused and
    not (csDesigning in ComponentState) then
  begin
    R := SliderRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
  if (soShowPoints in Options) then
  begin
    if Assigned(FOnDrawPoints) then
      FOnDrawPoints(Self)
    else
      InternalDrawPoints(Canvas, Increment, 3, 5);
  end;
  if csPaintCopy in ControlState then
    HighlightThumb := not Enabled
  else
    HighlightThumb := FThumbDown or not Enabled;
  DrawThumb(Canvas, P, HighlightThumb);
end;

function TJvCustomSlider.CanModify: Boolean;
begin
  Result := True;
end;

function TJvCustomSlider.GetSliderValue: Longint;
begin
  Result := FValue;
end;

function TJvCustomSlider.GetSliderRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
  if BevelStyle <> bvNone then
    InflateRect(Result, -FBevelWidth, -FBevelWidth);
end;

procedure TJvCustomSlider.DrawThumb(Canvas: TCanvas; Origin: TPoint;
  Highlight: Boolean);
var
  R: TRect;
  Image: TBitmap;
  TransColor: TColor;
begin
  if Orientation = soHorizontal then
    Image := ImageHThumb
  else
    Image := ImageVThumb;
  R := Rect(0, 0, Image.Width, Image.Height);
  if NumThumbStates = 2 then
  begin
    if Highlight then
      R.Left := (R.Right - R.Left) div 2
    else
      R.Right := (R.Right - R.Left) div 2;
  end;
  if soThumbOpaque in Options then
    TransColor := clNone
  else
    TransColor := Image.TransparentColor;
  DrawBitmapRectTransparent(Canvas, Origin.X, Origin.Y, R, Image, TransColor);
end;

procedure TJvCustomSlider.InternalDrawPoints(ACanvas: TCanvas; PointsStep,
  PointsHeight, ExtremePointsHeight: Longint);
const
  MinInterval = 3;
var
  RulerLength: Integer;
  Interval, Scale, PointsCnt, I, Val: Longint;
  X, H, X1, X2, Y1, Y2: Integer;
  Range: Double;
begin
  RulerLength := GetRulerLength;
  ACanvas.Pen.Color := clWindowText;
  Scale := 0;
  Range := MaxValue - MinValue;
  repeat
    Inc(Scale);
    PointsCnt := Round(Range / (Scale * PointsStep)) + 1;
    if PointsCnt > 1 then
      Interval := RulerLength div (PointsCnt - 1)
    else
      Interval := RulerLength;
  until (Interval >= MinInterval + 1) or (Interval >= RulerLength);
  Val := MinValue;
  for I := 1 to PointsCnt do
  begin
    H := PointsHeight;
    if I = PointsCnt then
      Val := MaxValue;
    if (Val = MaxValue) or (Val = MinValue) then
      H := ExtremePointsHeight;
    X := GetOffsetByValue(Val);
    if Orientation = soHorizontal then
    begin
      X1 := X + (FImages[siHThumb].Width div NumThumbStates) div 2;
      Y1 := FPointsRect.Top;
      X2 := X1;
      Y2 := Y1 + H;
    end
    else
    begin
      X1 := FPointsRect.Left;
      Y1 := X + FImages[siVThumb].Height div 2;
      X2 := X1 + H;
      Y2 := Y1;
    end;
    with ACanvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
    Inc(Val, Scale * PointsStep);
  end;
end;

procedure TJvCustomSlider.DefaultDrawPoints(PointsStep, PointsHeight,
  ExtremePointsHeight: Longint);
begin
  InternalDrawPoints(Canvas, PointsStep, PointsHeight, ExtremePointsHeight);
end;

procedure TJvCustomSlider.CreateElements;
var
  I: TSliderImage;
begin
  FRuler := TBitmap.Create;
  for I := Low(FImages) to High(FImages) do
    SetImage(Ord(I), nil);
  AdjustElements;
end;

procedure TJvCustomSlider.BuildRuler(R: TRect);
var
  DstR, BmpR: TRect;
  I, L, B, N, C, Offs, Len, RulerWidth: Integer;
  TmpBmp: TBitmap;
  Index: TSliderImage;
begin
  TmpBmp := TBitmap.Create;
  try
    if Orientation = soHorizontal then
      Index := siHRuler
    else
      Index := siVRuler;
    if Orientation = soHorizontal then
    begin
      L := R.Right - R.Left - 2 * Indent;
      if L < 0 then
        L := 0;
      TmpBmp.Width := L;
      TmpBmp.Height := FImages[Index].Height;
      L := TmpBmp.Width - 2 * FEdgeSize;
      B := FImages[Index].Width - 2 * FEdgeSize;
      RulerWidth := FImages[Index].Width;
    end
    else
    begin
      TmpBmp.Width := FImages[Index].Width;
      TmpBmp.Height := R.Bottom - R.Top - 2 * Indent;
      L := TmpBmp.Height - 2 * FEdgeSize;
      B := FImages[Index].Height - 2 * FEdgeSize;
      RulerWidth := FImages[Index].Height;
    end;
    N := (L div B) + 1;
    C := L mod B;
    for I := 0 to N - 1 do
    begin
      if I = 0 then
      begin
        Offs := 0;
        Len := RulerWidth - FEdgeSize;
      end
      else
      begin
        Offs := FEdgeSize + I * B;
        if I = N - 1 then
          Len := C + FEdgeSize
        else
          Len := B;
      end;
      if Orientation = soHorizontal then
        DstR := Rect(Offs, 0, Offs + Len, TmpBmp.Height)
      else
        DstR := Rect(0, Offs, TmpBmp.Width, Offs + Len);
      if I = 0 then
        Offs := 0
      else
      if I = N - 1 then
        Offs := FEdgeSize + B - C
      else
        Offs := FEdgeSize;
      if Orientation = soHorizontal then
        BmpR := Rect(Offs, 0, Offs + DstR.Right - DstR.Left, TmpBmp.Height)
      else
        BmpR := Rect(0, Offs, TmpBmp.Width, Offs + DstR.Bottom - DstR.Top);
      TmpBmp.Canvas.CopyRect(DstR, FImages[Index].Canvas, BmpR);
    end;
    FRuler.Assign(TmpBmp);
  finally
    TmpBmp.Free;
  end;
end;

procedure TJvCustomSlider.AdjustElements;
var
  SaveValue: Longint;
  R: TRect;
begin
  SaveValue := Value;
  R := SliderRect;
  BuildRuler(R);
  if Orientation = soHorizontal then
  begin
    if FImages[siHThumb].Height > FRuler.Height then
    begin
      FThumbRect := Bounds(R.Left + Indent, R.Top + Indent,
        FImages[siHThumb].Width div NumThumbStates, FImages[siHThumb].Height);
      FRulerOrg := Point(R.Left + Indent, R.Top + Indent +
        (FImages[siHThumb].Height - FRuler.Height) div 2);
      FPointsRect := Rect(FRulerOrg.X, R.Top + Indent +
        FImages[siHThumb].Height + 1,
        FRulerOrg.X + FRuler.Width, R.Bottom - R.Top - 1);
    end
    else
    begin
      FThumbRect := Bounds(R.Left + Indent, R.Top + Indent +
        (FRuler.Height - FImages[siHThumb].Height) div 2,
        FImages[siHThumb].Width div NumThumbStates, FImages[siHThumb].Height);
      FRulerOrg := Point(R.Left + Indent, R.Top + Indent);
      FPointsRect := Rect(FRulerOrg.X, R.Top + Indent + FRuler.Height + 1,
        FRulerOrg.X + FRuler.Width, R.Bottom - R.Top - 1);
    end;
  end
  else
  begin { soVertical }
    if FImages[siVThumb].Width div NumThumbStates > FRuler.Width then
    begin
      FThumbRect := Bounds(R.Left + Indent, R.Top + Indent,
        FImages[siVThumb].Width div NumThumbStates, FImages[siVThumb].Height);
      FRulerOrg := Point(R.Left + Indent + (FImages[siVThumb].Width div NumThumbStates -
        FRuler.Width) div 2, R.Top + Indent);
      FPointsRect := Rect(R.Left + Indent + FImages[siVThumb].Width div NumThumbStates + 1,
        FRulerOrg.Y, R.Right - R.Left - 1, FRulerOrg.Y + FRuler.Height);
    end
    else
    begin
      FThumbRect := Bounds(R.Left + Indent + (FRuler.Width -
        FImages[siVThumb].Width div NumThumbStates) div 2, R.Top + Indent,
        FImages[siVThumb].Width div NumThumbStates, FImages[siVThumb].Height);
      FRulerOrg := Point(R.Left + Indent, R.Top + Indent);
      FPointsRect := Rect(R.Left + Indent + FRuler.Width + 1, FRulerOrg.Y,
        R.Right - R.Left - 1, FRulerOrg.Y + FRuler.Height);
    end;
  end;
  Value := SaveValue;
  Invalidate;
end;

procedure TJvCustomSlider.Sized;
begin
  AdjustElements;
end;

procedure TJvCustomSlider.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomSlider.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvCustomSlider.RangeChanged;
begin
end;

procedure TJvCustomSlider.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := FUserImages <> TJvCustomSlider(Filer.Ancestor).FUserImages
    else
      Result := FUserImages <> [];
  end;

begin
  if Filer is TReader then
    inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('UserImages', ReadUserImages, WriteUserImages,DoWrite);
end;

procedure TJvCustomSlider.ReadUserImages(Stream: TStream);
begin
  Stream.ReadBuffer(FUserImages, SizeOf(FUserImages));
end;

procedure TJvCustomSlider.WriteUserImages(Stream: TStream);
begin
  Stream.WriteBuffer(FUserImages, SizeOf(FUserImages));
end;

function TJvCustomSlider.StoreImage(Index: Integer): Boolean;
begin
  Result := TSliderImage(Index) in FUserImages;
end;

function TJvCustomSlider.GetImage(Index: Integer): TBitmap;
begin
  Result := FImages[TSliderImage(Index)];
end;

procedure TJvCustomSlider.SliderImageChanged(Sender: TObject);
begin
  if not (csCreating in ControlState) then
    Sized;
end;

procedure TJvCustomSlider.SetImage(Index: Integer; Value: TBitmap);
var
  Idx: TSliderImage;
begin
  Idx := TSliderImage(Index);
  if FImages[Idx] = nil then
  begin
    FImages[Idx] := TBitmap.Create;
    FImages[Idx].OnChange := SliderImageChanged;
  end;
  if Value = nil then
  begin
    FImages[Idx].Handle := LoadBitmap(HInstance, ImagesResNames[Idx]);
    Exclude(FUserImages, Idx);
    if not (csReading in ComponentState) then
    begin
      if Idx in [siHThumb, siVThumb] then
        Exclude(FOptions, soThumbOpaque)
      else
        Exclude(FOptions, soRulerOpaque);
      Invalidate;
    end;
  end
  else
  begin
    FImages[Idx].Assign(Value);
    Include(FUserImages, Idx);
  end;
end;

procedure TJvCustomSlider.SetEdgeSize(Value: Integer);
var
  MaxSize: Integer;
begin
  if Orientation = soHorizontal then
    MaxSize := FImages[siHRuler].Width
  else
    MaxSize := FImages[siVRuler].Height;
  if Value * 2 < MaxSize then
    if Value <> FEdgeSize then
    begin
      FEdgeSize := Value;
      Sized;
    end;
end;

function TJvCustomSlider.GetNumThumbStates: TNumThumbStates;
begin
  Result := FNumThumbStates;
end;

procedure TJvCustomSlider.SetNumThumbStates(Value: TNumThumbStates);
begin
  if FNumThumbStates <> Value then
  begin
    FNumThumbStates := Value;
    AdjustElements;
  end;
end;

procedure TJvCustomSlider.SetBevelStyle(Value: TPanelBevel);
begin
  if Value <> FBevelStyle then
  begin
    FBevelStyle := Value;
    Sized;
    Update;
  end;
end;

procedure TJvCustomSlider.SetOrientation(Value: TSliderOrientation);
begin
  if Orientation <> Value then
  begin
    FOrientation := Value;
    Sized;
    if ComponentState * [csLoading, csUpdating] = [] then
      SetBounds(Left, Top, Height, Width);
  end;
end;

procedure TJvCustomSlider.SetOptions(Value: TSliderOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSlider.SetRange(Min, Max: Longint);
begin
  if (Min < Max) or (csReading in ComponentState) then
  begin
    FMinValue := Min;
    FMaxValue := Max;
    if not (csReading in ComponentState) then
      if Min + Increment > Max then
        FIncrement := Max - Min;
    if (soShowPoints in Options) then
      Invalidate;
    Self.Value := FValue;
    RangeChanged;
  end;
end;

procedure TJvCustomSlider.SetMinValue(Value: Longint);
begin
  if FMinValue <> Value then
    SetRange(Value, MaxValue);
end;

procedure TJvCustomSlider.SetMaxValue(Value: Longint);
begin
  if FMaxValue <> Value then
    SetRange(MinValue, Value);
end;

procedure TJvCustomSlider.SetIncrement(Value: Longint);
begin
  if not (csReading in ComponentState) and ((Value > MaxValue - MinValue) or
    (Value < 1)) then
    raise EJVCLException.CreateResFmt(@SOutOfRange, [1, MaxValue - MinValue]);
  if (Value > 0) and (FIncrement <> Value) then
  begin
    FIncrement := Value;
    Self.Value := FValue;
    Invalidate;
  end;
end;

function TJvCustomSlider.GetValueByOffset(Offset: Integer): Longint;
var
  Range: Double;
  R: TRect;
begin
  R := SliderRect;
  if Orientation = soVertical then
    Offset := ClientHeight - Offset - FImages[siVThumb].Height;
  Range := MaxValue - MinValue;
  Result := Round((Offset - R.Left - Indent) * Range / GetRulerLength);
  if not (soSmooth in Options) then
    Result := Round(Result / Increment) * Increment;
  Result := Min(MinValue + Max(Result, 0), MaxValue);
end;

function TJvCustomSlider.GetOffsetByValue(Value: Longint): Integer;
var
  Range: Double;
  R: TRect;
  MinIndent: Integer;
begin
  R := SliderRect;
  Range := MaxValue - MinValue;
  if Orientation = soHorizontal then
    MinIndent := R.Left + Indent
  else
    MinIndent := R.Top + Indent;
  Result := Round((Value - MinValue) / Range * GetRulerLength) + MinIndent;
  if Orientation = soVertical then
    Result := R.Top + R.Bottom - Result - FImages[siVThumb].Height;
  Result := Max(Result, MinIndent);
end;

function TJvCustomSlider.GetThumbPosition(var Offset: Integer): TPoint;
var
  R: TRect;
  MinIndent: Integer;
begin
  R := SliderRect;
  if Orientation = soHorizontal then
    MinIndent := R.Left + Indent
  else
    MinIndent := R.Top + Indent;
  Offset := Min(GetOffsetByValue(GetValueByOffset(Min(Max(Offset, MinIndent),
    MinIndent + GetRulerLength))), MinIndent + GetRulerLength);
  if Orientation = soHorizontal then
  begin
    Result.X := Offset;
    Result.Y := FThumbRect.Top;
  end
  else
  begin
    Result.Y := Offset;
    Result.X := FThumbRect.Left;
  end;
end;

function TJvCustomSlider.GetThumbOffset: Integer;
begin
  if Orientation = soHorizontal then
    Result := FThumbRect.Left
  else
    Result := FThumbRect.Top;
end;

procedure TJvCustomSlider.InvalidateThumb;
begin
  if HandleAllocated then
    InvalidateRect(Handle, @FThumbRect, not (csOpaque in ControlStyle));
end;

procedure TJvCustomSlider.SetThumbOffset(Value: Integer);
var
  ValueBefore: Longint;
  P: TPoint;
begin
  ValueBefore := FValue;
  P := GetThumbPosition(Value);
  InvalidateThumb;
  FThumbRect := Bounds(P.X, P.Y, RectWidth(FThumbRect), RectHeight(FThumbRect));
  InvalidateThumb;
  if FSliding then
  begin
    FValue := GetValueByOffset(Value);
    if ValueBefore <> FValue then
      Change;
  end;
end;

function TJvCustomSlider.GetRulerLength: Integer;
begin
  if Orientation = soHorizontal then
  begin
    Result := FRuler.Width;
    Dec(Result, FImages[siHThumb].Width div NumThumbStates);
  end
  else
  begin
    Result := FRuler.Height;
    Dec(Result, FImages[siVThumb].Height);
  end;
end;

procedure TJvCustomSlider.SetValue(Value: Longint);
var
  ValueChanged: Boolean;
begin
  if Value > MaxValue then
    Value := MaxValue;
  if Value < MinValue then
    Value := MinValue;
  ValueChanged := FValue <> Value;
  FValue := Value;
  ThumbOffset := GetOffsetByValue(Value);
  if ValueChanged then
    Change;
end;

procedure TJvCustomSlider.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    if Value then
    begin
      StopTracking;
      if FSliding then
        ThumbMouseUp(mbLeft, [], 0, 0);
    end;
    FReadOnly := Value;
  end;
end;

procedure TJvCustomSlider.ThumbJump(Jump: TJumpMode);
var
  NewValue: Longint;
begin
  if Jump <> jmNone then
  begin
    case Jump of
      jmHome:
        NewValue := MinValue;
      jmPrior:
        NewValue := (Round(Value / Increment) * Increment) - Increment;
      jmNext:
        NewValue := (Round(Value / Increment) * Increment) + Increment;
      jmEnd:
        NewValue := MaxValue;
    else
      Exit;
    end;
    if NewValue >= MaxValue then
      NewValue := MaxValue
    else
    if NewValue <= MinValue then
      NewValue := MinValue;
    if (NewValue <> Value) then
      Value := NewValue;
  end;
end;

function TJvCustomSlider.JumpTo(X, Y: Integer): TJumpMode;
begin
  Result := jmNone;
  if Orientation = soHorizontal then
  begin
    if FThumbRect.Left > X then
      Result := jmPrior
    else
    if FThumbRect.Right < X then
      Result := jmNext;
  end
  else
  if Orientation = soVertical then
  begin
    if FThumbRect.Top > Y then
      Result := jmNext
    else
    if FThumbRect.Bottom < Y then
      Result := jmPrior;
  end;
end;


procedure TJvCustomSlider.EnabledChanged;
begin
  inherited EnabledChanged;
  InvalidateThumb;
end;

procedure TJvCustomSlider.DoFocusChanged(Control: TWinControl);
var
  Active: Boolean;
begin
  Active := (Control = Self);
  if Active <> FFocused then
  begin
    FFocused := Active;
    if (soShowFocus in Options) then
      Invalidate;
  end;
  inherited DoFocusChanged(Control);
end;

procedure TJvCustomSlider.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows];
end;

procedure TJvCustomSlider.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  if not (csReading in ComponentState) then
    Sized;
end;


procedure TJvCustomSlider.StopTracking;
begin
  if FTracking then
  begin
    if FTimerActive then
    begin
      KillTimer(Handle, 1);
      FTimerActive := False;
    end;
    FTracking := False;
    MouseCapture := False;
    Changed;
  end;
end;

procedure TJvCustomSlider.TimerTrack;
var
  Jump: TJumpMode;
begin
  Jump := JumpTo(FMousePos.X, FMousePos.Y);
  if Jump = FStartJump then
  begin
    ThumbJump(Jump);
    if not FTimerActive then
    begin
      SetTimer(Handle, 1, JumpInterval, nil);
      FTimerActive := True;
    end;
  end;
end;

procedure TJvCustomSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
  P: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    if CanFocus then
      SetFocus;
    P := Point(X, Y);
    if PtInRectInclusive(FThumbRect,P) then
      ThumbMouseDown(Button, Shift, X, Y)
    else
    begin
      with FRulerOrg, FRuler do
        Rect := Bounds(X, Y, Width, Height);
      InflateRect(Rect, Ord(Orientation = soVertical) * 3,
        Ord(Orientation = soHorizontal) * 3);
      if PtInRectInclusive(Rect, P) and CanModify and not ReadOnly then
      begin
        MouseCapture := True;
        FTracking := True;
        FMousePos := P;
        FStartJump := JumpTo(X, Y);
        TimerTrack;
      end;
    end;
  end;
end;

procedure TJvCustomSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (csLButtonDown in ControlState) and FSliding then
    ThumbMouseMove(Shift, X, Y)
  else
  if FTracking then
    FMousePos := Point(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomSlider.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  if FSliding then
    ThumbMouseUp(Button, Shift, X, Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  Jump: TJumpMode;
begin
  Jump := jmNone;
  if Shift = [] then
  begin
    if Key = VK_HOME then
      Jump := jmHome
    else
    if Key = VK_END then
      Jump := jmEnd;
    if Orientation = soHorizontal then
    begin
      if Key = VK_LEFT then
        Jump := jmPrior
      else
      if Key = VK_RIGHT then
        Jump := jmNext;
    end
    else
    begin
      if Key = VK_UP then
        Jump := jmNext
      else
      if Key = VK_DOWN then
        Jump := jmPrior;
    end;
  end;
  if (Jump <> jmNone) and CanModify and not ReadOnly then
  begin
    Key := 0;
    ThumbJump(Jump);
    Changed;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomSlider.ThumbMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if CanFocus then
    SetFocus;
  if (Button = mbLeft) and CanModify and not ReadOnly then
  begin
    FSliding := True;
    FThumbDown := True;
    if Orientation = soHorizontal then
      FHit := X - FThumbRect.Left
    else
      FHit := Y - FThumbRect.Top;
    InvalidateThumb;
    Update;
  end;
end;

procedure TJvCustomSlider.ThumbMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (csLButtonDown in ControlState) and CanModify and not ReadOnly then
  begin
    if Orientation = soHorizontal then
      ThumbOffset := X - FHit
    else
      ThumbOffset := Y - FHit;
  end;
end;

procedure TJvCustomSlider.ThumbMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FSliding := False;
    FThumbDown := False;
    InvalidateThumb;
    Update;
    if CanModify and not ReadOnly then
      Changed;
  end;
end;

//=== { TJvCustomTrackBar } ==================================================

constructor TJvCustomTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := TJvSliderImages.Create;
  FImages.FSlider := Self;
end;

destructor TJvCustomTrackBar.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

//=== { TJvSliderImages } ====================================================

function TJvSliderImages.GetImage(Index: Integer): TBitmap;
begin
  Result := FSlider.GetImage(Index);
end;

procedure TJvSliderImages.SetImage(Index: Integer; Value: TBitmap);
begin
  FSlider.SetImage(Index, Value);
end;

function TJvSliderImages.StoreImage(Index: Integer): Boolean;
begin
  Result := FSlider.StoreImage(Index);
end;

function TJvSliderImages.GetNumThumbStates: TNumThumbStates;
begin
  Result := FSlider.NumThumbStates;
end;

procedure TJvSliderImages.SetNumThumbStates(Value: TNumThumbStates);
begin
  FSlider.NumThumbStates := Value;
end;

function TJvSliderImages.GetEdgeSize: Integer;
begin
  Result := FSlider.EdgeSize;
end;

procedure TJvSliderImages.SetEdgeSize(Value: Integer);
begin
  FSlider.EdgeSize := Value;
end;

end.

