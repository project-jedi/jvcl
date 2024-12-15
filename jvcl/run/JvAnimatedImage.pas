{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxAnimate.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAnimatedImage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Types,
  Graphics, Controls,
  Classes,
  JvTimer, JvComponent;

type
  TJvImageControl = class(TJvGraphicControl)
  private
    FDrawing: Boolean;
    FPaintBuffered: Boolean;
    FLock: TRTLCriticalSection;
  protected
    FGraphic: TGraphic;
    function DoPaletteChange: Boolean;
    procedure Paint; override;
    procedure BufferedPaint; virtual;
    procedure DoPaintImage; virtual; abstract;
    procedure DoPaintControl;
    procedure PaintDesignRect;
    procedure PaintImage;
    procedure PictureChanged;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Height default 105;
    property Width default 105;
  end;

  TGlyphOrientation = (goHorizontal, goVertical);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAnimatedImage = class(TJvImageControl)
  private
    FActive: Boolean;
    FGlyph: TBitmap;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FInactiveGlyph: Integer;
    FOrientation: TGlyphOrientation;
    FTimer: TJvTimer;
    FNumGlyphs: Integer;
    FGlyphNum: Integer;
    FCenter: Boolean;
    FStretch: Boolean;
    FTransparentColor: TColor;
    FTimerRepaint: Boolean;
    FOnFrameChanged: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FAsyncDrawing: Boolean;
    FTransparent: Boolean;
    procedure DefineBitmapSize;
    procedure ResetImageBounds;
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetActive(Value: Boolean);
    procedure SetAsyncDrawing(Value: Boolean);
    procedure SetCenter(Value: Boolean);
    procedure SetOrientation(Value: TGlyphOrientation);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphNum(Value: Integer);
    procedure SetInactiveGlyph(Value: Integer);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparent(const Value:Boolean);
    procedure ReadOpaque(Reader: TReader);
    procedure ImageChanged(Sender: TObject);
    procedure UpdateInactive;
    procedure TimerExpired(Sender: TObject);
    function TransparentStored: Boolean;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function GetPalette: HPALETTE; override;
    procedure AdjustSize; override;
    procedure Loaded; override;
    procedure BufferedPaint; override;
    procedure DoPaintImage; override;
    procedure FrameChanged; dynamic;
    procedure Start; dynamic;
    procedure Stop; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property AutoSize default True;
    property AsyncDrawing: Boolean read FAsyncDrawing write SetAsyncDrawing default False;
    property Active: Boolean read FActive write SetActive default False;
    property Center: Boolean read FCenter write SetCenter default False;
    property Orientation: TGlyphOrientation read FOrientation write SetOrientation
      default goHorizontal;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphNum: Integer read FGlyphNum write SetGlyphNum default 0;
    property Interval: Cardinal read GetInterval write SetInterval default 100;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property InactiveGlyph: Integer read FInactiveGlyph write SetInactiveGlyph default -1;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor
      stored TransparentStored;
    property Color;
    property Cursor;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property DragCursor;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property ParentColor default True;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnContextPopup;
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

type
  TJvLockedBitmap = class(TBitmap)
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  end;

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
  Forms,
  //JclSysUtils,
  JvConsts, JvJVCLUtils;

//=== { TJvLockedBitmap } ====================================================

procedure TJvLockedBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if not Empty then
    Canvas.Lock;
  try
    inherited Draw(ACanvas, Rect);
  finally
    if not Empty then
      Canvas.Unlock;
  end;
end;

//=== { TJvImageControl } ====================================================

constructor TJvImageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(FLock);
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse, csOpaque,
    csReplicatable, csDoubleClicks];
  Height := 105;
  Width := 105;
  ParentColor := True;
end;

destructor TJvImageControl.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TJvImageControl.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TJvImageControl.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

procedure TJvImageControl.PaintImage;
var
  Save: Boolean;
begin
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(Bounds(0, 0, ClientWidth, ClientHeight));
  end;
  Save := FDrawing;
  FDrawing := True;
  try
    DoPaintImage;
  finally
    FDrawing := Save;
  end;
end;

procedure TJvImageControl.Paint;
var
  Bmp: TBitmap;
  DC: HDC;
begin
  Bmp := TJvLockedBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    Bmp.Canvas.Lock;
    DC := Canvas.Handle;
    try
      Canvas.Handle := Bmp.Canvas.Handle;
      FPaintBuffered := True;
      try
        BufferedPaint;
      finally
        FPaintBuffered := False;
      end;
    finally
      Canvas.Handle := DC;
      Canvas.Draw(0, 0, Bmp);
      Bmp.Canvas.Unlock;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TJvImageControl.BufferedPaint;
begin
end;

procedure TJvImageControl.PaintDesignRect;
begin
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TJvImageControl.DoPaintControl;
var
  DC: HDC;
begin
  if GetCurrentThreadID = MainThreadID then
  begin
    Repaint;
    Exit;
  end;
  DC := GetDC(Parent.Handle);
  try
    IntersectClipRect(DC, Left, Top, Left + Width, Top + Height);
    MoveWindowOrg(DC, Left, Top);
    Perform(WM_PAINT, DC, 0);
  finally
    ReleaseDC(Parent.Handle, DC);
  end;
end;

function TJvImageControl.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := FGraphic;
  if Visible and (not (csLoading in ComponentState)) and
    (Tmp <> nil) and Tmp.PaletteModified then
  begin
    if GetPalette <> 0 then
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and ParentForm.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(WM_QUERYNEWPALETTE, 0, 0)
        else
          PostMessage(ParentForm.Handle, WM_QUERYNEWPALETTE, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end
    else
      Tmp.PaletteModified := False;
  end;
end;

procedure TJvImageControl.PictureChanged;
begin
  if not (csDestroying in ComponentState) then
  begin
    AdjustSize;
    if FGraphic <> nil then
      if DoPaletteChange and FDrawing then
        Update;
    if not FDrawing then
      Invalidate;
  end;
end;

//=== { TJvAnimatedImage } ===================================================

constructor TJvAnimatedImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TJvTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  AutoSize := True;
  FTransparent := False;
  FGlyph := TJvLockedBitmap.Create;
  FGraphic := FGlyph;
  FGlyph.OnChange := ImageChanged;
  FNumGlyphs := 1;
  FInactiveGlyph := -1;
  FTransparentColor := clNone;
  FOrientation := goHorizontal;
  FStretch := True;
end;

destructor TJvAnimatedImage.Destroy;
begin
  Destroying;
  FOnFrameChanged := nil;
  FOnStart := nil;
  FOnStop := nil;
  FGlyph.OnChange := nil;
  Active := False;
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvAnimatedImage.Loaded;
begin
  inherited Loaded;
  ResetImageBounds;
  UpdateInactive;
end;

function TJvAnimatedImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if not FGlyph.Empty then
    Result := FGlyph.Palette;
end;

procedure TJvAnimatedImage.ImageChanged(Sender: TObject);
begin
  Lock;
  try
    FTransparentColor := FGlyph.TransparentColor and not PaletteMask;
  finally
    Unlock;
  end;
  DefineBitmapSize;
  PictureChanged;
end;

procedure TJvAnimatedImage.UpdateInactive;
begin
  if (not Active) and (FInactiveGlyph >= 0) and
    (FInactiveGlyph < FNumGlyphs) and (FGlyphNum <> FInactiveGlyph) then
  begin
    Lock;
    try
      FGlyphNum := FInactiveGlyph;
    finally
      Unlock;
    end;
  end;
end;

function TJvAnimatedImage.TransparentStored: Boolean;
begin
  Result := (FGlyph.Empty and (FTransparentColor <> clNone)) or
    ((FGlyph.TransparentColor and not PaletteMask) <> FTransparentColor);
end;

procedure TJvAnimatedImage.SetTransparent(const Value:Boolean);
begin
  if Value <> FTransparent then
  begin
    Lock;
    try
      FTransparent := Value;
    finally
      Unlock;
    end;
    PictureChanged;
  end;
end;

procedure TJvAnimatedImage.SetTransparentColor(Value: TColor);
begin
  if Value <> TransparentColor then
  begin
    Lock;
    try
      FTransparentColor := Value;
    finally
      Unlock;
    end;
    PictureChanged;
  end;
end;

procedure TJvAnimatedImage.SetOrientation(Value: TGlyphOrientation);
begin
  if FOrientation <> Value then
  begin
    Lock;
    try
      FOrientation := Value;
    finally
      Unlock;
    end;
    ImageChanged(FGlyph);
  end;
end;

procedure TJvAnimatedImage.SetGlyph(Value: TBitmap);
begin
  Lock;
  try
    FGlyph.Assign(Value);
  finally
    Unlock;
  end;
end;

procedure TJvAnimatedImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    Lock;
    try
      FStretch := Value;
    finally
      Unlock;
    end;
    PictureChanged;
    if Active then
      Repaint;
  end;
end;

procedure TJvAnimatedImage.SetCenter(Value: Boolean);
begin
  if Value <> FCenter then
  begin
    Lock;
    try
      FCenter := Value;
    finally
      Unlock;
    end;
    PictureChanged;
    if Active then
      Repaint;
  end;
end;

procedure TJvAnimatedImage.SetGlyphNum(Value: Integer);
begin
  if Value <> FGlyphNum then
  begin
    if (Value < FNumGlyphs) and (Value >= 0) then
    begin
      Lock;
      try
        FGlyphNum := Value;
      finally
        Unlock;
      end;
      UpdateInactive;
      FrameChanged;
      PictureChanged;
    end;
  end;
end;

procedure TJvAnimatedImage.SetInactiveGlyph(Value: Integer);
begin
  if Value < 0 then
    Value := -1;
  if Value <> FInactiveGlyph then
  begin
    if (Value < FNumGlyphs) or (csLoading in ComponentState) then
    begin
      Lock;
      try
        FInactiveGlyph := Value;
        UpdateInactive;
      finally
        Unlock;
      end;
      FrameChanged;
      PictureChanged;
    end;
  end;
end;

procedure TJvAnimatedImage.SetNumGlyphs(Value: Integer);
begin
  Lock;
  try
    FNumGlyphs := Value;
    if FInactiveGlyph >= FNumGlyphs then
    begin
      FInactiveGlyph := -1;
      FGlyphNum := 0;
    end
    else
      UpdateInactive;
    ResetImageBounds;
  finally
    Unlock;
  end;
  FrameChanged;
  PictureChanged;
end;

procedure TJvAnimatedImage.DefineBitmapSize;
begin
  Lock;
  try
    FNumGlyphs := 1;
    FGlyphNum := 0;
    FImageWidth := 0;
    FImageHeight := 0;
    if (FOrientation = goHorizontal) and (FGlyph.Height > 0) and
      (FGlyph.Width mod FGlyph.Height = 0) then
      FNumGlyphs := FGlyph.Width div FGlyph.Height
    else
    if (FOrientation = goVertical) and (FGlyph.Width > 0) and
      (FGlyph.Height mod FGlyph.Width = 0) then
      FNumGlyphs := FGlyph.Height div FGlyph.Width;
    ResetImageBounds;
  finally
    Unlock;
  end;
end;

procedure TJvAnimatedImage.ResetImageBounds;
begin
  if FNumGlyphs < 1 then
    FNumGlyphs := 1;
  if FOrientation = goHorizontal then
  begin
    FImageHeight := FGlyph.Height;
    FImageWidth := FGlyph.Width div FNumGlyphs;
  end
  else
  {if Orientation = goVertical then}
  begin
    FImageWidth := FGlyph.Width;
    FImageHeight := FGlyph.Height div FNumGlyphs;
  end;
end;

procedure TJvAnimatedImage.AdjustSize;
begin
  if not (csReading in ComponentState) then
    if AutoSize and (FImageWidth > 0) and (FImageHeight > 0) then
      SetBounds(Left, Top, FImageWidth, FImageHeight);
end;

procedure TJvAnimatedImage.DoPaintImage;
var
  BmpIndex: Integer;
  SrcRect, DstRect: TRect;
begin
  if (not Active) and (FInactiveGlyph >= 0) and
    (FInactiveGlyph < FNumGlyphs) then
    BmpIndex := FInactiveGlyph
  else
    BmpIndex := FGlyphNum;
  { copy image from parent and back-level controls }
  if Transparent then
    CopyParentImage(Self, Canvas);
  if (FImageWidth > 0) and (FImageHeight > 0) then
  begin
    if Orientation = goHorizontal then
      SrcRect := Bounds(BmpIndex * FImageWidth, 0, FImageWidth, FImageHeight)
    else
    {if Orientation = goVertical then}
      SrcRect := Bounds(0, BmpIndex * FImageHeight, FImageWidth, FImageHeight);
    if Stretch then
      DstRect := ClientRect
    else
    if Center then
      DstRect := Bounds((ClientWidth - FImageWidth) div 2,
        (ClientHeight - FImageHeight) div 2, FImageWidth, FImageHeight)
    else
      DstRect := Rect(0, 0, FImageWidth, FImageHeight);
    StretchBitmapRectTransparent(Canvas, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, SrcRect, FGlyph, FTransparentColor);
  end;
end;

procedure TJvAnimatedImage.BufferedPaint;
begin
  PaintImage;
  if Transparent or FGlyph.Empty then
    PaintDesignRect;
end;

procedure TJvAnimatedImage.TimerExpired(Sender: TObject);
begin
  if csPaintCopy in ControlState then
    Exit;
  if Visible and (FNumGlyphs > 1) and (Parent <> nil) and
    Parent.HandleAllocated then
  begin
    Lock;
    try
      if FGlyphNum < FNumGlyphs - 1 then
        Inc(FGlyphNum)
      else
        FGlyphNum := 0;
      if (FGlyphNum = FInactiveGlyph) and (FNumGlyphs > 1) then
        if FGlyphNum < FNumGlyphs - 1 then
          Inc(FGlyphNum)
        else
          FGlyphNum := 0;
      Canvas.Lock;
      try
        FTimerRepaint := True;
        if AsyncDrawing and Assigned(FOnFrameChanged) then
          FTimer.Synchronize(FrameChanged)
        else
          FrameChanged;
        DoPaintControl;
      finally
        FTimerRepaint := False;
        Canvas.Unlock;
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TJvAnimatedImage.FrameChanged;
begin
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
end;

procedure TJvAnimatedImage.Stop;
begin
  if not (csReading in ComponentState) then
    if Assigned(FOnStop) then
      FOnStop(Self);
end;

procedure TJvAnimatedImage.Start;
begin
  if not (csReading in ComponentState) then
    if Assigned(FOnStart) then
      FOnStart(Self);
end;

function TJvAnimatedImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) and
    (FImageWidth > 0) and (FImageHeight > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FImageWidth;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FImageHeight;
  end;
end;

procedure TJvAnimatedImage.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TJvAnimatedImage.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvAnimatedImage.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      FTimer.OnTimer := TimerExpired;
      FTimer.Enabled := True;
      FActive := FTimer.Enabled;
      Start;
    end
    else
    begin
      FTimer.Enabled := False;
      FTimer.OnTimer := nil;
      FActive := False;
      UpdateInactive;
      FrameChanged;
      Stop;
      PictureChanged;
    end;
  end;
end;

procedure TJvAnimatedImage.SetAsyncDrawing(Value: Boolean);
begin
  if FAsyncDrawing <> Value then
  begin
    Lock;
    try
      {if Value then
        HookBitmap;}
      if Assigned(FTimer) then
        FTimer.SyncEvent := not Value;
      FAsyncDrawing := Value;
    finally
      Unlock;
    end;
  end;
end;

procedure TJvAnimatedImage.ReadOpaque(Reader: TReader);
begin
  Transparent := not Reader.ReadBoolean;
end;

procedure TJvAnimatedImage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Opaque', ReadOpaque, nil, False);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
