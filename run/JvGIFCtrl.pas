{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGIFCtrl.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGIFCtrl;

{$I jvcl.inc}

interface

uses
  Messages, Windows, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  JvAnimatedImage, JvGIF, JvTimer;

type
  TJvGIFAnimator = class(TJvImageControl)
  private
    FAnimate: Boolean;
    FImage: TJvGIFImage;
    FTimer: TJvTimer;
    FFrameIndex: Integer;
    FStretch: Boolean;
    FLoop: Boolean;
    FCenter: Boolean;
    FTransparent: Boolean;
    FTimerRepaint: Boolean;
    FCache: TBitmap;
    FCacheIndex: Integer;
    FTransColor: TColor;
    FAsyncDrawing: Boolean;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnFrameChanged: TNotifyEvent;
    procedure TimerDeactivate;
    function GetFrameBitmap(Index: Integer; var TransColor: TColor): TBitmap;
    function GetDelayTime(Index: Integer): Cardinal;
    procedure SetAsyncDrawing(Value: Boolean);
    procedure SetAnimate(Value: Boolean);
    procedure SetCenter(Value: Boolean);
    procedure SetImage(Value: TJvGIFImage);
    procedure SetFrameIndex(Value: Integer);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure ImageChanged(Sender: TObject);
    procedure TimerExpired(Sender: TObject);
    { Backwards compatibility; eventually remove }
    procedure ReadJvxAnimate(Reader: TReader);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function GetPalette: HPALETTE; override;
    procedure AdjustSize; override;
    procedure Paint; override;
    procedure DoPaintImage; override;
    procedure Change; dynamic;
    procedure FrameChanged; dynamic;
    procedure Start; dynamic;
    procedure Stop; dynamic;
    { Backwards compatibility; eventually remove }
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AsyncDrawing: Boolean read FAsyncDrawing write SetAsyncDrawing default False;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property AutoSize default True;
    property Center: Boolean read FCenter write SetCenter default False;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex default 0;
    property Image: TJvGIFImage read FImage write SetImage;
    property Loop: Boolean read FLoop write FLoop default True;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Anchors;
    property Constraints;
    property DragKind;
    property Align;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnClick;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvJCLUtils, JvJVCLUtils;

const
  { Maximum delay (10 sec) guarantees that a very long and slow
    GIF does not hang the system }
  MaxDelayTime = 10000;
  MinDelayTime = 50;

procedure TJvGIFAnimator.AdjustSize;
begin
  if not (csReading in ComponentState) then
    if AutoSize and Assigned(FImage) and not FImage.Empty then
      SetBounds(Left, Top, FImage.ScreenWidth, FImage.ScreenHeight);
end;

function TJvGIFAnimator.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) and Assigned(FImage) and
    not FImage.Empty then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FImage.ScreenWidth;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FImage.ScreenHeight;
  end;
end;

procedure TJvGIFAnimator.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TJvGIFAnimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TJvTimer.Create(Self);
  AutoSize := True;
  FImage := TJvGIFImage.Create;
  FGraphic := FImage;
  FImage.OnChange := ImageChanged;
  FCacheIndex := -1;
  FTransColor := clNone;
  FLoop := True;
  FTransparent := True;
end;

procedure TJvGIFAnimator.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('JvxAnimate', ReadJvxAnimate, nil, False);
end;

destructor TJvGIFAnimator.Destroy;
begin
  Destroying;
  FOnStart := nil;
  FOnStop := nil;
  FOnChange := nil;
  FOnFrameChanged := nil;
  Animate := False;
  FCache.Free;
  FImage.OnChange := nil;
  FImage.Free;
  inherited Destroy;
end;

procedure TJvGIFAnimator.DoPaintImage;
var
  Frame: TBitmap;
  Dest: TRect;
  TransColor: TColor;
begin
  { copy image from parent and back-level controls }
  if FImage.Transparent or FImage.Empty then
    CopyParentImage(Self, Canvas);
  if (not FImage.Empty) and (FImage.ScreenWidth > 0) and
    (FImage.ScreenHeight > 0) then
  begin
    TransColor := clNone;
    Frame := GetFrameBitmap(FrameIndex, TransColor);
    Frame.Canvas.Lock;
    try
      if Stretch then
        Dest := ClientRect
      else
      if Center then
        Dest := Bounds((ClientWidth - Frame.Width) div 2,
          (ClientHeight - Frame.Height) div 2, Frame.Width, Frame.Height)
      else
        Dest := Rect(0, 0, Frame.Width, Frame.Height);
      if (TransColor = clNone) or not FTransparent then
        Canvas.StretchDraw(Dest, Frame)
      else
      begin
        StretchBitmapRectTransparent(Canvas, Dest.Left, Dest.Top,
          RectWidth(Dest), RectHeight(Dest), Bounds(0, 0, Frame.Width,
          Frame.Height), Frame, TransColor);
      end;
    finally
      Frame.Canvas.Unlock;
    end;
  end;
end;

procedure TJvGIFAnimator.FrameChanged;
begin
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
end;

function TJvGIFAnimator.GetDelayTime(Index: Integer): Cardinal;
begin
  if (FFrameIndex >= 0) and (FFrameIndex < FImage.Count) and
    (FImage.Count > 1) then
  begin
    Result := FImage.Frames[FFrameIndex].AnimateInterval;
    if Result < MinDelayTime then
      Result := MinDelayTime
    else
    if Result > MaxDelayTime then
      Result := MaxDelayTime;
  end
  else
    Result := 0;
end;

function TJvGIFAnimator.GetFrameBitmap(Index: Integer;
  var TransColor: TColor): TBitmap;
var
  I, Last, First: Integer;
  SavePal: HPALETTE;
  UseCache: Boolean;
begin
  Index := Min(Index, FImage.Count - 1);
  UseCache := (FCache <> nil) and (FCacheIndex = Index - 1) and (FCacheIndex >= 0) and
    (FImage.Frames[FCacheIndex].DisposalMethod <> dmRestorePrevious);
  if UseCache then
  begin
    Result := FCache;
    TransColor := FTransColor;
  end
  else
  begin
    FCache.Free;
    FCache := nil;
    Result := TJvLockedBitmap.Create;
  end;
  Result.Canvas.Lock;
  try
    with Result do
    begin
      if not UseCache then
      begin
        Width := FImage.ScreenWidth;
        Height := FImage.ScreenHeight;
      end;
      Last := Index;
      First := Max(0, Last);
      SavePal := 0;
      if FImage.Palette <> 0 then
      begin
        SavePal := SelectPalette(Canvas.Handle, FImage.Palette, False);
        RealizePalette(Canvas.Handle);
      end;
      if not UseCache then
      begin
        if (FImage.Frames[FImage.FrameIndex].TransparentColor <> clNone) then
        begin
          TransColor := GetNearestColor(Canvas.Handle,
            ColorToRGB(FImage.Frames[FImage.FrameIndex].TransparentColor));
          Canvas.Brush.Color := PaletteColor(TransColor);
        end
        else
        if (FImage.BackgroundColor <> clNone) and FImage.Transparent then
          Canvas.Brush.Color := PaletteColor(FImage.BackgroundColor)
        else
          Canvas.Brush.Color := PaletteColor(clWindow);
        Canvas.FillRect(Bounds(0, 0, Width, Height));
        while First > 0 do
        begin
          if (FImage.ScreenWidth = FImage.Frames[First].Width) and
            (FImage.ScreenHeight = FImage.Frames[First].Height) then
          begin
            if (FImage.Frames[First].TransparentColor = clNone) or
              ((FImage.Frames[First].DisposalMethod = dmRestoreBackground) and
              (First < Last)) then
              Break;
          end;
          Dec(First);
        end;
        for I := First to Last - 1 do
        begin
          with FImage.Frames[I] do
            case DisposalMethod of
              dmUndefined, dmLeave:
                Draw(Canvas, Bounds(Origin.X, Origin.Y, Width, Height), True);
              dmRestoreBackground:
                if I > First then
                  Canvas.FillRect(Bounds(Origin.X, Origin.Y, Width, Height));
              dmRestorePrevious:
                begin { do nothing }
                end;
            end;
        end;
      end
      else
      begin
        with FImage.Frames[FCacheIndex] do
          if DisposalMethod = dmRestoreBackground then
            Canvas.FillRect(Bounds(Origin.X, Origin.Y, Width, Height));
      end;
      with FImage.Frames[Last] do
        Draw(Canvas, Bounds(Origin.X, Origin.Y, Width, Height), True);
      if (not UseCache) and (TransColor <> clNone) and FTransparent then
      begin
        TransparentColor := PaletteColor(TransColor);
        Transparent := True;
      end;
      if FImage.Palette <> 0 then
        SelectPalette(Canvas.Handle, SavePal, False);
    end;
    FCache := Result;
    FCacheIndex := Index;
    FTransColor := TransColor;
    Result.Canvas.Unlock;
  except
    Result.Canvas.Unlock;
    if not UseCache then
      Result.Free;
    raise;
  end;
end;

function TJvGIFAnimator.GetPalette: HPALETTE;
begin
  Result := 0;
  if not FImage.Empty then
    Result := FImage.Palette;
end;

procedure TJvGIFAnimator.ImageChanged(Sender: TObject);
begin
  Lock;
  try
    FCacheIndex := -1;
    FCache.Free;
    FCache := nil;
    FTransColor := clNone;
    FFrameIndex := FImage.FrameIndex;
    if (FFrameIndex >= 0) and (FImage.Count > 0) then
      FTimer.Interval := GetDelayTime(FFrameIndex);
  finally
    Unlock;
  end;
  PictureChanged;
  Change;
end;

procedure TJvGIFAnimator.Paint;
begin
  PaintImage;
  if FImage.Transparent or FImage.Empty then
    PaintDesignRect;
end;

procedure TJvGIFAnimator.ReadJvxAnimate(Reader: TReader);
begin
  Animate := Reader.ReadBoolean;
end;

procedure TJvGIFAnimator.SetAnimate(Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    if Value then
    begin
      FTimer.OnTimer := TimerExpired;
      FTimer.Enabled := True;
      FAnimate := FTimer.Enabled;
      Start;
    end
    else
    begin
      FTimer.Enabled := False;
      FTimer.OnTimer := nil;
      FAnimate := False;
      Stop;
      PictureChanged;
    end;
  end;
end;

procedure TJvGIFAnimator.SetAsyncDrawing(Value: Boolean);
begin
  if FAsyncDrawing <> Value then
  begin
    Lock;
    try
      if Assigned(FTimer) then
        FTimer.SyncEvent := not Value;
      FAsyncDrawing := Value;
    finally
      Unlock;
    end;
  end;
end;

procedure TJvGIFAnimator.SetCenter(Value: Boolean);
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
    if Animate then
      Repaint;
  end;
end;

procedure TJvGIFAnimator.SetFrameIndex(Value: Integer);
begin
  if Value <> FFrameIndex then
  begin
    if (Value < FImage.Count) and (Value >= 0) then
    begin
      Lock;
      try
        FFrameIndex := Value;
        if (FFrameIndex >= 0) and (FImage.Count > 0) then
          FTimer.Interval := GetDelayTime(FFrameIndex);
      finally
        Unlock;
      end;
      FrameChanged;
      PictureChanged;
    end;
  end;
end;

procedure TJvGIFAnimator.SetImage(Value: TJvGIFImage);
begin
  Lock;
  try
    FImage.Assign(Value);
  finally
    Unlock;
  end;
end;

procedure TJvGIFAnimator.SetStretch(Value: Boolean);
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
    if Animate then
      Repaint;
  end;
end;

procedure TJvGIFAnimator.SetTransparent(Value: Boolean);
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
    if Animate then
      Repaint;
  end;
end;

procedure TJvGIFAnimator.Start;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TJvGIFAnimator.Stop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

procedure TJvGIFAnimator.TimerDeactivate;
var
  F: TCustomForm;
begin
  SetAnimate(False);
  if csDesigning in ComponentState then
  begin
    F := GetParentForm(Self);
    if (F <> nil) and (F.Designer <> nil) then
      F.Designer.Modified;
  end;
end;

procedure TJvGIFAnimator.TimerExpired(Sender: TObject);
begin
  if csPaintCopy in ControlState then
    Exit;
  if Visible and (FImage.Count > 1) and (Parent <> nil) and
    Parent.HandleAllocated then
  begin
    Lock;
    try
      if FFrameIndex < FImage.Count - 1 then
        Inc(FFrameIndex)
      else
        FFrameIndex := 0;
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
        if (FFrameIndex >= 0) and (FFrameIndex < FImage.Count) then
          FTimer.Interval := GetDelayTime(FFrameIndex);
      end;
      if not FLoop and (FFrameIndex = 0) then
        if AsyncDrawing then
          FTimer.Synchronize(TimerDeactivate)
        else
          TimerDeactivate;
    finally
      Unlock;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

