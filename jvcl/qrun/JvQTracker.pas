{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTracker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far Right of the button to move.

-----------------------------------------------------------------------------}
// $Id$

unit JvQTracker;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, QGraphics, QControls, QExtCtrls,
  SysUtils, Classes,
  JvQComponent;

type
  TOnChangedValue = procedure(Sender: TObject; NewValue: Integer) of object;

  TjtbOrientation = (jtbHorizontal, jtbVertical);

  TJvTracker = class(TJvCustomControl)
  private
    FHitRect: TRect;
    FTrackRect: TRect;
    FThumbRect: TRect;
    FThumbPosition: Integer;
    FThumbMin: Integer;
    FThumbMax: Integer;
    FValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FTrackColor: TColor;
    FThumbColor: TColor;
    FBackColor: TColor;
    FThumbWidth: Integer;
    FThumbHeight: Integer;
    FTrackHeight: Integer;
    FOnChangedValue: TOnChangedValue;
    FShowCaption: Boolean;
    FCaptionColor: TColor;
    FTrackBorder: Boolean;
    FThumbBorder: Boolean;
    FBackBorder: Boolean;
    FCaptionBold: Boolean;
    FOrientation: TjtbOrientation;
    FBackBitmap: TBitmap;
    { Added By Steve Childs, 18/4/00 }
    FClickWasInRect: Boolean;
    FBorderColor: TColor;
    FTrackPositionColored: Boolean; // Was the original mouse click in the Track Rect ?
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
    procedure SetTrackColor(const Value: TColor);
    procedure SetThumbColor(const Value: TColor);
    procedure SetThumbWidth(const Value: Integer);
    procedure SetTrackRect;
    procedure SetThumbMinMax;
    procedure SetThumbRect;
    procedure SetThumbHeight(const Value: Integer);
    procedure SetTrackHeight(const Value: Integer);
    procedure UpdatePosition;
    procedure SetOnChangedValue(const Value: TOnChangedValue);
    procedure UpdateValue;
    procedure SetCaptionColor(const Value: TColor);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetBackBorder(const Value: Boolean);
    procedure SetTrackBorder(const Value: Boolean);
    procedure SetThumbBorder(const Value: Boolean);
    procedure SetCaptionBold(const Value: Boolean);
    procedure SetOrientation(const Value: TjtbOrientation);
    procedure SetBackBitmap(const Value: TBitmap);
    procedure BackBitmapChanged(Sender: TObject);
    { Added By Steve Childs, 18/4/00 }
    procedure SetBorderColor(const Value: TColor);
    procedure SetTrackPositionColored(const Value: Boolean);
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure DoChangedValue(NewValue: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoBoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Value: Integer read FValue write SetValue default 0;
    property Orientation: TjtbOrientation read FOrientation write SetOrientation default jtbHorizontal;
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property BackColor: TColor read FBackColor write SetBackColor default clSilver;
    property BackBorder: Boolean read FBackBorder write SetBackBorder default False;
    property TrackColor: TColor read FTrackColor write SetTrackColor default clGray;
    property TrackPositionColored: Boolean read FTrackPositionColored write SetTrackPositionColored;
    property TrackBorder: Boolean read FTrackBorder write SetTrackBorder default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    {
      Changed Next 4 By Steve Childs, 18/4/00, Corrects Spelling Mistake
      Although, this may cause more trouble than it's worth with exisiting users
      So you might want to comment these out
    }
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clSilver;
    property ThumbBorder: Boolean read FThumbBorder write SetThumbBorder default False;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth default 20;
    property ThumbHeight: Integer read FThumbHeight write SetThumbHeight default 16;
    property TrackHeight: Integer read FTrackHeight write SetTrackHeight default 6;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clBlack;
    property CaptionBold: Boolean read FCaptionBold write SetCaptionBold default False;
    property OnChangedValue: TOnChangedValue read FOnChangedValue write SetOnChangedValue;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvTracker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 24;
  FOrientation := jtbHorizontal;
  FTrackHeight := 6;
  FThumbWidth := 20;
  FThumbHeight := 16;
  FThumbBorder := False;
  FBackColor := clSilver;
  FTrackColor := clGray;
  FTrackBorder := True;
  FBorderColor := clBlack;
  FThumbColor := clSilver;
  FCaptionColor := clBlack;
  FShowCaption := True;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 0;
  FCaptionBold := False;
  FBackBorder := False;
  FBackBitmap := TBitmap.Create;
  FBackBitmap.OnChange := BackBitmapChanged;
end;

destructor TJvTracker.Destroy;
begin
  FBackBitmap.OnChange := nil;
  FBackBitmap.Free;
  inherited Destroy;
end;

procedure TJvTracker.UpdateValue;
begin
  FValue := Round(FMinimum +
    (FThumbPosition - FThumbMin) / (FThumbMax - FThumbMin) * (FMaximum - FMinimum));
end;

procedure TJvTracker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    if PtInRect(FHitRect, Point(X, Y)) then
    begin
      {
       Added By Steve Childs 18/04/00 - Set Flag To Tell MouseMove event that
       the mouse was originally clicked in the Track Rect
      }
      FClickWasInRect := True;
      case Orientation of
        jtbHorizontal:
          FThumbPosition := X;
        jtbVertical:
          FThumbPosition := Y;
      end;
      UpdateValue;
      SetThumbRect;
      Invalidate;
      DoChangedValue(FValue);
    end;
end;

procedure TJvTracker.SetThumbMinMax;
begin
  case Orientation of
    jtbHorizontal:
      begin
        FThumbMin := 5 + (FThumbWidth div 2);
        FThumbMax := Width - FThumbMin;
      end;
    jtbVertical:
      begin
        FThumbMin := 5 + (FThumbHeight div 2);
        FThumbMax := Height - FThumbMin;
      end;
  end;
end;

procedure TJvTracker.SetTrackRect;
var
  dy, dx: Integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        dy := (Height - FTrackHeight) div 2;
        FTrackRect := Rect(FThumbMin, dy, FThumbMax, Height - dy);
        FHitRect := FTrackRect;
        InflateRect(FHitRect, 0, (FThumbHeight - FTrackHeight) div 2);
      end;
    jtbVertical:
      begin
        dx := (Width - FTrackHeight) div 2;
        FTrackRect := Rect(dx, FThumbMin, Width - dx, FThumbMax);
        FHitRect := FTrackRect;
        InflateRect(FHitRect, (FThumbWidth - FTrackHeight) div 2, 0);
      end;
  end;
end;

procedure TJvTracker.SetThumbRect;
var
  dx, dy: Integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        dx := FThumbWidth div 2;
        dy := (Height - FThumbHeight) div 2;
        FThumbRect := Rect(FThumbPosition - dx, dy, FThumbPosition + dx, Height - dy);
      end;
    jtbVertical:
      begin
        dy := FThumbHeight div 2;
        dx := (Width - FThumbWidth) div 2;
        FThumbRect := Rect(dx, FThumbPosition - dy, Width - dx, FThumbPosition + dy);
      end;
  end;
end;

procedure TJvTracker.Paint;
var
  s: string;
  {Added By Steve Childs 18/04/00 - Double Buffer Bitmap}
  Buffer: TBitmap;
  col: TColor;
  r, g, b: Byte;
  fact: Double;

  procedure DrawBackBitmap;
  var
    ix, iy: Integer;
    BmpWidth, BmpHeight: Integer;
    hCanvas, BmpCanvas: HDC;
    bm: TBitmap;
  begin
    bm := FBackBitmap;
    begin
      BmpWidth := bm.Width;
      BmpHeight := bm.Height;
      BmpCanvas := bm.Canvas.Handle;
      { Changed By Steve Childs 18/04/00 - Now Points To Buffer.Canvas Bitmap}
      hCanvas := HDC(Buffer.Canvas.Handle);
      for iy := 0 to ClientHeight div BmpHeight do
        for ix := 0 to ClientWidth div BmpWidth do
          BitBlt(hCanvas, ix * BmpWidth, iy * BmpHeight,
            BmpWidth, BmpHeight, BmpCanvas,
            0, 0, SRCCOPY);
    end;

    { Old Code!!}
 {      hCanvas := THandle(Canvas.handle);
       for iy := 0 to ClientHeight div BmpHeight do
         for ix := 0 to ClientWidth div BmpWidth do
           BitBlt(hCanvas, ix * BmpWidth, iy * BmpHeight,
             BmpWidth, BmpHeight, BmpCanvas,
             0, 0, SRCCOPY);
     end;}
  end;

  procedure DrawBackground;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    if FBackBorder then
      Buffer.Canvas.Pen.Color := FBorderColor // modified 2-jul-2000 by Jan Verhoeven
    else
      Buffer.Canvas.Pen.Color := FBackColor;
    Buffer.Canvas.Brush.Color := FBackColor;
    Buffer.Canvas.Rectangle(Rect(0, 0, Width, Height));
  end;

  procedure DrawTrack;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    if FTrackPositionColored then
    begin // 2-jul-2000 Jan Verhoeven
      fact := Value / (Maximum - Minimum);
      r := GetRValue(FTrackColor);
      g := GetGValue(FTrackColor);
      b := GetBValue(FTrackColor);
      col := RGB(Trunc(fact * r), Trunc(fact * g), Trunc(fact * b));
      Buffer.Canvas.Brush.Color := col;
    end
    else
      Buffer.Canvas.Brush.Color := FTrackColor;
    Buffer.Canvas.FillRect(FTrackRect);
    Buffer.Canvas.Pen.Style := psSolid;
    if FTrackBorder then
      Frame3D(Buffer.Canvas, FTrackRect, clBlack, clBtnHighlight, 1);
  end;

  procedure DrawCaption;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    s := IntToStr(FValue);
    Buffer.Canvas.Brush.Style := bsClear;
    if CaptionBold then
      Buffer.Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Buffer.Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    Buffer.Canvas.Font.Color := CaptionColor;
    DrawText(Buffer.Canvas.Handle, PChar(s), -1, FThumbRect,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end;

  procedure DrawThumb;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    Buffer.Canvas.Brush.Color := FThumbColor;
    Buffer.Canvas.FillRect(FThumbRect);
    Buffer.Canvas.Pen.Style := psSolid;
    Frame3D(Buffer.Canvas, FThumbRect, clBtnHighlight, clBlack, 1);
  end;

begin
  { Added By Steve Childs 18/04/00 - Added Double Buffering}
  Buffer := TBitmap.Create;
  try
    { Added By Steve Childs 18/04/00 - Setup DoubleBuffer Bitmap}
    Buffer.Width := ClientWidth;
    Buffer.Height := ClientHeight;

    SetThumbMinMax;
    SetThumbRect;
    SetTrackRect;
    if Assigned(FBackBitmap) and (FBackBitmap.Height <> 0) and (FBackBitmap.Width <> 0) then
      DrawBackBitmap
    else
      DrawBackground;
    DrawTrack;
    DrawThumb;
    if ShowCaption then
      DrawCaption;
  finally
    { Added By Steve Childs 18/04/00 - Finally, Draw the Buffer onto Main Canvas}
    Canvas.Draw(0, 0, Buffer);
    { Added By Steve Childs 18/04/00 - Free Buffer}
    Buffer.Free;
  end;
end;

procedure TJvTracker.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetMaximum(const Value: Integer);
begin
  if (Value <> FMaximum) and (Value > FMinimum) then
  begin
    FMaximum := Value;
    if FValue > FMaximum then
      FValue := FMaximum;
    UpdatePosition;
  end;
end;

procedure TJvTracker.SetMinimum(const Value: Integer);
begin
  if (Value <> FMinimum) and (Value < FMaximum) then
  begin
    FMinimum := Value;
    if FValue < FMinimum then
      FValue := FMinimum;
    UpdatePosition;
  end;
end;

procedure TJvTracker.UpdatePosition;
var
  fac: Extended;
begin
  fac := (FValue - FMinimum) / (FMaximum - FMinimum);
  FThumbPosition := FThumbMin + Round((FThumbMax - FThumbMin) * fac);
  Invalidate;
end;

procedure TJvTracker.SetTrackColor(const Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetThumbColor(const Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetValue(const Value: Integer);
begin
  if (Value <> FValue) and (Value >= FMinimum) and (Value <= FMaximum) then
  begin
    FValue := Value;
    UpdatePosition;
    Invalidate;
  end;
end;

procedure TJvTracker.SetThumbWidth(const Value: Integer);
begin
  if FThumbWidth <> Value then
  begin
    FThumbWidth := Value;
    SetThumbMinMax;
    SetThumbRect;
    SetTrackRect;
    Invalidate;
  end;
end;

procedure TJvTracker.SetThumbHeight(const Value: Integer);
begin
  if (FThumbHeight <> Value) and (Value < Height) then
  begin
    FThumbHeight := Value;
    SetThumbMinMax;
    SetThumbRect;
    SetTrackRect;
    Invalidate;
  end;
end;

procedure TJvTracker.SetTrackHeight(const Value: Integer);
begin
  case Orientation of
    jtbHorizontal:
      if (FTrackHeight <> Value) and (Value < Height) then
      begin
        FTrackHeight := Value;
        SetTrackRect;
        Invalidate;
      end;
    jtbVertical:
      if (FTrackHeight <> Value) and (Value < Width) then
      begin
        FTrackHeight := Value;
        SetTrackRect;
        Invalidate;
      end;
  end;
end;

procedure TJvTracker.SetOnChangedValue(const Value: TOnChangedValue);
begin
  FOnChangedValue := Value;
end;

procedure TJvTracker.DoChangedValue(NewValue: Integer);
begin
  if Assigned(FOnChangedValue) then
    FOnChangedValue(Self, NewValue);
end;

procedure TJvTracker.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  SetThumbMinMax;
  SetTrackRect;
  UpdatePosition;
end;

procedure TJvTracker.SetCaptionColor(const Value: TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetBackBorder(const Value: Boolean);
begin
  if FBackBorder <> Value then
  begin
    FBackBorder := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetTrackBorder(const Value: Boolean);
begin
  if FTrackBorder <> Value then
  begin
    FTrackBorder := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetThumbBorder(const Value: Boolean);
begin
  if FThumbBorder <> Value then
  begin
    FThumbBorder := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetCaptionBold(const Value: Boolean);
begin
  if FCaptionBold <> Value then
  begin
    FCaptionBold := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetOrientation(const Value: TjtbOrientation);
var
  Tmp: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if csDesigning in ComponentState then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TJvTracker.SetBackBitmap(const Value: TBitmap);
begin
  FBackBitmap.Assign(Value);
end;

procedure TJvTracker.BackBitmapChanged(Sender: TObject);
begin
  Invalidate;
end;

function TJvTracker.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
{ Added By Steve Childs 18/04/00
  This elimates the flickering background when the thumb is updated
}
begin
  { Added By Steve Childs 18/04/00 - Tell Windows that we have cleared background }
  Result := True;
end;

procedure TJvTracker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ssLeft in Shift then
    if FClickWasInRect then
    begin
      {
        - Added By Steve Childs 18/04/00
        OK, we know that when the mouse button went down, the
        click was in the rect. So, we only need to check that it's now
        within the bounds of the track (otherwise the button goes off the
        end of the track!!)

      }
  //    If (X >= FTrackRect.Left) and (X <= FTrackRect.Right) then
      if PtInRect(FTrackRect, Point(X, Y)) then // 2-jul-2000 Jan Verhoeven
        if Orientation = jtbHorizontal then
          FThumbPosition := X
        else
          FThumbPosition := Y
      else
      begin
        { Added By Steve Childs 18/04/00
          If it's off the edges - Set Either to left or right, depending on
          which side the mouse is!!
        }
        // 2-jul-2000 Jan Verhoeven
        if Orientation = jtbHorizontal then
        begin
          if X < FTrackRect.Left then
            FThumbPosition := FTrackRect.Left - 1
          else
          if X > FTrackRect.Right then
            FThumbPosition := FTrackRect.Right + 1
          else
            FThumbPosition := X;
        end
        else
        begin
          if Y < FTrackRect.Top then
            FThumbPosition := FTrackRect.Top - 1
          else
          if Y > FTrackRect.Bottom then
            FThumbPosition := FTrackRect.Bottom + 1
          else
            FThumbPosition := Y;
        end;
        {      If X < FTrackRect.Left then
                FThumbPosition := FTrackRect.Left-1
              else
                // Must Be Off Right
                FThumbPosition := FTrackRect.Right+1;}
      end;
      UpdateValue;
      SetThumbRect;
      Invalidate;
      DoChangedValue(FValue);
    end;
end;

procedure TJvTracker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  { Added By Steve Childs 18/04/00 -  Clear Flag}
  FClickWasInRect := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvTracker.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TJvTracker.SetTrackPositionColored(const Value: Boolean);
begin
  if FTrackPositionColored <> Value then
  begin
    FTrackPositionColored := Value;
    Invalidate;
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
