{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTracker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2004-03-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far Right of the button to move.

-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvTracker;

interface

uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QExtCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JvComponent;

type
  TOnChangedValue = procedure(Sender: TObject; NewValue: Integer) of object;

  TjtbOrientation = (jtbHorizontal, jtbVertical);

  TJvTracker = class(TJvCustomControl)
  private
    FHitRect: TRect;
    FTrackRect: TRect;
    FTumbRect: TRect;
    FTumbPosition: Integer;
    FTumbMin: Integer;
    FTumbmax: Integer;
    FValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FTrackColor: TColor;
    FTumbColor: TColor;
    FBackColor: TColor;
    FTumbWidth: Integer;
    FTumbHeight: Integer;
    FTrackHeight: Integer;
    FOnChangedValue: TOnChangedValue;
    FShowCaption: Boolean;
    FCaptionColor: TColor;
    FTrackBorder: Boolean;
    FTumbBorder: Boolean;
    FBackBorder: Boolean;
    FCaptionBold: Boolean;
    FOrientation: TjtbOrientation;
    FBackBitmap: TBitmap;
    { Added By Steve Childs, 18/4/00 }
    FbClickWasInRect: Boolean;
    FBorderColor: TColor;
    FTrackPositionColor: Boolean; // Was the original mouse click in the Track Rect ?

    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
    procedure SetTrackColor(const Value: TColor);
    procedure SetTumbColor(const Value: TColor);
    procedure SetTumbWidth(const Value: Integer);
    procedure SetTrackRect;
    procedure SetTumbMinMax;
    procedure SetTumbRect;
    procedure SetTumbHeight(const Value: Integer);
    procedure SetTrackHeight(const Value: Integer);
    procedure UpdatePosition;
    procedure SetOnChangedValue(const Value: TOnChangedValue);
    procedure UpdateValue;
    procedure SetCaptionColor(const Value: TColor);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetBackBorder(const Value: Boolean);
    procedure SetTrackBorder(const Value: Boolean);
    procedure SetTumbBorder(const Value: Boolean);
    procedure SetCaptionBold(const Value: Boolean);
    procedure SetOrientation(const Value: TjtbOrientation);
    procedure SetBackBitmap(const Value: TBitmap);
    procedure BackBitmapChanged(Sender: TObject);
    { Added By Steve Childs, 18/4/00 }
    procedure SetBorderColor(const Value: TColor);
    procedure SetTrackPositionColor(const Value: Boolean);
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
    procedure Paint; override;
  published
    property Minimum: Integer read FMinimum write SetMinimum;
    property Maximum: Integer read FMaximum write SetMaximum;
    property Value: Integer read FValue write SetValue;
    property Orientation: TjtbOrientation read FOrientation write SetOrientation;
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property BackColor: TColor read FBackColor write SetBackColor;
    property BackBorder: Boolean read FBackBorder write SetBackBorder;
    property TrackColor: TColor read FTrackColor write SetTrackColor;
    property TrackPositionColor: Boolean read FTrackPositionColor write SetTrackPositionColor;
    property TrackBorder: Boolean read FTrackBorder write SetTrackBorder;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    {
      Changed Next 4 By Steve Childs, 18/4/00, Corrects Spelling Mistake
      Although, this may cause more trouble than it's worth with exisiting users
      So you might want to comment these out
    }
    property ThumbColor: TColor read FTumbColor write SetTumbColor;
    property ThumbBorder: Boolean read FTumbBorder write SetTumbBorder;
    property ThumbWidth: Integer read FTumbWidth write SetTumbWidth;
    property ThumbHeight: Integer read FTumbHeight write SetTumbHeight;

    property TrackHeight: Integer read FTrackHeight write SetTrackHeight;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property CaptionBold: Boolean read FCaptionBold write SetCaptionBold;
    property OnChangedValue: TOnChangedValue read FOnChangedValue write SetOnChangedValue;
  end;

implementation

{ TJvTracker }

constructor TJvTracker.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 24;
  FOrientation := jtbHorizontal;
  FTrackHeight := 6;
  FTumbWidth := 20;
  FTumbHeight := 16;
  FBackColor := clsilver;
  FTrackColor := clgray;
  FTrackBorder := True;
  FBorderColor := clblack;
  FTumbColor := clsilver;
  FCaptioncolor := clblack;
  FShowCaption := True;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 0;
  FBackBitmap := TBitmap.Create;
  FBackBitmap.OnChange := BackBitmapChanged;
end;

procedure TJvTracker.UpdateValue;
begin
  FValue := Round(FMinimum +
    (FTumbPosition - FTumbMin) / (FTumbMax - FTumbMin) * (FMaximum - FMinimum));
end;

procedure TJvTracker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    if PtInRect(FHitRect, Point(x, y)) then
    begin
      {
       Added By Steve Childs 18/04/00 - Set Flag To Tell MouseMove event that
       the mouse was originally clicked in the Track Rect
      }
      FbClickWasInRect := True;
      case Orientation of
        jtbHorizontal:
          FTumbPosition := x;
        jtbVertical:
          FTumbPosition := y;
      end;
      UpdateValue;
      SetTumbRect;
      Invalidate;
      DoChangedValue(FValue);
    end;
end;

procedure TJvTracker.SetTumbMinMax;
begin
  case Orientation of
    jtbHorizontal:
      begin
        FTumbMin := 5 + (FTumbwidth div 2);
        FTumbMax := Width - FTumbMin;
      end;
    jtbVertical:
      begin
        FTumbMin := 5 + (FTumbHeight div 2);
        FTumbMax := Height - FTumbMin;
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
        FTrackRect := Rect(FTumbMin, dy, FTumbMax, Height - dy);
        FHitRect := FTrackrect;
        inflateRect(FHitRect, 0, (FTumbHeight - FTrackHeight) div 2);
      end;
    jtbVertical:
      begin
        dx := (Width - FTrackHeight) div 2;
        FTrackRect := Rect(dx, FTumbMin, Width - dx, FTumbMax);
        FHitRect := FTrackrect;
        inflateRect(FHitRect, (FTumbWidth - FTrackHeight) div 2, 0);
      end;
  end;
end;

procedure TJvTracker.SetTumbRect;
var
  dx, dy: Integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        dx := FTumbWidth div 2;
        dy := (Height - FTumbHeight) div 2;
        FTumbrect := Rect(FTumbPosition - dx, dy, FTumbPosition + dx, Height - dy);
      end;
    jtbVertical:
      begin
        dy := FTumbHeight div 2;
        dx := (Width - FTumbWidth) div 2;
        FTumbrect := Rect(dx, FTumbPosition - dy, Width - dx, FTumbPosition + dy);
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
      hCanvas := HDC(Buffer.Canvas.handle);
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

  procedure DrawBackGround;
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
    if FTrackPositionColor then
    begin // 2-jul-2000 Jan Verhoeven
      fact := Value / (maximum - minimum);
      r := GetRValue(FtrackColor);
      g := GetGValue(FtrackColor);
      b := GetBValue(FtrackColor);
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
    Buffer.Canvas.Brush.Style := bsclear;
    if FCaptionBold then
      Buffer.Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Buffer.Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    Buffer.Canvas.Font.Color := FCaptionColor;
    DrawText(Buffer.Canvas.Handle, PChar(s), -1, FTumbRect,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end;

  procedure DrawTumb;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    Buffer.Canvas.Brush.Color := FTumbColor;
    Buffer.Canvas.FillRect(FTumbRect);
    Buffer.Canvas.Pen.Style := psSolid;
    Frame3D(Buffer.Canvas, FTumbRect, clBtnHighlight, clBlack, 1);
  end;

begin
  { Added By Steve Childs 18/04/00 - Added Double Buffering}
  Buffer := TBitmap.Create;
  try
    { Added By Steve Childs 18/04/00 - Setup DoubleBuffer Bitmap}
    Buffer.Width := ClientWidth;
    Buffer.Height := ClientHeight;

    SetTumbMinMax;
    SetTumbRect;
    SetTrackRect;
    if Assigned(FBackBitmap) and (FBackBitmap.Height <> 0) and (FBackBitmap.Width <> 0) then
      DrawBackBitmap
    else
      DrawBackground;
    DrawTrack;
    DrawTumb;
    if FShowCaption then
      DrawCaption;
  finally
    { Added By Steve Childs 18/04/00 - Finally, Draw the Buffer Onto Main Canvas}
    Canvas.Draw(0, 0, Buffer);
    { Added By Steve Childs 18/04/00 - Free Buffer}
    Buffer.Free;
  end;
end;

procedure TJvTracker.SetBackColor(const Value: TColor);
begin
  FBackColor := Value;
  Invalidate;
end;

procedure TJvTracker.SetMaximum(const Value: Integer);
begin
  if Value > FMinimum then
  begin
    FMaximum := Value;
    if FValue > FMaximum then
      FValue := FMaximum;
    UpdatePosition;
  end;
end;

procedure TJvTracker.SetMinimum(const Value: Integer);
begin
  if Value < FMaximum then
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
  FTumbPosition := FTumbMin + round((FTumbMax - FTumbMin) * fac);
  Invalidate;
end;

procedure TJvTracker.SetTrackColor(const Value: TColor);
begin
  FTrackColor := Value;
  Invalidate;
end;

procedure TJvTracker.SetTumbColor(const Value: TColor);
begin
  FTumbColor := Value;
  Invalidate;
end;

procedure TJvTracker.SetValue(const Value: Integer);
begin
  if (FValue >= FMinimum) and (FValue <= FMaximum) then
  begin
    FValue := Value;
    UpdatePosition;
    Invalidate;
  end;
end;

procedure TJvTracker.SetTumbWidth(const Value: Integer);
begin
  FTumbWidth := Value;
  SetTumbMinMax;
  SetTumbrect;
  SetTrackRect;
  Invalidate;
end;

procedure TJvTracker.SetTumbHeight(const Value: Integer);
begin
  if Value < Height then
  begin
    FTumbHeight := Value;
    SetTumbMinMax;
    SetTumbrect;
    SetTrackrect;
    Invalidate;
  end;
end;

procedure TJvTracker.SetTrackHeight(const Value: Integer);
begin
  case Orientation of
    jtbHorizontal:
      begin
        if Value < (Height) then
        begin
          FTrackHeight := Value;
          setTrackrect;
          Invalidate;
        end;
      end;
    jtbVertical:
      begin
        if Value < (Width) then
        begin
          FTrackHeight := Value;
          setTrackrect;
          Invalidate;
        end;
      end;
  end;
end;

procedure TJvTracker.SetOnChangedValue(const Value: TOnChangedValue);
begin
  FOnChangedValue := Value;
end;

procedure TJvTracker.DoChangedValue(NewValue: Integer);
begin
  if Assigned(OnChangedValue) then
    OnChangedValue(self, NewValue);
end;

procedure TJvTracker.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  SetTumbMinMax;
  SetTrackRect;
  UpdatePosition;
end;

procedure TJvTracker.SetCaptionColor(const Value: TColor);
begin
  FCaptionColor := Value;
  Invalidate;
end;

procedure TJvTracker.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  Invalidate;
end;

procedure TJvTracker.SetBackBorder(const Value: Boolean);
begin
  FBackBorder := Value;
  Invalidate
end;

procedure TJvTracker.SetTrackBorder(const Value: Boolean);
begin
  FTrackBorder := Value;
  Invalidate
end;

procedure TJvTracker.SetTumbBorder(const Value: Boolean);
begin
  FTumbBorder := Value;
  Invalidate;
end;

procedure TJvTracker.SetCaptionBold(const Value: Boolean);
begin
  FCaptionBold := Value;
  Invalidate;
end;

procedure TJvTracker.SetOrientation(const Value: TjtbOrientation);
var
  tmp: Integer;
begin
  FOrientation := Value;
  if (csDesigning in ComponentState) then
  begin
    tmp := Width;
    Width := Height;
    Height := tmp;
  end;
  Invalidate;
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
  inherited;
  if (ssLeft in Shift) then
    if FbClickWasInRect then
    begin
      {
        - Added By Steve Childs 18/04/00
        OK, we know that when the mouse button went down, the
        click was in the rect. So, we only need to check that it's now
        within the bounds of the track (otherwise the button goes off the
        end of the track!!)

      }
  //    If (X >= FTrackRect.Left) and (X <= FTrackRect.Right) then
      if PtInRect(FTrackRect, Point(x, y)) then // 2-jul-2000 Jan Verhoeven
        if Orientation = jtbHorizontal then
          FTumbPosition := x
        else
          FTumbPosition := y
      else
      begin
        { Added By Steve Childs 18/04/00
          If it's off the edges - Set Either to left or right, depending on
          which side the mouse is!!
        }
        // 2-jul-2000 Jan Verhoeven
        if Orientation = jtbHorizontal then
        begin
          if x < FTrackRect.left then
            FTumbPosition := FTrackRect.Left - 1
          else if x > FTrackRect.right then
            FTumbPosition := FTrackRect.Right + 1
          else
            FTumbPosition := x;
        end
        else
        begin
          if y < FTrackRect.top then
            FTumbPosition := FTrackRect.top - 1
          else if y > FTrackRect.bottom then
            FTumbPosition := FTrackRect.bottom + 1
          else
            FTumbPosition := y;
        end;
        {      If X < FTrackRect.Left then
                FTumbPosition := FTrackRect.Left-1
              else
                // Must Be Off Right
                FTumbPosition := FTrackRect.Right+1;}
      end;
      UpdateValue;
      SetTumbRect;
      Invalidate;
      DoChangedValue(FValue);
    end;
end;

procedure TJvTracker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  { Added By Steve Childs 18/04/00 -  Clear Flag}
  FbClickWasInRect := False;
  inherited;
end;

procedure TJvTracker.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
end;

procedure TJvTracker.SetTrackPositionColor(const Value: Boolean);
begin
  FTrackPositionColor := Value;
  Invalidate;
end;

end.
