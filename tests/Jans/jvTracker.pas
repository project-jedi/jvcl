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

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far Right of the button to move.

-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvTracker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TonChangedValue = procedure(sender: TObject; NewValue: integer) of object;

  TjtbOrientation = (jtbHorizontal, jtbVertical);

  TJvTracker = class(TCustomControl)
  private
    FHitRect: TRect;
    FTrackRect: TRect;
    FTumbRect: TRect;
    FTumbPosition: integer;
    FTumbMin: integer;
    FTumbmax: integer;
    FValue: integer;
    FMinimum: integer;
    FMaximum: integer;
    FTrackColor: TColor;
    FTumbColor: TColor;
    FBackColor: TColor;
    FTumbWidth: integer;
    FTumbHeight: integer;
    FTrackHeight: integer;
    FonChangedValue: TonChangedValue;
    FShowCaption: boolean;
    FCaptionColor: TColor;
    FTrackBorder: boolean;
    FTumbBorder: boolean;
    FBackBorder: boolean;
    FCaptionBold: boolean;
    FOrientation: TjtbOrientation;
    FBackBitmap: TBitmap;
    { Added By Steve Childs, 18/4/00 }
    FbClickWasInRect: Boolean;
    FBorderColor: Tcolor;
    FTrackPositionColor: boolean; // Was the original mouse click in the Track Rect ?

    procedure SetMaximum(const Value: integer);
    procedure SetMinimum(const Value: integer);
    procedure SetValue(const Value: integer);
    procedure SetBackColor(const Value: TColor);
    procedure SetTrackColor(const Value: TColor);
    procedure SetTumbColor(const Value: TColor);
    procedure SetTumbWidth(const Value: integer);
    procedure SetTrackRect;
    procedure SetTumbMinMax;
    procedure SetTumbRect;
    procedure SetTumbHeight(const Value: integer);
    procedure SetTrackHeight(const Value: integer);
    procedure UpdatePosition;
    procedure SetonChangedValue(const Value: TonChangedValue);
    procedure UpdateValue;
    procedure SetCaptionColor(const Value: TColor);
    procedure SetShowCaption(const Value: boolean);
    procedure SetBackBorder(const Value: boolean);
    procedure SetTrackBorder(const Value: boolean);
    procedure SetTumbBorder(const Value: boolean);
    procedure SetCaptionBold(const Value: boolean);
    procedure SetOrientation(const Value: TjtbOrientation);
    procedure SetBackBitmap(const Value: TBitmap);
    procedure BackBitmapChanged(sender: TObject);
    { Added By Steve Childs, 18/4/00 }
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure SetBorderColor(const Value: Tcolor);
    procedure SetTrackPositionColor(const Value: boolean);

    { Private declarations }
  protected
    { Protected declarations }
    procedure doChangedValue(NewValue: integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    { Published declarations }
    property Minimum: integer read FMinimum write SetMinimum;
    property Maximum: integer read FMaximum write SetMaximum;
    property Value: integer read FValue write SetValue;
    property Orientation: TjtbOrientation read FOrientation write SetOrientation;
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property BackColor: TColor read FBackColor write SetBackColor;
    property BackBorder: boolean read FBackBorder write SetBackBorder;
    property TrackColor: TColor read FTrackColor write SetTrackColor;
    property TrackPositionColor: boolean read FTrackPositionColor write SetTrackPositionColor;
    property TrackBorder: boolean read FTrackBorder write SetTrackBorder;
    property BorderColor: Tcolor read FBorderColor write SetBorderColor;
    {
      Changed Next 4 By Steve Childs, 18/4/00, Corrects Spelling Mistake
      Although, this may cause more trouble than it's worth with exisiting users
      So you might want to comment these out
    }
    property ThumbColor: TColor read FTumbColor write SetTumbColor;
    property ThumbBorder: boolean read FTumbBorder write SetTumbBorder;
    property ThumbWidth: integer read FTumbWidth write SetTumbWidth;
    property ThumbHeight: integer read FTumbHeight write SetTumbHeight;

    property TrackHeight: integer read FTrackHeight write SetTrackHeight;
    property ShowCaption: boolean read FShowCaption write SetShowCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property CaptionBold: boolean read FCaptionBold write SetCaptionBold;
    property onChangedValue: TonChangedValue read FonChangedValue write SetonChangedValue;
  end;

implementation

{ TJvTracker }

constructor TJvTracker.Create(AOwner: TComponent);
begin
  inherited;
  width := 150;
  height := 24;
  FOrientation := jtbHorizontal;
  FTrackHeight := 6;
  FTumbWidth := 20;
  FTumbHeight := 16;
  FBackColor := clsilver;
  FTrackColor := clgray;
  FTrackBorder := true;
  FBorderColor := clblack;
  FTumbColor := clsilver;
  FCaptioncolor := clblack;
  FShowCaption := true;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 0;
  FBackBitmap := TBitmap.Create;
  FBackBitmap.OnChange := BackBitmapChanged;
end;

procedure TJvTracker.UpdateValue;
begin
  FValue := round(FMinimum + (FTumbPosition - FTumbMin) / (FTumbMax - FTumbMin) * (FMaximum - FMinimum));
end;

procedure TJvTracker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssleft in shift) then
    if ptinRect(FHitRect, point(x, y)) then
    begin
      {
       Added By Steve Childs 18/04/00 - Set Flag To Tell MouseMove event that
       the mouse was originally clicked in the Track Rect
      }
      FbClickWasInRect := True;
      case Orientation of
        jtbHorizontal: FTumbPosition := x;
        jtbVertical: FTumbPosition := y;
      end;
      UpdateValue;
      SetTumbRect;
      invalidate;
      dochangedValue(FValue);
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
  dy, dx: integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        dy := (height - FTrackHeight) div 2;
        FTrackRect := Rect(FTumbMin, dy, FTumbMax, height - dy);
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
  dx, dy: integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        dx := FTumbWidth div 2;
        dy := (height - FTumbHeight) div 2;
        FTumbrect := Rect(FTumbPosition - dx, dy, FTumbPosition + dx, height - dy);
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
  fact: double;

  procedure DrawBackBitmap;
  var
    ix, iy: Integer;
    BmpWidth, BmpHeight: Integer;
    hCanvas, BmpCanvas: THandle;
    bm: Tbitmap;
  begin
    bm := FBackBitmap;
    begin
      BmpWidth := bm.Width;
      BmpHeight := bm.Height;
      BmpCanvas := bm.Canvas.Handle;
      { Changed By Steve Childs 18/04/00 - Now Points To Buffer.Canvas Bitmap}
      hCanvas := THandle(Buffer.canvas.handle);
      for iy := 0 to ClientHeight div BmpHeight do
        for ix := 0 to ClientWidth div BmpWidth do
          BitBlt(hCanvas, ix * BmpWidth, iy * BmpHeight,
            BmpWidth, BmpHeight, BmpCanvas,
            0, 0, SRCCOPY);
    end;

    { Old Code!!}
 {      hCanvas := THandle(canvas.handle);
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
    begin
      Buffer.canvas.pen.color := FBorderColor; // modified 2-jul-2000 by Jan Verhoeven
    end
    else
    begin
      Buffer.canvas.pen.color := FBackColor;
    end;
    Buffer.canvas.brush.color := FBackColor;
    Buffer.canvas.Rectangle(rect(0, 0, width, height));
  end;

  procedure DrawTrack;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    if FTrackPositionColor then
    begin // 2-jul-2000 Jan Verhoeven
      fact := value / (maximum - minimum);
      r := getrvalue(FtrackColor);
      g := getgvalue(FtrackColor);
      b := getbvalue(FtrackColor);
      col := rgb(trunc(fact * r), trunc(fact * g), trunc(fact * b));
      Buffer.canvas.brush.color := col;
    end
    else
      Buffer.canvas.brush.color := FTrackColor;
    Buffer.canvas.FillRect(FTrackRect);
    Buffer.canvas.pen.style := pssolid;
    if FTrackBorder then
      Frame3D(Buffer.Canvas, FTrackRect, clBlack, clBtnHighlight, 1);
  end;

  procedure DrawCaption;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    s := intToStr(FValue);
    Buffer.canvas.brush.style := bsclear;
    if FCaptionBold then
      Buffer.canvas.font.style := canvas.font.style + [fsbold]
    else
      Buffer.canvas.font.style := canvas.font.style - [fsbold];
    Buffer.canvas.font.color := FCaptionColor;
    drawText(Buffer.canvas.handle, pchar(s), -1, FTumbRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end;

  procedure DrawTumb;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    Buffer.canvas.brush.color := FTumbColor;
    Buffer.canvas.FillRect(FTumbRect);
    Buffer.canvas.pen.style := pssolid;
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
    if assigned(FBackBitmap) and (FBackBitmap.Height <> 0) and (FBackBitmap.Width <> 0) then
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
  invalidate;
end;

procedure TJvTracker.SetMaximum(const Value: integer);
begin
  if value > FMinimum then
  begin
    FMaximum := Value;
    if FValue > FMaximum then
      FValue := FMaximum;
    UpdatePosition;
  end;
end;

procedure TJvTracker.SetMinimum(const Value: integer);
begin
  if value < FMaximum then
  begin
    FMinimum := Value;
    if FValue < FMinimum then
      FValue := FMinimum;
    UpdatePosition;
  end;
end;

procedure TJvTracker.UpdatePosition;
var
  fac: extended;
begin
  fac := (FValue - FMinimum) / (FMaximum - FMinimum);
  FTumbPosition := FTumbMin + round((FTumbMax - FTumbMin) * fac);
  invalidate;
end;

procedure TJvTracker.SetTrackColor(const Value: TColor);
begin
  FTrackColor := Value;
  invalidate;
end;

procedure TJvTracker.SetTumbColor(const Value: TColor);
begin
  FTumbColor := Value;
  invalidate;
end;

procedure TJvTracker.SetValue(const Value: integer);
begin
  if (FValue >= FMinimum) and (FValue <= FMaximum) then
  begin
    FValue := Value;
    UpdatePosition;
    invalidate;
  end;
end;

procedure TJvTracker.SetTumbWidth(const Value: integer);
begin
  FTumbWidth := Value;
  SetTumbMinMax;
  SetTumbrect;
  SetTrackRect;
  invalidate;
end;

procedure TJvTracker.SetTumbHeight(const Value: integer);
begin
  if value < height then
  begin
    FTumbHeight := Value;
    SetTumbMinMax;
    SetTumbrect;
    SetTrackrect;
    invalidate;
  end;
end;

procedure TJvTracker.SetTrackHeight(const Value: integer);
begin
  case Orientation of
    jtbHorizontal:
      begin
        if value < (Height) then
        begin
          FTrackHeight := Value;
          setTrackrect;
          invalidate;
        end;
      end;
    jtbVertical:
      begin
        if value < (Width) then
        begin
          FTrackHeight := Value;
          setTrackrect;
          invalidate;
        end;
      end;
  end;
end;

procedure TJvTracker.SetonChangedValue(const Value: TonChangedValue);
begin
  FonChangedValue := Value;
end;

procedure TJvTracker.doChangedValue(NewValue: integer);
begin
  if assigned(onChangedValue) then
    onchangedvalue(self, NewValue);
end;

procedure TJvTracker.Resize;
begin
  inherited;
  SetTumbMinMax;
  SetTrackRect;
  UpdatePosition;
end;

procedure TJvTracker.SetCaptionColor(const Value: TColor);
begin
  FCaptionColor := Value;
  invalidate;
end;

procedure TJvTracker.SetShowCaption(const Value: boolean);
begin
  FShowCaption := Value;
  invalidate;
end;

procedure TJvTracker.SetBackBorder(const Value: boolean);
begin
  FBackBorder := Value;
  invalidate
end;

procedure TJvTracker.SetTrackBorder(const Value: boolean);
begin
  FTrackBorder := Value;
  invalidate
end;

procedure TJvTracker.SetTumbBorder(const Value: boolean);
begin
  FTumbBorder := Value;
  invalidate;
end;

procedure TJvTracker.SetCaptionBold(const Value: boolean);
begin
  FCaptionBold := Value;
  invalidate;
end;

procedure TJvTracker.SetOrientation(const Value: TjtbOrientation);
var
  tmp: integer;
begin
  FOrientation := Value;
  if (csDesigning in ComponentState) then
  begin
    tmp := width;
    width := height;
    height := tmp;
  end;
  invalidate;
end;

procedure TJvTracker.SetBackBitmap(const Value: TBitmap);
begin
  FBackBitmap.assign(Value);
end;

procedure TJvTracker.BackBitmapChanged(sender: TObject);
begin
  invalidate;
end;

procedure TJvTracker.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
{ Added By Steve Childs 18/04/00
  This elimates the flickering background when the thumb is updated
}
begin
  { Added By Steve Childs 18/04/00 - Tell Windows that we have cleared background }
  msg.Result := -1
end;

procedure TJvTracker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (ssleft in shift) then
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
      if ptinrect(FTrackRect, point(x, y)) then // 2-jul-2000 Jan Verhoeven
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
      invalidate;
      dochangedValue(FValue);
    end;
end;

procedure TJvTracker.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  { Added By Steve Childs 18/04/00 -  Clear Flag}
  FbClickWasInRect := False;
  inherited;
end;

procedure TJvTracker.SetBorderColor(const Value: Tcolor);
begin
  FBorderColor := Value;
end;

procedure TJvTracker.SetTrackPositionColor(const Value: boolean);
begin
  FTrackPositionColor := Value;
  invalidate;
end;

end.
