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
located at http://jvcl.delphi-jedi.org

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far Right of the button to move.

Change: (Lionel Reynaud, 2009-2011)
  - TrackColor property become TrackColorStart
  - Added TrackColorEnd property : now you can have a track color from TrackColorStart to TrackColorEnd
  - Added Step property : now track can go step by step
  - Added the action of the keyboard arrow
  - Added some inherited properties
  - bug corrected : add font property to the draw canvas
  - Add draw min, max values and introduce event property OnShowMinMaxValue

-----------------------------------------------------------------------------}
// $Id$

unit JvTracker;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes, // inline
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  {$IFDEF HAS_UNIT_TYPES}
  Types, // inline
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Graphics, Controls, ExtCtrls,
  SysUtils, Classes,
  JvComponent;

type
  TOnChangedValue = procedure(Sender: TObject; NewValue: Integer) of object;
  TOnShowMinMaxValue = procedure(Sender: TObject; var aMin, aMax: string) of object;

  TjtbOrientation = (jtbHorizontal, jtbVertical);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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
    FTrackColorStart: TColor;
    FTrackColorEnd: TColor;
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
    FClickWasInRect: Boolean; // Was the original mouse click in the Track Rect ?
    FBorderColor: TColor;
    FTrackPositionColored: Boolean;
    FTrackR, FTrackG, FTrackB: Integer;
    DTrackR, DTrackG, DTrackB: Integer;
    FStep: Integer;
    FOnShowMinMaxValue: TOnShowMinMaxValue;
    FShowMinMax: Boolean;
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
    procedure SetTrackColor(Index: Integer; const Value: TColor);
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
    procedure CalculateTrackColor;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure SetShowMinMax(const Value: Boolean);
    procedure ReadTrackColor(Reader: TReader);
  protected
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure DoChangedValue(NewValue: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Added By Steve Childs, 18/4/00 }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BoundsChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Step: Integer read FStep write FStep default 1;
    property Value: Integer read FValue write SetValue default 0;
    property Orientation: TjtbOrientation read FOrientation write SetOrientation default jtbHorizontal;
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property BackBorder: Boolean read FBackBorder write SetBackBorder default False;
    property TrackColorStart: TColor index 0 read FTrackColorStart write SetTrackColor default clGray;
    property TrackColorEnd: TColor index 1 read FTrackColorEnd write SetTrackColor default clGray;
    property TrackPositionColored: Boolean read FTrackPositionColored write SetTrackPositionColored default False;
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
    property ShowMinMax: Boolean read FShowMinMax write SetShowMinMax default False;
    property OnChangedValue: TOnChangedValue read FOnChangedValue write SetOnChangedValue;
    property OnShowMinMaxValue: TOnShowMinMaxValue read FOnShowMinMaxValue write FOnShowMinMaxValue;

    { inherited properties }
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    //property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
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
  FBackColor := clBtnFace;
  TrackColorStart := clGray;
  TrackColorEnd := clGray;
  FTrackBorder := True;
  FBorderColor := clBlack;
  FThumbColor := clSilver;
  FCaptionColor := clBlack;
  FShowCaption := True;
  FShowMinMax := False;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 0;
  FStep := 1;
  FCaptionBold := False;
  FBackBorder := False;
  FBackBitmap := TBitmap.Create;
  FBackBitmap.OnChange := BackBitmapChanged;
end;

procedure TJvTracker.ReadTrackColor(Reader: TReader);
begin
  TrackColorStart := Reader.ReadInteger;
end;

procedure TJvTracker.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // backward compatibility TrackColor was renamed to TrackColotStart (2011-06-11)
  Filer.DefineProperty('TrackColor', ReadTrackColor, nil, False);
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
  if FStep <> 1 then
    FValue := (FValue div FStep) * FStep;
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
  DX, DY: Integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        DY := (Height - FTrackHeight) div 2;
        FTrackRect := Rect(FThumbMin, DY, FThumbMax, Height - DY);
        FHitRect := FTrackRect;
        InflateRect(FHitRect, 0, (FThumbHeight - FTrackHeight) div 2);
      end;
    jtbVertical:
      begin
        DX := (Width - FTrackHeight) div 2;
        FTrackRect := Rect(DX, FThumbMin, Width - DX, FThumbMax);
        FHitRect := FTrackRect;
        InflateRect(FHitRect, (FThumbWidth - FTrackHeight) div 2, 0);
      end;
  end;
end;

procedure TJvTracker.SetThumbRect;
var
  DX, DY: Integer;
begin
  case Orientation of
    jtbHorizontal:
      begin
        DX := FThumbWidth div 2;
        DY := (Height - FThumbHeight) div 2;
        FThumbRect := Rect(FThumbPosition - DX, DY, FThumbPosition + DX, Height - DY);
      end;
    jtbVertical:
      begin
        DY := FThumbHeight div 2;
        DX := (Width - FThumbWidth) div 2;
        FThumbRect := Rect(DX, FThumbPosition - DY, Width - DX, FThumbPosition + DY);
      end;
  end;
end;

procedure TJvTracker.Paint;
var
  {Added By Steve Childs 18/04/00 - Double Buffer Bitmap}
  Buffer: TBitmap;

  procedure DrawBackBitmap;
  var
    IX, IY: Integer;
    BmpWidth, BmpHeight: Integer;
    hCanvas, BmpCanvas: HDC;
  begin
    BmpWidth := FBackBitmap.Width;
    BmpHeight := FBackBitmap.Height;
    BmpCanvas := FBackBitmap.Canvas.Handle;
    { Changed By Steve Childs 18/04/00 - Now Points To Buffer.Canvas Bitmap}
    hCanvas := Buffer.Canvas.Handle;
    for IY := 0 to ClientHeight div BmpHeight do
      for IX := 0 to ClientWidth div BmpWidth do
        BitBlt(hCanvas, IX * BmpWidth, IY * BmpHeight,
          BmpWidth, BmpHeight, BmpCanvas, 0, 0, SRCCOPY);

    { Old Code!!}
 {      hCanvas := THandle(Canvas.handle);
       for IY := 0 to ClientHeight div BmpHeight do
         for IX := 0 to ClientWidth div BmpWidth do
           BitBlt(hCanvas, IX * BmpWidth, IY * BmpHeight,
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
  var
    Factor: Integer;
    LColor: TColor;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    if FTrackPositionColored and (Maximum - Minimum > 0) then
    begin // 2-jul-2000 Jan Verhoeven
{ old colde
      Factor := Value / (Maximum - Minimum);
      R := GetRValue(FTrackColorStart);
      G := GetGValue(FTrackColorStart);
      B := GetBValue(FTrackColorStart);
      LColor := RGB(Trunc(Factor * R), Trunc(Factor * G), Trunc(Factor * B));
}
      Factor :=  Round((Value - Minimum) * 255 / (Maximum - Minimum));
      LColor := RGB(FTrackR + MulDiv(Factor, DTrackR, 255),
                    FTrackG + MulDiv(Factor, DTrackG, 255),
                    FTrackB + MulDiv(Factor, DTrackB, 255));

      Buffer.Canvas.Brush.Color := LColor;
    end
    else
      Buffer.Canvas.Brush.Color := FTrackColorStart;
    Buffer.Canvas.FillRect(FTrackRect);
    Buffer.Canvas.Pen.Style := psSolid;
    if FTrackBorder then
      Frame3D(Buffer.Canvas, FTrackRect, clBlack, clBtnHighlight, 1);
  end;

  procedure DrawCaption;
  var
    S: string;
  begin
    { Changed By Steve Childs 18/04/00 - Now Refers To Buffer Bitmap}
    S := IntToStr(FValue);
    Buffer.Canvas.Brush.Style := bsClear;
    if CaptionBold then
      Buffer.Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Buffer.Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    Buffer.Canvas.Font.Color := CaptionColor;
    DrawText(Buffer.Canvas.Handle, PChar(S), -1, FThumbRect,
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

  procedure DrawMinMax;
  var
    lMin, lMax: string;
    lRect: TRect;
  begin
    lMin := IntToStr(Minimum);
    lMax := IntToStr(Maximum);
    if Assigned(FOnShowMinMaxValue) then
      FOnShowMinMaxValue(Self,lMin,lMax);
    lRect := FTrackRect;
    Buffer.Canvas.Brush.Style := bsClear;
    Buffer.Canvas.Font.Size := Buffer.Canvas.Font.Size - 2; // Reduce size

    case Orientation of
      jtbHorizontal:
        begin
          lRect.Top := lRect.Top + TrackHeight;
          lRect.Bottom := lRect.Bottom + TrackHeight + 4;
          DrawText(Buffer.Canvas.Handle, PChar(lMin), -1, lRect,
            DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
          DrawText(Buffer.Canvas.Handle, PChar(lMax), -1, lRect,
            DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
        end;
      jtbVertical:
        begin
          lRect.Left := lRect.Left + TrackHeight;
          lRect.Right := lRect.Right + TrackHeight + ThumbWidth;
          DrawText(Buffer.Canvas.Handle, PChar(lMin), -1, lRect,
            DT_LEFT or DT_TOP or DT_SINGLELINE or DT_END_ELLIPSIS);
          DrawText(Buffer.Canvas.Handle, PChar(lMax), -1, lRect,
            DT_LEFT or DT_BOTTOM or DT_SINGLELINE or DT_END_ELLIPSIS);
        end;
    end;
  end;

begin
  { Added By Steve Childs 18/04/00 - Added Double Buffering}
  Buffer := TBitmap.Create;
  try
    { Added By Steve Childs 18/04/00 - Setup DoubleBuffer Bitmap}
    Buffer.Width := ClientWidth;
    Buffer.Height := ClientHeight;
    Buffer.Canvas.Font := Font;

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
    if ShowMinMax then
      DrawMinMax;
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
  Factor: Extended;
begin
  Factor := (FValue - FMinimum) / (FMaximum - FMinimum);
  FThumbPosition := FThumbMin + Round((FThumbMax - FThumbMin) * Factor);
  Invalidate;
end;

procedure TJvTracker.CalculateTrackColor;
begin
  FTrackR := FTrackColorStart  and $000000FF;
  FTrackG := (FTrackColorStart shr 8) and $000000FF;
  FTrackB := (FTrackColorStart shr 16) and $000000FF;
  DTrackR := (FTrackColorEnd   and $000000FF) - FTrackR;
  DTrackG := ((FTrackColorEnd  shr 8) and $000000FF) - FTrackG;
  DTrackB := ((FTrackColorEnd  shr 16) and $000000FF) - FTrackB;
end;

procedure TJvTracker.SetTrackColor(Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      if FTrackColorStart <> Value then
      begin
        FTrackColorStart := Value;
        CalculateTrackColor;
        Invalidate;
      end;
    1:
      if FTrackColorEnd <> Value then
      begin
        FTrackColorEnd := Value;
        CalculateTrackColor;
        Invalidate;
      end;
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
    FValue := (Value div FStep) * FStep;
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

procedure TJvTracker.BoundsChanged;
begin
  inherited BoundsChanged;
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

procedure TJvTracker.SetShowMinMax(const Value: Boolean);
begin
  if FShowMinMax <> Value then
  begin
    FShowMinMax := Value;
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

function TJvTracker.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
{ Added By Steve Childs 18/04/00
  This elimates the flickering background when the thumb is updated
}
begin
  { Added By Steve Childs 18/04/00 - Tell Windows that we have cleared background }
  Result := True;
end;

procedure TJvTracker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.SetFocus;
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

procedure TJvTracker.CNKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_LEFT, VK_UP:
      Value := Value - FStep;
    VK_RIGHT, VK_DOWN:
      Value := Value + FStep;
  else
    inherited;
    Exit;
  end;
  DoChangedValue(FValue);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
