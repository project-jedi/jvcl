{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFlyingText.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgFlyingText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvgTypes, JvgUtils, JvComponent, JvgCommClasses;

type
  TMyRect = record
    Left, Top, Width, Height: integer;
  end;

  TTextLineChangingEvent = procedure(Sender: TObject; LineNum: integer) of
    object;

  TJvgFlyingText = class(TJvCustomPanel)
  private
    FHorAlign: TglHorAlign;
    FVertAlign: TglVertAlign;
    FTransparent: boolean;
    FBackgrColor: TColor;
    FGradient: TJvgGradient;
    FThreeDGradient: TJvg3DGradient;
    FInteriorOffset: word;
    FScalePercent: TSpPercent;
    FStepScaleFactor: single;
    FResultFont: TFont;
    FTimerInterval: word;
    FActive: boolean;
    FClearOldText: boolean;
    FClearOldTextWhileDrawing: boolean;
    FCycled: boolean;
    FText: TStringList;
    FView3D: TJvg2DAlign;
    FDirection: TglScalingDir;
    FFastDraw: boolean;
    FShowTextWhilePassive: boolean;

    FOnTextLineChanging: TTextLineChangingEvent;

    FR, OldFR: TMyRect;
    FP, OldFP: TPoint;
    FBackgrBitmap,
      FTxtBitmap,
      FResBitmap,
      FScaledTxtBitmap,
      FPartTxtBitmap: TBitmap;
    fNeedRebuildBitmaps: boolean;
    FTimer: TTimer;
    FScaledWidth,
      FScaledHeight,
      FOldScaledWidth,
      FOldScaledHeight: single;
    FVisible: boolean;
    FStepShift: TJvgPointClass;

    Shift, OldShift: TPoint;
    uCurTextLine: Word;
    fNeedRepaintBackground: boolean;
    fNeedRemakeBackground: boolean;
    fLoaded: boolean;
    procedure CalcTxtBitmapWidth;
    procedure BuildBitmaps;
    procedure BuildTxtBitmap;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure OnParamsChanged(Sender: TObject);
    procedure Repaint_;

    procedure SetHorAlign(Value: TglHorAlign);
    procedure SetVertAlign(Value: TglVertAlign);
    procedure SetTransparent(Value: boolean);
    procedure SetBackgrColor(Value: TColor);
    procedure SetInteriorOffset(Value: word);
    procedure SetScalePercent(Value: TSpPercent);
    procedure SetStepScaleFactor(Value: Single);
    procedure SetResultFont(Value: TFont);
    procedure SetTimerInterval(Value: word);
    procedure SetActive(Value: boolean);
    procedure SetText(Value: TStringList);
    procedure SetFastDraw(Value: boolean);
    procedure SetDirection(Value: TglScalingDir);
    procedure SetShowTextWhilePassive(Value: boolean);
    procedure SetVisible(Value: boolean);

  protected
    property Color; //...hide
    procedure Paint; override;

  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnTimerProc(Sender: TObject);
    procedure RepaintBackground; //...for users
    procedure RemakeBackground; //...for users
  published
    property Anchors;
    property Align;
    property AlignTextHorizontal: TglHorAlign read FHorAlign write SetHorAlign
      default fhaCenter;
    property AlignTextVertical: TglVertAlign read FVertAlign write SetVertAlign
      default fvaCenter;
    property Transparent: boolean read FTransparent write SetTransparent
      default false;
    property BackgrColor: TColor read FBackgrColor write SetBackgrColor
      default clBlack;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Gradient3D: TJvg3DGradient read FThreeDGradient write
      FThreeDGradient;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset
      default 0;
    property ScalePercent: TSpPercent read FScalePercent write SetScalePercent
      default 5;
    property StepScaleFactor: single read FStepScaleFactor write
      SetStepScaleFactor;
    property ResultFont: TFont read FResultFont write SetResultFont;
    property TimerInterval: word read FTimerInterval write SetTimerInterval
      default 10;
    property Active: boolean read FActive write SetActive
      default false;
    property ClearOldText: boolean read FClearOldText write FClearOldText
      default true;
    property ClearOldTextWhileDrawing: boolean
      read FClearOldTextWhileDrawing write FClearOldTextWhileDrawing
      default true;
    property Cycled: boolean read FCycled write FCycled
      default false;
    property Text: TStringList read FText write SetText;
    property FastDraw: boolean read FFastDraw write SetFastDraw
      default true;
    property View3D: TJvg2DAlign read FView3D write FView3D;
    property Direction: TglScalingDir read FDirection write SetDirection
      default fsdRaising;
    property ShowTextWhilePassive: boolean
      read FShowTextWhilePassive write SetShowTextWhilePassive default true;
    property OnTextLineChanging: TTextLineChangingEvent
      read FOnTextLineChanging write FOnTextLineChanging;
    property Visible: boolean read FVisible write SetVisible
      default true;
    property StepShift: TJvgPointClass read FStepShift write FStepShift;
  end;

implementation

uses
  Math;

//*****************************************_____________LowLevel METHODS
//________________________________________________________

constructor TJvgFlyingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Visible := false;
  Width := 105;
  Height := 105;

  FTxtBitmap := TBitmap.create;
  FBackgrBitmap := TBitmap.create;
  FResBitmap := TBitmap.create;
  FScaledTxtBitmap := TBitmap.create;
  FPartTxtBitmap := TBitmap.create;
  FResultFont := TFont.Create;
  FTimer := TTimer.Create(Self);
  FGradient := TJvgGradient.Create;
  FThreeDGradient := TJvg3DGradient.Create;
  FText := TStringList.Create;
  FView3D := TJvg2DAlign.Create;
  FStepShift := TJvgPointClass.Create;
  uCurTextLine := 0;

  FThreeDGradient.FromColor := clBlack;
  FThreeDGradient.ToColor := clGray;

  FGradient.Active := true;
  FGradient.FromColor := clWhite;

  FGradient.OnChanged := OnParamsChanged;
  FThreeDGradient.OnChanged := OnParamsChanged;
  FView3D.OnChanged := OnParamsChanged;

  FTimer.Enabled := false;
  FTimer.OnTimer := OnTimerProc;
  FTimer.Interval := 10;

  //...defaults
  FHorAlign := fhaCenter;
  FVertAlign := fvaCenter;
  FTransparent := false;
  FBackgrColor := clBlack;
  FInteriorOffset := 0;
  FScalePercent := 5;
  FStepScaleFactor := 1.1;

  FResultFont.Color := clWhite;
  FResultFont.Name := 'Times';
  FResultFont.Style := [fsBold];
  FResultFont.size := 90;
  FResultFont.Color := clWhite;

  FTimerInterval := 10;
  FActive := false;
  FClearOldText := true;
  FClearOldTextWhileDrawing := true;
  FCycled := false;
  FFastDraw := true;
  FDirection := fsdRaising;
  FShowTextWhilePassive := true;
  FVisible := true;
  FStepShift.x := 0;
  FStepShift.y := 0;
  Shift := Point(0, 0);
  FText.Add('Hello');
  FText.Add('World');

  FScaledTxtBitmap.Canvas.Brush.Color := clBlack;
  FScaledTxtBitmap.Canvas.Brush.Style := bsSolid;
  FResBitmap.Canvas.Brush.Color := clBlack;
  FResBitmap.Canvas.Brush.Style := bsSolid;
  FPartTxtBitmap.Canvas.Brush.Color := clBlack;
  FPartTxtBitmap.Canvas.Brush.Style := bsSolid;

  fNeedRebuildBitmaps := true;
  fNeedRemakeBackground := false;
  fLoaded := false;
end;
//________________________________________________________

destructor TJvgFlyingText.Destroy;
begin
  FResBitmap.Free;
  FBackgrBitmap.Free;
  FTxtBitmap.Free;
  FScaledTxtBitmap.Free;
  FPartTxtBitmap.Free;
  FResultFont.Free;
  FGradient.Free;
  FThreeDGradient.Free;
  FText.Free;
  FView3D.Free;
  FStepShift.Free;
  inherited Destroy;
end;
//________________________________________________________

procedure TJvgFlyingText.Paint;
var
  r: TRect;
  //~~~~~~~~~~~~~~~~~LOCAL PROC

  procedure CalcPos(var FR: TMyRect; var FP: TPoint;
    FScaledWidth, FScaledHeight: integer; Shift: TPoint);
  begin
    case FHorAlign of
      fhaLeft:
        begin
          FR.Left := 0;
          FP.x := 0;
        end;
      fhaCenter:
        begin
          FR.Left := max(0, (Width - FScaledWidth) div 2);
          FP.x := max(0, -(Width - FScaledWidth) div 2);
        end;
      fhaRight:
        begin
          FR.Left := max(0, Width - FScaledWidth);
          FP.x := max(0, FScaledWidth - Width);
        end;
    end;
    case FVertAlign of
      fvaTop:
        begin
          FR.Top := 0;
          FP.y := 0;
        end;
      fvaCenter:
        begin
          FR.Top := max(0, (Height - FScaledHeight) div 2);
          FP.y := max(0, -(Height - FScaledHeight) div 2);
        end;
      fvaBottom:
        begin
          FR.Top := max(0, Height - FScaledHeight);
          FP.y := max(0, FScaledHeight - Height);
        end;
    end;
    FR.Left := FR.Left + Shift.x;
    FR.Top := FR.Top + Shift.y;
    FR.Width := min(Width, FScaledWidth);
    FR.Height := min(Height, FScaledHeight);
  end;
  //~~~~~~~~~~~~~~~~~END LOCAL PROC
begin
  //fNeedRebuildBitmaps := (OldLeft<>Left)or(OldTop<>Top);
  //OldLeft := Left; OldTop := Top;
  if fNeedRebuildBitmaps then
  begin
    fLoaded := true;
    fNeedRemakeBackground := true;
    BuildBitmaps;
    fNeedRebuildBitmaps := false;
    Visible := FVisible;
  end;

  StretchBlt(FScaledTxtBitmap.Canvas.Handle, 0, 0,
    trunc(FScaledWidth), trunc(FScaledHeight),
    FTxtBitmap.canvas.Handle, 0, 0,
    FTxtBitmap.Width, FTxtBitmap.Height, SRCCOPY);

  CalcPos(FR, FP, trunc(FScaledWidth), trunc(FScaledHeight), Shift);
  CalcPos(OldFR, OldFP, trunc(FOldScaledWidth),
    trunc(FOldScaledHeight), OldShift);

  if FFastDraw then
  begin
    if fNeedRepaintBackground then
    begin
      if FClearOldText or not FActive then
        BitBlt(Canvas.Handle, 0, 0, Width, Height,
          FBackgrBitmap.canvas.Handle, 0, 0, SRCCOPY);
    end;
    fNeedRepaintBackground := true;

    FPartTxtBitmap.Width := FR.Width;
    FPartTxtBitmap.Height := FR.Height;
    OldFR.Width := OldFR.Width + abs(Shift.x - OldShift.x);
    OldFR.Height := OldFR.Height + abs(Shift.y - OldShift.y);
    FResBitmap.Width := OldFR.Width;
    FResBitmap.Height := OldFR.Height;

    if FDirection = fsdRecessing then
    begin
      r := rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
      FillRect(FPartTxtBitmap.Canvas.Handle, r,
        FPartTxtBitmap.Canvas.Brush.Handle);
    end;

    BitBlt(FPartTxtBitmap.Canvas.Handle, 0, 0,
      FR.Width, FR.Height,
      FScaledTxtBitmap.canvas.Handle, FP.x, FP.y, SRCCOPY);
    {PartBackgr}
    if FClearOldTextWhileDrawing then
      BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
        FBackgrBitmap.canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY)
    else
      BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
        canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY);

    if FShowTextWhilePassive or FActive then
      CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap,
        ClientRect,
        (FR.Left - OldFR.Left), (FR.Top - OldFR.Top),
        fwoNone, fdsDefault, FTransparent, 0, clBlack);

    BitBlt(Canvas.Handle, OldFR.Left, OldFR.Top,
      FResBitmap.Width, FResBitmap.Height,
      FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
  end
  else {SLOW DRAWING}
  begin
    FPartTxtBitmap.Width := Width;
    FPartTxtBitmap.Height := Height;
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;

    BitBlt(FResBitmap.Canvas.Handle, 0, 0, Width, Height,
      FBackgrBitmap.canvas.Handle, 0, 0, SRCCOPY);
    if FDirection = fsdRecessing then
    begin
      r := rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
      FillRect(FPartTxtBitmap.Canvas.Handle, r,
        FPartTxtBitmap.Canvas.Brush.Handle);
    end;
    if FShowTextWhilePassive or FActive then
    begin
      BitBlt(FPartTxtBitmap.Canvas.Handle,
        FR.Left, FR.Top, FR.Width, FR.Height,
        FScaledTxtBitmap.canvas.Handle, FP.x, FP.y, SRCCOPY);

      CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap, ClientRect,
        0, 0, fwoNone, fdsDefault, FTransparent, 0, clBlack);
    end;
    BitBlt(Canvas.Handle, 0, 0, FResBitmap.Width, FResBitmap.Height,
      FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
  end;
  OldShift := Shift;
  Shift.x := Shift.x + FStepShift.x;
  Shift.y := Shift.y + FStepShift.y;

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, width, height);
    end;

end;
//________________________________________________________

procedure TJvgFlyingText.OnTimerProc(Sender: TObject);
begin
  if (not Assigned(FText)) or (FText.Count = 0) then
  begin
    Active := false;
    exit;
  end;

  if FDirection = fsdRaising then
  begin
    FScaledWidth := (FScaledWidth * FStepScaleFactor);
    FScaledHeight := (FScaledHeight * FStepScaleFactor);
    FOldScaledWidth := FScaledWidth;
    FOldScaledHeight := FScaledHeight;
  end
  else { fsdRecession }
  begin
    FOldScaledWidth := FScaledWidth;
    FOldScaledHeight := FScaledHeight;
    FScaledWidth := (FScaledWidth / FStepScaleFactor);
    FScaledHeight := (FScaledHeight / FStepScaleFactor);
  end;

  if (FScaledWidth < (FTxtBitmap.Width * FScalePercent / 100))
    or (FScaledWidth > FTxtBitmap.Width)
    or (FScaledHeight < (FTxtBitmap.Height * FScalePercent / 100))
    or (FScaledHeight > FTxtBitmap.Height) then
  begin
    if Assigned(FOnTextLineChanging) then
      FOnTextLineChanging(Self, uCurTextLine);
    if uCurTextLine < FText.Count - 1 then
      inc(uCurTextLine)
    else
    begin
      uCurTextLine := 0;
      if not FCycled then
      begin
        Active := false;
        BuildBitmaps;
        exit;
      end;
    end;
    BuildTxtBitmap;
  end
  else
    fNeedRepaintBackground := false;

  Repaint;
end;
//________________________________________________________

procedure TJvgFlyingText.RepaintBackground; //...for users
begin
  fNeedRepaintBackground := true;
  Repaint;
end;

procedure TJvgFlyingText.RemakeBackground; //...for users
begin
  fNeedRemakeBackground := true;
  Repaint;
end;
//________________________________________________________

procedure TJvgFlyingText.CalcTxtBitmapWidth;
var
  Size: TSize;
begin
  //if FText.Count <= uCurTextLine then exit;
  with FTxtBitmap do
  begin
    GetTextExtentPoint32(Canvas.handle, PChar(FText[uCurTextLine]),
      Length(FText[uCurTextLine]), Size);
    Width := Size.cx + FThreeDGradient.Depth;
    Height := Size.cy + FThreeDGradient.Depth;
    Shift := Point(0, 0);
    OldShift := Shift;
  end;
end;
//________________________________________________________

procedure TJvgFlyingText.BuildTxtBitmap;
var
  R: TRect;
  i, x, y: integer;
begin
  //if (not Assigned(FText))or(FText.Count=0) then exit;
  FTxtBitmap.Canvas.Font.Assign(FResultFont);
  if FText.Count <> 0 then
    CalcTxtBitmapWidth
  else
  begin
    FTxtBitmap.Width := 0;
    FTxtBitmap.Height := 0;
  end;
  R := rect(0, 0, FTxtBitmap.width, FTxtBitmap.height);

  FTxtBitmap.Canvas.Brush.Color := clBlack;
  FTxtBitmap.Canvas.Brush.Style := bsSolid;
  FTxtBitmap.Canvas.Fillrect(R);

  case FView3D.Horizontal of
    fhaLeft: x := 0;
    fhaCenter: x := FThreeDGradient.Depth div 2;
  else {fhaRight}
    x := FThreeDGradient.Depth;
  end;
  case FView3D.Vertical of
    fvaTop: y := 0;
    fvaCenter: y := FThreeDGradient.Depth div 2;
  else {fvaBottom}
    y := FThreeDGradient.Depth;
  end;

  SetBkMode(FTxtBitmap.Canvas.Handle, integer(TRANSPARENT));
  for i := 0 to FThreeDGradient.Depth - 1 do
  begin
    if FThreeDGradient.GType = fgtFlat then
      FThreeDGradient.TextOut(FTxtBitmap.Canvas.Handle, FText[uCurTextLine],
        r, x, y)
    else
    begin {fgt3D}
      FTxtBitmap.Canvas.Font.Color
        := FThreeDGradient.GetColorFromGradientLine(FThreeDGradient.Depth,
        i);
      FTxtBitmap.Canvas.TextOut(x, y, FText[uCurTextLine]);
    end;
    if x < (FThreeDGradient.Depth div 2) then
      inc(x)
    else
      dec(x);
    if y < (FThreeDGradient.Depth div 2) then
      inc(y)
    else
      dec(y);
  end;
  FGradient.TextOut(FTxtBitmap.Canvas.Handle, FText[uCurTextLine], r, x, y);

  {.calc scaling.}
  if FDirection = fsdRaising then
  begin
    FScaledWidth := (FTxtBitmap.Width * FScalePercent / 100);
    FScaledHeight := (FTxtBitmap.Height * FScalePercent / 100);
    FScaledHeight := FScaledHeight + 2;
  end
  else { fsdRecession }
  begin
    FScaledHeight := FTxtBitmap.Height;
    FScaledWidth := FTxtBitmap.Width;
  end;
  FOldScaledWidth := FScaledWidth;
  FOldScaledHeight := FScaledHeight;

  if FClearOldText then
  begin
    R := rect(0, 0, FPartTxtBitmap.width, FPartTxtBitmap.height);
    FPartTxtBitmap.Canvas.Fillrect(R);
  end;

  R := rect(0, 0, FTxtBitmap.width, FTxtBitmap.height);
  FScaledTxtBitmap.Width := FTxtBitmap.Width;
  FScaledTxtBitmap.Height := FTxtBitmap.Height;
  FScaledTxtBitmap.Canvas.Fillrect(R);

  fNeedRepaintBackground := true;
end;
//________________________________________________________

procedure TJvgFlyingText.BuildBitmaps;
var
  R: TRect;
  fOldTimer: boolean;
begin
  if not fLoaded then
  begin
    //    FPartTxtBitmap.Width:=Width; FPartTxtBitmap.Height:=Height;
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;
    exit;
  end;

  fOldTimer := FTimer.Enabled;
  FTimer.Enabled := false;
  if fNeedRemakeBackground then
  begin
    //________________________________PrepareBackground____begin
//    BringParentWindowToTop(Self);
    FBackgrBitmap.Width := Width;
    FBackgrBitmap.Height := Height;

    //..prepare tabula rasa
    if FTransparent then
      FBackgrBitmap.Canvas.Brush.Color := Parent.Brush.Color
    else
      FBackgrBitmap.Canvas.Brush.Color := FBackgrColor;
    FBackgrBitmap.Canvas.Brush.Style := bsSolid;
    FBackgrBitmap.Canvas.FillRect(ClientRect);

    if FTransparent then
      GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
        FBackgrBitmap.Canvas.Handle);
    //________________________________PrepareBackground____end
  //    if (FResBitmap.Width or FResBitmap.Height)<>0 then
  //      BitBlt( FBackgrBitmap.Canvas.Handle, 0, 0,
  //        Width, Height, canvas.Handle, 0, 0, SRCCOPY);
  end;

  //  if FTransparent then ShowWindow(Handle,SW_SHOW);

  BuildTxtBitmap;

  R := rect(0, 0, FResBitmap.width, FResBitmap.height);
  FResBitmap.Canvas.Fillrect(R);
  FTimer.Enabled := fOldTimer;
  fNeedRemakeBackground := false;
end;
//________________________________________________________

procedure TJvgFlyingText.WMSize(var Message: TWMSize);
begin
  fNeedRemakeBackground := true;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.OnParamsChanged(Sender: TObject);
begin
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.Repaint_;
//var R:TRect;
begin
  if not fLoaded then
    exit;
  Repaint;
  //  R:=Rect(0,0,Width,Height);
  //  InvalidateRect(Handle,@R,false);
end;
//________________________________________________________
//*****************************************_____________PROPERTY METHODS
//________________________________________________________

procedure TJvgFlyingText.SetHorAlign(Value: TglHorAlign);
begin
  FHorAlign := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetVertAlign(Value: TglVertAlign);
begin
  FVertAlign := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetTransparent(Value: boolean);
begin
  FTransparent := Value;
  fNeedRemakeBackground := true;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetBackgrColor(Value: TColor);
begin
  FBackgrColor := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetInteriorOffset(Value: word);
begin
  FInteriorOffset := Value;
end;
//________________________________________________________

procedure TJvgFlyingText.SetScalePercent(Value: TSpPercent);
begin
  FScalePercent := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetStepScaleFactor(Value: Single);
begin
  if Value < 1 then
    Value := 1;
  if Value > 2 then
    Value := 2;
  FStepScaleFactor := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetResultFont(Value: TFont);
begin
  if Value <> nil then
    FResultFont.Assign(Value);
  BuildBitmaps;
end;
//________________________________________________________

procedure TJvgFlyingText.SetTimerInterval(Value: word);
begin
  FTimerInterval := Value;
  FTimer.Interval := FTimerInterval;
end;
//________________________________________________________

procedure TJvgFlyingText.SetActive(Value: boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FActive;
end;
//________________________________________________________

procedure TJvgFlyingText.SetText(Value: TStringList);
var
  OldActive: boolean;
  i: integer;
begin
  OldActive := FActive;
  Active := false;
  if Assigned(Value) then
    FText.Assign(Value);
  uCurTextLine := 0;
  if FText.Count <> 0 then
    for i := 0 to FText.Count - 1 do
      FText[i] := Trim(FText[i]);
  BuildBitmaps;
  Repaint_;
  Active := OldActive;
end;
//________________________________________________________

procedure TJvgFlyingText.SetFastDraw(Value: boolean);
begin
  if FFastDraw = Value then
    exit;
  FFastDraw := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetDirection(Value: TglScalingDir);
begin
  if FDirection = Value then
    exit;
  FDirection := Value;
  if csDesigning in ComponentState then
  begin
    BuildBitmaps;
    Repaint_;
  end;
end;
//________________________________________________________

procedure TJvgFlyingText.SetShowTextWhilePassive(Value: boolean);
begin
  if FShowTextWhilePassive = Value then
    exit;
  FShowTextWhilePassive := Value;
  BuildBitmaps;
  Repaint_;
end;
//________________________________________________________

procedure TJvgFlyingText.SetVisible(Value: boolean);
begin
  if FVisible = Value then
    exit;
  FVisible := Value;
  inherited Visible := FVisible;
  if FVisible then
  begin
    BuildBitmaps;
    Repaint_;
  end;
end;
//________________________________________________________

end.

