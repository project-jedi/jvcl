{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFlyingText.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
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
    Left, Top, Width, Height: Integer;
  end;

  TTextLineChangingEvent = procedure(Sender: TObject; LineNum: Integer) of
    object;

  TJvgFlyingText = class(TJvCustomPanel)
  private
    FHorAlign: TglHorAlign;
    FVertAlign: TglVertAlign;
    FTransparent: Boolean;
    FBackgrColor: TColor;
    FGradient: TJvgGradient;
    FThreeDGradient: TJvg3DGradient;
    FInteriorOffset: word;
    FScalePercent: TSpPercent;
    FStepScaleFactor: single;
    FResultFont: TFont;
    FTimerInterval: word;
    FActive: Boolean;
    FClearOldText: Boolean;
    FClearOldTextWhileDrawing: Boolean;
    FCycled: Boolean;
    FText: TStringList;
    FView3D: TJvg2DAlign;
    FDirection: TglScalingDir;
    FFastDraw: Boolean;
    FShowTextWhilePassive: Boolean;

    FOnTextLineChanging: TTextLineChangingEvent;

    FR, OldFR: TMyRect;
    FP, OldFP: TPoint;
    FBackgrBitmap,
      FTxtBitmap,
      FResBitmap,
      FScaledTxtBitmap,
      FPartTxtBitmap: TBitmap;
    fNeedRebuildBitmaps: Boolean;
    FTimer: TTimer;
    FScaledWidth,
      FScaledHeight,
      FOldScaledWidth,
      FOldScaledHeight: single;
    FVisible: Boolean;
    FStepShift: TJvgPointClass;

    Shift, OldShift: TPoint;
    uCurTextLine: Word;
    fNeedRepaintBackground: Boolean;
    fNeedRemakeBackground: Boolean;
    fLoaded: Boolean;
    procedure CalcTxtBitmapWidth;
    procedure BuildBitmaps;
    procedure BuildTxtBitmap;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure OnParamsChanged(Sender: TObject);
    procedure Repaint_;

    function GetText: TStrings;
    procedure SetHorAlign(Value: TglHorAlign);
    procedure SetVertAlign(Value: TglVertAlign);
    procedure SetTransparent(Value: Boolean);
    procedure SetBackgrColor(Value: TColor);
    procedure SetInteriorOffset(Value: word);
    procedure SetScalePercent(Value: TSpPercent);
    procedure SetStepScaleFactor(Value: Single);
    procedure SetResultFont(Value: TFont);
    procedure SetTimerInterval(Value: word);
    procedure SetActive(Value: Boolean);
    procedure SetText(Value: TStrings);
    procedure SetFastDraw(Value: Boolean);
    procedure SetDirection(Value: TglScalingDir);
    procedure SetShowTextWhilePassive(Value: Boolean);
    procedure SetVisible(Value: Boolean);

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
    property AlignTextHorizontal: TglHorAlign read FHorAlign write SetHorAlign default fhaCenter;
    property AlignTextVertical: TglVertAlign read FVertAlign write SetVertAlign default fvaCenter;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property BackgrColor: TColor read FBackgrColor write SetBackgrColor default clBlack;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Gradient3D: TJvg3DGradient read FThreeDGradient write FThreeDGradient;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset default 0;
    property ScalePercent: TSpPercent read FScalePercent write SetScalePercent default 5;
    property StepScaleFactor: single read FStepScaleFactor write SetStepScaleFactor;
    property ResultFont: TFont read FResultFont write SetResultFont;
    property TimerInterval: word read FTimerInterval write SetTimerInterval default 10;
    property Active: Boolean read FActive write SetActive default False;
    property ClearOldText: Boolean read FClearOldText write FClearOldText default True;
    property ClearOldTextWhileDrawing: Boolean
      read FClearOldTextWhileDrawing write FClearOldTextWhileDrawing
      default True;
    property Cycled: Boolean read FCycled write FCycled default False;
    property Text: TStrings read GetText write SetText;
    property FastDraw: Boolean read FFastDraw write SetFastDraw default True;
    property View3D: TJvg2DAlign read FView3D write FView3D;
    property Direction: TglScalingDir read FDirection write SetDirection default fsdRaising;
    property ShowTextWhilePassive: Boolean
      read FShowTextWhilePassive write SetShowTextWhilePassive default True;
    property OnTextLineChanging: TTextLineChangingEvent
      read FOnTextLineChanging write FOnTextLineChanging;
    property Visible: Boolean read FVisible write SetVisible default True;
    property StepShift: TJvgPointClass read FStepShift write FStepShift;
  end;

implementation

uses
  Math;

constructor TJvgFlyingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Visible := False;
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

  FGradient.Active := True;
  FGradient.FromColor := clWhite;

  FGradient.OnChanged := OnParamsChanged;
  FThreeDGradient.OnChanged := OnParamsChanged;
  FView3D.OnChanged := OnParamsChanged;

  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimerProc;
  FTimer.Interval := 10;

  //...defaults
  FHorAlign := fhaCenter;
  FVertAlign := fvaCenter;
  FTransparent := False;
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
  FActive := False;
  FClearOldText := True;
  FClearOldTextWhileDrawing := True;
  FCycled := False;
  FFastDraw := True;
  FDirection := fsdRaising;
  FShowTextWhilePassive := True;
  FVisible := True;
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

  fNeedRebuildBitmaps := True;
  fNeedRemakeBackground := False;
  fLoaded := False;
end;

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

procedure TJvgFlyingText.Paint;
var
  r: TRect;

  procedure CalcPos(var FR: TMyRect; var FP: TPoint;
    FScaledWidth, FScaledHeight: Integer; Shift: TPoint);
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
    fLoaded := True;
    fNeedRemakeBackground := True;
    BuildBitmaps;
    fNeedRebuildBitmaps := False;
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
    fNeedRepaintBackground := True;

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

procedure TJvgFlyingText.OnTimerProc(Sender: TObject);
begin
  if FText.Count = 0 then
  begin
    Active := False;
    Exit;
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
    if uCurTextLine < Text.Count - 1 then
      inc(uCurTextLine)
    else
    begin
      uCurTextLine := 0;
      if not FCycled then
      begin
        Active := False;
        BuildBitmaps;
        exit;
      end;
    end;
    BuildTxtBitmap;
  end
  else
    fNeedRepaintBackground := False;

  Repaint;
end;

procedure TJvgFlyingText.RepaintBackground; //...for users
begin
  fNeedRepaintBackground := True;
  Repaint;
end;

procedure TJvgFlyingText.RemakeBackground; //...for users
begin
  fNeedRemakeBackground := True;
  Repaint;
end;

procedure TJvgFlyingText.CalcTxtBitmapWidth;
var
  Size: TSize;
begin
  //if Text.Count <= uCurTextLine then exit;
  with FTxtBitmap do
  begin
    GetTextExtentPoint32(Canvas.handle, PChar(Text[uCurTextLine]),
      Length(Text[uCurTextLine]), Size);
    Width := Size.cx + FThreeDGradient.Depth;
    Height := Size.cy + FThreeDGradient.Depth;
    Shift := Point(0, 0);
    OldShift := Shift;
  end;
end;

procedure TJvgFlyingText.BuildTxtBitmap;
var
  R: TRect;
  i, x, y: Integer;
begin
  //if (not Assigned(Text))or(FText.Count=0) then exit;
  FTxtBitmap.Canvas.Font.Assign(FResultFont);
  if Text.Count <> 0 then
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

  SetBkMode(FTxtBitmap.Canvas.Handle, Integer(TRANSPARENT));
  for i := 0 to FThreeDGradient.Depth - 1 do
  begin
    if FThreeDGradient.GType = fgtFlat then
      FThreeDGradient.TextOut(FTxtBitmap.Canvas.Handle, Text[uCurTextLine],
        r, x, y)
    else
    begin {fgt3D}
      FTxtBitmap.Canvas.Font.Color
        := FThreeDGradient.GetColorFromGradientLine(FThreeDGradient.Depth,
        i);
      FTxtBitmap.Canvas.TextOut(x, y, Text[uCurTextLine]);
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
  FGradient.TextOut(FTxtBitmap.Canvas.Handle, Text[uCurTextLine], r, x, y);

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

  fNeedRepaintBackground := True;
end;

procedure TJvgFlyingText.BuildBitmaps;
var
  R: TRect;
  fOldTimer: Boolean;
begin
  if not fLoaded then
  begin
    //    FPartTxtBitmap.Width:=Width; FPartTxtBitmap.Height:=Height;
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;
    exit;
  end;

  fOldTimer := FTimer.Enabled;
  FTimer.Enabled := False;
  if fNeedRemakeBackground then
  begin
    // PrepareBackground
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
    // PrepareBackground
  //    if (FResBitmap.Width or FResBitmap.Height)<>0 then
  //      BitBlt( FBackgrBitmap.Canvas.Handle, 0, 0,
  //        Width, Height, canvas.Handle, 0, 0, SRCCOPY);
  end;

  //  if FTransparent then ShowWindow(Handle,SW_SHOW);

  BuildTxtBitmap;

  R := rect(0, 0, FResBitmap.width, FResBitmap.height);
  FResBitmap.Canvas.Fillrect(R);
  FTimer.Enabled := fOldTimer;
  fNeedRemakeBackground := False;
end;

procedure TJvgFlyingText.WMSize(var Message: TWMSize);
begin
  fNeedRemakeBackground := True;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.OnParamsChanged(Sender: TObject);
begin
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.Repaint_;
//var R: TRect;
begin
  if FLoaded then
    Repaint;
  //  R:=Rect(0,0,Width,Height);
  //  InvalidateRect(Handle,@R,False);
end;

procedure TJvgFlyingText.SetHorAlign(Value: TglHorAlign);
begin
  FHorAlign := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetVertAlign(Value: TglVertAlign);
begin
  FVertAlign := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  fNeedRemakeBackground := True;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetBackgrColor(Value: TColor);
begin
  FBackgrColor := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetInteriorOffset(Value: word);
begin
  FInteriorOffset := Value;
end;

procedure TJvgFlyingText.SetScalePercent(Value: TSpPercent);
begin
  FScalePercent := Value;
  BuildBitmaps;
  Repaint_;
end;

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

procedure TJvgFlyingText.SetResultFont(Value: TFont);
begin
  if Value <> nil then
    FResultFont.Assign(Value);
  BuildBitmaps;
end;

procedure TJvgFlyingText.SetTimerInterval(Value: word);
begin
  FTimerInterval := Value;
  FTimer.Interval := FTimerInterval;
end;

procedure TJvgFlyingText.SetActive(Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FActive;
end;

function TJvgFlyingText.GetText: TStrings;
begin
  Result := FText;
end;

procedure TJvgFlyingText.SetText(Value: TStrings);
var
  OldActive: Boolean;
  i: Integer;
begin
  OldActive := FActive;
  Active := False;
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

procedure TJvgFlyingText.SetFastDraw(Value: Boolean);
begin
  if FFastDraw = Value then
    exit;
  FFastDraw := Value;
  BuildBitmaps;
  Repaint_;
end;

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

procedure TJvgFlyingText.SetShowTextWhilePassive(Value: Boolean);
begin
  if FShowTextWhilePassive = Value then
    exit;
  FShowTextWhilePassive := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetVisible(Value: Boolean);
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

end.

