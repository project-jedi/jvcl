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
  Dialogs, ExtCtrls,
  JvgTypes, JvgUtils, JvComponent, JvgCommClasses;

type
  TMyRect = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TTextLineChangingEvent = procedure(Sender: TObject; LineNum: Integer) of object;

  TJvgFlyingText = class(TJvCustomPanel)
  private
    FHorAlign: TglHorAlign;
    FVertAlign: TglVertAlign;
    FTransparent: Boolean;
    FBackgrColor: TColor;
    FGradient: TJvgGradient;
    FThreeDGradient: TJvg3DGradient;
    FInteriorOffset: Word;
    FScalePercent: TSpPercent;
    FStepScaleFactor: Single;
    FResultFont: TFont;
    FTimerInterval: Word;
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

    FR: TMyRect;
    OldFR: TMyRect;
    FP: TPoint;
    OldFP: TPoint;
    FBackgrBitmap: TBitmap;
    FTxtBitmap: TBitmap;
    FResBitmap: TBitmap;
    FScaledTxtBitmap: TBitmap;
    FPartTxtBitmap: TBitmap;
    FNeedRebuildBitmaps: Boolean;
    FTimer: TTimer;
    FScaledWidth: Single;
    FScaledHeight: Single;
    FOldScaledWidth: Single;
    FOldScaledHeight: Single;
    FVisible: Boolean;
    FStepShift: TJvgPointClass;

    FShift: TPoint;
    FOldShift: TPoint;
    FCurTextLine: Word;
    FNeedRepaintBackground: Boolean;
    FNeedRemakeBackground: Boolean;
    FLoaded: Boolean;
    procedure CalcTxtBitmapWidth;
    procedure BuildBitmaps;
    procedure BuildTxtBitmap;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure OnParamsChanged(Sender: TObject);
    procedure Repaint_;

    function GetText: TStrings;
    procedure SetHorAlign(Value: TglHorAlign);
    procedure SetVertAlign(Value: TglVertAlign);
    procedure SetTransparent(Value: Boolean);
    procedure SetBackgrColor(Value: TColor);
    procedure SetInteriorOffset(Value: Word);
    procedure SetScalePercent(Value: TSpPercent);
    procedure SetStepScaleFactor(Value: Single);
    procedure SetResultFont(Value: TFont);
    procedure SetTimerInterval(Value: Word);
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
    property InteriorOffset: Word read FInteriorOffset write SetInteriorOffset default 0;
    property ScalePercent: TSpPercent read FScalePercent write SetScalePercent default 5;
    property StepScaleFactor: Single read FStepScaleFactor write SetStepScaleFactor;
    property ResultFont: TFont read FResultFont write SetResultFont;
    property TimerInterval: Word read FTimerInterval write SetTimerInterval default 10;
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

  FTxtBitmap := TBitmap.Create;
  FBackgrBitmap := TBitmap.Create;
  FResBitmap := TBitmap.Create;
  FScaledTxtBitmap := TBitmap.Create;
  FPartTxtBitmap := TBitmap.Create;
  FResultFont := TFont.Create;
  FTimer := TTimer.Create(Self);
  FGradient := TJvgGradient.Create;
  FThreeDGradient := TJvg3DGradient.Create;
  FText := TStringList.Create;
  FView3D := TJvg2DAlign.Create;
  FStepShift := TJvgPointClass.Create;
  FCurTextLine := 0;

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
  FResultFont.Size := 90;
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
  FStepShift.X := 0;
  FStepShift.Y := 0;
  FShift := Point(0, 0);
  FText.Add('Hello');
  FText.Add('World');

  FScaledTxtBitmap.Canvas.Brush.Color := clBlack;
  FScaledTxtBitmap.Canvas.Brush.Style := bsSolid;
  FResBitmap.Canvas.Brush.Color := clBlack;
  FResBitmap.Canvas.Brush.Style := bsSolid;
  FPartTxtBitmap.Canvas.Brush.Color := clBlack;
  FPartTxtBitmap.Canvas.Brush.Style := bsSolid;

  FNeedRebuildBitmaps := True;
  FNeedRemakeBackground := False;
  FLoaded := False;
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
  R: TRect;

  procedure CalcPos(var FR: TMyRect; var FP: TPoint;
    FScaledWidth, FScaledHeight: Integer; Shift: TPoint);
  begin
    case FHorAlign of
      fhaLeft:
        begin
          FR.Left := 0;
          FP.X := 0;
        end;
      fhaCenter:
        begin
          FR.Left := Max(0, (Width - FScaledWidth) div 2);
          FP.X := Max(0, -(Width - FScaledWidth) div 2);
        end;
      fhaRight:
        begin
          FR.Left := Max(0, Width - FScaledWidth);
          FP.X := Max(0, FScaledWidth - Width);
        end;
    end;
    case FVertAlign of
      fvaTop:
        begin
          FR.Top := 0;
          FP.Y := 0;
        end;
      fvaCenter:
        begin
          FR.Top := Max(0, (Height - FScaledHeight) div 2);
          FP.Y := Max(0, -(Height - FScaledHeight) div 2);
        end;
      fvaBottom:
        begin
          FR.Top := Max(0, Height - FScaledHeight);
          FP.Y := Max(0, FScaledHeight - Height);
        end;
    end;
    FR.Left := FR.Left + Shift.X;
    FR.Top := FR.Top + Shift.Y;
    FR.Width := Min(Width, FScaledWidth);
    FR.Height := Min(Height, FScaledHeight);
  end;

begin
  //FNeedRebuildBitmaps := (OldLeft<>Left)or(OldTop<>Top);
  //OldLeft := Left; OldTop := Top;
  if FNeedRebuildBitmaps then
  begin
    FLoaded := True;
    FNeedRemakeBackground := True;
    BuildBitmaps;
    FNeedRebuildBitmaps := False;
    Visible := FVisible;
  end;

  StretchBlt(FScaledTxtBitmap.Canvas.Handle, 0, 0,
    Trunc(FScaledWidth), Trunc(FScaledHeight),
    FTxtBitmap.Canvas.Handle, 0, 0,
    FTxtBitmap.Width, FTxtBitmap.Height, SRCCOPY);

  CalcPos(FR, FP, Trunc(FScaledWidth), Trunc(FScaledHeight), FShift);
  CalcPos(OldFR, OldFP, Trunc(FOldScaledWidth),
    Trunc(FOldScaledHeight), FOldShift);

  if FFastDraw then
  begin
    if FNeedRepaintBackground then
    begin
      if FClearOldText or not FActive then
        BitBlt(Canvas.Handle, 0, 0, Width, Height,
          FBackgrBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end;
    FNeedRepaintBackground := True;

    FPartTxtBitmap.Width := FR.Width;
    FPartTxtBitmap.Height := FR.Height;
    OldFR.Width := OldFR.Width + Abs(FShift.X - FOldShift.X);
    OldFR.Height := OldFR.Height + Abs(FShift.Y - FOldShift.Y);
    FResBitmap.Width := OldFR.Width;
    FResBitmap.Height := OldFR.Height;

    if FDirection = fsdRecessing then
    begin
      R := Rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
      FillRect(FPartTxtBitmap.Canvas.Handle, R,
        FPartTxtBitmap.Canvas.Brush.Handle);
    end;

    BitBlt(FPartTxtBitmap.Canvas.Handle, 0, 0,
      FR.Width, FR.Height,
      FScaledTxtBitmap.Canvas.Handle, FP.X, FP.Y, SRCCOPY);
    {PartBackgr}
    if FClearOldTextWhileDrawing then
      BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
        FBackgrBitmap.Canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY)
    else
      BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
        Canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY);

    if FShowTextWhilePassive or FActive then
      CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap,
        ClientRect,
        (FR.Left - OldFR.Left), (FR.Top - OldFR.Top),
        fwoNone, fdsDefault, FTransparent, 0, clBlack);

    BitBlt(Canvas.Handle, OldFR.Left, OldFR.Top,
      FResBitmap.Width, FResBitmap.Height,
      FResBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end
  else {SLOW DRAWING}
  begin
    FPartTxtBitmap.Width := Width;
    FPartTxtBitmap.Height := Height;
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;

    BitBlt(FResBitmap.Canvas.Handle, 0, 0, Width, Height,
      FBackgrBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    if FDirection = fsdRecessing then
    begin
      R := Rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
      FillRect(FPartTxtBitmap.Canvas.Handle, R,
        FPartTxtBitmap.Canvas.Brush.Handle);
    end;
    if FShowTextWhilePassive or FActive then
    begin
      BitBlt(FPartTxtBitmap.Canvas.Handle,
        FR.Left, FR.Top, FR.Width, FR.Height,
        FScaledTxtBitmap.Canvas.Handle, FP.X, FP.Y, SRCCOPY);

      CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap, ClientRect,
        0, 0, fwoNone, fdsDefault, FTransparent, 0, clBlack);
    end;
    BitBlt(Canvas.Handle, 0, 0, FResBitmap.Width, FResBitmap.Height,
      FResBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end;
  FOldShift := FShift;
  FShift.X := FShift.X + FStepShift.X;
  FShift.Y := FShift.Y + FStepShift.Y;

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
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
      FOnTextLineChanging(Self, FCurTextLine);
    if FCurTextLine < Text.Count - 1 then
      Inc(FCurTextLine)
    else
    begin
      FCurTextLine := 0;
      if not FCycled then
      begin
        Active := False;
        BuildBitmaps;
        Exit;
      end;
    end;
    BuildTxtBitmap;
  end
  else
    FNeedRepaintBackground := False;

  Repaint;
end;

procedure TJvgFlyingText.RepaintBackground; //...for users
begin
  FNeedRepaintBackground := True;
  Repaint;
end;

procedure TJvgFlyingText.RemakeBackground; //...for users
begin
  FNeedRemakeBackground := True;
  Repaint;
end;

procedure TJvgFlyingText.CalcTxtBitmapWidth;
var
  Size: TSize;
begin
  //if Text.Count <= FCurTextLine then Exit;
  with FTxtBitmap do
  begin
    GetTextExtentPoint32(Canvas.Handle, PChar(Text[FCurTextLine]),
      Length(Text[FCurTextLine]), Size);
    Width := Size.cx + FThreeDGradient.Depth;
    Height := Size.cy + FThreeDGradient.Depth;
    FShift := Point(0, 0);
    FOldShift := FShift;
  end;
end;

procedure TJvgFlyingText.BuildTxtBitmap;
var
  R: TRect;
  I, X, Y: Integer;
begin
  //if (not Assigned(Text))or(FText.Count=0) then Exit;
  FTxtBitmap.Canvas.Font.Assign(FResultFont);
  if Text.Count <> 0 then
    CalcTxtBitmapWidth
  else
  begin
    FTxtBitmap.Width := 0;
    FTxtBitmap.Height := 0;
  end;
  R := Rect(0, 0, FTxtBitmap.Width, FTxtBitmap.Height);

  FTxtBitmap.Canvas.Brush.Color := clBlack;
  FTxtBitmap.Canvas.Brush.Style := bsSolid;
  FTxtBitmap.Canvas.FillRect(R);

  case FView3D.Horizontal of
    fhaLeft:
      X := 0;
    fhaCenter:
      X := FThreeDGradient.Depth div 2;
  else {fhaRight}
    X := FThreeDGradient.Depth;
  end;
  case FView3D.Vertical of
    fvaTop:
      Y := 0;
    fvaCenter:
      Y := FThreeDGradient.Depth div 2;
  else {fvaBottom}
    Y := FThreeDGradient.Depth;
  end;

  SetBkMode(FTxtBitmap.Canvas.Handle, Integer(TRANSPARENT));
  for I := 0 to FThreeDGradient.Depth - 1 do
  begin
    if FThreeDGradient.GType = fgtFlat then
      FThreeDGradient.TextOut(FTxtBitmap.Canvas.Handle, Text[FCurTextLine],
        R, X, Y)
    else
    begin {fgt3D}
      FTxtBitmap.Canvas.Font.Color
        := FThreeDGradient.GetColorFromGradientLine(FThreeDGradient.Depth,
        I);
      FTxtBitmap.Canvas.TextOut(X, Y, Text[FCurTextLine]);
    end;
    if X < (FThreeDGradient.Depth div 2) then
      Inc(X)
    else
      Dec(X);
    if Y < (FThreeDGradient.Depth div 2) then
      Inc(Y)
    else
      Dec(Y);
  end;
  FGradient.TextOut(FTxtBitmap.Canvas.Handle, Text[FCurTextLine], R, X, Y);

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
    R := Rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
    FPartTxtBitmap.Canvas.FillRect(R);
  end;

  R := Rect(0, 0, FTxtBitmap.Width, FTxtBitmap.Height);
  FScaledTxtBitmap.Width := FTxtBitmap.Width;
  FScaledTxtBitmap.Height := FTxtBitmap.Height;
  FScaledTxtBitmap.Canvas.FillRect(R);

  FNeedRepaintBackground := True;
end;

procedure TJvgFlyingText.BuildBitmaps;
var
  R: TRect;
  FOldTimer: Boolean;
begin
  if not FLoaded then
  begin
    //    FPartTxtBitmap.Width:=Width; FPartTxtBitmap.Height:=Height;
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;
    Exit;
  end;

  FOldTimer := FTimer.Enabled;
  FTimer.Enabled := False;
  if FNeedRemakeBackground then
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
  //        Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
  end;

  //  if FTransparent then ShowWindow(Handle,SW_SHOW);

  BuildTxtBitmap;

  R := Rect(0, 0, FResBitmap.Width, FResBitmap.Height);
  FResBitmap.Canvas.FillRect(R);
  FTimer.Enabled := FOldTimer;
  FNeedRemakeBackground := False;
end;

procedure TJvgFlyingText.WMSize(var Msg: TWMSize);
begin
  FNeedRemakeBackground := True;
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
  FNeedRemakeBackground := True;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetBackgrColor(Value: TColor);
begin
  FBackgrColor := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetInteriorOffset(Value: Word);
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

procedure TJvgFlyingText.SetTimerInterval(Value: Word);
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
  I: Integer;
begin
  OldActive := FActive;
  Active := False;
  if Assigned(Value) then
    FText.Assign(Value);
  FCurTextLine := 0;
  if FText.Count <> 0 then
    for I := 0 to FText.Count - 1 do
      FText[I] := Trim(FText[I]);
  BuildBitmaps;
  Repaint_;
  Active := OldActive;
end;

procedure TJvgFlyingText.SetFastDraw(Value: Boolean);
begin
  if FFastDraw = Value then
    Exit;
  FFastDraw := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetDirection(Value: TglScalingDir);
begin
  if FDirection = Value then
    Exit;
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
    Exit;
  FShowTextWhilePassive := Value;
  BuildBitmaps;
  Repaint_;
end;

procedure TJvgFlyingText.SetVisible(Value: Boolean);
begin
  if FVisible = Value then
    Exit;
  FVisible := Value;
  inherited Visible := FVisible;
  if FVisible then
  begin
    BuildBitmaps;
    Repaint_;
  end;
end;

end.

