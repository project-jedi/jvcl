{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDigits.PAS, released on 2003-01-15.

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

unit JvgDigits;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls,
  JvgTypes, JvComponent, JvgUtils, JvgCommClasses;

const
  JvDefaultPassiveColor = TColor($00202020);

type
  TJvgSpecialSymbol = (ssyNone, ssyColon, ssySlash, ssyBackslash);

  TJvgDigits = class(TJvGraphicControl)
  private
    FValue: Double;
    FDSize: TJvgPointClass;
    FActiveColor: TColor;
    FPassiveColor: TColor;
    FBackgroundColor: TColor;
    FInsertSpecialSymbolAt: Integer;
    FPositions: Word;
    FPenWidth: Word;
    FGap: Word;
    FInterspace: Word;
    FTransparent: Boolean;
    FAlignment: TAlignment;
    FInteriorOffset: Word;
    FPenStyle: TPenStyle;
    FSpecialSymbol: TJvgSpecialSymbol;
    FBevel: TJvgExtBevelOptions;
    FGradient: TJvgGradient;
    FDigitCount: Integer;
    FOldStrWidth: Integer;
    FOldDotPos: Integer;
    FNeedBackgroundPaint: Boolean;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetValue(NewValue: Double);
    procedure SetActiveColor(Value: TColor);
    procedure SetPassiveColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetPositions(Value: Word);
    procedure SetPenWidth(Value: Word);
    procedure SetInterspace(Value: Word);
    procedure SetGap(Value: Word);
    procedure SetTransparent(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetInteriorOffset(Value: Word);
    procedure SetInsertSpecialSymbolAt(Value: Integer);
    procedure SetPenStyle(Value: TPenStyle);
    procedure SetSpecialSymbol(Value: TJvgSpecialSymbol);
    procedure SetDigitCount(Value: Integer);
    procedure SmthChanged(Sender: TObject);
  public
    procedure Paint; override;
    procedure PaintTo(Canvas: TCanvas);
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Value: Double read FValue write SetValue;
    property DigitSize: TJvgPointClass read FDSize write FDSize;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clWhite;
    property PassiveColor: TColor read FPassiveColor write SetPassiveColor default JvDefaultPassiveColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBlack;
    property Positions: Word read FPositions write SetPositions default 0;
    property PenWidth: Word read FPenWidth write SetPenWidth default 1;
    property Gap: Word read FGap write SetGap default 1;
    property Interspace: Word read FInterspace write SetInterspace default 3;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property InteriorOffset: Word read FInteriorOffset write SetInteriorOffset default 0;
    property InsertSpecialSymbolAt: Integer read FInsertSpecialSymbolAt write SetInsertSpecialSymbolAt default -1;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle default psSolid;
    property SpecialSymbol: TJvgSpecialSymbol read FSpecialSymbol write SetSpecialSymbol default ssyNone;
    property Bevel: TJvgExtBevelOptions read FBevel write FBevel;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property DigitCount: Integer read FDigitCount write SetDigitCount default -1;
    property Width default 160;
    property Height default 28;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

  TJvgGraphDigitsElem =
    (dlT, dlC, dlB, dlTL, dlTR, dlBL, dlBR, dlDOT);
  TJvgGraphDigitsElemSet = set of TJvgGraphDigitsElem;

const
  DigitsSet: array [0..11] of TJvgGraphDigitsElemSet =
   (
    [dlT, dlB, dlTL, dlTR, dlBL, dlBR],      // 0
    [dlTR, dlBR],                            // 1
    [dlT, dlC, dlB, dlTR, dlBL],             // 2
    [dlT, dlC, dlB, dlTR, dlBR],             // 3
    [dlC, dlTL, dlTR, dlBR],                 // 4
    [dlT, dlC, dlB, dlTL, dlBR],             // 5
    [dlT, dlC, dlB, dlTL, dlBL, dlBR],       // 6
    [dlT, dlTR, dlBR],                       // 7
    [dlT, dlC, dlB, dlTL, dlTR, dlBL, dlBR], // 8
    [dlT, dlC, dlB, dlTL, dlTR, dlBR],       // 9
    [],                                      // ' '
    [dlDOT]                                  // ','
   );

implementation

constructor TJvgDigits.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDSize := TJvgPointClass.Create;
  //...set defaults
  Width := 160;
  Height := 28;
  //  if csDesigning in ComponentState then FValue:=1.1234567890;
  FDSize.X := 10;
  FDSize.Y := 21;
  FDSize.OnChanged := SmthChanged;
  FActiveColor := clWhite;
  FPassiveColor := JvDefaultPassiveColor;
  FBackgroundColor := clBlack;
  FPositions := 0;
  FPenWidth := 1;
  FGap := 1;
  FInterspace := 3;
  FTransparent := False;
  FInteriorOffset := 0;
  FInsertSpecialSymbolAt := -1;
  FDigitCount := -1;
  FPenStyle := psSolid;
  FNeedBackgroundPaint := True;
  Color := FBackgroundColor;
  FAlignment := taCenter;
  FSpecialSymbol := ssyNone;
  FBevel := TJvgExtBevelOptions.Create;
  FBevel.OnChanged := SmthChanged;
  FGradient := TJvgGradient.Create;
  FGradient.OnChanged := SmthChanged;
end;

destructor TJvgDigits.Destroy;
begin
  FDSize.Free;
  FBevel.Free;
  FGradient.Free;
  inherited Destroy;
end;

procedure TJvgDigits.Paint;
begin
  try
    if Canvas.Handle <> 0 then
      PaintTo(Canvas);
  except
  end;
end;

procedure TJvgDigits.PaintTo(Canvas: TCanvas);
var
  Pt, OldPt: TPoint;
  XPos, YPos, D, CenterY, S, I, IWidth: Integer;
  Str, SChar: string;
  R: TRect;
  SPassive: Boolean;
  ClientR: TRect;

  procedure FillBackground;
  begin
    if FTransparent or FGradient.Active then
      Exit;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.FillRect(ClientR);
  end;

  function DrawDigit(Pt: TPoint; CActive, CPassive: TColor): Integer;
  label
    deC_L, deB_L, deTL_L, deTR_L, deBL_L, deBR_L, deEND_L;
  begin
    with Canvas do
    begin
      if FInsertSpecialSymbolAt = I then
      begin
        case FSpecialSymbol of
          ssyColon:
            begin
              Pt.X := Pt.X + FInterspace;
              Windows.SetPixel(Handle, Pt.X, Pt.Y + FDSize.Y div 3,
                ColorToRGB(CActive));
              Windows.SetPixel(Handle, Pt.X, Pt.Y + FDSize.Y - FDSize.Y div 3,
                ColorToRGB(CActive));
              Pt.X := Pt.X + FInterspace * 2;
            end;
          ssySlash:
            begin
              Windows.MoveToEx(Handle, Pt.X + FDSize.X, Pt.Y + 1, @OldPt);
              Windows.LineTo(Handle, Pt.X, Pt.Y + FDSize.Y);
              Pt.X := Pt.X + FDSize.X + FInterspace;
            end;
          ssyBackslash:
            begin
              Windows.MoveToEx(Handle, Pt.X, Pt.Y + 1, @OldPt);
              Windows.LineTo(Handle, Pt.X + FDSize.X, Pt.Y + FDSize.Y);
              Pt.X := Pt.X + FDSize.X + FInterspace;
            end;
        end;
      end;
      //OldColonpt.X:=Pt.X;
      if Pen.Width = 1 then
        S := 1
      else
        S := 0;
      R := Rect(Pt.X, Pt.Y, Pt.X + FDSize.X, FDSize.Y + Pt.Y);
      CenterY := R.Top + (FDSize.Y - Pen.Width) div 2;

      SChar := Str[I];
      if SChar = ' ' then
        D := 10
      else
      if (SChar = ',') or (SChar = '.') then
        D := 11
      else
        D := StrToInt(SChar);

      //...Draw Dot
      if Pt.X <= Width then
        if dlDOT in DigitsSet[D] then
        begin
          FOldDotPos := I;
          Windows.SetPixel(Handle, Pt.X, R.Bottom,
            ColorToRGB(CActive));
          Pt.X := Pt.X + FInterspace;
        end
        else
        begin ///...Draw Digit
          if dlT in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deC_L;
          MoveTo(R.Left + FGap, R.Top);
          LineTo(R.Right - FGap + S, R.Top);
        deC_L:
          if dlC in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deB_L;
          MoveTo(R.Left + FGap, CenterY);
          LineTo(R.Right - FGap + S, CenterY);
        deB_L:
          if dlB in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deTL_L;
          MoveTo(R.Left + FGap, R.Bottom);
          LineTo(R.Right - FGap + S, R.Bottom);
        deTL_L:
          if dlTL in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deTR_L;
          MoveTo(R.Left, R.Top + FGap);
          LineTo(R.Left, CenterY - FGap + S);
        deTR_L:
          if dlTR in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deBL_L;
          MoveTo(R.Right, R.Top + FGap);
          LineTo(R.Right, CenterY - FGap + S);
        deBL_L:
          if dlBL in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deBR_L;
          MoveTo(R.Left, R.Bottom - FGap);
          LineTo(R.Left, CenterY + Pen.Width - S + FGap);
        deBR_L:
          if dlBR in DigitsSet[D] then
            Pen.Color := CActive
          else
          if SPassive then
            Pen.Color := CPassive
          else
            goto deEND_L;
          MoveTo(R.Right, R.Bottom - FGap);
          LineTo(R.Right, CenterY + Pen.Width - S + FGap);
        deEND_L:
          Pt.X := Pt.X + FDSize.X + FInterspace;
        end;
    end;
    Result := Pt.X;
  end;

begin
  ClientR := GetClientRect;
  //--- gradient and Bevels
  if FGradient.Active then
    with FBevel, FGradient do
    begin
      InflateRect(ClientR, -FInteriorOffset, -FInteriorOffset);
      GradientBox(Canvas.Handle, ClientR, Gradient,
        Integer(BevelPenStyle), BevelPenWidth);
    end;
  if FBevel.Active then
    with FBevel do
    begin
      ClientR := ClientRect;
      Dec(ClientR.Right);
      Dec(ClientR.Bottom);
      Canvas.Pen.Width := BevelPenWidth;
      Canvas.Pen.Style := BevelPenStyle;
      ClientR := DrawBoxEx(Canvas.Handle, ClientR,
        Sides, Inner, Outer, Bold, 0, True);
      Inc(ClientR.Right);
      Inc(ClientR.Bottom);
    end;
  //InflateRect(ClientR,-InteriorOffset,-InteriorOffset);
  R := ClientR; //Dec( R.Right ); Dec( R.Bottom );
  Str := FloatToStr(FValue);
  if (DigitCount <> -1) and (DigitCount > Length(Str)) then
    for I := 1 to DigitCount - Length(Str) do
      Str := Str + '0';
  if FPositions > 0 then
    Str := Spaces(FPositions - Length(Str)) + Str;

  IWidth := 0;
  for I := 1 to Length(Str) do
    if Str[I] <> ',' then
      Inc(IWidth, FDSize.X + Interspace)
    else
      Inc(IWidth, Interspace);
  Inc(IWidth, Interspace);

  if (FInsertSpecialSymbolAt > 0) and (FInsertSpecialSymbolAt <= Length(Str)) then
    if FSpecialSymbol = ssyColon then
      Inc(IWidth, Interspace * 3)
    else
      Inc(IWidth, FDSize.X + Interspace);
  //else Inc( IWidth ,6 );
  case Alignment of
    taLeftJustify:
      XPos := Interspace;
    taCenter:
      XPos := (ClientR.Right - ClientR.Left - IWidth) div 2 + Interspace;
  else //taRightJustify
    XPos := ClientR.Right - ClientR.Left - IWidth + Interspace;
  end;
  YPos := (Height - FDSize.Y) div 2;
  I := Pos(',', Str);
  if (I <> 0) and (I <> FOldDotPos) then
    FNeedBackgroundPaint := True;
  //if (FInsertSpecialSymbolAt>0)and(OldSpSymbolxPos<>XPos) then FNeedBackgroundPaint:=True;
  with Canvas do
  begin
    if FNeedBackgroundPaint then
      FillBackground;
    Pen.Color := FActiveColor;
    Pen.Style := PenStyle;
    Pen.Width := FPenWidth;

    Pt.X := XPos;
    Pt.Y := YPos;
    SPassive := not FGradient.Active;
    for I := 1 to Length(Str) do
      Pt.X := DrawDigit(Pt, FActiveColor, FPassiveColor);
  end;
  FNeedBackgroundPaint := True;
end;

procedure TJvgDigits.WMSize(var Msg: TWMSize);
begin
  FNeedBackgroundPaint := True;
end;

procedure TJvgDigits.SmthChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJvgDigits.SetValue(NewValue: Double);
begin
  try
    if FValue <> NewValue then
    begin
      FValue := NewValue;
      FNeedBackgroundPaint := FOldStrWidth <> Length(FloatToStr(FValue));
      if FNeedBackgroundPaint then
        FOldStrWidth := Length(FloatToStr(FValue));
      Repaint;
    end;
  except
  end;
end;

procedure TJvgDigits.SetActiveColor(Value: TColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    FNeedBackgroundPaint := False;
    Repaint;
  end;
end;

procedure TJvgDigits.SetPassiveColor(Value: TColor);
begin
  if FPassiveColor <> Value then
  begin
    FPassiveColor := Value;
    FNeedBackgroundPaint := False;
    Repaint;
  end;
end;

procedure TJvgDigits.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetPositions(Value: Word);
begin
  if FPositions <> Value then
  begin
    FPositions := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetPenWidth(Value: Word);
begin
  if FPenWidth <> Value then
  begin
    FPenWidth := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetInterspace(Value: Word);
begin
  if FInterspace <> Value then
  begin
    FInterspace := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetGap(Value: Word);
begin
  if FGap <> Value then
  begin
    FGap := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetInteriorOffset(Value: Word);
begin
  if FInteriorOffset <> Value then
  begin
    FInteriorOffset := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetInsertSpecialSymbolAt(Value: Integer);
begin
  if FInsertSpecialSymbolAt <> Value then
  begin
    FInsertSpecialSymbolAt := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetPenStyle(Value: TPenStyle);
begin
  if FPenStyle <> Value then
  begin
    FPenStyle := Value;
    FNeedBackgroundPaint := False;
    Repaint;
  end;
end;

procedure TJvgDigits.SetSpecialSymbol(Value: TJvgSpecialSymbol);
begin
  if FSpecialSymbol <> Value then
  begin
    FSpecialSymbol := Value;
    if Value = ssyNone then
      FInsertSpecialSymbolAt := -1;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

procedure TJvgDigits.SetDigitCount(Value: Integer);
begin
  if FDigitCount <> Value then
  begin
    FDigitCount := Value;
    FNeedBackgroundPaint := True;
    Repaint;
  end;
end;

end.

