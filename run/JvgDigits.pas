{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDigits.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgDigits;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  JvgTypes,
  JVComponent,
  JvgUtils,
  JvgCommClasses;

type
  TGraphDigitsElem = (_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_,
    _deDOT_);
  TGraphDigitsElemSet = set of TGraphDigitsElem;
  TColorsPair = record ActiveColor, PassiveColor: TColor;
  end;
  TSpSymbol = (_none_, _colon_, _slash_, _backslash_);

  TJvgDigits = class(TJvGraphicControl)
  private
    FValue: Double;
    FDSize: TJvgPointClass;
    FActiveColor: TColor;
    FPassiveColor: TColor;
    FBackgrColor: TColor;
    FInsertSpSymbolAt: integer;
    FPositions: word;
    FPenWidth: word;
    FGap: word;
    FInterspace: word;
    FTransparent: boolean;
    FAlignment: TAlignment;
    FInteriorOffset: word;
    FPenStyle: TPenStyle;
    FSpecialSymbol: TSpSymbol;
    FBevel: TJvgExtBevel;
    FGradient: TJvgGradient;
    FDigitCount: integer;

    OldStrWidth: integer;
    OldDotPos: integer;
    fNeedRepaint: boolean;
    ClientR: TRect;
    ColorsPair: TColorsPair;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetValue(NewValue: Double);
    procedure SetActiveColor(Value: TColor);
    procedure SetPassiveColor(Value: TColor);
    procedure SetBackgrColor(Value: TColor);
    procedure SetPositions(Value: Word);
    procedure SetPenWidth(Value: Word);
    procedure SetInterspace(Value: Word);
    procedure SetGap(Value: Word);
    procedure SetTransparent(Value: boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetInteriorOffset(Value: word);
    procedure SetInsertSpSymbolAt(Value: integer);
    procedure SetPenStyle(Value: TPenStyle);
    procedure SetSpecialSymbol(Value: TSpSymbol);
    procedure SetDigitCount(Value: integer);

    procedure SmthChanged(Sender: TObject);
  public
    procedure Paint; override;
    procedure PaintTo(Canvas: TCanvas);
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property Value: Double read FValue write SetValue;
    property DigitSize: TJvgPointClass read FDSize write FDSize;
    property ActiveColor: TColor read FActiveColor write SetActiveColor
      default clWhite;
    property PassiveColor: TColor read FPassiveColor write SetPassiveColor
      default $00202020;
    property BackgrColor: TColor read FBackgrColor write SetBackgrColor
      default clBlack;
    property Positions: word read FPositions write SetPositions
      default 0;
    property PenWidth: word read FPenWidth write SetPenWidth
      default 1;
    property Gap: word read FGap write SetGap
      default 1;
    property Interspace: word read FInterspace write SetInterspace
      default 3;
    property Transparent: boolean read FTransparent write SetTransparent
      default false;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taCenter;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset
      default 0;
    property InsertSpSymbolAt: integer read FInsertSpSymbolAt write
      SetInsertSpSymbolAt
      default -1;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle
      default psSolid;
    property SpecialSymbol: TSpSymbol
      read FSpecialSymbol write SetSpecialSymbol default _none_;
    property Bevel: TJvgExtBevel read FBevel write FBevel;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property DigitCount: integer read FDigitCount write SetDigitCount
      default -1;
  end;

const
  DigitsSet: array[0..11] of TGraphDigitsElemSet
  = ([_deT_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_], //...0
    [_deTR_, _deBR_], //...1
    [_deT_, _deC_, _deB_, _deTR_, _deBL_], //...2
    [_deT_, _deC_, _deB_, _deTR_, _deBR_], //...3
    [_deC_, _deTL_, _deTR_, _deBR_], //...4
    [_deT_, _deC_, _deB_, _deTL_, _deBR_], //...5
    [_deT_, _deC_, _deB_, _deTL_, _deBL_, _deBR_], //...6
    [_deT_, _deTR_, _deBR_], //...7
    [_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_], //...8
    [_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBR_], //...9
    [], //...' '
    [_deDOT_] //...','
    );

implementation

//*****************************************_____________LowLevel METHODS

constructor TJvgDigits.Create(AOwner: TComponent);
begin
  inherited;
  FDSize := TJvgPointClass.Create;
  //...set defaults
  Width := 160;
  Height := 28;
  //  if csDesigning in ComponentState then FValue:=1.1234567890;
  FDSize.x := 10;
  FDSize.y := 21;
  FDSize.OnChanged := SmthChanged;
  FActiveColor := clWhite;
  FPassiveColor := $00202020;
  FBackgrColor := clBlack;
  FPositions := 0;
  FPenWidth := 1;
  FGap := 1;
  FInterspace := 3;
  FTransparent := false;
  FInteriorOffset := 0;
  FInsertSpSymbolAt := -1;
  FDigitCount := -1;
  FPenStyle := psSolid;
  fNeedRepaint := true;
  Color := FBackgrColor;
  FAlignment := taCenter;
  FSpecialSymbol := _none_;
  FBevel := TJvgExtBevel.Create;
  FBevel.OnChanged := SmthChanged;
  FGradient := TJvgGradient.Create;
  FGradient.OnChanged := SmthChanged;
end;

destructor TJvgDigits.Destroy;
begin
  FDSize.Free;
  FBevel.Free;
  FGradient.Free;
  inherited;
end;

procedure TJvgDigits.Paint;
begin
  try
    if Canvas.handle <> 0 then
      PaintTo(Canvas);
  except
  end;
end;

procedure TJvgDigits.PaintTo(Canvas: TCanvas);
var
  pt, OldPt: TPoint;
  xPos, yPos, D, CenterY, s, i, IWidth: integer;
  str, sChar: string;
  r: TRect;
  SPassive: boolean;

  procedure FillBackgr; //**************************LOCAL PROC
  begin
    if FTransparent or FGradient.Active then
      exit;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBackgrColor;
    Canvas.FillRect(ClientR);
  end; //********************************************LOCAL PROC END

  function DrawDigit(pt: TPoint; C: TColorsPair): integer; //****LOCAL PROC
  label
    deC_L, deB_L, deTL_L, deTR_L, deBL_L, deBR_L, deEND_L;
  begin
    with Canvas do
    begin
      if FInsertSpSymbolAt = i then
      begin

        case FSpecialSymbol of
          _Colon_:
            begin
              pt.x := pt.x + FInterspace;
              Windows.SetPixel(Handle, pt.x, pt.y + FDsize.y div 3,
                ColorToRGB(C.ActiveColor));
              Windows.SetPixel(Handle, pt.x, pt.y + FDSize.y - FDsize.y
                div 3, ColorToRGB(C.ActiveColor));
              pt.x := pt.x + FInterspace * 2;
            end;
          _slash_:
            begin
              Windows.MoveToEx(Handle, pt.x + FDSize.x, pt.y + 1,
                @OldPt);
              Windows.LineTo(Handle, pt.x, pt.y + FDSize.y);
              pt.x := pt.x + FDSize.x + FInterspace;
            end;
          _backslash_:
            begin
              Windows.MoveToEx(Handle, pt.x, pt.y + 1, @OldPt);
              Windows.LineTo(Handle, pt.x + FDSize.x, pt.y + FDSize.y);
              pt.x := pt.x + FDSize.x + FInterspace;
            end;
        end;
      end;
      //OldColonpt.x:=pt.x;
      if Pen.Width = 1 then
        s := 1
      else
        s := 0;
      r := Rect(pt.x, pt.y, pt.x + FDSize.x, FDSize.y + pt.y);
      CenterY := r.top + (FDSize.y - Pen.Width) div 2;

      sChar := str[i];
      if sChar = ' ' then
        d := 10
      else if (sChar = ',') or (sChar = '.') then
        d := 11
      else
        d := StrToInt(sChar);

      //...Draw Dot
      if pt.x <= Width then
        if _deDOT_ in DigitsSet[d] then
        begin
          OldDotPos := i;
          Windows.SetPixel(Handle, pt.x, r.bottom,
            ColorToRGB(C.ActiveColor));
          pt.x := pt.x + FInterspace;
        end
        else
        begin ///...Draw Digit

          if _deT_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deC_L;
          MoveTo(r.left + FGap, r.top);
          LineTo(r.right - FGap + s, r.top);
          deC_L:
          if _deC_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deB_L;
          MoveTo(r.left + FGap, CenterY);
          LineTo(r.right - FGap + s, CenterY);
          deB_L:
          if _deB_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deTL_L;
          MoveTo(r.left + FGap, r.bottom);
          LineTo(r.right - FGap + s, r.bottom);
          deTL_L:
          if _deTL_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deTR_L;
          MoveTo(r.left, r.top + FGap);
          LineTo(r.left, CenterY - FGap + s);
          deTR_L:
          if _deTR_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deBL_L;
          MoveTo(r.right, r.top + FGap);
          LineTo(r.right, CenterY - FGap + s);
          deBL_L:
          if _deBL_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deBR_L;
          MoveTo(r.left, r.bottom - FGap);
          LineTo(r.left, CenterY + Pen.Width - s + FGap);
          deBR_L:
          if _deBR_ in DigitsSet[d] then
            Pen.Color := C.ActiveColor
          else if SPassive then
            Pen.Color := C.PassiveColor
          else
            goto deEND_L;
          MoveTo(r.right, r.bottom - FGap);
          LineTo(r.right, CenterY + Pen.Width - s + FGap);
          deEND_L:
          pt.x := pt.x + FDSize.x + FInterspace;
        end;
    end;
    Result := pt.x;
  end; //********************************************LOCAL PROC END

begin //*********************************************MAIN PAINT PROC
  ClientR := GetClientRect;
  //--- gradient and Bevels
  if FGradient.Active then
    with FBevel, FGradient do
    begin
      InflateRect(ClientR, -FInteriorOffset, -FInteriorOffset);
      GradientBox(Canvas.handle, ClientR, Gradient,
        integer(BevelPenStyle), BevelPenWidth);
    end;
  if FBevel.Active then
    with FBevel do
    begin
      ClientR := ClientRect;
      dec(ClientR.right);
      dec(ClientR.bottom);
      Canvas.Pen.Width := BevelPenWidth;
      Canvas.Pen.Style := BevelPenStyle;
      ClientR := DrawBoxEx(Canvas.Handle, ClientR,
        Sides, Inner, Outer,
        Bold, 0, true);
      inc(ClientR.Right);
      inc(ClientR.Bottom);
    end;
  //---
//  InflateRect(ClientR,-InteriorOffset,-InteriorOffset);
  r := ClientR; //dec( r.right ); dec( r.bottom );
  str := FloatToStr(FValue);
  if (DigitCount <> -1) and (DigitCount > Length(str)) then
    for i := 1 to DigitCount - Length(str) do
      str := str + '0';
  if FPositions > 0 then
    str := Spaces(FPositions - Length(str)) + str;

  IWidth := 0;
  for i := 1 to Length(str) do
    if str[i] <> ',' then
      inc(IWidth, FDSize.x + InterSpace)
    else
      inc(IWidth, InterSpace);
  inc(IWidth, InterSpace);

  if (FInsertSpSymbolAt > 0) and (FInsertSpSymbolAt <= Length(str)) then
    if FSpecialSymbol = _colon_ then
      inc(IWidth, InterSpace * 3)
    else
      inc(IWidth, FDSize.x + InterSpace);
  //else inc( IWidth ,6 );
  case Alignment of
    taLeftJustify: xPos := InterSpace;
    taCenter: xPos := (ClientR.right - ClientR.left - IWidth) div 2 +
      InterSpace;
  else {taRightJustify:}
    xPos := ClientR.right - ClientR.left - IWidth + InterSpace;
  end;
  yPos := (Height - FDSize.y) div 2;
  i := Pos(',', str);
  if (i <> 0) and (i <> OldDotPos) then
    fNeedRepaint := true;
  //if (FInsertSpSymbolAt>0)and(OldSpSymbolxPos<>xPos) then fNeedRepaint:=true;
  with Canvas do
  begin
    if fNeedRepaint then
      FillBackgr;
    Pen.Color := FActiveColor;
    Pen.Style := PenStyle;
    Pen.Width := FPenWidth;

    pt.x := xPos;
    pt.y := yPos;
    ColorsPair.ActiveColor := FActiveColor;
    ColorsPair.PassiveColor := FPassiveColor;
    SPassive := not FGradient.Active;
    for i := 1 to Length(str) do
      pt.x := DrawDigit(pt, ColorsPair);
  end;
  fNeedRepaint := true;
end;

procedure TJvgDigits.WMSize(var Message: TWMSize);
begin
  fNeedRepaint := true;
end;

procedure TJvgDigits.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TJvgDigits.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TJvgDigits.SmthChanged(Sender: TObject);
begin
  Paint;
end;
//...______________________________________________PROPERTIES METHODS

procedure TJvgDigits.SetValue(NewValue: Double);
begin
  try
    if FValue = NewValue then
      exit;
    FValue := NewValue;
    if OldStrWidth <> Length(FloatToStr(FValue)) then
    begin
      OldStrWidth := Length(FloatToStr(FValue));
      fNeedRepaint := true;
      rePaint;
    end
    else
    begin
      fNeedRepaint := false;
      Paint;
    end;
  except
  end;
end;

procedure TJvgDigits.SetActiveColor(Value: TColor);
begin
  if FActiveColor = Value then
    exit;
  FActiveColor := Value;
  fNeedRepaint := false;
  Paint;
end;

procedure TJvgDigits.SetPassiveColor(Value: TColor);
begin
  if FPassiveColor = Value then
    exit;
  FPassiveColor := Value;
  fNeedRepaint := false;
  Paint;
end;

procedure TJvgDigits.SetBackgrColor(Value: TColor);
begin
  if FBackgrColor = Value then
    exit;
  FBackgrColor := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetPositions(Value: Word);
begin
  if FPositions = Value then
    exit;
  FPositions := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetPenWidth(Value: Word);
begin
  if FPenWidth = Value then
    exit;
  FPenWidth := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetInterspace(Value: Word);
begin
  if FInterspace = Value then
    exit;
  FInterspace := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetGap(Value: Word);
begin
  if FGap = Value then
    exit;
  FGap := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetTransparent(Value: boolean);
begin
  if FTransparent = Value then
    exit;
  FTransparent := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetAlignment(Value: TAlignment);
begin
  if FAlignment = Value then
    exit;
  FAlignment := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetInteriorOffset(Value: word);
begin
  if FInteriorOffset = Value then
    exit;
  FInteriorOffset := Value;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetInsertSpSymbolAt(Value: integer);
begin
  if FInsertSpSymbolAt = Value then
    exit;
  FInsertSpSymbolAt := Value;
  fNeedRepaint := true;
  Paint;
end;

procedure TJvgDigits.SetPenStyle(Value: TPenStyle);
begin
  if FPenStyle = Value then
    exit;
  FPenStyle := Value;
  fNeedRepaint := false;
  Paint;
end;

procedure TJvgDigits.SetSpecialSymbol(Value: TSpSymbol);
begin
  if FSpecialSymbol = Value then
    exit;
  FSpecialSymbol := Value;
  if Value = _none_ then
    FInsertSpSymbolAt := -1;
  fNeedRepaint := true;
  rePaint;
end;

procedure TJvgDigits.SetDigitCount(Value: integer);
begin
  if FDigitCount = Value then
    exit;
  FDigitCount := Value;
  fNeedRepaint := true;
  rePaint;
end;

end.
