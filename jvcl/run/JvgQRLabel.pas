{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgQRLabel.PAS, released on 2003-01-15.

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

unit JvgQRLabel;

interface

uses
  Windows, Classes, Graphics, QRCtrls, DB,
  JvgTypes;

type
  TJvgQRLabel = class(TQRCustomLabel)
  private
    FDirection: TglLabelDir;
    FEscapment: Integer;
    FAlignment: TAlignment;
    FPrinting: Boolean;
    procedure SetDirection(Value: TglLabelDir);
    procedure SetEscapment(Value: Integer);
  protected
    procedure SetAlignment(Value: TAlignment); override;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Paint; override;
    procedure Print(OfsX, OfsY: Integer); override;
    procedure PaintLabel(Caption: string; Canvas: TCanvas; OfsX, OfsY: Integer);
  published
    property Direction: TglLabelDir read FDirection write SetDirection
      default fldLeftRight;
    property Escapment: Integer read FEscapment write SetEscapment default 0;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Align;
    property AutoSize;
    property AlignToBand;
    property AutoStretch;
    property Color;
    property Caption;
    property Font;
    property Transparent;
    property Visible;
    property Enabled;
  end;

  TJvgQRDBText = class(TJvgQRLabel)
  private
    FDataSet: TDataSet;
    FDataField: string;
    procedure SetDataField(Value: string);
  protected
    procedure Paint; override;
    procedure Print(OfsX, OfsY: Integer); override;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
    property DataField: string read FDataField write SetDataField;
  end;

implementation

uses
  SysUtils, Controls,
  JvgUtils;

//=== TJvgQRLabel ============================================================

constructor TJvgQRLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := True;
  FAlignment := taLeftJustify;
  FEscapment := 0;
end;

procedure TJvgQRLabel.Paint;
begin
  FPrinting := False;
  PaintLabel(Caption, Canvas, 0, 0);
end;

procedure TJvgQRLabel.PaintLabel(Caption: string; Canvas: TCanvas; OfsX, OfsY: Integer);
var
  FreeFontHandle: THandle;
  R: TRect;
  X, Y: Integer;
  Size, TextSize: TSize;
  PixFactor: Single;
begin
  X := 0;
  Y := 0;

  Canvas.Font := Font;
  Canvas.Font.Size := Round(Font.Size * Zoom / 100);

  FreeFontHandle := CreateRotatedFont(Canvas.Font, Escapment);
  Canvas.Font.Handle := FreeFontHandle;

  GetTextExtentPoint32(Canvas.Handle, PChar(Caption), Length(Caption), Size);
  Inc(Size.cx);
  Inc(Size.cy);
  TextSize := Size;

  if (Align = alNone) and AutoSize then
    case FDirection of
      fldLeftRight, fldRightLeft:
        begin
          Width := Size.cx;
          Height := Size.cy;
        end;
    else {fldDownUp, fldUpDown:}
      Width := Size.cy;
      Height := Size.cx;
    end;
  case FDirection of
    fldLeftRight:
      begin //if Align = alNone then begin Width:=max(w,Size.cx);Height:=max(h,Size.cy); end;
        case Alignment of
          taCenter:
            X := (Width - Size.cx) div 2;
          taRightJustify:
            X := Width - Size.cx;
        end;
      end;
    fldRightLeft:
      begin //if Align = alNone then begin Width:=max(w,Size.cx);Height:=max(h,Size.cy);X:=Width;Y:=Height; end;
        case Alignment of
          taCenter:
            X := (Width + Size.cx) div 2;
          taLeftJustify:
            X := Width - (Size.cx - TextSize.cx) - 2;
        else
          X := TextSize.cx;
        end;
        Y := TextSize.cy;
      end;
    fldDownUp:
      begin //if Align = alNone then begin Height:=max(h,Size.cx);Width:=max(w,Size.cy);Y:=Height-2; end;
        case Alignment of
          taCenter:
            Y := (Height + TextSize.cx - (Size.cy - TextSize.cy)) div 2;
          taRightJustify:
            Y := TextSize.cx - 4;
        else
          Y := Height - (Size.cy - TextSize.cy) - 2;
        end;
      end;
    fldUpDown:
      begin //if Align = alNone then begin Height:=max(h,Size.cx);Width:=max(w,Size.cy);X:=Width; end;
        case Alignment of
          taCenter:
            Y := (Height - Size.cx) div 2;
          taRightJustify:
            Y := Height - Size.cx;
        else
          Y := 1;
        end;
        X := TextSize.cy;
      end;
  end;

  PixFactor := (Height / Self.Size.Height);
  if Assigned(QRPrinter) then
  begin
    X := QRPrinter.XPos(OfsX {+ Self.Size.Left} + Round(X / PixFactor));
    Y := QRPrinter.YPos(OfsY {+ Self.Size.Top} + Round(Y / PixFactor));
  end;

  if Transparent then
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT)
  else
    SetBkMode(Canvas.Handle, OPAQUE);
  SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));

  if FPrinting then
  begin
    //      with QRPrinter do R := Bounds(XPos(OfsX), YPos(OfsY), {XPos}trunc(Width * Zoom / 100), trunc(Height*Zoom / 100));
    with QRPrinter do
      R := Rect(XPos(OfsX {+ Self.Size.Left}), YPos(OfsY {+ Self.Size.Top}), XPos(OfsX + Self.Size.Left +
        Self.Size.Width), YPos(OfsY + Self.Size.Top + Self.Size.Height));
    ExtTextOut(Canvas.Handle, X {QRPrinter.XPos(OfsX)+X}, Y {QRPrinter.YPos(OfsY)+Y}, ETO_CLIPPED, @R, PChar(Caption),
      Length(Caption), nil);
  end
  else
    Canvas.TextOut(OfsX + X, OfsY + Y, Caption);
  //      ExtTextOut(Canvas.Handle, OfsX+X,OfsY+Y, ETO_CLIPPED, @R, PChar(Caption), Length(Caption), nil);
  DeleteObject(FreeFontHandle);
  //    QRPrinter.Canvas.Font.Assign(OldFont);
    (*
      SaveIndex := SaveDC(Canvas.Handle);
      SetViewportOrgEx(Canvas.Handle, OfsX, OfsY, nil);
      gLabel.ExternalCanvas := Canvas;
      gLabel.Paint;
      RestoreDC(Canvas.Handle, SaveIndex);
  *)
end;

procedure TJvgQRLabel.Print(OfsX, OfsY: Integer);
begin
  //  JvgLabel.Direction := fldDownUp;
  if ParentReport.FinalPass then
  begin
    FPrinting := True;
    PaintLabel(Caption, QRPrinter.Canvas, Round(Size.Left + OfsX), Round(Size.Top + OfsY));
  end;
end;

procedure TJvgQRLabel.SetDirection(Value: TglLabelDir);
const
  RadianEscapments: array [TgllabelDir] of Integer =
    (0, -1800, -900, 900);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    FEscapment := RadianEscapments[FDirection];
    Repaint;
    //CreateLabelFont;
  end;
end;

procedure TJvgQRLabel.SetEscapment(Value: Integer);
begin
  if FEscapment <> Value then
  begin
    FEscapment := Value;
    Repaint;
    //CreateLabelFont;
  end;
end;

procedure TJvgQRLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Repaint;
  end;
end;

//=== TJvgQRDBText ===========================================================

procedure TJvgQRDBText.Paint;
begin
  FPrinting := False;
  //  if DataField <> '' then Caption := DataField;
  PaintLabel(Caption, Canvas, 0, 0);
end;

procedure TJvgQRDBText.Print(OfsX, OfsY: Integer);
begin
  if ParentReport.FinalPass then
  begin
    FPrinting := True;
    if (DataField <> '') and Assigned(DataSet) and (DataSet.FindField(DataField) <> nil) then
      Caption := DataSet.FindField(DataField).AsString;
    PaintLabel(Caption, QRPrinter.Canvas, Round(Size.Left + OfsX), Round(Size.Top + OfsY));
  end;
end;

procedure TJvgQRDBText.SetDataField(Value: string);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    Caption := FDataField;
    Repaint;
  end;
end;

end.

