{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgQRLabel.PAS, released on 2003-01-15.

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

unit JvgQRLabel;

interface

uses Windows, classes, graphics, QRCTRLS, JvgTypes, db;

type

  TJvgQRLabel = class(TQRCustomLabel)
  private
    FDirection: TglLabelDir;
    FEscapment: integer;
    FAlignment: TAlignment;
    fPrinting: boolean;
    procedure SetDirection(Value: TglLabelDir);
    procedure SetEscapment(Value: integer);
    procedure SetAlignment(Value: TAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Paint; override;
    procedure Print(OfsX, OfsY: integer); override;
    procedure PaintLabel(Caption: string; Canvas: TCanvas; OfsX, OfsY: Integer);
  published
    property Direction: TglLabelDir read FDirection write SetDirection
      default fldLeftRight;
    property Escapment: integer read FEscapment write SetEscapment;
    property Alignment: TAlignment read FAlignment write SetAlignment;
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
    procedure Print(OfsX, OfsY: integer); override;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
    property DataField: string read FDataField write SetDataField;
  end;

implementation
uses JvgUtils, sysUtils, controls;

constructor TJvgQRLabel.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := true;
end;

destructor TJvgQRLabel.Destroy;
begin
  inherited;
end;

procedure TJvgQRLabel.Paint;
begin
  fPrinting := false;
  PaintLabel(Caption, Canvas, 0, 0);
end;

procedure TJvgQRLabel.PaintLabel(Caption: string; Canvas: TCanvas; OfsX, OfsY: Integer);
var
  FreeFontHandle: THandle;
  R: TRect;
  x, y: integer;
  Size, TextSize: TSIZE;
  PixFactor: single;
begin
  X := 0;
  Y := 0;

  Canvas.Font := Font;
  Canvas.Font.Size := Round(Font.Size * Zoom / 100);

  FreeFontHandle := CreateRotatedFont(Canvas.Font, Escapment);
  Canvas.Font.Handle := FreeFontHandle;

  GetTextExtentPoint32(Canvas.handle, PChar(Caption),
    length(Caption), Size);
  inc(Size.cx, 1);
  inc(Size.cy, 1);
  TextSize := Size;

  if (Align = alNone) and AutoSize then
    case FDirection of
      fldLeftRight, fldRightLeft:
        begin
          width := Size.cx;
          height := Size.cy;
        end;
    else {fldDownUp,fldUpDown:}
      begin
        width := Size.cy;
        height := Size.cx;
      end;
    end;
  case FDirection of
    fldLeftRight:
      begin //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy); end;
        case Alignment of
          taCenter: x := (Width - Size.cx) div 2;
          taRightJustify: x := Width - Size.cx;
        end;
      end;
    fldRightLeft:
      begin //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy);x:=width;y:=height; end;
        case Alignment of
          taCenter: x := (Width + Size.cx) div 2;
          taLeftJustify: x := Width - (Size.cx - TextSize.cx) - 2;
        else
          x := TextSize.cx;
        end;
        y := TextSize.cy;
      end;
    fldDownUp:
      begin //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);y:=height-2; end;
        case Alignment of
          taCenter: y := (Height + TextSize.cx - (Size.cy - TextSize.cy)) div 2;
          taRightJustify: y := TextSize.cx - 4;
        else
          y := Height - (Size.cy - TextSize.cy) - 2;
        end;
      end;
    fldUpDown:
      begin //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);x:=width; end;
        case Alignment of
          taCenter: y := (Height - Size.cx) div 2;
          taRightJustify: y := Height - Size.cx;
        else
          y := 1;
        end;
        x := TextSize.cy;
      end;
  end;

  PixFactor := (Height / self.Size.Height);
  if assigned(QRPrinter) then
  begin
    X := QRPrinter.XPos(OfsX {+ self.Size.Left} + Round(X / PixFactor));
    Y := QRPrinter.YPos(OfsY {+ self.Size.Top} + Round(Y / PixFactor));
  end;

  if Transparent then
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT)
  else
    SetBkMode(Canvas.Handle, OPAQUE);
  SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));

  if fPrinting then
  begin
    //      with QRPrinter do R := Bounds(XPos(OfsX), YPos(OfsY), {XPos}trunc(Width * Zoom / 100), trunc(Height*Zoom / 100));
    with QRPrinter do
      R := Rect(XPos(OfsX {+ self.Size.Left}), YPos(OfsY {+ self.Size.Top}), XPos(OfsX + self.Size.Left + self.Size.Width), YPos(OfsY + self.Size.Top + self.Size.Height));
    ExtTextOut(Canvas.Handle, X {QRPrinter.XPos(OfsX)+X}, Y {QRPrinter.YPos(OfsY)+Y}, ETO_CLIPPED, @R, PChar(Caption), Length(Caption), nil);
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
    fPrinting := true;
    PaintLabel(Caption, QRPrinter.Canvas, round(Size.Left + OfsX), round(Size.Top + OfsY));
  end;
end;

procedure TJvgQRLabel.SetDirection(Value: TglLabelDir);
const
  RadianEscapments: array[TgllabelDir] of integer = (0, -1800, -900, 900);
begin
  FDirection := Value;
  FEscapment := RadianEscapments[FDirection];
  repaint;
  //  CreateLabelFont;
end;

procedure TJvgQRLabel.SetEscapment(Value: integer);
begin
  FEscapment := Value;
  repaint;
  //  CreateLabelFont;
end;

procedure TJvgQRLabel.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  repaint;
end;
//------------------------------------------------------------------------------

procedure TJvgQRDBText.Paint;
begin
  fPrinting := false;
  //  if DataField <> '' then Caption := DataField;
  PaintLabel(Caption, Canvas, 0, 0);
end;

procedure TJvgQRDBText.Print(OfsX, OfsY: Integer);
begin
  if ParentReport.FinalPass then
  begin
    fPrinting := true;
    if (DataField <> '') and Assigned(DataSet) and (DataSet.FindField(DataField) <> nil) then
      Caption := DataSet.FindField(DataField).AsString;
    PaintLabel(Caption, QRPrinter.Canvas, round(Size.Left + OfsX), round(Size.Top + OfsY));
  end;
end;

procedure TJvgQRDBText.SetDataField(Value: string);
begin
  FDataField := Value;
  Caption := FDataField;
  repaint;
end;

end.
