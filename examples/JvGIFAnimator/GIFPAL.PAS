{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit GIFPal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, JvGrids, JvExGrids;

const
  NumPaletteEntries = 256;

type
  TPaletteForm = class(TForm)
    RightPanel: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    GridPanel: TPanel;
    ColorGrid: TJvDrawGrid ;
    procedure ColorGridDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FPaletteEntries: array[0..NumPaletteEntries - 1] of TPaletteEntry;
    FPalette: HPALETTE;
    FCount: Integer;
    function CellColor(Row, Col: Longint): TColor;
    procedure DrawSquare(Row, Col: Longint; CellRect: TRect; ShowSelector: Boolean);
    procedure ColorToCell(AColor: TColor; var Col, Row: Longint);
  protected
    function GetPalette: HPALETTE; override;
  public
    { Public declarations }
    procedure SetPalette(Palette: HPALETTE);
  end;

var
  PaletteForm: TPaletteForm;

function SelectColor(Palette: HPALETTE; AColor: TColor): TColor;

implementation

uses JvJVCLUtils, Math;

{$R *.DFM}

function SelectColor(Palette: HPALETTE; AColor: TColor): TColor;
var
  Col, Row: Longint;
begin
  Result := AColor;
  if Palette = 0 then begin
    Beep; Exit;
  end;
  with TPaletteForm.Create(Application) do
  try
    SetPalette(Palette);
    ColorToCell(AColor, Col, Row);
    ColorGrid.Col := Col;
    ColorGrid.Row := Row;
    ActiveControl := ColorGrid;
    if ShowModal = mrOk then begin
      Result := CellColor(ColorGrid.Row, ColorGrid.Col);
    end;
  finally
    Free;
  end;
end;

procedure TPaletteForm.ColorToCell(AColor: TColor; var Col, Row: Longint);
var
  I: Word;
begin
  I := GetNearestPaletteIndex(FPalette, ColorToRGB(AColor));
  if I < FCount then begin
    Row := I div ColorGrid.RowCount;
    Col := I - (ColorGrid.ColCount * Row);
  end
  else begin
    Col := -1;
    Row := -1;
  end;
end;

function TPaletteForm.CellColor(Row, Col: Longint): TColor;
var
  PalIndex: Integer;
begin
  PalIndex := Col + (Row * ColorGrid.ColCount);
  with FPaletteEntries[PalIndex] do
    Result := TColor(RGB(peRed, peGreen, peBlue));
end;

procedure TPaletteForm.DrawSquare(Row, Col: Longint; CellRect: TRect;
  ShowSelector: Boolean);
var
  SavePal: HPalette;
begin
  ColorGrid.Canvas.Pen.Color := clBtnFace;
  with CellRect do ColorGrid.Canvas.Rectangle(Left, Top, Right, Bottom);
  InflateRect(CellRect, -1, -1);
  Frame3D(ColorGrid.Canvas, CellRect, clBtnShadow, clBtnHighlight, 2);
  SavePal := 0;
  if FPalette <> 0 then begin
    SavePal := SelectPalette(ColorGrid.Canvas.Handle, FPalette, False);
    RealizePalette(ColorGrid.Canvas.Handle);
  end;
  ColorGrid.Canvas.Brush.Color := PaletteColor(CellColor(Row, Col));
  ColorGrid.Canvas.Pen.Color := PaletteColor(CellColor(Row, Col));
  with CellRect do
    ColorGrid.Canvas.Rectangle(Left, Top, Right, Bottom);
  if FPalette <> 0 then
    SelectPalette(ColorGrid.Canvas.Handle, SavePal, True);
  if ShowSelector then begin
    ColorGrid.Canvas.Brush.Color := Self.Color;
    ColorGrid.Canvas.Pen.Color := Self.Color;
    InflateRect(CellRect, -1, -1);
    ColorGrid.Canvas.DrawFocusRect(CellRect);
  end;
end;

function TPaletteForm.GetPalette: HPALETTE;
begin
  if FPalette <> 0 then Result := FPalette
  else Result := inherited GetPalette;
end;

procedure TPaletteForm.SetPalette(Palette: HPALETTE);
var
  I: Integer;
begin
  FCount := Min(PaletteEntries(Palette), NumPaletteEntries);
  FPalette := Palette;
  GetPaletteEntries(Palette, 0, FCount, FPaletteEntries);
  if FCount <= 16 then begin
    ColorGrid.RowCount := 2;
    ColorGrid.ColCount := 8;
    ColorGrid.DefaultColWidth := 31;
    ColorGrid.DefaultRowHeight := 31;
  end
  else begin
    ColorGrid.RowCount := FCount div 16;
    if FCount mod 16 > 0 then
      ColorGrid.RowCount := ColorGrid.RowCount + 1;
    ColorGrid.ColCount := 16;
    ColorGrid.DefaultColWidth := 18;
    ColorGrid.DefaultRowHeight := 18;
  end;
  for I := FCount to NumPaletteEntries - 1 do
    FillChar(FPaletteEntries[I], SizeOf(TPaletteEntry), $80);
  ClientWidth := (ColorGrid.ColCount * ColorGrid.DefaultColWidth) +
    (GridPanel.BorderWidth * 2) + RightPanel.Width +
    (ColorGrid.Width - ColorGrid.ClientWidth);
  ClientHeight := (ColorGrid.RowCount * ColorGrid.DefaultRowHeight) +
    (GridPanel.BorderWidth * 2) + (ColorGrid.Height - ColorGrid.ClientHeight);
  if HandleAllocated then PostMessage(Handle, WM_QUERYNEWPALETTE, 0, 0);
end;

procedure TPaletteForm.ColorGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  DrawSquare(Row, Col, Rect, gdFocused in State);
end;

procedure TPaletteForm.FormCreate(Sender: TObject);
begin
  with ColorGrid.Canvas do begin
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
  end;
end;

end.
