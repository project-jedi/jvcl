{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Graphics.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvInterpreter_Graphics;

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF COMPLIB_VCL}
  Windows, Classes, Graphics, JvInterpreter_Windows;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Types, Classes, QGraphics, JvInterpreter_Types;
  {$ENDIF COMPLIB_CLX}

{ TFont }

{ constructor Create }

procedure TFont_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFont.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TFont_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Handle: HFont }

procedure TFont_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := Integer(TFont(Args.Obj).Handle);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := P2V(TFont(Args.Obj).Handle);
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Handle(Value: HFont) }

procedure TFont_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TFont(Args.Obj).Handle := Value;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TFont(Args.Obj).Handle := V2P(Value);
  {$ENDIF COMPLIB_CLX}
end;

{ property Read PixelsPerInch: Integer }

procedure TFont_Read_PixelsPerInch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).PixelsPerInch;
end;

{ property Write PixelsPerInch(Value: Integer) }

procedure TFont_Write_PixelsPerInch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).PixelsPerInch := Value;
end;

{$IFDEF COMPILER3_UP}

{ property Read Charset: TFontCharset }

procedure TFont_Read_Charset(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Charset;
end;

{ property Write Charset(Value: TFontCharset) }

procedure TFont_Write_Charset(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Charset := Value;
end;

{$ENDIF COMPILER3_UP}

{ property Read Color: TColor }

procedure TFont_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TFont_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Color := Value;
end;

{ property Read Height: Integer }

procedure TFont_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Height;
end;

{ property Write Height(Value: Integer) }

procedure TFont_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Height := Value;
end;

{ property Read Name: TFontName }

procedure TFont_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Name;
end;

{ property Write Name(Value: TFontName) }

procedure TFont_Write_Name(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Name := Value;
end;

{ property Read Pitch: TFontPitch }

procedure TFont_Read_Pitch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Pitch;
end;

{ property Write Pitch(Value: TFontPitch) }

procedure TFont_Write_Pitch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Pitch := Value;
end;

{ property Read Size: Integer }

procedure TFont_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFont(Args.Obj).Size;
end;

{ property Write Size(Value: Integer) }

procedure TFont_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Size := Value;
end;

{ property Read Style: TFontStyles }

procedure TFont_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(byte(TFont(Args.Obj).Style));
end;

{ property Write Style(Value: TFontStyles) }

procedure TFont_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFont(Args.Obj).Style := TFontStyles(byte(V2S(Value)));
end;

{ TPen }

{ constructor Create }

procedure TPen_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPen.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TPen_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPen(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Handle: HPen }

procedure TPen_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := Integer(TPen(Args.Obj).Handle);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := P2V(TPen(Args.Obj).Handle);
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Handle(Value: HPen) }

procedure TPen_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TPen(Args.Obj).Handle := Value;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TPen(Args.Obj).Handle := V2P(Value);
  {$ENDIF COMPLIB_CLX}
end;

{ property Read Color: TColor }

procedure TPen_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPen(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TPen_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPen(Args.Obj).Color := Value;
end;

{ property Read Mode: TPenMode }

procedure TPen_Read_Mode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPen(Args.Obj).Mode;
end;

{ property Write Mode(Value: TPenMode) }

procedure TPen_Write_Mode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPen(Args.Obj).Mode := Value;
end;

{ property Read Style: TPenStyle }

procedure TPen_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPen(Args.Obj).Style;
end;

{ property Write Style(Value: TPenStyle) }

procedure TPen_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPen(Args.Obj).Style := Value;
end;

{ property Read Width: Integer }

procedure TPen_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPen(Args.Obj).Width;
end;

{ property Write Width(Value: Integer) }

procedure TPen_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPen(Args.Obj).Width := Value;
end;

{ TBrush }

{ constructor Create }

procedure TBrush_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBrush.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TBrush_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBrush(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Bitmap: TBitmap }

procedure TBrush_Read_Bitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBrush(Args.Obj).Bitmap);
end;

{ property Write Bitmap(Value: TBitmap) }

procedure TBrush_Write_Bitmap(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBrush(Args.Obj).Bitmap := V2O(Value) as TBitmap;
end;

{ property Read Handle: HBrush }

procedure TBrush_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := Integer(TBrush(Args.Obj).Handle);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := P2V(TBrush(Args.Obj).Handle);
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Handle(Value: HBrush) }

procedure TBrush_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TBrush(Args.Obj).Handle := Value;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TBrush(Args.Obj).Handle := V2P(Value);
  {$ENDIF COMPLIB_CLX}
end;

{ property Read Color: TColor }

procedure TBrush_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBrush(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TBrush_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBrush(Args.Obj).Color := Value;
end;

{ property Read Style: TBrushStyle }

procedure TBrush_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBrush(Args.Obj).Style;
end;

{ property Write Style(Value: TBrushStyle) }

procedure TBrush_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBrush(Args.Obj).Style := Value;
end;

{ TCanvas }

{ constructor Create }

procedure TCanvas_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCanvas.Create);
end;

{ procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); }

procedure TCanvas_Arc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Arc(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4], Args.Values[5],
    Args.Values[6], Args.Values[7]);
end;

{ procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap; const Source: TRect; Color: TColor); }

{$IFDEF COMPLIB_VCL}
procedure TCanvas_BrushCopy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).BrushCopy(Var2Rect(Args.Values[0]), V2O(Args.Values[1]) as TBitmap, Var2Rect(Args.Values[2]),
    Args.Values[3]);
end;
{$ENDIF COMPLIB_VCL}

{ procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); }

procedure TCanvas_Chord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Chord(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4],
    Args.Values[5], Args.Values[6], Args.Values[7]);
end;

{ procedure CopyRect(const Dest: TRect; Canvas: TCanvas; const Source: TRect); }

procedure TCanvas_CopyRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).CopyRect(Var2Rect(Args.Values[0]), V2O(Args.Values[1]) as TCanvas, Var2Rect(Args.Values[2]));
end;

{ procedure Draw(X, Y: Integer; Graphic: TGraphic); }

procedure TCanvas_Draw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Draw(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TGraphic);
end;

{ procedure DrawFocusRect(const Rect: TRect); }

procedure TCanvas_DrawFocusRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).DrawFocusRect(Var2Rect((Args.Values[0])));
end;

{ procedure Ellipse(X1, Y1, X2, Y2: Integer); }

procedure TCanvas_Ellipse(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Ellipse(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure FillRect(const Rect: TRect); }

procedure TCanvas_FillRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).FillRect(Var2Rect(Args.Values[0]));
end;

{$IFDEF COMPLIB_VCL}

{ procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle); }

procedure TCanvas_FloodFill(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).FloodFill(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure FrameRect(const Rect: TRect); }

procedure TCanvas_FrameRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).FrameRect(Var2Rect(Args.Values[0]));
end;

{$ENDIF COMPLIB_VCL}

{ procedure LineTo(X, Y: Integer); }

procedure TCanvas_LineTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).LineTo(Args.Values[0], Args.Values[1]);
end;

{ procedure Lock; }

{$IFDEF COMPILER3_UP}
procedure TCanvas_Lock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Lock;
end;
{$ENDIF COMPILER3_UP}

{ procedure MoveTo(X, Y: Integer); }

procedure TCanvas_MoveTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).MoveTo(Args.Values[0], Args.Values[1]);
end;

{ procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); }

procedure TCanvas_Pie(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Pie(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4], Args.Values[5],
    Args.Values[6], Args.Values[7]);
end;

{ procedure Polygon(const Points: array of TPoint); }

procedure TCanvas_Polygon(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  TCanvas(Args.Obj).Polygon(Args.Values[0]);
  NotImplemented('TCanvas.Polygon');
end;

{ procedure Polyline(const Points: array of TPoint); }

procedure TCanvas_Polyline(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  TCanvas(Args.Obj).Polyline(Args.Values[0]);
  NotImplemented('TCanvas.Polyline');
end;

{ procedure Rectangle(X1, Y1, X2, Y2: Integer); }

procedure TCanvas_Rectangle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Rectangle(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure Refresh; }

procedure TCanvas_Refresh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Refresh;
end;

{ procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); }

procedure TCanvas_RoundRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).RoundRect(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4],
    Args.Values[5]);
end;

{ procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); }

procedure TCanvas_StretchDraw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).StretchDraw(Var2Rect(Args.Values[0]), V2O(Args.Values[1]) as TGraphic);
end;

{ function TextExtent(const Text: string): TSize; }

procedure TCanvas_TextExtent(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TCanvas(Args.Obj).TextExtent(Args.Values[0]);
  NotImplemented('TCanvas.TextExtent');
end;

{ function TextHeight(const Text: string): Integer; }

procedure TCanvas_TextHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCanvas(Args.Obj).TextHeight(Args.Values[0]);
end;

{ procedure TextOut(X, Y: Integer; const Text: string); }

procedure TCanvas_TextOut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).TextOut(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string); }

procedure TCanvas_TextRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).TextRect(Var2Rect(Args.Values[0]), Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function TextWidth(const Text: string): Integer; }

procedure TCanvas_TextWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCanvas(Args.Obj).TextWidth(Args.Values[0]);
end;

{$IFDEF COMPILER3_UP}

{ function TryLock: Boolean; }

procedure TCanvas_TryLock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCanvas(Args.Obj).TryLock;
end;

{ procedure Unlock; }

procedure TCanvas_Unlock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Unlock;
end;

{$ENDIF COMPILER3_UP}

{ property Read ClipRect: TRect }

procedure TCanvas_Read_ClipRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TCanvas(Args.Obj).ClipRect);
end;

{ property Read Handle: HDC }

procedure TCanvas_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := Integer(TCanvas(Args.Obj).Handle);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := P2V(TCanvas(Args.Obj).Handle);
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Handle(Value: HDC) }

procedure TCanvas_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TCanvas(Args.Obj).Handle := Value;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TCanvas(Args.Obj).Handle := V2P(Value);
  {$ENDIF COMPLIB_CLX}
end;

{ property Read LockCount: Integer }

{$IFDEF COMPILER3_UP}
procedure TCanvas_Read_LockCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCanvas(Args.Obj).LockCount;
end;
{$ENDIF COMPILER3_UP}

{ property Read PenPos: TPoint }

procedure TCanvas_Read_PenPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TCanvas(Args.Obj).PenPos);
end;

{ property Write PenPos(Value: TPoint) }

procedure TCanvas_Write_PenPos(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).PenPos := Var2Point(Value);
end;

{ property Read Brush: TBrush }

procedure TCanvas_Read_Brush(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCanvas(Args.Obj).Brush);
end;

{ property Write Brush(Value: TBrush) }

procedure TCanvas_Write_Brush(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Brush := V2O(Value) as TBrush;
end;

{ property Read CopyMode: TCopyMode }

procedure TCanvas_Read_CopyMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCanvas(Args.Obj).CopyMode;
end;

{ property Write CopyMode(Value: TCopyMode) }

procedure TCanvas_Write_CopyMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).CopyMode := Value;
end;

{ property Read Font: TFont }

procedure TCanvas_Read_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCanvas(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }

procedure TCanvas_Write_Font(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Font := V2O(Value) as TFont;
end;

{ property Read Pen: TPen }

procedure TCanvas_Read_Pen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCanvas(Args.Obj).Pen);
end;

{ property Write Pen(Value: TPen) }

procedure TCanvas_Write_Pen(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCanvas(Args.Obj).Pen := V2O(Value) as TPen;
end;

{ TGraphic }

{ procedure LoadFromFile(const Filename: string); }

procedure TGraphic_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure SaveToFile(const Filename: string); }

procedure TGraphic_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TGraphic_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure SaveToStream(Stream: TStream); }

procedure TGraphic_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{$IFDEF COMPLIB_VCL}

{ procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); }

procedure TGraphic_LoadFromClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).LoadFromClipboardFormat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); }

procedure TGraphic_SaveToClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).SaveToClipboardFormat(Word(TVarData(Args.Values[0]).VSmallInt),
    THandle(TVarData(Args.Values[1]).VInteger), HPALETTE(TVarData(Args.Values[2]).VInteger));
end;

{$ENDIF COMPLIB_VCL}

{ property Read Empty: Boolean }

procedure TGraphic_Read_Empty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).Empty;
end;

{ property Read Height: Integer }

procedure TGraphic_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).Height;
end;

{ property Write Height(Value: Integer) }

procedure TGraphic_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).Height := Value;
end;

{ property Read Modified: Boolean }

procedure TGraphic_Read_Modified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).Modified;
end;

{ property Write Modified(Value: Boolean) }

procedure TGraphic_Write_Modified(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).Modified := Value;
end;

{$IFDEF COMPILER3_UP}
{$IFDEF COMPLIB_VCL}

{ property Read Palette: HPALETTE }

procedure TGraphic_Read_Palette(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TGraphic(Args.Obj).Palette);
end;

{ property Write Palette(Value: HPALETTE) }

procedure TGraphic_Write_Palette(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).Palette := Value;
end;

{ property Read PaletteModified: Boolean }

procedure TGraphic_Read_PaletteModified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).PaletteModified;
end;

{ property Write PaletteModified(Value: Boolean) }

procedure TGraphic_Write_PaletteModified(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).PaletteModified := Value;
end;

{$ENDIF COMPLIB_VCL}

{ property Read Transparent: Boolean }

procedure TGraphic_Read_Transparent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).Transparent;
end;

{ property Write Transparent(Value: Boolean) }

procedure TGraphic_Write_Transparent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).Transparent := Value;
end;

{$ENDIF COMPILER3_UP}

{ property Read Width: Integer }

procedure TGraphic_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGraphic(Args.Obj).Width;
end;

{ property Write Width(Value: Integer) }

procedure TGraphic_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TGraphic(Args.Obj).Width := Value;
end;

{ TPicture }

{ constructor Create }

procedure TPicture_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPicture.Create);
end;

{ procedure LoadFromFile(const Filename: string); }

procedure TPicture_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure SaveToFile(const Filename: string); }

procedure TPicture_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).SaveToFile(Args.Values[0]);
end;

{$IFDEF COMPLIB_VCL}

{ procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); }

procedure TPicture_LoadFromClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).LoadFromClipboardFormat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); }

procedure TPicture_SaveToClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).SaveToClipboardFormat(Word(TVarData(Args.Values[0]).VSmallInt),
    THandle(TVarData(Args.Values[1]).VInteger), HPALETTE(TVarData(Args.Values[2]).VInteger));
end;

{ function SupportsClipboardFormat(AFormat: Word): Boolean; }

procedure TPicture_SupportsClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPicture(Args.Obj).SupportsClipboardFormat(Args.Values[0]);
end;

{$ENDIF COMPLIB_VCL}

{ procedure Assign(Source: TPersistent); }

procedure TPicture_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Bitmap: TBitmap }

procedure TPicture_Read_Bitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPicture(Args.Obj).Bitmap);
end;

{ property Write Bitmap(Value: TBitmap) }

procedure TPicture_Write_Bitmap(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).Bitmap := V2O(Value) as TBitmap;
end;

{ property Read Graphic: TGraphic }

procedure TPicture_Read_Graphic(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPicture(Args.Obj).Graphic);
end;

{ property Write Graphic(Value: TGraphic) }

procedure TPicture_Write_Graphic(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).Graphic := V2O(Value) as TGraphic;
end;

{$IFDEF COMPILER3_UP}
{$IFDEF COMPLIB_VCL}

{ property Read PictureAdapter: IChangeNotifier }

procedure TPicture_Read_PictureAdapter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPicture(Args.Obj).PictureAdapter;
end;

{ property Write PictureAdapter(Value: IChangeNotifier) }

procedure TPicture_Write_PictureAdapter(const Value: Variant; Args: TJvInterpreterArgs);
begin
//  TPicture(Args.Obj).PictureAdapter := Value;
  NotImplemented('TPicture.PictureAdapter');
end;

{$ENDIF COMPLIB_VCL}
{$ENDIF COMPILER3_UP}

{ property Read Height: Integer }

procedure TPicture_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPicture(Args.Obj).Height;
end;

{ property Read Icon: TIcon }

procedure TPicture_Read_Icon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPicture(Args.Obj).Icon);
end;

{ property Write Icon(Value: TIcon) }

procedure TPicture_Write_Icon(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).Icon := V2O(Value) as TIcon;
end;

{$IFDEF COMPLIB_VCL}

{ property Read Metafile: TMetafile }

procedure TPicture_Read_Metafile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPicture(Args.Obj).Metafile);
end;

{ property Write Metafile(Value: TMetafile) }

procedure TPicture_Write_Metafile(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPicture(Args.Obj).Metafile := V2O(Value) as TMetafile;
end;

{$ENDIF COMPLIB_VCL}

{ property Read Width: Integer }

procedure TPicture_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPicture(Args.Obj).Width;
end;

{$IFDEF COMPLIB_VCL}

{ TMetafile }

{ constructor Create }

procedure TMetafile_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TMetafile.Create);
end;

{ procedure Clear; }

procedure TMetafile_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).Clear;
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TMetafile_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure SaveToFile(const Filename: String); }

procedure TMetafile_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure SaveToStream(Stream: TStream); }

procedure TMetafile_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); }

procedure TMetafile_LoadFromClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).LoadFromClipboardFormat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); }

procedure TMetafile_SaveToClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).SaveToClipboardFormat(Word(TVarData(Args.Values[0]).VSmallInt),
    THandle(TVarData(Args.Values[1]).VInteger), HPALETTE(TVarData(Args.Values[2]).VInteger));
end;

{ procedure Assign(Source: TPersistent); }

procedure TMetafile_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function ReleaseHandle: HENHMETAFILE; }

{$IFDEF COMPILER3_UP}
procedure TMetafile_ReleaseHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TMetafile(Args.Obj).ReleaseHandle);
end;
{$ENDIF COMPILER3_UP}

{ property Read CreatedBy: String }

procedure TMetafile_Read_CreatedBy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).CreatedBy;
end;

{ property Read Description: String }

procedure TMetafile_Read_Description(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).Description;
end;

{ property Read Enhanced: Boolean }

procedure TMetafile_Read_Enhanced(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).Enhanced;
end;

{ property Write Enhanced(Value: Boolean) }

procedure TMetafile_Write_Enhanced(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).Enhanced := Value;
end;

{ property Read Handle: HENHMETAFILE }

procedure TMetafile_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TMetafile(Args.Obj).Handle);
end;

{ property Write Handle(Value: HENHMETAFILE) }

procedure TMetafile_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).Handle := Value;
end;

{ property Read MMWidth: Integer }

procedure TMetafile_Read_MMWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).MMWidth;
end;

{ property Write MMWidth(Value: Integer) }

procedure TMetafile_Write_MMWidth(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).MMWidth := Value;
end;

{ property Read MMHeight: Integer }

procedure TMetafile_Read_MMHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).MMHeight;
end;

{ property Write MMHeight(Value: Integer) }

procedure TMetafile_Write_MMHeight(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).MMHeight := Value;
end;

{ property Read Inch: Word }

procedure TMetafile_Read_Inch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TMetafile(Args.Obj).Inch;
end;

{ property Write Inch(Value: Word) }

procedure TMetafile_Write_Inch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMetafile(Args.Obj).Inch := Value;
end;

{$ENDIF COMPLIB_VCL}

{ TBitmap }

{ constructor Create }

procedure TBitmap_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBitmap.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TBitmap_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure Dormant; }

procedure TBitmap_Dormant(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).Dormant;
end;

{ procedure FreeImage; }

procedure TBitmap_FreeImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).FreeImage;
end;

{ procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); }

{$IFDEF COMPLIB_VCL}
procedure TBitmap_LoadFromClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).LoadFromClipboardFormat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;
{$ENDIF COMPLIB_VCL}

{ procedure LoadFromStream(Stream: TStream); }

procedure TBitmap_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure LoadFromResourceName(Instance: THandle; const ResName: String); }

procedure TBitmap_LoadFromResourceName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).LoadFromResourceName(Args.Values[0], Args.Values[1]);
end;

{ procedure LoadFromResourceID(Instance: THandle; ResID: Integer); }

procedure TBitmap_LoadFromResourceID(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).LoadFromResourceID(Args.Values[0], Args.Values[1]);
end;

{ procedure Mask(TransparentColor: TColor); }

{$IFDEF COMPILER3_UP}
procedure TBitmap_Mask(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).Mask(Args.Values[0]);
end;
{$ENDIF COMPILER3_UP}

{$IFDEF COMPLIB_VCL}

{ function ReleaseHandle: HBITMAP; }

procedure TBitmap_ReleaseHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TBitmap(Args.Obj).ReleaseHandle);
end;

{ function ReleaseMaskHandle: HBITMAP; }

{$IFDEF COMPILER3_UP}
procedure TBitmap_ReleaseMaskHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TBitmap(Args.Obj).ReleaseMaskHandle);
end;
{$ENDIF COMPILER3_UP}

{ function ReleasePalette: HPALETTE; }

procedure TBitmap_ReleasePalette(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TBitmap(Args.Obj).ReleasePalette);
end;

{ procedure SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE); }

procedure TBitmap_SaveToClipboardFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).SaveToClipboardFormat(Word(TVarData(Args.Values[0]).VSmallInt),
    THandle(TVarData(Args.Values[1]).VInteger), HPALETTE(TVarData(Args.Values[2]).VInteger));
end;

{$ENDIF COMPLIB_VCL}

{ procedure SaveToStream(Stream: TStream); }

procedure TBitmap_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ property Read Canvas: TCanvas }

procedure TBitmap_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBitmap(Args.Obj).Canvas);
end;

{ property Read Handle: HBITMAP }

procedure TBitmap_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := Integer(TBitmap(Args.Obj).Handle);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := P2V(TBitmap(Args.Obj).Handle);
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Handle(Value: HBITMAP) }

procedure TBitmap_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TBitmap(Args.Obj).Handle := Value;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TBitmap(Args.Obj).Handle := V2P(Value);
  {$ENDIF COMPLIB_CLX}
end;

{$IFDEF COMPLIB_VCL}
{$IFDEF COMPILER3_UP}

{ property Read HandleType: TBitmapHandleType }

procedure TBitmap_Read_HandleType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).HandleType;
end;

{ property Write HandleType(Value: TBitmapHandleType) }

procedure TBitmap_Write_HandleType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).HandleType := Value;
end;

{$ENDIF COMPILER3_UP}

{ property Read IgnorePalette: Boolean }

procedure TBitmap_Read_IgnorePalette(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).IgnorePalette;
end;

{ property Write IgnorePalette(Value: Boolean) }

procedure TBitmap_Write_IgnorePalette(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).IgnorePalette := Value;
end;

{ property Read MaskHandle: HBITMAP }

{$IFDEF COMPILER3_UP}
procedure TBitmap_Read_MaskHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TBitmap(Args.Obj).MaskHandle);
end;
{$ENDIF COMPILER3_UP}

{$ENDIF COMPLIB_VCL}

{ property Read Monochrome: Boolean }

procedure TBitmap_Read_Monochrome(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).Monochrome;
end;

{ property Write Monochrome(Value: Boolean) }

procedure TBitmap_Write_Monochrome(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).Monochrome := Value;
end;

{$IFDEF COMPILER3_UP}

{ property Read PixelFormat: TPixelFormat }

procedure TBitmap_Read_PixelFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).PixelFormat;
end;

{ property Write PixelFormat(Value: TPixelFormat) }

procedure TBitmap_Write_PixelFormat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).PixelFormat := Value;
end;

{ property Read ScanLine[Integer]: Pointer }

procedure TBitmap_Read_ScanLine(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TBitmap(Args.Obj).ScanLine[Args.Values[0]]);
end;

{$ENDIF COMPILER3_UP}

{ property Read TransparentColor: TColor }

procedure TBitmap_Read_TransparentColor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).TransparentColor;
end;

{$IFDEF COMPILER3_UP}

{ property Write TransparentColor(Value: TColor) }

procedure TBitmap_Write_TransparentColor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).TransparentColor := Value;
end;

{ property Read TransparentMode: TTransparentMode }

procedure TBitmap_Read_TransparentMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitmap(Args.Obj).TransparentMode;
end;

{ property Write TransparentMode(Value: TTransparentMode) }

procedure TBitmap_Write_TransparentMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitmap(Args.Obj).TransparentMode := Value;
end;

{$ENDIF COMPILER3_UP}

{ TIcon }

{ constructor Create }

procedure TIcon_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIcon.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TIcon_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIcon(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TIcon_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIcon(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ function ReleaseHandle: HICON; }

{$IFDEF COMPLIB_VCL}
procedure TIcon_ReleaseHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TIcon(Args.Obj).ReleaseHandle);
end;
{$ENDIF COMPLIB_VCL}

{ procedure SaveToStream(Stream: TStream); }

procedure TIcon_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIcon(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ property Read Handle: HICON }

procedure TIcon_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TIcon(Args.Obj).Handle);
end;

{ property Write Handle(Value: HICON) }

{$IFDEF COMPLIB_VCL}
procedure TIcon_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIcon(Args.Obj).Handle := Value;
end;
{$ENDIF COMPLIB_VCL}

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cGraphics = 'Graphics';
begin
  with JvInterpreterAdapter do
  begin
    { TFontStyle }
    AddConst(cGraphics, 'fsBold', Integer(fsBold));
    AddConst(cGraphics, 'fsItalic', Integer(fsItalic));
    AddConst(cGraphics, 'fsUnderline', Integer(fsUnderline));
    AddConst(cGraphics, 'fsStrikeOut', Integer(fsStrikeOut));
    { TFontPitch }
    AddConst(cGraphics, 'fpDefault', Integer(fpDefault));
    AddConst(cGraphics, 'fpVariable', Integer(fpVariable));
    AddConst(cGraphics, 'fpFixed', Integer(fpFixed));
    { TPenStyle }
    AddConst(cGraphics, 'psSolid', Integer(psSolid));
    AddConst(cGraphics, 'psDash', Integer(psDash));
    AddConst(cGraphics, 'psDot', Integer(psDot));
    AddConst(cGraphics, 'psDashDot', Integer(psDashDot));
    AddConst(cGraphics, 'psDashDotDot', Integer(psDashDotDot));
    AddConst(cGraphics, 'psClear', Integer(psClear));
    {$IFDEF COMPLIB_VCL}
    AddConst(cGraphics, 'psInsideFrame', Integer(psInsideFrame));
    {$ENDIF COMPLIB_VCL}
    { TPenMode }
    AddConst(cGraphics, 'pmBlack', Integer(pmBlack));
    AddConst(cGraphics, 'pmWhite', Integer(pmWhite));
    AddConst(cGraphics, 'pmNop', Integer(pmNop));
    AddConst(cGraphics, 'pmNot', Integer(pmNot));
    AddConst(cGraphics, 'pmCopy', Integer(pmCopy));
    AddConst(cGraphics, 'pmNotCopy', Integer(pmNotCopy));
    AddConst(cGraphics, 'pmMergePenNot', Integer(pmMergePenNot));
    AddConst(cGraphics, 'pmMaskPenNot', Integer(pmMaskPenNot));
    AddConst(cGraphics, 'pmMergeNotPen', Integer(pmMergeNotPen));
    AddConst(cGraphics, 'pmMaskNotPen', Integer(pmMaskNotPen));
    AddConst(cGraphics, 'pmMerge', Integer(pmMerge));
    AddConst(cGraphics, 'pmNotMerge', Integer(pmNotMerge));
    AddConst(cGraphics, 'pmMask', Integer(pmMask));
    AddConst(cGraphics, 'pmNotMask', Integer(pmNotMask));
    AddConst(cGraphics, 'pmXor', Integer(pmXor));
    AddConst(cGraphics, 'pmNotXor', Integer(pmNotXor));
    { TBrushStyle }
    AddConst(cGraphics, 'bsSolid', Integer(bsSolid));
    AddConst(cGraphics, 'bsClear', Integer(bsClear));
    AddConst(cGraphics, 'bsHorizontal', Integer(bsHorizontal));
    AddConst(cGraphics, 'bsVertical', Integer(bsVertical));
    AddConst(cGraphics, 'bsFDiagonal', Integer(bsFDiagonal));
    AddConst(cGraphics, 'bsBDiagonal', Integer(bsBDiagonal));
    AddConst(cGraphics, 'bsCross', Integer(bsCross));
    AddConst(cGraphics, 'bsDiagCross', Integer(bsDiagCross));
    { TFont }
    AddClass(cGraphics, TFont, 'TFont');
    AddGet(TFont, 'Create', TFont_Create, 0, [0], varEmpty);
    AddGet(TFont, 'Assign', TFont_Assign, 1, [varEmpty], varEmpty);
    AddGet(TFont, 'Handle', TFont_Read_Handle, 0, [0], varEmpty);
    AddSet(TFont, 'Handle', TFont_Write_Handle, 0, [0]);
    AddGet(TFont, 'PixelsPerInch', TFont_Read_PixelsPerInch, 0, [0], varEmpty);
    AddSet(TFont, 'PixelsPerInch', TFont_Write_PixelsPerInch, 0, [0]);
    {$IFDEF COMPILER3_UP}
    AddGet(TFont, 'Charset', TFont_Read_Charset, 0, [0], varEmpty);
    AddSet(TFont, 'Charset', TFont_Write_Charset, 0, [0]);
    {$ENDIF COMPILER3_UP}
    AddGet(TFont, 'Color', TFont_Read_Color, 0, [0], varEmpty);
    AddSet(TFont, 'Color', TFont_Write_Color, 0, [0]);
    AddGet(TFont, 'Height', TFont_Read_Height, 0, [0], varEmpty);
    AddSet(TFont, 'Height', TFont_Write_Height, 0, [0]);
    AddGet(TFont, 'Name', TFont_Read_Name, 0, [0], varEmpty);
    AddSet(TFont, 'Name', TFont_Write_Name, 0, [0]);
    AddGet(TFont, 'Pitch', TFont_Read_Pitch, 0, [0], varEmpty);
    AddSet(TFont, 'Pitch', TFont_Write_Pitch, 0, [0]);
    AddGet(TFont, 'Size', TFont_Read_Size, 0, [0], varEmpty);
    AddSet(TFont, 'Size', TFont_Write_Size, 0, [0]);
    AddGet(TFont, 'Style', TFont_Read_Style, 0, [0], varEmpty);
    AddSet(TFont, 'Style', TFont_Write_Style, 0, [0]);
    { TPen }
    AddClass(cGraphics, TPen, 'TPen');
    AddGet(TPen, 'Create', TPen_Create, 0, [0], varEmpty);
    AddGet(TPen, 'Assign', TPen_Assign, 1, [varEmpty], varEmpty);
    AddGet(TPen, 'Handle', TPen_Read_Handle, 0, [0], varEmpty);
    AddSet(TPen, 'Handle', TPen_Write_Handle, 0, [0]);
    AddGet(TPen, 'Color', TPen_Read_Color, 0, [0], varEmpty);
    AddSet(TPen, 'Color', TPen_Write_Color, 0, [0]);
    AddGet(TPen, 'Mode', TPen_Read_Mode, 0, [0], varEmpty);
    AddSet(TPen, 'Mode', TPen_Write_Mode, 0, [0]);
    AddGet(TPen, 'Style', TPen_Read_Style, 0, [0], varEmpty);
    AddSet(TPen, 'Style', TPen_Write_Style, 0, [0]);
    AddGet(TPen, 'Width', TPen_Read_Width, 0, [0], varEmpty);
    AddSet(TPen, 'Width', TPen_Write_Width, 0, [0]);
    { TBrush }
    AddClass(cGraphics, TBrush, 'TBrush');
    AddGet(TBrush, 'Create', TBrush_Create, 0, [0], varEmpty);
    AddGet(TBrush, 'Assign', TBrush_Assign, 1, [varEmpty], varEmpty);
    AddGet(TBrush, 'Bitmap', TBrush_Read_Bitmap, 0, [0], varEmpty);
    AddSet(TBrush, 'Bitmap', TBrush_Write_Bitmap, 0, [0]);
    AddGet(TBrush, 'Handle', TBrush_Read_Handle, 0, [0], varEmpty);
    AddSet(TBrush, 'Handle', TBrush_Write_Handle, 0, [0]);
    AddGet(TBrush, 'Color', TBrush_Read_Color, 0, [0], varEmpty);
    AddSet(TBrush, 'Color', TBrush_Write_Color, 0, [0]);
    AddGet(TBrush, 'Style', TBrush_Read_Style, 0, [0], varEmpty);
    AddSet(TBrush, 'Style', TBrush_Write_Style, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    { TFillStyle }
    AddConst(cGraphics, 'fsSurface', Integer(fsSurface));
    AddConst(cGraphics, 'fsBorder', Integer(fsBorder));
    { TFillMode }
    AddConst(cGraphics, 'fmAlternate', Integer(fmAlternate));
    AddConst(cGraphics, 'fmWinding', Integer(fmWinding));
    {$ENDIF COMPLIB_VCL}
    { TCanvasStates }
    AddConst(cGraphics, 'csHandleValid', Integer(csHandleValid));
    AddConst(cGraphics, 'csFontValid', Integer(csFontValid));
    AddConst(cGraphics, 'csPenValid', Integer(csPenValid));
    AddConst(cGraphics, 'csBrushValid', Integer(csBrushValid));
    { TCanvas }
    AddClass(cGraphics, TCanvas, 'TCanvas');
    AddGet(TCanvas, 'Create', TCanvas_Create, 0, [0], varEmpty);
    AddGet(TCanvas, 'Arc', TCanvas_Arc, 8, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TCanvas, 'BrushCopy', TCanvas_BrushCopy, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TCanvas, 'Chord', TCanvas_Chord, 8, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    AddGet(TCanvas, 'CopyRect', TCanvas_CopyRect, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'Draw', TCanvas_Draw, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'DrawFocusRect', TCanvas_DrawFocusRect, 1, [varEmpty], varEmpty);
    AddGet(TCanvas, 'Ellipse', TCanvas_Ellipse, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'FillRect', TCanvas_FillRect, 1, [varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TCanvas, 'FloodFill', TCanvas_FloodFill, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'FrameRect', TCanvas_FrameRect, 1, [varEmpty], varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TCanvas, 'LineTo', TCanvas_LineTo, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddGet(TCanvas, 'Lock', TCanvas_Lock, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TCanvas, 'MoveTo', TCanvas_MoveTo, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'Pie', TCanvas_Pie, 8, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    AddGet(TCanvas, 'Polygon', TCanvas_Polygon, 1, [varEmpty], varEmpty);
    AddGet(TCanvas, 'Polyline', TCanvas_Polyline, 1, [varEmpty], varEmpty);
    AddGet(TCanvas, 'Rectangle', TCanvas_Rectangle, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'Refresh', TCanvas_Refresh, 0, [0], varEmpty);
    AddGet(TCanvas, 'RoundRect', TCanvas_RoundRect, 6, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCanvas, 'StretchDraw', TCanvas_StretchDraw, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'TextExtent', TCanvas_TextExtent, 1, [varEmpty], varEmpty);
    AddGet(TCanvas, 'TextHeight', TCanvas_TextHeight, 1, [varEmpty], varEmpty);
    AddGet(TCanvas, 'TextOut', TCanvas_TextOut, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'TextRect', TCanvas_TextRect, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCanvas, 'TextWidth', TCanvas_TextWidth, 1, [varEmpty], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddGet(TCanvas, 'TryLock', TCanvas_TryLock, 0, [0], varEmpty);
    AddGet(TCanvas, 'Unlock', TCanvas_Unlock, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TCanvas, 'ClipRect', TCanvas_Read_ClipRect, 0, [0], varEmpty);
    AddGet(TCanvas, 'Handle', TCanvas_Read_Handle, 0, [0], varEmpty);
    AddSet(TCanvas, 'Handle', TCanvas_Write_Handle, 0, [0]);
    {$IFDEF COMPILER3_UP}
    AddGet(TCanvas, 'LockCount', TCanvas_Read_LockCount, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TCanvas, 'PenPos', TCanvas_Read_PenPos, 0, [0], varEmpty);
    AddSet(TCanvas, 'PenPos', TCanvas_Write_PenPos, 0, [0]);
    AddGet(TCanvas, 'Brush', TCanvas_Read_Brush, 0, [0], varEmpty);
    AddSet(TCanvas, 'Brush', TCanvas_Write_Brush, 0, [0]);
    AddGet(TCanvas, 'CopyMode', TCanvas_Read_CopyMode, 0, [0], varEmpty);
    AddSet(TCanvas, 'CopyMode', TCanvas_Write_CopyMode, 0, [0]);
    AddGet(TCanvas, 'Font', TCanvas_Read_Font, 0, [0], varEmpty);
    AddSet(TCanvas, 'Font', TCanvas_Write_Font, 0, [0]);
    AddGet(TCanvas, 'Pen', TCanvas_Read_Pen, 0, [0], varEmpty);
    AddSet(TCanvas, 'Pen', TCanvas_Write_Pen, 0, [0]);
    {$IFDEF COMPILER3_UP}
    { TProgressStage }
    AddConst(cGraphics, 'psStarting', Integer(psStarting));
    AddConst(cGraphics, 'psRunning', Integer(psRunning));
    AddConst(cGraphics, 'psEnding', Integer(psEnding));
    {$ENDIF COMPILER3_UP}
    { TGraphic }
    AddClass(cGraphics, TGraphic, 'TGraphic');
    AddGet(TGraphic, 'LoadFromFile', TGraphic_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TGraphic, 'SaveToFile', TGraphic_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TGraphic, 'LoadFromStream', TGraphic_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TGraphic, 'SaveToStream', TGraphic_SaveToStream, 1, [varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TGraphic, 'LoadFromClipboardFormat', TGraphic_LoadFromClipboardFormat, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TGraphic, 'SaveToClipboardFormat', TGraphic_SaveToClipboardFormat, 3, [varByRef, varByRef, varByRef],
      varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TGraphic, 'Empty', TGraphic_Read_Empty, 0, [0], varEmpty);
    AddGet(TGraphic, 'Height', TGraphic_Read_Height, 0, [0], varEmpty);
    AddSet(TGraphic, 'Height', TGraphic_Write_Height, 0, [0]);
    AddGet(TGraphic, 'Modified', TGraphic_Read_Modified, 0, [0], varEmpty);
    AddSet(TGraphic, 'Modified', TGraphic_Write_Modified, 0, [0]);
    {$IFDEF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    AddGet(TGraphic, 'Palette', TGraphic_Read_Palette, 0, [0], varEmpty);
    AddSet(TGraphic, 'Palette', TGraphic_Write_Palette, 0, [0]);
    AddGet(TGraphic, 'PaletteModified', TGraphic_Read_PaletteModified, 0, [0], varEmpty);
    AddSet(TGraphic, 'PaletteModified', TGraphic_Write_PaletteModified, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TGraphic, 'Transparent', TGraphic_Read_Transparent, 0, [0], varEmpty);
    AddSet(TGraphic, 'Transparent', TGraphic_Write_Transparent, 0, [0]);
    {$ENDIF COMPILER3_UP}
    AddGet(TGraphic, 'Width', TGraphic_Read_Width, 0, [0], varEmpty);
    AddSet(TGraphic, 'Width', TGraphic_Write_Width, 0, [0]);
    { TPicture }
    AddClass(cGraphics, TPicture, 'TPicture');
    AddGet(TPicture, 'Create', TPicture_Create, 0, [0], varEmpty);
    AddGet(TPicture, 'LoadFromFile', TPicture_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TPicture, 'SaveToFile', TPicture_SaveToFile, 1, [varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TPicture, 'LoadFromClipboardFormat', TPicture_LoadFromClipboardFormat, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TPicture, 'SaveToClipboardFormat', TPicture_SaveToClipboardFormat, 3, [varByRef, varByRef, varByRef],
      varEmpty);
    AddGet(TPicture, 'SupportsClipboardFormat', TPicture_SupportsClipboardFormat, 1, [varEmpty], varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TPicture, 'Assign', TPicture_Assign, 1, [varEmpty], varEmpty);
    AddGet(TPicture, 'Bitmap', TPicture_Read_Bitmap, 0, [0], varEmpty);
    AddSet(TPicture, 'Bitmap', TPicture_Write_Bitmap, 0, [0]);
    AddGet(TPicture, 'Graphic', TPicture_Read_Graphic, 0, [0], varEmpty);
    AddSet(TPicture, 'Graphic', TPicture_Write_Graphic, 0, [0]);
    {$IFDEF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    AddGet(TPicture, 'PictureAdapter', TPicture_Read_PictureAdapter, 0, [0], varEmpty);
    AddSet(TPicture, 'PictureAdapter', TPicture_Write_PictureAdapter, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER3_UP}
    AddGet(TPicture, 'Height', TPicture_Read_Height, 0, [0], varEmpty);
    AddGet(TPicture, 'Icon', TPicture_Read_Icon, 0, [0], varEmpty);
    AddSet(TPicture, 'Icon', TPicture_Write_Icon, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    AddGet(TPicture, 'Metafile', TPicture_Read_Metafile, 0, [0], varEmpty);
    AddSet(TPicture, 'Metafile', TPicture_Write_Metafile, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TPicture, 'Width', TPicture_Read_Width, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    { TMetafile }
    AddClass(cGraphics, TMetafile, 'TMetafile');
    AddGet(TMetafile, 'Create', TMetafile_Create, 0, [0], varEmpty);
    AddGet(TMetafile, 'Clear', TMetafile_Clear, 0, [0], varEmpty);
    AddGet(TMetafile, 'LoadFromStream', TMetafile_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TMetafile, 'SaveToFile', TMetafile_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TMetafile, 'SaveToStream', TMetafile_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TMetafile, 'LoadFromClipboardFormat', TMetafile_LoadFromClipboardFormat, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TMetafile, 'SaveToClipboardFormat', TMetafile_SaveToClipboardFormat, 3, [varByRef, varByRef, varByRef],
      varEmpty);
    AddGet(TMetafile, 'Assign', TMetafile_Assign, 1, [varEmpty], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddGet(TMetafile, 'ReleaseHandle', TMetafile_ReleaseHandle, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TMetafile, 'CreatedBy', TMetafile_Read_CreatedBy, 0, [0], varEmpty);
    AddGet(TMetafile, 'Description', TMetafile_Read_Description, 0, [0], varEmpty);
    AddGet(TMetafile, 'Enhanced', TMetafile_Read_Enhanced, 0, [0], varEmpty);
    AddSet(TMetafile, 'Enhanced', TMetafile_Write_Enhanced, 0, [0]);
    AddGet(TMetafile, 'Handle', TMetafile_Read_Handle, 0, [0], varEmpty);
    AddSet(TMetafile, 'Handle', TMetafile_Write_Handle, 0, [0]);
    AddGet(TMetafile, 'MMWidth', TMetafile_Read_MMWidth, 0, [0], varEmpty);
    AddSet(TMetafile, 'MMWidth', TMetafile_Write_MMWidth, 0, [0]);
    AddGet(TMetafile, 'MMHeight', TMetafile_Read_MMHeight, 0, [0], varEmpty);
    AddSet(TMetafile, 'MMHeight', TMetafile_Write_MMHeight, 0, [0]);
    AddGet(TMetafile, 'Inch', TMetafile_Read_Inch, 0, [0], varEmpty);
    AddSet(TMetafile, 'Inch', TMetafile_Write_Inch, 0, [0]);
    {$IFDEF COMPILER3_UP}
    {$ENDIF COMPLIB_VCL}
    { TBitmapHandleType }
    {$IFDEF COMPILER3_UP}
    AddConst(cGraphics, 'bmDIB', Integer(bmDIB));
    AddConst(cGraphics, 'bmDDB', Integer(bmDDB));
    { TPixelFormat }
    AddConst(cGraphics, 'pfDevice', Integer(pfDevice));
    AddConst(cGraphics, 'pf1bit', Integer(pf1bit));
    AddConst(cGraphics, 'pf4bit', Integer(pf4bit));
    AddConst(cGraphics, 'pf8bit', Integer(pf8bit));
    AddConst(cGraphics, 'pf15bit', Integer(pf15bit));
    AddConst(cGraphics, 'pf16bit', Integer(pf16bit));
    AddConst(cGraphics, 'pf24bit', Integer(pf24bit));
    AddConst(cGraphics, 'pf32bit', Integer(pf32bit));
    AddConst(cGraphics, 'pfCustom', Integer(pfCustom));
    { TTransparentMode }
    AddConst(cGraphics, 'tmAuto', Integer(tmAuto));
    AddConst(cGraphics, 'tmFixed', Integer(tmFixed));
    {$ENDIF COMPILER3_UP}
    { TBitmap }
    {$ENDIF COMPILER3_UP}
    AddClass(cGraphics, TBitmap, 'TBitmap');
    AddGet(TBitmap, 'Create', TBitmap_Create, 0, [0], varEmpty);
    AddGet(TBitmap, 'Assign', TBitmap_Assign, 1, [varEmpty], varEmpty);
    AddGet(TBitmap, 'Dormant', TBitmap_Dormant, 0, [0], varEmpty);
    AddGet(TBitmap, 'FreeImage', TBitmap_FreeImage, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TBitmap, 'LoadFromClipboardFormat', TBitmap_LoadFromClipboardFormat, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TBitmap, 'LoadFromStream', TBitmap_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TBitmap, 'LoadFromResourceName', TBitmap_LoadFromResourceName, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBitmap, 'LoadFromResourceID', TBitmap_LoadFromResourceID, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddGet(TBitmap, 'Mask', TBitmap_Mask, 1, [varEmpty], varEmpty);
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    AddGet(TBitmap, 'ReleaseHandle', TBitmap_ReleaseHandle, 0, [0], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddGet(TBitmap, 'ReleaseMaskHandle', TBitmap_ReleaseMaskHandle, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TBitmap, 'ReleasePalette', TBitmap_ReleasePalette, 0, [0], varEmpty);
    AddGet(TBitmap, 'SaveToClipboardFormat', TBitmap_SaveToClipboardFormat, 3, [varByRef, varByRef, varByRef],
      varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TBitmap, 'SaveToStream', TBitmap_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TBitmap, 'Canvas', TBitmap_Read_Canvas, 0, [0], varEmpty);
    AddGet(TBitmap, 'Handle', TBitmap_Read_Handle, 0, [0], varEmpty);
    AddSet(TBitmap, 'Handle', TBitmap_Write_Handle, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    {$IFDEF COMPILER3_UP}
    AddGet(TBitmap, 'HandleType', TBitmap_Read_HandleType, 0, [0], varEmpty);
    AddSet(TBitmap, 'HandleType', TBitmap_Write_HandleType, 0, [0]);
    {$ENDIF COMPILER3_UP}
    AddGet(TBitmap, 'IgnorePalette', TBitmap_Read_IgnorePalette, 0, [0], varEmpty);
    AddSet(TBitmap, 'IgnorePalette', TBitmap_Write_IgnorePalette, 0, [0]);
    {$IFDEF COMPILER3_UP}
    AddGet(TBitmap, 'MaskHandle', TBitmap_Read_MaskHandle, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    {$ENDIF COMPLIB_VCL}
    AddGet(TBitmap, 'Monochrome', TBitmap_Read_Monochrome, 0, [0], varEmpty);
    AddSet(TBitmap, 'Monochrome', TBitmap_Write_Monochrome, 0, [0]);
    {$IFDEF COMPILER3_UP}
    AddGet(TBitmap, 'PixelFormat', TBitmap_Read_PixelFormat, 0, [0], varEmpty);
    AddSet(TBitmap, 'PixelFormat', TBitmap_Write_PixelFormat, 0, [0]);
    AddGet(TBitmap, 'ScanLine', TBitmap_Read_ScanLine, 1, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddGet(TBitmap, 'TransparentColor', TBitmap_Read_TransparentColor, 0, [0], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddSet(TBitmap, 'TransparentColor', TBitmap_Write_TransparentColor, 0, [0]);
    AddGet(TBitmap, 'TransparentMode', TBitmap_Read_TransparentMode, 0, [0], varEmpty);
    AddSet(TBitmap, 'TransparentMode', TBitmap_Write_TransparentMode, 0, [0]);
    {$ENDIF COMPILER3_UP}
    { TIcon }
    AddClass(cGraphics, TIcon, 'TIcon');
    AddGet(TIcon, 'Create', TIcon_Create, 0, [0], varEmpty);
    AddGet(TIcon, 'Assign', TIcon_Assign, 1, [varEmpty], varEmpty);
    AddGet(TIcon, 'LoadFromStream', TIcon_LoadFromStream, 1, [varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TIcon, 'ReleaseHandle', TIcon_ReleaseHandle, 0, [0], varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TIcon, 'SaveToStream', TIcon_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TIcon, 'Handle', TIcon_Read_Handle, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddSet(TIcon, 'Handle', TIcon_Write_Handle, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    { TFontData }
  end;
end;

end.

