{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDBGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgDBGrid;

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls, Grids, DBGrids, ImgList,
  JVCLVer;

type

  TJvgDBGrid = class(TDBGrid)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCaptionTextAlignment: TAlignment;
    FAutoColumnSize: Boolean;
    FCaptionHeight: Integer;
    FBitmap: TBitmap;
    FBmp: TBitmap;
    FImage: TImage;
    FGlyphs: TImageList;
    FSingleGlyph: Boolean;
    FGlyphsChangeLink: TChangeLink;
    FGlyph: TBitmap;
    FFixedWidthCols: Integer;
    FRecalculateWidth: Boolean;
    procedure SetCaptionTextAlignment(Value: TAlignment);
    procedure SetCaptionHeight(Value: Integer);
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetGlyphs(Value: TImageList);
    procedure SetSingleGlyph(Value: Boolean);
    procedure SetAutoColumnSize(const Value: Boolean);
    procedure UpdateSize;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure GlyphsListChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoColumnSize: Boolean read FAutoColumnSize write SetAutoColumnSize default True;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 17;
    property CaptionTextAlignment: TAlignment read FCaptionTextAlignment write SetCaptionTextAlignment default taCenter;
    property FixedWidthCols: Integer read FFixedWidthCols write FFixedWidthCols;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property Image: TImage read FImage write SetImage;
    property SingleGlyph: Boolean read FSingleGlyph write SetSingleGlyph default False;
  end;

implementation

uses
  Math,
  JvgTypes, JvgUtils;

constructor TJvgDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionTextAlignment := taCenter;
  FCaptionHeight := 17;
  FAutoColumnSize := True;
  FSingleGlyph := False;
  FGlyph := TBitmap.Create;
  FGlyphsChangeLink := TChangeLink.Create;
  FGlyphsChangeLink.OnChange := GlyphsListChanged;
end;

destructor TJvgDBGrid.Destroy;
begin
  FBitmap.Free;
  FGlyphsChangeLink.Free;
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvgDBGrid.Loaded;
begin
  inherited Loaded;
  if Assigned(FBitmap) and (not FBitmap.Empty) then
    FBmp := FBitmap;
  RowHeights[0] := FCaptionHeight;
  if AutoColumnSize then
    UpdateSize;
end;

procedure TJvgDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  cAlignments: array [TAlignment] of Longint = (ES_LEFT, ES_RIGHT, ES_CENTER);
var
  R: TRect;
  Str: string;
  CaptionHeight_: Integer;
  X, X1, Y, Y1, IHeight, IWidth, Index: Integer;
begin
  if (ARow > 0) and (ACol > 0) then
  begin
    inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;
  R := ARect;

  if IsItAFilledBitmap(FBmp) then
  begin
    X := R.Left;
    Y := R.Top;
    //    IHeight := R.Bottom - R.Top;
    IWidth := R.Right - R.Left;
    X1 := X;
    Y1 := Y;
    while X1 < R.Right do
    begin
      if X1 + IWidth > R.Right then
        IWidth := R.Right - X1;
      while Y1 < R.Bottom do
      begin
        IHeight := R.Bottom - R.Top;
        if Y1 + IHeight > R.Bottom then
          IHeight := R.Bottom - Y1;
        BitBlt(Canvas.Handle, X1, Y1, Min(IWidth, FBmp.Width), Min(IHeight, FBmp.Height),
          FBmp.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(Y1, Min(IHeight, FBmp.Height));
      end;
      Inc(X1, Min(IWidth, FBmp.Width));
      Y1 := Y;
    end;

    if ACol = 0 then
    begin
      DrawBoxEx(Canvas.Handle, ARect, ALLGLSIDES, bvNone, bvRaised, False, 0, True);
      Exit;
    end;
  end;

  if (ARow <> 0) or (ACol < 1) then
  begin
    inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;

  Str := Columns[ACol - 1].Title.Caption;

  Canvas.Font := Columns[ACol - 1].Title.Font;
  InflateRect(ARect, -1, -1);
  R := ARect;
  InflateRect(R, 1, 1);
  DrawBoxEx(Canvas.Handle, R, ALLGLSIDES, bvNone, bvRaised, False,
    Columns[ACol - 1].Title.Color, IsItAFilledBitmap(FBmp) or
      (Columns[ACol - 1].Title.Color = Color));

  if Assigned(FGlyphs) then
  begin
    if FSingleGlyph then
      Index := 0
    else
      Index := ACol - 1;
    if Index < FGlyphs.Count then
    begin
      FGlyphs.GetBitmap(Index, FGlyph);
      CreateBitmapExt(Canvas.Handle, FGlyph,
        R, 2, Max(0, (R.Bottom - R.Top - FGlyph.Height) div 2),
        fwoNone, fdsDefault, True,
        GetTransparentColor(FGlyph, ftcLeftBottomPixel), 0);
      Inc(ARect.Left, FGlyph.Width);
      R := ARect;
    end;
  end;

  SetBkMode(Canvas.Handle, TRANSPARENT);
  DrawText(Canvas.Handle, PChar(Str), -1, R,
    cAlignments[FCaptionTextAlignment] or DT_WORDBREAK or DT_CALCRECT);

  if FCaptionHeight < 0 then
    CaptionHeight_ := R.Bottom - R.Top
  else
    CaptionHeight_ := FCaptionHeight;

  RowHeights[0] := CaptionHeight_;

  ARect.Top := ARect.Top + Max(0, (ARect.Bottom - R.Bottom) div 2);
  DrawText(Canvas.Handle, PChar(Str), -1, ARect,
    cAlignments[FCaptionTextAlignment] or DT_WORDBREAK);
end;

procedure TJvgDBGrid.GlyphsListChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJvgDBGrid.SetCaptionTextAlignment(Value: TAlignment);
begin
  if FCaptionTextAlignment <> Value then
  begin
    FCaptionTextAlignment := Value;
    Repaint;
  end;
end;

procedure TJvgDBGrid.SetCaptionHeight(Value: Integer);
begin
  FCaptionHeight := Value;
  if FCaptionHeight >= 0 then
    RowHeights[0] := FCaptionHeight
  else
    Repaint;
end;

function TJvgDBGrid.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TJvgDBGrid.SetBitmap(Value: TBitmap);
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    FBmp := FBitmap
  else
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
    FBmp := nil;
  Invalidate;
end;

procedure TJvgDBGrid.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
  if Assigned(FBitmap) then
    FBmp := FBitmap
  else
    FBmp := nil;
  Invalidate;
end;

procedure TJvgDBGrid.SetGlyphs(Value: TImageList);
begin
  if Assigned(FGlyphs) then
    FGlyphs.UnregisterChanges(FGlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
    FGlyphs.RegisterChanges(FGlyphsChangeLink);
end;

procedure TJvgDBGrid.SetSingleGlyph(Value: Boolean);
begin
  FSingleGlyph := Value;
  Repaint;
end;

procedure TJvgDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Assigned(Glyphs) and (AComponent = Glyphs) and (Operation = opRemove) then
    Glyphs := nil;
  if Assigned(Image) and (AComponent = Image) and (Operation = opRemove) then
    Image := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvgDBGrid.UpdateSize;
var
  Msg: TWMSize;
begin
  WMSize(Msg);
end;

procedure TJvgDBGrid.SetAutoColumnSize(const Value: Boolean);
begin
  if FAutoColumnSize <> Value then
  begin
    FAutoColumnSize := Value;
    UpdateSize;
  end;
end;

procedure TJvgDBGrid.WMSize(var Msg: TWMSize);
var
  I, TotalWidth, FreeClientWidth: Integer;
begin
  if Msg.Msg <> 0 then
    inherited;

  if FRecalculateWidth then
    Exit;
  FRecalculateWidth := True;
  try
    if (not AutoColumnSize) or (Width = 0) then
      Exit;
    TotalWidth := 0;
    for I := FixedWidthCols to Columns.Count - 1 do
      Inc(TotalWidth, Columns[I].Width + 1);
    FreeClientWidth := ClientWidth - 2;
    if dgIndicator in Options then
      Dec(FreeClientWidth, 10);
    Dec(FreeClientWidth, Columns.Count);
    if ScrollBars in [ssVertical, ssBoth] then
      Dec(FreeClientWidth, GetSystemMetrics(SM_CXHSCROLL) + 2);

    for I := 0 to FixedWidthCols - 1 do
      Dec(FreeClientWidth, Columns[I].Width);

    for I := FixedWidthCols to Columns.Count - 1 do
      Columns[I].Width := MulDiv(Columns[I].Width, FreeClientWidth, TotalWidth) - 1;
  finally
    FRecalculateWidth := False;
  end;
end;

end.

