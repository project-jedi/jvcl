{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDBGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Rob den Braasem [rbraasem@xs4all.nl]

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgDBGrid;

interface
uses
  Windows, Messages, Classes, Controls, Graphics, JvgTypes, JvgCommClasses,
  JvgUtils, StdCtrls, ExtCtrls, grids, dbgrids, JVCLVer, ImgList;

type

  TJvgDBGrid = class(TDBGrid)
  private
    FAlignment: TAlignment;
    FAutoColumnSize: boolean;
    FCaptionHeight: integer;
    FBitmap, bmp: TBitmap;
    FImage: TImage;
    FGlyphs: TImageList;
    FSingleGlyph: boolean;

    GlyphsChangeLink: TChangeLink;
    Glyph: TBitmap;
    FAboutJVCL: TJVCLAboutInfo;
    FFixedWidthCols: integer;
doRecalculateWidth: boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaptionHeight(Value: integer);
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetGlyphs(Value: TImageList);
    procedure SetSingleGlyph(Value: boolean);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetAutoColumnSize(const Value: boolean);
    procedure UpdateSize;
  protected
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure GlyphsListChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    AlignAll: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property CaptionTextAlignment: TAlignment read FAlignment write SetAlignment
      default taCenter;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight
      default 17;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property SingleGlyph: boolean read FSingleGlyph write SetSingleGlyph
      default false;
    property AutoColumnSize: boolean read FAutoColumnSize write SetAutoColumnSize default true;
    property FixedWidthCols: integer read FFixedWidthCols write FFixedWidthCols;
  end;

implementation

constructor TJvgDBGrid.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taCenter;
  FCaptionHeight := 17;
  FSingleGlyph := false;
  Glyph := TBitmap.Create;
  GlyphsChangeLink := TChangeLink.Create;
  GlyphsChangeLink.OnChange := GlyphsListChanged;
  // defaults
  FAutoColumnSize := true;
end;

destructor TJvgDBGrid.Destroy;
begin
  if Assigned(FBitmap) then FBitmap.Free;
  GlyphsChangeLink.Free;
  Glyph.Free;
  inherited;
end;

procedure TJvgDBGrid.Loaded;
begin
  inherited;
  if Assigned(FBitmap) and (not FBitmap.Empty) then Bmp := FBitmap;
  RowHeights[0] := FCaptionHeight;
  if AutoColumnSize then UpdateSize;
end;

procedure TJvgDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = (ES_LEFT, ES_RIGHT, ES_CENTER);
var
  R: TRect;
  Str: string;
  CaptionHeight_: integer;
  x, x_, y, y_, IHeight, IWidth, Index: integer;
begin

  R := ARect;
  if (ARow > 0) and (ACol > 0) then
  begin
    inherited;
    exit;
  end;

  if IsItAFilledBitmap(bmp) then
  begin
    x := r.Left;
    y := r.top;
    IHeight := r.bottom - r.top;
    IWidth := r.Right - r.Left;
    x_ := x;
    y_ := y;
    while x_ < r.right do
    begin
      if x_ + IWidth > r.right then IWidth := r.right - x_;
      while y_ < r.bottom do
      begin
        IHeight := r.bottom - r.top;
        if y_ + IHeight > r.bottom then IHeight := r.bottom - y_;
        BitBlt(Canvas.Handle, x_, y_, min(IWidth, bmp.Width), min(IHeight, bmp.Height), bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(y_, min(IHeight, bmp.Height));
      end;
      Inc(x_, min(IWidth, bmp.Width));
      y_ := y;
    end;

    if ACol = 0 then
    begin
      DrawBoxEx(Canvas.Handle, ARect, ALLGLSIDES, bvNone, bvRaised, false, 0, true);
      exit;
    end;
  end;

  if (ARow <> 0) or (ACol < 1) then
  begin
    inherited;
    exit;
  end;

  Str := Columns[ACol - 1].Title.Caption;

  Canvas.Font := Columns[ACol - 1].Title.Font;
  InflateRect(ARect, -1, -1);
  R := ARect;
  InflateRect(R, 1, 1);
  DrawBoxEx(Canvas.Handle, R, ALLGLSIDES, bvNone, bvRaised, false, Columns[ACol - 1].Title.Color, IsItAFilledBitmap(bmp) or (Columns[ACol - 1].Title.Color = Color));

  if Assigned(FGlyphs) then
  begin
    if FSingleGlyph then
      Index := 0
    else
      Index := ACol - 1;
    if Index < FGlyphs.Count then
    begin
      FGlyphs.GetBitmap(Index, Glyph);
      CreateBitmapExt(Canvas.Handle, Glyph,
        R, 2, max(0, (R.Bottom - R.Top - Glyph.Height) div 2),
        fwoNone, fdsDefault, true,
        GetTransparentColor(Glyph, ftcLeftBottomPixel), 0
        );
      inc(ARect.Left, Glyph.Width);
      R := ARect;
    end;
  end;

  SetBkMode(Canvas.Handle, TRANSPARENT);
  DrawText(Canvas.Handle, PChar(Str), -1, R, aAlignments[FAlignment] or DT_WORDBREAK or
    DT_CALCRECT);

  if FCaptionHeight < 0 then
    CaptionHeight_ := R.Bottom - R.top
  else
    CaptionHeight_ := FCaptionHeight;

  RowHeights[0] := CaptionHeight_;

  ARect.Top := ARect.Top + max(0, (ARect.Bottom - R.Bottom) div 2);
  DrawText(Canvas.Handle, PChar(Str), -1, ARect, aAlignments[FAlignment] or DT_WORDBREAK);
  //              DT_CENTER or DT_WORDBREAK );

end;

procedure TJvgDBGrid.GlyphsListChanged(Sender: TObject);
begin
  Repaint;
end;

//*****************************************_____________PROPERTY METHODS

procedure TJvgDBGrid.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  Repaint;
end;

procedure TJvgDBGrid.SetCaptionHeight(Value: integer);
begin
  FCaptionHeight := Value;
  if FCaptionHeight >= 0 then
    RowHeights[0] := FCaptionHeight
  else
    Repaint;
end;

function TJvgDBGrid.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TJvgDBGrid.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    Bmp := FBitmap
  else if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else
    Bmp := nil;
  Invalidate;
end;

procedure TJvgDBGrid.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else if Assigned(FBitmap) then
    Bmp := FBitmap
  else
    Bmp := nil;
  Invalidate;
end;

procedure TJvgDBGrid.SetGlyphs(Value: TImageList);
begin
  if Assigned(FGlyphs) then FGlyphs.UnregisterChanges(GlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
    FGlyphs.RegisterChanges(GlyphsChangeLink);

end;

procedure TJvgDBGrid.SetSingleGlyph(Value: boolean);
begin
  FSingleGlyph := Value;
  Repaint;
end;
//-------------------------------------------------------------------------------


procedure TJvgDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Glyphs)and(AComponent = Glyphs) and (Operation = opRemove) then Glyphs := nil;
  if Assigned(Image)and(AComponent = Image) and (Operation = opRemove) then Image := nil;
end;

procedure TJvgDBGrid.UpdateSize;
var
  Message: TWMSize;
begin
  WMSize(Message);
end;

procedure TJvgDBGrid.SetAutoColumnSize(const Value: boolean);
begin
  if FAutoColumnSize = Value then exit;
  FAutoColumnSize := Value;
  UpdateSize;
end;

procedure TJvgDBGrid.WMSize(var Message: TWMSize);
var
  i, TotalWidth, FreeClientWidth: integer;
begin
  if Message.Msg <> 0 then inherited;

  if doRecalculateWidth then exit;
  doRecalculateWidth := true;

  try
    if (not AutoColumnSize)or(Width=0) then exit;
    TotalWidth := 0;
    for i:=FixedWidthCols to Columns.Count-1 do inc(TotalWidth, Columns[i].Width+1);
    FreeClientWidth := ClientWidth - 2;
    if dgIndicator in Options then dec(FreeClientWidth, 10);
    dec(FreeClientWidth, Columns.Count);
    if ScrollBars in [ssVertical, ssBoth] then dec(FreeClientWidth, GetSystemMetrics(SM_CXHSCROLL)+2);

    for i:=0 to FixedWidthCols-1 do dec(FreeClientWidth, Columns[i].Width);

    for i:=FixedWidthCols to Columns.Count-1 do
      Columns[i].Width := MulDiv(Columns[i].Width, FreeClientWidth, TotalWidth)-1;
  finally
    doRecalculateWidth := false;
  end;

end;

end.
