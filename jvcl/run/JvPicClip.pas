{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPicClip.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPicClip;

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Graphics, Controls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows, QGraphics, QControls, Types, QImgList,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF COMPILER6_UP}
  JvComponent;

type
  TJvCellRange = 1..MaxInt;

  TJvPicClip = class(TJvComponent)
  private
    FPicture: TPicture;
    FRows: TJvCellRange;
    FCols: TJvCellRange;
    FBitmap: TBitmap;
    FMasked: Boolean;
    FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    procedure CheckIndex(Index: Integer);
    function GetCell(Col, Row: Cardinal): TBitmap;
    function GetGraphicCell(Index: Integer): TBitmap;
    function GetDefaultMaskColor: TColor;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function IsMaskStored: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetHeight(Value: Integer);
    procedure SetPicture(Value: TPicture);
    procedure SetWidth(Value: Integer);
    procedure SetMaskColor(Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetIndex(Col, Row: Cardinal): Integer;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer);
    procedure DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
    procedure LoadBitmapRes(Instance: THandle; ResID: PChar);
    property Cells[Col, Row: Cardinal]: TBitmap read GetCell;
    property GraphicCell[Index: Integer]: TBitmap read GetGraphicCell;
    property IsEmpty: Boolean read GetIsEmpty;
    property Count: Integer read GetCount;
  published
    property Cols: TJvCellRange read FCols write FCols default 1;
    property Height: Integer read GetHeight write SetHeight stored False;
    property Masked: Boolean read FMasked write FMasked default True;
    property Rows: TJvCellRange read FRows write FRows default 1;
    property Picture: TPicture read FPicture write SetPicture;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored IsMaskStored;
    property Width: Integer read GetWidth write SetWidth stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  SysUtils,
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  JvJVCLUtils, JvConsts;

constructor TJvPicClip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBitmap := TBitmap.Create;
  FRows := 1;
  FCols := 1;
  FMaskColor := GetDefaultMaskColor;
  FMasked := True;
end;

destructor TJvPicClip.Destroy;
begin
  FOnChange := nil;
  FPicture.OnChange := nil;
  FBitmap.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvPicClip.Assign(Source: TPersistent);
begin
  if Source is TJvPicClip then
  begin
    with TJvPicClip(Source) do
    begin
      Self.FRows := Rows;
      Self.FCols := Cols;
      Self.FMasked := Masked;
      Self.FMaskColor := MaskColor;
      Self.FPicture.Assign(FPicture);
    end;
  end
  else
  if (Source is TPicture) or (Source is TGraphic) then
    FPicture.Assign(Source)
  else
    inherited Assign(Source);
end;

type
  THackImageList = class(TImageList);

procedure TJvPicClip.AssignTo(Dest: TPersistent);
var
  I: Integer;
  SaveChange: TNotifyEvent;
begin
  if Dest is TPicture then
    Dest.Assign(FPicture)
  else
  if (Dest is TGraphic) and (FPicture.Graphic <> nil) and
    (FPicture.Graphic is TGraphic(Dest).ClassType) then
    Dest.Assign(FPicture.Graphic)
  else
  if (Dest is TImageList) and not IsEmpty then
  begin
    with TImageList(Dest) do
    begin
      SaveChange := OnChange;
      try
        OnChange := nil;
        Clear;
        Width := Self.Width;
        Height := Self.Height;
        for I := 0 to Self.Count - 1 do
          if Self.Masked and (MaskColor <> clNone) then
            TImageList(Dest).AddMasked(GraphicCell[I], MaskColor)
          else
            TImageList(Dest).Add(GraphicCell[I], nil);
        Masked := Self.Masked;
      finally
        OnChange := SaveChange;
      end;
      THackImageList(Dest).Change;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJvPicClip.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvPicClip.GetIsEmpty: Boolean;
begin
  Result := (Picture.Graphic = nil) or Picture.Graphic.Empty;
end;

function TJvPicClip.GetCount: Integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Cols * Rows;
end;

procedure TJvPicClip.Draw(Canvas: TCanvas; X, Y, Index: Integer);
var
  Image: TGraphic;
begin
  if Index < 0 then
    Image := Picture.Graphic
  else
    Image := GraphicCell[Index];
  if (Image <> nil) and not Image.Empty then
    if FMasked and (FMaskColor <> clNone) and
      (Picture.Graphic is TBitmap) then
      DrawBitmapTransparent(Canvas, X, Y, TBitmap(Image), FMaskColor)
    else
      Canvas.Draw(X, Y, Image);
end;

procedure TJvPicClip.DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
var
  X, Y: Integer;
begin
  X := (Rect.Left + Rect.Right - Width) div 2;
  Y := (Rect.Bottom + Rect.Top - Height) div 2;
  Draw(Canvas, X, Y, Index);
end;

procedure TJvPicClip.LoadBitmapRes(Instance: THandle; ResID: PChar);
var
  Bmp: TBitmap;
begin
  Bmp := MakeModuleBitmap(Instance, ResID);
  try
    Picture.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvPicClip.CheckIndex(Index: Integer);
begin
  if (Index >= Cols * Rows) or (Index < 0) then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
end;

function TJvPicClip.GetIndex(Col, Row: Cardinal): Integer;
begin
  Result := Col + (Row * Cols);
  if (Result >= Cols * Rows) or IsEmpty then
    Result := -1;
end;

function TJvPicClip.GetCell(Col, Row: Cardinal): TBitmap;
begin
  Result := GetGraphicCell(GetIndex(Col, Row));
end;

function TJvPicClip.GetGraphicCell(Index: Integer): TBitmap;
begin
  CheckIndex(Index);
  AssignBitmapCell(Picture.Graphic, FBitmap, Cols, Rows, Index);
  if Picture.Graphic is TBitmap then
    if FBitmap.PixelFormat <> pfDevice then
      FBitmap.PixelFormat := TBitmap(Picture.Graphic).PixelFormat;
  FBitmap.TransparentColor := FMaskColor or PaletteMask;
  FBitmap.Transparent := (FMaskColor <> clNone) and Masked;
  Result := FBitmap;
end;

function TJvPicClip.GetDefaultMaskColor: TColor;
begin
  Result := clOlive;
  if (Picture.Graphic <> nil) and (Picture.Graphic is TBitmap) then
    Result := TBitmap(Picture.Graphic).TransparentColor and not PaletteMask;
end;

function TJvPicClip.GetHeight: Integer;
begin
  Result := Picture.Height div FRows;
end;

function TJvPicClip.GetWidth: Integer;
begin
  Result := Picture.Width div FCols;
end;

function TJvPicClip.IsMaskStored: Boolean;
begin
  Result := MaskColor <> GetDefaultMaskColor;
end;

procedure TJvPicClip.SetMaskColor(Value: TColor);
begin
  if Value <> FMaskColor then
  begin
    FMaskColor := Value;
    Changed;
  end;
end;

procedure TJvPicClip.PictureChanged(Sender: TObject);
begin
  FMaskColor := GetDefaultMaskColor;
  if not (csReading in ComponentState) then
    Changed;
end;

procedure TJvPicClip.SetHeight(Value: Integer);
begin
  if (Value > 0) and (Picture.Height div Value > 0) then
    Rows := Picture.Height div Value;
end;

procedure TJvPicClip.SetWidth(Value: Integer);
begin
  if (Value > 0) and (Picture.Width div Value > 0) then
    Cols := Picture.Width div Value;
end;

procedure TJvPicClip.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

end.

