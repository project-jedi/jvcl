{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgImageGroup.PAS, released on 2003-01-15.

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

unit JvgImageGroup;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgUtils, JvgCommClasses;

type
  {$IFDEF USEJVCL}
  TJvgImageGroup = class(TJvGraphicControl)
  {$ELSE}
  TJvgImageGroup = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FImageList: TImageList;
    //    FPassiveMask: TBitmap;
    //    FActiveMask: TBitmap;
    //    FSelectedMask: TBitmap;
    //    FSingleSelected: Boolean;
    FTransparent: Boolean;
    FTransparentColor: TColor;
    FMasked: Boolean;
    FMaskedColor: TColor;
    FDisabledMaskColor: TColor;
    FAutoTrColor: TglAutoTransparentColor;
    FFastDraw: Boolean;
    FNeedRemakeBackground: Boolean;
    FImage: TBitmap;
    //    OldWidth, OldHeight,
    //      OldLeft, OldTop: Integer;
    //    procedure SmthChanged(Sender: TObject);
    procedure SetImageList(Value: TImageList);
    procedure SetTransparent(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMasked(Value: Boolean);
    procedure SetMaskedColor(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetFastDraw(Value: Boolean);
  protected
    procedure Paint; override;
    procedure WMSize(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateResBitmap;
    procedure RemakeBackground;
  published
    property Images: TImageList read FImageList write SetImageList;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clOlive;
    property Masked: Boolean read FMasked write SetMasked default False;
    property MaskedColor: TColor read FMaskedColor write SetMaskedColor default clOlive;
    property DisabledMaskColor: TColor read FDisabledMaskColor write SetDisabledMaskColor default clBlack;
    property AutoTransparentColor: TglAutoTransparentColor read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property FastDraw: Boolean read FFastDraw write SetFastDraw default False;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;
  
constructor TJvgImageGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  Width := 105;
  Height := 105;

  FImage := TBitmap.Create;
  //...defaults
  FTransparent := False;
  FTransparentColor := clOlive;
  FMasked := False;
  FMaskedColor := clOlive;
  FDisabledMaskColor := clBlack;
  FAutoTrColor := ftcLeftBottomPixel;
  FFastDraw := False;
end;

destructor TJvgImageGroup.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TJvgImageGroup.WMSize(var Msg: TMessage);
begin
  if csDesigning in ComponentState then
    CreateResBitmap;
end;

procedure TJvgImageGroup.Paint;
begin
  //  if fNeedRebuildImage then
    CreateResBitmap;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FImage.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJvgImageGroup.RemakeBackground;
begin
  FNeedRemakeBackground := True;
  Repaint;
end;

procedure TJvgImageGroup.CreateResBitmap;
var
  I: Integer;
  Bitmap: TBitmap;
begin
  if (FImageList = nil) or (FImageList.Count = 0) then
    Exit;

  Bitmap := TBitmap.Create;

  FImage.Width := FImageList.Width * FImageList.Count;
  FImage.Height := FImageList.Height;
  Width := Max(FImage.Width, Width);
  Height := Max(FImage.Height, Height);
  with FImage do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Bounds(0, 0, Width, Height));
  end;

  if FTransparent then
    GetParentImageRect(Self, Bounds(Left, Top, FImage.Width, FImage.Height),
      FImage.Canvas.Handle);

  for I := 0 to FImageList.Count - 1 do
  begin
    FImageList.GetBitmap(I, Bitmap);

    if FMasked then
      ChangeBitmapColor(FImage, FMaskedColor, clBtnFace);

    CreateBitmapExt(FImage.Canvas.Handle, Bitmap, ClientRect,
      I * FImageList.Width, 0,
      fwoNone, fdsDefault,
      FTransparent, FTransparentColor, FDisabledMaskColor);
  end;
  Bitmap.Free;
end;

{
procedure TJvgImageGroup.SmthChanged(Sender: TObject);
begin
  Invalidate;
end;
}

procedure TJvgImageGroup.SetImageList(Value: TImageList);
begin
  FImageList := Value;
  //  SetAutoTrColor( FAutoTrColor );
  Invalidate;
end;

procedure TJvgImageGroup.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvgImageGroup.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then
  begin
    //  FAutoTrColor:=ftcUser;
    FTransparentColor := Value;
    Invalidate;
  end;
end;

procedure TJvgImageGroup.SetMasked(Value: Boolean);
begin
  if FMasked <> Value then
  begin
    FMasked := Value;
    Invalidate;
  end;
end;

procedure TJvgImageGroup.SetMaskedColor(Value: TColor);
begin
  if FMaskedColor <> Value then
  begin
    FMaskedColor := Value;
    Invalidate;
  end;
end;

procedure TJvgImageGroup.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor <> Value then
  begin
    FDisabledMaskColor := Value;
    Invalidate;
  end;
end;

procedure TJvgImageGroup.SetAutoTrColor(Value: TglAutoTransparentColor);
//var x, y : Integer;
begin {
  FAutoTrColor := Value;
  if (FAutoTrColor=ftcUser)or((FBitmap.Width or FBitmap.Height)=0)then
    Exit;
  case FAutoTrColor of
    ftcLeftTopPixel: begin x:=0; y:=0; end;
    ftcLeftBottomPixel: begin x:=0; y:=FBitmap.Height-1; end;
    ftcRightTopPixel: begin x:=FBitmap.Width-1; y:=0; end;
    ftcRightBottomPixel: begin x:=FBitmap.Width-1; y:=FBitmap.Height-1; end;
  end;
  FTransparentColor := GetPixel(FBitmap.Canvas.Handle,x,y);
  Invalidate;}
end;

procedure TJvgImageGroup.SetFastDraw(Value: Boolean);
begin
  FFastDraw := Value;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

