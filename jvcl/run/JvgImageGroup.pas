{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgImageGroup.PAS, released on 2003-01-15.

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

unit JvgImageGroup;

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
  JvgUtils,
  JVComponent,
  JvgCommClasses;

type
  TJvgImageGroup = class(TJvGraphicControl)
  private
    FImageList: TImageList;
    FPassiveMask: TBitmap;
    FActiveMask: TBitmap;
    FSelectedMask: TBitmap;
    FSingleSelected: boolean;
    FTransparent: boolean;
    FTransparentColor: TColor;
    FMasked: boolean;
    FMaskedColor: TColor;
    FDisabledMaskColor: TColor;

    FAutoTrColor: TglAutoTransparentColor;
    FFastDraw: boolean;
    fNeedRemakeBackground: boolean;
    Image: TBitmap;
    OldWidth, OldHeight,
      OldLeft, OldTop: integer;

    procedure SmthChanged(Sender: TObject);

    procedure SetImageList(Value: TImageList);
    procedure SetTransparent(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMasked(Value: boolean);
    procedure SetMaskedColor(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetFastDraw(Value: boolean);

  protected
    procedure Paint; override;
    procedure WMSize(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateResBitmap;
    procedure RemakeBackground;
  published
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

    property Images: TImageList read FImageList write SetImageList;
    property Transparent: boolean read FTransparent write SetTransparent
      default false;
    property TransparentColor: TColor read FTransparentColor
      write SetTransparentColor default clOlive;
    property Masked: boolean read FMasked write SetMasked
      default false;
    property MaskedColor: TColor read FMaskedColor
      write SetMaskedColor default clOlive;
    property DisabledMaskColor: TColor read FDisabledMaskColor
      write SetDisabledMaskColor default clBlack;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property FastDraw: boolean read FFastDraw write SetFastDraw
      default false;
  end;

procedure Register;

implementation

procedure Register;
begin
end;

//*****************************************_____________LowLevel METHODS
//________________________________________________________

constructor TJvgImageGroup.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  Width := 105;
  Height := 105;

  Image := TBitmap.create;
  //...defaults
  FTransparent := false;
  FTransparentColor := clOlive;
  FMasked := false;
  FMaskedColor := clOlive;
  FDisabledMaskColor := clBlack;
  FAutoTrColor := ftcLeftBottomPixel;
  FFastDraw := false;
end;
//________________________________________________________

destructor TJvgImageGroup.Destroy;
begin
  Image.free;
  inherited;
end;
//________________________________________________________

procedure TJvgImageGroup.WMSize(var Msg: TMessage);
begin
  if csDesigning in ComponentState then
    CreateResBitmap;
end;
//________________________________________________________

procedure TJvgImageGroup.Paint;
begin
  //  if fNeedRebuildImage then
  begin
    CreateResBitmap;
  end;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0,
    SRCCOPY);
end;

procedure TJvgImageGroup.RemakeBackground;
begin
  fNeedRemakeBackground := true;
  Repaint;
end;
//________________________________________________________

procedure TJvgImageGroup.CreateResBitmap;
var
  i: integer;
  Bitmap: TBitmap;
begin
  if (FImageList = nil) or (FImageList.Count = 0) then
    exit;

  Bitmap := TBitmap.Create;

  Image.Width := FImageList.Width * FImageList.Count;
  Image.Height := FImageList.Height;
  Width := max(Image.Width, Width);
  Height := max(Image.Height, Height);
  with Image do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Bounds(0, 0, Width, Height));
  end;

  if FTransparent then
    GetParentImageRect(self, Bounds(Left, Top, Image.Width, Image.Height),
      Image.Canvas.Handle);

  for i := 0 to FImageList.Count - 1 do
  begin
    FImageList.GetBitmap(i, Bitmap);

    if FMasked then
      ChangeBitmapColor(Image, FMaskedColor, clBtnFace);

    CreateBitmapExt(Image.Canvas.Handle, Bitmap, ClientRect,
      i * FImageList.Width, 0,
      fwoNone, fdsDefault,
      FTransparent, FTransparentColor, FDisabledMaskColor);

  end;
  Bitmap.Free;
end;
//________________________________________________________

procedure TJvgImageGroup.SmthChanged(Sender: TObject);
begin
  Invalidate;
end;

//*****************************************_____________PROPERTY METHODS
//________________________________________________________

procedure TJvgImageGroup.SetImageList(Value: TImageList);
begin
  FImageList := Value;
  //  SetAutoTrColor( FAutoTrColor );
  Invalidate;
end;
//________________________________________________________

procedure TJvgImageGroup.SetTransparent(Value: boolean);
begin
  if FTransparent = Value then
    exit;
  FTransparent := Value;
  Invalidate;
end;
//________________________________________________________

procedure TJvgImageGroup.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then
    exit;
  //  FAutoTrColor:=ftcUser;
  FTransparentColor := Value;
  Invalidate;
end;
//________________________________________________________

procedure TJvgImageGroup.SetMasked(Value: boolean);
begin
  if FMasked = Value then
    exit;
  FMasked := Value;
  Invalidate;
end;
//________________________________________________________

procedure TJvgImageGroup.SetMaskedColor(Value: TColor);
begin
  if FMaskedColor = Value then
    exit;
  FMaskedColor := Value;
  Invalidate;
end;

procedure TJvgImageGroup.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor = Value then
    exit;
  FDisabledMaskColor := Value;
  Invalidate;
end;
//________________________________________________________

procedure TJvgImageGroup.SetAutoTrColor(Value: TglAutoTransparentColor);
//var x,y :integer;
begin {
  FAutoTrColor := Value;
  if (FAutoTrColor=ftcUser)or((FBitmap.Width or FBitmap.Height)=0)then
    exit;
  case FAutoTrColor of
    ftcLeftTopPixel: begin x:=0; y:=0; end;
    ftcLeftBottomPixel: begin x:=0; y:=FBitmap.Height-1; end;
    ftcRightTopPixel: begin x:=FBitmap.Width-1; y:=0; end;
    ftcRightBottomPixel: begin x:=FBitmap.Width-1; y:=FBitmap.Height-1; end;
  end;
  FTransparentColor := GetPixel(FBitmap.Canvas.Handle,x,y);
  Invalidate;}
end;
//________________________________________________________

procedure TJvgImageGroup.SetFastDraw(Value: boolean);
begin
  FFastDraw := Value;
end;

end.
