{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransled.PAS, released on 2002-12-23.

The Initial Developer of the Original Code is Thomas Hensle (http://www.thensle.de)
Portions created by Thomas Hensle are Copyright (C) 2002 Thomas Hensle.
Portions created by XXXX Corp. are Copyright (C) 2002, 2003 XXXX Corp.
All Rights Reserved.

Contributor(s):
Thomas Huber (Thomas_D_huber@t-online.de)
peter3 (load new image only when needed, center image in control, draw border at designtime)

Last Modified: 2002-12-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTransLED;

interface

uses
  Windows, Classes, Graphics, JvComponent;

type
  TJvTransLED = class(TJvGraphicControl)
  private
    FImgPict: TBitmap;
    FImgMask: TBitmap;
    FColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Color: TColor read FColor write SetColor default clLime;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Height default 17;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 17;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Controls;

{$R JvTransLED.res}

const
  cMaskLEDName = 'JVTR_MASK_LED';
  cGreenLEDName = 'JVTR_GREEN_LED';

constructor TJvTransLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImgPict := TBitmap.Create;
  FImgMask := TBitmap.Create;
  FImgMask.LoadFromResourceName(HInstance, cMaskLEDName);
  Color := clLime;
  Width := 17;
  Height := 17;
end;

destructor TJvTransLED.Destroy;
begin
  FImgPict.Free;
  FImgMask.Free;
  inherited Destroy;
end;

procedure TJvTransLED.Paint;
var
  DestRect, SrcRect: TRect;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
  SrcRect := Rect(0, 0, FImgPict.Width, FImgPict.Height);
  DestRect := SrcRect;
  OffsetRect(DestRect, (ClientWidth - FImgPict.Width) div 2, (ClientHeight - FImgPict.Height) div 2);
  Canvas.CopyMode := cmSrcAnd;
  Canvas.CopyRect(DestRect, FImgMask.Canvas, SrcRect);
  Canvas.CopyMode := cmSrcPaint;
  Canvas.CopyRect(DestRect, FImgPict.Canvas, SrcRect);
end;

procedure TJvTransLED.SetColor(Value: TColor);
var
  X, Y: Integer;
begin
  if Value <> FColor then
  begin
    FColor := Value;
    FImgPict.LoadFromResourceName(HInstance, cGreenLEDName);
    FImgPict.PixelFormat := pf24bit;
    for X := 0 to FImgPict.Width-1 do
      for Y := 0 to FImgPict.Height-1 do
        if FImgPict.Canvas.Pixels[X, Y] = clLime then
          FImgPict.Canvas.Pixels[X, Y] := Color;
    Repaint;
  end;
end;

procedure TJvTransLED.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  {$IFDEF COMPILER6_UP}
  if AutoSize and (Align in [alNone, alCustom]) then
  {$ELSE}
  if AutoSize and (Align = alNone) then
  {$ENDIF}
    inherited SetBounds(ALeft, ATop, FImgPict.Width, FImgPict.Height)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

end.

