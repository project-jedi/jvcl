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
  TJvTLActiveLight = (alRed, alBlue, alGreen, alYellow, alGray, alDkGray);
  TJvTLLightArray = array[TJvTLActiveLight] of TColor;

type
  TJvTransLED = class(TJvGraphicControl)
  private
    ImgPict, ImgMask: TBitmap;
    FActiveLight: TJvTLActiveLight;
    procedure SetActiveLight(Value: TJvTLActiveLight);
  protected
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

  published
    property ActiveLight: TJvTLActiveLight read FActiveLight write SetActiveLight;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;

    property OnClick;
    property OnMouseMove;
    property ShowHint;
  end;

implementation

uses Controls;
{$R JvTransLED.res}

constructor TJvTransLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImgPict := TBitmap.Create;
  ImgMask := TBitmap.Create;
  ImgMask.Handle := LoadBitmap(hInstance, 'JVTR_MASK_LED');
  FActiveLight := alGreen;
  Width := 17;
  Height := 17;
end;

destructor TJvTransLED.Destroy;
begin
  ImgPict.Free;
  ImgMask.Free;
  inherited;
end;

procedure TJvTransLED.Paint;
const
  cBmpName: array[TJvTLActiveLight] of PChar =
  {  alRed, alBlue, alGreen, alYellow, alGray, alDkGray }
  ('JVTR_RED_LED', 'JVTR_BLUE_LED', 'JVTR_GREEN_LED', 'JVTR_YELLOW_LED', 'JVTR_GRAY_LED', 'JVTR_DK_GRAY_LED');
var
  DestRect, SrcRect: TRect;
begin
  if ImgPict.Handle = 0 then
    ImgPict.Handle := LoadBitmap(hInstance, cBmpName[FActiveLight]);
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
  SrcRect := Rect(0, 0, ImgPict.Width, ImgPict.Height);
  DestRect := SrcRect;
  OffsetRect(DestRect,(ClientWidth - ImgPict.Width) div 2,(ClientHeight - ImgPict.Height) div 2);
  Canvas.CopyMode := cmSrcAnd;
  Canvas.CopyRect(DestRect, ImgMask.Canvas, SrcRect);
  Canvas.CopyMode := cmSrcPaint;
  Canvas.CopyRect(DestRect, ImgPict.Canvas, SrcRect);
end;

procedure TJvTransLED.SetActiveLight(Value: TJvTLActiveLight);
begin
  if Value <> FActiveLight then
  begin
    FActiveLight := Value;
    ImgPict.Handle := 0;
    Repaint;
  end;
end;


procedure TJvTransLED.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  {$IFDEF COMPILER6_UP}
  if AutoSize and (Align in [alNone,alCustom]) then
  {$ELSE}
  if AutoSize and (Align = alNone) then
  {$ENDIF}
    inherited SetBounds(ALeft,ATop,ImgPict.Width,ImgPict.Height)
  else
    inherited;
end;

end.

