{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShade.PAS, released on 2003-01-15.

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

unit JvgShade;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvgTypes, JvgUtils, JvgCommClasses;

type

  TJvgShade = class(TCustomPanel)
  private
    FImage: TBitmap;
    fLoaded: boolean;
    fNeedRebuildImage: boolean;

  protected
    property Color; //...hide
    procedure Paint; override;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RemakeBackground; //...for users
  published
    property Align;
    property Enabled;
    property Visible;
    property Image: TBitmap read FImage write FImage;
  end;

procedure Register;

implementation
//uses test;

procedure Register;
begin
//  RegisterComponents('Proba', [TJvgShade]);
end;
//*****************************************_____________LowLevel METHODS
//________________________________________________________

constructor TJvgShade.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 105;
  Height := 105;
  Image := TBitmap.create;
  fLoaded := true;
  //...defaults
  fNeedRebuildImage := (csDesigning in ComponentState) and not (csLoading in ComponentState);
end;
//________________________________________________________

destructor TJvgShade.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;
//________________________________________________________

procedure TJvgShade.WMSize(var Msg: TMessage);
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then RemakeBackground;
end;
//________________________________________________________

procedure TJvgShade.Paint;
var
  i, j, o: integer;
  RGB: COLORREF;
  R, G, B: byte;
const
  SHIFTCOLOR = $003939;
begin
  if fNeedRebuildImage then
  begin
    Image.Width := Width;
    Image.Height := Height;
    //..prepare tabula rasa :)
    Image.Canvas.Brush.Color := Parent.Brush.Color;
    Image.Canvas.Brush.Style := bsSolid;
    Image.Canvas.FillRect(ClientRect);
    GetParentImageRect(self, Bounds(Left, Top, Width, Height), Image.Canvas.Handle);
    for j := 0 to Height do
      for i := 0 to Width do
        //	if Image.Canvas.Pixels[i,j] > SHIFTCOLOR then
      begin
        if o <> Image.Canvas.Pixels[i, j] then
        begin
          //o := Image.Canvas.Pixels[i,j];
          //Form1.Memo1.Lines.Add(Format('%x',[o]));
        end;
        //	  if Image.Canvas.Pixels[i,j] = $C8B8A0 then
        RGB := Image.Canvas.Pixels[i, j];
        R := byte(RGB shr 16);
        G := byte(RGB shr 8);
        B := byte(RGB);
        //	  RShift := $
        Image.Canvas.Pixels[i, j] := Image.Canvas.Pixels[i, j] + SHIFTCOLOR;
      end;
    fNeedRebuildImage := false
  end;

  BitBlt(Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0, SRCCOPY);

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, width, height);
    end;

end;

procedure TJvgShade.RemakeBackground; //...for users
begin
  fNeedRebuildImage := true;
  Repaint;
end;
//________________________________________________________

end.
