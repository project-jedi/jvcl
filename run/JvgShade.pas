{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShade.PAS, released on 2003-01-15.

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

unit JvgShade;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgUtils, JvgCommClasses;

type
  {$IFDEF USEJVCL}
  TJvgShade = class(TJvCustomPanel)
  {$ELSE}
  TJvgShade = class(TCustomPanel)
  {$ENDIF USEJVCL}
  private
    FImage: TBitmap;
    FLoaded: Boolean;
    FNeedRebuildImage: Boolean;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  protected
    property Color;
    procedure Paint; override;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemakeBackground;
  published
    property Align;
    property Enabled;
    property Height default 105;
    property Image: TBitmap read FImage write FImage;
    property Visible;
    property Width default 105;
  end;

{$IFNDEF USEJVCL}
  {$UNDEF UNITVERSIONING}
{$ENDIF ~USEJVCL}

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvgShade.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 105;
  Height := 105;
  FImage := TBitmap.Create;
  FLoaded := True;
  FNeedRebuildImage := (csDesigning in ComponentState) and
    not (csLoading in ComponentState);
end;

destructor TJvgShade.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TJvgShade.WMSize(var Msg: TMessage);
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    RemakeBackground;
end;

procedure TJvgShade.Paint;
var
  I, J, N: Integer;
const
  cShiftColor = TColor($003939);
begin
  N := 0;
  if FNeedRebuildImage then
  begin
    Image.Width := Width;
    Image.Height := Height;
    //..prepare tabula rasa :)
    Image.Canvas.Brush.Color := Parent.Brush.Color;
    Image.Canvas.Brush.Style := bsSolid;
    Image.Canvas.FillRect(ClientRect);
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      Image.Canvas.Handle);
    for J := 0 to Height-1 do
      for I := 0 to Width-1 do
        // if Image.Canvas.Pixels[I, J] > cShiftColor then
      begin
        if N <> Image.Canvas.Pixels[I, J] then
        begin
          //N := Image.Canvas.Pixels[I, J];
          //Form1.Memo1.Lines.Add(Format('%x', [N]));
        end;
        // if Image.Canvas.Pixels[I, J] = $C8B8A0 then
//            RGB := Image.Canvas.Pixels[I, J];
//            R := Byte(RGB shr 16);
//            G := Byte(RGB shr 8);
//            B := Byte(RGB);
        // RShift := $
        Image.Canvas.Pixels[I, J] := Image.Canvas.Pixels[I, J] + cShiftColor;
      end;
    FNeedRebuildImage := False;
  end;

  BitBlt(Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0, SRCCOPY);

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TJvgShade.RemakeBackground;
begin
  FNeedRebuildImage := True;
  Invalidate;
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

