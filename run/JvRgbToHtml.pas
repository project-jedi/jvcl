{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRgbToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRgbToHtml;

interface

uses
  Windows, SysUtils, Classes, Graphics,
  JvComponent;

type
  TJvRgbToHtml = class(TJvComponent)
  private
    FHtml: string;
    FColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetHtml(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RgbColor: TColor read FColor write SetColor default clBlack;
    property HtmlColor: string read FHtml write SetHtml;
  end;

function RgbToHtml(Value: TColor): string;

implementation

function RgbToHtml(Value: TColor): string;
begin
  with TJvRgbToHtml.Create(nil) do
  begin
    RgbColor := Value;
    Result := HtmlColor;
    Free;
  end;
end;

constructor TJvRgbToHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBlack;
  FHtml := '000000';
end;

procedure TJvRgbToHtml.SetColor(const Value: TColor);
var
  Clr: TColor;
begin
  FColor := Value;
  Clr := ColorToRGB(Value);
  FHtml := IntToHex(GetRValue(Clr), 2) + IntToHex(GetGValue(Clr), 2) + IntToHex(GetBValue(Clr), 2);
end;

procedure TJvRgbToHtml.SetHtml(const Value: string);
var
  C: TColor;
  R, G, B: Byte;
begin
  try
    if Length(Value) = 6 then
    begin
      R := StrToInt('$' + Copy(Value, 1, 2));
      G := StrToInt('$' + Copy(Value, 3, 2));
      B := StrToInt('$' + Copy(Value, 5, 2));
      C := RGB(R, G, B);
      FColor := C;

      FHtml := Value;
    end;
  except
  end;
end;

end.

