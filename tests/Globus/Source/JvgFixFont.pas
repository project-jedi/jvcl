{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFixFont.PAS, released on 2003-01-15.

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

unit JvgFixFont;

interface

uses
  Windows,
  Messages,
  SysUtils,
  JvComponent,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs;

type
  TJvgPublicControlFont = class(TControl)
  public
    property Font;
  end;

  TJvgFixFont = class(TJvComponent)
  private
    FFont: TFont;
    procedure FixFont(Window: TWinControl);
    procedure SetFont(const Value: TFont);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    property Font: TFont read FFont write SetFont;
  published
    { Published declarations }
  end;

implementation

//____________________________

constructor TJvgFixFont.Create(AOwner: TComponent);
begin
  inherited;
  FixFont(TWinControl(Owner));
end;

procedure TJvgFixFont.FixFont(Window: TWinControl);
var
  i: integer;
begin
  with Window do
  begin
    TJvgPublicControlFont(Window).Font.Assign(fFont);
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TWinControl then
        FixFont(TWinControl(Components[i]))
      else if Components[i] is TControl then
        TJvgPublicControlFont(Components[i]).Font.Assign(FFont);
  end;
end;

procedure TJvgFixFont.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

end.
