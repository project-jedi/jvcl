{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSmallFontsDefense.PAS, released on 2003-01-15.

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

// Component prevents your apps from BIG fonts.

unit JvgSmallFontsDefense;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, grids;

type
  TglSmallFontsDefenceOptions_ = (fdoExcludeGrids);
  TglSmallFontsDefenceOptions = set of TglSmallFontsDefenceOptions_;

  TJvgSmallFontsDefence = class(TComponent)
  private
    FOptions: TglSmallFontsDefenceOptions;
    procedure UpdateFonts(Control: TWinControl);
    procedure SetOptions(const Value: TglSmallFontsDefenceOptions);
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TglSmallFontsDefenceOptions read FOptions write SetOptions;
  end;

procedure Register;

implementation
uses JvgUtils, JvgTypes;

procedure Register;
begin
  RegisterComponents('Gl Components', [TJvgSmallFontsDefence]);
end;

{ TJvgSmallFontsDefence }

constructor TJvgSmallFontsDefence.Create(AOwner: TComponent);
begin
  inherited;
  if (Owner is TForm) then (Owner as TForm).Scaled := false;
end;

procedure TJvgSmallFontsDefence.Loaded;
begin
  inherited;
  if (Owner is TForm) then (Owner as TForm).Scaled := false;
  if csDesigning in ComponentState then
  begin
    if not IsSmallFonts then
      ShowMessage('Проектирование приложения в режиме крупных шрифтов недопустимо!'#13#10'Компонент TJvgSmallFontsDefence отказывается работать в таких условиях.');
  end
  else
    UpdateFonts((Owner as TForm));
end;

procedure TJvgSmallFontsDefence.SetOptions(const Value: TglSmallFontsDefenceOptions);
begin
  FOptions := Value;
end;

procedure TJvgSmallFontsDefence.UpdateFonts(Control: TWinControl);
var
  i: integer;

  procedure UpdateFont(Font: TFont);
  begin
    if CompareText(Font.Name, 'MS Sans Serif') <> 0 then exit;
    Font.Name := 'Arial';
  end;
begin
  if IsSmallFonts then exit;
  if (fdoExcludeGrids in Options) and (Control is TCustomGrid) then exit;
  UpdateFont(TJvgShowFont(Control).Font);
  with Control do
    for i := 0 to ControlCount - 1 do
    begin
      UpdateFont(TJvgShowFont(Controls[i]).Font);
      if Controls[i] is TWinControl then UpdateFonts(Controls[i] as TWinControl);
    end;

end;

end.
