{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFixFont.PAS, released on 2003-01-15.

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

unit JvgFixFont;

{$I jvcl.inc}

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Graphics, Controls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvgFixFont = class(TJvComponent)
  private
    FFont: TFont;
    procedure FixFont(Window: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Font: TFont read FFont write FFont;
  end;

implementation

type
  TControlAccessProtected = class(TControl);

constructor TJvgFixFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
end;

destructor TJvgFixFont.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvgFixFont.Loaded;
begin
  if Owner is TWinControl then
    FixFont(TWinControl(Owner));
end;

procedure TJvgFixFont.FixFont(Window: TWinControl);
var
  I: Integer;
begin
  with Window do
  begin
    TControlAccessProtected(Window).Font.Assign(FFont);
    for I := 0 to ComponentCount - 1 do
      if Components[I] is TWinControl then
        FixFont(TWinControl(Components[I]))
      else
      if Components[I] is TControl then
        TControlAccessProtected(Components[I]).Font.Assign(FFont);
  end;
end;

end.

