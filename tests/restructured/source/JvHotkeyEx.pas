{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHotKeyEx.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): _________________________________.

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHotkeyEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Menus, JVCLVer;

type
  TJvHotKeyEx = class(TEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    function GetShortcut: TShortcut;
    procedure SetShortcut(const Value: TShortcut);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Shortcut:TShortcut read GetShortcut write SetShortcut;
  end;

implementation

{**************************************************************************}
procedure TJvHotKeyEx.Change;
var
 st: string;
begin
  inherited;
  st := ShortCutToText(TextToShortcut(Text));
  if st<>Text then
    Text := st;
end;
{**************************************************************************}
constructor TJvHotKeyEx.Create(AOwner: TComponent);
begin
  inherited;
  Text := '';
end;
{**************************************************************************}
function TJvHotKeyEx.GetShortcut: TShortcut;
begin
  result := TextToShortCut(Text);
end;
{**************************************************************************}
procedure TJvHotKeyEx.KeyDown(var Key: Word; Shift: TShiftState);
var
 s: TShortCut;
begin
  inherited;
  if (Key=VK_CONTROL) then
    Shift := Shift-[ssCtrl];
  if (Key=VK_MENU) then
    Shift := Shift-[ssAlt];
  if (Key=VK_SHIFT) then
    Shift := Shift-[ssShift];
  s := Menus.Shortcut(Key,Shift);
  Text := ShortCutToText(s);
  Key := 0;
  Shift := [];
end;
{**************************************************************************}
procedure TJvHotKeyEx.KeyPress(var Key: Char);
begin
  inherited;
  Key := #0;
end;
{**************************************************************************}
procedure TJvHotKeyEx.SetShortcut(const Value: TShortcut);
begin
  Text := ShortCutToText(Value);
end;
{**************************************************************************}
end.
