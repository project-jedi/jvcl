{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMousePositionner.PAS, released on 2001-02-28.

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

unit JvMousePositionner;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  JvTypes, JvComponent;

type
  TJvMousePositionner = class(TJvComponent)
  private
    FControl: TControl;
  protected
  public
  published
    property Control: TControl read FControl write FControl;
    procedure Execute(Steps: Integer = 200; Delay: Integer = 10);
    procedure ExecuteEx(Dest: TPoint; Steps: Integer = 200; Delay: Integer = 10);
  end;

implementation

{**************************************************}

procedure TJvMousePositionner.Execute(Steps: Integer; Delay: Integer);
var
  src, dest, tmp: TPoint;
  diffx, diffy: Real;
begin
  if Assigned(Control) then
  begin
    while Steps > 0 do
    begin
      GetCursorPos(src);
      dest.x := Control.ClientRect.Left + Control.ClientWidth div 2;
      dest.y := Control.ClientRect.Top + Control.ClientHeight div 2;
      dest := FControl.ClientToScreen(Dest);
      diffx := (dest.x - src.x) / Steps;
      diffy := (dest.y - src.y) / Steps;
      tmp.x := Round(src.x + diffx);
      tmp.y := Round(src.y + diffy);
      SetCursorPos(tmp.x, tmp.y);

      Sleep(Delay);
      Dec(Steps);
    end;
  end;
end;
{**************************************************}

procedure TJvMousePositionner.ExecuteEx(Dest: TPoint; Steps, Delay: Integer);
var
  src, tmp: TPoint;
  diffx, diffy: Real;
begin
  while Steps > 0 do
  begin
    GetCursorPos(src);
    diffx := (dest.x - src.x) / Steps;
    diffy := (dest.y - src.y) / Steps;
    if (Round(diffx) <> 0) or (Round(diffy) <> 0) then
    begin
      tmp.x := Round(src.x + diffx);
      tmp.y := Round(src.y + diffy);
      SetCursorPos(tmp.x, tmp.y);

      Sleep(Delay);
    end;
    Dec(Steps);
  end;
end;

initialization
  RegisterClass(TJvMousePositionner);
end.
