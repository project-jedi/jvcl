{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMousePositionner.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMousePositionner;

interface

uses
  Windows, SysUtils, Classes, Controls,
  JvTypes, JvComponent;

type
  TJvMousePositionner = class(TJvComponent)
  private
    FControl: TControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    procedure Execute(Steps: Integer = 200; Delay: Integer = 10);
    procedure ExecuteEx(Dest: TPoint; Steps: Integer = 200; Delay: Integer = 10);
  published
    property Control: TControl read FControl write FControl;
  end;

implementation

procedure TJvMousePositionner.Execute(Steps: Integer; Delay: Integer);
var
  Src, Dest, Tmp: TPoint;
  DiffX, DiffY: Real;
begin
  if Assigned(Control) then
  begin
    while Steps > 0 do
    begin
      GetCursorPos(Src);
      Dest.X := Control.ClientRect.Left + Control.ClientWidth div 2;
      Dest.Y := Control.ClientRect.Top + Control.ClientHeight div 2;
      Dest := FControl.ClientToScreen(Dest);
      DiffX := (Dest.X - Src.X) / Steps;
      DiffY := (Dest.Y - Src.Y) / Steps;
      Tmp.X := Round(Src.X + DiffX);
      Tmp.Y := Round(Src.Y + DiffY);
      SetCursorPos(Tmp.X, Tmp.Y);

      Sleep(Delay);
      Dec(Steps);
    end;
  end;
end;

procedure TJvMousePositionner.ExecuteEx(Dest: TPoint; Steps, Delay: Integer);
var
  Src, Tmp: TPoint;
  DiffX, DiffY: Real;
begin
  while Steps > 0 do
  begin
    GetCursorPos(Src);
    DiffX := (Dest.X - Src.X) / Steps;
    DiffY := (Dest.Y - Src.Y) / Steps;
    if (Round(DiffX) <> 0) or (Round(DiffY) <> 0) then
    begin
      Tmp.X := Round(Src.X + DiffX);
      Tmp.Y := Round(Src.Y + DiffY);
      SetCursorPos(Tmp.X, Tmp.Y);

      Sleep(Delay);
    end;
    Dec(Steps);
  end;
end;

procedure TJvMousePositionner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

initialization
  RegisterClass(TJvMousePositionner);

end.

