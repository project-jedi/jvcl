{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOfficeColorForm.PAS, released on 2004-02-26.

The Initial Developer of the Original Code is dejoy [dejoy att ynl dott gov dott cn]
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):dejoy.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Color form for the @link(TJvOfficeColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOfficeColorForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  JvComponent, JvOfficeColorPanel, JvOfficeDragBarForm;

{------------------------------------------------------------------------------}

type
  TJvOfficeColorForm = class(TJvOfficeDragBarForm)
  private
    function GetColorPanel: TJvCustomOfficeColorPanel;
  public
    constructor Create(AOwner: TComponent; AOfficeColorPanelClass: TJvOfficeColorPanelClass); reintroduce; virtual;
    property ColorPanel: TJvCustomOfficeColorPanel read GetColorPanel;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

type
  TJvOfficeColorPanelAccessProtected = class(TJvOfficeColorPanel);

//=== { TJvOfficeColorForm } =================================================

constructor TJvOfficeColorForm.Create(AOwner: TComponent;
  AOfficeColorPanelClass: TJvOfficeColorPanelClass);
begin
  inherited Create(AOwner);
  SetClient(AOfficeColorPanelClass.Create(Self));
  with TJvOfficeColorPanelAccessProtected(ColorPanel) do
  begin
    Parent := Self;
    FlatBorder := True;
    BorderWidth := 0;
  end;
end;

function TJvOfficeColorForm.GetColorPanel: TJvCustomOfficeColorPanel;
begin
  Result := TJvCustomOfficeColorPanel(Client);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
