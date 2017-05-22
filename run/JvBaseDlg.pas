{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDlg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBaseDlg;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Dialogs,
  JVCLVer;

type

  TJvCommonDialog = class(TCommonDialog)
  private
    FAboutJVCL: TJVCLAboutInfo;
  public
    {$IFNDEF RTL180_UP}
    function Execute: Boolean; overload; override;
    function Execute(ParentWnd: HWND): Boolean; reintroduce; overload; virtual; abstract;
    {$ENDIF ~RTL180_UP}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
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

uses
  Controls, Forms;


{ TJvCommonDialog }

{$IFNDEF RTL180_UP}
function TJvCommonDialog.Execute: Boolean;
var
  ParentWnd: HWND;
  F: TCustomForm;
begin
  ParentWnd := 0;
  if Owner is TControl then
  begin
    F := GetParentForm(TControl(Owner));
    if F <> nil then
      ParentWnd := F.Handle;
  end;
  if ParentWnd = 0 then
    ParentWnd := GetForegroundWindow;
  if ParentWnd = 0 then
    ParentWnd := GetDesktopWindow;

  Result := Execute(ParentWnd);
end;
{$ENDIF ~RTL180_UP}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.
