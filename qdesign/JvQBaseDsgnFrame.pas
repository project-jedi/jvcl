{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDsgnFrame.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQBaseDsgnFrame;

{$I jvcl.inc}

interface

uses
  SysUtils, QWindows, QMessages, QGraphics, QControls, QForms, QDialogs,
  Classes;

type
  TfmeJvBaseDesign = class(TFrame)
  protected
    { Retrieve the registry key to store settings in. Get's the info from the owning form; provided
      it's a TJvBaseDesign form. }
    function RegKey: string;
  public
    { Determines if the settings for this frame class should be stored/restored upon
      storing/restoring TfrmJvBaseDesign form settings. Defaults to False. }
    function SettingsStored: Boolean; dynamic;
    { Store the settings for this frame. Descendants that want to store additional settings should
      override this method. Automatically called from the form's StoreSettings (if it's a
      TJvBaseDesign form and StoreSettings for this frame returns True). }
    procedure StoreSettings; dynamic;
    { Restore the settings for this frame. Descendants that want to restore additional settings
      should override this method. Automatically called from the form's StoreSettings (if it's a
      TJvBaseDesign form and StoreSettings for this frame returns True). }
    procedure RestoreSettings; dynamic;
  end;

implementation

{$R *.xfm}

uses
  JvQBaseDsgnForm;

type
  TOpenBaseDesign = class(TJvBaseDesign);

function TfmeJvBaseDesign.RegKey: string;
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if (ParentForm <> nil) and (ParentForm is TJvBaseDesign) then
    Result := TOpenBaseDesign(ParentForm).GetRegKey
  else
    Result := '';
end;

function TfmeJvBaseDesign.SettingsStored: Boolean;
begin
  Result := False;
end;

procedure TfmeJvBaseDesign.StoreSettings;
begin
end;

procedure TfmeJvBaseDesign.RestoreSettings;
begin
end;

end.
