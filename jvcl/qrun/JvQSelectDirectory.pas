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

The Original Code is: JvSelectDirectory.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSelectDirectory;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  Classes,  
  QDialogs, 
  JvQBaseDlg;

type
  { TODO -opeter3 : Rewrite to not depend on FileCtrl? }
  TJvSelectDirectory = class(TJvCommonDialog)
  private
    FDirectory: string;
    FHelpContext: Longint;
    FInitialDir: string; 
    FTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property Directory: string read FDirectory;
    property HelpContext: Longint read FHelpContext write FHelpContext default 0;
    property InitialDir: string read FInitialDir write FInitialDir; 
    property Title: string read FTitle write FTitle;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvSelectDirectory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirectory := '';
  FInitialDir := '';
  FHelpContext := 0; 
  FTitle := '';
end;

function TJvSelectDirectory.Execute: Boolean;

var
  Dir: WideString;

begin
  FDirectory := InitialDir;  
  Dir := FDirectory;
  Result := SelectDirectory(Title, InitialDir, Dir);
  FDirectory := Dir; 
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

