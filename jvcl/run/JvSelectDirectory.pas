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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSelectDirectory;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes,
  FileCtrl,
  JvBaseDlg;

type
  { TODO -opeter3 : Rewrite to not depend on FileCtrl? }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSelectDirectory = class(TJvCommonDialog)
  private
    FDirectory: string;
    FHelpContext: Longint;
    FInitialDir: string;
    FClassicDialog: Boolean;
    FOptions: TSelectDirOpts;
    FTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property Directory: string read FDirectory;
    property HelpContext: Longint read FHelpContext write FHelpContext default 0;
    property InitialDir: string read FInitialDir write FInitialDir;
    property ClassicDialog: Boolean read FClassicDialog write FClassicDialog default True;
    property Options: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
    property Title: string read FTitle write FTitle;
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


constructor TJvSelectDirectory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirectory := '';
  FInitialDir := '';
  FHelpContext := 0;
  FClassicDialog := True;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
  FTitle := '';
end;

function TJvSelectDirectory.Execute(ParentWnd: HWND): Boolean;
begin
  FDirectory := InitialDir;
  DoShow;
  try
    if ClassicDialog then
      Result := SelectDirectory(FDirectory, Options, HelpContext)
    else
      Result := SelectDirectory(Title, InitialDir, FDirectory);
  finally
    DoClose;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
