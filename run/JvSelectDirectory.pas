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

unit JvSelectDirectory;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  Classes,
  {$IFDEF VCL}
  FileCtrl,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QDialogs,
  {$ENDIF VisualCLX}
  JvBaseDlg;

type
  { TODO -opeter3 : Rewrite to not depend on FileCtrl? }
  TJvSelectDirectory = class(TJvCommonDialog)
  private
    FDirectory: string;
    FHelpContext: Longint;
    FInitialDir: string;
    {$IFDEF VCL}
    FClassicDialog: Boolean;
    FOptions: TSelectDirOpts;
    {$ENDIF VCL}
    FTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property Directory: string read FDirectory;
    property HelpContext: Longint read FHelpContext write FHelpContext default 0;
    property InitialDir: string read FInitialDir write FInitialDir;
    {$IFDEF VCL}
    property ClassicDialog: Boolean read FClassicDialog write FClassicDialog default True;
    property Options: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
    {$ENDIF VCL}
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
  {$IFDEF VCL}
  FClassicDialog := True;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
  {$ENDIF VCL}
  FTitle := '';
end;

function TJvSelectDirectory.Execute: Boolean;
{$IFDEF VisualCLX}
var
  Dir: WideString;
{$ENDIF VisualCLX}
begin
  FDirectory := InitialDir;
  {$IFDEF VCL}
  if ClassicDialog then
    Result := SelectDirectory(FDirectory, Options, HelpContext)
  else
    Result := SelectDirectory(Title, InitialDir, FDirectory);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Dir := FDirectory;
  Result := SelectDirectory(Title, InitialDir, Dir);
  FDirectory := Dir;
  {$ENDIF VisualCLX}
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

