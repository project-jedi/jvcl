{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgConfig.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit PgConfig;

interface

uses
  SysUtils, Classes, Controls,
  Core, JVCL3Install, DelphiData, JVCLData;

type
  { A page where the user can configure the compile process and the jvcl.inc
    file. }
  TConfigPage = class(TInstallerPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  InstallerConsts, FrmConfigPage, PgPackageSelection, PgSummary;

{ TConfigPage }

function TConfigPage.NextPage: IInstallerPage;
begin
  case Installer.InstallType of
    itFreshInstall:
      Result := TPackageSelectionPage.Create(Installer);
    itUpdate:
      Result := TSummaryPage.Create(Installer);
  end;
end;

function TConfigPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameConfigPage.Build(Installer, Client).CheckBoxDeveloperInstall;
end;

procedure TConfigPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsConfigPageTitle;
  SubTitle := RsConfigPageSubTitle;
end;

end.
