{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgPackageSelection.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit PgPackageSelection;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, JVCL3Install, DelphiData, JVCLData;

type
  { A page where the user can select the packages that should be installed and
    compiled. }
  TPackageSelectionPage = class(TInstallerPage, IUserDefinedPage)
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
  public
    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl; virtual;
  end;

implementation

uses
  InstallerConsts, FrmPackageSelection, PgSummary;

{ TPackageSelectionPage }

function TPackageSelectionPage.NextPage: IInstallerPage;
begin
  Result := TSummaryPage.Create(Installer);
end;

function TPackageSelectionPage.CanNext: Boolean;
begin
  Result := True;
end;

function TPackageSelectionPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFramePackageSelection.Build(Installer, Client).ListViewTargetIDEs;
end;

procedure TPackageSelectionPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsPackageSelectionPageTitle;
  SubTitle := RsPackageSelectionPageSubTitle;
end;

end.