{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgUninstall.pas, released on 2004-04-06.

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

unit PgUninstall;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, JVCL3Install, DelphiData, JVCLData,
  FrmUninstall;

type
  { A page where the compile progress is displayed. }
  TUninstallPage = class(TInstallerPage, IUserDefinedPage, IUninstallPage)
  private
    FFrame: TFrameUninstall;
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
    procedure Action; override;
  public
    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl; virtual;
  end;

implementation

uses
  InstallerConsts;

{ TUninstallPage }

function TUninstallPage.NextPage: IInstallerPage;
begin
  Result := nil;
end;

function TUninstallPage.CanNext: Boolean;
begin
  Result := True;
end;

function TUninstallPage.SetupPage(Client: TWinControl): TWinControl;
begin
  FFrame.Free;
  FFrame := TFrameUninstall.Build(Installer, Client);
  Result := FFrame;
end;

procedure TUninstallPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsUninstallPageTitle;
  SubTitle := RsUninstallPageSubTitle;
end;

procedure TUninstallPage.Action;
begin
  if Assigned(FFrame) then
    FFrame.Execute;
end;

end.
