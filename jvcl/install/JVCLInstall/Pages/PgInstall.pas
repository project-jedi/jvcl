{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgProgress.pas, released on 2004-04-05.

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

unit PgInstall;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, JVCL3Install, DelphiData, JVCLData,
  FrmInstall;

type
  { A page where the compile progress is displayed. }
  TInstallPage = class(TInstallerPage, IUserDefinedPage, IInstallPage)
  private
    FFrame: TFrameInstall;
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
    function CanPrev: Boolean; override;
    procedure Action; override;
    procedure Abort;
  public
    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl; virtual;
  end;

implementation

uses
  InstallerConsts;

{ TInstallPage }

function TInstallPage.NextPage: IInstallerPage;
begin
  Result := nil;
end;

function TInstallPage.CanNext: Boolean;
begin
  Result := True;
end;

function TInstallPage.SetupPage(Client: TWinControl): TWinControl;
begin
  FFrame.Free;
  FFrame := TFrameInstall.Build(Installer, Client);
  Result := FFrame;
end;

procedure TInstallPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsInstallPageTitle;
  SubTitle := RsInstallPageSubTitle;
end;

function TInstallPage.CanPrev: Boolean;
begin
  Result := True;
end;

procedure TInstallPage.Action;
begin
  if Assigned(FFrame) then
    FFrame.Execute;
end;

procedure TInstallPage.Abort;
begin
  if Assigned(FFrame) then
    FFrame.Aborted := True;
end;

end.
