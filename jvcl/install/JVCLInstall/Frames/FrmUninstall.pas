{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmUninstall.pas, released on 2004-04-06.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit FrmUninstall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls,
  JVCL3Install, JVCLData, PackageUtils, Compile, Utils;

type
  TFrameUninstall = class(TFrame)
    LblTarget: TLabel;
    ProgressBar: TProgressBar;
  private
    FInitializing: Boolean;
    FInstaller: TInstaller;
    procedure Init;
  protected
    procedure EvProgress(Sender: TObject; const Text: string; Position, Max: Integer);
    property Installer: TInstaller read FInstaller;
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameUninstall;
    procedure Execute;
  end;

implementation

{$R *.dfm}

{ TFrameUninstall }

class function TFrameUnInstall.Build(Installer: TInstaller;
  Client: TWinControl): TFrameUninstall;
begin
  Result := TFrameUninstall.Create(Client);
  Result.FInstaller := Installer;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameUninstall.EvProgress(Sender: TObject; const Text: string;
  Position, Max: Integer);
begin
  ProgressBar.Max := Max;
  ProgressBar.Position := Position;
  LblTarget.Caption := Text;
  Application.ProcessMessages;
end;

procedure TFrameUninstall.Init;
begin
  FInitializing := True;
  try
    LblTarget.Caption := '';
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
  finally
    FInitializing := False;
  end;
end;

procedure TFrameUninstall.Execute;
var
  i: Integer;
begin
  for i := 0 to Installer.SelTargetCount - 1 do
    if Installer.SelTargets[i].InstallJVCL then
      Installer.SelTargets[i].DeinstallJVCL(EvProgress);
end;

end.
