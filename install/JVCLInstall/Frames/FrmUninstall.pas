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
    LblFilename: TLabel;
    ProgressBarDelete: TProgressBar;
  private
    FInitializing: Boolean;
    FInstaller: TInstaller;
    procedure Init;
  protected
    procedure EvProgress(Sender: TObject; const Text: string; Position, Max: Integer);
    procedure EvDeleteFiles(TargetConfig: TTargetConfig);
    property Installer: TInstaller read FInstaller;
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameUninstall;
    procedure Execute;
  end;

implementation

uses
  InstallerConsts;

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

procedure TFrameUninstall.EvDeleteFiles(TargetConfig: TTargetConfig);
var
  List: TStrings;
  i, Percentage: Integer;
  Count: Integer;
begin
  List := TStringList.Create;
  try
   // find files to delete
    FindFiles(TargetConfig.UnitOutDir, '*.*', True, List, // do not localize
      ['.dcu', '.obj']);                                  // do not localize
    FindFiles(TargetConfig.BplDir, 'Jv*.*', True, List,   // do not localize
      ['.bpl', '.dcp', '.tds']);                          // do not localize
    if TargetConfig.Target.IsBCB then
      FindFiles(TargetConfig.DcpDir, 'Jv*.*', True, List, // do not localize
        ['.bpl', '.dcp', '.tds', '.lib', '.bpi']);        // do not localize

    ProgressBarDelete.Max := 100;
    ProgressBarDelete.Position := 0;
    Application.ProcessMessages;

    Count := List.Count;
    for i := 0 to Count - 1 do
    begin
      LblFilename.Caption := Format(RsDeletingFile, [ExtractFileName(List[i])]);
      LblFilename.Update;
      DeleteFile(List[i]);
      Percentage := i * 100 div Count;
      if Percentage <> ProgressBarDelete.Position then
      begin
        ProgressBarDelete.Position := Percentage;
        Application.ProcessMessages;
      end;
    end;
    LblFilename.Caption := '';
    ProgressBarDelete.Position := 0;
  finally
    List.Free;
  end;
end;

procedure TFrameUninstall.Init;
begin
  FInitializing := True;
  try
    LblTarget.Caption := '';
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;

    LblFilename.Caption := '';
    ProgressBarDelete.Max := 100;
    ProgressBarDelete.Position := 0;
    ProgressBarDelete.Visible := Installer.Data.DeleteFilesOnUninstall; 
  finally
    FInitializing := False;
  end;
end;

procedure TFrameUninstall.Execute;
var
  i: Integer;
begin
  Init;
  for i := 0 to Installer.SelTargetCount - 1 do
    if Installer.SelTargets[i].InstallJVCL then
    begin
      if Installer.Data.DeleteFilesOnUninstall then
        Installer.SelTargets[i].DeinstallJVCL(EvProgress, EvDeleteFiles)
      else
        Installer.SelTargets[i].DeinstallJVCL(EvProgress, nil);
    end;
end;

end.
