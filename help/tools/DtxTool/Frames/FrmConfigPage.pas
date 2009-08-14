{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmConfigPage.pas, released on 2004-03-29.

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

{$I jvcl.inc}

unit FrmConfigPage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  ShellAPI, CommCtrl,
  JvConsts,
  HelpBuild, HelpBuildData, JVCLHelpUtils,
  JvExStdCtrls, ImgList, Mask, JvExMask, JvToolEdit, CheckLst;

type
  TFrameConfigPage = class(TFrame)
    edtHelpDir: TJvDirectoryEdit;
    Label1: TLabel;
    chlOptions: TCheckListBox;
    Label2: TLabel;
    Label3: TLabel;
    edtJVCLxxDir: TJvDirectoryEdit;
    procedure chlOptionsClickCheck(Sender: TObject);
    procedure edtHelpDirChange(Sender: TObject);
    procedure edtHelpDirExit(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptionsCount: Integer;
    FAllTasks: TGUIDList;
    procedure Init;
    procedure FillOptions(Strings: TStrings);
    procedure SetTasks;
    procedure GetTasks;
    procedure GetDirs;
    procedure SetDirs;
    function GetAllTasks: TGuidList;
  protected
    procedure BeginSettingOptions;
    procedure EndSettingOptions;
    function SettingOptions: Boolean;
    property HelpBuilder: THelpBuilder read FHelpBuilder;
    property AllTasks: TGuidList read GetAllTasks;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameConfigPage;
  end;

implementation

uses
  Core, Math;

{$R *.dfm}
//=== { TFrameConfigPage } ===================================================

class function TFrameConfigPage.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameConfigPage;
begin
  Result := TFrameConfigPage.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameConfigPage.chlOptionsClickCheck(Sender: TObject);
begin
  GetTasks;
  HelpBuilder.PackageInstaller.UpdatePages;
end;

procedure TFrameConfigPage.FillOptions(Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    for I := 0 to AllTasks.Count - 1 do
      if HelpBuilder.Data.IsOptionalTask[AllTasks[i]] then
        Strings.AddObject(HelpBuildNiceStr(AllTasks[i]), TObject(I));

    //    for Task := Low(THelpBuildTask) to High(THelpBuildTask) do
    //      if Task in HelpBuilder.Data.OptionalTasks then
    //        Strings.AddObject(cHelpBuildTaskStr[Task], TObject(Task));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TFrameConfigPage.GetDirs;
begin
  if SettingOptions then
    Exit;

  HelpBuilder.Data.HelpDir := edtHelpDir.Text;
  HelpBuilder.Data.JVCLxxDir := edtJVCLxxDir.Text;
end;

procedure TFrameConfigPage.GetTasks;
var
  I: Integer;
  Index: Integer;
begin
  if SettingOptions then
    Exit;

  for I := 0 to chlOptions.Count - 1 do
  begin
    Index := Integer(chlOptions.Items.Objects[i]);
    HelpBuilder.Data.Task[AllTasks[Index]] := chlOptions.Checked[i];
  end;
end;

procedure TFrameConfigPage.Init;
begin
  BeginSettingOptions;
  try
    FillOptions(chlOptions.Items);
    SetTasks;
    SetDirs;
  finally
    EndSettingOptions;
  end;
end;

procedure TFrameConfigPage.BeginSettingOptions;
begin
  Inc(FSettingOptionsCount);
end;

procedure TFrameConfigPage.EndSettingOptions;
begin
  Dec(FSettingOptionsCount);
end;

function TFrameConfigPage.SettingOptions: Boolean;
begin
  Result := FSettingOptionsCount > 0;
end;

procedure TFrameConfigPage.SetDirs;
begin
  BeginSettingOptions;
  try
    edtHelpDir.Text := HelpBuilder.Data.HelpDir;
    edtJVCLxxDir.Text := HelpBuilder.Data.JVCLxxDir;
  finally
    EndSettingOptions;
  end;
end;

procedure TFrameConfigPage.SetTasks;
var
  Index: Integer;
  I: Integer;
begin
  BeginSettingOptions;
  try
    for I := 0 to chlOptions.Count - 1 do
    begin
      Index := Integer(chlOptions.Items.Objects[i]);
      chlOptions.Checked[i] := HelpBuilder.Data.Task[AllTasks[Index]];
    end;
  finally
    EndSettingOptions;
  end;
end;

procedure TFrameConfigPage.edtHelpDirChange(Sender: TObject);
begin
  GetDirs;
end;

procedure TFrameConfigPage.edtHelpDirExit(Sender: TObject);
begin
  GetDirs;
end;

function TFrameConfigPage.GetAllTasks: TGuidList;
begin
  if not Assigned(FAllTasks) then
  begin
    FAllTasks := AllHelpBuildTasks(HelpBuilder.Data.HelpBuildType);
  end;
  Result := FAllTasks;
end;

end.
