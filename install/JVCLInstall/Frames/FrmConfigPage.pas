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
home page, located at http://jvcl.sourceforge.net

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
  JVCL3Install, JvExStdCtrls, JVCLData, ImgList, FrmDirEditBrowse;

type
  TFrameConfigPage = class(TFrame)
    GroupBoxJvclInc: TGroupBox;
    CheckBoxXPTheming: TCheckBox;
    CheckBoxRegisterGlobalDesignEditors: TCheckBox;
    CheckBoxDxgettextSupport: TCheckBox;
    CheckBoxRegisterJvGif: TCheckBox;
    LblDxgettextHomepage: TLabel;
    CheckBoxUseJVCL: TCheckBox;
    GroupBoxInstallOptions: TGroupBox;
    CheckBoxDeveloperInstall: TCheckBox;
    CheckBoxCleanPalettes: TCheckBox;
    LblOptionsFor: TLabel;
    ComboBoxTargetIDE: TComboBox;
    ImageListTargets: TImageList;
    CheckBoxBuild: TCheckBox;
    CheckBoxCompileOnly: TCheckBox;
    FrameDirEditBrowseBPL: TFrameDirEditBrowse;
    FrameDirEditBrowseDCP: TFrameDirEditBrowse;
    FrameDirEditBrowseHPP: TFrameDirEditBrowse;
    BtnEditJvclInc: TButton;
    LblBCBGuide: TLabel;
    CheckBoxCompileJclDcp: TCheckBox;
    CheckBoxVerbose: TCheckBox;
    CheckBoxGenerateMapFiles: TCheckBox;
    procedure CheckBoxDeveloperInstallClick(Sender: TObject);
    procedure CheckBoxXPThemingClick(Sender: TObject);
    procedure ComboBoxTargetIDEChange(Sender: TObject);
    procedure ComboBoxTargetIDEDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BtnEditJvclIncClick(Sender: TObject);
    procedure LblDxgettextHomepageClick(Sender: TObject);
    procedure LblBCBGuideClick(Sender: TObject);
    procedure CheckBoxCompileJclDcpClick(Sender: TObject);
  private
    FInitializing: Integer;
    FInstaller: TInstaller;
    procedure Init;
    procedure UpdateJvclIncSettings;
    function GetSelTargetConfig: TTargetConfig;
    procedure BplDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
    procedure DcpDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
    procedure HppDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
  protected
    property Installer: TInstaller read FInstaller;

    property SelTargetConfig: TTargetConfig read GetSelTargetConfig;
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameConfigPage;
  end;

implementation

uses
  InstallerConsts, Core, MainConfig, Main, Utils, Math;

{$R *.dfm}

{ TFrameConfigPage }

class function TFrameConfigPage.Build(Installer: TInstaller;
  Client: TWinControl): TFrameConfigPage;
begin
  Result := TFrameConfigPage.Create(Client);
  Installer.PackageInstaller.Translate(Result);
  Result.FInstaller := Installer;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameConfigPage.BplDirChanged(Sender: TObject; UserData: TObject;
  var Dir: string);
begin
  SelTargetConfig.BplDir := Dir;
end;

procedure TFrameConfigPage.DcpDirChanged(Sender: TObject; UserData: TObject;
  var Dir: string);
begin
  SelTargetConfig.DcpDir := Dir;
end;

procedure TFrameConfigPage.HppDirChanged(Sender, UserData: TObject;
  var Dir: string);
begin
  SelTargetConfig.HppDir := Dir;
end;

function TFrameConfigPage.GetSelTargetConfig: TTargetConfig;
begin
  with ComboBoxTargetIDE do
  begin
    if ItemIndex <= 0 then
      Result := nil
    else
      Result := TTargetConfig(Items.Objects[ItemIndex]);
  end;
end;

procedure TFrameConfigPage.Init;
var
  i: Integer;
  x: Integer;
begin
  Inc(FInitializing);
  try
    x := BtnEditJvclInc.BoundsRect.Right;
    BtnEditJvclInc.Width := Max(BtnEditJvclInc.Width, LblOptionsFor.Canvas.TextWidth(BtnEditJvclInc.Caption) + 16);
    BtnEditJvclInc.Left := BtnEditJvclInc.Left - (BtnEditJvclInc.BoundsRect.Right - x);


    ImageListTargets.Clear;

    FrameDirEditBrowseBPL.OnChange := BplDirChanged;
    FrameDirEditBrowseDCP.OnChange := DcpDirChanged;
    FrameDirEditBrowseHPP.OnChange := HppDirChanged;
    FrameDirEditBrowseHPP.AllowEmpty := True;

    with ComboBoxTargetIDE do
    begin
      Items.Clear;
      Items.Add(RsAllTargets);
      for i := 0 to Installer.SelTargetCount - 1 do
      begin
        with Installer.SelTargets[i] do
        begin
          if InstallJVCL then
          begin
            Items.AddObject(Target.DisplayName, Installer.SelTargets[i]);
            AddIconFileToImageList(ImageListTargets, Target.Executable);
          end;
        end;
      end;
      if Items.Count = 2 then
      begin
        ItemIndex := 1;
        ComboBoxTargetIDE.Enabled := False;
      end
      else
      begin
        ComboBoxTargetIDE.Enabled := True;
        ItemIndex := 0;
      end;
    end;
    ComboBoxTargetIDEChange(ComboBoxTargetIDE);
    
   // jvcl.inc
    LblDxgettextHomepage.Left := CheckBoxDxgettextSupport.Left + 16;
    LblDxgettextHomepage.Top := CheckBoxDxgettextSupport.Top;
    LblDxgettextHomepage.OnClick := Installer.DoHomepageClick;
    LblDxgettextHomepage.Caption := CheckBoxDxgettextSupport.Caption;

    CheckBoxDxgettextSupport.Visible := Installer.Data.IsDxgettextInstalled;
    LblDxgettextHomepage.Visible := not Installer.Data.IsDxgettextInstalled;

   // common options 
    CheckBoxCompileJclDcp.Checked := Installer.Data.CompileJclDcp;
    CheckBoxVerbose.Checked := Installer.Data.Verbose;

    UpdateJvclIncSettings;
  finally
    Dec(FInitializing);
  end;
end;

procedure TFrameConfigPage.CheckBoxDeveloperInstallClick(Sender: TObject);
var
  TargetConfig: TTargetConfig;
begin
  if FInitializing > 0 then
    Exit;
  if TCheckBox(Sender).State = cbGrayed then
    TCheckBox(Sender).State := cbChecked;

  CheckBoxDeveloperInstall.Enabled := not CheckBoxCompileOnly.Checked;
  CheckBoxCleanPalettes.Enabled := not CheckBoxCompileOnly.Checked;

  if ComboBoxTargetIDE.ItemIndex <= 0 then
  begin
    if Sender = CheckBoxDeveloperInstall then
      Installer.Data.DeveloperInstall := Integer(CheckBoxDeveloperInstall.Checked)
    else if Sender = CheckBoxCleanPalettes then
      Installer.Data.CleanPalettes := Integer(CheckBoxCleanPalettes.Checked)
    else if Sender = CheckBoxBuild then
      Installer.Data.Build := Integer(CheckBoxBuild.Checked)
    else if Sender = CheckBoxCompileOnly then
      Installer.Data.CompileOnly := Integer(CheckBoxCompileOnly.Checked)
    else if Sender = CheckBoxGenerateMapFiles then
      Installer.Data.GenerateMapFiles := Integer(CheckBoxGenerateMapFiles.Checked)
    ;
  end
  else
  begin
    TargetConfig := SelTargetConfig;
    if Sender = CheckBoxDeveloperInstall then
      TargetConfig.DeveloperInstall := CheckBoxDeveloperInstall.Checked
    else if Sender = CheckBoxCleanPalettes then
      TargetConfig.CleanPalettes := CheckBoxCleanPalettes.Checked
    else if Sender = CheckBoxBuild then
      TargetConfig.Build := CheckBoxBuild.Checked
    else if Sender = CheckBoxCompileOnly then
      TargetConfig.CompileOnly := CheckBoxCompileOnly.Checked
    else if Sender = CheckBoxGenerateMapFiles then
      TargetConfig.GenerateMapFiles := CheckBoxGenerateMapFiles.Checked
    ;
  end;
  PackageInstaller.UpdatePages;
end;

procedure TFrameConfigPage.CheckBoxCompileJclDcpClick(Sender: TObject);
begin
  if FInitializing > 0 then
    Exit;
  if Sender = CheckBoxCompileJclDcp then
    Installer.Data.CompileJclDcp := CheckBoxCompileJclDcp.Checked
  else if Sender = CheckBoxVerbose then
    Installer.Data.Verbose := CheckBoxVerbose.Checked
  ;
end;

procedure TFrameConfigPage.UpdateJvclIncSettings;
begin
  with Installer.Data do
  begin
    CheckBoxXPTheming.Checked := JVCLConfig.Enabled['JVCLThemesEnabled'];
    CheckBoxRegisterGlobalDesignEditors.Checked := JVCLConfig.Enabled['JVCL_REGISTER_GLOBAL_DESIGNEDITORS'];
    CheckBoxDxgettextSupport.Checked := JVCLConfig.Enabled['USE_DXGETTEXT'];
    CheckBoxRegisterJvGif.Checked := JVCLConfig.Enabled['USE_JV_GIF'];
    CheckBoxUseJVCL.Checked := JVCLConfig.Enabled['USEJVCL'];
  end;
end;

procedure TFrameConfigPage.CheckBoxXPThemingClick(Sender: TObject);
begin
  if FInitializing > 0 then
    Exit;

  with Installer.Data do
  begin
    JVCLConfig.Enabled['JVCLThemesEnabled'] := CheckBoxXPTheming.Checked;
    JVCLConfig.Enabled['JVCL_REGISTER_GLOBAL_DESIGNEDITORS'] := CheckBoxRegisterGlobalDesignEditors.Checked;
    JVCLConfig.Enabled['USE_DXGETTEXT'] := CheckBoxDxgettextSupport.Checked;
    JVCLConfig.Enabled['USE_JV_GIF'] := CheckBoxRegisterJvGif.Checked;
    JVCLConfig.Enabled['USEJVCL'] := CheckBoxUseJVCL.Checked;
  end;
end;

procedure TFrameConfigPage.ComboBoxTargetIDEChange(Sender: TObject);
var
  TargetConfig: TTargetConfig;
  ItemIndex: Integer;
begin
  Inc(FInitializing);
  try
    ItemIndex := ComboBoxTargetIDE.ItemIndex;

    if ItemIndex <= 0 then
    begin
      CheckBoxDeveloperInstall.State := TCheckBoxState(Installer.Data.DeveloperInstall);
      CheckBoxCleanPalettes.State := TCheckBoxState(Installer.Data.CleanPalettes);
      CheckBoxBuild.State := TCheckBoxState(Installer.Data.Build);
      CheckBoxCompileOnly.State := TCheckBoxState(Installer.Data.CompileOnly);
      CheckBoxGenerateMapFiles.State := TCheckBoxState(Installer.Data.GenerateMapFiles);
    end
    else
    begin
      TargetConfig := SelTargetConfig;

      CheckBoxDeveloperInstall.Checked := TargetConfig.DeveloperInstall;
      CheckBoxCleanPalettes.Checked := TargetConfig.CleanPalettes;
      CheckBoxBuild.Checked := TargetConfig.Build;
      CheckBoxCompileOnly.Checked := TargetConfig.CompileOnly;
      CheckBoxGenerateMapFiles.Checked := TargetConfig.GenerateMapFiles;

      FrameDirEditBrowseBPL.EditDirectory.Text := TargetConfig.BplDir;
      FrameDirEditBrowseDCP.EditDirectory.Text := TargetConfig.DcpDir;
      if TargetConfig.Target.IsBCB then
        FrameDirEditBrowseHPP.EditDirectory.Text := TargetConfig.HppDir;
    end;

    FrameDirEditBrowseBPL.Visible := ItemIndex > 0;
    FrameDirEditBrowseDCP.Visible := ItemIndex > 0;
    FrameDirEditBrowseHPP.Visible := (ItemIndex > 0) and SelTargetConfig.Target.IsBCB;
    LblBCBGuide.Visible := FrameDirEditBrowseHPP.Visible;
  finally
    Dec(FInitializing);
  end;
end;

procedure TFrameConfigPage.ComboBoxTargetIDEDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  i: Integer;
begin
  with TComboBox(Control), TComboBox(Control).Canvas do
  begin
    FillRect(Rect);
    if Index > 0 then
    begin
      ImageListTargets.Draw(TComboBox(Control).Canvas, Rect.Left + 1, Rect.Top, Index - 1);
      Inc(Rect.Left, ImageListTargets.Width + 3);
    end
    else
      Inc(Rect.Left, 3);
    TextRect(Rect, Rect.Left, Rect.Top + 1, Items[Index]);
    if Index = 0 then
    begin
      Inc(Rect.Left, TextWidth(Items[Index]) + 2);
      for i := 0 to ImageListTargets.Count - 1 do
      begin
        ImageListTargets.Draw(TComboBox(Control).Canvas, Rect.Left + 1, Rect.Top, i);
        Inc(Rect.Left, ImageListTargets.Width + 3);
      end;
    end;
  end;
end;

procedure TFrameConfigPage.BtnEditJvclIncClick(Sender: TObject);
begin
  if FormJvclIncConfig.Config = nil then
  begin
    FormJvclIncConfig.Config := Installer.Data.JVCLConfig;
    FormJvclIncConfig.imgProjectJEDI.Picture.Assign(FormMain.ImageLogo.Picture);
  end;
  FormJvclIncConfig.UpdateCheckStates;
  FormJvclIncConfig.ShowModal;
  UpdateJvclIncSettings;
end;

procedure TFrameConfigPage.LblDxgettextHomepageClick(Sender: TObject);
begin
  Installer.DoHomepageClick(Sender);
end;

procedure TFrameConfigPage.LblBCBGuideClick(Sender: TObject);
var
  Filename: string;
begin
  Filename := Installer.Data.JVCLDir + '\' + SInstallHTM;
  if not OpenAtAnchor(Filename, SBCBGuideAnchor) then
    MessageDlg(Format(RsCannotOpen, [Filename]), mtError, [mbOk], 0);
end;

end.
