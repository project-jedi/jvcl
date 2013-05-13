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

unit FrmConfigPage;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  ShellAPI, CommCtrl,
  JvConsts,
  JVCL3Install, JvExStdCtrls, JVCLData, ImgList, FrmDirEditBrowse;

type
  TFrameConfigPage = class(TFrame)
    GroupBoxJvclInc: TGroupBox;
    CheckBoxXPTheming: TCheckBox;
    CheckBoxRegisterGlobalDesignEditors: TCheckBox;
    CheckBoxDxgettextSupport: TCheckBox;
    CheckBoxRegisterJvGif: TCheckBox;
    GroupBoxInstallOptions: TGroupBox;
    CheckBoxDeveloperInstall: TCheckBox;
    CheckBoxCleanPalettes: TCheckBox;
    ImageListTargets: TImageList;
    CheckBoxBuild: TCheckBox;
    CheckBoxIDERegister: TCheckBox;
    FrameDirEditBrowseBPL: TFrameDirEditBrowse;
    FrameDirEditBrowseHPP: TFrameDirEditBrowse;
    LblBCBGuide: TLabel;
    CheckBoxVerbose: TCheckBox;
    CheckBoxGenerateMapFiles: TCheckBox;
    CheckBoxUnitVersioning: TCheckBox;
    CheckBoxIgnoreMakeErrors: TCheckBox;
    ComboBoxTargetIDE: TComboBox;
    BtnEditJvclInc: TButton;
    PanelBk: TPanel;
    LblOptionsFor: TLabel;
    CheckBoxDebugUnits: TCheckBox;
    CheckBoxLinkMapFiles: TCheckBox;
    CheckBoxDeleteMapFiles: TCheckBox;
    LblEnvPathWarning: TLabel;
    CheckBoxAddBplDirToPath: TCheckBox;
    CheckBoxCreateJdbgFiles: TCheckBox;
    procedure FrameDirEditBrowseBPLEditDirectoryChange(Sender: TObject);
    procedure CheckBoxDeveloperInstallClick(Sender: TObject);
    procedure CheckBoxXPThemingClick(Sender: TObject);
    procedure ComboBoxTargetIDEChange(Sender: TObject);
    procedure ComboBoxTargetIDEDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BtnEditJvclIncClick(Sender: TObject);
    procedure LblDxgettextHomepageClick(Sender: TObject);
    procedure LblBCBGuideClick(Sender: TObject);
    procedure CheckBoxCompileJclDcpClick(Sender: TObject);
    procedure FrameDirEditBrowseHPPBtnJCLDirBrowseClick(Sender: TObject);
  private
    FInitializing: Integer;
    FInstaller: TInstaller;
    procedure Init;
    procedure UpdateJvclIncSettings;
    function GetSelTargetConfig: TTargetConfig;
    procedure BplDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
    procedure HppDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
    procedure SetJVCLConfig(const Id: string; CheckBox: TCheckBox);
    procedure GetJVCLConfig(const Id: string; CheckBox: TCheckBox);
    procedure SetJVCLDesigntimeConfig(const Id: string; CheckBox: TCheckBox);
    procedure GetJvclDesigntimeConfig(const Id: string; CheckBox: TCheckBox);
  protected
    property Installer: TInstaller read FInstaller;

    property SelTargetConfig: TTargetConfig read GetSelTargetConfig;
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameConfigPage;
  end;

implementation

uses
  InstallerConsts, Core, MainConfig, Main, Utils, Math, JVCLConfiguration,
  DelphiData, PackageInformation;

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
  SelTargetConfig.BplDir := ExcludeTrailingPathDelimiter(Dir);
end;

procedure TFrameConfigPage.HppDirChanged(Sender, UserData: TObject;
  var Dir: string);
begin
  SelTargetConfig.HppDir := ExcludeTrailingPathDelimiter(Dir);
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

    CheckBoxVerbose.Checked := Installer.Data.Verbose;
    CheckBoxIgnoreMakeErrors.Checked := Installer.Data.IgnoreMakeErrors;

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

  CheckBoxDebugUnits.Enabled := not CheckBoxDeveloperInstall.Checked;
  CheckBoxCleanPalettes.Enabled := CheckBoxIDERegister.Checked;
  CheckBoxLinkMapFiles.Enabled := CheckBoxGenerateMapFiles.Checked;
  CheckBoxCreateJdbgFiles.Enabled := CheckBoxGenerateMapFiles.Checked;
  CheckBoxDeleteMapFiles.Enabled := CheckBoxGenerateMapFiles.Checked and
           (CheckBoxLinkMapFiles.Checked or CheckBoxCreateJdbgFiles.Checked);

  if ComboBoxTargetIDE.ItemIndex <= 0 then
  begin
    if Sender = CheckBoxDeveloperInstall then
      Installer.Data.DeveloperInstall := Integer(CheckBoxDeveloperInstall.Checked)
    else if Sender = CheckBoxDebugUnits then
      Installer.Data.DebugUnits := Integer(CheckBoxDebugUnits.Checked)
    else if Sender = CheckBoxCleanPalettes then
      Installer.Data.CleanPalettes := Integer(CheckBoxCleanPalettes.Visible and CheckBoxCleanPalettes.Checked)
    else if Sender = CheckBoxBuild then
      Installer.Data.Build := Integer(CheckBoxBuild.Checked)
    else if Sender = CheckBoxIDERegister then
      Installer.Data.CompileOnly := Integer(not (CheckBoxIDERegister.Visible and CheckBoxIDERegister.Checked))
    else if Sender = CheckBoxGenerateMapFiles then
      Installer.Data.GenerateMapFiles := Integer(CheckBoxGenerateMapFiles.Checked)
    else if Sender = CheckBoxLinkMapFiles then
      Installer.Data.LinkMapFiles := Integer(CheckBoxLinkMapFiles.Checked)
    else if Sender = CheckBoxCreateJdbgFiles then
      Installer.Data.CreateJdbgFiles := Integer(CheckBoxCreateJdbgFiles.Checked)
    else if Sender = CheckBoxDeleteMapFiles then
      Installer.Data.DeleteMapFiles := Integer(CheckBoxDeleteMapFiles.Checked)
    ;
  end
  else
  begin
    TargetConfig := SelTargetConfig;
    if Sender = CheckBoxDeveloperInstall then
      TargetConfig.DeveloperInstall := CheckBoxDeveloperInstall.Checked
    else if Sender = CheckBoxDebugUnits then
      TargetConfig.DebugUnits := CheckBoxDebugUnits.Checked
    else if Sender = CheckBoxCleanPalettes then
      TargetConfig.CleanPalettes := CheckBoxCleanPalettes.Visible and CheckBoxCleanPalettes.Checked
    else if Sender = CheckBoxBuild then
      TargetConfig.Build := CheckBoxBuild.Checked
    else if Sender = CheckBoxIDERegister then
      TargetConfig.CompileOnly := not (CheckBoxIDERegister.Visible and CheckBoxIDERegister.Checked)
    else if Sender = CheckBoxGenerateMapFiles then
      TargetConfig.GenerateMapFiles := CheckBoxGenerateMapFiles.Checked
    else if Sender = CheckBoxLinkMapFiles then
      TargetConfig.LinkMapFiles := CheckBoxLinkMapFiles.Checked
    else if Sender = CheckBoxCreateJdbgFiles then
      TargetConfig.CreateJdbgFiles := CheckBoxCreateJdbgFiles.Checked
    else if Sender = CheckBoxDeleteMapFiles then
      TargetConfig.DeleteMapFiles := CheckBoxDeleteMapFiles.Checked
    else if Sender = CheckBoxAddBplDirToPath then // only for SelTargetConfig
      TargetConfig.AddBplDirToPath := CheckBoxAddBplDirToPath.Checked;
    ;
  end;

  PackageInstaller.UpdatePages;
end;

procedure TFrameConfigPage.CheckBoxCompileJclDcpClick(Sender: TObject);
begin
  if FInitializing > 0 then
    Exit;
  if Sender = CheckBoxVerbose then
    Installer.Data.Verbose := CheckBoxVerbose.Checked
  else if Sender = CheckBoxIgnoreMakeErrors then
    Installer.Data.IgnoreMakeErrors := CheckBoxIgnoreMakeErrors.Checked
  ;
end;

procedure TFrameConfigPage.GetJVCLConfig(const Id: string; CheckBox: TCheckBox);
var
  i, e, Count: Integer;
begin
  CheckBox.AllowGrayed := False;

  if SelTargetConfig = nil then
  begin
    // for all
    e := 0;
    Count := 0;
    for i := 0 to Installer.SelTargetCount - 1 do
      if Installer.SelTargets[i].InstallJVCL then
      begin
        Inc(Count);
        if Installer.SelTargets[i].JVCLConfig.Enabled[Id] then
          Inc(e);
      end;

    if e = 0 then
      CheckBox.Checked := False
    else if e = Count then
      CheckBox.Checked := True
    else
    begin
      CheckBox.AllowGrayed := True;
      CheckBox.State := cbGrayed;
    end;
  end
  else
    CheckBox.Checked := SelTargetConfig.JVCLConfig.Enabled[Id];
end;

procedure TFrameConfigPage.SetJVCLConfig(const Id: string; CheckBox: TCheckBox);
var
  i: Integer;
begin
  if SelTargetConfig = nil then
  begin
    // for all
    for i := 0 to Installer.SelTargetCount - 1 do
    begin
      if Installer.SelTargets[i].InstallJVCL and (CheckBox.State <> cbGrayed) then
      begin
        Installer.SelTargets[i].JVCLConfig.Enabled[Id] := CheckBox.Checked;
        CheckBox.AllowGrayed := False;
      end;
    end;
  end
  else
    SelTargetConfig.JVCLConfig.Enabled[Id] := CheckBox.Checked;
end;

procedure TFrameConfigPage.GetJvclDesigntimeConfig(const Id: string; CheckBox: TCheckBox);
var
  i, e, Count: Integer;
begin
  CheckBox.AllowGrayed := False;

  if SelTargetConfig = nil then
  begin
    // for all
    e := 0;
    Count := 0;
    for i := 0 to Installer.SelTargetCount - 1 do
      if Installer.SelTargets[i].InstallJVCL then
      begin
        Inc(Count);
        if Installer.SelTargets[i].JVCLRegistryConfig.Enabled[Id] then
          Inc(e);
      end;

    if e = 0 then
      CheckBox.Checked := False
    else if e = Count then
      CheckBox.Checked := True
    else
    begin
      CheckBox.AllowGrayed := True;
      CheckBox.State := cbGrayed;
    end;
  end
  else
    CheckBox.Checked := SelTargetConfig.JVCLRegistryConfig.Enabled[Id];
end;

procedure TFrameConfigPage.SetJvclDesigntimeConfig(const Id: string; CheckBox: TCheckBox);
var
  i: Integer;
begin
  if SelTargetConfig = nil then
  begin
    // for all
    for i := 0 to Installer.SelTargetCount - 1 do
    begin
      if Installer.SelTargets[i].InstallJVCL and (CheckBox.State <> cbGrayed) then
      begin
        Installer.SelTargets[i].JVCLRegistryConfig.Enabled[Id] := CheckBox.Checked;
        CheckBox.AllowGrayed := False;
      end;
    end;
  end
  else
    SelTargetConfig.JVCLRegistryConfig.Enabled[Id] := CheckBox.Checked;
end;

procedure TFrameConfigPage.UpdateJvclIncSettings;
begin
  if (SelTargetConfig <> nil) and (SelTargetConfig.Target.Version >= 7) then
  begin
    CheckBoxXPTheming.Enabled := False;
    CheckBoxXPTheming.Checked := True;
  end
  else
  begin
    CheckBoxXPTheming.Enabled := True;
    GetJVCLConfig('JVCLThemesEnabled', CheckBoxXPTheming);
  end;

  GetJvclDesigntimeConfig('RegisterGlobalDesignEditors', CheckBoxRegisterGlobalDesignEditors);
  GetJVCLConfig('USE_DXGETTEXT', CheckBoxDxgettextSupport);
  GetJVCLConfig('USE_JV_GIF', CheckBoxRegisterJvGif);
  GetJVCLConfig('UNITVERSIONING', CheckBoxUnitVersioning);
end;

procedure TFrameConfigPage.CheckBoxXPThemingClick(Sender: TObject);
var
  i: Integer;
begin
  if FInitializing > 0 then
    Exit;

  if (Sender = CheckBoxXPTheming) and (CheckBoxXPTheming.Tag = 0) then
  begin
    CheckBoxXPTheming.Tag := 1;
    // Is a Delphi/BCB version below 7 installed
    for i := 0 to Installer.SelTargetCount - 1 do
    begin
      if Installer.SelTargets[i].InstallJVCL then
      begin
        if Installer.SelTargets[i].Target.Version < 7 then
        begin
          // give the user a hint to the readme.htm
          if FileExists(Installer.Data.JVCLDir + PathDelim + SReadmeHTM) then
          begin
            if MessageDlg(RsReadReadmeForXPThemingInfo, mtInformation,
              [mbYes, mbNo], 0) = mrYes then
              if not OpenAtAnchor(Installer.Data.JVCLDir + PathDelim + SReadmeHTM, 'Theming') then
                MessageDlg(RsErrorOpeningReadmeHTM, mtError, [mbOk], 0);
          end;
          Break;
        end;
      end;
    end;
  end;

  try
    SetJVCLConfig('JVCLThemesEnabled', CheckBoxXPTheming);
    SetJvclDesigntimeConfig('RegisterGlobalDesignEditors', CheckBoxRegisterGlobalDesignEditors);
    SetJVCLConfig('USE_DXGETTEXT', CheckBoxDxgettextSupport);
    SetJVCLConfig('USE_JV_GIF', CheckBoxRegisterJvGif);
    SetJVCLConfig('UNITVERSIONING', CheckBoxUnitVersioning);
  except
    on E: Exception do
      MessageDlg(RsJVCLConfigurationError, mtError, [mbOk], 0);
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
      // for all
      BtnEditJvclInc.Caption := RsEditJvclIncAll;

      CheckBoxDeveloperInstall.State := TCheckBoxState(Installer.Data.DeveloperInstall);
      CheckBoxCleanPalettes.State := TCheckBoxState(Installer.Data.CleanPalettes);
      CheckBoxBuild.State := TCheckBoxState(Installer.Data.Build);
      case TCheckBoxState(Installer.Data.CompileOnly) of
        cbUnchecked:
          CheckBoxIDERegister.State := cbChecked; // invert
        cbChecked:
          CheckBoxIDERegister.State := cbUnchecked; // invert
      else
        CheckBoxIDERegister.State := cbGrayed;
      end;
      CheckBoxGenerateMapFiles.State := TCheckBoxState(Installer.Data.GenerateMapFiles);
      CheckBoxLinkMapFiles.State := TCheckBoxState(Installer.Data.LinkMapFiles);
      CheckBoxCreateJdbgFiles.State := TCheckBoxState(Installer.Data.CreateJdbgFiles);
      CheckBoxDeleteMapFiles.State := TCheckBoxState(Installer.Data.DeleteMapFiles);
      CheckBoxDebugUnits.State := TCheckBoxState(Installer.Data.DebugUnits);

      CheckBoxAddBplDirToPath.Checked := False;
      LblEnvPathWarning.Visible := False;
      CheckBoxAddBplDirToPath.Visible := False;
    end
    else
    begin
      // for selected
      TargetConfig := SelTargetConfig;
      BtnEditJvclInc.Caption := Format(RsEditJvclInc, [ExtractFileName(TargetConfig.JVCLConfig.Filename)]);

      CheckBoxDeveloperInstall.Checked := TargetConfig.DeveloperInstall;
      CheckBoxDebugUnits.Checked := TargetConfig.DebugUnits;
      CheckBoxCleanPalettes.Checked := TargetConfig.CleanPalettes;
      CheckBoxBuild.Checked := TargetConfig.Build;
      CheckBoxIDERegister.Checked := not TargetConfig.CompileOnly;
      CheckBoxGenerateMapFiles.Checked := TargetConfig.GenerateMapFiles;
      CheckBoxLinkMapFiles.Checked := TargetConfig.LinkMapFiles;
      CheckBoxCreateJdbgFiles.Checked := TargetConfig.CreateJdbgFiles;
      CheckBoxDeleteMapFiles.Checked := TargetConfig.DeleteMapFiles;

      FrameDirEditBrowseBPL.EditDirectory.Text := TargetConfig.BplDir;
      if TargetConfig.Target.SupportsPersonalities([persBCB]) then
        FrameDirEditBrowseHPP.EditDirectory.Text := TargetConfig.HppDir;

      CheckBoxAddBplDirToPath.Checked := TargetConfig.AddBplDirToPath;
    end;

    CheckBoxIDERegister.Visible := (ItemIndex = 0) or (SelTargetConfig.Target.Platform <> ctpWin64);
    CheckBoxCleanPalettes.Visible := CheckBoxIDERegister.Visible;
    CheckBoxCleanPalettes.Enabled := CheckBoxIDERegister.Checked;
    CheckBoxDebugUnits.Enabled := not CheckBoxDeveloperInstall.Checked;
    CheckBoxLinkMapFiles.Enabled := CheckBoxGenerateMapFiles.Checked;
    CheckBoxCreateJdbgFiles.Enabled := CheckBoxGenerateMapFiles.Checked;
    CheckBoxDeleteMapFiles.Enabled := (CheckBoxLinkMapFiles.Checked or CheckBoxCreateJdbgFiles.Checked)
      and CheckBoxGenerateMapFiles.Checked;
    FrameDirEditBrowseBPL.Visible := ItemIndex > 0;
    FrameDirEditBrowseHPP.Visible := (ItemIndex > 0) and SelTargetConfig.Target.SupportsPersonalities([persBCB]);
    //LblBCBGuide.Visible := FrameDirEditBrowseHPP.Visible;

    UpdateJvclIncSettings;
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
var
  S: string;
begin
  if SelTargetConfig = nil then
  begin
    MessageDlg(RsEditJvclIncAllError, mtError, [mbOk], 0);
    Exit;
  end;

  if FormJvclIncConfig.imgProjectJEDI.Picture.Graphic = nil then
    FormJvclIncConfig.imgProjectJEDI.Picture.Assign(FormMain.ImageLogo.Picture);

  FormJvclIncConfig.Config.Assign(SelTargetConfig.JVCLConfig);
  S := SelTargetConfig.Target.PlatformName;
  if S <> '' then
    S := ' ' + S;
  if FormJvclIncConfig.Execute(SelTargetConfig.Target.Name + ' ' + SelTargetConfig.Target.VersionStr + S) then
  begin
    if FormJvclIncConfig.Config.Modified then
    begin
      SelTargetConfig.JVCLConfig.Assign(FormJvclIncConfig.Config);
      SelTargetConfig.JVCLConfig.Modified := True;
    end;
  end;
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

procedure TFrameConfigPage.FrameDirEditBrowseBPLEditDirectoryChange(
  Sender: TObject);
var
  Dir: string;
begin
  FrameDirEditBrowseBPL.EditDirectoryChange(Sender);

  Dir := FrameDirEditBrowseBPL.EditDirectory.Text;
  if (SelTargetConfig <> nil) {and DirectoryExists(Dir)} then
  begin
    //there is no need to check the env path for the 64-bit target, because there is no 64-bit IDE yet
    if (not SelTargetConfig.Target.IsInEnvPath(Dir)) and (SelTargetConfig.Target.Platform <> ctpWin64) then
    begin
      if DirectoryExists(Dir) then
        FrameDirEditBrowseBPL.EditDirectory.Font.Color := clBlue;
      LblEnvPathWarning.Visible := True;
      CheckBoxAddBplDirToPath.Visible := True;
      Exit;
    end;
  end;
  CheckBoxAddBplDirToPath.Visible := False;
  LblEnvPathWarning.Visible := False;
end;

procedure TFrameConfigPage.FrameDirEditBrowseHPPBtnJCLDirBrowseClick(
  Sender: TObject);
begin
  FrameDirEditBrowseHPP.BtnDirBrowseClick(Sender);
end;

end.
