{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmMain.pas, released on 2003-11-27.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}
{$I windowsonly.inc}

{
command line switches:
  -NoUpdateCheck     Do not check for installed updates
  -IgnoreIDE         Do not check for running Delphi/BCB instances
  -NoDelphi          Ignore Delphi product targets      (see CoreData.pas.TTargetList.GetTargets)
  -NoBCB             Ignore C++Builder product targets  (see CoreData.pas.TTargetList.GetTargets)
}

unit FrmMain;
interface
{$IFDEF COMPILER6_UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, ComCtrls, ImgList, CommCtrl, ActnList, Buttons,
  Menus, ShellAPI, FileCtrl,
  CoreData, BuildHelpers;

type
  TFormMain = class(TForm)
    PanelBottom: TPanel;
    LblVersions: TLabel;
    LblPackages: TLabel;
    BtnQuit: TBitBtn;
    BtnInstall: TBitBtn;
    ListViewPackages: TListView;
    ListViewTargets: TListView;
    ImageListPackages: TImageList;
    BtnUninstall: TBitBtn;
    ImageListTargets: TImageList;
    ActionList1: TActionList;
    ActionInstall: TAction;
    ActionUninstall: TAction;
    ImageOpen: TImage;
    imgProjectJEDI: TImage;
    BevelTop: TBevel;
    PopupMenuPackages: TPopupMenu;
    MenuSelectAll: TMenuItem;
    MenuSelectNone: TMenuItem;
    MenuInvertSelection: TMenuItem;
    GroupBoxOptions: TGroupBox;
    BtnAdvancedOptions: TBitBtn;
    CheckBoxOptTheming: TCheckBox;
    CheckBoxOptRegGlobalDsgnEditor: TCheckBox;
    CheckBoxOptDxgettext: TCheckBox;
    CheckBoxOptJvGIF: TCheckBox;
    CheckBoxShowRuntimePackages: TCheckBox;
    Panel1: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    CheckBoxClearJVCLPalette: TCheckBox;
    CheckBoxBuild: TCheckBox;
    CheckBoxDeveloperInstall: TCheckBox;
    CheckBoxInstallJcl: TCheckBox;
    CheckBoxCompileOnly: TCheckBox;
    EditHppFilesDir: TEdit;
    BtnHppFilesBrowse: TButton;
    LblBCBInstallation: TLabel;
    CheckBoxHppFilesDir: TCheckBox;
    Bevel7: TBevel;
    CheckBoxPersonalEdition: TCheckBox;
    Bevel8: TBevel;
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnAdvancedOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewTargetsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure CheckBoxOptThemingClick(Sender: TObject);
    procedure ListViewTargetsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListViewPackagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure BtnInstallClick(Sender: TObject);
    procedure CheckBoxClearJVCLPaletteClick(Sender: TObject);
    procedure ActionInstallUpdate(Sender: TObject);
    procedure BtnUninstallClick(Sender: TObject);
    procedure imgProjectJEDIClick(Sender: TObject);
    procedure MenuInvertSelectionClick(Sender: TObject);
    procedure imgProjectJEDIMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LblVersionsClick(Sender: TObject);
    procedure LblPackagesClick(Sender: TObject);
    procedure CheckBoxShowRuntimePackagesClick(Sender: TObject);
    procedure BtnHppFilesBrowseClick(Sender: TObject);
    procedure LblBCBInstallationClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FXPThemeSupportFirstClick: Boolean;
    FInFillingList: Boolean;
    FLockOptClick: Integer;
    FLockPackageCheckStates: Integer;
    function GetSelTarget: TTargetInfo;
    procedure FillTargetList;
    procedure UpdateTargetList;
    procedure Update_JVCL_INC_Config;
    procedure UpdatePackageList;
    procedure UpdatePackageCheckStates;

    procedure TransferTargetOptions(ToTarget: Boolean);

    function CheckTargetUpdates(Target: TTargetInfo): Boolean;
      // ** CheckTargetUpdates is responsilbe for UPDATE warnings **
  public
    { Public-Deklarationen }
    property SelTarget: TTargetInfo read GetSelTarget;
  end;

var
  FormMain: TFormMain;

implementation
uses
  MainConfig, JVCLConfiguration, FrmMake, AHCompBrowseFolderDlg;

{$R *.dfm}

function OpenAtAnchor(const FileName, Anchor: string): Boolean;
var
  Cmd: string;
begin
  SetLength(Cmd, MAX_PATH);
  Result := FindExecutable(PChar(FileName), nil, PChar(Cmd)) > 32;
  SetLength(Cmd, StrLen(PChar(Cmd)));
  if Result then
    Result := ShellExecute(0, 'open', PChar(Cmd), PChar(FileName + '#' + Anchor), nil,
      SW_SHOWNORMAL) > 32;
end;

function NoYesDlg(const Text: string): TModalResult;
var Dlg: TForm;
begin
  Dlg := CreateMessageDialog(Text, mtConfirmation, [mbYes, mbNo]);
  try
    if Dlg.FindComponent('No') is TWinControl then
      Dlg.ActiveControl := TWinControl(Dlg.FindComponent('No'));
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function TFormMain.GetSelTarget: TTargetInfo;
begin
  if ListViewTargets.Selected = nil then
    Result := nil
  else
    Result := TTargetInfo(ListViewTargets.Selected.Data);
end;

procedure TFormMain.FillTargetList;
var
  i: Integer;
  ListItem: TListItem;
  FileInfo: TShFileInfo;
begin
  FInFillingList := True;
  ListViewTargets.Items.BeginUpdate;
  try
    ImageListTargets.Clear;
    ListViewTargets.Items.Clear;
    for i := 0 to TargetList.Count - 1 do
    begin
      TargetList[i].Packages.ReadPackages;
      ListItem := ListViewTargets.Items.Add;
      CheckTargetUpdates(TargetList[i]);
      ListItem.Checked := TargetList[i].CompileFor;
      ListItem.Caption := TargetList[i].ProductName;
      ListItem.SubItems.Add(TargetList[i].Version + ' ' + TargetList[i].VersionType);
      ListItem.SubItems.Add('-');

      if FileExists(TargetList[i].Executable) then
      begin
        FillChar(FileInfo, SizeOf(FileInfo), 0);
        SHGetFileInfo(PChar(TargetList[i].Executable), 0, FileInfo, SizeOf(FileInfo),
          SHGFI_ICON or SHGFI_SMALLICON);
        if FileInfo.hIcon <> 0 then
        begin
          ImageList_AddIcon(ImageListTargets.Handle, FileInfo.hIcon);
          DestroyIcon(FileInfo.hIcon);
          ListItem.ImageIndex := ImageListTargets.Count - 1;
        end;
      end;

      ListItem.Data := TargetList[i];
    end;
    UpdateTargetList;
  finally
    ListViewTargets.Items.EndUpdate;
    FInFillingList := False;
  end;
end;

procedure TFormMain.UpdateTargetList;
var
  i: Integer;
  ListItem: TListItem;
  Ver: Integer;
begin
  UpdatePackageCheckStates;
  ListViewTargets.Items.BeginUpdate;
  try
    for i := 0 to ListViewTargets.Items.Count - 1 do
    begin
      ListItem := ListViewTargets.Items[i];
      Ver := TTargetInfo(ListItem.Data).IsOldJVCLInstalled;
      if TTargetInfo(ListItem.Data).IsJVCLInstalled then
        ListItem.SubItems[1] := '3'
      else if Ver = 0 then
        ListItem.SubItems[1] := '-'
      else
        ListItem.SubItems[1] := IntToStr(Ver);
    end;
  finally
    ListViewTargets.Items.EndUpdate;
  end;
end;

procedure TFormMain.UpdatePackageList;
var
  i: Integer;
  Packages: TPackageList;
  ListItem: TListItem;
begin
  ListViewPackages.Items.BeginUpdate;
  try
    ListViewPackages.Items.Clear;
    if SelTarget <> nil then
    begin
      Packages := SelTarget.Packages;
      for i := 0 to Packages.Count - 1 do
      begin
        if not CheckBoxShowRuntimePackages.Checked and
           not Packages[i].IsDesign then
          Continue;
        ListItem := ListViewPackages.Items.Add;
        ListItem.Data := Packages[i];
        ListItem.Checked := Packages[i].Install;
        ListItem.Caption := Packages[i].DisplayName;
        ListItem.SubItems.Add(Packages[i].Description);
        if not Packages[i].IsDesign then
          ListItem.ImageIndex := 2 // Runtime package
        else if Packages[i].RequiresDB then
          ListItem.ImageIndex := 1
        else
          ListItem.ImageIndex := 0;
      end;
    end;
  finally
    ListViewPackages.Items.EndUpdate;
  end;
end;

procedure TFormMain.UpdatePackageCheckStates;
var
  i: Integer;
  ListItem: TListItem;
begin
  if FLockPackageCheckStates > 0 then Exit;
  
  for i := 0 to ListViewPackages.Items.Count - 1 do
  begin
    ListItem := ListViewPackages.Items[i];
    ListItem.Checked := TPackageInfo(ListItem.Data).Install;
  end;
end;

procedure TFormMain.Update_JVCL_INC_Config;
begin
  Inc(FLockOptClick);
  try
   // special handling for XP Theme Support (Delphi 7)
    CheckBoxOptTheming.Enabled := (SelTarget <> nil) and (SelTarget.MajorVersion < 7);

   // load configuration
    CheckBoxOptTheming.Checked := FormMainConfig.Config.Enabled['JVCLThemesEnabled'];
    CheckBoxOptRegGlobalDsgnEditor.Checked := FormMainConfig.Config.Enabled['JVCL_REGISTER_GLOBAL_DESIGNEDITORS'];
    CheckBoxOptDxgettext.Checked := FormMainConfig.Config.Enabled['USE_DXGETTEXT'];
    CheckBoxOptJvGIF.Checked := FormMainConfig.Config.Enabled['USE_Jv_GIF'];
  finally
    Dec(FLockOptClick);
  end;
end;

function TFormMain.CheckTargetUpdates(Target: TTargetInfo): Boolean;
var
  NeedsUpdate: Integer;
begin
  if FindCmdSwitch('-NoUpdateCheck') then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if Target.NeedsUpdate then
    Exit;
    
  NeedsUpdate := 0;
  if Target.IsDelphi then
  begin
    case Target.MajorVersion of
      5: NeedsUpdate := 1; // needs at least Update #1
      6: NeedsUpdate := 2; // needs at least Update #2
      7: ;
    end;
  end
  else
  begin
    case Target.MajorVersion of
      5: ;//NeedsUpdate :=
      6: NeedsUpdate := 4; // needs at least Update #4
    end;
  end;
  Target.NeedsUpdate := NeedsUpdate > Target.LastUpdate;
  if Target.NeedsUpdate then
  begin
    Target.CompileFor := False;
    if not FInFillingList then // set in FillTargetList()
      MessageDlg(Format('Please install the latest updates for %s from www.borland.com.',
        [Target.DisplayName]), mtInformation, [mbOk], 0);
  end
  else
    Result := True;
end;

procedure TFormMain.TransferTargetOptions(ToTarget: Boolean);

  procedure SetEnable(e: Boolean);
  begin
    CheckBoxClearJVCLPalette.Enabled := e;
    CheckBoxBuild.Enabled := e;
    CheckBoxDeveloperInstall.Enabled := e;
    CheckBoxInstallJcl.Enabled := e;
    CheckBoxCompileOnly.Enabled := e;
    CheckBoxHppFilesDir.Enabled := e;
    EditHppFilesDir.Enabled := e;
    BtnHppFilesBrowse.Enabled := e;
  end;

var
  Selected: Boolean;
begin
  if FLockOptClick > 0 then Exit;
  Inc(FLockOptClick);
  try
    Selected := SelTarget <> nil;

    if ToTarget then
    begin
      if SelTarget <> nil then
      begin
        SelTarget.ClearJVCLPalette := CheckBoxClearJVCLPalette.Checked;
        SelTarget.Build := CheckBoxBuild.Checked;
        SelTarget.DeveloperInstall := CheckBoxDeveloperInstall.Checked;
        SelTarget.InstallJcl := CheckBoxInstallJcl.Checked;
        SelTarget.CompileOnly := CheckBoxCompileOnly.Checked;
        SelTarget.HppFilesDir := SelTarget.ExpandDirMacros(EditHppFilesDir.Text);
        SelTarget.MoveHppFiles := CheckBoxHppFilesDir.Checked;
      end;
    end

    else // -----------------------------------

    begin
      SetEnable(Selected);
      if Selected then
      begin
        CheckTargetUpdates(SelTarget);
        Update_JVCL_INC_Config;
        CheckBoxClearJVCLPalette.Checked := SelTarget.ClearJVCLPalette;
        CheckBoxBuild.Checked := SelTarget.Build;
        CheckBoxDeveloperInstall.Checked := SelTarget.DeveloperInstall;
        CheckBoxInstallJcl.Checked := SelTarget.InstallJcl;
        if (SelTarget.IsJCLInstalled) and (SelTarget.IsOldJVCLInstalled = 0) then
          CheckBoxInstallJcl.Font.Style := []
        else
          CheckBoxInstallJcl.Font.Style := [fsBold];
        CheckBoxCompileOnly.Checked := SelTarget.CompileOnly;

        if SelTarget.IsBCB then
          EditHppFilesDir.Text := SelTarget.InsertDirMacros(SelTarget.HppFilesDir)
        else
          EditHppFilesDir.Text := '';
        CheckBoxHppFilesDir.Checked := SelTarget.IsBCB and SelTarget.MoveHppFiles;
        CheckBoxHppFilesDir.Enabled := SelTarget.IsBCB;
        BtnHppFilesBrowse.Enabled := SelTarget.IsBCB;
        LblBCBInstallation.Visible := SelTarget.IsBCB;

        CheckBoxPersonalEdition.Checked := SelTarget.IsPersonal or
          FormMainConfig.Config.Enabled['DelphiPersonalEdition'];
      end
      else
      begin
        CheckBoxClearJVCLPalette.Checked := False;
        CheckBoxBuild.Checked := False;
        CheckBoxDeveloperInstall.Checked := False;
        CheckBoxInstallJcl.Checked := False;
        CheckBoxInstallJcl.Font.Style := [];
        CheckBoxCompileOnly.Checked := False;
        CheckBoxHppFilesDir.Checked := False;
        EditHppFilesDir.Text := '';
        CheckBoxPersonalEdition.Checked := False;
      end;
      UpdatePackageList;
    end;

  finally
    Dec(FLockOptClick);
  end;
end;

procedure TFormMain.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.BtnAdvancedOptionsClick(Sender: TObject);
begin
  FormMainConfig.UpdateCheckStates;
  FormMainConfig.ShowModal;
  Update_JVCL_INC_Config;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.HintHidePause := 10000;
  if not FindCmdSwitch('-IgnoreIDE') then
  begin
    if IsDelphiRunning then
    begin
      MessageDlg('Please close all Delphi/BCB instances before starting the JVCL installation.'#10#10 +
        'JVCL 3 Package Installer terminated.', mtError, [mbOk], 0);
      Application.Terminate;
      Application.ShowMainForm := False;
      Exit;
    end;
  end;

   // load user configuration, config is saved in BtnInstallClick
  TargetList.LoadFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  FXPThemeSupportFirstClick := True;
  FillTargetList;
end;

procedure TFormMain.FormShow(Sender: TObject);
var Index: Integer;
begin
 // prepare JVCLConfig sub-project
  FormMainConfig.Filename := JVCLDir + '\common\JVCL.INC';
  FormMainConfig.BtnQuit.Caption := '&Close';
  FormMainConfig.BtnReload.Click;
  FormMainConfig.BtnSave.Glyph := BtnInstall.Glyph;
  FormMainConfig.BtnQuit.Glyph := BtnQuit.Glyph;
  FormMainConfig.BtnReload.Glyph := ImageOpen.Picture.Bitmap;

  Update_JVCL_INC_Config;
  UpdatePackageList;

 // select first target
  Index := 0;
  while (Index < ListViewTargets.Items.Count) and
        (TTargetInfo(ListViewTargets.Items[Index].Data).NeedsUpdate) do
    Inc(Index);
  if Index >= ListViewTargets.Items.Count then
    Index := 0;
  if Index < ListViewTargets.Items.Count then
   ListViewTargets.Items[Index].Selected := True;
end;

procedure TFormMain.ListViewTargetsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  TransferTargetOptions(False);
end;

procedure TFormMain.CheckBoxOptThemingClick(Sender: TObject);
begin
  if FLockOptClick > 0 then Exit;
  Inc(FLockOptClick);
  try
    if Sender = CheckBoxOptTheming then
    begin
      if FXPThemeSupportFirstClick then
      begin
        if CheckBoxOptTheming.Checked then
        begin
          FXPThemeSupportFirstClick := False;
          if FileExists(JVCLDir + '\readme.htm') then
          begin
            if MessageDlg('Please read the readme.htm for details about theming with Delphi/BCB 5 and 6.'#10 +
              'Do you want to open readme.htm ?',
              mtWarning, [mbYes, mbNo], 0) = mrYes then
              if not OpenAtAnchor(JVCLDir + '\readme.htm', 'Theming') then
                MessageDlg('Cannot open readme.htm', mtError, [mbOk], 0);
          end
          else
            MessageDlg('Please read the readme.htm for details about theming with Delphi/BCB 5 and 6.',
              mtWarning, [mbOk], 0);
        end;
      end;
      FormMainConfig.Config.Enabled['JVCLThemesEnabled'] := CheckBoxOptTheming.Checked;
    end
    else
    begin
     // save configuration
      FormMainConfig.Config.Enabled['JVCL_REGISTER_GLOBAL_DESIGNEDITORS'] := CheckBoxOptRegGlobalDsgnEditor.Checked;
      FormMainConfig.Config.Enabled['USE_DXGETTEXT'] := CheckBoxOptDxgettext.Checked;
      FormMainConfig.Config.Enabled['USE_Jv_GIF'] := CheckBoxOptJvGIF.Checked;
    end;
  finally
    Dec(FLockOptClick);
  end;
end;

procedure TFormMain.imgProjectJEDIMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (X <= 3) and (Y <= 3) then
  begin
    ShowMessage('JVCL 3 Package Installer was written by:'#10#10 +
      'Andreas Hausladen'); // add your name
  end;
end;

procedure TFormMain.ListViewTargetsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  Target: TTargetInfo;
  Dir: string;
begin
  if Change = ctState then
  begin
    if (Item <> nil) and (Item.Data <> nil) and
       (TTargetInfo(Item.Data).CompileFor <> Item.Checked) then
    begin
      Target := Item.Data;
      if Item.Checked then
      begin
        Target.NeedsUpdate := False;
        if not CheckTargetUpdates(Target) then
        begin
          Item.Checked := False;
        end
        else if (not Target.IsJCLInstalled) and (Target.JCLDir = '') then
        begin
          Item.Checked := False;
          if not FInFillingList then // set in FillTargetList()
          begin
           // ask user for JCL root directory
            Dir := Target.RootDir;
            if SelectDirectory('Select JCL root directory', '', Dir) then
            begin
              if DirectoryExists(Dir + '\source\common') then
              begin
                Target.SetJCLDir(Dir);
                Item.Checked := True;
              end
              else
                Dir := '';
            end
            else
              Dir := '';
            if Dir = '' then
              MessageDlg('JCL is not installed for this target and no JCL directory available.'#10 +
                         'Please install the newest JCL or download it to $(JVCL)\..\JCL', mtWarning, [mbOk], 0);
          end;
        end;
      end;
      Target.CompileFor := Item.Checked;
    end;
  end;
end;

procedure TFormMain.ListViewPackagesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
  begin
    if (Item <> nil) and (Item.Data <> nil) then
    begin
      TPackageInfo(Item.Data).Install := Item.Checked;
      UpdatePackageCheckStates;
    end;
  end;
end;

procedure TFormMain.CheckBoxClearJVCLPaletteClick(Sender: TObject);
begin
  TransferTargetOptions(True);
end;

procedure TFormMain.BtnInstallClick(Sender: TObject);
var
  i: Integer;
begin
 // save JVCL.INC if modified
  if FormMainConfig.Config.Modified then
    FormMainConfig.BtnSave.Click;

  TargetList.SaveToFile(ChangeFileExt(ParamStr(0), '.ini'));


  // (ahuser) Should we really ask the user?
//  if MessageDlg('Are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  for i := 0 to TargetList.Count - 1 do
  begin
    if (TargetList[i].CompileFor) and (TargetList[i].IsOldJVCLInstalled > 0) then
      if MessageDlg('An older JVCL version was found:'#10#10 +
                    'JVCL ' + IntToStr(TargetList[i].IsOldJVCLInstalled) + ' in ' +
                    TargetList[i].DisplayName +
                    #10#10'Do you want to uninstall the old JVCL from the IDE?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit
      else
        TargetList[i].UninstallOldJVCL;
  end;
  FormMake.Execute;
end;

procedure TFormMain.ActionInstallUpdate(Sender: TObject);
var
  i: Integer;
  e, AtLeastOneInstalled: Boolean;
begin
  e := False;
  AtLeastOneInstalled := False;
  for i := 0 to TargetList.Count - 1 do
    if TargetList[i].CompileFor then
    begin
      if TargetList[i].IsJVCLInstalled then
        AtLeastOneInstalled := True;
      e := True;
    end;
  ActionInstall.Enabled := e;
  ActionUninstall.Enabled := e and AtLeastOneInstalled;
end;

procedure TFormMain.BtnUninstallClick(Sender: TObject);
var
  i, MakeTargetIndex: Integer;
  RemoveBplDcp: Boolean;
  Make: TMakeFile;
  Target: TTargetInfo;
  RemTargets: string;
begin
  RemTargets := '';
  for i := 0 to TargetList.Count - 1 do
    if (TargetList[i].CompileFor) and (TargetList[i].IsJVCLInstalled) then
      RemTargets := RemTargets + '  ' + TargetList[i].DisplayName + #10;
  Delete(RemTargets, Length(RemTargets), 1);

  if NoYesDlg('Do you really want to uninstall the JVCL 3 packages?'#10#10 +
    'Products:'#10 + RemTargets) = mrYes then
  begin
    RemoveBplDcp := False;
    case MessageDlg('Delete the JVCL 3 .bpl and .dcp files?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) of
      mrCancel: Exit;
      mrYes: RemoveBplDcp := True;
    end;

    for i := 0 to TargetList.Count - 1 do
    begin
      Target := TargetList[i];
      if (Target.CompileFor) and (Target.IsJVCLInstalled) then
      begin
        TargetList[i].RegistryUninstall;
        if RemoveBplDcp then
        begin
         // delete .bpl and .dcp files
          Make := TMakeFile.Create(JVCLPackageDir + '\' + Target.BpgName);
          try
            for MakeTargetIndex := 0 to Make.TargetCount - 1 do
            begin
              DeleteFile(Target.BplDir + '\' + Make.Targets[MakeTargetIndex].Name);
              DeleteFile(Target.DcpDir + '\' + ChangeFileExt(Make.Targets[MakeTargetIndex].Name, '.dcp'));
            end;
          finally
            Make.Free;
          end;
        end;
      end;
    end;
    UpdateTargetList;
    MessageDlg('JVCL 3 packages were uninstalled.'#10#10'Products:'#10 + RemTargets, mtInformation, [mbOk], 0);
  end;
end;

procedure TFormMain.imgProjectJEDIClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.delphi-jedi.org', nil, nil, SW_SHOW);
end;

procedure TFormMain.MenuInvertSelectionClick(Sender: TObject);
var i: Integer;
begin
  Inc(FLockPackageCheckStates);
  try
    for i := 0 to ListViewPackages.Items.Count - 1 do
    begin
      if Sender = MenuSelectAll then
        ListViewPackages.Items[i].Checked := True
      else
      if Sender = MenuSelectNone then
        ListViewPackages.Items[i].Checked := False
      else
      if Sender = MenuInvertSelection then
        ListViewPackages.Items[i].Checked := not ListViewPackages.Items[i].Checked;
    end;
  finally
    Dec(FLockPackageCheckStates);
  end;
  UpdatePackageCheckStates;
end;

procedure TFormMain.LblVersionsClick(Sender: TObject);
begin
  ListViewTargets.SetFocus;
end;

procedure TFormMain.LblPackagesClick(Sender: TObject);
begin
  ListViewPackages.SetFocus;
end;

procedure TFormMain.CheckBoxShowRuntimePackagesClick(Sender: TObject);
begin
  UpdatePackageList;
end;

procedure TFormMain.BtnHppFilesBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := SelTarget.HppFilesDir;
  if BrowseDirectory(Dir, 'Select the directory where the .hpp files should go.', 0) then
  begin
    SelTarget.HppFilesDir := Dir;
    EditHppFilesDir.Text := SelTarget.InsertDirMacros(SelTarget.HppFilesDir);
  end;
end;

function FileNameToURL(const S: string): string;
var i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    case Result[i] of
      '\': Result[i] := '/';
      ':': Result[i] := '|';
    end;
  Result := 'file:///' + Result;
end;

procedure TFormMain.LblBCBInstallationClick(Sender: TObject);
begin
  if not OpenAtAnchor(JVCLDir + '\install.htm', 'AddJVCLPathToBCB') then
    MessageDlg('Cannot open install.htm', mtError, [mbOk], 0);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TargetList.SaveToFile(ChangeFileExt(ParamStr(0), '.ini'));
end;

initialization
  Screen.IconFont.Name := 'Tahoma';

end.
