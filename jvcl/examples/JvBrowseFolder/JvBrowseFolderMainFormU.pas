{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvBrowseFolderMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvBaseDlg, JvBrowseFolder, JvComponent;

type
  TJvBrowseFolderMainForm = class(TForm)
    JvBrowseFolder1: TJvBrowseForFolderDialog;
    Button1: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Edit4: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Label5: TLabel;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure JvBrowseFolder1Change(Sender: TObject; Directory: string);
  end;

var
  JvBrowseFolderMainForm: TJvBrowseFolderMainForm;

implementation

{$R *.dfm}

procedure TJvBrowseFolderMainForm.Button1Click(Sender: TObject);
begin
  //Set options
  JvBrowseFolder1.Title := Edit4.Text;
  JvBrowseFolder1.Options := [];
  if CheckBox1.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odNewDialogStyle];
  if CheckBox2.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odBrowseForComputer];
  if CheckBox3.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odOnlyDirectory];
  if CheckBox4.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odOnlyPrinters];
  if CheckBox5.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odIncludeFiles];
  if CheckBox6.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odIncludeUrls];
  if CheckBox7.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odEditBox];
  if CheckBox8.Checked then
    JvBrowseFolder1.Options := JvBrowseFolder1.Options + [odShareable];
  case ComboBox1.ItemIndex of
    1:  JvBrowseFolder1.Rootdirectory := fdControlPanel;
    2:  JvBrowseFolder1.Rootdirectory := fdRecycleBin;
    3:  JvBrowseFolder1.Rootdirectory := fdDesktop;
    4:  JvBrowseFolder1.Rootdirectory := fdDesktopDirectory;
    5:  JvBrowseFolder1.Rootdirectory := fdMyComputer;
    6:  JvBrowseFolder1.Rootdirectory := fdFonts;
    7:  JvBrowseFolder1.Rootdirectory := fdNetHood;
    8:  JvBrowseFolder1.Rootdirectory := fdNetwork;
    9:  JvBrowseFolder1.Rootdirectory := fdPersonal;
    10: JvBrowseFolder1.Rootdirectory := fdPrinters;
    11: JvBrowseFolder1.Rootdirectory := fdPrograms;
    12: JvBrowseFolder1.Rootdirectory := fdRecent;
    13: JvBrowseFolder1.Rootdirectory := fdSendTo;
    14: JvBrowseFolder1.Rootdirectory := fdStartMenu;
    15: JvBrowseFolder1.Rootdirectory := fdStartup;
    16: JvBrowseFolder1.Rootdirectory := fdTemplates;
  else
    JvBrowseFolder1.Rootdirectory := fdRootFolder;
  end;

  //Execute and get back the directory
  if JvBrowseFolder1.Execute then
  begin
    Edit1.Text := JvBrowseFolder1.Directory;
    Edit2.Text := JvBrowseFolder1.DisplayName;

    //If it's a printer or anything else other than a directory, you have to use
    //JvBrowseFolder1.LastPidl
  end;
end;

procedure TJvBrowseFolderMainForm.JvBrowseFolder1Change(Sender: TObject; Directory: string);
begin
  Edit3.Text := Directory;
end;

end.

