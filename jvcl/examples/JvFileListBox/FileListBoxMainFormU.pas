{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit FileListBoxMainFormU;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, JvDriveCtrls, StdCtrls, JvCombobox, JvListBox, JvLabel,
  JvComponent, ExtCtrls, JvCaptionPanel, JvExStdCtrls, JvExExtCtrls,
  JvExControls;

type
  TFileListBoxMainForm = class(TForm)
    JvLabel6: TJvLabel;
    JvLabel8: TJvLabel;
    JvDriveList1: TJvDriveList;
    Label1: TLabel;
    Label2: TLabel;
    JvCaptionPanel1: TJvCaptionPanel;
    JvFileListBox1: TJvFileListBox;
    JvDriveCombo1: TJvDriveCombo;
    JvDirectoryListBox1: TJvDirectoryListBox;
    Edit1: TEdit;
    Label3: TLabel;
    procedure JvDirectoryListBox1DriveChangeError(Sender: TObject;
      var NewDrive: Char);
    procedure JvCaptionPanel1ButtonClick(Sender: TObject;
      Button: TJvCapBtnStyle);
  end;

var
  FileListBoxMainForm : TFileListBoxMainForm;

implementation

{$R *.dfm}

procedure TFileListBoxMainForm.JvDirectoryListBox1DriveChangeError(
  Sender: TObject; var NewDrive: Char);
begin
  ShowMessageFmt('Could not change to the selected drive (%s:). Please make sure it is available and try again',[NewDrive]);
  JvDriveCombo1.Drive := JvDirectoryListBox1.Drive;
end;

procedure TFileListBoxMainForm.JvCaptionPanel1ButtonClick(Sender: TObject;
  Button: TJvCapBtnStyle);
begin
  if Button = capClose then
    ShowMessage('Sorry, you can''t close this window!');
end;

end.
