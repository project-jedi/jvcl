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

unit fDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvCommonExecDlg, JvRenameError, JvDeleteError,
  JvCopyError, JvCommonDialogD, JvDiskPrompt,
  JvConnectNetwork, JvSelectDirectory, JvBaseDlg, JvFindFiles, JvComponent,
  JvWinDialogs, ComCtrls, JvBrowseFolder, JvPageSetupTitled, JvPageSetup,
  JvAddPrinter, JvCalc, JvDialogs;

type
  TJvDialogsDemoFrm = class(TForm)
    JvFormatDriveDialog1: TJvFormatDriveDialog;
    JvFindFiles1: TJvFindFilesDialog;
    JvBrowseFolder1: TJvBrowseForFolderDialog;   //TJvShellAbout;
    JvSelectDirectory1: TJvSelectDirectory;
    JvConnectNetwork1: TJvConnectNetwork;
    JvDisconnectNetwork1: TJvDisconnectNetwork;
    JvCalculator1: TJvCalculator;
    JvDiskPrompt1: TJvDiskPrompt;
    JvCopyError1: TJvCopyError;
    JvDeleteError1: TJvDeleteError;
    JvRenameError1: TJvRenameError;
    JvShutdownDlg1: TJvExitWindowsDialog;
    JvShellAboutDialog1: TJvShellAboutDialog;
    JvAddHardwareDialog1: TJvAddHardwareDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button32: TButton;
    Button33: TButton;
    Button25: TButton;
    Button24: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button18: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    JvChooseIconDlg1: TJvChangeIconDialog;
    JvRunDlg1: TJvRunDialog;
    JvFindComputerDlg1: TJvComputerNameDialog;
    JvObjectPropertiesDlg1: TJvObjectPropertiesDialog;
    JvOutOfMemoryDlg1: TJvOutOfMemoryDialog;
    JvOutOfSpaceDlg1: TJvDiskFullDialog;
    JvPageSetupDialog1: TJvPageSetupDialog;
    JvPageSetupTitledDialog1: TJvPageSetupTitledDialog;
    JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog;
    JvAppletDialog1: TJvAppletDialog;
    JvNewLinkDialog1: TJvNewLinkDialog;
    JvOpenWithDialog1: TJvOpenWithDialog;
    JvAddPrinterDialog1: TJvAddPrinterDialog;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    Button40: TButton;
    Button3: TButton;
    Button7: TButton;
    Button41: TButton;
    JvOpenDialog1: TJvOpenDialog;
    JvSaveDialog1: TJvSaveDialog;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button39Click(Sender: TObject);
    procedure Button38Click(Sender: TObject);
    procedure Button36Click(Sender: TObject);
    procedure Button37Click(Sender: TObject);
    procedure Button40Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button41Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  end;
var
  JvDialogsDemoFrm: TJvDialogsDemoFrm;

implementation

{$R *.dfm}

procedure TJvDialogsDemoFrm.Button1Click(Sender: TObject);
begin
  JvFormatDriveDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button2Click(Sender: TObject);
begin
  JvFindFiles1.Execute;
end;

procedure TJvDialogsDemoFrm.Button3Click(Sender: TObject);
begin
  JvBrowseFolder1.Execute;
end;

procedure TJvDialogsDemoFrm.Button4Click(Sender: TObject);
begin
  JvShellAboutDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button5Click(Sender: TObject);
begin
  JvSelectDirectory1.Execute;
end;

procedure TJvDialogsDemoFrm.Button8Click(Sender: TObject);
begin
  { TODO : BUG - This routine doesn't work }
  { DONE -opeter3 : Works on Win2k... }
  JvAddPrinterDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button9Click(Sender: TObject);
begin
  JvConnectNetwork1.Execute;
end;

procedure TJvDialogsDemoFrm.Button10Click(Sender: TObject);
begin
  JvDisconnectNetwork1.Execute;
end;

procedure TJvDialogsDemoFrm.Button18Click(Sender: TObject);
begin
  JvCalculator1.Execute;
end;

procedure TJvDialogsDemoFrm.Button20Click(Sender: TObject);
begin
  JvDiskPrompt1.Execute;
end;

procedure TJvDialogsDemoFrm.Button21Click(Sender: TObject);
begin
  JvCopyError1.Execute;
end;

procedure TJvDialogsDemoFrm.Button22Click(Sender: TObject);
begin
  JvDeleteError1.Execute;
end;

procedure TJvDialogsDemoFrm.Button23Click(Sender: TObject);
begin
  JvRenameError1.Execute;
end;

procedure TJvDialogsDemoFrm.Button24Click(Sender: TObject);
begin
  JvShutdownDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button25Click(Sender: TObject);
begin
  JvAddHardwareDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button26Click(Sender: TObject);
begin
  JvChooseIconDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button27Click(Sender: TObject);
begin
  JvRunDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button28Click(Sender: TObject);
begin
  JvFindComputerDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button29Click(Sender: TObject);
var
  St: string;
begin
  St := GetSpecialFolderPath('My Computer', False);
  JvObjectPropertiesDlg1.ObjectName := St;
  JvObjectPropertiesDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button30Click(Sender: TObject);
begin
  JvOutOfMemoryDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button31Click(Sender: TObject);
begin
  JvOutOfSpaceDlg1.Execute;
end;

procedure TJvDialogsDemoFrm.Button39Click(Sender: TObject);
begin
  JvOrganizeFavoritesDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button38Click(Sender: TObject);
var
  WinDir: array [0..255] of Char;
begin
  GetWindowsDirectory(WinDir, SizeOf(WinDir));

  JvOpenDialog1.Filter := 'Applet files (*.cpl)|*.cpl';
  JvOpenDialog1.InitialDir := ExtractFileDrive(StrPas(WinDir));
  if JvOpenDialog1.Execute then
  begin
    JvAppletDialog1.AppletName := JvOpenDialog1.FileName;
    JvAppletDialog1.Execute;
  end;
end;

procedure TJvDialogsDemoFrm.Button36Click(Sender: TObject);
begin
  JvAppletDialog1.AppletName := '';
  JvAppletDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button37Click(Sender: TObject);
begin
  JvNewLinkDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button40Click(Sender: TObject);
begin
  JvOpenWithDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button7Click(Sender: TObject);
begin
  JvPageSetupDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button41Click(Sender: TObject);
begin
  JvPageSetupTitledDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button32Click(Sender: TObject);
begin
  JvOpenDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.Button33Click(Sender: TObject);
begin
  JvSaveDialog1.Execute;
end;

procedure TJvDialogsDemoFrm.FormActivate(Sender: TObject);
begin
  JvDialogsDemoFrm.ClientHeight := PageControl1.Height;
  JvDialogsDemoFrm.ClientWidth := PageControl1.Width;
end;

end.



