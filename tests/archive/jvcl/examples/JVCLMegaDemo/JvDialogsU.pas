{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au

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

unit JvDialogsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvBaseDlg, StdCtrls, JvNagScreen, JvImageDlg, JvWinDialogs,
  JvCommonDialogD, JvDiskPrompt, JvCopyError,
  JvDeleteError, JvPageSetup, JvPageSetupTitled, JvObjPickerComp,
  JvCalc, JvTipOfDay, ExtCtrls, JvCaptionPanel, JvFindFiles,
  JvGroupBox, Buttons, JvBitBtn, JvDlg, JvBrowseFolder, JvSelectDirectory,
  JvDialogs, JvConnectNetwork, JvAddPrinter, JvRenameError, JvFindReplace;

type
  TJvDialogs = class(TForm)
    JvImageDlg1: TJvImageDlg;
    JvCalculator1: TJvCalculator;
    JvDiskPrompt1: TJvDiskPrompt;
    JvCopyError1: TJvCopyError;
    JvDeleteError1: TJvDeleteError;
    JvPageSetupDialog1: TJvPageSetupDialog;
    JvPageSetupTitledDialog1: TJvPageSetupTitledDialog;
    JvObjectPickerDialog1: TJvObjectPickerDialog;
    JvCaptionPanel1: TJvCaptionPanel;
    Button17: TButton;
    JvFindFilesDialog1: TJvFindFilesDialog;
    JvProgressForm1: TJvProgressForm;
    JvBitBtn2: TJvBitBtn;
    JvBitBtn3: TJvBitBtn;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    JvSelectDirectory1: TJvSelectDirectory;
    JvBitBtn4: TJvBitBtn;
    JvOpenDialog1: TJvOpenDialog;
    JvSaveDialog1: TJvSaveDialog;
    JvBitBtn5: TJvBitBtn;
    JvBitBtn6: TJvBitBtn;
    JvBitBtn7: TJvBitBtn;
    JvBitBtn8: TJvBitBtn;
    JvConnectNetwork1: TJvConnectNetwork;
    JvDisconnectNetwork1: TJvDisconnectNetwork;
    JvBitBtn9: TJvBitBtn;
    JvBitBtn10: TJvBitBtn;
    JvBitBtn11: TJvBitBtn;
    JvBitBtn12: TJvBitBtn;
    JvBitBtn13: TJvBitBtn;
    JvAddPrinterDialog1: TJvAddPrinterDialog;
    JvBitBtn14: TJvBitBtn;
    JvBitBtn15: TJvBitBtn;
    JvBitBtn16: TJvBitBtn;
    JvBitBtn17: TJvBitBtn;
    JvFormatDriveDialog1: TJvFormatDriveDialog;
    JvColorDialog1: TJvColorDialog;
    JvBitBtn18: TJvBitBtn;
    JvBitBtn19: TJvBitBtn;
    JvBitBtn20: TJvBitBtn;
    JvBitBtn21: TJvBitBtn;
    JvBitBtn22: TJvBitBtn;
    JvBitBtn23: TJvBitBtn;
    JvBitBtn24: TJvBitBtn;
    JvBitBtn25: TJvBitBtn;
    JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog;
    JvComputerNameDialog1: TJvComputerNameDialog;
    JvAppletDialog1: TJvAppletDialog;
    JvChangeIconDialog1: TJvChangeIconDialog;
    JvShellAboutDialog1: TJvShellAboutDialog;
    JvBitBtn26: TJvBitBtn;
    JvBitBtn27: TJvBitBtn;
    JvBitBtn28: TJvBitBtn;
    JvBitBtn29: TJvBitBtn;
    JvRunDialog1: TJvRunDialog;
    JvObjectPropertiesDialog1: TJvObjectPropertiesDialog;
    JvNewLinkDialog1: TJvNewLinkDialog;
    JvBitBtn30: TJvBitBtn;
    JvBitBtn31: TJvBitBtn;
    JvBitBtn32: TJvBitBtn;
    JvBitBtn33: TJvBitBtn;
    JvBitBtn34: TJvBitBtn;
    JvBitBtn35: TJvBitBtn;
    JvBitBtn36: TJvBitBtn;
    JvBitBtn37: TJvBitBtn;
    JvBitBtn38: TJvBitBtn;
    JvBitBtn39: TJvBitBtn;
    JvBitBtn40: TJvBitBtn;
    JvBitBtn41: TJvBitBtn;
    JvBitBtn42: TJvBitBtn;
    JvBitBtn43: TJvBitBtn;
    JvBitBtn44: TJvBitBtn;
    JvBitBtn45: TJvBitBtn;
    JvAddHardwareDialog1: TJvAddHardwareDialog;
    JvOpenWithDialog1: TJvOpenWithDialog;
    JvDiskFullDialog1: TJvDiskFullDialog;
    JvExitWindowsDialog1: TJvExitWindowsDialog;
    JvOutOfMemoryDialog1: TJvOutOfMemoryDialog;
    JvRenameError1: TJvRenameError;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button17Click(Sender: TObject);
    procedure JvPasswordForm1Ok(Sender: TObject; Password: String;
      var Accept: Boolean);
    procedure JvPasswordForm1Cancel(Sender: TObject);
    procedure JvBitBtn2Click(Sender: TObject);
    procedure JvProgressForm1Show(Sender: TObject);
    procedure JvBitBtn3Click(Sender: TObject);
    procedure JvBitBtn4Click(Sender: TObject);
    procedure JvBitBtn5Click(Sender: TObject);
    procedure JvBitBtn7Click(Sender: TObject);
    procedure JvBitBtn9Click(Sender: TObject);
    procedure JvBitBtn10Click(Sender: TObject);
    procedure JvBitBtn11Click(Sender: TObject);
    procedure JvBitBtn12Click(Sender: TObject);
    procedure JvBitBtn13Click(Sender: TObject);
    procedure JvBitBtn15Click(Sender: TObject);
    procedure JvBitBtn14Click(Sender: TObject);
    procedure JvBitBtn16Click(Sender: TObject);
    procedure JvBitBtn17Click(Sender: TObject);
    procedure JvBitBtn18Click(Sender: TObject);
    procedure JvBitBtn20Click(Sender: TObject);
    procedure JvBitBtn21Click(Sender: TObject);
    procedure JvBitBtn22Click(Sender: TObject);
    procedure JvBitBtn23Click(Sender: TObject);
    procedure JvBitBtn25Click(Sender: TObject);
    procedure JvBitBtn24Click(Sender: TObject);
    procedure JvBitBtn28Click(Sender: TObject);
    procedure JvBitBtn27Click(Sender: TObject);
    procedure JvBitBtn26Click(Sender: TObject);
    procedure JvBitBtn29Click(Sender: TObject);
    procedure JvBitBtn30Click(Sender: TObject);
    procedure JvBitBtn31Click(Sender: TObject);
    procedure JvBitBtn36Click(Sender: TObject);
    procedure JvBitBtn39Click(Sender: TObject);
    procedure JvBitBtn40Click(Sender: TObject);
    procedure JvBitBtn42Click(Sender: TObject);
    procedure JvBitBtn45Click(Sender: TObject);
    procedure JvBitBtn38Click(Sender: TObject);
    procedure JvBitBtn41Click(Sender: TObject);
    procedure JvBitBtn43Click(Sender: TObject);
    procedure JvBitBtn44Click(Sender: TObject);
  end;

implementation

uses FindReplaceMainFormU;
{$R *.DFM}

procedure TJvDialogs.Button17Click(Sender: TObject);
begin
  JvObjectPickerDialog1.Execute;
end;

procedure TJvDialogs.JvPasswordForm1Ok(Sender: TObject; Password: String;
  var Accept: Boolean);
begin
  if Application.MessageBox(PChar('You click OK with password <' + Password + '>. ' +
                          'Click OK to accept or Cancel to refuse.'),'OnOK event handle',MB_OKCANCEL) = MB_OK then
      Accept := true;
end;

procedure TJvDialogs.JvPasswordForm1Cancel(Sender: TObject);
begin
  Application.MessageBox(PChar('You pressed Cancel'),'OnCancel event handle',MB_OK);
end;

procedure TJvDialogs.JvBitBtn2Click(Sender: TObject);
var
  I: integer;
begin
  jvProgressForm1.Execute;
end;

procedure TJvDialogs.JvProgressForm1Show(Sender: TObject);
begin
 jvProgressForm1.ProgressPosition := jvProgressForm1.ProgressMin;
  while jvProgressForm1.ProgressPosition < jvProgressForm1.ProgressMax do
  begin
    Sleep(50);
    jvProgressForm1.InfoLabel := 'Long operation in progress: item'+ IntToStr(Random(100));
    jvProgressForm1.ProgressPosition := jvProgressForm1.ProgressPosition + 1;
    Application.ProcessMessages;
    if jvProgressForm1.Cancel then Exit;
  end;    { while }

end;

procedure TJvDialogs.JvBitBtn3Click(Sender: TObject);
begin
  JvBrowseForFolderDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn4Click(Sender: TObject);
begin
  jvSelectDirectory1.Execute;
end;

procedure TJvDialogs.JvBitBtn5Click(Sender: TObject);
begin
  jvOpenDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn7Click(Sender: TObject);
begin
  jvSaveDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn9Click(Sender: TObject);
begin
  jvConnectNetwork1.Execute;
end;

procedure TJvDialogs.JvBitBtn10Click(Sender: TObject);
begin
  jvDisconnectNetwork1.Execute;
end;

procedure TJvDialogs.JvBitBtn11Click(Sender: TObject);
begin
  jvPageSetupDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn12Click(Sender: TObject);
begin
  jvPageSetupTitledDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn13Click(Sender: TObject);
begin
  jvAddPrinterDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn15Click(Sender: TObject);
begin
  jvFindFilesDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn14Click(Sender: TObject);
begin
  jvFormatDriveDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn16Click(Sender: TObject);
begin
  jvColorDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn17Click(Sender: TObject);
begin
  jvOrganizeFavoritesDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn18Click(Sender: TObject);
begin
  JvComputerNameDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn20Click(Sender: TObject);
var
  WinDir: array[0..255] of char;
  FileToFind: string;
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

procedure TJvDialogs.JvBitBtn21Click(Sender: TObject);
begin
  jvChangeIconDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn22Click(Sender: TObject);
begin
  JvShellAboutDialog1.execute;
end;

procedure TJvDialogs.JvBitBtn23Click(Sender: TObject);
begin
  jvRunDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn25Click(Sender: TObject);
begin
  JvObjectPropertiesDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn24Click(Sender: TObject);
begin
  JvNewLinkDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn28Click(Sender: TObject);
begin
  jvAddHardwareDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn27Click(Sender: TObject);
begin
  JvOpenWithDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn26Click(Sender: TObject);
begin
  jvDiskFullDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn29Click(Sender: TObject);
begin
  jvExitWindowsDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn30Click(Sender: TObject);
begin
  JvOutOfMemoryDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn31Click(Sender: TObject);
begin
  jvObjectPickerDialog1.Execute;
end;

procedure TJvDialogs.JvBitBtn36Click(Sender: TObject);
begin
  with TJvNagScreen.Create(Application) do
  try
    Picture.LoadFromResourceName(HInstance, 'NAGPIC');
    Execute;
  finally
    Free;
  end;

end;

procedure TJvDialogs.JvBitBtn39Click(Sender: TObject);
begin
  JvImageDlg1.Execute;
end;

procedure TJvDialogs.JvBitBtn40Click(Sender: TObject);
begin
  JvDiskPrompt1.OwnerWindow := HWND_DESKTOP;
  JvDiskPrompt1.Execute;
end;

procedure TJvDialogs.JvBitBtn42Click(Sender: TObject);
begin
  JvCopyError1.OwnerWindow := HWND_DESKTOP;
  JvCopyError1.Execute;
end;

procedure TJvDialogs.JvBitBtn45Click(Sender: TObject);
begin
  JvDeleteError1.OwnerWindow := HWND_DESKTOP;
  JvDeleteError1.Execute;
end;

procedure TJvDialogs.JvBitBtn38Click(Sender: TObject);
begin
  JvRenameError1.OwnerWindow := HWND_DESKTOP;
  JvRenameError1.Execute;
end;

procedure TJvDialogs.JvBitBtn41Click(Sender: TObject);
begin
  with TJvTipOfDay.Create(Application) do
  try
    Tips.Append('Hintline 1');
    Tips.Append('Hintline 2');
    Tips.Append('Hintline 3');
    Tips.Append('Hintline 4');
    Execute;
  finally
    Free;
  end;
end;

procedure TJvDialogs.JvBitBtn43Click(Sender: TObject);
begin
  JvCalculator1.Execute;
end;

procedure TJvDialogs.JvBitBtn44Click(Sender: TObject);
var
  Form1: TForm;
begin
  Form1 := TFindReplaceMainForm.Create(nil);
  Form1.Position := poScreenCenter;
  Form1.ShowModal;
  Form1.Free;
end;

end.

