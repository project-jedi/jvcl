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

unit jvWinDialogsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvAddPrinter, JvWinDialogs, JvConnectNetwork, JvBaseDlg,
  JvSelectDirectory, JvComponent, JvBrowseFolder;

type
  TJvWinDialogs = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    JvBrowseFolder1: TJvBrowseForFolderDialog;
    JvSelectDirectory1: TJvSelectDirectory;
    JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog;
    JvComputerNameDialog1: TJvComputerNameDialog;
    JvAppletDialog1: TJvAppletDialog;
    JvChangeIconDialog1: TJvChangeIconDialog;
    JvShellAboutDialog1: TJvShellAboutDialog;
    JvRunDialog1: TJvRunDialog;
    JvObjectPropertiesDialog1: TJvObjectPropertiesDialog;
    JvNewLinkDialog1: TJvNewLinkDialog;
    JvAddHardwareDialog1: TJvAddHardwareDialog;
    JvOpenWithDialog1: TJvOpenWithDialog;
    JvDiskFullDialog1: TJvDiskFullDialog;
    JvExitWindowsDialog1: TJvExitWindowsDialog;
    JvOutOfMemoryDialog1: TJvOutOfMemoryDialog;
    JvConnectNetwork1: TJvConnectNetwork;
    JvDisconnectNetwork1: TJvDisconnectNetwork;
    JvAddPrinterDialog1: TJvAddPrinterDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TJvWinDialogs.Button1Click(Sender: TObject);
begin
  JvBrowseFolder1.Execute;
end;

procedure TJvWinDialogs.Button2Click(Sender: TObject);
begin
  JvSelectDirectory1.execute;
end;

procedure TJvWinDialogs.Button3Click(Sender: TObject);
begin
  JvOrganizeFavoritesDialog1.execute;
end;

procedure TJvWinDialogs.Button4Click(Sender: TObject);
begin
  JvComputerNameDialog1.execute;
end;

procedure TJvWinDialogs.Button7Click(Sender: TObject);
begin
  JvChangeIconDialog1.execute;
end;

procedure TJvWinDialogs.Button15Click(Sender: TObject);
begin
  JvShellAboutDialog1.execute;
end;

procedure TJvWinDialogs.Button21Click(Sender: TObject);
begin
  JvRunDialog1.execute;
end;

procedure TJvWinDialogs.Button20Click(Sender: TObject);
begin
  JvObjectPropertiesDialog1.execute;
end;

procedure TJvWinDialogs.Button19Click(Sender: TObject);
begin
  JvNewLinkDialog1.execute;
//  ShowMessage('Makes an Error');
end;

procedure TJvWinDialogs.Button8Click(Sender: TObject);
begin
  JvAddHardwareDialog1.execute;
end;

procedure TJvWinDialogs.Button14Click(Sender: TObject);
begin
  JvOpenWithDialog1.execute;
end;

procedure TJvWinDialogs.Button13Click(Sender: TObject);
begin
  JvDiskFullDialog1.execute;
end;

procedure TJvWinDialogs.Button12Click(Sender: TObject);
begin
  JvExitWindowsDialog1.execute;
end;

procedure TJvWinDialogs.Button11Click(Sender: TObject);
begin
  JvOutOfMemoryDialog1.execute;
end;

procedure TJvWinDialogs.Button10Click(Sender: TObject);
begin
  JvConnectNetwork1.Execute;
end;

procedure TJvWinDialogs.Button9Click(Sender: TObject);
begin
  JvDisconnectNetwork1.execute;
end;

procedure TJvWinDialogs.Button16Click(Sender: TObject);
begin
  JvAddPrinterDialog1.execute;
end;

end.

