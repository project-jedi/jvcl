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

unit testunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvWinDialogs, JvComponent, JvBaseDlg, JvBrowseFolder;

type
  TForm1 = class(TForm)
    OrganizeFavoritesDialog: TjvOrganizeFavoritesDialog;
    BrowseFolderDialog: TJvBrowseForFolderDialog;
    AppletDialog: TjvAppletDialog;
    ChangeIconDialog: TjvChangeIconDialog;
    ShellAboutDialog: TjvShellAboutDialog;
    OutOfMemoryDialog: TjvOutOfMemoryDialog;
    RunDialog: TjvRunDialog;
    ComputerNameDialog: TjvComputerNameDialog;
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
    ObjectPropertiesDialog: TjvObjectPropertiesDialog;
    Button11: TButton;
    Button13: TButton;
    psvNewLinkDialog: TjvNewLinkDialog;
    AddHardwareDialog: TjvAddHardwareDialog;
    Button14: TButton;
    JvFormatDriveDialog1: TJvFormatDriveDialog;
    JvAppletDialog1: TJvAppletDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  OrganizeFavoritesDialog.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BrowseFolderDialog.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  AppletDialog.Execute;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ChangeIconDialog.Execute;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ShellAboutDialog.Execute;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  OutOfMemoryDialog.Execute;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  RunDialog.Execute;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  ComputerNameDialog.Execute;
end;

procedure TForm1.Button11Click(Sender: TObject);
var St : string;
begin
  St := GetSpecialFolderpath('My Computer',false);
  ObjectPropertiesDialog.ObjectName := St;
  ObjectPropertiesDialog.Execute;
end;


procedure TForm1.Button13Click(Sender: TObject);
begin
   psvNewLinkDialog.Execute;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
   AddHardwareDialog.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  JvAppletDialog1.Execute;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  JvFormatDriveDialog1.Execute;
end;

end.
