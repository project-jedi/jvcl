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

unit jvTrayIconDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, JvComponent, JvTrayIcon;

type
  TfrTrayicon = class(TForm)
    JvTrayIcon1: TJvTrayIcon;
    PopupMenu1: TPopupMenu;
    ShowTray1: TMenuItem;
    ShowForm1: TMenuItem;
    Close1: TMenuItem;
    Label1: TLabel;
    procedure ShowTray1Click(Sender: TObject);
    procedure ShowForm1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfrTrayicon.ShowTray1Click(Sender: TObject);
begin
  JvTrayIcon1.Active := true;
end;

procedure TfrTrayicon.ShowForm1Click(Sender: TObject);
begin
  JvTrayIcon1.active := false;
end;

procedure TfrTrayicon.Close1Click(Sender: TObject);
begin
  close;
end;

end.
