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

unit frmDelphiStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvDockControlForm, Menus, ExtCtrls, ComCtrls,
  JvDockDelphiStyle, JvDockSupportClass;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    lbDockClient1: TJvDockClient;
    procedure lbDockClient1FormShow(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Main;


{$R *.DFM}

procedure TForm1.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm1.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
