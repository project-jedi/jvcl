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

unit frmVCStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDockControlForm, Menus, StdCtrls, ComCtrls, ExtCtrls, JvDockSupportClass;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Main;

{$R *.DFM}

procedure TForm2.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

procedure TForm2.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

end.
