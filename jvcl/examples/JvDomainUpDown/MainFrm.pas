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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPanel, ExtCtrls, ComCtrls, StdCtrls, Mask,
  JvUpDown, JvJCLUtils;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    JvDomainUpDown1: TJvDomainUpDown;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Label1.Caption := Format('%d',[JvDomainUpDown1.Position]);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenObject(Edit1.Text);
end;

end.
