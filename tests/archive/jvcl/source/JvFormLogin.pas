{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormLogin.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormLogin;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls,
  JvEdit;

type
  TFormLogi = class(TForm)
    Label1: TLabel;
    EdValue1: TJvEdit;
    Label2: TLabel;
    EdValue2: TJvEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    class function Execute(var Edit1Text, Edit2Text: string;
      const FormCaption, Label1Caption, Label2Caption: string;
      PasswordChar: Char): Boolean;
  end;

implementation

{$R *.DFM}

class function TFormLogi.Execute(var Edit1Text, Edit2Text: string;
  const FormCaption, Label1Caption, Label2Caption: string;
  PasswordChar: Char): Boolean;
begin
  with Self.Create(Application) do
  try
    Caption := FormCaption;
    Label1.Caption := Label1Caption;
    Label2.Caption := Label2Caption;
    EdValue1.Text := Edit1Text;
    EdValue2.PasswordChar := PasswordChar;
    EdValue2.Text := Edit2Text;
    Result := ShowModal = mrOK;
    if Result then
    begin
      Edit1Text := EdValue1.Text;
      Edit2Text := EdValue2.Text;
    end;
  finally
    Free;
  end;

end;

end.
