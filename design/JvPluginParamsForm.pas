{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SxPluginParams.PAS, released on 2001-11-11.

The Initial Developer of the Original Code is Ralf Steinhaeusser [ralfiii@gmx.net]
Portions created by Ralf Steinhaeusser are Copyright (C) 2001 Ralf Steinhaeusser.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-09-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPluginParams;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TfrmPluginParams = class(TForm)
    butOK: TButton;
    butCancel: TButton;
    gbPluginSettings: TGroupBox;
    edName: TEdit;
    rbPackage: TRadioButton;
    rbDLL: TRadioButton;
    lblCreateInfo: TLabel;
    procedure SettingsChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  frmPluginParams: TfrmPluginParams;

implementation

uses
  JvTypes;

const
  SInfoText = 'The settings above will create the following project:' +
    CrLf + CrLf +
    '* A project called Plg%0:s.%1:s' + CrLf +
    '* A unit called Plugin%0:s, containing the data module T%0:s.';

{$R *.DFM}

procedure TfrmPluginParams.SettingsChanged(Sender: TObject);

  function RbToPrjExt: string;
  begin
    Result := 'dpk';
    if rbDLL.Checked then
      Result := 'dpr';
  end;

begin
  lblCreateInfo.Caption := Format(SInfoText, [edName.Text, RbToPrjExt]);
  butOK.Enabled := Trim(edName.Text) <> '';
end;

procedure TfrmPluginParams.FormShow(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

end.
