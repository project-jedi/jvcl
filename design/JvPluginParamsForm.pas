{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SxPluginParams.PAS, released on 2001-11-11.

The Initial Developer of the Original Code is Ralf Steinhaeusser [ralfiii att gmx dott net]
Portions created by Ralf Steinhaeusser are Copyright (C) 2001 Ralf Steinhaeusser.
All Rights Reserved.

Contributor(s):
Stefaan Lesage - converted to use new OTA

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPluginParamsForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TfrmPluginParams = class(TForm)
    butOK: TButton;
    butCancel: TButton;
    gbPluginSettings: TGroupBox;
    edtPluginName: TEdit;
    rbPackage: TRadioButton;
    rbDLL: TRadioButton;
    lblCreateInfo: TLabel;
    lblPluginName: TLabel;
    lblLabel1: TLabel;
    edtPluginAuthor: TEdit;
    lblDescription: TLabel;
    mmoDescripton: TMemo;
    edtPluginCopyright: TEdit;
    lblLabel2: TLabel;
    edtPluginUID: TEdit;
    lblLabel3: TLabel;
    lblLabel4: TLabel;
    edtPluginHostProject: TEdit;
    procedure SettingsChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

implementation

uses
  JvTypes, JvDsgnConsts;

{$R *.dfm}

procedure TfrmPluginParams.SettingsChanged(Sender: TObject);

  function RbToPrjExt: string;
  begin
    Result := 'dpk';
    if rbDLL.Checked then
      Result := 'dpr';
  end;

begin
  lblCreateInfo.Caption := Format(RsPluginParamsFormInfoText, [edtPluginName.Text, RbToPrjExt]);
  butOK.Enabled := Trim(edtPluginName.Text) <> '';
  edtPluginUID.Text := 'JVCL.' + edtPluginHostProject.Text + '.Plg' + edtPluginName.Text;
end;

procedure TfrmPluginParams.FormShow(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

end.
