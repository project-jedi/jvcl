{:JvBandObject wizard form.       }
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandObjectDLLWizardForm.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs@ctzen.com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2001-mm-dd

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBandObjectDLLWizardForm;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms;

type
  TzWizardForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    EditBandName: TEdit;
    RgBandType: TRadioGroup;
    Bevel1: TBevel;
    Label2: TLabel;
    EditBandDesc: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  JvTypes;

procedure TzWizardForm.Button1Click(Sender: TObject);
begin
  with EditBandName do
  begin
    Text := Trim(Text);
    if not IsValidIdent(Text) then
    begin
      SetFocus;
      raise Exception.Create('Band name has to be a valid identifier!');
    end;
  end;
  with EditBandDesc do
  begin
    Text := Trim(Text);
    if Text = '' then
    begin
      SetFocus;
      raise Exception.Create('Please enter band description!');
    end;
  end;
  ModalResult := mrOK;
end;

procedure TzWizardForm.Button3Click(Sender: TObject);
const
  CrLf2 = #13#10#13#10;
  cHelpText = CrLf +
    'Band Name' + CrLf +
    'Enter a band name, e.g. MyBand.' + CrLf +
    'This will be the class name of the band object.' + CrLf +
    'No need to prefix it with ''T'' as it will be generated.' + CrLf2 +
    'Description' + CrLf +
    'Enter a menuitem text, e.g. &My Band' + CrLf +
    'This text will appear in the Explorer Bar or Toolbars menu.' + CrLf2 +
    'Band Type' + CrLf +
    'Select the type of band object to create.';
var
  HelpForm: TForm;
  HelpText: TMemo;
begin
  HelpForm := TForm.Create(Application);
  try
    with HelpForm do
    begin
      Caption := Self.Caption + ' Help';
      BorderStyle := bsDialog;
      Top := Self.Top + Self.Height div 2;
      Left := Self.Left + Self.Width div 2;
      Height := 260;
      Width := 350;
    end;
    HelpText := TMemo.Create(HelpForm);
    with HelpText do
    begin
      Parent := HelpForm;
      Align := alClient;
      ReadOnly := True;
      Text := cHelpText;
    end;
    HelpForm.ShowModal;
  finally
    HelpForm.Free;
  end;
end;

end.

