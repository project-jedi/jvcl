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
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  Forms,
  JvTypes, JvComponent;

type
  TzWizardForm = class(TJvForm)
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


resourcestring
  sBandNameHasToBeAValidIdentifier = 'Band name has to be a valid identifier!';
  sPleaseEnterBandDescription = 'Please enter band description!';
  sHelp = 'Help';

implementation

{$R *.DFM}

resourcestring
  cHelpText = sLineBreak +
    'Band Name' + sLineBreak +
    'Enter a band name, e.g. MyBand.' + sLineBreak +
    'This will be the class name of the band object.' + sLineBreak +
    'No need to prefix it with ''T'' as it will be generated.' + sLineBreak + sLineBreak +
    'Description' + sLineBreak +
    'Enter a menuitem text, e.g. &My Band' + sLineBreak +
    'This text will appear in the Explorer Bar or Toolbars menu.' + sLineBreak + sLineBreak +
    'Band Type' + sLineBreak +
    'Select the type of band object to create.';


procedure TzWizardForm.Button1Click(Sender: TObject);
begin
  with EditBandName do
  begin
    Text := Trim(Text);
    if not IsValidIdent(Text) then
    begin
      SetFocus;
      raise Exception.Create(sBandNameHasToBeAValidIdentifier);
    end;
  end;
  with EditBandDesc do
  begin
    Text := Trim(Text);
    if Text = '' then
    begin
      SetFocus;
      raise Exception.Create(sPleaseEnterBandDescription);
    end;
  end;
  ModalResult := mrOK;
end;

procedure TzWizardForm.Button3Click(Sender: TObject);
var
  HelpForm: TForm;
  HelpText: TMemo;
begin
  HelpForm := TForm.Create(Application);
  try
    with HelpForm do
    begin
      Caption := Self.Caption + ' ' + sHelp;
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

