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

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBandObjectDLLWizardForm;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  Forms,
  JvTypes, JvComponent;

type
  TzWizardForm = class(TJvForm)
    OK: TButton;
    Cancel: TButton;
    Label1: TLabel;
    EditBandName: TEdit;
    RgBandType: TRadioGroup;
    Bevel1: TBevel;
    Label2: TLabel;
    EditBandDesc: TEdit;
    Help: TButton;
    procedure OKClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  end;

implementation

uses
  JvDsgnConsts;

{$R *.dfm}

procedure TzWizardForm.OKClick(Sender: TObject);
begin
  with EditBandName do
  begin
    Text := Trim(Text);
    if not IsValidIdent(Text) then
    begin
      SetFocus;
      raise EJVCLException.CreateRes(@RsEBandNameHasToBeAValidIdentifier);
    end;
  end;
  with EditBandDesc do
  begin
    Text := Trim(Text);
    if Text = '' then
    begin
      SetFocus;
      raise EJVCLException.CreateRes(@RsEPleaseEnterBandDescription);
    end;
  end;
  ModalResult := mrOk;
end;

procedure TzWizardForm.HelpClick(Sender: TObject);
var
  HelpForm: TForm;
  HelpText: TMemo;
begin
  HelpForm := TForm.Create(Application);
  try
    with HelpForm do
    begin
      Caption := Format(RsBandHelpCaptionFmt, [Self.Caption, RsHelp]);
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
      Text := RsHelpText;
    end;
    HelpForm.ShowModal;
  finally
    HelpForm.Free;
  end;
end;

end.

