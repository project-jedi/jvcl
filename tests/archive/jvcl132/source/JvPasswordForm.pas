{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPasswordForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvPasswordForm;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvFormPass, JvBaseDlg, JvTypes;

type
  TJvPasswordForm = class(TJvCommonDialogP)
  private
    FPass: TPassForm;
    FTitle: string;
    FOkCaption: string;
    FCancelCaption: string;
    FLabelCaption: string;
    FTextValue: string;
    FOnCancel: TNotifyEvent;
    FOnOk: TOnOk;
    FPassChar: Char;
    procedure OnOkClick(Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    procedure Execute; override;
    property Title: string read FTitle write FTitle;
    property OkCaption: string read FOkCaption write FOkCaption;
    property CancelCaption: string read FCancelCaption write FCancelCaption;
    property LabelCaption: string read FLabelCaption write FLabelCaption;
    property TextValue: string read FTextValue write FTextValue;
    property OnCancel: TNotifyevent read FOnCancel write FOnCancel;
    property OnOk: TOnOk read FOnOk write FOnOk;
    property PasswordChar: Char read FPassChar write FPassChar;
  end;

implementation

resourcestring
  RC_PassEnter = 'Enter password';
  RC_OkCaption = '&OK';
  RC_CancelCaption = '&Cancel';
  RC_PassChar = '*';
  RC_LabelPassCaption = '&Password:';

  {**************************************************}

procedure TJvPasswordForm.OnOkClick(Sender: TObject);
var
  Can: Boolean;
begin
  try
    Can := False;
    if Assigned(FOnOk) then
      FOnOk(Self, FPass.edPassword.Text, Can);
    if Can then
    begin
      FPass.btnCancel.OnClick := nil;
      FPass.Close;
    end;
  except
  end;
end;

{**************************************************}

procedure TJvPasswordForm.OnCancelClick(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

{**************************************************}

constructor TJvPasswordForm.Create(AOwner: TComponent);
begin
  inherited;
  FTitle := RC_PassEnter;
  FOkCaption := RC_OkCaption;
  FCancelCaption := RC_CancelCaption;
  FLabelCaption := RC_LabelPassCaption;
  FTextValue := '';
  FPassChar := RC_PassChar[1];
end;

{**************************************************}

procedure TJvPasswordForm.Execute;
begin
  try
    if FPass = nil then
      FPass := TPassForm.Create(Application);
    FPass.Caption := FTitle;
    FPass.btnOK.Caption := FOkCaption;
    FPass.btnCancel.Caption := FCancelCaption;
    FPass.label1.Caption := FLabelCaption;
    FPass.edPassword.Text := FTextValue;
    FPass.edPassword.PasswordChar := FPassChar;
    FPass.btnOK.onclick := OnOkClick;
    FPass.btnCancel.onclick := OnCancelClick;
    FPass.ShowModal;
  finally
    FPass.Free;
    FPass := nil;
  end;
end;

end.
