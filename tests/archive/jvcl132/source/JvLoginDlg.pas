{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLoginDlg.PAS, released on 2001-02-28.

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

unit JvLoginDlg;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormLogin, JvBaseDlg, JvTypes;

type
  TJvLoginDlg = class(TJvCommonDialog)
  private
    FCaption: string;
    FCaption2: string;
    FTitle: string;
    FUsername: string;
    FPassword: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FirstLabel: string read FCaption write FCaption;
    property SecondLabel: string read FCaption2 write FCaption2;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Title: string read FTitle write FTitle;
    function Execute: boolean; override;
  end;

implementation

resourcestring
  RC_LabelPassCaption = '&Username:';
  RC_Label2Caption = '&Password:';
  RC_TitleLogin = 'Login';

  {**************************************************}

constructor TJvLoginDlg.Create(AOwner: TComponent);
begin
  FCaption := RC_LabelPassCaption;
  FCaption2 := RC_Label2Caption;
  FTitle := RC_TitleLogin;
  FUsername := '';
  FPassword := '';
  inherited;
end;
{**************************************************}

function TJvLoginDlg.Execute: boolean;
begin
  with TFormLogi.Create(Application) do
  try
    Caption := FTitle;
    Label1.Caption := FCaption;
    Label2.Caption := FCaption2;
    edUserName.Text := FUsername;
    edPassword.Text := FPassword;
    Result := Showmodal = mrOK;
    if Result then
    begin
      FUsername := edUserName.Text;
      FPassword := edPassword.Text;
    end;
  finally
    Free;
  end;
end;
{**************************************************}
end.

