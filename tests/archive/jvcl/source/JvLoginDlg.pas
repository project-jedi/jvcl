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

{$I JVCL.INC}

unit JvLoginDlg;

interface

uses
  SysUtils, Classes, Dialogs,
  JvFormLogin, JvBaseDlg;

type
  TJvLoginDlg = class(TJvCommonDialog)
  private
    FFirstLabel: string;
    FSecondLabel: string;
    FTitle: string;
    FUsername: string;
    FPassword: string;
    FPasswordChar: Char;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property FirstLabel: string read FFirstLabel write FFirstLabel;
    property SecondLabel: string read FSecondLabel write FSecondLabel;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Title: string read FTitle write FTitle;
    // (rom) added PasswordChar
    property PasswordChar: Char read FPasswordChar write FPasswordChar default '*';
  end;

implementation

resourcestring
  RC_FirstLabelCaption = '&Username:';
  RC_SecondLabelCaption = '&Password:';
  RC_TitleLogin = 'Login';

constructor TJvLoginDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFirstLabel := RC_FirstLabelCaption;
  FSecondLabel := RC_SecondLabelCaption;
  FTitle := RC_TitleLogin;
  FUsername := '';
  FPassword := '';
  FPasswordChar := '*';
end;

function TJvLoginDlg.Execute: Boolean;
begin
  Result := TFormLogi.Execute(FUsername, FPassword,
    Title, FirstLabel, SecondLabel, PasswordChar);
end;

end.

