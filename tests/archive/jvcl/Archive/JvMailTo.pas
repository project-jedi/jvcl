{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMailTo.PAS, released on 2001-02-28.

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

unit JvMailTo;

{$OBJEXPORTALL On}

interface

// (rom) should be JclMapi based

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvLabel, JvFunctions;

type
  TJvMailTo = class(TJvLabel)
  private
    FSubject: string;
    FTo: string;
    FBCC: string;
    FCC: string;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property MailSubject: string read FSubject write FSubject;
    property MailDestinator: string read FTo write FTo;
    property MailBcc: string read FBcc write FBCC;
    property MailCC: string read FCC write FCC;
  end;

implementation

resourcestring
  RC_DefaultSubject = 'JvPack';
  RC_DefaultEmail = 'JEDI-VCL@yahoogroups.com';

  {***************************************************}

constructor TJvMailTo.Create(AOwner: TComponent);
begin
  inherited;
  FSubject := RC_DefaultSubject;
  FTo := RC_DefaultEmail;
  FBCC := '';
  FCC := '';
  Cursor := crHandPoint;
  Font.Color := clBlue;
  HotTrack := True;
  HotTrackFont.Color := clRed;
  HotTrackFont.Style := [fsUnderline];
end;

{**************************************************}

procedure TJvMailTo.Click;
var
  mail: string;
begin
  inherited;
  mail := 'mailto:' + FTo;
  if FCC <> '' then
    mail := mail + '?CC=' + FCC;
  if FSubject <> '' then
    mail := mail + '?SUBJECT=' + FSubject;
  if FBcc <> '' then
    mail := mail + '?BCC=' + FBCC;
  OpenObject(mail);
end;

end.
