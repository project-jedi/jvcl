{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSurfTo.PAS, released on 2001-02-28.

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

unit JvSurfTo;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvFunctions, JvComponent;

type
  TJvSurfTo = class(TJvComponent)
  published
    procedure SurfTo(url: string);
    procedure MailTo(Email, Cc, Bcc, Subject: string);
  end;

implementation

{*************************************************}

procedure TJvSurfTo.SurfTo(url: string);
begin
  OpenObject(Url);
end;

{*************************************************}

procedure TJvSurfTo.MailTo(Email, Cc, Bcc, Subject: string);
var
  mail: string;
begin
  mail := 'mailto:' + Email;
  if Subject <> '' then
    mail := mail + '?SUBJECT=' + Subject;
  if Bcc <> '' then
    mail := mail + '?BCC=' + Bcc;
  if Cc <> '' then
    mail := mail + '?CC=' + Cc;
  OpenObject(mail);
end;

end.
