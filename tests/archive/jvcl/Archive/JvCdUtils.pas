{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCdUtils.PAS, released on 2001-02-28.

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

unit JvCdUtils;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, MMSystem, Forms, JvComponent;

type
  TJvCdUtils = class(TJvComponent)
  published
    procedure OpenCdDrive;
    procedure CloseCdDrive;
  end;

implementation

resourcestring
  RC_OpenCDDrive = 'Set cdaudio door open wait';
  RC_CloseCDDrive = 'Set cdaudio door closed wait';

  {*********************************************************}

procedure TJvCdUtils.OpenCdDrive;
begin
  mciSendString(PChar(RC_OpenCDDrive), nil, 0, Application.Handle);
end;

{*********************************************************}

procedure TJvCdUtils.CloseCdDrive;
begin
  mciSendString(PChar(RC_CloseCDDrive), nil, 0, Application.Handle);
end;

end.
