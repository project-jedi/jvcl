{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAssociateExtension.PAS, released on 2001-02-28.

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

unit JvAssociateExtension;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, JvTypes, JvComponent;

type
  TJvAssociateExtension = class(TJvComponent)
  published
    procedure Associate(IconPath, ProgramName, Path, Extension: string);
  end;

implementation

resourcestring
  RC_DefaultIcon = 'DefaultIcon';

  {*****************************************************}

procedure RebuildIconCache;
var
  bidon: DWord;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, bidon);
end;

{*****************************************************}

procedure AssociateFileExtension(IconPath, ProgramName, Path, Extension: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey(RC_DefaultIcon, True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + extension, True);
    WriteString('', ProgramName);
    Free;
  end;
  RebuildIconCache;
end;

{*****************************************************}

procedure TJvAssociateExtension.Associate(IconPath, ProgramName, Path, Extension: string);
begin
  AssociateFileExtension(IconPath, ProgramName, Path, Extension);
end;

end.
