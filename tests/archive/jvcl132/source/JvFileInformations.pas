{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFileInformations.PAS, released on 2001-02-28.

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

unit JvFileInformations;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShellApi, JvTypes, JvComponent;

type
  TJvFileInformations = class(TJvComponent)
  private
  published
    function GetInformation(Value: string): TFileInformation;
  end;

implementation

{*****************************************************}

function TJvFileInformations.GetInformation(Value: string): TFileInformation;
var
  tmp: SHFILEINFO;
  flags: Cardinal;
begin
  flags := SHGFI_ICONLOCATION;
  SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.Location := tmp.szDisplayName;

  flags := SHGFI_DISPLAYNAME + SHGFI_ATTRIBUTES + SHGFI_TYPENAME + SHGFI_SYSICONINDEX;
  SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.DisplayName := tmp.szDisplayName;
  Result.Attributes := tmp.dwAttributes;
  Result.TypeName := tmp.szTypeName;
  Result.SysIconIndex := tmp.iIcon;

  flags := SHGFI_EXETYPE + SHGFI_ICON;
  Result.ExeType := SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.Icon := tmp.hIcon;
end;

end.
