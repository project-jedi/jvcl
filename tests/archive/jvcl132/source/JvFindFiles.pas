{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindFiles.PAS, released on 2001-02-28.

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

unit JvFindFiles;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ActiveX, ShellApi, Shlobj,
  JvBaseDlg, JvTypes;

type
  TJvFindFiles = class(TJvCommonDialog)
  private
    FDrive: Char;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Drive: Char read FDrive write FDrive default 'A';
    function Execute: Boolean; override;
  end;

implementation

{*****************************************************}

constructor TJvFindFiles.Create(AOwner: TComponent);
begin
  inherited;
  FDrive := 'C';
end;

{*****************************************************}

function TJvFindFiles.Execute: Boolean;
var
  PMalloc: IMalloc;
  sei: TShellExecuteInfo;
begin
  try
    SHGetMalloc(PMalloc);
    ZeroMemory(@sei, SizeOf(sei));
    with sei do
    begin
      cbSize := SizeOf(sei);
      lpFile := PChar(FDrive + ':\');
      lpVerb := 'find';
    end;
    ShellExecuteEx(@sei);
  finally
    pMalloc._Release;
    pMalloc := nil;
  end;
  Result := True;
end;

end.
