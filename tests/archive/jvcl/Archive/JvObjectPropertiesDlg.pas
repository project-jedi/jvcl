{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvObjectPropertiesDlg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvObjectPropertiesDlg;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvHDialogs2, JvTypes;

type
  TJvObjectPropertiesDlg = class(TJvCommonDialog)
  private
    FFileType: TJvFileKind;
    FFileName: TFileName;
  public
    constructor Create(AOwner: TComponent); override;
  published
    function Execute: Boolean; override;
    property FileType: TJvFileKind read FFileType write FFileType default ftFile;
    property FileName: TFileName read FFileName write FFileName;
  end;

implementation

{**************************************************}

constructor TJvObjectPropertiesDlg.Create(AOwner: TComponent);
begin
  inherited;
  FFileType := ftFile;
end;

{**************************************************}

function TJvObjectPropertiesDlg.Execute: Boolean;
var
  Flags: Integer;
begin
  if FFileType = ftFile then
    Flags := OPF_PATHNAME
  else
    Flags := OPF_PRINTERNAME;
  Result := SHObjectProperties(0, Flags, PWideChar(WideString(FFileName)), nil);
end;

end.
