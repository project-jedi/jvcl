{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChooseIconDlg.PAS, released on 2001-02-28.

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

unit JvChooseIconDlg;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvHDialogs2, JvTypes;

type
  TJvChooseIconDlg = class(TJvCommonDialog)
  private
    FIndex: Integer;
    FFileName: TFileName;
  published
    function Execute: Boolean; override;
    property FileName: TFileName read FFileName write FFileName;
    property IconIndex: Integer read FIndex write FIndex;
  end;

implementation

{**************************************************}

function TJvChooseIconDlg.Execute: Boolean;
var
  pSize, pIndex: DWord;
begin
  pIndex := FIndex;
  pSize := Length(FFileName);
  Result := SHPickIconDlg(0, PWideChar(WideString(FFileName)), pSize, pIndex);
  FIndex := pIndex;
end;

end.
