{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRenameError.PAS, released on 2001-02-28.

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

unit JvRenameError;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupApi, JvCommonDialogD, JvBaseDlg, JvTypes;

type
  TJvRenameError = class(TJvCommonDialogD)
  private
    FError: Integer;
    FStyle: TDeleteStyles;
    FDest: string;
    FSource: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SourceFile: string read FSource write FSource;
    property DestFile: string read FDest write FDest;
    property W32ErrorCode: Integer read FError write FError default 0;
    property Style: TDeleteStyles read FStyle write FStyle;
    function Execute: TDiskRes; override;
  end;

implementation

{**************************************************}

constructor TJvRenameError.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TWinControl then
  begin
    FStyle := [];
    FError := 0;
    FDest := '';
    FSource := '';
  end;
end;

{**************************************************}

function TJvRenameError.Execute: TDiskRes;
var
  Sty: DWORD;
begin
  Result := dsError;
  Sty := 0;
  if idNoBeep in Style then
    Sty := Sty or IDF_NOBEEP;
  if idNoForeground in Style then
    Sty := Sty or IDF_NOFOREGROUND;

  if @FSetupRenameError <> nil then
  begin
    case FSetupRenameError(FHandle, PChar(FTitle), PChar(FSource), PChar(FDest), FError, Sty) of
      DPROMPT_SUCCESS:
        Result := dsSuccess;
      DPROMPT_CANCEL:
        Result := dsCancel;
      DPROMPT_SKIPFILE:
        Result := dsSkipfile;
    else
      Result := dsError;
    end;
  end;
end;

end.
