{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDeleteError.PAS, released on 2001-02-28.

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

unit JvDeleteError;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupApi, JvCommonDialogD, JvBaseDlg, JvTypes;

type
  TJvDeleteError = class(TJvCommonDialogD)
  private
    FError: Integer;
    FFile: string;
    FStyle: TDeleteStyles;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName: string read FFile write FFile;
    property W32ErrorCode: Integer read FError write FError default 0;
    property Style: TDeleteStyles read FStyle write FStyle;
    function Execute: TDiskRes; override;
  end;

implementation

{**************************************************}

constructor TJvDeleteError.Create(AOwner: TComponent);
begin
  inherited;
  // (rom) TODO? The if is not needed
  if AOwner is TWinControl then
  begin
    FStyle := [];
    FFile := '';
    FError := 0;
  end;
end;

{**************************************************}

function TJvDeleteError.Execute: TDiskRes;
var
  Sty: DWORD;
begin
  Result := dsError;
  Sty := 0;
  if idNoBeep in FStyle then
    Sty := Sty or IDF_NOBEEP;
  if idNoForeground in FStyle then
    Sty := Sty or IDF_NOFOREGROUND;

  if @FSetupDeleteError <> nil then
  begin
    case FSetupDeleteError(FHandle, PChar(FTitle), PChar(FFile), FError, Sty) of
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
