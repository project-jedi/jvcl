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

{$I JVCL.INC}

unit JvDeleteError;

interface

uses
  Windows, SysUtils, Classes,
  JvCommonDialogD, JvTypes;

type
  TJvDeleteError = class(TJvCommonDialogD)
  private
    FWin32ErrorCode: Integer;
    FFileName: string;
    FStyle: TJvDeleteStyles;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName: string read FFileName write FFileName;
    property Win32ErrorCode: Integer read FWin32ErrorCode write FWin32ErrorCode default 0;
    property Style: TJvDeleteStyles read FStyle write FStyle;
    function Execute: TJvDiskRes; override;
  end;

implementation

uses
  JclSysUtils;

const
  IDF_NOBROWSE     = $00000001;
  IDF_NOSKIP       = $00000002;
  IDF_NODETAILS    = $00000004;
  IDF_NOCOMPRESSED = $00000008;
  IDF_CHECKFIRST   = $00000100;
  IDF_NOBEEP       = $00000200;
  IDF_NOFOREGROUND = $00000400;
  IDF_WARNIFSKIP   = $00000800;
  IDF_OEMDISK      = DWORD($80000000);

  DPROMPT_SUCCESS        = 0;
  DPROMPT_CANCEL         = 1;
  DPROMPT_SKIPFILE       = 2;
  DPROMPT_BUFFERTOOSMALL = 3;
  DPROMPT_OUTOFMEMORY    = 4;

type
  TSetupDeleteError = function(hwndParent: HWND; const DialogTitle, File_: PAnsiChar;
    Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;

constructor TJvDeleteError.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := [];
  FFileName := '';
  FWin32ErrorCode := 0;
end;

function TJvDeleteError.Execute: TJvDiskRes;
var
  Sty: DWORD;
  SetupDeleteError: TSetupDeleteError;
begin
  Sty := 0;
  if idNoBeep in Style then
    Sty := Sty or IDF_NOBEEP;
  if idNoForeground in Style then
    Sty := Sty or IDF_NOFOREGROUND;

  SetupDeleteError := GetProcAddress(SetupApiDllHandle, 'SetupDeleteErrorA');
  case SetupDeleteError(OwnerWindow, PCharOrNil(Title), PChar(FileName), FWin32ErrorCode, Sty) of
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

end.

