{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDiskPrompt.PAS, released on 2001-02-28.

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

unit JvDiskPrompt;

interface

uses
  Windows, SysUtils, Classes,
  JvCommonDialogD, JvTypes;

type
  TJvDiskPrompt = class(TJvCommonDialogD)
  private
    FPathToSource: string;
    FTagFile: string;
    FNewPath: string;
    FFileSought: string;
    FDiskName: string;
    FStyle: TJvDiskStyles;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: TJvDiskRes; override;
  published
    property DiskName: string read FDiskName write FDiskName;
    property PathToSource: string read FPathToSource write FPathToSource;
    property FileSought: string read FFileSought write FFileSought;
    property TagFile: string read FTagFile write FTagFile;
    property NewPath: string read FNewPath write FNewPath;
    property Style: TJvDiskStyles read FStyle write FStyle default [];
  end;

implementation

uses
  JclSysUtils;

const
  DPROMPT_SUCCESS        = 0;
  DPROMPT_CANCEL         = 1;
  DPROMPT_SKIPFILE       = 2;
  DPROMPT_BUFFERTOOSMALL = 3;
  DPROMPT_OUTOFMEMORY    = 4;

type
  TSetupPromptForDisk = function(hwndParent: HWND; const DialogTitle, DiskName,
    PathToSource, FileSought, TagFile: PChar; DiskPromptStyle: DWORD;
    PathBuffer: PChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;

constructor TJvDiskPrompt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDiskName := '';
  FPathToSource := '';
  FFileSought := '';
  FTagFile := '';
  FNewPath := '';
  FStyle := [];
end;

function TJvDiskPrompt.Execute: TJvDiskRes;
var
  Required: DWORD;
  Res: array [0..255] of Char;
  SetupPromptForDisk: TSetupPromptForDisk;
begin
  SetupPromptForDisk := GetProcAddress(SetupApiDllHandle, 'SetupPromptForDiskA');
  case SetupPromptForDisk(OwnerWindow, PCharOrNil(Title), PCharOrNil(DiskName),
      PCharOrNil(PathToSource), PChar(FileSought), PCharOrNil(TagFile),
      JvDiskStylesToDWORD(Style), Res, SizeOf(Res), Required) of
    DPROMPT_SUCCESS:
      begin
        FNewPath := Res;
        Result := dsSuccess;
      end;
    DPROMPT_CANCEL:
      Result := dsCancel;
    DPROMPT_SKIPFILE:
      Result := dsSkipfile;
  else
    Result := dsError;
  end;
end;

end.
