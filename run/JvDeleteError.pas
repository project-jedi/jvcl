{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDeleteError.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDeleteError;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes,
  JvCustomFileMessageDialog, JvTypes;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDeleteError = class(TJvCustomFileMessageDialog)
  private
    FWin32ErrorCode: Integer;
    FFileName: TFileName;
    FStyle: TJvDeleteStyles;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName: TFileName read FFileName write FFileName;
    property Win32ErrorCode: Integer read FWin32ErrorCode write FWin32ErrorCode default 0;
    property Style: TJvDeleteStyles read FStyle write FStyle;
    function Execute: TJvDiskRes; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvSetupApi;

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
begin
  Sty := 0;
  if idNoBeep in Style then
    Sty := Sty or IDF_NOBEEP;
  if idNoForeground in Style then
    Sty := Sty or IDF_NOFOREGROUND;

  case SetupDeleteError(OwnerWindow, Pointer(Title), PChar(FileName), FWin32ErrorCode, Sty) of
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
