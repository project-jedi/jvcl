{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCpuUsage.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCpuUsage;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Classes, Registry,
  JvComponent;

// (rom) the whole component seems to be badly designed

type
  TJvCpuUsage = class(TJvComponent)
  private
    FValue: Cardinal;
    FUsage: string;
    FRegistry: TRegistry;
    function GetUsage: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Usage: string read GetUsage write FUsage;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils;

const
  RC_CpuUsageKey = 'KERNEL\CPUUsage';
  RC_PerfStart = 'PerfStats\StartStat';
  RC_PerfStop = 'PerfStats\StopStat';
  RC_PerfStat = 'PerfStats\StatData';

constructor TJvCpuUsage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_DYN_DATA;
  FRegistry.OpenKey(RC_PerfStart, False);
  FRegistry.ReadBinaryData(RC_CpuUsageKey, FValue, SizeOf(FValue));
end;

destructor TJvCpuUsage.Destroy;
begin
  FRegistry.OpenKey(RC_PerfStop, False);
  FRegistry.ReadBinaryData(RC_CpuUsageKey, FValue, SizeOf(FValue));
  FRegistry.CloseKey;
  FRegistry.Free;
  inherited Destroy;
end;

function TJvCpuUsage.GetUsage: string;
begin
  FRegistry.OpenKey(RC_PerfStat, False);
  FRegistry.ReadBinaryData(RC_CpuUsageKey, FValue, SizeOf(FValue));
  FRegistry.CloseKey;
  Result := IntToStr(FValue);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

