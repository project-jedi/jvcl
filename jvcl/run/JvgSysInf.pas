{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSysInf.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgSysInf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  JvComponent;

type

  TJvgSysInfo = class(TJvComponent)
  private
    FMemoryLoad: Integer;
    FTotalPhys: Integer;
    FAvailPhys: Integer;
    FTotalPageFile: Integer;
    FAvailPageFile: Integer;
    FTotalVirtual: Integer;
    FAvailVirtual: Integer;
    FColorDepth: Integer;
    FSystemFont: string;
    FOSPlatform: string;
    FVRefreshRate: Integer;
    FGraphicResolution: string;
    FCPUKind: Integer;
    FCPUName: string;
    FComputerName: string;
    FUserName: string;
    FINone: Integer;
    FSNone: string;
  protected
    procedure Loaded; override;
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
  published
    property MemoryLoad: Integer read FMemoryLoad write FINone stored False;
    property TotalPhys: Integer read FTotalPhys write FINone stored False;
    property AvailPhys: Integer read FAvailPhys write FINone stored False;
    property TotalPageFile: Integer read FTotalPageFile write FINone stored False;
    property AvailPageFile: Integer read FAvailPageFile write FINone stored False;
    property TotalVirtual: Integer read FTotalVirtual write FINone stored False;
    property AvailVirtual: Integer read FAvailVirtual write FINone stored False;

    property CPUKind: Integer read FCPUKind write FINone stored False;
    property CPUName: string read FCPUName write FSNone stored False;
    property ColorDepth: Integer read FColorDepth write FINone stored False;
    property SystemFont: string read FSystemFont write FSNone stored False;
    property OSPlatform: string read FOSPlatform write FSNone stored False;
    property VRefreshRate: Integer read FVRefreshRate write FINone stored False;
    property GraphicResolution: string read FGraphicResolution write FSNone stored False;
    property ComputerName: string read FComputerName write FSNone stored False;
    property UserName: string read FUserName write FSNone stored False;
  end;

implementation

uses
  JvgUtils;

const
  i8086 = 1; { & 8088 CPU }
  i80286 = 2;
  i80386 = 3;
  i80486 = 4;
  iPentium = 5; { P5 - Pentium }
  iPentiumPro = 6; { P6 - Pentium Pro & Celeron}

constructor TJvgSysInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then
    Refresh;
end;

procedure TJvgSysInfo.Loaded;
begin
  // (rom) added inherited Loaded
  inherited Loaded;
  Refresh;
end;

procedure TJvgSysInfo.Refresh;
var
  DC: HDC;
  OSVersionInfo: TOSVersionInfo;
  SI: TSystemInfo;
  MS: TMemoryStatus;
begin
  MS.dwLength := SizeOf(MS);
  GlobalMemoryStatus(MS);
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  GetSystemInfo(SI);

  FMemoryLoad := MS.dwMemoryLoad;
  FTotalPhys := MS.dwTotalPhys;
  FAvailPhys := MS.dwAvailPhys;
  FTotalPageFile := MS.dwTotalPageFile;
  FAvailPageFile := MS.dwAvailPageFile;
  FTotalVirtual := MS.dwTotalVirtual;
  FAvailVirtual := MS.dwAvailVirtual;

  FCPUKind := SI.wProcessorLevel;

  case SI.wProcessorLevel of
    i8086:
      FCPUName := '8086';
    i80286:
      FCPUName := '80286';
    i80386:
      FCPUName := '80386';
    i80486:
      FCPUName := '80486';
    iPentium:
      FCPUName := 'Pentium';
    iPentiumPro:
      FCPUName := 'Pentium Pro/Celeron';
  else
    FCPUName := Format('P%d', [SI.wProcessorLevel]);
  end;

  if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    FOSPlatform := 'NT'
  else
  if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    FOSPlatform := '95';

  DC := GetDC(HWND_DESKTOP);

  FVRefreshRate := GetDeviceCaps(DC, VREFRESH);
  FColorDepth := GetDeviceCaps(DC, BITSPIXEL);

  FGraphicResolution := Format('%dx%d', [GetDeviceCaps(DC, HORZRES),
    GetDeviceCaps(DC, VERTRES)]);

  if GetDeviceCaps(DC, LOGPIXELSX) = 96 then
    FSystemFont := 'SmallFont'
  else
  if GetDeviceCaps(DC, LOGPIXELSX) = 120 then
    FSystemFont := 'BigFont';

  ReleaseDC(HWND_DESKTOP, DC);

  FComputerName := JvgUtils.ComputerName;
  FUserName := JvgUtils.UserName;
end;

end.

