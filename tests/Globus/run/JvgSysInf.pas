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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgSysInf;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  JvComponent,
  extctrls;

type

  TJvgSysInfo = class(TJvComponent)
  private
    {data fields for properties}
    FMemoryLoad: integer;
    FTotalPhys: integer;
    FAvailPhys: integer;
    FTotalPageFile: integer;
    FAvailPageFile: integer;
    FTotalVirtual: integer;
    FAvailVirtual: integer;
    FColorDepth: integer;
    FSystemFont: string;
    FOSPlatform: string;
    FVRefreshRate: integer;
    FGraphicResolution: string;
    FCPUKind: integer;
    FCPUName: string;
    FComputerName: string;
    FUserName: string;
    iNone: integer;
    sNone: string;
  protected
    procedure Loaded; override;
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
  published
    property MemoryLoad: integer read FMemoryLoad write iNone stored false;
    property TotalPhys: integer read FTotalPhys write iNone stored false;
    property AvailPhys: integer read FAvailPhys write iNone stored false;
    property TotalPageFile: integer read FTotalPageFile write iNone stored
      false;
    property AvailPageFile: integer read FAvailPageFile write iNone stored
      false;
    property TotalVirtual: integer read FTotalVirtual write iNone stored
      false;
    property AvailVirtual: integer read FAvailVirtual write iNone stored
      false;

    property CPUKind: Integer read FCPUKind write iNone stored false;
    property CPUName: string read FCPUName write sNone stored false;
    property ColorDepth: integer read FColorDepth write iNone stored false;
    property SystemFont: string read FSystemFont write sNone stored false;
    property OSPlatform: string read FOSPlatform write sNone stored false;
    property VRefreshRate: integer read FVRefreshRate write iNone stored
      false;
    property GraphicResolution: string read FGraphicResolution write sNone
      stored false;
    property ComputerName: string read FComputerName write sNone stored false;
    property UserName: string read FUserName write sNone stored false;
  end;

const
  i8086 = 1; { & 8088 CPU }
  i80286 = 2;
  i80386 = 3;
  i80486 = 4;
  iPentium = 5; { P5 - Pentium }
  iPentiumPro = 6; { P6 - Pentium Pro & Celeron}

procedure Register;

implementation
uses JvgUtils;

constructor TJvgSysInfo.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then
    Refresh;
end;

procedure TJvgSysInfo.Loaded;
begin
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
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  GetSystemInfo(SI);
  DC := GetDC(0);

  FMemoryLoad := MS.dwMemoryLoad;
  FTotalPhys := MS.dwTotalPhys;
  FAvailPhys := MS.dwAvailPhys;
  FTotalPageFile := MS.dwTotalPageFile;
  FAvailPageFile := MS.dwAvailPageFile;
  FTotalVirtual := MS.dwTotalVirtual;
  FAvailVirtual := MS.dwAvailVirtual;

  FCPUKind := SI.wProcessorLevel;

  case SI.wProcessorLevel of
    //    i8086: Result := '8086';
    //    i80286: Result := '80286';
    i80386: FCPUName := '80386';
    i80486: FCPUName := '80486';
    iPentium: FCPUName := 'Pentium';
    iPentiumPro: FCPUName := 'Pentium Pro/Celeron';
  else
    FCPUName := Format('P%d', [SI.wProcessorLevel]);
  end;

  if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    FOSPlatform := 'NT'
  else if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    FOSPlatform := '95';

  FVRefreshRate := GetDeviceCaps(DC, VREFRESH);
  FColorDepth := GetDeviceCaps(DC, BITSPIXEL);

  FGraphicResolution := Format('%dx%d', [GetDeviceCaps(DC, HORZRES),
    GetDeviceCaps(DC, VERTRES)]);

  if GetDeviceCaps(DC, LOGPIXELSX) = 96 then
    FSystemFont := 'SmallFont'
  else if GetDeviceCaps(DC, LOGPIXELSX) = 120 then
    FSystemFont := 'BigFont';

  FComputerName := JvgUtils.ComputerName;
  FUserName := JvgUtils.UserName;

  ReleaseDC(0, DC);
end;

{registration procedure}

procedure Register;
begin
  //   RegisterComponents('Globus Components', [TJvgSysInfo]);
end;

end.
