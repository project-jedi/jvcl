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

UNIT JvgSysInf;

INTERFACE

USES
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

TYPE

   TJvgSysInfo = CLASS(TJvComponent)
   PRIVATE
      {data fields for properties}
      FMemoryLoad: integer;
      FTotalPhys: integer;
      FAvailPhys: integer;
      FTotalPageFile: integer;
      FAvailPageFile: integer;
      FTotalVirtual: integer;
      FAvailVirtual: integer;
      FColorDepth: integer;
      FSystemFont: STRING;
      FOSPlatform: STRING;
      FVRefreshRate: integer;
      FGraphicResolution: STRING;
      FCPUKind: integer;
      FCPUName: STRING;
      FComputerName: STRING;
      FUserName: STRING;
      iNone: integer;
      sNone: STRING;
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
   PUBLIC
      PROCEDURE Refresh;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
   PUBLISHED
      PROPERTY MemoryLoad: integer READ FMemoryLoad WRITE iNone STORED false;
      PROPERTY TotalPhys: integer READ FTotalPhys WRITE iNone STORED false;
      PROPERTY AvailPhys: integer READ FAvailPhys WRITE iNone STORED false;
      PROPERTY TotalPageFile: integer READ FTotalPageFile WRITE iNone STORED
         false;
      PROPERTY AvailPageFile: integer READ FAvailPageFile WRITE iNone STORED
         false;
      PROPERTY TotalVirtual: integer READ FTotalVirtual WRITE iNone STORED
         false;
      PROPERTY AvailVirtual: integer READ FAvailVirtual WRITE iNone STORED
         false;

      PROPERTY CPUKind: Integer READ FCPUKind WRITE iNone STORED false;
      PROPERTY CPUName: STRING READ FCPUName WRITE sNone STORED false;
      PROPERTY ColorDepth: integer READ FColorDepth WRITE iNone STORED false;
      PROPERTY SystemFont: STRING READ FSystemFont WRITE sNone STORED false;
      PROPERTY OSPlatform: STRING READ FOSPlatform WRITE sNone STORED false;
      PROPERTY VRefreshRate: integer READ FVRefreshRate WRITE iNone STORED
         false;
      PROPERTY GraphicResolution: STRING READ FGraphicResolution WRITE sNone
         STORED false;
      PROPERTY ComputerName: STRING READ FComputerName WRITE sNone STORED false;
      PROPERTY UserName: STRING READ FUserName WRITE sNone STORED false;
   END;

CONST
   i8086                      = 1;      { & 8088 CPU }
   i80286                     = 2;
   i80386                     = 3;
   i80486                     = 4;
   iPentium                   = 5;      { P5 - Pentium }
   iPentiumPro                = 6;      { P6 - Pentium Pro & Celeron}

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils;

CONSTRUCTOR TJvgSysInfo.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   IF csDesigning IN ComponentState THEN
      Refresh;
END;

PROCEDURE TJvgSysInfo.Loaded;
BEGIN
   Refresh;
END;

PROCEDURE TJvgSysInfo.Refresh;
VAR
   DC                         : HDC;
   OSVersionInfo              : TOSVersionInfo;
   SI                         : TSystemInfo;
   MS                         : TMemoryStatus;
BEGIN
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

   CASE SI.wProcessorLevel OF
      //    i8086: Result := '8086';
      //    i80286: Result := '80286';
      i80386: FCPUName := '80386';
      i80486: FCPUName := '80486';
      iPentium: FCPUName := 'Pentium';
      iPentiumPro: FCPUName := 'Pentium Pro/Celeron';
   ELSE
      FCPUName := Format('P%d', [SI.wProcessorLevel]);
   END;

   IF OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT THEN
      FOSPlatform := 'NT'
   ELSE IF OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS THEN
      FOSPlatform := '95';

   FVRefreshRate := GetDeviceCaps(DC, VREFRESH);
   FColorDepth := GetDeviceCaps(DC, BITSPIXEL);

   FGraphicResolution := Format('%dx%d', [GetDeviceCaps(DC, HORZRES),
      GetDeviceCaps(DC, VERTRES)]);

   IF GetDeviceCaps(DC, LOGPIXELSX) = 96 THEN
      FSystemFont := 'SmallFont'
   ELSE IF GetDeviceCaps(DC, LOGPIXELSX) = 120 THEN
      FSystemFont := 'BigFont';

   FComputerName := JvgUtils.ComputerName;
   FUserName := JvgUtils.UserName;

   ReleaseDC(0, DC);
END;

{registration procedure}

PROCEDURE Register;
BEGIN
//   RegisterComponents('Globus Components', [TJvgSysInfo]);
END;

END.

