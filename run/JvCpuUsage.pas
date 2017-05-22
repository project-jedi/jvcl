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

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com]
  Olivier Sannier [obones att altern dott org]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCpuUsage;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Registry,
  JvComponentBase;

type
  TJvCpuUsage = class(TJvComponent)
  private
    // Used with GetSystemTimes
    FPrevIdleTimeGST: TDateTime;
    FPrevSystemTimeGST: TDateTime;

    // Used with NtQuerySystemInformation (because GetSystemTimes is not available)
    FPrevIdleTimeNQSI: LARGE_INTEGER;
    FPrevSystemTimeNQSI: LARGE_INTEGER;

    // Used when neither NtQuerySystemInformation nor GetSystemTimes are available
    FRegistry: TRegistry;

    function GetUsage: Double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Usage: Double read GetUsage;
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
  Math, SysUtils, JclDateTime;

const
  RC_CpuUsageKey = 'KERNEL\CPUUsage';
  RC_PerfStart = 'PerfStats\StartStat';
  RC_PerfStop = 'PerfStats\StopStat';
  RC_PerfStat = 'PerfStats\StatData';

const
  SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeInformation = 3;

type
  TPDWord = ^DWORD;

  TSystem_Basic_Information = packed record
    dwUnknown1: DWORD;
    uKeMaximumIncrement: ULONG;
    uPageSize: ULONG;
    uMmNumberOfPhysicalPages: ULONG;
    uMmLowestPhysicalPage: ULONG;
    uMmHighestPhysicalPage: ULONG;
    uAllocationGranularity: ULONG;
    pLowestUserAddress: Pointer;
    pMmHighestUserAddress: Pointer;
    uKeActiveProcessors: ULONG;
    bKeNumberProcessors: byte;
    bUnknown2: byte;
    wUnknown3: word;
  end;

type
  TSystem_Performance_Information = packed record
    liIdleTime: LARGE_INTEGER; {LARGE_INTEGER}
    dwSpare: array[0..75] of DWORD;
  end;

type
  TSystem_Time_Information = packed record
    liKeBootTime: LARGE_INTEGER;
    liKeSystemTime: LARGE_INTEGER;
    liExpTimeZoneBias: LARGE_INTEGER;
    uCurrentTimeZoneId: ULONG;
    dwReserved: DWORD;
  end;

type
  TNtQuerySystemInformation = function(infoClass: DWORD;
    buffer: Pointer;
    bufSize: DWORD;
    returnSize: TPDword): DWORD; stdcall;

var
  NtQuerySystemInformation: TNtQuerySystemInformation;


type
  TGetSystemTimes = function (var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;

var
  GetSystemTimes: TGetSystemTimes; 

function Li2Double(Value: LARGE_INTEGER): Double;
begin
  Result := (Value.HighPart * IntPower(2, 32)) + Value.LowPart;
end;

function GetCPUUsageNQSI(var PrevIdleTime: LARGE_INTEGER; var PrevSystemTime: LARGE_INTEGER): Double;
var
  SysBaseInfo: TSystem_Basic_Information;
  SysPerfInfo: TSystem_Performance_Information;
  SysTimeInfo: TSystem_Time_Information;
  Status: Integer;
  SystemTime: Double;
  IdleTime: Double;
begin
  Result := 0;

  if not Assigned(NtQuerySystemInformation) then
    Exit;

  Status := NtQuerySystemInformation(SystemBasicInformation, @SysBaseInfo, SizeOf(SysBaseInfo), nil);
  if Status <> 0 then
    Exit;

  // get new system time
  Status := NtQuerySystemInformation(SystemTimeInformation, @SysTimeInfo, SizeOf(SysTimeInfo), Nil);
  if Status <> 0 then
    Exit;

  // get new CPU's idle time
  Status := NtQuerySystemInformation(SystemPerformanceInformation, @SysPerfInfo, SizeOf(SysPerfInfo), nil);
  if Status <> 0 then
    Exit;

  // if it's a first call - skip it
  if (PrevIdleTime.QuadPart <> 0) then
  begin
    // CurrentValue = NewValue - OldValue
    IdleTime := Li2Double(SysPerfInfo.liIdleTime) - Li2Double(PrevIdleTime);
    SystemTime := Li2Double(SysTimeInfo.liKeSystemTime) - Li2Double(PrevSystemTime);

    // CurrentCpuIdle = IdleTime / SystemTime
    IdleTime := IdleTime / SystemTime;

    // CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
    IdleTime := 100.0 - IdleTime * 100.0 / SysBaseInfo.bKeNumberProcessors + 0.5;

    // Show Percentage
    Result := IdleTime;
    if Result > 100 then
      Result := 100;
  end;

  // store new CPU's idle and system time
  PrevIdleTime := SysPerfInfo.liIdleTime;
  PrevSystemTime := SysTimeInfo.liKeSystemTime;
end;

function GetCPUUsageGST(var PrevIdleTime: TDateTime; var PrevSystemTime: TDateTime): Double;
var
  ProcessorsCount: Integer;

  TmpNewIdleTime: TFileTime;
  TmpNewKernelTime: TFileTime;
  TmpNewUserTime: TFileTime;

  NewIdleTime: TDateTime;

  NewSystemTime: TDateTime;
  IdleTimeDiff: TDateTime;
  SystemTimeDiff: TDateTime;
  SysInfo: TSystemInfo;
begin
  Result := 0;
  
  if not Assigned(GetSystemTimes) then
    Exit;
    
  // Get number of CPUs
  GetSystemInfo(SysInfo);
  ProcessorsCount := SysInfo.dwNumberOfProcessors;

  // Get system times
  if not GetSystemTimes(TmpNewIdleTime, TmpNewKernelTime, TmpNewUserTime) then
    Exit;
  NewSystemTime := Now;
  NewIdleTime := FileTimeToDateTime(TmpNewIdleTime);

  // compute the CPU usage but skip if it's a first call
  if PrevIdleTime <> 0 then
  begin
    IdleTimeDiff := NewIdleTime - PrevIdleTime;
    SystemTimeDiff := NewSystemTime - PrevSystemTime;

    Result := 100.0 - ((IdleTimeDiff) * 100) / (SystemTimeDiff) / ProcessorsCount;

    if Result > 100 then
      Result := 100;
  end;

  PrevIdleTime := NewIdleTime;
  PrevSystemTime := NewSystemTime;
end;


constructor TJvCpuUsage.Create(AOwner: TComponent);
var
  CurValue: Cardinal;
begin
  inherited Create(AOwner);

  if not Assigned(@NtQuerySystemInformation) then
  begin
    FRegistry := TRegistry.Create;

    FRegistry.RootKey := HKEY_DYN_DATA;
    FRegistry.OpenKey(RC_PerfStart, False);
    FRegistry.ReadBinaryData(RC_CpuUsageKey, CurValue, SizeOf(CurValue));
  end;
end;

destructor TJvCpuUsage.Destroy;
var
  CurValue: Cardinal;
begin
  if not Assigned(@NtQuerySystemInformation) then
  begin
    FRegistry.OpenKey(RC_PerfStop, False);
    FRegistry.ReadBinaryData(RC_CpuUsageKey, CurValue, SizeOf(CurValue));
    FRegistry.CloseKey;

    FRegistry.Free;
  end;

  inherited Destroy;
end;

function TJvCpuUsage.GetUsage: Double;
var
  CurValue: Cardinal;
begin
  if Assigned(@GetSystemTimes) then
  begin
    Result := GetCPUUsageGST(FPrevIdleTimeGST, FPrevSystemTimeGST);
  end
  else
  if Assigned(@NtQuerySystemInformation) then
  begin
    Result := GetCPUUsageNQSI(FPrevIdleTimeNQSI, FPrevSystemTimeNQSI);
  end
  else
  begin
    FRegistry.OpenKey(RC_PerfStat, False);
    FRegistry.ReadBinaryData(RC_CpuUsageKey, CurValue, SizeOf(CurValue));
    FRegistry.CloseKey;

    Result := CurValue;
  end;
end;

initialization
  NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
  GetSystemTimes := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetSystemTimes');
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
