{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComputerInfo.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
p3 [peter3 at sourceforge dot net] - changed property writers to dummy methods - call SetXX methods directly to change values

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  PLEASE NOTE!
    In previous versions of this component, published properties could be used to change the
    computer info values in the registry. These published properties were also stored in the DFM file.
    At start-up, the registry values on the end-users computer could be changed (silently) to the
    developers values. The current implementation uses another scheme:
      the published properties are now all read-only and the class is derived from TObject, making it impossible to
      install in the IDE. To change a value, you must call the SetXX method explicitly
  2003-09-23:
    - Converted from TComponent -> TObject. If you are using this class in your projects, you will need
     to instantiate it dynamically since you can no longer drop it on a form.

    - If you open a(n old) form containing a TJvComputerinfo component, you will get an error message from Delphi
     ('TJvComputerInfo not found' or similar). Please click "Ignore All" to remove the component from the form permanently.
      You now need to create the class in code and get/set properties manually.

      This change has been done to promote safety since the TComponent based version could
      modify important registry values on end-user computers, creating a lot of problems.
  2003-12-10:
    Made all properties read-only.
    Changed UserName to RegisteredOwner
    Changed Company to RegisteredOrganization
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvComputerInfo;

interface

uses
  Windows, SysUtils, Classes, Registry, MMSystem,
  {$IFDEF VCL}
  Controls,
  {$ENDIF VCL}
  JvComponent, JvTypes;

type
  TJvComputerInfo = class(TObject)
  private
    function GetRegisteredOrganization: string;
    function GetComputerName: string;
    function GetRegisteredOwner: string;
    function GetComment: string;
    function GetWorkGroup: string;
    function GetDVDRegion: Integer;
    function GetProductID: string;
    function GetProductKey: string;
    function GetProductName: string;
    function GetVersion: string;
    function GetVersionNumber: string;
    function GetDayRunning: Integer;
    function GetTimeRunning: TTime;
    procedure WriteReg(Base: HKEY; KeyName, ValueName, Value: string);
    function ReadReg(Base: HKEY; KeyName, ValueName: string): string;
    function GetLoggedOnUser: string;
    function GetRealComputerName: string;
  public
    procedure SetCompany(const Value: string);
    procedure SetComputerName(const Value: string);
    procedure SetUsername(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetWorkGroup(const Value: string);
    procedure SetDVDRegion(const Value: Integer);
    procedure SetProductID(const Value: string);
    procedure SetProductKey(const Value: string);
    procedure SetProductName(const Value: string);
    procedure SetVersion(const Value: string);
    procedure SetVersionNumber(const Value: string);
  published
    // (p3)
    property RealComputerName: string read GetRealComputerName;
    property LoggedOnUser: string read GetLoggedOnUser;

    // This is the same as RealComputerName if you are running on NT
    property ComputerName: string read GetComputerName;
    property RegisteredOwner: string read GetRegisteredOwner;
    property RegisteredOrganization: string read GetRegisteredOrganization;

    property Comment: string read GetComment;
    property WorkGroup: string read GetWorkGroup;
    property ProductID: string read GetProductID;
    property ProductKey: string read GetProductKey;
    property ProductName: string read GetProductName;
    property DVDRegion: Integer read GetDVDRegion;
    property VersionNumber: string read GetVersionNumber;
    property Version: string read GetVersion;
    property TimeRunning: TTime read GetTimeRunning;
    property DayRunning: Integer read GetDayRunning;
  end;

implementation

const
  RC_VNetKey = 'System\CurrentControlSet\Services\Vxd\VNETSUP';
  RC_VNetKeyNT = '';
  RC_CurrentKey = 'Software\Microsoft\Windows\CurrentVersion';
  RC_CurrentKeyNT = 'Software\Microsoft\Windows NT\CurrentVersion';

  cOSCurrentKey: array [Boolean] of PChar =
    (RC_CurrentKey, RC_CurrentKeyNT);

  cComment = 'Comment';
  cRegisteredOrganization = 'RegisteredOrganization';
  cComputerName = 'ComputerName';
  cDVD_Region = 'DVD_Region';
  cProductID = 'ProductID';
  cProductKey = 'ProductKey';
  cProductName = 'ProductName';
  cRegisteredOwner = 'RegisteredOwner';
  cVersion = 'Version';
  cVersionNumber =  'VersionNumber';
  cWorkgroup =  'Workgroup';

function IsNT: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function TJvComputerInfo.GetComment: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cComment);
end;

function TJvComputerInfo.GetRegisteredOrganization: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[isNT], cRegisteredOrganization);
end;

function TJvComputerInfo.GetComputerName: string;
var
  Buf: array [0..31] of Char;
  Size: Cardinal;
begin
  if IsNT then
  begin
    Size := SizeOf(Buf);
    Windows.GetComputerName(Buf, Size);
    Result := Buf;
  end
  else
    Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cComputerName);
end;

function TJvComputerInfo.GetDayRunning: Integer;
begin
  Result := (((timeGetTime div 1000) div 60) div 60) div 24;
end;

function TJvComputerInfo.GetDVDRegion: Integer;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(cOSCurrentKey[IsNT], False);
    try
      if ValueExists(cDVD_Region) then
        Result := ReadInteger(cDVD_Region)
      else
        Result := -1;
    except
      Result := -1;
    end;
    Free;
  end;
end;

function TJvComputerInfo.GetProductID: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductID);
end;

function TJvComputerInfo.GetProductKey: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductKey);
end;

function TJvComputerInfo.GetProductName: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductName);
end;

function TJvComputerInfo.GetTimeRunning: TTime;
var
  h, m, s, mi: Integer;
  d: DWord;
begin
  d := timeGetTime;

  mi := d mod 1000;
  d := d div 1000;

  s := d mod 60;
  d := d div 60;

  m := d mod 60;
  d := d div 60;

  h := d mod 24;
  Result := EncodeTime(h, m, s, mi);
end;

function TJvComputerInfo.GetRegisteredOwner: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cRegisteredOwner);
end;

function TJvComputerInfo.GetVersion: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cVersion);
end;

function TJvComputerInfo.GetVersionNumber: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cVersionNumber);
end;

function TJvComputerInfo.GetWorkGroup: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cWorkgroup);
end;

function TJvComputerInfo.ReadReg(Base: HKEY; KeyName, ValueName: string): string;
begin
  with TRegistry.Create do
  begin
    RootKey := Base;
    OpenKey(KeyName, False);
    try
      if ValueExists(ValueName) then
        Result := ReadString(ValueName)
      else
        Result := '';
    except
      Result := '';
    end;
    Free;
  end;
end;

procedure TJvComputerInfo.SetComment(const Value: string);
begin
  if not IsNT then
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cComment, Value);
end;

procedure TJvComputerInfo.SetCompany(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cRegisteredOrganization, Value);
end;

procedure TJvComputerInfo.SetComputerName(const Value: string);
begin
  if not IsNT then
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cComputerName, Value);
end;

procedure TJvComputerInfo.SetDVDRegion(const Value: Integer);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(cOSCurrentKey[IsNT], False) then
      WriteInteger(cDVD_Region, Value);
    Free;
  end;
end;

procedure TJvComputerInfo.SetProductID(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductId, Value);
end;

procedure TJvComputerInfo.SetProductKey(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductKey, Value);
end;

procedure TJvComputerInfo.SetProductName(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cProductName, Value);
end;

procedure TJvComputerInfo.SetUsername(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cRegisteredOwner, Value);
end;

procedure TJvComputerInfo.SetVersion(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cVersion, Value);
end;

procedure TJvComputerInfo.SetVersionNumber(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], cVersionNumber, Value);
end;

procedure TJvComputerInfo.WriteReg(Base: HKEY; KeyName, ValueName, Value: string);
begin
  with TRegistry.Create do
  begin
    RootKey := Base;
    if OpenKey(KeyName, False) then
      WriteString(ValueName, Value);
    Free;
  end;
end;

procedure TJvComputerInfo.SetWorkGroup(const Value: string);
begin
  if not IsNT then
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, cWorkgroup, Value);
end;

function TJvComputerInfo.GetLoggedOnUser: string;
var
  Buf: array [0..255] of Char; // too large really, but who knows if it'll change?
  Size: Cardinal;
begin
  Size := SizeOf(Buf);
  Buf[0] := #0;
  if Windows.GetUserName(Buf, Size) then
    Result := Buf
  else
    Result := '';
end;

function TJvComputerInfo.GetRealComputerName: string;
var
  Buf: array [0..255] of Char; // too large really, but who knows if it'll change?
  Size: Cardinal;
begin
  Size := SizeOf(Buf);
  Buf[0] := #0;
  Windows.GetComputerName(Buf, Size);
  Result := Buf;
end;

end.

