{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComputerInfo.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
p3 [peter3@peter3.com] - changed property writers to dummy methods - call SetXX methods directly to change values

Last Modified: 2003-03-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  PLEASE NOTE!
    In previous versions of this component, published properties could be used to change the
    computer info values in the registry. These published properties were also stored in the DFM file.
    At start-up, the registry values on the end-users computer could be changed (silently) to the
    developers values. The current implementation uses another scheme: the published properties all have
    a dummy property setter (this is so you can see the value at design-time) but to actually change any value,
    you must call the SetXXX methods explicitly to change the values. This change also means that if you used
    a previous version of this component, you only need to recompile the application using it to remove any risk
    of modifying the end users computer.

    There is one problem though. Using code like

      JvComputerInfo1.Company := 'New company';

    to update a registry value, will not work anymore (the value is just ignored) and
    you will not receive any error messages telling you so. You must check
    through your code and manually change the code to

      JvComputerInfo1.SetCompany('New company');

    instead.

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvComputerInfo;

interface

uses
  Windows, SysUtils, Classes, Controls, Registry, MMSystem,
  JvComponent;

type
  TJvComputerInfo = class(TJvComponent)
  private
    FDummyStr:string;
    FDummyInt:integer;
    FDummyTime: TTime;
    function GetCompany: string;
    function GetComputerName: string;
    function GetUsername: string;
    function GetComment: string;
    function GetWorkGroup: string;
    function GetDVDRegion: Integer;
    function GetProductID: string;
    function GetProductKey: string;
    function GetProductName: string;
    function GetVersion: string;
    function GetVersionNumber: string;
    function GetDay: Integer;
    function GetTime: TTime;
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
    property RealComputerName: string read GetRealComputerName write FDummyStr;
    property LoggedOnUser: string read GetLoggedOnUser write FDummyStr;

    // This is the same as RealComputerName if you are running on NT
    property ComputerName: string read GetComputerName write FDummyStr;
    // this is really RegisteredOwner
    property Username: string read GetUsername write FDummyStr;
    // this is really RegisteredOrganization
    property Company: string read GetCompany write FDummyStr;
    property Comment: string read GetComment write FDummyStr;
    property WorkGroup: string read GetWorkGroup write FDummyStr;
    property ProductID: string read GetProductID write FDummyStr;
    property ProductKey: string read GetProductKey write FDummyStr;
    property ProductName: string read GetProductName write FDummyStr;
    property DVDRegion: Integer read GetDVDRegion write FDummyInt;
    property VersionNumber: string read GetVersionNumber write FDummyStr;
    property Version: string read GetVersion write FDummyStr;
    property TimeRunning: TTime read GetTime write FDummyTime stored false;
    property DayRunning: Integer read GetDay write FDummyInt stored false;
  end;

implementation

const
  RC_VNetKey = 'System\CurrentControlSet\Services\Vxd\VNETSUP';
  RC_VNetKeyNT = '';
  RC_CurrentKey = 'Software\Microsoft\Windows\CurrentVersion';
  RC_CurrentKeyNT = 'Software\Microsoft\Windows NT\CurrentVersion';

const
  cOSCurrentKey: array [Boolean] of string =
  (RC_CurrentKey, RC_CurrentKeyNT);

function IsNT: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function TJvComputerInfo.GetComment: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'Comment');
end;

function TJvComputerInfo.GetCompany: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[isNT], 'RegisteredOrganization');
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
    Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'ComputerName');
end;

function TJvComputerInfo.GetDay: Integer;
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
      if ValueExists('DVD_Region') then
        Result := ReadInteger('DVD_Region')
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
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductID');
end;

function TJvComputerInfo.GetProductKey: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductKey');
end;

function TJvComputerInfo.GetProductName: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductName');
end;

function TJvComputerInfo.GetTime: TTime;
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

function TJvComputerInfo.GetUsername: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'RegisteredOwner');
end;

function TJvComputerInfo.GetVersion: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'Version');
end;

function TJvComputerInfo.GetVersionNumber: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'VersionNumber');
end;

function TJvComputerInfo.GetWorkGroup: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'Workgroup');
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
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'Comment', Value);
end;

procedure TJvComputerInfo.SetCompany(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'RegisteredOrganization', Value);
end;

procedure TJvComputerInfo.SetComputerName(const Value: string);
begin
  if not IsNT then
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'ComputerName', Value);
end;

procedure TJvComputerInfo.SetDVDRegion(const Value: Integer);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(cOSCurrentKey[IsNT], False) then
      WriteInteger('DVD_Region', Value);
    Free;
  end;
end;

procedure TJvComputerInfo.SetProductID(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductId', Value);
end;

procedure TJvComputerInfo.SetProductKey(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductKey', Value);
end;

procedure TJvComputerInfo.SetProductName(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'ProductName', Value);
end;

procedure TJvComputerInfo.SetUsername(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'RegisteredOwner', Value);
end;

procedure TJvComputerInfo.SetVersion(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'Version', Value);
end;

procedure TJvComputerInfo.SetVersionNumber(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, cOSCurrentKey[IsNT], 'VersionNumber', Value);
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
    WriteReg(HKEY_LOCAL_MACHINE, RC_VNetKey, 'Workgroup', Value);
end;

function TJvComputerInfo.GetLoggedOnUser: string;
var
  Buf: array [0..255] of Char; // too large really, but who knows if it'll change?
  {$IFDEF COMPILER5_UP}
  Size: Cardinal;
  {$ELSE}
  Size: Integer;
  {$ENDIF}
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
  {$IFDEF COMPILER5_UP}
  Size: Cardinal;
  {$ELSE}
  Size: Integer;
  {$ENDIF}
begin
  Size := SizeOf(Buf);
  Buf[0] := #0;
  Windows.GetComputerName(Buf, Size);
  Result := buf;
end;

end.

