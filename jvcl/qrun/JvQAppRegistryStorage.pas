{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppRegistryStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQAppRegistryStorage;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Classes,
  JvQAppStorage, JvQTypes;

type
  TJvAppRegistryStorage = class(TJvCustomAppStorage)
  private
    FRegHKEY: HKEY;
  protected
    function GetRegRoot: TJvRegKey;
    procedure SetRegRoot(Value: TJvRegKey);

    { Create the registry key path if it doesn't exist yet. Any key in the path that doesn't exist
      is created. }
    procedure CreateKey(Key: string);

    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;

    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; override;
    function PathExistsInt(const Path: string): Boolean; override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;

    function DoReadBoolean(const Path: string; Default: Boolean): Boolean; override;
    procedure DoWriteBoolean(const Path: string; Value: Boolean); override;
    function DoReadInteger(const Path: string; Default: Integer): Integer; override;
    procedure DoWriteInteger(const Path: string; Value: Integer); override;
    function DoReadFloat(const Path: string; Default: Extended): Extended; override;
    procedure DoWriteFloat(const Path: string; Value: Extended); override;
    function DoReadString(const Path: string; const Default: string): string; override;
    procedure DoWriteString(const Path: string; const Value: string); override;
    function DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; override;
    procedure DoWriteBinary(const Path: string; const Buf; BufSize: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RegRoot: TJvRegKey read GetRegRoot write SetRegRoot default hkCurrentUser;
    property Root;
    property SubStorages;
  end;

implementation

uses
  SysUtils,
  JclRegistry, JclResources,
  JvQConsts, JvQResources;

const
  cCount = 'Count';

{ (rom) disabled unused
const
  HKEY_Names: array [HKEY_CLASSES_ROOT..HKEY_DYN_DATA, 0..1] of PChar =
   (
    ('HKEY_CLASSES_ROOT', 'HKCR'),
    ('HKEY_CURRENT_USER', 'HKCU'),
    ('HKEY_LOCAL_MACHINE', 'HKLM'),
    ('HKEY_USERS', 'HKU'),
    ('HKEY_PERFORMANCE_DATA', 'HKPD'),
    ('HKEY_CURRENT_CONFIG', 'HKCC'),
    ('HKEY_DYN_DATA', 'HKDD')
   );
}

//=== { TJvAppRegistryStorage } ==============================================

constructor TJvAppRegistryStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegHKEY := HKEY_CURRENT_USER;
end;

function TJvAppRegistryStorage.GetRegRoot: TJvRegKey;
begin
  Result := TJvRegKey(FRegHKEY - HKEY_CLASSES_ROOT);
end;

procedure TJvAppRegistryStorage.SetRegRoot(Value: TJvRegKey);
begin
  if Value <> RegRoot then
    FRegHKEY := HKEY_CLASSES_ROOT + Longword(Ord(Value));
end;

procedure TJvAppRegistryStorage.CreateKey(Key: string);
var
  ResKey: HKEY;
begin
  if not RegKeyExists(FRegHKEY, Key) then
    if Windows.RegCreateKey(FRegHKEY, PChar(Key), ResKey) = ERROR_SUCCESS then
      RegCloseKey(ResKey)
    else
      raise EJVCLException.CreateResFmt(@RsEUnableToCreateKey, [Key]);
end;

procedure TJvAppRegistryStorage.EnumFolders(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  Key: string;
  TmpHKEY: HKEY;
  I: Integer;
  SubKeyName: array [0..255] of Char;
  EnumRes: Longint;
begin
  Key := GetAbsPath(Path);
  if RegKeyExists(FRegHKEY, Key) then
    if RegOpenKey(FRegHKEY, PChar(Key), TmpHKEY) = ERROR_SUCCESS then
    begin
      Strings.BeginUpdate;
      try
        I := 0;
        repeat
          EnumRes := RegEnumKey(TmpHKEY, I, SubKeyName, SizeOf(SubKeyName));
          if (EnumRes = ERROR_SUCCESS) and (not ReportListAsValue or
              not ListStored(Path + RegPathDelim + SubKeyName)) then
            Strings.Add(SubKeyName);
          Inc(I);
        until EnumRes <> ERROR_SUCCESS;
        if EnumRes <> ERROR_NO_MORE_ITEMS then
          raise EJclRegistryError.CreateRes(@RsEEnumeratingRegistry);
      finally
        RegCloseKey(TmpHKEY);
        Strings.EndUpdate;
      end;
    end;
end;

procedure TJvAppRegistryStorage.EnumValues(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  PathIsList: Boolean;
  Key: string;
  TmpHKEY: HKEY;
  I: Integer;
  Name: array [0..511] of Char;
  NameLen: Cardinal;
  EnumRes: Longint;
begin
  PathIsList := ReportListAsValue and ListStored(Path);
  if PathIsList then
    Strings.Add('');
  Key := GetAbsPath(Path);
  if RegKeyExists(FRegHKEY, Key) then
    if RegOpenKey(FRegHKEY, PChar(Key), TmpHKEY) = ERROR_SUCCESS then
    begin
      Strings.BeginUpdate;
      try
        I := 0;
        repeat
          NameLen := SizeOf(Name);
          EnumRes := RegEnumValue(TmpHKEY, I, Name, NameLen, nil, nil, nil, nil);
          if (EnumRes = ERROR_SUCCESS) and (not PathIsList or (not AnsiSameText(cCount, Name) and
              not NameIsListItem(Name))) then
            Strings.Add(Name);
          Inc(I);
        until EnumRes <> ERROR_SUCCESS;
        if EnumRes <> ERROR_NO_MORE_ITEMS then
          raise EJclRegistryError.CreateRes(@RsEEnumeratingRegistry);
      finally
        RegCloseKey(TmpHKEY);
        Strings.EndUpdate;
      end;
    end;
end;

function TJvAppRegistryStorage.IsFolderInt(const Path: string; ListIsValue: Boolean): Boolean;
var
  RefPath: string;
  PathHKEY: HKEY;
  I: Integer;
  Name: array [0..511] of Char;
  NameLen: Cardinal;
  EnumRes: Longint;
begin
  Result := False;
  RefPath := GetAbsPath(Path);
  if RegOpenKey(FRegHKEY, PChar(RefPath), PathHKEY) = ERROR_SUCCESS then
    try
      Result := True;
      if ListIsValue and (RegQueryValueEx(PathHKEY, cCount, nil, nil, nil, nil) = ERROR_SUCCESS) then
      begin
        Result := False;
        I := 0;
        repeat
          NameLen := SizeOf(Name);
          EnumRes := RegEnumValue(PathHKEY, I, Name, NameLen, nil, nil, nil, nil);
          Result := (EnumRes = ERROR_SUCCESS) and not AnsiSameText(cCount, Name) and
            not NameIsListItem(Name);
          Inc(I);
        until (EnumRes <> ERROR_SUCCESS) or Result;
        if EnumRes <> ERROR_NO_MORE_ITEMS then
          raise EJclRegistryError.CreateRes(@RsEEnumeratingRegistry);
      end;
    finally
      RegCloseKey(PathHKEY);
    end;
end;

function TJvAppRegistryStorage.PathExistsInt(const Path: string): Boolean;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := RegKeyExists(FRegHKEY, SubKey + RegPathDelim + ValueName);
end;

function TJvAppRegistryStorage.ValueStoredInt(const Path: string): Boolean;
var
  SubKey: string;
  ValueName: string;
  TmpKey: HKEY;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := RegKeyExists(FRegHKEY, SubKey);
  if Result then
    if RegOpenKey(FRegHKEY, PChar(SubKey), TmpKey) = ERROR_SUCCESS then
      try
        Result := RegQueryValueEx(TmpKey, PChar(ValueName), nil, nil, nil, nil) = ERROR_SUCCESS;
      finally
        RegCloseKey(TmpKey);
      end
    else
      raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyRead, [SubKey]);
end;

procedure TJvAppRegistryStorage.DeleteValueInt(const Path: string);
var
  SubKey: string;
  ValueName: string;
begin
  if ValueStored(Path) then
  begin
    SplitKeyPath(Path, SubKey, ValueName);
    RegDeleteEntry(FRegHKEY, SubKey, ValueName);
  end;
end;

procedure TJvAppRegistryStorage.DeleteSubTreeInt(const Path: string);
var
  KeyRoot: string;
begin
  KeyRoot := GetAbsPath(Path);
  if RegKeyExists(FRegHKEY, KeyRoot) then
    RegDeleteKeyTree(FRegHKEY, KeyRoot);
end;

function TJvAppRegistryStorage.DoReadInteger(const Path: string; Default: Integer): Integer;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  try
    Result := RegReadIntegerDef(FRegHKEY, SubKey, ValueName, Default);
  except
    on E: EJclRegistryError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvAppRegistryStorage.DoWriteInteger(const Path: string; Value: Integer);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteInteger(FRegHKEY, SubKey, ValueName, Value);
end;

function TJvAppRegistryStorage.DoReadBoolean(const Path: string; Default: Boolean): Boolean;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  try
    Result := RegReadBoolDef(FRegHKEY, SubKey, ValueName, Default);
  except
    on E: EJclRegistryError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvAppRegistryStorage.DoWriteBoolean(const Path: string; Value: Boolean);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteBool(FRegHKEY, SubKey, ValueName, Value);
end;

function TJvAppRegistryStorage.DoReadFloat(const Path: string; Default: Extended): Extended;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := Default;
  try
    RegReadBinary(FRegHKEY, SubKey, ValueName, Result, SizeOf(Result));
  except
    on E: EJclRegistryError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvAppRegistryStorage.DoWriteFloat(const Path: string; Value: Extended);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteBinary(FRegHKEY, SubKey, ValueName, Value, SizeOf(Value));
end;

function TJvAppRegistryStorage.DoReadString(const Path: string; const Default: string): string;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  try
    Result := RegReadStringDef(FRegHKEY, SubKey, ValueName, Default);
  except
    on E: EJclRegistryError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvAppRegistryStorage.DoWriteString(const Path: string; const Value: string);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteString(FRegHKEY, SubKey, ValueName, Value);
end;

function TJvAppRegistryStorage.DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := RegReadBinary(FRegHKEY, SubKey, ValueName, Buf, BufSize);
end;

procedure TJvAppRegistryStorage.DoWriteBinary(const Path: string; const Buf; BufSize: Integer);
var
  SubKey: string;
  ValueName: string;
  TmpBuf: Byte;
begin
  TmpBuf := Byte(Buf);
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteBinary(FRegHKEY, SubKey, ValueName, TmpBuf, BufSize);
end;

end.

