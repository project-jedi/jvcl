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
  Hofi

Last Modified: 2005-04-10

Changes:
2005-04-10:      by outchy
  * Issue 2854: wrong parameter in TJvAppRegistryStorage.DoReadBinary
                and TJvAppRegistryStorage.DoWriteBinary
2004-10-11:      by Hofi
  * Changed
      in class
        TJvAppRegistryStorage
          Root can be set to 'Software\%COMPANY_NAME%\%APPL_NAME%' by default
            via UseOldDefaultRoot property, just like earlier in the original
            RX TFormStorage version. (see below what %APPL_NAME% and %COMPANY_NAME% is)
            You can use
              - '%NONE%' to sign an empty Root
                (design time empty value or spaces automatically converted)
              - '%APPL_NAME%' to sign in Root a new path element equal to Application name
              - '%COMPANY_NAME%' to sign in Root a new path element equal to DefCompanyName
          Create(AOwner: TComponent);
  * Added
      to class
        TJvAppRegistryStorage
          property UseOldDefaultRoot:
          procedure Loaded; override;
          procedure SetRoot(const Value: string);
          procedure SetUseOldDefaultRoot(Value: Boolean);

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppRegistryStorage;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Forms,
  JvAppStorage, JvTypes;

type
  TJvAppRegistryStorageOptions = class(TJvAppStorageOptions)
  private
  public
  published
    //Flag to determine if a stringlist should be stored as single string and not as list of string items
    property StoreStringListAsSingleString;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAppRegistryStorage = class(TJvCustomAppStorage)
  private
    FRegHKEY: HKEY;
    FUseOldDefaultRoot: Boolean;
    FAppNameErrorHandled: Boolean;
    FCompanyNameErrorHandled: Boolean;
    function GetStorageOptions: TJvAppRegistryStorageOptions;
    procedure SetStorageOptions(const Value: TJvAppRegistryStorageOptions);
  protected
    procedure Loaded; override;

    function GetRegRoot: TJvRegKey;
    procedure SetRegRoot(Value: TJvRegKey);
    procedure SetRoot(const Value: string);
    procedure SetUseOldDefaultRoot(Value: Boolean);

    { Create the registry key path if it doesn't exist yet. Any key in the path that doesn't exist
      is created. }
    procedure CreateKey(const Key: string);

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
    function DoReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer; override;
    procedure DoWriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer); override;
    function DoReadWideString(const Path: string; const Default: Widestring): Widestring; override;
    procedure DoWriteWideString(const Path: string; const Value: Widestring); override;
    class function GetStorageOptionsClass: TJvAppStorageOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RegRoot: TJvRegKey read GetRegRoot write SetRegRoot default hkCurrentUser;
    property Root read GetRoot write SetRoot;
    property SubStorages;
    property FlushOnDestroy;
    property UseOldDefaultRoot: Boolean read FUseOldDefaultRoot write SetUseOldDefaultRoot stored True default False ;

    property ReadOnly;
    property StorageOptions: TJvAppRegistryStorageOptions read GetStorageOptions
        write SetStorageOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Rev$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Dialogs,
  JclRegistry, JclResources,
  JvConsts, JvResources;

const
  cCount = 'Count';
  cSoftwareKey = 'Software';
  cNoRootMask = '%NONE%';
  cAppNameMask = '%APPL_NAME%';
  cCompanyNameMask = '%COMPANY_NAME%';
  cDefaultAppName = 'MyJVCLApplication';
  cDefaultCompanyName = 'MyCompany';
  cOldDefaultRootMask =  cSoftwareKey + RegPathDelim + cCompanyNameMask + RegPathDelim + cAppNameMask;

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
  FUseOldDefaultRoot := False;
  FAppNameErrorHandled := False;
  FCompanyNameErrorHandled := False;
end;

procedure TJvAppRegistryStorage.SetRoot(const Value: string);
var
  S: string;
  Changed: Boolean;

begin
  inherited SetRoot(Value);
  if csDesigning in ComponentState then
  begin
    if Value <> cOldDefaultRootMask then
      FUseOldDefaultRoot := False;
    if GetRoot = '' then
      SetRoot(cNoRootMask);
  end
  else
  begin
    { this makes GetDefaultIniRegKey unnecessary ?!?! }
    if GetRoot = cNoRootMask then
      SetRoot('')
    else
      s := ActiveTranslateStringEngine.TranslateString(GetRoot, Changed);
      if changed then
        SetRoot (s);
  end;
end;

procedure TJvAppRegistryStorage.Loaded;
begin
  inherited Loaded;
  SetUseOldDefaultRoot(UseOldDefaultRoot);
  SetRoot(GetRoot);
end;

procedure TJvAppRegistryStorage.SetUseOldDefaultRoot(Value: Boolean);
begin
  FUseOldDefaultRoot := Value;
  if FUseOldDefaultRoot then
    SetRoot(cOldDefaultRootMask);
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

procedure TJvAppRegistryStorage.CreateKey(const Key: string);
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
          EnumRes := RegEnumKey(TmpHKEY, I, SubKeyName, Length(SubKeyName));
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
  NameLen: DWORD;
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
          NameLen := Length(Name);
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
        {$IFNDEF COMPILER25_UP}
        Result := False;
        {$ENDIF ~COMPILER25_UP}
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
      raise EJclRegistryError.CreateResFmt(@RsUnableToOpenKeyRead, [SubKey, ValueName]);
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
  DataType: Cardinal;
  {$IFDEF CPUX64}
  Ext80Value: Extended80;
  {$ENDIF CPUX64}
begin
  SplitKeyPath(Path, SubKey, ValueName);
  {$IFNDEF COMPILER25_UP}
  Result := Default;
  {$ENDIF ~COMPILER25_UP}
  try
    if not RegGetDataType(FRegHKEY, SubKey, ValueName, DataType) or (DataType = REG_BINARY) then
    begin
      {$IFDEF CPUX64}
      // Keep backward compatiblity to x86 Extended type
      RegReadBinary(FRegHKEY, SubKey, ValueName, Ext80Value, SizeOf(Ext80Value));
      try
        Result := Ext80Value
      except
        Result := Default;
      end;
      {$ELSE}
      RegReadBinary(FRegHKEY, SubKey, ValueName, Result, SizeOf(Result));
      {$ENDIF CPUX64}
    end
    else
      raise EJclRegistryError.CreateResFmt(@RsWrongDataType, ['', SubKey, ValueName]);
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
  {$IFDEF CPUX64}
  Ext80Value: Extended80;
  {$ENDIF CPUX64}
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  {$IFDEF CPUX64}
  // Keep backward compatiblity to x86 Extended type
  Ext80Value := Value;
  RegWriteBinary(FRegHKEY, SubKey, ValueName, Ext80Value, SizeOf(Ext80Value));
  {$ELSE}
  RegWriteBinary(FRegHKEY, SubKey, ValueName, Value, SizeOf(Value));
  {$ENDIF CPUX64}
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

function TJvAppRegistryStorage.DoReadWideString(const Path: string; const Default: Widestring): Widestring;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  try
    Result := RegReadWideStringDef(FRegHKEY, SubKey, ValueName, Default);
  except
    on E: EJclRegistryError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvAppRegistryStorage.DoWriteWideString(const Path: string; const Value: Widestring);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteWideString(FRegHKEY, SubKey, ValueName, Value);
end;

function TJvAppRegistryStorage.DoReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := RegReadBinary(FRegHKEY, SubKey, ValueName, Buf^, BufSize);
end;

procedure TJvAppRegistryStorage.DoWriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer);
var
  SubKey: string;
  ValueName: string;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  CreateKey(SubKey);
  RegWriteBinary(FRegHKEY, SubKey, ValueName, Buf^, BufSize);
end;

function TJvAppRegistryStorage.GetStorageOptions: TJvAppRegistryStorageOptions;
begin
  Result := TJvAppRegistryStorageOptions(inherited StorageOptions);
end;

class function TJvAppRegistryStorage.GetStorageOptionsClass:
    TJvAppStorageOptionsClass;
begin
  Result := TJvAppRegistryStorageOptions;
end;

procedure TJvAppRegistryStorage.SetStorageOptions(const Value:
    TJvAppRegistryStorageOptions);
begin
  (Inherited StorageOptions).Assign(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
