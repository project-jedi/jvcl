{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegIni.PAS, released on 2004-03-16.

The Initial Developer of the Original Code is: André Snepvangers [asn att xs4all dott nl]
Copyright (c) 2004 André Snepvangers
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Under construction.
-----------------------------------------------------------------------------}
// $Id$

unit JvQRegistryIniFile;

interface

uses
  Classes, SysUtils, IniFiles;


//
// returns user homedir (string!)
//
function HKEY_CURRENT_USER: string;
  
type
  TJvRegistryIniFile = class(TObject)
  private
    FHKEY : string;
    FIniName: string;
    FSection: string;
    FIniFile: TIniFile;
    function GetFilename: string;
    procedure SetRoot(Value: string);
  protected
    //
    //  Keyname to IniFile/Section conversion:
    //
    //  - .borland = does not exist:
    //    .borland/.jvcl/Settings -> Inifile = .borland/.jvcl , Section = [Settings]
    //    .borland/jvcl/Settings -> IniFile = .borland, Section = [jvcl/Settings]
    //
    //  - .borland = existing dir:
    //    .borland/jvcl/Settings -> Inifile = .borland/jvcl , Section = [Settings]
    //
    //  - .borland = existing inifile
    //    .borland/.jvcl/Settings -> Inifile = .borland , Section = [.jvcl/Settings]
    //
    function Key2IniSection(const Key: string; out ininame: string;
      out section: string; AllowCreate: boolean = false): boolean; dynamic;
  public
//    constructor Create; override;
    constructor Create(const Root: string); overload;
    destructor Destroy; override;
    function OpenKey(const Key: string; AllowCreate: boolean): boolean;
    function CloseKey: boolean;

    function KeyExists(const Key: String): Boolean;
    function ValueExists(const Ident: string): boolean;
    function DeleteValue(const Ident: string): boolean;
    function DeleteKey(const Key: String): boolean;

    procedure GetValueNames(Strings: TStrings);

    function ReadString(const Ident: string): string;
    procedure WriteString(const Ident, Value: String);
    function ReadInteger(const Ident: string): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    function ReadBool(const Ident: string): Boolean;
    procedure WriteBool(const Ident: string; Value: Boolean);
    function ReadBinaryStream(const Ident: string; Value: TStream): Integer;
    function ReadDate(const Ident: string): TDateTime;
    function ReadDateTime(const Ident: string): TDateTime;
    function ReadFloat(const Ident: string): Double;
    function ReadTime(const Ident: string): TDateTime;
    procedure WriteBinaryStream(const Ident: string; Value: TStream);
    procedure WriteDate(const Ident: string; Value: TDateTime);
    procedure WriteDateTime(const Ident: string; Value: TDateTime);
    procedure WriteFloat(const Ident: string; Value: Double);
    procedure WriteTime(const Ident: string; Value: TDateTime);
    property CurrentKey: string read FSection;
    property CurrentRoot: string read GetFilename;
    property RootKey: string read FHKEY write SetRoot;
  end;

  TRegIniFile = TJvRegistryIniFile;

implementation

uses
  QForms, DateUtils, RTLConsts;

type
  EJvRegIniFileException = class(Exception);

procedure ReadError(const Name: string);
begin
  raise EJvRegIniFileException.CreateResFmt(@SInvalidRegType, [Name]);
end;

function HKEY_CURRENT_USER: string;
begin
  Result := GetEnvironmentVariable('HOME');
end;

constructor TJvRegistryIniFile.Create(const Root: string);
begin
  inherited Create;
  if Root <> ''
  then
    FHKey := Root
  else
    FHKey := HKEY_CURRENT_USER; //+ '/.' + ExtractFileName(Application.ExeName));
end;

destructor TJvRegistryIniFile.Destroy;
begin
  if assigned(FIniFile)
  then
    FIniFile.Free;
  inherited;
end;

procedure TJvRegistryIniFile.SetRoot(Value: string);
begin
  if Value <> FHKEY then
  begin
    if assigned(FIniFile) then
    begin
      FIniFile.Free;
      FIniFile := nil;
    end;
    FHKEY := Value;
  end;
end;

function TJvRegistryIniFile.GetFilename: string;
begin
  Result := FIniFile.FileName;
end;

function TJvRegistryIniFile.Key2IniSection(const Key: string;
  out ininame: string; out section: string; AllowCreate: boolean): boolean;
var
  strlist: TStrings;
  i : integer;
begin
  // TODO allowcreate
  strlist := TStringList.Create;
  strlist.Delimiter := '/';
  strlist.DelimitedText := Key;
  Ininame := FHKEY;
  i := 0;
  while (i < strlist.Count) and FileExists(IniName + PathDelim + strlist[i]) do
  begin
    IniName := IniName + PathDelim + strlist[i];
    inc(i);
  end;
  if i < strList.Count then
  begin
    Section := strList[i];
    inc(i);
    while i < strlist.count do
    begin
      Section := Section + PathDelim + strlist[i];
      inc(i);
    end
  end
  else
    Section := '' ;
  strlist.free;
  Result := (FileExists(IniName) or AllowCreate) and not DirectoryExists(IniName);
end;

function TJvRegistryIniFile.OpenKey(const Key: string; AllowCreate: boolean): boolean;
begin
  Result := Key2IniSection(Key, FIniName, FSection, AllowCreate);
  if Result = false then exit;
  try
    FIniFile := TIniFile.Create(FIniName);
    Result := FIniFile.SectionExists(FSection) or AllowCreate;
  except
    Result := false;
  end;
end;

function TJvRegistryIniFile.CloseKey: boolean;
begin
  if assigned(FIniFile) then
    FreeAndNil(FIniFile);
  FIniName := '';
  Result := true;
end;

function TJvRegistryIniFile.DeleteKey(const Key: String): boolean;
var
  Name, Sect: string;
begin
  Result := Key2IniSection(Key, Name, Sect);
  if Result then
    if Sect <> '' then
    begin
      with TIniFile.Create(Name) do
      begin
        EraseSection(Sect);
        Result := not SectionExists(Sect);
        Free;
      end;
    end
    else
    begin
      try
        if not DirectoryExists(Name) then
          DeleteFile(Name);
        // else   // delete directory ?
        Result := not FileExists(Name)
      except
        Result := false;
      end;
    end;
end;

procedure TJvRegistryIniFile.GetValueNames(strings :TStrings);
begin
  FIniFile.ReadSectionValues(FSection, strings);
end;

function TJvRegistryIniFile.KeyExists(const Key: string): boolean;
begin
  Result := FIniFile.SectionExists(Key);
end;

function TJvRegistryIniFile.ValueExists(const Ident: string): boolean;
begin
  Result := FIniFile.ValueExists(FSection, Ident);
end;

function TJvRegistryIniFile.DeleteValue(const Ident: string): boolean;
begin
  FIniFile.DeleteKey(FSection, Ident);
  Result := ValueExists(Ident);
end;

function TJvRegistryIniFile.ReadString(const Ident: string): string;
begin
  Result := FIniFile.ReadString(FSection, Ident , '');
end;

procedure TJvRegistryIniFile.WriteString(const Ident, Value: String);
begin
  FIniFile.WriteString(FSection, Ident, Value);
end;

function TJvRegistryIniFile.ReadInteger(const Ident: string): Longint;
begin
  Result := 0;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    try
      Result := strtoint(FIniFile.ReadString(FSection, Ident, ''));
    except
      ReadError(Ident);
    end;
end;

procedure TJvRegistryIniFile.WriteInteger(const Ident: string; Value: integer);
begin
  FIniFile.WriteInteger(FSection, Ident, Value);
end;

function TJvRegistryIniFile.ReadBool(const Ident: string): Boolean;
begin
  Result := false;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    Result := FIniFile.ReadBool(FSection, Ident, false);
end;

procedure TJvRegistryIniFile.WriteBool(const Ident: string; Value: Boolean);
begin
  FIniFile.WriteBool(FSection, Ident, Value);
end;

function TJvRegistryIniFile.ReadBinaryStream(const Ident: string; Value: TStream): Integer;
begin
  Result := 0;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    Result := FIniFile.ReadBinaryStream(FSection, Ident, Value);
end;

function TJvRegistryIniFile.ReadDate(const Ident: string): TDateTime;
begin
  Result := Today;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    try
      Result := StrToDate(FIniFile.ReadString(FSection, Ident, ''));
    except
      ReadError(Ident);
    end;
end;

function TJvRegistryIniFile.ReadDateTime(const Ident: string): TDateTime;
begin
  Result := Now;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    try
      Result := StrToDateTime(FIniFile.ReadString(FSection, Ident, ''));
    except
      ReadError(Ident);
    end;
end;

function TJvRegistryIniFile.ReadFloat(const Ident: string): Double;
begin
  Result := 0.0;
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    try
      Result := StrToFloat(FIniFile.ReadString(FSection, Ident, ''));
    except
      ReadError(Ident);
    end;
end;

function TJvRegistryIniFile.ReadTime(const Ident: string): TDateTime;
begin
  Result := TimeOf(Now);
  if not FIniFile.ValueExists(FSection, Ident) then
    ReadError(Ident)
  else
    try
      Result := StrToTime(FIniFile.ReadString(FSection, Ident, ''));
    except
      ReadError(Ident);
    end;
end;

procedure TJvRegistryIniFile.WriteBinaryStream(const Ident: string; Value: TStream);
begin
  FIniFile.WriteBinaryStream(FSection, Ident, Value);
end;

procedure TJvRegistryIniFile.WriteDate(const Ident: string; Value: TDateTime);
begin
  FIniFile.WriteDate(FSection, Ident, Value);
end;

procedure TJvRegistryIniFile.WriteDateTime(const Ident: string; Value: TDateTime);
begin
  FIniFile.WriteDateTime(FSection, Ident, Value);
end;

procedure TJvRegistryIniFile.WriteFloat(const Ident: string; Value: Double);
begin
  FIniFile.WriteFloat(FSection, Ident, Value);
end;

procedure TJvRegistryIniFile.WriteTime(const Ident: string; Value: TDateTime);
begin
  FIniFile.WriteTime(FSection, Ident, Value);
end;

end.
