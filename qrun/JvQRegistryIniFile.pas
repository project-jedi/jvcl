{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegIni.PAS, released on 2004-03-16.

The Initial Developer of the Original Code is: André Snepvangers [asn@xs4all.nl]
Copyright (c) 2004 André Snepvangers
All Rights Reserved.

Contributor(s):

Last Modified: 2004-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvQRegistryIniFile;

interface

uses
  Classes, SysUtils, IniFiles;

type
  TJvRegistryIniFile = class(TObject)
  private
    FSection: string;
    FIniFile: TIniFile;
    function GetFilename: string;
  protected
  public
//    constructor Create; override;
    constructor Create(const FileName: string); overload;
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
  end;

implementation

uses
  QForms, DateUtils, RTLConsts;

type
  EJvRegIniFileException = class(Exception);

procedure ReadError(const Name: string);
begin
  raise EJvRegIniFileException.CreateResFmt(@SInvalidRegType, [Name]);
end;

constructor TJvRegistryIniFile.Create(const FileName: string);
begin
  inherited Create;
  if FileName = ''
  then
    FIniFile := TIniFile.Create(FileName)
  else
    FIniFile := TIniFile.Create(GetEnvironmentVariable('HOME')+ '/.' + ExtractFileName(Application.ExeName));
end;

destructor TJvRegistryIniFile.Destroy;
begin
  FIniFile.Free;
end;

function TJvRegistryIniFile.GetFilename: string;
begin
  Result := FIniFile.FileName;
end;

function TJvRegistryIniFile.OpenKey(const Key: string; AllowCreate: boolean): boolean;
begin
  Result := FIniFile.SectionExists(Key) or AllowCreate;
  FSection := Key;
end;

function TJvRegistryIniFile.CloseKey: boolean;
begin
  FIniFile.UpdateFile;
  Result := true;
end;

function TJvRegistryIniFile.DeleteKey(const Key: String): boolean;
begin
  FIniFile.EraseSection(Key);
  Result := FIniFile.SectionExists(Key);
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
