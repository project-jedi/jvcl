{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppIniStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar

Last Modified: 2004-01-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvAppIniStorage;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, QWindows,
  {$ENDIF LINUX}
  SysUtils, Classes, IniFiles,
  JvAppStorage;

type
  TJvAppIniStorage = class(TJvCustomAppStorage)
  private
    FDefaultSection : string;
  protected
    function ValueExists(const Section, Key: string): Boolean; virtual; abstract;
    function ReadValue(const Section, Key: string): string; virtual; abstract;
    procedure WriteValue(const Section, Key, Value: string); virtual; abstract;
    procedure RemoveValue(const Section, Key: string); virtual; abstract;
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    function DoReadInteger(const Path: string; Default: Integer): Integer; override;
    procedure DoWriteInteger(const Path: string; Value: Integer); override;
    function DoReadFloat(const Path: string; Default: Extended): Extended; override;
    procedure DoWriteFloat(const Path: string; Value: Extended); override;
    function DoReadString(const Path: string; Default: string): string; override;
    procedure DoWriteString(const Path: string; Value: string); override;
    function DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; override;
    procedure DoWriteBinary(const Path: string; const Buf; BufSize: Integer); override;

    property DefaultSection : string read FDefaultSection write FDefaultSection;
  end;

  { Storage to INI file. Optionally a buffered version (TMemIniFile) is used. IdleDelay will then
    determine how long there has to be no key or mouse activities or writes to the storage before
    it is written to the actual file. The non-buffered version will write directly to the file
    (TIniFile) is used. }
  TJvAppIniFileStorage = class(TJvAppIniStorage)
  private
    FBuffered: Boolean;
    FHasWritten: Boolean;
    FIniFile: TCustomIniFile;
    FLastUserAct: Longint;
    FFileName: TJvAppStorageFileName;
  protected
    procedure CreateIniFile(Name: string);
    procedure DestroyIniFile;
    procedure SetBuffered(Value: Boolean);
    function GetFileName: TJvAppStorageFileName;
    procedure SetFileName(Value: TJvAppStorageFileName);
    procedure FileNameChanged(Sender: TObject);
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    function PathExistsInt(const Path: string): boolean; override;
    function ValueExists(const Section, Key: string): Boolean; override;
    function IsFolderInt(Path: string; ListIsValue: Boolean = True): Boolean; override;
    function ReadValue(const Section, Key: string): string; override;
    procedure WriteValue(const Section, Key, Value: string); override;
    procedure RemoveValue(const Section, Key: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;

    property HasWritten: Boolean read FHasWritten;
    property IniFile: TCustomIniFile read FIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Flush;
  published
    property Buffered: Boolean read FBuffered write SetBuffered;
    property DefaultSection;
    property FileName: TJvAppStorageFileName read GetFileName write SetFileName;
    property SubStorages;
  end;

  { In memory INI file. The contents is not backed by a file at all, but descendants may be written
    that will read/store the contents to a physical device (file, DB, etc.).

    Disclaimer: this class has not yet been tested!! }
  TJvCustomAppIniMemoryStorage = class(TJvAppIniStorage)
  private
    FIsInternalChange: Boolean;
    FSections: TStringList;
    FStrings: TStringList;
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const S: string);
  protected
    procedure StringsChanged(Sender: TObject);
    procedure RebuildSections;
    function LocateSection(const Section: string; var Index: Integer): Boolean;
    function LocateValue(const Section, Key: string; var Index: Integer): Boolean;
    function LocateValueInSection(const Key: string; var Index: Integer): Boolean;
    function ValueExists(const Section, Key: string): Boolean; override;
    function ReadValue(const Section, Key: string): string; override;
    procedure WriteValue(const Section, Key, Value: string); override;
    procedure RemoveValue(const Section, Key: string); override;

    property IsInternalChange: Boolean read FIsInternalChange;
    // (rom) changed to indexed property
    property Strings[Index: Integer]: string read GetString write SetString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  JvTypes, JvResources;

const
  cNullDigit = '0';
  cCount = 'Count';
  cSectionHeaderStart = '[';
  cSectionHeaderEnd = ']';
  cKeyValueSeparator = '=';

function AnsiSameTextShortest(S1, S2: string): Boolean;
begin
  if Length(S1) > Length(S2) then
    SetLength(S1, Length(S2))
  else
  if Length(S2) > Length(S1) then
    SetLength(S2, Length(S1));
  Result := AnsiSameText(S1, S2);
end;

function BinStrToBuf(Value: string; var Buf; BufSize: Integer): Integer;
var
  P: PChar;
begin
  if Odd(Length(Value)) then
    Value := cNullDigit + Value;
  if (Length(Value) div 2) < BufSize then
    BufSize := Length(Value) div 2;
  Result := 0;
  P := PChar(Value);
  while (BufSize > 0) do
  begin
    PChar(Buf)[Result] := Chr(StrToInt('$' + P[0] + P[1]));
    Inc(Result);
    Dec(BufSize);
    Inc(P, 2);
  end;
end;

function BufToBinStr(const Buf; BufSize: Integer): string;
var
  P: PChar;
  S: string;
begin
  SetLength(Result, BufSize * 2);
  P := PChar(Result);
  Inc(P, (BufSize - 1) * 2); // Point to end of string ^
  while BufSize > 0 do
  begin
    S := IntToHex(Ord(PChar(Buf)[BufSize]), 2);
    P[0] := S[1];
    P[1] := S[2];
    Dec(P, 2);
    Dec(BufSize);
  end;
end;

//=== TJvAppIniStorage =========================================================

procedure TJvAppIniStorage.SplitKeyPath(const Path: string; out Key, ValueName: string);
begin
  inherited SplitKeyPath(Path, Key, ValueName);
  if Key = '' then
    Key := DefaultSection;
end;

function TJvAppIniStorage.ValueStoredInt(const Path: string): Boolean;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  Result := ValueExists(Section, Key);
end;

procedure TJvAppIniStorage.DeleteValueInt(const Path: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  RemoveValue(Section, Key);
end;

function TJvAppIniStorage.DoReadInteger(const Path: string; Default: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if Value = '' then
      Value := cNullDigit;
    Result := StrToInt(Value);
  end
  else
    Result := Default;
end;

procedure TJvAppIniStorage.DoWriteInteger(const Path: string; Value: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, IntToStr(Value));
end;

function TJvAppIniStorage.DoReadFloat(const Path: string; Default: Extended): Extended;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if Value = '' then
      Value := cNullDigit;
    Result := StrToFloat(Value);
  end
  else
    Result := Default;
end;

procedure TJvAppIniStorage.DoWriteFloat(const Path: string; Value: Extended);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, FloatToStr(Value));
end;

function TJvAppIniStorage.DoReadString(const Path: string; Default: string): string;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
    Result := ReadValue(Section, Key)
  else
    Result := Default;
end;

procedure TJvAppIniStorage.DoWriteString(const Path: string; Value: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, Value);
end;

function TJvAppIniStorage.DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    Result := BinStrToBuf(Value, Buf, BufSize);
  end
  else
    Result := 0;
end;

procedure TJvAppIniStorage.DoWriteBinary(const Path: string; const Buf; BufSize: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, BufToBinStr(Buf, BufSize));
end;

//=== TJvAppIniFileStorage =====================================================

procedure TJvAppIniFileStorage.CreateIniFile(Name: string);
begin
  if Buffered then
    FIniFile := TMemIniFile.Create(Name)
  else
    FIniFile := TIniFile.Create(Name);
end;

procedure TJvAppIniFileStorage.DestroyIniFile;
begin
  Flush;
  FreeAndNil(FIniFile);
end;

procedure TJvAppIniFileStorage.SetBuffered(Value: Boolean);
begin
  if Value <> Buffered then
  begin
    if Buffered and (IniFile <> nil) then
      IniFile.UpdateFile;
    FBuffered := Value;
    if IniFile <> nil then
    begin
      DestroyIniFile;
      CreateIniFile(FileName.GetFileName);
    end;
  end;
end;

function TJvAppIniFileStorage.GetFileName: TJvAppStorageFileName;
begin
  Result := FFileName;
end;

procedure TJvAppIniFileStorage.SetFileName(Value: TJvAppStorageFileName);
begin
end;

procedure TJvAppIniFileStorage.FileNameChanged(Sender: TObject);
begin
  DestroyIniFile;
  CreateIniFile(FileName.GetFileName);
end;

procedure TJvAppIniFileStorage.EnumFolders(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  RefPath: string;
  I: Integer;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := DefaultSection;
  IniFile.ReadSections(Strings);
  I := Strings.Count - 1;
  while I >= 0 do
  begin
    if (RefPath <> '') and ((Copy(Strings[I], 1, Length(RefPath) + 1) <> RefPath + '\') or
      (Pos('\', Copy(Strings[I], 2 + Length(RefPath), Length(Strings[I]) - Length(RefPath))) > 0)) then
      Strings.Delete(I)
    else
    if ReportListAsValue and ValueExists(Strings[I], cCount) then
      Strings.Delete(I)
    else
    if RefPath <> '' then
      Strings[I] := Copy(Strings[I], 1 + Length(RefPath), Length(Strings[I]) - Length(RefPath));
    Dec(I);
  end;
end;

procedure TJvAppIniFileStorage.EnumValues(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  PathIsList: Boolean;
  RefPath: string;
  I: Integer;
begin
  PathIsList := ReportListAsValue and ListStored(Path);
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := DefaultSection;
  IniFile.ReadSectionValues(RefPath, Strings);
  for I := Strings.Count - 1 downto 0 do
  begin
    Strings[I] := Copy(Strings[I], 1, Pos(cKeyValueSeparator, Strings[I]) - 1);
    if PathIsList and (AnsiSameText(cCount, Strings[I]) or NameIsListItem(Strings[I])) then
      Strings.Delete(I);
  end;
  if PathIsList then
    Strings.Add('');
end;


function TJvAppIniFileStorage.ValueExists(const Section, Key: string): Boolean;
begin
  if IniFile <> nil then
    Result := IniFile.ValueExists(Section, Key)
  else
    Result := False;
end;

function TJvAppIniFileStorage.ReadValue(const Section, Key: string): string;
begin
  if Section = '' then
    raise EJVCLAppStorageError.Create(RsEReadValueFailed);
  if IniFile <> nil then
    Result := IniFile.ReadString(Section, Key, '')
  else
    Result := '';
end;

procedure TJvAppIniFileStorage.WriteValue(const Section, Key, Value: string);
begin
  if IniFile <> nil then
  begin
    if Section = '' then
      raise EJVCLAppStorageError.Create(RsEWriteValueFailed);
    IniFile.WriteString(Section, Key, Value);
    FLastUserAct := GetTickCount;
    FHasWritten := True;
  end;
end;

procedure TJvAppIniFileStorage.DeleteSubTreeInt(const Path: string);
var
  TopSection: string;
  Sections: TStringList;
  I: Integer;
begin
  if IniFile <> nil then
  begin
    TopSection := GetAbsPath(Path);
    Sections := TStringList.Create;
    try
      IniFile.ReadSections(Sections);
      if TopSection = '' then
        for I := 0 to Sections.Count - 1 do
          IniFile.EraseSection(Sections[I])
      else
        for I := 0 to Sections.Count - 1 do
          if Pos(TopSection, Sections[I]) = 1 then
            IniFile.EraseSection(Sections[I]);
    finally
      Sections.Free;
    end;
  end;
end;

procedure TJvAppIniFileStorage.RemoveValue(const Section, Key: string);
begin
  if IniFile <> nil then
  begin
    if IniFile.ValueExists(Section, Key) then
    begin
      IniFile.DeleteKey(Section, Key);
      FLastUserAct := GetTickCount;
      FHasWritten := True;
    end
    else
    if IniFile.SectionExists(Section + '\' + Key) then
    begin
      IniFile.EraseSection(Section + '\' + Key);
      FLastUserAct := GetTickCount;
      FHasWritten := True;
    end;
  end;
end;

constructor TJvAppIniFileStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := TJvAppStorageFileName.Create('ini');
  FFileName.OnChange := FileNameChanged;
end;

destructor TJvAppIniFileStorage.Destroy;
begin
  Flush;
  inherited Destroy;
end;

function TJvAppIniFileStorage.PathExistsInt(const Path: string): boolean;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  Result := IniFile.SectionExists(Section + '\' + Key);
end;

procedure TJvAppIniFileStorage.Flush;
begin
  if Buffered and (IniFile <> nil) and HasWritten then
  begin
    IniFile.UpdateFile;
    FHasWritten := False;
  end;
end;

function TJvAppIniFileStorage.IsFolderInt(Path: string; ListIsValue: Boolean): Boolean;
var
  RefPath: string;
  ValueNames: TStrings;
  I: Integer;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := DefaultSection;
  Result := IniFile.SectionExists(RefPath);
  if Result and ListIsValue and IniFile.ValueExists(RefPath, cCount) then
  begin
    Result := False;
    ValueNames := TStringList.Create;
    try
      EnumValues(Path, ValueNames, True);
      I := ValueNames.Count - 1;
      while Result and (I >= 0) do
      begin
        Result := not AnsiSameText(ValueNames[I], cCount) and not NameIsListItem(ValueNames[I]);
        Dec(I);
      end;
    finally
      ValueNames.Free;
    end;
  end;
end;

//=== TJvCustomAppIniMemoryStorage ============================================

constructor TJvCustomAppIniMemoryStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FStrings.OnChange := StringsChanged;
  FSections := TStringList.Create;
end;

destructor TJvCustomAppIniMemoryStorage.Destroy;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FSections);
  inherited Destroy;
end;

function TJvCustomAppIniMemoryStorage.GetString(Index: Integer): string;
begin
  Result := FStrings[Index];
end;

procedure TJvCustomAppIniMemoryStorage.SetString(Index: Integer; const S: string);
begin
  FStrings[Index] := S;
end;

procedure TJvCustomAppIniMemoryStorage.StringsChanged(Sender: TObject);
begin
  if not IsInternalChange then
    RebuildSections;
end;

procedure TJvCustomAppIniMemoryStorage.RebuildSections;
var
  I: Integer;
begin
  FSections.Clear;
  for I := 0 to FStrings.Count - 1 do
    if Copy(FStrings[I], 1, 1) = cSectionHeaderStart then
      FSections.AddObject(Copy(FStrings[I], 2, Length(FStrings[I]) - 2), TObject(I));
  FSections.Sort;
end;

function TJvCustomAppIniMemoryStorage.LocateSection(const Section: string; var Index: Integer): Boolean;
begin
  Result := FSections.Find(Section, Index);
  if not Result then
    Index := FStrings.Count
  else
    Index := Integer(FSections.Objects[Index]);
end;

function TJvCustomAppIniMemoryStorage.LocateValue(const Section, Key: string; var Index: Integer): Boolean;
begin
  Result := LocateSection(Section, Index) and LocateValueInSection(Key, Index);
end;

function TJvCustomAppIniMemoryStorage.LocateValueInSection(const Key: string; var Index: Integer): Boolean;
begin
  while (Index < FStrings.Count) and not AnsiSameTextShortest(FStrings[Index], Key + cKeyValueSeparator) do
    Inc(Index);
  Result := Index < FStrings.Count;
end;

function TJvCustomAppIniMemoryStorage.ValueExists(const Section, Key: string): Boolean;
var
  Idx: Integer;
begin
  Result := LocateValue(Section, Key, Idx);
end;

function TJvCustomAppIniMemoryStorage.ReadValue(const Section, Key: string): string;
var
  Idx: Integer;
begin
  if LocateValue(Section, Key, Idx) then
    Result := Copy(FStrings[Idx], Length(Key) + 1, Length(FStrings[Idx]) - Length(Key) - 1)
  else
    Result := '';
end;

procedure TJvCustomAppIniMemoryStorage.WriteValue(const Section, Key, Value: string);
var
  SectIdx: Integer;
  KeyIdx: Integer;
begin
  FIsInternalChange := True;
  try
    if not LocateSection(Section, SectIdx) then
    begin
      SectIdx := FStrings.Add(cSectionHeaderStart + Section + cSectionHeaderEnd);
      FSections.AddObject(Section, TObject(SectIdx));
      FSections.Sort;
    end;
    KeyIdx := SectIdx + 1;
    if not LocateValueInSection(Key, KeyIdx) then
    begin
      FStrings.Insert(KeyIdx, Key + cKeyValueSeparator + Value);
      for SectIdx := 0 to FSections.Count - 1 do
        if Integer(FSections.Objects[SectIdx]) >= KeyIdx then
          FSections.Objects[SectIdx] := TObject(Integer(FSections.Objects[SectIdx]) + 1);
    end
    else
      FStrings[KeyIdx] := Key + cKeyValueSeparator + Value;
  finally
    FIsInternalChange := False;
  end;
end;

procedure TJvCustomAppIniMemoryStorage.RemoveValue(const Section, Key: string);
var
  Idx: Integer;
  DelCount: Integer;
  SectIdx: Integer;
begin
  FIsInternalChange := True;
  try
    DelCount := 0;
    if LocateValue(Section, Key, Idx) then
    begin
      FStrings.Delete(Idx);
      DelCount := 1;
    end
    else
    if LocateSection(Section + '\' + Key, Idx) then
    begin
      if FSections.Find(Section + '\' + Key, SectIdx) then
        FSections.Delete(SectIdx);
      repeat
        FStrings.Delete(Idx);
        Inc(DelCount);
      until (Idx > FStrings.Count) or (Copy(FStrings[Idx], 1, 1) = cSectionHeaderStart);
    end;
    if DelCount > 0 then
      for SectIdx := 0 to FSections.Count - 1 do
        if Integer(FSections.Objects[SectIdx]) > Idx then
          FSections.Objects[SectIdx] := TObject(Integer(FSections.Objects[SectIdx]) - DelCount);
  finally
    FIsInternalChange := False;
  end;
end;

end.
