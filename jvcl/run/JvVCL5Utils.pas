{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTD5Compat.pas, released on 2005-05-23.

The Initial Developer of the Original Code is Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Portions created by Andreas Hausladen are Copyright (C) 2005 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvVCL5Utils;

{$I jvcl.inc}

{$IFDEF COMPILER5}

interface

uses
  Windows, SysUtils, Classes, TypInfo, ActiveX;

// Classes
type
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);

  TCollection = class(Classes.TCollection)
  private
    function GetNextID: Integer;
  protected
    procedure Added(var Item: TCollectionItem); virtual; {deprecated;}
    procedure Deleting(Item: TCollectionItem); virtual; {deprecated;}
    property NextID: Integer read GetNextID;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    procedure SetItemName(Item: Classes.TCollectionItem); override;
  public
    constructor Create(ItemClass: TCollectionItemClass);

    function Owner: TPersistent;
    procedure Delete(Index: Integer);
  end;

  TOwnedCollection = class(Classes.TOwnedCollection)
  private
    function GetNextID: Integer;
  protected
    procedure Added(var Item: TCollectionItem); virtual; {deprecated;}
    procedure Deleting(Item: TCollectionItem); virtual; {deprecated;}
    property NextID: Integer read GetNextID;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    procedure SetItemName(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Owner: TPersistent;
    procedure Delete(Index: Integer);
  end;


function GetRelocAddress(ProcAddress: Pointer): Pointer;
function InstallProcHook(ProcAddress, HookProc, OrgCallProc: Pointer): Boolean;
function UninstallProcHook(OrgCallProc: Pointer): Boolean;

// SysUtils

const
  PathDelim = '\';
  DriveDelim = ';';
  sLineBreak = #13#10;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToDateTime(const S: string; out Date: TDateTime): Boolean;
function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
// function StrToFloatDef(const Str: string; Default: Extended): Extended;
procedure RaiseLastOSError;
function IncludeTrailingPathDelimiter(const APath: string): string;
function ExcludeTrailingPathDelimiter(const APath: string): string;
function DirectoryExists(const Name: string): Boolean;
function ForceDirectories(Dir: string): Boolean;
function SameFileName(const FN1, FN2: string): Boolean;
function GetEnvironmentVariable(const Name: string): string;

function Supports(Instance: TObject; const Intf: TGUID): Boolean; overload;
function Supports(AClass: TClass; const Intf: TGUID): Boolean; overload;
function FileIsReadOnly(const FileName: string): Boolean;


function WideCompareText(const S1, S2: WideString): Integer;
function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;
function CompareDateTime(const A, B: TDateTime): Integer;

// StrUtils
function AnsiStartsText(const SubText, Text: string): Boolean;
function AnsiEndsText(const SubText, Text: string): Boolean;
function AnsiStartsStr(const SubStr, Str: string): Boolean;
function AnsiEndsStr(const SubStr, Str: string): Boolean;

// Math
type
  TValueSign = -1..1;

const
  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);

function Sign(const AValue: Integer): TValueSign; overload;
function Sign(const AValue: Int64): TValueSign; overload;
function Sign(const AValue: Double): TValueSign; overload;

// Variants
function FindVarData(const V: Variant): PVarData;
function VarIsStr(const V: Variant): Boolean;
function VarIsType(const V: Variant; AVarType: TVarType): Boolean;

{$ENDIF COMPILER5}

implementation

{$IFDEF COMPILER5}

var
  GlobalCollectionHooked: Boolean = False;

type
  TPrivateCollection = class(TPersistent)
  public
    FItemClass: TCollectionItemClass;
    FItems: TList;
    FUpdateCount: Integer;
    FNextID: Integer; // <-- we are interested in this field
  end;

  TPrivateCollectionItem = class(TPersistent)
  public
    FCollection: TCollection;
  end;

  TPublishedCollectionItem = class(TCollectionItem)
  published
    property Collection;
  end;

procedure OrgTCollection_Delete(Self: Classes.TCollection; Index: Integer);
asm
        DD    0, 0, 0, 0  // 16 Bytes
end;

procedure OrgTCollectionItem_SetCollection(Self: TCollectionItem; Value: Classes.TCollection);
asm
        DD    0, 0, 0, 0  // 16 Bytes
end;

procedure TCollectionItem_SetCollection(Self: TCollectionItem; Value: Classes.TCollection);
var
  Col: Classes.TCollection;
begin
  Col := TPrivateCollectionItem(Self).FCollection;
  if Col <> Value then
  begin
    if Col <> nil then
    begin
      if Col is TCollection then
        TCollection(Col).Notify(Self, cnExtracting)
      else if Col is TOwnedCollection then
        TOwnedCollection(Col).Notify(Self, cnExtracting);
    end;
    OrgTCollectionItem_SetCollection(Self, Value);
  end;
end;

procedure TCollection_Delete(Self: Classes.TCollection; Index: Integer);
begin
  if Self is TOwnedCollection then
    TOwnedCollection(Self).Notify(Self.Items[Index], cnDeleting)
  else if Self is TCollection then
    TCollection(Self).Notify(Self.Items[Index], cnDeleting);
  TCollectionItem(Self.Items[Index]).Free;
end;

procedure HookCollection;
var
  Info: PPropInfo;
begin
  if not GlobalCollectionHooked then
  begin
    GlobalCollectionHooked := True;
    InstallProcHook(@Classes.TCollection.Delete, @TCollection_Delete, @OrgTCollection_Delete);

    Info := GetPropInfo(TPublishedCollectionItem, 'Collection');
    InstallProcHook(Info.SetProc, @TCollectionItem_SetCollection, @OrgTCollectionItem_SetCollection);
  end;
end;

procedure UnhookCollection;
begin
  if GlobalCollectionHooked then
  begin
    GlobalCollectionHooked := False;
    UninstallProcHook(@OrgTCollection_Delete);
    UninstallProcHook(@OrgTCollectionItem_SetCollection);
  end;
end;

{ TCollection }

constructor TCollection.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  if not GlobalCollectionHooked then
    HookCollection;
end;

procedure TCollection.Added(var Item: TCollectionItem);
begin
end;

procedure TCollection.Delete(Index: Integer);
begin
  Notify(TCollectionItem(Items[Index]), cnDeleting);
  inherited Delete(Index);
end;

procedure TCollection.Deleting(Item: TCollectionItem);
begin
end;

function TCollection.GetNextID: Integer;
begin
  Result := TPrivateCollection(Self).FNextID;
end;

procedure TCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Added(Item);
    cnDeleting:
      Deleting(Item);
  end;
end;

function TCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;

procedure TCollection.SetItemName(Item: Classes.TCollectionItem);
begin
  inherited SetItemName(Item);
  Notify(TCollectionItem(Item), cnAdded);
end;

{ TOwnedCollection }

constructor TOwnedCollection.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if not GlobalCollectionHooked then
    HookCollection;
end;

procedure TOwnedCollection.Delete(Index: Integer);
begin
  Notify(TCollectionItem(Items[Index]), cnDeleting);
  inherited Delete(Index);
end;

procedure TOwnedCollection.Added(var Item: TCollectionItem);
begin
end;

procedure TOwnedCollection.Deleting(Item: TCollectionItem);
begin
end;

function TOwnedCollection.GetNextID: Integer;
begin
  Result := TPrivateCollection(Self).FNextID;
end;

procedure TOwnedCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Added(Item);
    cnDeleting:
      Deleting(Item);
  end;
end;

function TOwnedCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;

procedure TOwnedCollection.SetItemName(Item: Classes.TCollectionItem);
begin
  inherited SetItemName(Item);
  Notify(TCollectionItem(Item), cnAdded);
end;


{ This code installs a JUMP-Hook into a function. }
type
  PPointer = ^Pointer;

function ReadProtectedMemory(Address: Pointer; var Buffer; Count: Cardinal): Boolean;
var
  N: Cardinal;
begin
  Result := ReadProcessMemory(GetCurrentProcess, Address, @Buffer, Count, N);
  Result := Result and (N = Count);
end;

function WriteProtectedMemory(Address: Pointer; const Buffer; Count: Cardinal): Boolean;
var
  N: Cardinal;
begin
  Result := WriteProcessMemory(GetCurrentProcess, Address, @Buffer, Count, N);
  Result := Result and (N = Count);
end;

type
  TJumpCode = packed record
    Jmp: Byte; // jmp Offset
    Offset: Integer;
  end;

  TOrgCallCode = packed record
    Code: array[0..SizeOf(TJumpCode) + 4] of Byte;
    Jmp: Byte; // jmp Offset
    Offset: Integer;
    Address: Pointer;
  end;

function GetRelocAddress(ProcAddress: Pointer): Pointer;
type
  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;
var
  Relocation: TRelocationRec;
  Data: Byte;
begin
  Result := ProcAddress;
  // the relocation table might be protected
  if ReadProtectedMemory(ProcAddress, Data, SizeOf(Data)) then
    if Data = $FF then // ProcAddress is in a DLL or package
      if ReadProtectedMemory(ProcAddress, Relocation, SizeOf(Relocation)) then
        Result := Relocation.Address^;
end;

type
  TModRM = record
    Mode: Byte;
    RegOp: Byte;
    RM: Byte;
  end;

function GetModRM(B: Byte): TModRM;
begin
  Result.Mode := B shr 6;
  Result.RegOp := (B shr 3) and $07;
  Result.RM := B and $07;
end;

function GetDisassembledByteCount(const Bytes: array of Byte): Integer;
var
  i, LastByteCount: Integer;
  ModRM: TModRM;
begin
  Result := 0;
  LastByteCount := 0;
  i := 0;
  while i < Length(Bytes) do
  begin
    LastByteCount := Result;
    case Bytes[i] of
      $53..$56: ; // push reg
      $8B, $3B: // mov/cmp
        begin
          Inc(i);
          ModRM := GetModRM(Bytes[i]);
          case ModRM.Mode of
            $00: if ModRM.RM = $07 then
                   Inc(i, 2); // mov reg, disp16
            $01: Inc(i); // mov reg, [reg]+disp8
            $02: Inc(i, 2); // mov reg, [reg]+disp16
          end;
        end;
      $E8: Inc(i, 4); // call rel32
      $5B..$5E: ; // pop reg
      $C3: ; // ret
      $E9: Inc(i, 4); // jmp rel32

      $83: // add
        begin
          Inc(i, 2);
        end;
      $89:
        begin
          Inc(i, 2);
        end;
    end;                                         
    Inc(i);
    Result := i;
  end;
  if i > Length(Bytes) then
    Result := LastByteCount;
end;

function InstallProcHook(ProcAddress, HookProc, OrgCallProc: Pointer): Boolean;
var
  Code: TJumpCode;
  OrgCallCode: TOrgCallCode;
  i, Count: Integer;
begin
  ProcAddress := GetRelocAddress(ProcAddress);
  Result := False;
  if Assigned(ProcAddress) and Assigned(HookProc) then
  begin
    if OrgCallProc <> nil then
    begin
      if ReadProtectedMemory(ProcAddress, OrgCallCode, SizeOf(OrgCallCode.Code)) then
      begin
        Count := GetDisassembledByteCount(OrgCallCode.Code);
        for i := Count to SizeOf(OrgCallCode.Code) do
          OrgCallCode.Code[i] := $90; // NOP
        OrgCallCode.Jmp := $E9;
        OrgCallCode.Offset := (Integer(ProcAddress) {+ SizeOf(Code)}+ Count) -
          Integer(OrgCallProc) -
          (SizeOf(OrgCallCode) - SizeOf(OrgCallCode.Address));
        OrgCallCode.Address := ProcAddress;

        WriteProtectedMemory(OrgCallProc, OrgCallCode, SizeOf(OrgCallCode));
        FlushInstructionCache(GetCurrentProcess, OrgCallProc, SizeOf(OrgCallCode));
      end;
    end;

    Code.Jmp := $E9;
    Code.Offset := Integer(HookProc) - (Integer(ProcAddress)) - SizeOf(Code);

    { The strange thing is that something overwrites the $e9 with a "PUSH xxx" }
    if WriteProtectedMemory(Pointer(Cardinal(ProcAddress)), Code, SizeOf(Code)) then
    begin
      FlushInstructionCache(GetCurrentProcess, ProcAddress, SizeOf(Code));
      Result := True;
    end;
  end;
end;

function UninstallProcHook(OrgCallProc: Pointer): Boolean;
var
  OrgCallCode: TOrgCallCode;
  ProcAddress: Pointer;
begin
  Result := False;
  if Assigned(OrgCallProc) then
    if OrgCallProc <> nil then
      if ReadProtectedMemory(OrgCallProc, OrgCallCode, SizeOf(OrgCallCode)) then
      begin
        ProcAddress := OrgCallCode.Address;

        Result := WriteProtectedMemory(ProcAddress, OrgCallCode, SizeOf(TJumpCode));
        FlushInstructionCache(GetCurrentProcess, ProcAddress, SizeOf(OrgCallCode));
      end;
end;


// SysUtils
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function TryStrToDateTime(const S: string; out Date: TDateTime): Boolean;
begin
  Result := True;
  try
    Date := StrToDateTime(S);
  except
    Result := False;
  end;
end;

{ TODO -oJVCL -cTODO : Implement these better for D5! }

function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
begin
  // stupid and slow but at least simple
  try
    Result := StrToDateTime(S);
  except
    Result := Default;
  end;
end;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
begin
  // stupid and slow but at least simple
  try
    Result := StrToDate(S);
  except
    Result := Default;
  end;
end;

const
  OneMillisecond = 1 / 24 / 60 / 60 / 1000; // as TDateTime

function CompareDateTime(const A, B: TDateTime): Integer;
begin
  if Abs(A - B) < OneMillisecond then
    Result := 0
  else
  if A < B then
    Result := -1
  else
    Result := 1;
end;

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function IncludeTrailingPathDelimiter(const APath: string): string;
begin
  if (APath <> '') and (APath[Length(APath)] <> PathDelim) then
    Result := APath + PathDelim
  else
    Result := APath;
end;

function ExcludeTrailingPathDelimiter(const APath: string): string;
var
  I: Integer;
begin
  Result := APath;
  I := Length(Result);
  while (I > 0) and (Result[I] = PathDelim) do
    Dec(I);
  SetLength(Result, I);
end;

function DirectoryExists(const Name: string): Boolean;
var
  Code: Cardinal;
begin
  Code := Integer(GetFileAttributes(PChar(Name)));
  Result := (Code <> $FFFFFFFF) and (Code and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Dir[Length(Dir)] = PathDelim then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir) or (ExtractFilePath(Dir) = Dir) then
    Exit; { avoid 'xyz:\' problem }
  Result := ForceDirectories(ExtractFilePath(Dir));
  if Result then
    Result := CreateDir(Dir);
end;

function SameFileName(const FN1, FN2: string): Boolean;
begin
  Result := CompareText(FN1, FN2) = 0;
end;

function GetEnvironmentVariable(const Name: string): string;
var
  Len: Integer;
begin
  SetLength(Result, 4 * 1024);
  Len := Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result));
  if Len <= Length(Result) then
    SetLength(Result, Len)
  else
  begin
    SetLength(Result, Len - 1);
    Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Len);
  end;
end;

function Supports(Instance: TObject; const Intf: TGUID): Boolean;
begin
  Result := (Instance <> nil) and (Instance.GetInterfaceEntry(Intf) <> nil);
end;

function Supports(AClass: TClass; const Intf: TGUID): Boolean;
begin
  Result := (AClass <> nil) and (AClass.GetInterfaceEntry(Intf) <> nil);
end;

function FileIsReadOnly(const FileName: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(FileName));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_READONLY <> 0);
end;

function WideCompareText(const S1, S2: WideString): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Result := CompareText(string(S1), string(S2))
  else
    Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) - 2;
end;

function WideUpperCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    CharUpperBuffW(Pointer(Result), Length(Result));
end;

function WideLowerCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    CharLowerBuffW(Pointer(Result), Length(Result));
end;


// StrUtils
function AnsiStartsText(const SubText, Text: string): Boolean;
var
  SubTextLen: Integer;
begin
  SubTextLen := Length(SubText);
  if SubTextLen > Length(Text) then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PChar(Text), SubTextLen, PChar(SubText), SubTextLen) = 2;
end;

function AnsiEndsText(const SubText, Text: string): Boolean;
var
  SubTextStart: Integer;
begin
  SubTextStart := Length(Text) - Length(SubText) + 1;
  if (SubTextStart > 0) and (SubText <> '') and (ByteType(Text, SubTextStart) <> mbTrailByte) then
    Result := AnsiStrIComp(Pointer(SubText), PChar(Pointer(Text)) + SubTextStart - 1) = 0
  else
    Result := False;
end;

function AnsiStartsStr(const SubStr, Str: string): Boolean;
var
  SubStrLen: Integer;
begin
  SubStrLen := Length(SubStr);
  if SubStrLen > Length(Str) then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, 0,
      PChar(Str), SubStrLen, PChar(SubStr), SubStrLen) = 2;
end;

function AnsiEndsStr(const SubStr, Str: string): Boolean;
var
  SubStrStart: Integer;
begin
  SubStrStart := Length(Str) - Length(SubStr) + 1;
  if (SubStrStart > 0) and (SubStr <> '') and (ByteType(Str, SubStrStart) <> mbTrailByte) then
    Result := AnsiStrComp(Pointer(SubStr), PChar(Pointer(Str)) + SubStrStart - 1) = 0
  else
    Result := False;
end;


// Math
function Sign(const AValue: Integer): TValueSign;
begin
  if AValue < 0 then
    Result := NegativeValue
  else
  if AValue > 0 then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function Sign(const AValue: Int64): TValueSign;
begin
  if AValue < 0 then
    Result := NegativeValue
  else
  if AValue > 0 then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function Sign(const AValue: Double): TValueSign;
begin
  if (PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000 then
    Result := ZeroValue
  else
  if (PInt64(@AValue)^ and $8000000000000000) = $8000000000000000 then
    Result := NegativeValue
  else
    Result := PositiveValue;
end;

// Variants
function VarIsStr(const V: Variant): Boolean;
var
  VarType: TVarType;
  VarData: PVarData;
begin
  VarData := @TVarData(V);
  while VarData.VType = varByRef or varVariant do
    VarData := PVarData(VarData.VPointer);

  VarType := VarData^.VType;
  Result := (VarType = varOleStr) or (VarType = varString);
end;

function FindVarData(const V: Variant): PVarData;
begin
  Result := @TVarData(V);
  while Result.VType = varByRef or varVariant do
    Result := PVarData(Result.VPointer);
end;

function VarIsType(const V: Variant; AVarType: TVarType): Boolean;
begin
  Result := FindVarData(V)^.VType = AVarType;
end;

initialization

finalization
  if GlobalCollectionHooked then
    UnhookCollection;

{$ENDIF COMPILER5}

end.
