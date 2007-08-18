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

interface

{$IFDEF COMPILER5}

uses
  Windows, SysUtils, Classes, TypInfo, ActiveX, MultiMon, Forms, Controls,
  Graphics, ImgList, WinInet;

// Classes
type
  TInterfacedPersistent = class(TPersistent);

  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);

  TCollection = class(Classes.TCollection)
  // warning: DO NOT ADD FIELDS !!!
  private
    function GetNextID: Integer;
  protected
    procedure Added(var Item: TCollectionItem); virtual; {deprecated;}
    procedure Deleting(Item: TCollectionItem); virtual; {deprecated;}
    property NextID: Integer read GetNextID;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function Owner: TPersistent;
    procedure Delete(Index: Integer);
  end;

  TOwnedCollection = class(Classes.TOwnedCollection)
  // warning: DO NOT ADD FIELDS !!!
  private
    function GetNextID: Integer;
  protected
    procedure Added(var Item: TCollectionItem); virtual; {deprecated;}
    procedure Deleting(Item: TCollectionItem); virtual; {deprecated;}
    property NextID: Integer read GetNextID;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    procedure SetItemName(Item: Classes.TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Owner: TPersistent;
    procedure Delete(Index: Integer);
  end;

const
  MaxBucketItems = $1000;

type
  { The hash tables cannot be used with Key='' }
  PBucketListItem = ^TBucketListItem;
  TBucketListItem = record
    Key: Pointer;
    Value: Pointer;
    Next: PBucketListItem;
  end;

  TBucketProc = procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);

  TCustomBucketList = class(TObject)
  private
    FItems: array[0..MaxBucketItems - 1] of PBucketListItem;
    FCount: Integer;
    function GetData(AItem: Pointer): Pointer;
    procedure SetData(AItem: Pointer; const Value: Pointer);
  protected
    function BucketFor(AItem: Pointer): Integer; {virtual;}
  public
    destructor Destroy; override;
    procedure Clear; virtual;

    function Add(AItem, AData: Pointer): Pointer;
    function Remove(AItem: Pointer): Pointer;

    function ForEach(AProc: TBucketProc; AInfo: Pointer = nil): Boolean;
    procedure Assign(AList: TCustomBucketList);

    function Exists(AItem: Pointer): Boolean;
    function Find(AItem: Pointer; out AData: Pointer): Boolean;
    property Data[AItem: Pointer]: Pointer read GetData write SetData; default;
  end;

  TBucketListSizes = (bl2, bl4, bl8, bl16, bl32, bl64, bl128, bl256);

  TBucketList = class(TCustomBucketList)
  public
    constructor Create(ABuckets: TBucketListSizes = bl16);
  end;


function GetRelocAddress(ProcAddress: Pointer): Pointer;
function InstallProcHook(ProcAddress, HookProc, OrgCallProc: Pointer): Boolean;
function UninstallProcHook(OrgCallProc: Pointer): Boolean;

function AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

// SysUtils

const
  PathDelim = '\';
  DriveDelim = ';';
  sLineBreak = #13#10;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToDateTime(const S: string; out Date: TDateTime): Boolean;
function TryStrToTime(const S: string; out Time: TDateTime): Boolean;
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

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;

function StrNextChar(const Str: PChar): PChar;

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
type
  TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);

function FindVarData(const V: Variant): PVarData;
function VarIsStr(const V: Variant): Boolean;
function VarIsType(const V: Variant; AVarType: TVarType): Boolean;
function VarCompareValue(const A, B: Variant): TVariantRelationship;

// Misc
function GetMonitorWorkareaRect(Monitor: TMonitor): TRect;

type
  UTF8String = type string;

// System
type
  TVarType = Word;
  PPointer = ^Pointer;

// Controls
type
  TTime = type TDateTime;
  {$EXTERNALSYM TTime}
  TDate = type TDateTime;
  {$EXTERNALSYM TDate}

// Controls
// obones 2005/10/30: Commented out as it clashes in C++ Builder 5 at least.
// Symptoms are a message saying "Cannot load JvStdCtrlsC5D, a class named
// 'TCustomImageList' is already registered.".
// As it seems no one is using the new Draw method, there is no harm done.
{type
  TCustomImageList = class(ImgList.TCustomImageList)
  // warning: DO NOT ADD FIELDS !!!
  public
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      Enabled: Boolean); overload;
  end;}

// Grid
type
  TEditStyle = (esSimple, esEllipsis, esPickList);

// DateUtils
function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;

// Graphics
const
  clCream = TColor($F0FBFF);

// WinInet
function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;

{$ENDIF COMPILER5}

{$IFNDEF COMPILER7_UP}
  // For Delphi 5 / BCB 5 and those who will not install Delphi 6 Update 2

// Windows
const
  {$EXTERNALSYM SPI_GETMENUSHOWDELAY}
  SPI_GETMENUSHOWDELAY = 106;
  {$EXTERNALSYM SPI_SETMENUSHOWDELAY}
  SPI_SETMENUSHOWDELAY = 107;
  {$EXTERNALSYM SPI_GETMENUFADE}
  SPI_GETMENUFADE = $1012;
  {$EXTERNALSYM SPI_SETMENUFADE}
  SPI_SETMENUFADE = $1013;
  {$EXTERNALSYM SPI_GETSELECTIONFADE}
  SPI_GETSELECTIONFADE = $1014;
  {$EXTERNALSYM SPI_SETSELECTIONFADE}
  SPI_SETSELECTIONFADE = $1015;
  {$EXTERNALSYM SPI_GETTOOLTIPANIMATION}
  SPI_GETTOOLTIPANIMATION = $1016;
  {$EXTERNALSYM SPI_SETTOOLTIPANIMATION}
  SPI_SETTOOLTIPANIMATION = $1017;
  {$EXTERNALSYM SPI_GETTOOLTIPFADE}
  SPI_GETTOOLTIPFADE = $1018;
  {$EXTERNALSYM SPI_SETTOOLTIPFADE}
  SPI_SETTOOLTIPFADE = $1019;
  {$EXTERNALSYM SPI_GETCURSORSHADOW}
  SPI_GETCURSORSHADOW = $101A;
  {$EXTERNALSYM SPI_SETCURSORSHADOW}
  SPI_SETCURSORSHADOW = $101B;
  {$EXTERNALSYM SPI_GETUIEFFECTS}
  SPI_GETUIEFFECTS = $103E;
  {$EXTERNALSYM SPI_SETUIEFFECTS}
  SPI_SETUIEFFECTS = $103F;
  {$EXTERNALSYM COLOR_MENUHILIGHT}
  COLOR_MENUHILIGHT = 29;
  {$EXTERNALSYM COLOR_MENUBAR}
  COLOR_MENUBAR = 30;
  {$EXTERNALSYM SPI_GETKEYBOARDCUES}
  SPI_GETKEYBOARDCUES = $100A;
  {$EXTERNALSYM SPI_SETKEYBOARDCUES}
  SPI_SETKEYBOARDCUES = $100B;
{$ENDIF !COMPILER7_UP}

implementation

{$IFDEF COMPILER5}

uses
  CommCtrl,
  JclSysUtils;

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
      else
      if Col is TOwnedCollection then
        TOwnedCollection(Col).Notify(Self, cnExtracting);
    end;
    OrgTCollectionItem_SetCollection(Self, Value);
  end;
end;

procedure TCollection_Delete(Self: Classes.TCollection; Index: Integer);
begin
  if Self is TOwnedCollection then
    TOwnedCollection(Self).Notify(Self.Items[Index], cnDeleting)
  else
  if Self is TCollection then
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

//=== { TCollection } ========================================================

constructor TCollection.Create(ItemClass: Classes.TCollectionItemClass);
begin
  inherited Create(ItemClass);
  if not GlobalCollectionHooked then
    HookCollection;
end;

procedure TCollection.Added(var Item: Classes.TCollectionItem);
begin
end;

procedure TCollection.Delete(Index: Integer);
begin
  Notify(TCollectionItem(Items[Index]), cnDeleting);
  inherited Delete(Index);
end;

procedure TCollection.Deleting(Item: Classes.TCollectionItem);
begin
end;

function TCollection.GetNextID: Integer;
begin
  Result := TPrivateCollection(Self).FNextID;
end;

procedure TCollection.Notify(Item: Classes.TCollectionItem; Action: TCollectionNotification);
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

//=== { TOwnedCollection } ===================================================

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

procedure TOwnedCollection.Added(var Item: Classes.TCollectionItem);
begin
end;

procedure TOwnedCollection.Deleting(Item: Classes.TCollectionItem);
begin
end;

function TOwnedCollection.GetNextID: Integer;
begin
  Result := TPrivateCollection(Self).FNextID;
end;

procedure TOwnedCollection.Notify(Item: Classes.TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Added(Item);
    cnDeleting:
      Deleting(Item);
  end;
end;

procedure TCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  Notify(Item, cnAdded);
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


{ TCustomBucketList }

destructor TCustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCustomBucketList.Add(AItem, AData: Pointer): Pointer;
var
  N: PBucketListItem;
  Hash: Integer;
begin
  New(N);
  Hash := BucketFor(AItem);
  N.Next := FItems[Hash];
  FItems[Hash] := N;
  Inc(FCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

procedure AssignProc(AInfo: Pointer; AItem, AData: Pointer; out AContinue: Boolean);
begin
  AContinue := True;
  TCustomBucketList(AInfo).Add(AItem, AData);
end;

procedure TCustomBucketList.Assign(AList: TCustomBucketList);
begin
  Clear;
  ForEach(AssignProc, Self);
end;

procedure TCustomBucketList.Clear;
var
  P, N: PBucketListItem;
  i: Integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        Dispose(P);
        P := N;
        Dec(FCount);
      end;
      FItems[i] := nil;
      if FCount = 0 then
        Break;
    end;
  end;
  FCount := 0;
end;

function TCustomBucketList.Exists(AItem: Pointer): Boolean;
var
  Data: Pointer;
begin
  Result := Find(AItem, Data);
end;

function TCustomBucketList.Find(AItem: Pointer; out AData: Pointer): Boolean;
var
  N: PBucketListItem;
begin
  AData := nil;
  N := FItems[BucketFor(AItem)];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      AData := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

function TCustomBucketList.ForEach(AProc: TBucketProc; AInfo: Pointer): Boolean;
var
  P: PBucketListItem;
  i: Integer;
begin
  Result := False;
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        AProc(AInfo, P.Key, P.Value, Result);
        if not Result then
          Exit;
        P := P.Next;
      end;
    end;
  end;
end;

function TCustomBucketList.GetData(AItem: Pointer): Pointer;
begin
  if not Find(AItem, Result) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    RaiseLastWin32Error;
  end;
end;

function TCustomBucketList.Remove(AItem: Pointer): Pointer;
var
  Index: Integer;
  P, N: PBucketListItem;
begin
  Index := BucketFor(AItem);
  N := FItems[Index];
  if N <> nil then
  begin
    if N.Key = AItem then
    begin
      Result := N.Value;
      P := N.Next;
      Dispose(N);
      FItems[Index] := P;
      Dec(FCount);
      Exit;
    end
    else
    begin
      P := N;
      N := N.Next;
      while N <> nil do
      begin
        if N.Key = AItem then
        begin
          Result := N.Value;
          P.Next := N.Next;
          Dispose(N);
          Dec(FCount);
          Exit;
        end;
        P := N;
        N := N.Next;
      end;
    end;
  end;
  Result := nil;
end;

procedure TCustomBucketList.SetData(AItem: Pointer; const Value: Pointer);
var
  N: PBucketListItem;
begin
  N := FItems[BucketFor(AItem)];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      N.Value := Value;
      Exit;
    end;
    N := N.Next;
  end;
  SetLastError(ERROR_INVALID_PARAMETER);
  RaiseLastWin32Error;
end;

function TCustomBucketList.BucketFor(AItem: Pointer): Integer;
begin
  Result := Integer(AItem) mod MaxBucketItems;
end;

{ TBucketList }

constructor TBucketList.Create(ABuckets: TBucketListSizes);
begin
  inherited Create;
end;

{------------------------------------------------------------------------------}

function ReadProtectedMemory(Address: Pointer; var Buffer; Count: Cardinal): Boolean;
var
  N: Cardinal;
begin
  Result := ReadProcessMemory(GetCurrentProcess, Address, @Buffer, Count, N);
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

function AllocateHWnd(Method: TWndMethod): HWND;
begin
  Result := Forms.AllocateHWnd(Method);
end;

procedure DeallocateHWnd(Wnd: HWND);
begin
  Forms.DeallocateHWnd(Wnd);
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
  I, LastByteCount: Integer;
  ModRM: TModRM;
begin
  Result := 0;
  LastByteCount := 0;
  I := 0;
  while I < Length(Bytes) do
  begin
    LastByteCount := Result;
    case Bytes[I] of
      $53..$56:
        ; // push reg
      $8B, $3B: // mov/cmp
        begin
          Inc(I);
          ModRM := GetModRM(Bytes[I]);
          case ModRM.Mode of
            $00:
              if ModRM.RM = $07 then
                Inc(I, 2); // mov reg, disp16
            $01:
              Inc(I); // mov reg, [reg]+disp8
            $02:
              Inc(I, 2); // mov reg, [reg]+disp16
          end;
        end;
      $E8:
        Inc(I, 4); // call rel32
      $5B..$5E:
        ; // pop reg
      $C3:
        ; // ret
      $E9:
        Inc(I, 4); // jmp rel32
      $83: // add
        Inc(I, 2);
      $89:
        Inc(I, 2);
    end;
    Inc(I);
    Result := I;
  end;
  if I > Length(Bytes) then
    Result := LastByteCount;
end;

function InstallProcHook(ProcAddress, HookProc, OrgCallProc: Pointer): Boolean;
var
  Code: TJumpCode;
  OrgCallCode: TOrgCallCode;
  I, Count: Integer;
  WrittenBytes: Cardinal;
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
        for I := Count to SizeOf(OrgCallCode.Code) do
          OrgCallCode.Code[I] := $90; // NOP
        OrgCallCode.Jmp := $E9;
        OrgCallCode.Offset := (Integer(ProcAddress) {+ SizeOf(Code)}+ Count) -
          Integer(OrgCallProc) -
          (SizeOf(OrgCallCode) - SizeOf(OrgCallCode.Address));
        OrgCallCode.Address := ProcAddress;

        WriteProtectedMemory(OrgCallProc, @OrgCallCode, SizeOf(OrgCallCode), WrittenBytes);
      end;
    end;

    Code.Jmp := $E9;
    Code.Offset := Integer(HookProc) - (Integer(ProcAddress)) - SizeOf(Code);

    { The strange thing is that something overwrites the $e9 with a "PUSH xxx" }
    Result := WriteProtectedMemory(ProcAddress, @Code, SizeOf(Code), WrittenBytes);
  end;
end;

function UninstallProcHook(OrgCallProc: Pointer): Boolean;
var
  OrgCallCode: TOrgCallCode;
  ProcAddress: Pointer;
  WrittenBytes: Cardinal;
begin
  Result := False;
  if Assigned(OrgCallProc) then
    if OrgCallProc <> nil then
      if ReadProtectedMemory(OrgCallProc, OrgCallCode, SizeOf(OrgCallCode)) then
      begin
        ProcAddress := OrgCallCode.Address;

        Result := WriteProtectedMemory(ProcAddress, @OrgCallCode, SizeOf(TJumpCode), WrittenBytes);
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

function TryStrToTime(const S: string; out Time: TDateTime): Boolean;
begin
  Result := True;
  try
    Time := StrToTime(S);
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

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  BoolStrs: array[Boolean, Boolean] of String = (('0', '-1'), ('False', 'True')); // do not localize
begin
  Result := BoolStrs[UseBoolStrs, B];
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

function StrNextChar(const Str: PChar): PChar;
begin
  Result := CharNext(Str);
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

function GetMonitorWorkareaRect(Monitor: TMonitor): TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  GetMonitorInfo(Monitor.Handle, @MonInfo);
  Result := MonInfo.rcWork;
end;

function VarCompareValue(const A, B: Variant): TVariantRelationship;
const
  CTruth: array [Boolean] of TVariantRelationship = (vrNotEqual, vrEqual);
var
  LA, LB: TVarData;
begin
  LA := FindVarData(A)^;
  LB := FindVarData(B)^;
  if LA.VType = varEmpty then
    Result := CTruth[LB.VType = varEmpty]
  else
  if LA.VType = varNull then
    Result := CTruth[LB.VType = varNull]
  else
  if LB.VType in [varEmpty, varNull] then
    Result := vrNotEqual
  else
  if A = B then
    Result := vrEqual
  else
  if A < B then
    Result := vrLessThan
  else
    Result := vrGreaterThan;
end;


//=== { TCustomImageList } ===================================================

{procedure TCustomImageList.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean);
const
  DrawingStyles: array[TDrawingStyle] of Longint =
  (ILD_FOCUS, ILD_SELECTED, ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint =
  (0, ILD_MASK);
begin
  if HandleAllocated then
    DoDraw(Index, Canvas, X, Y, DrawingStyles[ADrawingStyle] or
      Images[AImageType], Enabled);
end;}

function IncYear(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
begin
  Result := IncMonth(AValue, ANumberOfYears * 12);
end;

function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
  external 'wininet.dll' name 'FtpGetFileSize';

initialization

finalization
  if GlobalCollectionHooked then
    UnhookCollection;

{$ENDIF COMPILER5}

end.
