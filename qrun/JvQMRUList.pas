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

The Original Code is: JvMru.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Arioch [the_Arioch att nm dott ru]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvQMRUList;

interface

{*******************************************************}
{   This unit is an interface to the MRU List (comctl32)}
{   Informations from :                                 }
{      http://www.geocities.com/SiliconValley/4942      }
{*******************************************************}

(*
the_Arioch att nm dott ru

Changes are:
 0) Memory leaks in GetItem and EnumerateItems been fixed in JVCL 1.32
 1) fixed bug 2 Microsoft bugs. Read article at URL above.
 2) added ItemData property that allows to read data w|o using event
 3) EnumerateItems now relies upon GetItem to remove duplication of code.
     Now, if any bug - You may fix it one time, not 2 times :)
 4) one more thing - i cannot get the reason that almost all of the methods
    of the component are published rather than public. I think it is also a bug
 5) added MoveToTop(index) method; Warning! it changes ItemData property
 6) added DelayedWrite property
 7) renamed DeleteString to DeleteItem - cause it is the same for both String and Data
 8) added UseUnicode property - if List is of string type then it will use WideString methods
 9) added WantUnicode property - it will set UseUnicode respecting to used platform
10) some storage modifiers added for published property
xx) why keep UnicodeAvailable in every component? I wish Delphi could map
    property to a global variable :(
*)

uses
  Windows, SysUtils, Classes,
  JvQComponent, JvQTypes, JvQFinalize;

type
  TJvDataType = (dtString, dtBinary);
  TOnEnumData = procedure(Sender: TObject; Data: Pointer; Size: Integer; Index: Integer) of object;
  TOnEnumText = procedure(Sender: TObject; Value: string; Index: Integer) of object;
  TOnEnumUnicodeText = procedure(Sender: TObject; Value: WideString; Index: Integer) of object;

  TJvMRUReturnData = record
    case Byte of
      0: (P: Pointer; );
      1: (S: PChar; );
      2: (Ws: PWideChar; );
  end;
  PJvMruReturnData = ^TJvMRUReturnData;
  TMRUCount = 0..29;

  TJvMRUList = class(TJvComponent)
  private
    FUnicodeAvailable: Boolean;
    FUseUnicode: Boolean;
    FDelayedWrite: Boolean;
    FWantUnicode: Boolean;
    FMax: TMRUCount;
    FSubKey: WideString;
    FKey: TJvRegKey;
    FList: THandle;
    FType: TJvDataType;
    FOnEnumData: TOnEnumData;
    FOnEnumText: TOnEnumText;
    FOnEnumUnicodeText: TOnEnumUnicodeText;
    FItemIndex: Integer;
    FItemData: TJvMRUReturnData;
    procedure SetKey(const Value: TJvRegKey);
    procedure SetMax(const Value: TMRUCount);
    function GetSubKey: string;
    procedure SetSubKeyUnicode(const Value: WideString);
    procedure SetSubKey(const Value: string);
    procedure SetType(const Value: TJvDataType);
    procedure SetUseUnicode(const Value: Boolean);
    procedure SetWantUnicode(const Value: Boolean);
    procedure SetItemData(const P: Pointer);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetItemDataAsPChar: PChar;
    function GetItemDataAsPWideChar: pWideChar;
  protected
    function InternalGetItem(Index: Integer; FireEvent: Boolean = True): Boolean;
    procedure ReCreateList;
    procedure NeedUnicode;
    procedure DoEnumText; virtual;
    procedure DoUnicodeEnumText; virtual;
    // Arioch: even DataSize can be retained later from properties - but let 'em be.
    procedure DoEnumData(DataSize: Integer); virtual;
  public
    procedure Close;
    procedure Open;
    function ItemDataSize: Integer;
    property ItemDataAsPointer: Pointer read FItemData.P;
    property ItemDataAsPChar: PChar read GetItemDataAsPChar;
    property ItemDataAsPWideChar: pWideChar read GetItemDataAsPWideChar;
    property ItemIndex: Integer read FItemIndex;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MoveToTop(const Index: Integer);

    property UnicodeAvailable: Boolean read FUnicodeAvailable;
    property UseUnicode: Boolean read FUseUnicode write SetUseUnicode;

    // Arioch: the methods below are not public but published in original code
    function AddString(Value: string): Boolean;
    function AddPChar(Value: string): Boolean;
    function AddData(Value: Pointer; Size: Integer): Boolean;
    function GetItemsCount: Integer;
    function EnumItems: Boolean;
    function GetMostRecentItem: Boolean;
    function GetItem(Index: Integer = 0): Boolean;
    function FindString(Value: string): Integer;
    function FindData(Value: Pointer; Size: Integer): Integer;

    function DeleteItem(Index: Integer = 0): Boolean;
    function DeleteKey: Boolean;

    // Arioch: the following are function for Unicode Enabling
    function AddUnicodeString(Value: WideString): Boolean;
    function AddUnicodePChar(Value: PWideChar): Boolean;
    function FindUnicodeString(Value: WideString): Integer;
  published
    property DelayedWrite: Boolean read FDelayedWrite write FDelayedWrite default False;
    property WantUnicode: Boolean read FWantUnicode write SetWantUnicode default False;
    property RootKey: TJvRegKey read FKey write SetKey default hkCurrentUser;
    property SubKey: string read GetSubKey write SetSubKey stored False;
    // Arioch: it will be read from RCDATA for compatiblility, but unicode value should be stored!
    property SubKeyUnicode: WideString read FSubKey write SetSubKeyUnicode stored True;

    property MaxItems: TMRUCount read FMax write SetMax default 10;
    property DataType: TJvDataType read FType write SetType default dtstring;

    property OnEnumText: TOnEnumText read FOnEnumText write FOnEnumText;
    property OnEnumUnicodeText: TOnEnumUnicodeText read FOnEnumUnicodeText write FOnEnumUnicodeText;
    property OnEnumData: TOnEnumData read FOnEnumData write FOnEnumData;
    property Active: Boolean read GetActive write SetActive;
  end;

  EMruException = class(EJVCLException);

implementation

uses
  Registry,
  JvQResources;

const
  sUnitName = 'JvMRUList';  

var
  hComCtlDll: HMODULE = 0;

const
  DllComCtlName = 'COMCTL32.DLL';

type
  MruCompareString = function(lpszString1, lpszString2: PChar): Integer;
  MruCompareData = function(lpData1, lpData2: Pointer; cbData: DWORD): Integer;
  MruCompareStringW = function(lpszString1, lpszString2: PWideChar): Integer;

  PMruRec = ^TMruRec;
  TMruRec = packed record
    cbSize: DWORD;
    nMaxItems: DWORD;
    dwFlags: DWORD;
    hKey: HKEY;
    case Boolean of
      False: (
        lpszSubKey: PChar;
        case Boolean of
          False:
            (lpfnCompareString: MruCompareString; );
          True:
            (lpfnCompareData: MruCompareData; );
          );
      True: (
        lpszSubKeyW: PWideChar;
        lpfnCompareStringW: MruCompareStringW; );
  end;

const
  MRUF_STRING_LIST = 0;
  MRUF_BINARY_LIST = 1;
  MRUF_DELAYED_SAVE = 2;

type
  TCreateMruList = function(lpCreateInfo: PMruRec): THandle; stdcall;
  TFreeMruList = procedure(hList: THandle); stdcall;

  TAddMruString = function(hList: THandle; lpszString: PChar): Integer; stdcall;
  TAddMruStringW = function(hList: THandle; lpszString: PWideChar): Integer; stdcall;
  TAddMruData = function(hList: THandle; lpData: Pointer; cbData: DWORD): Integer; stdcall;

  TDelMruString = function(hList: THandle; nItemPos: Integer): Boolean; stdcall;

  TEnumMruList = function(hList: THandle; nItemPos: Integer; lpBuffer: Pointer; nBufferSize: DWord): Integer; stdcall;

  TFindMruString = function(hList: THandle; lpszString: PChar; lpRegNum: Pinteger): Integer; stdcall;
  TFindMruStringW = function(hList: THandle; lpszString: PWideChar; lpRegNum: Pinteger): Integer; stdcall;
  TFindMruData = function(hList: THandle; lpData: Pointer; cbData: DWORD; lpRegNum: Pinteger): Integer; stdcall;

var
  CreateMruList: TCreateMruList;
  FreeMruList: TFreeMruList;
  AddMruString: TAddMruString;
  AddMruData: TAddMruData;
  DelMruString: TDelMruString;
  EnumMruList: TEnumMruList;
  FindMruString: TFindMruString;
  FindMruData: TFindMruData;

  //Arioch:  Unicode functions for WinNT
  CreateMruListW: TCreateMruList;
  AddMruStringW: TAddMrustringW;
  FindMruStringW: TFindMruStringW;
  EnumMruListW: TEnumMruList;

procedure InitializeDLL; forward;

constructor TJvMRUList.Create(AOwner: TComponent);
begin
  InitializeDLL;

  inherited Create(AOwner);
  FList := 0;
  FMax := 10;
  FType := dtString;
  FKey := hkCurrentUser;
  FUnicodeAvailable := Win32Platform = VER_PLATFORM_WIN32_NT;
  FDelayedWrite := False;
  SetWantUnicode(False);
  FItemData.P := nil;

  // ReCreateList;
  Close; // since there is PUBLISHED .Active property - let it control how it will be.
end;

destructor TJvMRUList.Destroy;
begin
  if FList <> 0 then
    FreeMruList(FList);
  inherited Destroy;
end;

function TJvMRUList.AddData(Value: Pointer; Size: Integer): Boolean;
begin
  Result := False;
  if FList <> 0 then
    Result := AddMruData(FList, Value, Size) <> -1;
end;

function TJvMRUList.AddPChar(Value: string): Boolean;
begin
  Result := False;
  if FList <> 0 then
  begin
    Result := AddMruString(FList, PChar(Value)) <> -1;
    // (p3) call EnumText here ?
    //  Arioch: Why? What for?
    //  Whether You want them - make a special separate set of events
    //  And there's danger that eventHandler tries to get a list of items,
    //  thus, killing current section!
  end;
end;

function TJvMRUList.AddUnicodePChar(Value: PWideChar): Boolean;
begin
  NeedUnicode;
  Result := False;
  if FList <> 0 then
  begin
    Result := AddMruStringW(FList, PWideChar(Value)) <> -1;
    // (p3) call EnumText here?
    // See above
  end;
end;

function TJvMRUList.AddString(Value: string): Boolean;
begin
  Result := AddPchar(PChar(Value));
end;

function TJvMRUList.AddUnicodeString(Value: WideString): Boolean;
begin
  Result := AddUnicodePChar(PWideChar(Value));
end;

function TJvMRUList.DeleteItem(Index: Integer): Boolean;
begin
  Result := False;
  if FList <> 0 then
  begin
    Result := DelMruString(FList, Index);
    ReCreateList; // Arioch: fixes MS's bug
  end;
end;

function TJvMRUList.EnumItems: Boolean;
var
  Index: Integer;
begin
  Result := False;
  if FList = 0 then
    Exit;

  Index := 0;
  while GetItem(Index) do
    Inc(Index);
  if Index > 0 then
    Result := True;
end;

function TJvMRUList.FindData(Value: Pointer; Size: Integer): Integer;
begin
  Result := -1;
  if FList <> 0 then
    Result := FindMruData(FList, Value, Size, nil);
end;

function TJvMRUList.FindString(Value: string): Integer;
begin
  Result := -1;
  if FList <> 0 then
    Result := FindMruString(FList, PChar(Value), nil);
end;

function TJvMRUList.FindUnicodeString(Value: WideString): Integer;
begin
  NeedUnicode;
  Result := -1;
  if FList <> 0 then
    Result := FindMruStringW(FList, PWideChar(Value), nil);
end;

function TJvMRUList.GetItem(Index: Integer): Boolean;
begin
  Result := InternalGetItem(Index);
end;

function TJvMRUList.InternalGetItem(Index: Integer; FireEvent: Boolean): Boolean;
var
  I: Integer;
  P: Pointer;
  EnP: TEnumMruList;
begin
  Result := False;
  if FList = 0 then
    Exit;
  P := nil;

  if FType = dtString then
  begin
    if not UseUnicode then
    begin
      ReAllocMem(P, 256);
      I := EnumMruList(FList, Index, P, 256);
      if I > 255 then
      begin
        ReAllocMem(P, I + 1);
        I := EnumMruList(FList, Index, P, I + 1);
      end;
      if I <> -1 then
      begin
        Result := True;
        SetItemData(P);
        FItemIndex := Index;
        if FireEvent then
          DoEnumText
      end;
    end
    else
    begin // Unicode
      ReAllocMem(P, 512);
      I := EnumMruListW(FList, Index, P, 256);
      if I > 255 then
      begin
        ReAllocMem(P, (I + 1) * 2);
        I := EnumMruListW(FList, Index, P, I + 1);
      end;
      if I <> -1 then
      begin
        Result := True;
        SetItemData(P);
        FItemIndex := Index;
        if FireEvent then
          DoUnicodeEnumText;
      end;
    end
  end
  else // FType = dtBinary
  begin
    ReAllocMem(P, 1024);

    if UnicodeAvailable then
      EnP := EnumMruListW
    else
      EnP := EnumMruList;
    //Arioch: work-around MS bug

    I := EnP(FList, Index, P, 1024);

    if I >= 1024 then
    begin
      ReAllocMem(P, 64000); // Arioch: Hmmm We'll never guess how much may there appear :)
      I := EnP(FList, 0, P, 64000);
    end;

    if I <> -1 then
    begin
      Result := True;
      ReAllocMem(P, I);
      // Arioch: should we waste more memory than we need?
      // and we can know the size of memory allocated
      // with GetMem and ReAllocMem, so we know how big Data was
      SetItemData(P);
      FItemIndex := Index;
      if FireEvent then
        DoEnumData(I);
    end;
  end;
end;

function TJvMRUList.GetItemsCount: Integer;
begin
  if FList <> 0 then
    Result := EnumMruList(FList, -1, nil, 0)
  else
    Result := 0;
end;

function TJvMRUList.GetMostRecentItem: Boolean;
begin
  Result := GetItem(0);
end;

function TJvMRUList.GetSubKey: string;
begin
  Result := string(FSubKey);
end;

procedure TJvMRUList.MoveToTop(const Index: Integer);
var
  B: Boolean;
begin
  B := False;
  if InternalGetItem(Index, False) then
  begin
    if FType = dtString then
    begin
      if UseUnicode then
        B := AddUnicodePChar(ItemDataAsPWideChar)
      else
        B := AddPChar(ItemDataAsPChar);
    end
    else
      B := AddData(ItemDataAsPointer, ItemDataSize);
  end;
  if B then
    FItemIndex := 0;
end;

procedure TJvMRUList.NeedUnicode;
begin
  if not UnicodeAvailable then
    raise EMruException.CreateRes(@RsEErrorMRUUnicode);
end;

procedure TJvMRUList.ReCreateList;
begin
  Close;
  Open;
end;

procedure TJvMRUList.SetItemData(const P: Pointer);
begin
  if P = FItemData.P then
    Exit;
  if FItemData.P <> nil then
    FreeMem(FItemData.P);
  FItemData.P := P;
end;

procedure TJvMRUList.SetKey(const Value: TJvRegKey);
begin
  if Value <> FKey then
  begin
    FKey := Value;
    ReCreateList;
  end;
end;

procedure TJvMRUList.SetMax(const Value: TMRUCount);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    ReCreateList;
  end;
end;

procedure TJvMRUList.SetSubKey(const Value: string);
begin
  SetSubKeyUnicode(WideString(Value));
end;

procedure TJvMRUList.SetSubKeyUnicode(const Value: WideString);
begin
  if Value <> FSubKey then
  begin
    FSubKey := Value;
    ReCreateList;
  end;
end;

procedure TJvMRUList.SetType(const Value: TJvDataType);
begin
  if Value <> FType then
  begin
    FType := Value;
    ReCreateList;
  end;
end;

procedure TJvMRUList.SetUseUnicode(const Value: Boolean);
begin
  if Value then
    NeedUnicode;
  if FUseUnicode = Value then
    Exit;
  FUseUnicode := Value;
end;

procedure TJvMRUList.SetWantUnicode(const Value: Boolean);
begin
  if FWantUnicode = Value then
    Exit;

  FWantUnicode := Value;
  FUseUnicode := FWantUnicode and FUnicodeAvailable;
end;

procedure TJvMRUList.Close;
begin
  if FList <> 0 then
  begin
    FreeMruList(FList);
    FList := 0;
  end;

  FItemIndex := -1;
  SetItemData(Pointer(nil));
end;

procedure TJvMRUList.Open;
var
  FLst: TMruRec;
begin
  if csDesigning in ComponentState then
    Exit;

  if FSubKey <> '' then
  begin
    FLst.cbSize := SizeOf(FList);
    FLst.nMaxItems := FMax;
    case FType of
      dtString:
        begin
          FLst.dwFlags := MRUF_STRING_LIST;
          FLst.lpfnCompareString := nil;
        end;
      dtBinary:
        begin
          FLst.dwFlags := MRUF_BINARY_LIST;
          FLst.lpfnCompareData := nil;
        end;
    end;
    if FDelayedWrite then
      with FLst do
        dwFlags := MRUF_DELAYED_SAVE or dwFlags;
    case FKey of
      hkClassesRoot:
        FLst.hKey := HKEY_CLASSES_ROOT;
      hkCurrentUser:
        FLst.hKey := HKEY_CURRENT_USER;
      hkLocalMachine:
        FLst.hKey := HKEY_LOCAL_MACHINE;
      hkUsers:
        FLst.hKey := HKEY_USERS;
      hkCurrentConfig:
        FLst.hKey := HKEY_CURRENT_CONFIG;
    end;
    if UseUnicode then
    // Arioch changed this
      FLst.lpszSubKeyW := PWideChar(FSubKey)
    else
      FLst.lpszSubKey := PChar(GetSubKey);

    if UseUnicode then
    // Arioch changed this
      FList := CreateMruListW(@FLst)
    else
      FList := CreateMruList(@FLst);

    if FList = 0 then
      raise EMruException.CreateRes(@RsEErrorMRUCreating);
  end;
end;

function TJvMRUList.ItemDataSize: Integer;
// Arioch: Here we rely on undocumented internal structure
// that has been used by GetMem/FreeMem for ages!
// for example see sources for GetMem.Inc in VCL sources
//
// JVCL should have a list were it relies upon undocumented parts of Delphi,
//  Windows, etc..., so when new version of D,Win,... is realeased we could
//  check the list instead of hunting for misty bug;
begin
  if ItemDataAsPointer <> nil then
    Result := Integer(Pointer(Integer(ItemDataAsPointer) - SizeOf(Integer))^)
  else
    Result := 0;
end;

procedure TJvMRUList.DoEnumText;
begin
  if Assigned(FOnEnumText) then
    FOnEnumText(Self, string(FItemData.S), ItemIndex);
//    FOnEnumText(Self, S, Index);
end;

procedure TJvMRUList.DoUnicodeEnumText;
begin
  if Assigned(FOnEnumUnicodeText) then
    FOnEnumUnicodeText(Self, WideString(FItemData.Ws), FItemIndex);
//    FOnEnumUnicodeText(Self, S, Index);
end;

procedure TJvMRUList.DoEnumData(DataSize: Integer);
begin
  if Assigned(FOnEnumData) then
    FOnEnumData(Self, FItemData.P, DataSize, FItemIndex);
end;

function TJvMRUList.DeleteKey: Boolean;
begin
  Result := False;
  with TRegistry.Create do
  try
    if (FList = 0) and (SubKey <> '') and KeyExists(SubKey) then
      Result := DeleteKey(SubKey);
  finally
    Free;
  end;
end;

function TJvMRUList.GetActive: Boolean;
begin
  Result := FList <> 0;
end;

procedure TJvMRUList.SetActive(const Value: Boolean);
begin
  if GetActive <> Value then
  begin
    if Value then
      Open
    else
      Close;
  end;
end;

function TJvMRUList.GetItemDataAsPChar: PChar;
begin
  Result := FItemData.S;
end;

function TJvMRUList.GetItemDataAsPWideChar: pWideChar;
begin
  Result := FItemData.Ws;
end;

procedure FinalizeDLL;
begin
  if hComCtlDll > 0 then
  begin
    FreeLibrary(hComCtlDll);
    hComCtlDll := 0;
  end;
end;

procedure InitializeDLL;
begin
  if hComCtlDll = 0 then
  begin
    hComCtlDll := LoadLibrary(DllComCtlName);
    if hComCtlDll > 0 then
    begin
      // (rom) can we get them by name?
      CreateMruList := GetProcAddress(hComCtlDll, PChar(151));
      FreeMruList := GetProcAddress(hComCtlDll, PChar(152));
      AddMruString := GetProcAddress(hComCtlDll, PChar(153));
      AddMruData := GetProcAddress(hComCtlDll, PChar(167));
      DelMruString := GetProcAddress(hComCtlDll, PChar(156));
      EnumMruList := GetProcAddress(hComCtlDll, PChar(154));
      FindMruString := GetProcAddress(hComCtlDll, PChar(155));
      FindMruData := GetProcAddress(hComCtlDll, PChar(169));

      if Win32Platform = VER_PLATFORM_Win32_NT then
      begin
        CreateMRUListW := GetProcAddress(hComCtlDll, PChar(400));
        AddMRUStringW := GetProcAddress(hComCtlDll, PChar(401));
        FindMRUStringW := GetProcAddress(hComCtlDll, PChar(402));
        EnumMRUListW := GetProcAddress(hComCtlDll, PChar(403));
      end;
    end
    else 
      RaiseLastOSError; 
    if hComCtlDll > 0 then
      AddFinalizeProc(sUnitName, FinalizeDLL);
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

