{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMru.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Arioch [the_Arioch@nm.ru]

Last Modified: 2002-07-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMru;

interface

{*******************************************************}
{   This unit is an interface to the MRU List (comctl32)}
{   Informations from :                                 }
{      http://www.geocities.com/SiliconValley/4942      }
{*******************************************************}

(*
the_Arioch@nm.ru

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
  JvComponent, JvTypes;

type
  TJvDataType = (dtString, dtBinary);
  TOnEnumData = procedure(Sender: TObject; Data: Pointer; Size: Integer; Index: Integer) of object;
  TOnEnumText = procedure(Sender: TObject; Value: string; Index: Integer) of object;
  TOnEnumUnicodeText = procedure(Sender: TObject; Value: WideString; Index: Integer) of object;

  TJvMruReturnData = record
    case Byte of
      0: (P: Pointer; );
      1: (S: PChar; );
      2: (Ws: PWideChar; );
  end;
  PJvMruReturnData = ^TJvMruReturnData;
  TMRUCount = 0..29;

  TJvMruList = class(TJvComponent)
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
    FItemData: TJvMruReturnData;
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
    property ItemDataAsPChar: PChar read FItemData.S;
    property ItemDataAsPWideChar: pWideChar read FItemData.Ws;
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
    function AddUnicodeString(Value: widestring): Boolean;
    function AddUnicodePChar(Value: PWideChar): Boolean;
    function FindUnicodeString(Value: widestring): Integer;
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
  JvFunctions;

resourcestring
  RC_ErrorMRU_Creating = 'Unable to create MRU';
  RC_ErrorMRU_Unicode = 'Windows NT required for Unicode in MRU';

var
  hComCtlDll: HMODULE = 0;

const
  DllComCtlName = 'COMCTL32.DLL';

type
  MruCompareString = function(lpszString1, lpszString2: PChar): Integer;
  MruCompareData = function(lpData1, lpData2: Pointer; cbData: DWORD): Integer;
  MruCompareStringW = function(lpszString1, lpszString2: PWideChar): Integer;

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
  PMruRec = ^TMruRec;

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
  AddMruString: TAddMrustring;
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

constructor TJvMruList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := 0;
  FMax := 10;
  FType := dtString;
  FKey := hkCurrentUser;
  FUnicodeAvailable := Win32Platform = VER_PLATFORM_Win32_NT;
  FDelayedWrite := False;
  SetWantUnicode(False);
  FItemData.P := nil;

  // ReCreateList;
  Close; // since there is PUBLISHED .Active property - let it control how it will be.
end;

destructor TJvMruList.Destroy;
begin
  if FList <> 0 then
    FreeMruList(FList);
  inherited Destroy;
end;

function TJvMruList.AddData(Value: Pointer; Size: Integer): Boolean;
begin
  Result := False;
  if FList <> 0 then
    Result := AddMruData(FList, Value, Size) <> -1;
end;

function TJvMruList.AddPChar(Value: string): Boolean;
begin
  Result := False;
  if FList <> 0 then
  begin
    Result := AddMruString(FList, PChar(Value)) <> -1;
    // (p3) call EnumText here ?
    //  Arioch: Why? What for?
    //  Whether You want them - make a special sepearate set of events
    //  And there's danger that eventHandler tries to get a list of items,
    //  thus, killing current section!
  end;
end;

function TJvMruList.AddUnicodePChar(Value: PWideChar): Boolean;
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

function TJvMruList.AddString(Value: string): Boolean;
begin
  Result := AddPchar(PChar(Value));
end;

function TJvMruList.AddUnicodeString(Value: widestring): Boolean;
begin
  Result := AddUnicodePChar(PWideChar(Value));
end;

function TJvMruList.DeleteItem(Index: Integer): Boolean;
begin
  Result := False;
  if FList <> 0 then
  begin
    Result := DelMruString(FList, Index);
    ReCreateList; // Arioch: fixes MS's bug
  end;
end;

function TJvMruList.EnumItems: Boolean;
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

function TJvMruList.FindData(Value: Pointer; Size: Integer): Integer;
begin
  Result := -1;
  if FList <> 0 then
    Result := FindMruData(FList, Value, Size, nil);
end;

function TJvMruList.FindString(Value: string): Integer;
begin
  Result := -1;
  if FList <> 0 then
    Result := FindMruString(FList, PChar(Value), nil);
end;

function TJvMruList.FindUnicodeString(Value: widestring): Integer;
begin
  NeedUnicode;
  Result := -1;
  if FList <> 0 then
    Result := FindMruStringW(FList, PWideChar(Value), nil);
end;

function TJvMruList.GetItem(Index: Integer): Boolean;
begin
  Result := InternalGetItem(Index);
end;

function TJvMruList.InternalGetItem(Index: Integer; FireEvent: Boolean): Boolean;
var
  i: Integer;
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
      i := EnumMruList(FList, Index, P, 256);
      if i > 255 then
      begin
        ReAllocMem(P, i + 1);
        i := EnumMruList(FList, Index, P, i + 1);
      end;
      if i <> -1 then
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
      i := EnumMruListW(FList, Index, P, 256);
      if i > 255 then
      begin
        ReAllocMem(P, (i + 1) * 2);
        i := EnumMruListW(FList, Index, P, i + 1);
      end;
      if i <> -1 then
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

    i := EnP(FList, Index, P, 1024);

    if i >= 1024 then
    begin
      ReAllocMem(P, 64000); // Arioch: Hmmm We'll never guess how much may there appear :)
      i := EnP(FList, 0, P, 64000);
    end;

    if i <> -1 then
    begin
      Result := True;
      ReAllocMem(P, i);
      // Arioch: should we waste more memory than we need?
      // and we can know the size of memory allocated
      // with GetMem and ReAllocMem, so we know how big Data was
      SetItemData(P);
      FItemIndex := Index;
      if FireEvent then
        DoEnumData(i);
    end;
  end;
end;

function TJvMruList.GetItemsCount: Integer;
begin
  Result := 0;
  if FList <> 0 then
    Result := EnumMruList(FList, -1, nil, 0);
end;

function TJvMruList.GetMostRecentItem: Boolean;
begin
  Result := GetItem(0);
end;

function TJvMruList.GetSubKey: string;
begin
  Result := string(FSubKey);
end;

procedure TJvMruList.MoveToTop(const Index: Integer);
var
  b: Boolean;
begin
  b := False;
  if InternalGetItem(Index, False) then
  begin
    if FType = dtString then
    begin
      if UseUnicode then
        b := AddUnicodePChar(ItemDataAsPWideChar)
      else
        b := AddPChar(ItemDataAsPChar);
    end
    else
      b := AddData(ItemDataAsPointer, ItemDataSize);
  end;
  if b then
    FItemIndex := 0;
end;

procedure TJvMruList.NeedUnicode;
begin
  if not UnicodeAvailable then
    raise EMruException.Create(RC_ErrorMRU_Unicode);
end;

procedure TJvMruList.ReCreateList;
begin
  Close;
  Open;
end;

procedure TJvMruList.SetItemData(const P: Pointer);
begin
  if P = FItemData.P then
    Exit;
  if FItemData.P <> nil then
    FreeMem(FItemData.P);
  FItemData.P := P;
end;

procedure TJvMruList.SetKey(const Value: TJvRegKey);
begin
  if Value <> FKey then
  begin
    FKey := Value;
    ReCreateList;
  end;
end;

procedure TJvMruList.SetMax(const Value: TMRUCount);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    ReCreateList;
  end;
end;

procedure TJvMruList.SetSubKey(const Value: string);
begin
  SetSubKeyUnicode(WideString(Value));
end;

procedure TJvMruList.SetSubKeyUnicode(const Value: WideString);
begin
  if Value <> FSubKey then
  begin
    FSubKey := Value;
    ReCreateList;
  end;
end;

procedure TJvMruList.SetType(const Value: TJvDataType);
begin
  if Value <> FType then
  begin
    FType := Value;
    ReCreateList;
  end;
end;

procedure TJvMruList.SetUseUnicode(const Value: Boolean);
begin
  if Value then
    NeedUnicode;
  if FUseUnicode = Value then
    Exit;
  FUseUnicode := Value;
end;

procedure TJvMruList.SetWantUnicode(const Value: Boolean);
begin
  if FWantUnicode = Value then
    Exit;

  FWantUnicode := Value;
  FUseUnicode := FWantUnicode and FUnicodeAvailable;
end;

procedure TJvMruList.Close;
begin
  if FList <> 0 then
  begin
    FreeMruList(FList);
    FList := 0;
  end;

  FItemIndex := -1;
  SetItemData(Pointer(nil));
end;

procedure TJvMruList.Open;
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
      raise EMruException.Create(RC_ErrorMRU_Creating);
  end;
end;

function TJvMruList.ItemDataSize: Integer;
// Arioch: Here we rely on undocumemted internal structure
// that has been used by GetMem/FreeMem for ages!
// for example see sources for GetMem.Inc in VCL sources
//
// JVCL should have a list were it relies upon undocumented parts of Delphi,
//  Windows, etc..., so when new version of D,Win,... is realeased we could
//  check the list instead of hunting for misty bug;
begin
  Result := 0;
  if ItemDataAsPointer = nil then
    Exit;
  Result := Integer(Pointer(Integer(ItemDataAsPointer) - SizeOf(Integer))^);
end;

procedure TJvMruList.DoEnumText;
begin
  if Assigned(FOnEnumText) then
    FOnEnumText(Self, string(FItemData.S), ItemIndex);
//    FOnEnumText(Self, S, Index);
end;

procedure TJvMruList.DoUnicodeEnumText;
begin
  if Assigned(FOnEnumUnicodeText) then
    FOnEnumUnicodeText(Self, WideString(FItemData.Ws), FItemIndex);
//    FOnEnumUnicodeText(Self, S, Index);
end;

procedure TJvMruList.DoEnumData(DataSize: Integer);
begin
  if Assigned(FOnEnumData) then
    FOnEnumData(Self, FItemData.P, DataSize, FItemIndex);
end;

function TJvMruList.DeleteKey: Boolean;
begin
  Result := False;
  with TRegistry.Create do
  try
    if (FList = 0) and (SubKey <> '') and KeyExists(SubKey) then
      Result := DeleteKey(SubKey);
  finally
    free;
  end;
end;

function TJvMruList.GetActive: Boolean;
begin
  Result := FList <> 0;
end;

procedure TJvMruList.SetActive(const Value: Boolean);
begin
  if GetActive <> Value then
  begin
    if Value then
      Open
    else
      Close;
  end;
end;

initialization
  hComCtlDll := LoadLibrary(DllComCtlName);
  if hComCtlDll <> 0 then
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
    (* else  Since TurboPascal 7.0 all the global vars are filled with Zero'es at the start, yes?
    begin
      CreateMRUListW := nil;
      AddMRUStringW := nil;
      FindMRUStringW := nil;
      EnumMRUListW := nil;
    end;   *)
  end
  else
    PError('MRU');

finalization
  if hComCtlDll <> 0 then
    FreeLibrary(hComCtlDll);

end.

