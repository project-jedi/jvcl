{******************************************************************************}
{                                                                              }
{ Project JEDI VCL                                                             }
{ HID component for complete HID access                                        }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JvHidControllerClass.pas.                                 }
{                                                                              }
{ The Initial Developer of the Original Code is Robert Marquardt.              }
{ (robert_marquardt@gmx.de)                                                    }
{                                                                              }
{ Portions created by Robert Marquardt are                                     }
{ Copyright (c) 1999-2002 Robert Marquardt.                                    }
{                                                                              }
{ Last modified: March 11, 2002                                                }
{                                                                              }
{******************************************************************************}

unit JvHidControllerClass;

interface

uses
  Windows, Messages, Classes, Forms, SysUtils, Dialogs,
  DBT, SetupApi, Hid;

const
  // a version string for the component
  cHidControllerClassVersion = '1.0.2';

  // strings from the registry for CheckOutByClass
  cHidKeyboardClass = 'Keyboard';
  cHidMouseClass    = 'Mouse';
  cHidNoClass       = 'HIDClass';

type
  // forward declarations
  TJvHidDeviceController = class;
  TJvHidDevice           = class;

  // the Event function declarations
  TJvHidEnumerateEvent  = function (const HidDev: TJvHidDevice;
                                    const Idx: Integer): Boolean of object;
  TJvHidUnplugEvent     = procedure(const HidDev: TJvHidDevice) of object;

  // open overlapped read or write file handle
  TJvOpenExMode = (omRead, omWrite);

  // the physical descriptor
  TJvPhysicalDescriptor = array of WORD;

  // all HID relevant driver entries in the registry
  TJvPnPInfo = record
    DeviceID:        DWORD;
    DevicePath:      string;
    // registry values
    Capabilities:    DWORD;
    ClassDescr:      string;
    ClassGUID:       string;
    CompatibleIDs:   string;
    ConfigFlags:     DWORD;
    DeviceDescr:     string;
    Driver:          string;
    FriendlyName:    string;
    HardwareID:      string;
    LowerFilters:    string;
    Mfg:             string;
    UpperFilters:    string;
    Address:         string;
    BusNumber:       DWORD;
    BusType:         string;
    Characteristics: string;
    DevType:         string;
    EnumeratorName:  string;
    Exclusive:       DWORD;
    LegacyBusType:   string;
    LocationInfo:    string;
    PhysDevObjName:  string;
    Security:        string;
    Service:         string;
    UINumber:        DWORD;
    UINumberFormat:  string;
  end;

  // the representation of a HID device

  TJvHidDevice = class(TObject)
  private
    // internal control variables
    FMyController:         TJvHidDeviceController;
    FIsPluggedIn:          Boolean;
    FIsCheckedOut:         Boolean;
    FIsEnumerated:         Boolean;
    FHidFileHandle:        THandle;
    FHidOverlappedRead:    THandle;
    FHidOverlappedWrite:   THandle;
    FOvlRead:              TOverlapped;
    FOvlWrite:             TOverlapped;
    // internal properties part
    FAttributes:           THIDDAttributes;
    FCaps:                 THIDPCaps;
    FPnPInfo:              TJvPnPInfo;
    FVendorName:           WideString;
    FProductName:          WideString;
    FPhysicalDescriptor:   TJvPhysicalDescriptor;
    FSerialNumber:         WideString;
    FLanguageStrings:      TStringList;
    FPreparsedData:        PHIDPPreparsedData;
    FNumInputBuffers:      Integer;
    FNumOverlappedBuffers: Integer;
    FLinkCollection:       array of THIDPLinkCollectionNode;
    FMaxDataListLength:    ULONG;
    FMaxUsageListLength:   ULONG;
    FMaxButtonListLength:  ULONG;
    FReportTypeParam:      THIDPReportType;
    FUsagePageParam:       TUsage;
    FLinkCollectionParam:  WORD;
    FUsageParam:           TUsage;
    FUnplug:               TJvHidUnplugEvent;

    // tells if access to device is allowed
    function  IsAccessible: Boolean;
    procedure GetMax;

    // internal property implementors
    function  GetDeviceStringAnsi    (Idx: Byte): string;
    function  GetDeviceStringUnicode (Idx: Byte): WideString;
    function  GetLinkCollectionNode  (Idx: WORD): THIDPLinkCollectionNode;
    function  GetConfiguration: THIDDConfiguration;
    procedure SetConfiguration       (const Config: THIDDConfiguration);
    procedure SetNumInputBuffers     (const Num: Integer);
    procedure SetNumOverlappedBuffers(const Num: Integer);
    procedure SetReportTypeParam     (const ReportType: THIDPReportType);
    procedure SetUsagePageParam      (const UsagePage: TUsage);

    // Constructor is hidden! Only a TJvHidDeviceController can create a TJvHidDevice object.
    constructor CtlCreate(const PnPInfo: TJvPnPInfo;
                          const Controller: TJvHidDeviceController);

  protected
    // internal event implementors
    procedure DoUnplug;
    procedure SetUnplug(const Event: TJvHidUnplugEvent);

  public
    // dummy constructor
    constructor Create;
    destructor Destroy; override;

    // management properties
    property Attributes:    THIDDAttributes    read FAttributes;
    property Caps:          THIDPCaps          read FCaps;
    property Configuration: THIDDConfiguration read GetConfiguration write SetConfiguration;
    property PreparsedData: PHIDPPreparsedData read FPreparsedData;
    // indexed properties
    property DeviceStrings       [Idx: Byte]: string                  read GetDeviceStringAnsi;
    property DeviceStringsUnicode[Idx: Byte]: WideString              read GetDeviceStringUnicode;
    property LinkCollectionNodes [Idx: WORD]: THIDPLinkCollectionNode read GetLinkCollectionNode;

    // methods
    function  CancelIO             (const Mode: TJvOpenExMode): Boolean;
    procedure CloseFile;
    procedure CloseFileEx          (const Mode: TJvOpenExMode);
    function  DeviceIoControl      (IoControlCode: DWORD; InBuffer: Pointer; InSize: DWORD;
                                    OutBuffer: Pointer; OutSize: DWORD;
                                    var BytesReturned: DWORD):                      Boolean;
    function  FlushQueue:                                                           Boolean;
    function  GetButtonCaps        (ButtonCaps: PHIDPButtonCaps; var Count: WORD):  NTSTATUS;
    function  GetButtons           (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetButtonsEx         (UsageList: PUsageAndPage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetData              (DataList: PHIDPData; var DataLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetFeature           (var Report; const Size: Integer):               Boolean;
    function  GetScaledUsageValue  (var UsageValue: Integer;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetSpecificButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD):  NTSTATUS;
    function  GetSpecificValueCaps (ValueCaps:  PHIDPValueCaps;  var Count: WORD):  NTSTATUS;
    function  GetUsages            (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetUsagesEx          (UsageList: PUsageAndPage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetUsageValue        (var UsageValue: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetUsageValueArray   (UsageValue: PChar; UsageValueByteLength: WORD;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  GetValueCaps         (ValueCaps:  PHIDPValueCaps;  var Count: WORD):  NTSTATUS;
    function  OpenFile:                                                             Boolean;
    function  OpenFileEx           (Mode: TJvOpenExMode):                           Boolean;
    function  SetButtons           (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  SetData              (DataList: PHIDPData; var DataLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  SetFeature           (var Report; const Size: Integer):               Boolean;
    function  SetScaledUsageValue  (UsageValue: Integer;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  SetUsages            (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  SetUsageValue        (UsageValue: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  SetUsageValueArray   (UsageValue: PChar; UsageValueByteLength: WORD;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  UnsetButtons         (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  UnsetUsages          (UsageList: PUsage; var UsageLength: ULONG;
                                    var Report; ReportLength: ULONG):               NTSTATUS;
    function  ReadFile             (var Report; ToRead:  DWORD; var BytesRead:    DWORD): Boolean;
    function  ReadFileEx           (var Report; ToRead:  DWORD;
                                    CallBack: TPROverlappedCompletionRoutine):            Boolean;
    function  WriteFile            (var Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
    function  WriteFileEx          (var Report; ToWrite: DWORD;
                                    CallBack: TPROverlappedCompletionRoutine):            Boolean;

    // management properties
    property HidFileHandle:        THandle               read FHidFileHandle;
    property HidOverlappedRead:    THandle               read FHidOverlappedRead;
    property HidOverlappedWrite:   THandle               read FHidOverlappedWrite;
    property IsCheckedOut:         Boolean               read FIsCheckedOut;
    property IsPluggedIn:          Boolean               read FIsPluggedIn;
    property LanguageStrings:      TStringList           read FLanguageStrings;
    property MaxButtonListLength:  ULONG                 read FMaxButtonListLength;
    property MaxDataListLength:    ULONG                 read FMaxDataListLength;
    property MaxUsageListLength:   ULONG                 read FMaxUsageListLength;
    property PhysicalDescriptor:   TJvPhysicalDescriptor read FPhysicalDescriptor;
    property PnPInfo:              TJvPnPInfo            read FPnPInfo;
    property ProductName:          WideString            read FProductName;
    property SerialNumber:         WideString            read FSerialNumber;
    property VendorName:           WideString            read FVendorName;

  published
    property LinkCollectionParam:  WORD              read FLinkCollectionParam  write FLinkCollectionParam;
    property NumInputBuffers:      Integer           read FNumInputBuffers      write SetNumInputBuffers;
    property NumOverlappedBuffers: Integer           read FNumOverlappedBuffers write SetNumOverlappedBuffers;
    property ReportTypeParam:      THIDPReportType   read FReportTypeParam      write SetReportTypeParam;
    property UsagePageParam:       TUsage            read FUsagePageParam       write SetUsagePageParam;
    property UsageParam:           TUsage            read FUsageParam           write FUsageParam;
    // the only event property
    property OnUnplug:             TJvHidUnplugEvent read FUnplug               write SetUnplug;
  end;

  // controller class to manage all HID devices

  TJvHidDeviceController = class(TComponent)
  private
    // internal properties part
    FHidGuid:              TGUID;
    FDeviceChangeEvent:    TNotifyEvent;
    FEnumerateEvent:       TJvHidEnumerateEvent;
    FDevUnplugEvent:       TJvHidUnplugEvent;
    FDeviceChangeFired:    Boolean;
    // internal list of all HID device objects
    FList:                 TList;
    // counters for the list
    FNumCheckedInDevices:  Integer;
    FNumCheckedOutDevices: Integer;
    FNumUnpluggedDevices:  Integer;
    // internal worker functions
    function  CheckThisOut(var HidDev: TJvHidDevice; Idx: Integer; Check: Boolean): Boolean;
    procedure FillInList  (var List: TList);
    function  EventPipe   (var Msg: TMessage): Boolean;

  protected
    procedure DoDeviceChange;
    // internal event implementors
    function  DoEnumerate         (HidDev: TJvHidDevice; Idx: Integer): Boolean;
    procedure SetDeviceChangeEvent(const Notifier:   TNotifyEvent);
    procedure SetEnumerate        (const Enumerator: TJvHidEnumerateEvent);
    procedure SetDevUnplug        (const Unplugger:  TJvHidUnplugEvent);

  public
    // just to be complete the GUID
    property HidGuid: TGUID read FHidGuid;

    // normal constructor/destructor
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // methods to hand out HID device objects
    procedure CheckIn              (var HidDev: TJvHidDevice);
    function  CheckOut             (var HidDev: TJvHidDevice): Boolean;
    function  CheckOutByClass      (var HidDev: TJvHidDevice; const ClassName:   string):     Boolean;
    function  CheckOutByID         (var HidDev: TJvHidDevice; const Vid, Pid:    Integer):    Boolean;
    function  CheckOutByIndex      (var HidDev: TJvHidDevice; const Idx:         Integer):    Boolean;
    function  CheckOutByProductName(var HidDev: TJvHidDevice; const ProductName: WideString): Boolean;
    function  CheckOutByVendorName (var HidDev: TJvHidDevice; const VendorName:  WideString): Boolean;
    function  CountByClass         (const ClassName:   string):     Integer;
    function  CountByID            (const Vid, Pid:    Integer):    Integer;
    function  CountByProductName   (const ProductName: WideString): Integer;
    function  CountByVendorName    (const VendorName:  WideString): Integer;
    // iterate over the HID devices
    function  Enumerate: Integer;
    class function HidVersion: string;

    property NumCheckedInDevices:  Integer read FNumCheckedInDevices;
    property NumCheckedOutDevices: Integer read FNumCheckedOutDevices;
    property NumUnpluggedDevices:  Integer read FNumUnpluggedDevices;

  published
    // the iterator event
    property  OnEnumerate:    TJvHidEnumerateEvent read FEnumerateEvent    write SetEnumerate;
    // the central event for HID device changes
    property  OnDeviceChange: TNotifyEvent         read FDeviceChangeEvent write SetDeviceChangeEvent;
    // this event is copied to TJvHidDeviceOnUnplug on creation
    property  OnDeviceUnplug: TJvHidUnplugEvent    read FDevUnplugEvent    write SetDevUnplug;
    // to be callable at design time
    procedure DeviceChange;
  end;

// helpers to check the HID function and method results
function HidCheck(const RetVal: NTSTATUS): NTSTATUS; overload;
function HidCheck(const RetVal: LongBool): LongBool; overload;
function HidError(const RetVal: NTSTATUS): NTSTATUS;


implementation
uses
  JvTypes;

{.$R HidControllerClass.dcr}

type
  EControllerError = class(EJVCLException);
  EHidClientError  = class(EJVCLException);

var
  // counter to prevent a second TJvHidDeviceController instance
  GlobalInstanceCount: Integer = 0;

//== these are declared inconsistent in Windows.pas ============================

function ReadFileEx(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var Overlapped: TOverlapped; lpCompletionRoutine: TPROverlappedCompletionRoutine): BOOL; stdcall;
  external 'kernel32.dll' name 'ReadFileEx';
function WriteFileEx(hFile: THandle; var Buffer; nNumberOfBytesToWrite: DWORD;
  var Overlapped: TOverlapped; lpCompletionRoutine: TPROverlappedCompletionRoutine): BOOL; stdcall;
 external 'kernel32.dll' name 'WriteFileEx';

//== TJvHidDevice ==============================================================

// internal helpers to read values from a devices registry area

function GetRegistryPropertyString(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): string;
var
  BytesReturned: DWORD;
  RegDataType:   DWORD;
  Buffer:        array [0..256] of Char;
begin
  BytesReturned := 0;
  RegDataType   := 0;
  Buffer[0]     := #0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, @Buffer[0], SizeOf(Buffer), BytesReturned);
  Result := Buffer;
end;

//------------------------------------------------------------------------------

function GetRegistryPropertyDWord(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
var
  BytesReturned: DWORD;
  RegDataType:   DWORD;
begin
  BytesReturned := 0;
  RegDataType   := 0;
  Result        := 0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, @Result, SizeOf(Result), BytesReturned);
end;

//-- TJvHidDevice: basics and internals ----------------------------------------

// dummy constructor to catch invalid Creates

constructor TJvHidDevice.Create;
begin
  FHidFileHandle      := INVALID_HANDLE_VALUE;
  FHidOverlappedRead  := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite := INVALID_HANDLE_VALUE;
  raise EControllerError.Create('Direct creation of a TJvHidDevice object is not allowed');
end;

// create and fill in a HidDevice object
// the constructor is only accessible from TJvHidController
// PnPInfo contains all info the JvHidDeviceController collected
// Controller is the devices controller object to be referenced
// internally

constructor TJvHidDevice.CtlCreate(const PnPInfo: TJvPnPInfo; const Controller: TJvHidDeviceController);
var
  I:      Integer;
  Len:    Integer;
  Siz:    ULONG;
  Buffer: array [0..253] of WideChar;
  IDs:    array [0..253] of WORD;
  Name:   array [0..255] of Char;
begin
  inherited Create;

  // initialize private data
  FPnPInfo              := PnPInfo;
  FMyController         := Controller;
  FIsPluggedIn          := True;
  FIsCheckedOut         := False;
  FIsEnumerated         := False;
  FHidOverlappedRead    := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite   := INVALID_HANDLE_VALUE;
  FVendorName           := '';
  FProductName          := '';
  SetLength(FPhysicalDescriptor, 0);
  FSerialNumber         := '';
  FLanguageStrings      := TStringList.Create;
  FPreparsedData        := nil;
  FillChar(FCaps, SizeOf(FCaps), #0);
  FNumInputBuffers      := 0;
  FNumOverlappedBuffers := 0;
  SetLength(FLinkCollection, 0);
  FMaxDataListLength    := 0;
  FMaxUsageListLength   := 0;
  FMaxButtonListLength  := 0;
  FReportTypeParam      := HIDP_Input;
  FUsagePageParam       := 0;
  FLinkCollectionParam  := 0;
  FUsageParam           := 0;
  FUnplug               := Controller.FDevUnplugEvent;

  FHidFileHandle := CreateFile(PChar(FPnPInfo.DevicePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  // Win2000 hack
  if HidFileHandle = INVALID_HANDLE_VALUE then
    FHidFileHandle := CreateFile(PChar(FPnPInfo.DevicePath), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if HidFileHandle <> INVALID_HANDLE_VALUE then
  begin
    // get all device data through HID functions
    // this eliminates the need to redeclare them as methods
    FAttributes.Size := SizeOf(THIDDAttributes);
    HidD_GetPreparsedData(HidFileHandle, FPreparsedData);
    HidD_GetAttributes   (HidFileHandle, FAttributes);
    HidP_GetCaps(FPreparsedData, FCaps);
    // calculate length of StringDescriptor 0
    FillChar(IDs, SizeOf(IDs), $FF);
    Len := 0;
    if HidD_GetIndexedString(HidFileHandle, 0, PWideChar(@IDs), SizeOf(IDs)) then
      for I := High(IDs) downto 0 do
        if IDs[I] <> $FFFF then
        begin
          Len := I+1;
          Break;
        end;
    // transform id into localized language name
    for I := 0 to Len - 1 do
    begin
      Name[0] := #0;
      if GetLocaleInfo(WORD(IDs[I]), LOCALE_SLANGUAGE, Name, SizeOf(Name)) <> 0 then
        FLanguageStrings.Add(Name)
      else
        FLanguageStrings.Add(Format('unknown Locale ID $%.4x',[WORD(IDs[I])]));
    end;
    if HidD_GetManufacturerString(HidFileHandle, Buffer, SizeOf(Buffer)) then
      FVendorName := Buffer;
    if HidD_GetProductString(HidFileHandle, Buffer, SizeOf(Buffer)) then
      FProductName := Buffer;
    I := 0;
    SetLength(FPhysicalDescriptor, 2048);
    while not HidD_GetPhysicalDescriptor(HidFileHandle, FPhysicalDescriptor[0], I*SizeOf(WORD)) do
    begin
      Inc(I);
      if (I > 2048) or (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      begin
        I := 0;
        Break;
      end;
    end;
    SetLength(FPhysicalDescriptor, I);
    // compensate for buggy function
    if HidD_GetSerialNumberString(HidFileHandle, Buffer, SizeOf(Buffer)) then
      for I := 0 to Len - 1 do
        if IDs[I] <> WORD(Buffer[I]) then
        begin
          FSerialNumber := Buffer;
          Break;
        end;
    Siz := FCaps.NumberLinkCollectionNodes;
    SetLength(FLinkCollection, Siz);
    HidP_GetLinkCollectionNodes(@FLinkCollection[0], Siz, FPreparsedData);
  end;
  // the file is closed to stop using up resources
  CloseFile;
end;

//------------------------------------------------------------------------------

// If a TJvHidDevice is destroyed the TJvHidController has to be informed.
// If the device is plugged in this TJvHidDevice instance is destroyed,
// but another instance is created in the controller list

destructor TJvHidDevice.Destroy;
var
  I: Integer;
begin
  // to prevent strange problems
  OnUnplug := nil;
  // free the data which needs special handling
  CloseFile;
  CloseFileEx(omRead);
  CloseFileEx(omWrite);
  if FPreparsedData <> nil then
    HidD_FreePreparsedData(FPreparsedData);
  FLanguageStrings.Free;

  // if controller exists
  if FMyController <> nil then
    with FMyController do
    begin
      // delete device from controller list
      for I := 0 to FList.Count - 1 do
        if FList.Items[I] = Self then
        begin
          // if device is plugged in create a checked in copy
          if IsPluggedIn then
          begin
            FList.Items[I] := TJvHidDevice.CtlCreate(FPnPInfo, FMyController);
            if IsCheckedOut then
            begin
              Dec(FNumCheckedOutDevices);
              Inc(FNumCheckedInDevices);
            end;
          end
          else
          begin
            FList.Delete(I);
            Dec(FNumUnpluggedDevices);
          end;
          Break;
        end;
    end;

  inherited Destroy;
end;

//------------------------------------------------------------------------------

// if check changes change check only here

function TJvHidDevice.IsAccessible: Boolean;
begin
  Result := IsPluggedIn and (IsCheckedOut or FIsEnumerated);
end;

//------------------------------------------------------------------------------

// open the device "file" (for the other methods)

function TJvHidDevice.OpenFile: Boolean;
begin
  // check if open allowed (propagates this state)
  if IsAccessible then
    if HidFileHandle = INVALID_HANDLE_VALUE then // if not already opened
    begin
      FHidFileHandle := CreateFile(PChar(FPnPInfo.DevicePath), GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      // Win2000 hack
      if HidFileHandle = INVALID_HANDLE_VALUE then
        FHidFileHandle := CreateFile(PChar(FPnPInfo.DevicePath), 0,
          FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      if HidFileHandle <> INVALID_HANDLE_VALUE then
      begin
        if NumInputBuffers <> 0 then
          HidD_SetNumInputBuffers(HidFileHandle, NumInputBuffers);
        HidD_GetNumInputBuffers(HidFileHandle, FNumInputBuffers);
      end;
    end;
  Result := HidFileHandle <> INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

// open second device "file" for ReadFileEx and WriteFileEx

function TJvHidDevice.OpenFileEx(Mode: TJvOpenExMode): Boolean;
begin
  Result := False;
  // check if open allowed (propagates this state)
  if IsAccessible then
    if Mode = omRead then
    begin
      if HidOverlappedRead = INVALID_HANDLE_VALUE then // if not already opened
      begin
        FHidOverlappedRead := CreateFile(PChar(FPnPInfo.DevicePath), GENERIC_READ,
          FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
        if FHidOverlappedRead <> INVALID_HANDLE_VALUE then
        begin
          if NumOverlappedBuffers <> 0 then
            HidD_SetNumInputBuffers(FHidOverlappedRead, NumOverlappedBuffers);
          HidD_GetNumInputBuffers(FHidOverlappedRead, FNumOverlappedBuffers);
        end;
      end;
      Result := FHidOverlappedRead <> INVALID_HANDLE_VALUE;
    end
    else
    begin
      if HidOverlappedWrite = INVALID_HANDLE_VALUE then // if not already opened
        FHidOverlappedWrite := CreateFile(PChar(FPnPInfo.DevicePath), GENERIC_WRITE,
          FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
      Result := FHidOverlappedWrite <> INVALID_HANDLE_VALUE;
    end;
end;

//------------------------------------------------------------------------------

// implement OnUnplug event

procedure TJvHidDevice.DoUnplug;
begin
  CloseFile;
  CloseFileEx(omRead);
  CloseFileEx(omWrite);
  FIsPluggedIn := False;
  // event even for checked in devices
  if Assigned(FUnplug) then
    FUnplug(Self);
  // guarantees that event is only called once
  OnUnplug := nil;
end;

//------------------------------------------------------------------------------

// assign the OnUnplug event

procedure TJvHidDevice.SetUnplug(const Event: TJvHidUnplugEvent);
begin
  if @Event <> @FUnplug then
    FUnplug := Event;
end;

//------------------------------------------------------------------------------

// implementing indexed properties read

function TJvHidDevice.GetDeviceStringAnsi(Idx: Byte): string;
begin
  Result := WideCharToString(PWideChar(GetDeviceStringUnicode(Idx)));
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetDeviceStringUnicode(Idx: Byte): WideString;
var
  Buffer: array [0..256] of WideChar;
begin
  Result := '';
  if Idx <> 0 then
    if OpenFile then
      if HidD_GetIndexedString(HidFileHandle, Idx, Buffer, SizeOf(Buffer)) then
        Result := Buffer;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetLinkCollectionNode(Idx: WORD): THIDPLinkCollectionNode;
begin
  FillChar(Result, SizeOf(THIDPLinkCollectionNode), #0);
  if (Idx > 0) and (Idx <= Length(FLinkCollection)) then
    Result := FLinkCollection[Idx-1];
end;

//------------------------------------------------------------------------------

// implementing properties write

procedure TJvHidDevice.SetNumInputBuffers(const Num: Integer);
begin
  if (Num <> FNumInputBuffers) and OpenFile then
  begin
    HidD_SetNumInputBuffers(HidFileHandle, Num);
    HidD_GetNumInputBuffers(HidFileHandle, FNumInputBuffers);
  end;
end;

//------------------------------------------------------------------------------

procedure TJvHidDevice.SetNumOverlappedBuffers(const Num: Integer);
begin
  if (Num <> FNumInputBuffers) and OpenFileEx(omRead) then
  begin
    HidD_SetNumInputBuffers(HidOverlappedRead, Num);
    HidD_GetNumInputBuffers(HidOverlappedRead, FNumOverlappedBuffers);
  end;
end;

//------------------------------------------------------------------------------

// internal helper for the following functions

procedure TJvHidDevice.GetMax;
begin
  if IsAccessible then
  begin
    FMaxDataListLength   := HidP_MaxDataListLength  (ReportTypeParam, PreparsedData);
    FMaxUsageListLength  := HidP_MaxUsageListLength (ReportTypeParam, UsagePageParam, PreparsedData);
    FMaxButtonListLength := HidP_MaxButtonListLength(ReportTypeParam, UsagePageParam, PreparsedData);
  end;
end;

//------------------------------------------------------------------------------

procedure TJvHidDevice.SetReportTypeParam(const ReportType: THIDPReportType);
begin
  FReportTypeParam := ReportType;
  GetMax;
end;

//------------------------------------------------------------------------------

procedure TJvHidDevice.SetUsagePageParam(const UsagePage: TUsage);
begin
  FUsagePageParam := UsagePage;
  GetMax;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetConfiguration: THIDDConfiguration;
begin
  Result.cookie         := nil;
  Result.size           := 0;
  Result.RingBufferSize := 0;
  if OpenFile then
    HidD_GetConfiguration(HidFileHandle, Result, SizeOf(THIDDConfiguration));
end;

//------------------------------------------------------------------------------

procedure TJvHidDevice.SetConfiguration(const Config: THIDDConfiguration);
begin
  if OpenFile then
    HidD_SetConfiguration(HidFileHandle, Config, SizeOf(THIDDConfiguration));
end;

//-- TJvHidDevice methods ------------------------------------------------------

// generally the parameter count of the methods is reduced with the Param properties
// first assign the Param properties the desired value then call a method
// normally you will address the same Usage, UsagePage, ReportType or LinkCollection
// with more than one method
//
// the methods will open the device file when needed
// this file is not closed until unplug or destruction to speed up access

// cancel asynchronous operations on either HidOverlappedRead or HidOverlappedWrite

function TJvHidDevice.CancelIO(const Mode: TJvOpenExMode): Boolean;
begin
  Result := False;
  if (Mode = omRead) and (HidOverlappedRead <> INVALID_HANDLE_VALUE) then
    Result := Windows.CancelIO(HidOverlappedRead)
  else
  if (Mode = omWrite) and (HidOverlappedWrite <> INVALID_HANDLE_VALUE) then
    Result := Windows.CancelIO(HidOverlappedWrite);
end;

//------------------------------------------------------------------------------

// close the device "file"
// if you want to open the file directly close this
// to get undisturbed access

procedure TJvHidDevice.CloseFile;
begin
  if HidFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(HidFileHandle);
  FNumInputBuffers := 0;
  FHidFileHandle   := INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

// same for the other device "file"

procedure TJvHidDevice.CloseFileEx(const Mode: TJvOpenExMode);
begin
  if Mode = omRead then
  begin
    if HidOverlappedRead <> INVALID_HANDLE_VALUE then
      CloseHandle(HidOverlappedRead);
    FNumOverlappedBuffers := 0;
    FHidOverlappedRead := INVALID_HANDLE_VALUE;
  end
  else
  begin
    if HidOverlappedWrite <> INVALID_HANDLE_VALUE then
      CloseHandle(HidOverlappedWrite);
    FHidOverlappedWrite := INVALID_HANDLE_VALUE;
  end;
end;

//------------------------------------------------------------------------------

// all the methods which directly map to a HID-function

function TJvHidDevice.FlushQueue: Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_FlushQueue(HidFileHandle);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetFeature(var Report; const Size: Integer): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_GetFeature(HidFileHandle, Report, Size);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetFeature(var Report; const Size: Integer): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_SetFeature(HidFileHandle, Report, Size);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetSpecificButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetSpecificButtonCaps(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam, ButtonCaps, Count, PreparsedData);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtonCaps_(ReportTypeParam, ButtonCaps, Count, PreparsedData);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetSpecificValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetSpecificValueCaps(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam, ValueCaps, Count, PreparsedData);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetValueCaps_(ReportTypeParam, ValueCaps, Count, PreparsedData);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetData(DataList: PHIDPData; var DataLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetData(ReportTypeParam, DataList, DataLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetData(DataList: PHIDPData; var DataLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetData(ReportTypeParam, DataList, DataLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetUsages(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetButtons(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetUsagesEx(UsageList: PUsageAndPage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsagesEx(ReportTypeParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetButtonsEx(UsageList: PUsageAndPage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtonsEx(ReportTypeParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetUsages(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetButtons(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.UnsetUsages(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_UnsetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.UnsetButtons(UsageList: PUsage; var UsageLength: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_UnsetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetUsageValue(var UsageValue: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                 UsageValue, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetScaledUsageValue(var UsageValue: Integer; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetScaledUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                       UsageValue, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetUsageValueArray(UsageValue: PChar; UsageValueByteLength: WORD; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsageValueArray(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                      UsageValue, UsageValueByteLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetUsageValue(UsageValue: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                 UsageValue, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetScaledUsageValue(UsageValue: Integer; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetScaledUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                       UsageValue, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.SetUsageValueArray(UsageValue: PChar; UsageValueByteLength: WORD; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL;  // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsageValueArray(ReportTypeParam, UsagePageParam, LinkCollectionParam, UsageParam,
                                      UsageValue, UsageValueByteLength, PreparsedData, Report, ReportLength);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.DeviceIoControl(IoControlCode: DWORD; InBuffer: Pointer; InSize: DWORD;
  OutBuffer: Pointer; OutSize: DWORD; var BytesReturned: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.DeviceIoControl(HidFileHandle, IoControlCode, InBuffer, InSize, OutBuffer, OutSize, BytesReturned, nil);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.ReadFile(var Report; ToRead: DWORD; var BytesRead: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.ReadFile(HidFileHandle, Report, ToRead, BytesRead, nil);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.WriteFile(var Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.WriteFile(HidFileHandle, Report, ToWrite, BytesWritten, nil);
end;

//------------------------------------------------------------------------------

// the TOverlapped structure is not needed externally
// the hEvent element is used to transport the device object
// to the callback function
// Better not implement a Delphi event with that

function TJvHidDevice.ReadFileEx(var Report; ToRead: DWORD; CallBack: TPROverlappedCompletionRoutine): Boolean;
begin
  Result := False;
  if OpenFileEx(omRead) then
  begin
    FillChar(FOvlRead, SizeOf(TOverlapped), #0);
    FOvlRead.hEvent := DWORD(Self);
    Result := JvHidControllerClass.ReadFileEx(HidOverlappedRead, Report, ToRead, FOvlRead, CallBack);
  end;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.WriteFileEx(var Report; ToWrite: DWORD; CallBack: TPROverlappedCompletionRoutine): Boolean;
begin
  Result := False;
  if OpenFileEx(omWrite) then
  begin
    FillChar(FOvlWrite, SizeOf(TOverlapped), #0);
    FOvlWrite.hEvent := DWORD(Self);
    Result := JvHidControllerClass.WriteFileEx(HidOverlappedWrite, Report, ToWrite, FOvlWrite, CallBack);
  end;
end;

//== TJvHidDeviceController ====================================================

// internal worker function to find all Hid devices and create their objects

procedure TJvHidDeviceController.FillInList(var List: TList);
var
  PnPHandle:               HDEVINFO;
  DevData:                 TSPDevInfoData;
  DeviceInterfaceData:     TSPDeviceInterfaceData;
  FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
  Success:                 LongBool;
  Devn:                    Integer;
  BytesReturned:           DWORD;
  HidDev:                  TJvHidDevice;
  PnPInfo:                 TJvPnPInfo;
begin
  // create list
  List := TList.Create;

  if not IsHidLoaded then
    Exit;

  // Get a handle for the Plug and Play node and request currently active HID devices
  PnPHandle := SetupDiGetClassDevs(@FHidGuid, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if PnPHandle = Pointer(INVALID_HANDLE_VALUE) then
    Exit;
  Devn := 0;
  repeat
    DeviceInterfaceData.cbSize := SizeOf(TSPDeviceInterfaceData);
    // Is there a HID device at this table entry?
    Success := SetupDiEnumDeviceInterfaces(PnPHandle, nil, FHidGuid, Devn, DeviceInterfaceData);
    if Success then
    begin
      DevData.cbSize := SizeOf(DevData);
      BytesReturned  := 0;
      SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, nil, 0, BytesReturned, @DevData);
      if (BytesReturned <> 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        FunctionClassDeviceData := AllocMem(BytesReturned);
        FunctionClassDeviceData.cbSize := 5;
        if SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, FunctionClassDeviceData, BytesReturned, BytesReturned, @DevData) then
        begin
          // fill in PnPInfo of device
          with PnPInfo do
          begin
            DeviceID        := DevData.DevInst;
            DevicePath      := PChar(@FunctionClassDeviceData.DevicePath);

            Capabilities    := GetRegistryPropertyDWord (PnPHandle, DevData, SPDRP_CAPABILITIES);
            ClassDescr      := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CLASS);
            ClassGUID       := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CLASSGUID);
            CompatibleIDs   := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_COMPATIBLEIDS);
            ConfigFlags     := GetRegistryPropertyDWord (PnPHandle, DevData, SPDRP_CONFIGFLAGS);
            DeviceDescr     := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DEVICEDESC);
            Driver          := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DRIVER);
            FriendlyName    := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_FRIENDLYNAME);
            HardwareID      := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_HARDWAREID);
            LowerFilters    := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_LOWERFILTERS);
            Mfg             := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_MFG);
            UpperFilters    := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_UPPERFILTERS);

            Address         := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_ADDRESS);
            BusNumber       := GetRegistryPropertyDWord (PnPHandle, DevData, SPDRP_BUSNUMBER);
            BusType         := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_BUSTYPEGUID);
            Characteristics := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CHARACTERISTICS);
            DevType         := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DEVTYPE);
            EnumeratorName  := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_ENUMERATOR_NAME);
            Exclusive       := GetRegistryPropertyDWord (PnPHandle, DevData, SPDRP_EXCLUSIVE);
            LegacyBusType   := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_LEGACYBUSTYPE);
            LocationInfo    := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_LOCATION_INFORMATION);
            PhysDevObjName  := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_PHYSICAL_DEVICE_OBJECT_NAME);
            Security        := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_SECURITY);
            Service         := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_SERVICE);
            UINumber        := GetRegistryPropertyDWord (PnPHandle, DevData, SPDRP_UI_NUMBER);
            UINumberFormat  := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_UI_NUMBER_DESC_FORMAT);
          end;
          // create HID device object and add it to the device list
          HidDev := TJvHidDevice.CtlCreate(PnPInfo, Self);
          List.Add(HidDev);
          Inc(Devn);
        end;
        FreeMem(FunctionClassDeviceData);
      end;
    end;
  until not Success;
  SetupDiDestroyDeviceInfoList(PnPHandle);
end;

//------------------------------------------------------------------------------

// the controller fills its list on creation and connects the event pipe

constructor TJvHidDeviceController.Create(AOwner: TComponent);
const
  cHidGuid: TGUID = '{4d1e55b2-f16f-11cf-88cb-001111000030}';
begin
  inherited Create(AOwner);
  FDeviceChangeEvent    := nil;
  FEnumerateEvent       := nil;
  FDevUnplugEvent       := nil;
  FList                 := nil;
  FDeviceChangeFired    := False;
  FNumCheckedInDevices  := 0;
  FNumCheckedOutDevices := 0;
  FNumUnpluggedDevices  := 0;
  if LoadSetupApi then
    LoadHid;
  if IsHidLoaded then
    HidD_GetHidGuid(FHidGuid)
  else
    FHidGuid := cHidGuid;

  // this is just to remind you that one controller is sufficient
  Inc(GlobalInstanceCount);
  if GlobalInstanceCount > 1 then
    raise EControllerError.Create('Only one TJvHidDeviceController allowed per program');

  FillInList(FList);
  FNumCheckedInDevices := FList.Count;
  if IsHidLoaded then
    // only hook messages if there is a HID DLL
    Application.HookMainWindow(EventPipe);
end;

//------------------------------------------------------------------------------

// implement OnDeviceChange event

procedure TJvHidDeviceController.DoDeviceChange;
begin
  if Assigned(FDeviceChangeEvent) then
    FDeviceChangeEvent(Self);
end;

//------------------------------------------------------------------------------

// unplug or kill all controlled TJvHidDevices on controller destruction

destructor TJvHidDeviceController.Destroy;
var
  I:      Integer;
  HidDev: TJvHidDevice;
begin
  Dec(GlobalInstanceCount);
  // to prevent strange problems
  FDeviceChangeEvent := nil;
  FDevUnplugEvent    := nil;
  OnEnumerate        := nil;
  // unhook event pipe
  if IsHidLoaded then
    Application.UnhookMainWindow(EventPipe);

  if Assigned(FList) then
    for I := 0 to FList.Count - 1 do
    begin
      HidDev := FList.Items[I];
      with HidDev do
      begin
        // set to uncontrolled
        FMyController := nil;
        if IsCheckedOut then
          DoUnplug // pull the plug for checked out TJvHidDevices
        else
          Free;    // kill TJvHidDevices which are not checked out
      end;
    end;
  FList.Free;

  UnloadHid;
  UnloadSetupApi;

  inherited Destroy;
end;

//------------------------------------------------------------------------------

// gets all the Windows events/messages directly

function TJvHidDeviceController.EventPipe(var Msg: TMessage): Boolean;
begin
  Result := False;
  // sort out WM_DEVICECHANGE : DBT_DEVNODES_CHANGED
  if (Msg.Msg = WM_DEVICECHANGE) and (TWMDeviceChange(Msg).Event = DBT_DEVNODES_CHANGED) then
    DeviceChange;
end;

//------------------------------------------------------------------------------

// implements OnDeviceChange event
// it is published to allow calling at design time

procedure TJvHidDeviceController.DeviceChange;
var
  I:       Integer;
  J:       Integer;
  NewList: TList;
  HidDev:  TJvHidDevice;
  Changed: Boolean;
begin
  Changed := False;
  // get new device list
  FillInList(NewList);

  // unplug devices in FList which are not in NewList
  for I := FList.Count - 1 downto 0 do
  begin
    HidDev := FList.Items[I];
    for J := NewList.Count - 1 downto 0 do
      if (TJvHidDevice(NewList.Items[J]).PnPInfo.DeviceID = HidDev.PnPInfo.DeviceID) and HidDev.IsPluggedIn then
      begin
        HidDev := nil;
        Break;
      end;
    if HidDev <> nil then
    begin
      HidDev.DoUnplug;
      // delete from list
      if not HidDev.IsCheckedOut then
        FList.Delete(I);
      Changed := True;
    end;
  end;

  // delete devices from NewList which are in FList
  for I := 0 to NewList.Count - 1 do
    for J := 0 to FList.Count - 1 do
      if (TJvHidDevice(NewList[I]).PnPInfo.DeviceID = TJvHidDevice(FList[J]).PnPInfo.DeviceID) and TJvHidDevice(FList[J]).IsPluggedIn then
      begin
        TJvHidDevice(NewList[I]).FMyController := nil; // prevent Free/Destroy from accessing this controller
        TJvHidDevice(NewList[I]).Free;
        NewList[I] := nil;
        Break;
      end;

  // add the remains in NewList to FList
  for I := 0 to NewList.Count - 1 do
    if NewList[I] <> nil then
    begin
      FList.Add(NewList[I]);
      Changed := True;
    end;

  // throw away helper list
  NewList.Free;

  // recount the devices
  FNumCheckedInDevices  := 0;
  FNumCheckedOutDevices := 0;
  FNumUnpluggedDevices  := 0;
  for I := 0 to FList.Count - 1 do
  begin
    HidDev := FList.Items[I];
    Inc(FNumCheckedInDevices,  Ord(not HidDev.IsCheckedOut));
    Inc(FNumCheckedOutDevices, Ord(HidDev.IsCheckedOut));
    Inc(FNumUnpluggedDevices,  Ord(not HidDev.IsPluggedIn));
  end;
  FNumCheckedOutDevices := FNumCheckedOutDevices - FNumUnpluggedDevices;

  if Changed or not FDeviceChangeFired then
    DoDeviceChange;
end;

//------------------------------------------------------------------------------

class function TJvHidDeviceController.HidVersion: string;
var
  Dummy: DWORD;
  Size: UINT;
  Buf: string;
  Value: PChar;
begin
  Result := '';
  Size := GetFileVersionInfoSize(HidModuleName, Dummy);
  if Size > 0 then
  begin
    SetLength(Buf, Size);
    GetFileVersionInfo(HidModuleName, INVALID_HANDLE_VALUE, Size, PChar(Buf));
    if VerQueryValue(PChar(Buf), 'StringFileInfo\040904E4\FileVersion', Pointer(Value), Size) then
      Result := Value;
  end;
end;

//------------------------------------------------------------------------------

// assign OnDeviceChange and immediately fire if needed

procedure TJvHidDeviceController.SetDeviceChangeEvent(const Notifier: TNotifyEvent);
begin
  if @FDeviceChangeEvent <> @Notifier then
  begin
    FDeviceChangeEvent := Notifier;
    DeviceChange;
    FDeviceChangeFired := True;
  end;
end;

//------------------------------------------------------------------------------

// implement OnEnumerate event

function TJvHidDeviceController.DoEnumerate(HidDev: TJvHidDevice; Idx: Integer): Boolean;
begin
  Result := False;
  if Assigned(FEnumerateEvent) then
  begin
    HidDev.FIsEnumerated := True;
    Result := FEnumerateEvent(HidDev, Idx);
    HidDev.FIsEnumerated := False;
    if not HidDev.IsCheckedOut then
    begin
      HidDev.CloseFile;
      HidDev.CloseFileEx(omRead);
      HidDev.CloseFileEx(omWrite);
    end;
  end;
end;

//------------------------------------------------------------------------------

// assign OnEnumerate event

procedure TJvHidDeviceController.SetEnumerate(const Enumerator: TJvHidEnumerateEvent);
begin
  FEnumerateEvent := Enumerator;
end;

//------------------------------------------------------------------------------

// assign OnDevUnplug event

procedure TJvHidDeviceController.SetDevUnplug(const Unplugger: TJvHidUnplugEvent);
var
   I:   Integer;
   Dev: TJvHidDevice;
begin
  if @Unplugger <> @FDevUnplugEvent then
  begin
    // change all OnUnplug events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if @Dev.FUnplug = @FDevUnplugEvent then
        Dev.OnUnplug := Unplugger;
    end;
    FDevUnplugEvent := Unplugger;
  end;
end;

//------------------------------------------------------------------------------

// send an OnEnumerate event for all plugged HidDevices
// it is explicitly allowed to check out any device in the event

function TJvHidDeviceController.Enumerate: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn then
    begin
      Inc(Result);
      if not DoEnumerate(FList[I], I) then
        Break;
    end;
end;

//-- TJvHidDeviceController methods --------------------------------------------

// internal worker function to check out a TJvHidDevice

function TJvHidDeviceController.CheckThisOut(var HidDev: TJvHidDevice; Idx: Integer; Check: Boolean): Boolean;
begin
  Result := Check and not TJvHidDevice(FList.Items[Idx]).IsCheckedOut;
  if Result then
  begin
    HidDev := FList[Idx];
    HidDev.FIsCheckedOut := True;
    Inc(FNumCheckedOutDevices);
    Dec(FNumCheckedInDevices);
  end;
end;

//------------------------------------------------------------------------------

// method CheckOutByProductName hands out the first HidDevice with a matching ProductName

function TJvHidDeviceController.CheckOutByProductName(var HidDev: TJvHidDevice; const ProductName: WideString): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  if ProductName <> '' then
    for I := 0 to FList.Count - 1 do
    begin
      Result := CheckThisOut(HidDev, I, ProductName = TJvHidDevice(FList[I]).ProductName);
      if Result then
        Break;
    end;
end;

//------------------------------------------------------------------------------

// method CheckOutByVendorName hands out the first HidDevice with a matching VendorName

function TJvHidDeviceController.CheckOutByVendorName(var HidDev: TJvHidDevice; const VendorName: WideString): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  if VendorName <> '' then
    for I := 0 to FList.Count - 1 do
    begin
      Result := CheckThisOut(HidDev, I, VendorName = TJvHidDevice(FList[I]).VendorName);
      if Result then
        Break;
    end;
end;

//------------------------------------------------------------------------------

// method CheckOutByClass hands out the first HidDevice with a matching Class
// Class comes from the registry (examples: 'Mouse', 'Keyboard')

function TJvHidDeviceController.CheckOutByClass(var HidDev: TJvHidDevice; const ClassName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  if ClassName <> '' then
    for I := 0 to FList.Count - 1 do
    begin
      Result := CheckThisOut(HidDev, I, ClassName = TJvHidDevice(FList[I]).PnPInfo.ClassDescr);
      if Result then
        Break;
    end;
end;

//------------------------------------------------------------------------------

// method CheckOutByID hands out the first HidDevice with a matching VendorID and ProductID
// Pid = -1 matches all ProductIDs

function TJvHidDeviceController.CheckOutByID(var HidDev: TJvHidDevice; const Vid, Pid: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Result := CheckThisOut(HidDev, I, (Vid = TJvHidDevice(FList[I]).Attributes.VendorID) and
      ((Pid = TJvHidDevice(FList[I]).Attributes.ProductID) or (Pid = -1)));
    if Result then
      Break;
  end;
end;

//------------------------------------------------------------------------------

// method CheckOutByIndex hands out the HidDevice in the list with the named index
// this is mainly for check out during OnEnumerate

function TJvHidDeviceController.CheckOutByIndex(var HidDev: TJvHidDevice; const Idx: Integer): Boolean;
begin
  Result := False;
  HidDev := nil;
  if (Idx >= 0) and (Idx < FList.Count) then
    Result := CheckThisOut(HidDev, Idx, True);
end;

//------------------------------------------------------------------------------

// method CheckOut simply hands out the first available HidDevice in the list

function TJvHidDeviceController.CheckOut(var HidDev: TJvHidDevice): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Result := CheckThisOut(HidDev, I, True);
    if Result then
      Break;
  end;
end;

//------------------------------------------------------------------------------

// method CheckIn hands a checked out HidDevice back in

procedure TJvHidDeviceController.CheckIn(var HidDev: TJvHidDevice);
begin
  if HidDev <> nil then
  begin
    HidDev.CloseFile;
    HidDev.CloseFileEx(omRead);
    HidDev.CloseFileEx(omWrite);

    if HidDev.IsPluggedIn then
    begin
      HidDev.FIsCheckedOut := False;
      Dec(FNumCheckedOutDevices);
      Inc(FNumCheckedInDevices);
    end
    else
      HidDev.Free;
    HidDev := nil;
  end;
end;

//------------------------------------------------------------------------------

function TJvHidDeviceController.CountByClass(const ClassName: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and (ClassName = TJvHidDevice(FList[I]).PnPInfo.ClassDescr) then
      Inc(Result);
end;

//------------------------------------------------------------------------------

function TJvHidDeviceController.CountByID(const Vid, Pid: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and
       (Vid = TJvHidDevice(FList[I]).Attributes.VendorID) and
       ((Pid = TJvHidDevice(FList[I]).Attributes.ProductID) or (Pid = -1)) then
      Inc(Result);
end;

//------------------------------------------------------------------------------

function TJvHidDeviceController.CountByProductName(const ProductName: WideString): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and (ProductName = TJvHidDevice(FList[I]).ProductName) then
      Inc(Result);
end;

//------------------------------------------------------------------------------

function TJvHidDeviceController.CountByVendorName(const VendorName: WideString): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and (VendorName = TJvHidDevice(FList[I]).VendorName) then
      Inc(Result);
end;

//------------------------------------------------------------------------------

// a helper function to check the return values just
// like Win32Check
// the functions return the parameter to be transparent

function HidCheck(const RetVal: NTSTATUS): NTSTATUS;
begin
  if RetVal <> HIDP_STATUS_SUCCESS then
    HidError(RetVal);
  Result := RetVal;
end;

//------------------------------------------------------------------------------

function HidCheck(const RetVal: LongBool): LongBool;
begin
  if not RetVal then
    raise EHidClientError.Create('HidClient Error: a boolean function failed');
  Result := RetVal;
end;

//------------------------------------------------------------------------------

function HidError(const RetVal: NTSTATUS): NTSTATUS;
var
  ErrBuf: string;
begin
  // only check HID errors
  if ((RetVal and NTSTATUS($00FF0000)) = HIDP_STATUS_SUCCESS) and
     ((RetVal and NTSTATUS($C0000000)) <> 0) then
  begin
    case RetVal of
      HIDP_STATUS_NULL:                    ErrBuf := 'device not plugged in';
      HIDP_STATUS_INVALID_PREPARSED_DATA:  ErrBuf := 'invalid preparsed data';
      HIDP_STATUS_INVALID_REPORT_TYPE:     ErrBuf := 'invalid report type';
      HIDP_STATUS_INVALID_REPORT_LENGTH:   ErrBuf := 'invalid report length';
      HIDP_STATUS_USAGE_NOT_FOUND:         ErrBuf := 'usage not found';
      HIDP_STATUS_VALUE_OUT_OF_RANGE:      ErrBuf := 'value out of range';
      HIDP_STATUS_BAD_LOG_PHY_VALUES:      ErrBuf := 'bad logical or physical values';
      HIDP_STATUS_BUFFER_TOO_SMALL:        ErrBuf := 'buffer too small';
      HIDP_STATUS_INTERNAL_ERROR:          ErrBuf := 'internal error';
      HIDP_STATUS_I8042_TRANS_UNKNOWN:     ErrBuf := '8042 key translation impossible';
      HIDP_STATUS_INCOMPATIBLE_REPORT_ID:  ErrBuf := 'incompatible report ID';
      HIDP_STATUS_NOT_VALUE_ARRAY:         ErrBuf := 'not a value array';
      HIDP_STATUS_IS_VALUE_ARRAY:          ErrBuf := 'is a value array';
      HIDP_STATUS_DATA_INDEX_NOT_FOUND:    ErrBuf := 'data index not found';
      HIDP_STATUS_DATA_INDEX_OUT_OF_RANGE: ErrBuf := 'data index out of range';
      HIDP_STATUS_BUTTON_NOT_PRESSED:      ErrBuf := 'button not pressed';
      HIDP_STATUS_REPORT_DOES_NOT_EXIST:   ErrBuf := 'report does not exist';
      HIDP_STATUS_NOT_IMPLEMENTED:         ErrBuf := 'not implemented';
    else
      ErrBuf := Format('unknown HID error %x', [RetVal]);
    end;
    raise EHidClientError.Create('HidClient Error: ' + ErrBuf);
  end;
  Result := RetVal;
end;

end.
