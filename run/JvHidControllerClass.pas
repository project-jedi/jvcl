{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHidControllerClass.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Robert Marquardt [robert_marquardt@gmx.de]
Portions created by Robert Marquardt are Copyright (C) 1999-2003 Robert Marquardt.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-02-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  I do not clean the source style for this file ;-)
-----------------------------------------------------------------------------}

{$I JVCL.INC}

// (rom) switch to make this component independent of JVCL
{.DEFINE STANDALONE}

unit JvHidControllerClass;

interface

uses
  Windows, Messages, Classes, Forms, SysUtils,
  {$IFDEF STANDALONE}
  DBT, SetupApi, Hid, ModuleLoader;
  {$ELSE}
  DBT, SetupApi, Hid, ModuleLoader,
  JvComponent;
  {$ENDIF STANDALONE}

const
  // a version string for the component
  cHidControllerClassVersion = '1.0.10';

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
  TJvHidOpenExMode = (omhRead, omhWrite);

  // the physical descriptor
  TJvPhysicalDescriptor = array of WORD;

  // all USB relevant driver entries in the registry
  TJvHidPnPInfo = class(TObject)
    FDeviceID: DWORD;
    FDevicePath: string;
    FCapabilities: DWORD;
    FClassDescr: string;
    FClassGUID: string;
    FCompatibleIDs: TStringList;
    FConfigFlags: DWORD;
    FDeviceDescr: string;
    FDriver: string;
    FFriendlyName: string;
    FHardwareID: TStringList;
    FLowerFilters: TStringList;
    FMfg: string;
    FUpperFilters: TStringList;
    FAddress: string;
    FBusNumber: DWORD;
    FBusType: string;
    FCharacteristics: string;
    FDevType: string;
    FEnumeratorName: string;
    FExclusive: DWORD;
    FLegacyBusType: string;
    FLocationInfo: string;
    FPhysDevObjName: string;
    FSecurity: string;
    FService: string;
    FUINumber: DWORD;
    FUINumberFormat: string;
    function GetRegistryPropertyString(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): string;
    function GetRegistryPropertyStringList(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): TStringList;
    function GetRegistryPropertyDWord(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
  public
    property DeviceID:        DWORD       read FDeviceID;
    property DevicePath:      string      read FDevicePath;
    // registry values
    property Capabilities:    DWORD       read FCapabilities;
    property ClassDescr:      string      read FClassDescr;
    property ClassGUID:       string      read FClassGUID;
    property CompatibleIDs:   TStringList read FCompatibleIDs;
    property ConfigFlags:     DWORD       read FConfigFlags;
    property DeviceDescr:     string      read FDeviceDescr;
    property Driver:          string      read FDriver;
    property FriendlyName:    string      read FFriendlyName;
    property HardwareID:      TStringList read FHardwareID;
    property LowerFilters:    TStringList read FLowerFilters;
    property Mfg:             string      read FMfg;
    property UpperFilters:    TStringList read FUpperFilters;
    property Address:         string      read FAddress;
    property BusNumber:       DWORD       read FBusNumber;
    property BusType:         string      read FBusType;
    property Characteristics: string      read FCharacteristics;
    property DevType:         string      read FDevType;
    property EnumeratorName:  string      read FEnumeratorName;
    property Exclusive:       DWORD       read FExclusive;
    property LegacyBusType:   string      read FLegacyBusType;
    property LocationInfo:    string      read FLocationInfo;
    property PhysDevObjName:  string      read FPhysDevObjName;
    property Security:        string      read FSecurity;
    property Service:         string      read FService;
    property UINumber:        DWORD       read FUINumber;
    property UINumberFormat:  string      read FUINumberFormat;
    constructor Create(APnPHandle: HDEVINFO; ADevData: TSPDevInfoData; ADevicePath: PChar);
    destructor Destroy; override;
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
    FPnPInfo:              TJvHidPnPInfo;
    FVendorName:           WideString;
    FProductName:          WideString;
    FPhysicalDescriptor:   TJvPhysicalDescriptor;
    FPreparsedData:        PHIDPPreparsedData;
    FSerialNumber:         WideString;
    FLanguageStrings:      TStringList;
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
    FHasReadWriteAccess:   Boolean;

    // tells if access to device is allowed
    function  IsAccessible: Boolean;
    procedure GetMax;

    // internal property implementors
    function  GetDeviceStringAnsi   (Idx: Byte): string;
    function  GetDeviceStringUnicode(Idx: Byte): WideString;
    function  GetLinkCollectionNode (Idx: WORD): THIDPLinkCollectionNode;
    function  GetConfiguration:         THIDDConfiguration;
    function  GetPreparsedData:         PHIDPPreparsedData;
    function  GetCaps:                  THIDPCaps;
    function  GetVendorName:            WideString;
    function  GetProductName:           WideString;
    function  GetSerialNumber:          WideString;
    function  GetPhysicalDescriptor:    TJvPhysicalDescriptor;
    function  GetLanguageStrings:       TStringList;
    function  GetOverlappedReadResult:  DWORD;
    function  GetOverlappedWriteResult: DWORD;
    procedure SetConfiguration       (const Config: THIDDConfiguration);
    procedure SetNumInputBuffers     (const Num: Integer);
    procedure SetNumOverlappedBuffers(const Num: Integer);
    procedure SetReportTypeParam     (const ReportType: THIDPReportType);
    procedure SetUsagePageParam      (const UsagePage: TUsage);

    // Constructor is hidden! Only a TJvHidDeviceController can create a TJvHidDevice object.
    constructor CtlCreate(const APnPInfo: TJvHidPnPInfo;
                          const Controller: TJvHidDeviceController);

  protected
    // internal event implementors
    procedure DoUnplug;
    procedure SetUnplug(const Event: TJvHidUnplugEvent);

  public
    // dummy constructor
    constructor Create;
    destructor Destroy; override;

    // read only properties
    property Attributes:               THIDDAttributes       read FAttributes;
    property Caps:                     THIDPCaps             read GetCaps;
    property HasReadWriteAccess:       Boolean               read FHasReadWriteAccess;
    property HidFileHandle:            THandle               read FHidFileHandle;
    property HidOverlappedRead:        THandle               read FHidOverlappedRead;
    property HidOverlappedWrite:       THandle               read FHidOverlappedWrite;
    property HidOverlappedReadResult:  DWORD                 read GetOverlappedReadResult;
    property HidOverlappedWriteResult: DWORD                 read GetOverlappedWriteResult;
    property IsCheckedOut:             Boolean               read FIsCheckedOut;
    property IsPluggedIn:              Boolean               read FIsPluggedIn;
    property LanguageStrings:          TStringList           read GetLanguageStrings;
    property MaxButtonListLength:      ULONG                 read FMaxButtonListLength;
    property MaxDataListLength:        ULONG                 read FMaxDataListLength;
    property MaxUsageListLength:       ULONG                 read FMaxUsageListLength;
    property PhysicalDescriptor:       TJvPhysicalDescriptor read GetPhysicalDescriptor;
    property PnPInfo:                  TJvHidPnPInfo         read FPnPInfo;
    property PreparsedData:            PHIDPPreparsedData    read GetPreparsedData;
    property ProductName:              WideString            read GetProductName;
    property SerialNumber:             WideString            read GetSerialNumber;
    property VendorName:               WideString            read GetVendorName;
    // read write properties
    property Configuration:        THIDDConfiguration read GetConfiguration      write SetConfiguration;
    property LinkCollectionParam:  WORD               read FLinkCollectionParam  write FLinkCollectionParam;
    property NumInputBuffers:      Integer            read FNumInputBuffers      write SetNumInputBuffers;
    property NumOverlappedBuffers: Integer            read FNumOverlappedBuffers write SetNumOverlappedBuffers;
    property ReportTypeParam:      THIDPReportType    read FReportTypeParam      write SetReportTypeParam;
    property UsagePageParam:       TUsage             read FUsagePageParam       write SetUsagePageParam;
    property UsageParam:           TUsage             read FUsageParam           write FUsageParam;
    // indexed properties
    property DeviceStrings       [Idx: Byte]: string                  read GetDeviceStringAnsi;
    property DeviceStringsUnicode[Idx: Byte]: WideString              read GetDeviceStringUnicode;
    property LinkCollectionNodes [Idx: WORD]: THIDPLinkCollectionNode read GetLinkCollectionNode;
    // the only event property
    property OnUnplug:             TJvHidUnplugEvent read FUnplug write SetUnplug;

    // methods
    function  CancelIO             (const Mode: TJvHidOpenExMode): Boolean;
    procedure CloseFile;
    procedure CloseFileEx          (const Mode: TJvHidOpenExMode);
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
    function  OpenFileEx           (Mode: TJvHidOpenExMode):                        Boolean;
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
  end;

  // controller class to manage all HID devices

  {$IFDEF STANDALONE}
  TJvHidDeviceController = class(TComponent)
  {$ELSE}
  TJvHidDeviceController = class(TJvComponent)
  {$ENDIF STANDALONE}
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
    // methods to count HID device objects
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
function HidErrorString(const RetVal: NTSTATUS): string;

{$IFDEF STANDALONE}

// to register the component in the palette
procedure Register;

{$ENDIF}

implementation

{$IFDEF STANDALONE}

type
  EControllerError = class(Exception);
  EHidClientError  = class(Exception);

{$ELSE}

uses
  JvTypes;

type
  EControllerError = class(EJVCLException);
  EHidClientError  = class(EJVCLException);

{$ENDIF STANDALONE}

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

//== TJvHidPnPInfo =============================================================

constructor TJvHidPnPInfo.Create(APnPHandle: HDEVINFO; ADevData: TSPDevInfoData; ADevicePath: PChar);
begin
  FDeviceID   := ADevData.DevInst;
  FDevicePath := ADevicePath;

  FCapabilities    := GetRegistryPropertyDWord     (APnPHandle, ADevData, SPDRP_CAPABILITIES);
  FClassDescr      := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_CLASS);
  FClassGUID       := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_CLASSGUID);
  FCompatibleIDs   := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_COMPATIBLEIDS);
  FConfigFlags     := GetRegistryPropertyDWord     (APnPHandle, ADevData, SPDRP_CONFIGFLAGS);
  FDeviceDescr     := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_DEVICEDESC);
  FDriver          := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_DRIVER);
  FFriendlyName    := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_FRIENDLYNAME);
  FHardwareID      := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_HARDWAREID);
  FLowerFilters    := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_LOWERFILTERS);
  FMfg             := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_MFG);
  FUpperFilters    := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_UPPERFILTERS);

  FAddress         := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_ADDRESS);
  FBusNumber       := GetRegistryPropertyDWord     (APnPHandle, ADevData, SPDRP_BUSNUMBER);
  FBusType         := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_BUSTYPEGUID);
  FCharacteristics := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_CHARACTERISTICS);
  FDevType         := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_DEVTYPE);
  FEnumeratorName  := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_ENUMERATOR_NAME);
  FExclusive       := GetRegistryPropertyDWord     (APnPHandle, ADevData, SPDRP_EXCLUSIVE);
  FLegacyBusType   := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_LEGACYBUSTYPE);
  FLocationInfo    := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_LOCATION_INFORMATION);
  FPhysDevObjName  := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_PHYSICAL_DEVICE_OBJECT_NAME);
  FSecurity        := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_SECURITY);
  FService         := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_SERVICE);
  FUINumber        := GetRegistryPropertyDWord     (APnPHandle, ADevData, SPDRP_UI_NUMBER);
  FUINumberFormat  := GetRegistryPropertyString    (APnPHandle, ADevData, SPDRP_UI_NUMBER_DESC_FORMAT);
end;

//------------------------------------------------------------------------------

destructor TJvHidPnPInfo.Destroy;
begin
  FCompatibleIDs.Free;
  FHardwareID.Free;
  FLowerFilters.Free;
  FUpperFilters.Free;
end;

//------------------------------------------------------------------------------

// internal helpers to read values from a devices registry area

function TJvHidPnPInfo.GetRegistryPropertyString(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): string;
var
  BytesReturned: DWORD;
  RegDataType:   DWORD;
  Buffer:        array [0..256] of Char;
begin
  BytesReturned := 0;
  RegDataType   := 0;
  Buffer[0]     := #0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, PBYTE(@Buffer[0]), SizeOf(Buffer), BytesReturned);
  Result := Buffer;
end;

//------------------------------------------------------------------------------

function TJvHidPnPInfo.GetRegistryPropertyStringList(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): TStringList;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
  Buffer: array[0..16383] of Char;
  P: PChar;
begin
  BytesReturned := 0;
  RegDataType   := 0;
  Buffer[0]     := #0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, PBYTE(@Buffer[0]), SizeOf(Buffer), BytesReturned);
  Result := TStringList.Create;
  P := @Buffer[0];
  while P[0] <> #0 do
  begin
    Result.Add(P);
    P := P + StrLen(P) + 1;
  end;
end;

//------------------------------------------------------------------------------

function TJvHidPnPInfo.GetRegistryPropertyDWord(PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
var
  BytesReturned: DWORD;
  RegDataType:   DWORD;
begin
  BytesReturned := 0;
  RegDataType   := 0;
  Result        := 0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, PBYTE(@Result), SizeOf(Result), BytesReturned);
end;

//== TJvHidDevice ==============================================================

// dummy constructor to catch invalid Creates

constructor TJvHidDevice.Create;
begin
  FHidFileHandle      := INVALID_HANDLE_VALUE;
  FHidOverlappedRead  := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite := INVALID_HANDLE_VALUE;
  raise EControllerError.Create('Direct creation of a TJvHidDevice object is not allowed');
end;

//------------------------------------------------------------------------------

// create and fill in a HidDevice object
// the constructor is only accessible from TJvHidController
// PnPInfo contains all info the JvHidDeviceController collected
// Controller is the devices controller object to be referenced
// internally

constructor TJvHidDevice.CtlCreate(const APnPInfo: TJvHidPnPInfo; const Controller: TJvHidDeviceController);
begin
  inherited Create;

  // initialize private data
  FPnPInfo              := APnPInfo;
  FMyController         := Controller;
  FIsPluggedIn          := True;
  FIsCheckedOut         := False;
  FIsEnumerated         := False;
  FHidOverlappedRead    := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite   := INVALID_HANDLE_VALUE;
  FVendorName           := '';
  FProductName          := '';
  FPreparsedData        := nil;
  SetLength(FPhysicalDescriptor, 0);
  FSerialNumber         := '';
  FLanguageStrings      := TStringList.Create;
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

  FHidFileHandle := CreateFile(PChar(PnPInfo.DevicePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  FHasReadWriteAccess := HidFileHandle <> INVALID_HANDLE_VALUE;
  // Win2000 hack
  if not HasReadWriteAccess then
    FHidFileHandle := CreateFile(PChar(PnPInfo.DevicePath), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if HidFileHandle <> INVALID_HANDLE_VALUE then
  begin
    FAttributes.Size := SizeOf(THIDDAttributes);
    if not HidD_GetAttributes(HidFileHandle, FAttributes) then
      raise EControllerError.Create('device cannot be identified');
  end
  else
    raise EControllerError.Create('device cannot be opened');
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
  CloseFileEx(omhRead);
  CloseFileEx(omhWrite);
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
            FPnPInfo := nil;
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

  PnPInfo.Free;
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
      FHidFileHandle := CreateFile(PChar(PnPInfo.DevicePath), GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      FHasReadWriteAccess := HidFileHandle <> INVALID_HANDLE_VALUE;
      // Win2000 hack
      if not HasReadWriteAccess then
        FHidFileHandle := CreateFile(PChar(PnPInfo.DevicePath), 0,
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

function TJvHidDevice.OpenFileEx(Mode: TJvHidOpenExMode): Boolean;
begin
  Result := False;
  // check if open allowed (propagates this state)
  if IsAccessible then
    if Mode = omhRead then
    begin
      if HidOverlappedRead = INVALID_HANDLE_VALUE then // if not already opened
      begin
        FHidOverlappedRead := CreateFile(PChar(PnPInfo.DevicePath), GENERIC_READ,
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
        FHidOverlappedWrite := CreateFile(PChar(PnPInfo.DevicePath), GENERIC_WRITE,
          FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
      Result := FHidOverlappedWrite <> INVALID_HANDLE_VALUE;
    end;
end;

//------------------------------------------------------------------------------

// implement OnUnplug event

procedure TJvHidDevice.DoUnplug;
begin
  CloseFile;
  CloseFileEx(omhRead);
  CloseFileEx(omhWrite);
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
  Buffer: array [0..253] of WideChar;
begin
  Result := '';
  if Idx <> 0 then
    if OpenFile then
      if HidD_GetIndexedString(HidFileHandle, Idx, Buffer, SizeOf(Buffer)) then
        Result := Buffer;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetLinkCollectionNode(Idx: WORD): THIDPLinkCollectionNode;
var
  Siz: ULONG;
begin
  if Length(FLinkCollection) = 0 then
  begin
    Siz := Caps.NumberLinkCollectionNodes;
    SetLength(FLinkCollection, Siz);
    HidP_GetLinkCollectionNodes(@FLinkCollection[0], Siz, PreparsedData);
  end;
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
  if (Num <> FNumInputBuffers) and OpenFileEx(omhRead) then
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

function TJvHidDevice.GetPreparsedData: PHIDPPreparsedData;
begin
  if FPreparsedData = nil then
    if OpenFile then
    begin
      HidD_GetPreparsedData(HidFileHandle, FPreparsedData);
      CloseFile;
    end;
  Result := FPreparsedData;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetCaps: THIDPCaps;
begin
  FillChar(Result, SizeOf(THIDPCaps), #0);
  HidP_GetCaps(PreparsedData, Result);
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetVendorName: WideString;
var
  Buffer: array [0..253] of WideChar;
begin
  if FVendorName = '' then
    if OpenFile then
    begin
      FillChar(Buffer, SizeOf(Buffer), #0);
      if HidD_GetManufacturerString(HidFileHandle, Buffer, SizeOf(Buffer)) then
        FVendorName := Buffer;
      CloseFile;
    end;
  Result := FVendorName;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetProductName: WideString;
var
  Buffer: array [0..253] of WideChar;
begin
  if FProductName = '' then
    if OpenFile then
    begin
      FillChar(Buffer, SizeOf(Buffer), #0);
      if HidD_GetProductString(HidFileHandle, Buffer, SizeOf(Buffer)) then
        FProductName := Buffer;
      CloseFile;
    end;
  Result := FProductName;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetSerialNumber: WideString;
var
  I: Integer;
  Len: Integer;
  IDs:    array [0..253] of WORD;
  Buffer: array [0..253] of WideChar;
begin
  if FSerialNumber = '' then
    if OpenFile then
    begin
      FillChar(Buffer, SizeOf(Buffer), #0);
      if HidD_GetSerialNumberString(HidFileHandle, Buffer, SizeOf(Buffer)) then
      begin
        // calculate length of StringDescriptor 0
        FillChar(IDs, SizeOf(IDs), $FF);
        Len := 0;
        HidD_GetIndexedString(HidFileHandle, 0, PWideChar(@IDs), SizeOf(IDs));
        for I := High(IDs) downto 0 do
          if IDs[I] <> $FFFF then
          begin
            if IDs[I] = 0 then
              Len := I
            else
              Len := I+1;
            Break;
          end;
        // compensate for buggy function
        for I := 0 to Len - 1 do
          if IDs[I] <> WORD(Buffer[I]) then
          begin
            FSerialNumber := Buffer;
            Break;
          end;
      end;
      CloseFile;
    end;
  Result := FSerialNumber;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetPhysicalDescriptor: TJvPhysicalDescriptor;
var
  I: Integer;
begin
  if Length(FPhysicalDescriptor) = 0 then
    if OpenFile then
    begin
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
      CloseFile;
    end;
  Result := FPhysicalDescriptor;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetLanguageStrings: TStringList;
var
  I:    Integer;
  Len:  Integer;
  IDs:  array [0..253] of WORD;
  Name: array [0..255] of Char;
begin
  if FLanguageStrings.Count = 0 then
    if OpenFile then
    begin
      // calculate length of StringDescriptor 0
      FillChar(IDs, SizeOf(IDs), $FF);
      Len := 0;
      if HidD_GetIndexedString(HidFileHandle, 0, PWideChar(@IDs), SizeOf(IDs)) then
        for I := High(IDs) downto 0 do
          if IDs[I] <> $FFFF then
          begin
            if IDs[I] = 0 then
              Len := I
            else
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
      CloseFile;
    end;
  Result := FLanguageStrings;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetOverlappedReadResult: DWORD;
begin
  Result := 0;
  if HidOverlappedRead <> INVALID_HANDLE_VALUE then
    if not GetOverlappedResult(HidOverlappedRead, FOvlRead, Result, False) then
      Result := 0;
end;

//------------------------------------------------------------------------------

function TJvHidDevice.GetOverlappedWriteResult: DWORD;
begin
  Result := 0;
  if HidOverlappedWrite <> INVALID_HANDLE_VALUE then
    if not GetOverlappedResult(HidOverlappedWrite, FOvlWrite, Result, False) then
      Result := 0;
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

function TJvHidDevice.CancelIO(const Mode: TJvHidOpenExMode): Boolean;

  function CallCancelIO(Handle: THandle): Boolean;
  type
    TCancelIOFunc = function(hFile: THandle): BOOL; stdcall;
  var
    hKernel: TModuleHandle;
    CancelIOFunc: TCancelIOFunc;
  begin
    hKernel := INVALID_HANDLE_VALUE;
    Result := LoadModule(hKernel, 'Kernel32.dll');
    if Result then
    begin
      @CancelIOFunc := GetModuleSymbol(hKernel, 'CancelIO');
      if Assigned(CancelIOFunc) then
        Result := CancelIOFunc(Handle)
      else
        Result := False;
      UnloadModule(hKernel);
    end;
  end;

begin
  Result := False;
  if (Mode = omhRead) and (HidOverlappedRead <> INVALID_HANDLE_VALUE) then
    Result := CallCancelIO(HidOverlappedRead)
  else
  if (Mode = omhWrite) and (HidOverlappedWrite <> INVALID_HANDLE_VALUE) then
    Result := CallCancelIO(HidOverlappedWrite);
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

procedure TJvHidDevice.CloseFileEx(const Mode: TJvHidOpenExMode);
begin
  if Mode = omhRead then
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
  if OpenFileEx(omhRead) then
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
  if OpenFileEx(omhWrite) then
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
  PnPHandle: HDEVINFO;
  DevData: TSPDevInfoData;
  DeviceInterfaceData: TSPDeviceInterfaceData;
  FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
  Success: LongBool;
  Devn: Integer;
  BytesReturned: DWORD;
  HidDev: TJvHidDevice;
  PnPInfo: TJvHidPnPInfo;
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
          PnPInfo := TJvHidPnPInfo.Create(PnPHandle, DevData, PChar(@FunctionClassDeviceData.DevicePath));
          // create HID device object and add it to the device list
          try
            HidDev := TJvHidDevice.CtlCreate(PnPInfo, Self);
            List.Add(HidDev);
          except
            // ignore device if unreadable
          end;
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
  I: Integer;
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
      HidDev.CloseFileEx(omhRead);
      HidDev.CloseFileEx(omhWrite);
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
    HidDev.CloseFileEx(omhRead);
    HidDev.CloseFileEx(omhWrite);

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
    raise EHidClientError.Create('HID Error: a boolean function failed');
  Result := RetVal;
end;

//------------------------------------------------------------------------------

function HidError(const RetVal: NTSTATUS): NTSTATUS;
var
  ErrBuf: string;
begin
  ErrBuf := HidErrorString(RetVal);
  // only react to HID errors
  if ErrBuf <> '' then
    raise EHidClientError.Create(ErrBuf);
  Result := RetVal;
end;

//------------------------------------------------------------------------------

function HidErrorString(const RetVal: NTSTATUS): string;
begin
  Result := '';
  // only check HID errors
  if ((RetVal and NTSTATUS($00FF0000)) = HIDP_STATUS_SUCCESS) and
     ((RetVal and NTSTATUS($C0000000)) <> 0) then
  begin
    case RetVal of
      HIDP_STATUS_NULL:                    Result := 'device not plugged in';
      HIDP_STATUS_INVALID_PREPARSED_DATA:  Result := 'invalid preparsed data';
      HIDP_STATUS_INVALID_REPORT_TYPE:     Result := 'invalid report type';
      HIDP_STATUS_INVALID_REPORT_LENGTH:   Result := 'invalid report length';
      HIDP_STATUS_USAGE_NOT_FOUND:         Result := 'usage not found';
      HIDP_STATUS_VALUE_OUT_OF_RANGE:      Result := 'value out of range';
      HIDP_STATUS_BAD_LOG_PHY_VALUES:      Result := 'bad logical or physical values';
      HIDP_STATUS_BUFFER_TOO_SMALL:        Result := 'buffer too small';
      HIDP_STATUS_INTERNAL_ERROR:          Result := 'internal error';
      HIDP_STATUS_I8042_TRANS_UNKNOWN:     Result := '8042 key translation impossible';
      HIDP_STATUS_INCOMPATIBLE_REPORT_ID:  Result := 'incompatible report ID';
      HIDP_STATUS_NOT_VALUE_ARRAY:         Result := 'not a value array';
      HIDP_STATUS_IS_VALUE_ARRAY:          Result := 'is a value array';
      HIDP_STATUS_DATA_INDEX_NOT_FOUND:    Result := 'data index not found';
      HIDP_STATUS_DATA_INDEX_OUT_OF_RANGE: Result := 'data index out of range';
      HIDP_STATUS_BUTTON_NOT_PRESSED:      Result := 'button not pressed';
      HIDP_STATUS_REPORT_DOES_NOT_EXIST:   Result := 'report does not exist';
      HIDP_STATUS_NOT_IMPLEMENTED:         Result := 'not implemented';
    else
      Result := Format('unknown HID error %x', [RetVal]);
    end;
    Result := 'HID Error: ' + Result;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF STANDALONE}

// We place the component on the new 'Project JEDI' palette.
// This is to encourage you to become a member.
// Have a look at http://delphi-jedi.org for further details.

procedure Register;
begin
  RegisterComponents('Project JEDI', [TJvHidDeviceController]);
end;

{$ENDIF STANDALONE}

end.
