{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHidControllerClass.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Robert Marquardt [robert_marquardt att gmx dott de]
Portions created by Robert Marquardt are Copyright (C) 1999-2003 Robert Marquardt.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvHidControllerClass;

{$DEFINE DEFAULT_JVCL_INC}

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, SysUtils,
  JvComponentBase,
  DBT, JvSetupApi, Hid, JvTypes;

const
  // a version string for the component
  cHidControllerClassVersion = '1.0.35';

  // strings from the registry for CheckOutByClass
  cHidNoClass = 'HIDClass';

type
  // forward declarations
  TJvHidDeviceController = class;
  TJvHidDevice = class;
  TJvHidPnPInfo = class;

  // the Event function declarations
  TJvHidEnumerateEvent = function(HidDev: TJvHidDevice;
    const Idx: Integer): Boolean of object;
  TJvHidPlugEvent = procedure(HidDev: TJvHidDevice) of object;
  TJvHidUnplugEvent = TJvHidPlugEvent;
  TJvHidDataEvent = procedure(HidDev: TJvHidDevice; ReportID: Byte;
    const Data: Pointer; Size: Word) of object;
  TJvHidDataErrorEvent = procedure(HidDev: TJvHidDevice; Error: DWORD) of object;
  TJvHidDeviceCreateError = procedure(Controller: TJvHidDeviceController; PnPInfo: TJvHidPnPInfo; var Handled: Boolean; var RetryCreate: Boolean) of object;

  // check out test function
  TJvHidCheckCallback = function(HidDev: TJvHidDevice): Boolean; stdcall;

  // open overlapped read or write file handle
  TJvHidOpenExMode = (omhRead, omhWrite);

  // the physical descriptor
  TJvPhysicalDescriptor = array of WORD;

  // all USB relevant driver entries in the registry
  TJvHidPnPInfo = class(TObject)
  private
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
    FDevType: DWORD;
    FEnumeratorName: string;
    FExclusive: DWORD;
    FLegacyBusType: DWORD;
    FLocationInfo: string;
    FPhysDevObjName: string;
    FSecuritySDS: string;
    FService: string;
    FUINumber: DWORD;
    FUINumberFormat: string;
    function GetRegistryPropertyString(PnPHandle: HDEVINFO;
      const DevData: TSPDevInfoData; Prop: DWORD): string;
    function GetRegistryPropertyStringList(PnPHandle: HDEVINFO;
      const DevData: TSPDevInfoData; Prop: DWORD): TStringList;
    function GetRegistryPropertyDWord(PnPHandle: HDEVINFO;
      const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
    function GetRegistryPropertyGuid(PnPHandle: HDEVINFO;
      const DevData: TSPDevInfoData; Prop: DWORD): TGuid;
    function GetCompatibleIDs: TStrings;
    function GetHardwareID: TStrings;
    function GetLowerFilters: TStrings;
    function GetUpperFilters: TStrings;
  public
    property DeviceID: DWORD read FDeviceID;
    property DevicePath: string read FDevicePath;
    // registry values
    property Capabilities: DWORD read FCapabilities;
    property ClassDescr: string read FClassDescr;
    property ClassGUID: string read FClassGUID;
    property CompatibleIDs: TStrings read GetCompatibleIDs;
    property ConfigFlags: DWORD read FConfigFlags;
    property DeviceDescr: string read FDeviceDescr;
    property Driver: string read FDriver;
    property FriendlyName: string read FFriendlyName;
    property HardwareID: TStrings read GetHardwareID;
    property LowerFilters: TStrings read GetLowerFilters;
    property Mfg: string read FMfg;
    property UpperFilters: TStrings read GetUpperFilters;
    property Address: string read FAddress;
    property BusNumber: DWORD read FBusNumber;
    property BusType: string read FBusType;
    property Characteristics: string read FCharacteristics;
    property DevType: DWORD read FDevType;
    property EnumeratorName: string read FEnumeratorName;
    property Exclusive: DWORD read FExclusive;
    property LegacyBusType: DWORD read FLegacyBusType;
    property LocationInfo: string read FLocationInfo;
    property PhysDevObjName: string read FPhysDevObjName;
    property SecuritySDS: string read FSecuritySDS;
    property Service: string read FService;
    property UINumber: DWORD read FUINumber;
    property UINumberFormat: string read FUINumberFormat;
    constructor Create(APnPHandle: HDEVINFO; ADevData: TSPDevInfoData; const ADevicePath: string);
    destructor Destroy; override;
  end;

  // a thread helper class to implement TJvHidDevice.OnData

  TJvHidDeviceReadThread = class(TJvCustomThread)
  private
    FErr: DWORD;
    procedure DoData;
    procedure DoDataError;
    constructor CtlCreate(const Dev: TJvHidDevice);
  protected
    procedure Execute; override;
  public
    Device: TJvHidDevice;
    NumBytesRead: Cardinal;
    Report: array of Byte;
    constructor Create(CreateSuspended: Boolean);
  end;

  // the representation of a HID device

  TJvHidDevice = class(TObject)
  private
    // internal control variables
    FMyController: TJvHidDeviceController;
    FIsPluggedIn: Boolean;
    FIsCheckedOut: Boolean;
    FIsEnumerated: Boolean;
    FHidFileHandle: THandle;
    FHidOverlappedRead: THandle;
    FHidOverlappedWrite: THandle;
    FOvlRead: TOverlapped;
    FOvlWrite: TOverlapped;
    // internal properties part
    FAttributes: THIDDAttributes;
    FPnPInfo: TJvHidPnPInfo;
    {$IFDEF UNICODE}
    FVendorName: string;
    FProductName: string;
    FSerialNumber: string;
    {$ELSE}
    FVendorName: WideString;
    FProductName: WideString;
    FSerialNumber: WideString;
    {$ENDIF UNICODE}
    FPhysicalDescriptor: TJvPhysicalDescriptor;
    FPreparsedData: PHIDPPreparsedData;
    FLanguageStrings: TStringList;
    FNumInputBuffers: Integer;
    FNumOverlappedBuffers: Integer;
    FPollingDelayTime: Integer;
    FThreadSleepTime: Integer;
    FLinkCollection: array of THIDPLinkCollectionNode;
    FMaxDataListLength: ULONG;
    FMaxUsageListLength: ULONG;
    FMaxButtonListLength: ULONG;
    FReportTypeParam: THIDPReportType;
    FUsagePageParam: TUsage;
    FLinkCollectionParam: WORD;
    FUsageParam: TUsage;
    FOnData: TJvHidDataEvent;
    FOnDataError: TJvHidDataErrorEvent;
    FOnUnplug: TJvHidUnplugEvent;
    FHasReadWriteAccess: Boolean;
    FDataThread: TJvHidDeviceReadThread;
    FTag: Integer;
    // tells if access to device is allowed
    function IsAccessible: Boolean;
    procedure GetMax;
    // internal property implementors
    function GetDeviceString(Idx: Byte): string;
    {$IFNDEF UNICODE}
    function GetDeviceStringWideString(Idx: Byte): WideString;
    {$ENDIF ~UNICODE}
    function GetLinkCollectionNode(Idx: WORD): THIDPLinkCollectionNode;
    function GetConfiguration: THIDDConfiguration;
    function GetPreparsedData: PHIDPPreparsedData;
    function GetCaps: THIDPCaps;
    function GetVendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
    function GetProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
    function GetSerialNumber: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
    function GetPhysicalDescriptor: TJvPhysicalDescriptor;
    function GetLanguageStrings: TStrings;
    function GetOverlappedReadResult: DWORD;
    function GetOverlappedWriteResult: DWORD;
    procedure SetConfiguration(const Config: THIDDConfiguration);
    procedure SetOnData(const DataEvent: TJvHidDataEvent);
    procedure SetNumInputBuffers(const Num: Integer);
    procedure SetNumOverlappedBuffers(const Num: Integer);
    procedure SetReportTypeParam(const ReportType: THIDPReportType);
    procedure SetPollingDelayTime(const DelayTime: Integer);
    procedure SetThreadSleepTime(const SleepTime: Integer);
    procedure SetUsagePageParam(const UsagePage: TUsage);
    procedure StartThread;
    procedure StopThread;
    // Constructor is hidden! Only a TJvHidDeviceController can create a TJvHidDevice object.
    // APnPInfo becomes the property of this class, do not try to free it yourself,
    // even if this call raises an exception.
    // The destructor of this class will take care of the cleanup even when an exception
    // is raised (as specified by the Delphi language)
    constructor CtlCreate(const APnPInfo: TJvHidPnPInfo; const Controller: TJvHidDeviceController);
  protected
    // internal event implementor
    procedure DoUnplug;
  public
    // dummy constructor
    constructor Create;
    destructor Destroy; override;
    // methods
    function CancelIO(const Mode: TJvHidOpenExMode): Boolean;
    procedure CloseFile;
    procedure CloseFileEx(const Mode: TJvHidOpenExMode);
    function DeviceIoControl(IoControlCode: DWORD; InBuffer: Pointer; InSize: DWORD;
      OutBuffer: Pointer; OutSize: DWORD;
      var BytesReturned: DWORD): Boolean;
    function FlushQueue: Boolean;
    function GetButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
    function GetButtons(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetButtonsEx(UsageList: PUsageAndPage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetData(DataList: PHIDPData; var DataLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetFeature(var Report; const Size: Integer): Boolean;
    function GetScaledUsageValue(var UsageValue: Integer;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetSpecificButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
    function GetSpecificValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
    function GetUsages(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetUsagesEx(UsageList: PUsageAndPage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetUsageValue(var UsageValue: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetUsageValueArray(UsageValue: PAnsiChar; UsageValueByteLength: WORD;
      var Report; ReportLength: ULONG): NTSTATUS;
    function GetValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
    function OpenFile: Boolean;
    function OpenFileEx(Mode: TJvHidOpenExMode): Boolean;
    function SetButtons(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function SetData(DataList: PHIDPData; var DataLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function SetFeature(var Report; const Size: Integer): Boolean;
    function SetScaledUsageValue(UsageValue: Integer;
      var Report; ReportLength: ULONG): NTSTATUS;
    function SetUsages(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function SetUsageValue(UsageValue: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function SetUsageValueArray(UsageValue: PAnsiChar; UsageValueByteLength: WORD;
      var Report; ReportLength: ULONG): NTSTATUS;
    function UnsetButtons(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function UnsetUsages(UsageList: PUsage; var UsageLength: ULONG;
      var Report; ReportLength: ULONG): NTSTATUS;
    function ReadFile(var Report; ToRead: DWORD; var BytesRead: DWORD): Boolean;
    function ReadFileEx(var Report; ToRead: DWORD;
      CallBack: TPROverlappedCompletionRoutine): Boolean;
    function WriteFile(var Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
    function WriteFileEx(var Report; ToWrite: DWORD;
      CallBack: TPROverlappedCompletionRoutine): Boolean;
    function CheckOut: Boolean;
    // Windows version dependent methods
    // added in Win 2000
    function GetExtendedAttributes(ReportType: THIDPReportType; DataIndex: Word;
      Attributes: PHIDPExtendedAttributes; var LengthAttributes: ULONG): NTSTATUS;
    function InitializeReportForID(ReportType: THIDPReportType; ReportID: Byte;
      var Report; ReportLength: ULONG): NTSTATUS;
    // added in Win XP
    function GetInputReport(var Report; const Size: ULONG): Boolean;
    function SetOutputReport(var Report; const Size: ULONG): Boolean;
    // read only properties
    property Attributes: THIDDAttributes read FAttributes;
    property Caps: THIDPCaps read GetCaps;
    property HasReadWriteAccess: Boolean read FHasReadWriteAccess;
    property HidFileHandle: THandle read FHidFileHandle;
    property HidOverlappedRead: THandle read FHidOverlappedRead;
    property HidOverlappedWrite: THandle read FHidOverlappedWrite;
    property HidOverlappedReadResult: DWORD read GetOverlappedReadResult;
    property HidOverlappedWriteResult: DWORD read GetOverlappedWriteResult;
    property IsCheckedOut: Boolean read FIsCheckedOut;
    property IsPluggedIn: Boolean read FIsPluggedIn;
    property LanguageStrings: TStrings read GetLanguageStrings;
    property MaxButtonListLength: ULONG read FMaxButtonListLength;
    property MaxDataListLength: ULONG read FMaxDataListLength;
    property MaxUsageListLength: ULONG read FMaxUsageListLength;
    property PhysicalDescriptor: TJvPhysicalDescriptor read GetPhysicalDescriptor;
    property PnPInfo: TJvHidPnPInfo read FPnPInfo;
    property PreparsedData: PHIDPPreparsedData read GetPreparsedData;
    {$IFDEF UNICODE}
    property ProductName: string read GetProductName;
    property SerialNumber: string read GetSerialNumber;
    property VendorName: string read GetVendorName;
    {$ELSE}
    property ProductName: WideString read GetProductName;
    property SerialNumber: WideString read GetSerialNumber;
    property VendorName: WideString read GetVendorName;
    {$ENDIF UNICODE}
    // read write properties
    property Configuration: THIDDConfiguration read GetConfiguration write SetConfiguration;
    property LinkCollectionParam: WORD read FLinkCollectionParam write FLinkCollectionParam;
    property NumInputBuffers: Integer read FNumInputBuffers write SetNumInputBuffers;
    property NumOverlappedBuffers: Integer read FNumOverlappedBuffers write SetNumOverlappedBuffers;
    property ReportTypeParam: THIDPReportType read FReportTypeParam write SetReportTypeParam;
    property Tag: Integer read FTag write FTag;
    property PollingDelayTime: Integer read FPollingDelayTime write SetPollingDelayTime;
    property ThreadSleepTime: Integer read FThreadSleepTime write SetThreadSleepTime;
    property UsagePageParam: TUsage read FUsagePageParam write SetUsagePageParam;
    property UsageParam: TUsage read FUsageParam write FUsageParam;
    // indexed properties
    property DeviceStrings[Idx: Byte]: string read GetDeviceString;
    {$IFDEF UNICODE}
    property DeviceStringsUnicode[Idx: Byte]: string read GetDeviceString;
    {$ELSE}
    property DeviceStringsUnicode[Idx: Byte]: WideString read GetDeviceStringWideString;
    {$ENDIF UNICODE}
    property LinkCollectionNodes[Idx: WORD]: THIDPLinkCollectionNode read GetLinkCollectionNode;
    // event properties
    property OnData: TJvHidDataEvent read FOnData write SetOnData;
    property OnDataError: TJvHidDataErrorEvent read FOnDataError write FOnDataError;
    property OnUnplug: TJvHidUnplugEvent read FOnUnplug write FOnUnplug;
  end;

  // controller class to manage all HID devices

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvHidDeviceController = class(TJvComponent)
  private
    // internal properties part
    FHidGuid: TGUID;
    FOnArrival: TJvHidPlugEvent;
    FOnDeviceChange: TNotifyEvent;
    FOnEnumerate: TJvHidEnumerateEvent;
    FOnDeviceData: TJvHidDataEvent;
    FOnDeviceDataError: TJvHidDataErrorEvent;
    FOnDeviceUnplug: TJvHidUnplugEvent;
    FOnRemoval: TJvHidUnplugEvent;
    FOnDeviceCreateError: TJvHidDeviceCreateError;
    FDevPollingDelayTime: Integer;
    FDevThreadSleepTime: Integer;
    FVersion: string;
    FDummy: string;
    // internal list of all HID device objects
    FList: TList;
    // counters for the list
    FNumCheckedInDevices: Integer;
    FNumCheckedOutDevices: Integer;
    FNumUnpluggedDevices: Integer;
    // reentrancy
    FInDeviceChange: Boolean;
    FLParam: LPARAM;
    // window to catch WM_DEVICECHANGE
    FHWnd: HWND;
    // internal worker functions
    function CheckThisOut(var HidDev: TJvHidDevice; Idx: Integer; Check: Boolean): Boolean;
    procedure EventPipe(var Msg: TMessage);
    // internal event implementors
    procedure SetOnDeviceChange(const Notifier: TNotifyEvent);
    procedure SetDevPollingDelayTime(const DevTime: Integer);
    procedure SetDevThreadSleepTime(const DevTime: Integer);
    procedure SetOnDeviceData(const DataEvent: TJvHidDataEvent);
    procedure SetOnDeviceDataError(const DataErrorEvent: TJvHidDataErrorEvent);
    procedure SetOnDeviceUnplug(const Unplugger: TJvHidUnplugEvent);
  protected
    procedure DoArrival(HidDev: TJvHidDevice);
    procedure DoRemoval(HidDev: TJvHidDevice);
    procedure DoDeviceChange;
    function DoEnumerate(HidDev: TJvHidDevice; Idx: Integer): Boolean;
  public
    // normal constructor/destructor
    constructor Create(AOwner: TComponent); overload; override;

    /// <summary> Constructor for the device manager.
    /// </summary>
    /// <see cref="TJvHidDevice"/>
    /// <param name="AOwner"> The owner of this component.
    /// </param>
    /// <param name="AOnHidCtlDeviceCreateError"> Event method for device errors.
    ///   You need this event handler for catching and handling of device errors.
    ///   The constructor will start the search for devices, you could get
    ///   device errors in this search, which will abort the search if no
    ///   event method is given.
    /// </param>
    /// <param name="AOnDeviceChange"> Event method for device changes.
    ///   (e.g. a new device is added)
    /// </param>
    constructor Create(AOwner: TComponent; AOnHidCtlDeviceCreateError: TJvHidDeviceCreateError;
      AOnDeviceChange: TNotifyEvent = nil); reintroduce; overload;
    destructor Destroy; override;
    // methods to hand out HID device objects
    procedure CheckIn(var HidDev: TJvHidDevice);
    function CheckOut(var HidDev: TJvHidDevice): Boolean;
    function CheckOutByClass(var HidDev: TJvHidDevice; const ClassName: string): Boolean;
    function CheckOutByID(var HidDev: TJvHidDevice; const Vid, Pid: Integer): Boolean;
    function CheckOutByIndex(var HidDev: TJvHidDevice; const Idx: Integer): Boolean;
    function CheckOutByProductName(var HidDev: TJvHidDevice; const ProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Boolean; overload;
    function CheckOutByVendorName(var HidDev: TJvHidDevice; const VendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Boolean; overload;
    function CheckOutByCallback(var HidDev: TJvHidDevice; Check: TJvHidCheckCallback): Boolean;
    // methods to count HID device objects
    function CountByClass(const ClassName: string): Integer;
    function CountByID(const Vid, Pid: Integer): Integer;
    function CountByProductName(const ProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;
    function CountByVendorName(const VendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;
    function CountByCallback(Check: TJvHidCheckCallback): Integer;
    // iterate over the HID devices
    function Enumerate: Integer;
    class function HidVersion: string;
    // just to be complete the GUID
    property HidGuid: TGUID read FHidGuid;
    property NumCheckedInDevices: Integer read FNumCheckedInDevices;
    property NumCheckedOutDevices: Integer read FNumCheckedOutDevices;
    property NumUnpluggedDevices: Integer read FNumUnpluggedDevices;
  published
    property DevPollingDelayTime: Integer read FDevPollingDelayTime write SetDevPollingDelayTime default 0;
    property DevThreadSleepTime: Integer read FDevThreadSleepTime write SetDevThreadSleepTime default 100;
    property Version: string read FVersion write FDummy stored False;
    property OnArrival: TJvHidPlugEvent read FOnArrival write FOnArrival;
    // the iterator event
    property OnEnumerate: TJvHidEnumerateEvent read FOnEnumerate write FOnEnumerate;
    // the central event for HID device changes
    property OnDeviceChange: TNotifyEvent read FOnDeviceChange write SetOnDeviceChange;
    // this event is triggered when an error occurred while creating a given TJvHidDevice
    property OnDeviceCreateError: TJvHidDeviceCreateError read FOnDeviceCreateError write FOnDeviceCreateError;
    // these events are copied to TJvHidDevices on creation
    property OnDeviceData: TJvHidDataEvent read FOnDeviceData write SetOnDeviceData;
    property OnDeviceDataError: TJvHidDataErrorEvent read FOnDeviceDataError write SetOnDeviceDataError;
    property OnDeviceUnplug: TJvHidUnplugEvent read FOnDeviceUnplug write SetOnDeviceUnplug;
    property OnRemoval: TJvHidUnplugEvent read FOnRemoval write FOnRemoval;
    // to be callable at design time
    procedure DeviceChange;
  end;

// helpers to check the HID function and method results
function HidCheck(const RetVal: NTSTATUS): NTSTATUS; overload;
function HidCheck(const RetVal: LongBool): LongBool; overload;
function HidError(const RetVal: NTSTATUS): NTSTATUS;
function HidErrorString(const RetVal: NTSTATUS): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF RTL230_UP}
  Types,
  {$ELSE}
  ActiveX,
  {$ENDIF RTL230_UP}
  JvResources;

type
  EControllerError = class(EJVCLException);
  EHidClientError = class(EJVCLException);

//=== these are declared inconsistent in Windows.pas =========================

function ReadFileEx(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var Overlapped: TOverlapped; lpCompletionRoutine: TPROverlappedCompletionRoutine): BOOL; stdcall;
  external kernel32 name 'ReadFileEx';

function WriteFileEx(hFile: THandle; var Buffer; nNumberOfBytesToWrite: DWORD;
  var Overlapped: TOverlapped; lpCompletionRoutine: TPROverlappedCompletionRoutine): BOOL; stdcall;
  external kernel32 name 'WriteFileEx';

//=== { TJvHidDeviceReadThread } =============================================

constructor TJvHidDeviceReadThread.CtlCreate(const Dev: TJvHidDevice);
begin
  inherited Create(False);
  Device := Dev;
  NumBytesRead := 0;
  SetLength(Report, Dev.Caps.InputReportByteLength);
end;

constructor TJvHidDeviceReadThread.Create(CreateSuspended: Boolean);
begin
  raise EControllerError.CreateRes(@RsEDirectThreadCreationNotAllowed);
end;

procedure TJvHidDeviceReadThread.DoData;
begin
  Device.OnData(Device, Report[0], @Report[1], NumBytesRead - 1);
end;

procedure TJvHidDeviceReadThread.DoDataError;
begin
  if Assigned(Device.FOnDataError) then
    Device.FOnDataError(Device, FErr);
end;

procedure DummyReadCompletion(ErrorCode: DWORD; Count: DWORD; Ovl: POverlapped); stdcall;
begin
end;

procedure TJvHidDeviceReadThread.Execute;
var
  SleepRet: DWORD;
begin
  NameThread(ThreadName);
  SleepRet := WAIT_IO_COMPLETION;
  try
    while not Terminated do
    begin
      // read data
      SleepRet := WAIT_IO_COMPLETION;
      FillChar(Report[0], Device.Caps.InputReportByteLength, #0);
      if Device.ReadFileEx(Report[0], Device.Caps.InputReportByteLength, @DummyReadCompletion) then
      begin
        // wait for read to complete
        repeat
          SleepRet := SleepEx(Device.ThreadSleepTime, True);
        until Terminated or (SleepRet = WAIT_IO_COMPLETION);
        // show data read
        if not Terminated then
        begin
          NumBytesRead := Device.HidOverlappedReadResult;
          if NumBytesRead > 0 then
            // synchronizing only works if the component is not instanciated in a DLL
            if IsLibrary then
              DoData
            else
              Synchronize(DoData);
          if Device.PollingDelayTime > 0 then  // Throttle device polling
            SleepEx(Device.PollingDelayTime, True);
        end;
      end
      else
      begin
        FErr := GetLastError;
        Synchronize(DoDataError);
        SleepEx(Device.ThreadSleepTime, True);  // avoid 100% CPU usage (Mantis 5749)
      end;
    end;
  finally
    // cancel ReadFileEx call or the callback will
    // crash your program
    if SleepRet <> WAIT_IO_COMPLETION then
      Device.CancelIO(omhRead);
  end;
end;

//=== { TJvHidPnPInfo } ======================================================

constructor TJvHidPnPInfo.Create(APnPHandle: HDEVINFO; ADevData: TSPDevInfoData; const ADevicePath: string);
begin
  inherited Create;
  FDeviceID := ADevData.DevInst;
  FDevicePath := ADevicePath;

  // primary information
  FCapabilities := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_CAPABILITIES);
  FClassDescr := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_CLASS);
  FClassGUID := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_CLASSGUID);
  FCompatibleIDs := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_COMPATIBLEIDS);
  FConfigFlags := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_CONFIGFLAGS);
  FDeviceDescr := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_DEVICEDESC);
  FDriver := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_DRIVER);
  FFriendlyName := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_FRIENDLYNAME);
  FHardwareID := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_HARDWAREID);
  FLowerFilters := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_LOWERFILTERS);
  FMfg := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_MFG);
  FUpperFilters := GetRegistryPropertyStringList(APnPHandle, ADevData, SPDRP_UPPERFILTERS);
  FService := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_SERVICE);

  // secondary information not all likely to exist for a HID device
  FAddress := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_ADDRESS);
  FBusNumber := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_BUSNUMBER);
  FBusType := GuidToString(GetRegistryPropertyGuid(APnPHandle, ADevData, SPDRP_BUSTYPEGUID));
  FCharacteristics := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_CHARACTERISTICS);
  FDevType := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_DEVTYPE);
  FEnumeratorName := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_ENUMERATOR_NAME);
  FExclusive := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_EXCLUSIVE);
  FLegacyBusType := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_LEGACYBUSTYPE);
  FLocationInfo := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_LOCATION_INFORMATION);
  FPhysDevObjName := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_PHYSICAL_DEVICE_OBJECT_NAME);
  FSecuritySDS := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_SECURITY_SDS);
  FUINumber := GetRegistryPropertyDWord(APnPHandle, ADevData, SPDRP_UI_NUMBER);
  FUINumberFormat := GetRegistryPropertyString(APnPHandle, ADevData, SPDRP_UI_NUMBER_DESC_FORMAT);
end;

destructor TJvHidPnPInfo.Destroy;
begin
  FCompatibleIDs.Free;
  FHardwareID.Free;
  FLowerFilters.Free;
  FUpperFilters.Free;
  inherited Destroy;
end;

function TJvHidPnPInfo.GetCompatibleIDs: TStrings;
begin
  Result := FCompatibleIDs;
end;

function TJvHidPnPInfo.GetHardwareID: TStrings;
begin
  Result := FHardwareID;
end;

function TJvHidPnPInfo.GetLowerFilters: TStrings;
begin
  Result := FLowerFilters;
end;

function TJvHidPnPInfo.GetUpperFilters: TStrings;
begin
  Result := FUpperFilters;
end;

// internal helpers to read values from a devices registry area

function TJvHidPnPInfo.GetRegistryPropertyString(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): string;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
  Buffer: PChar;
  StackBuffer: array[0..1023] of Char;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Result := '';
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, RegDataType, nil, 0, BytesReturned);
  if BytesReturned > 0 then
  begin
    if BytesReturned + SizeOf(Char) <= SizeOf(StackBuffer) then
    begin
      Buffer := @StackBuffer;
      // enforce terminator
      Buffer[BytesReturned] := #0;
    end
    else
      Buffer := AllocMem((BytesReturned + 1) * SizeOf(Char));

    try
      Buffer[0] := #0;
      SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, RegDataType, PByte(@Buffer[0]),
        BytesReturned, BytesReturned);
      Result := Buffer;
    finally
      if Buffer <> @StackBuffer then
        FreeMem(Buffer);
    end;
  end;
end;

function TJvHidPnPInfo.GetRegistryPropertyStringList(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): TStringList;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
  Buffer: PChar;
  P: PChar;
  StackBuffer: array[0..16383] of Char;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Result := TStringList.Create;
  try
    SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, RegDataType, nil, 0, BytesReturned);
    if BytesReturned > 0 then
    begin
      if BytesReturned + 2 * SizeOf(Char) <= SizeOf(StackBuffer) then
      begin
        Buffer := @StackBuffer;
        // enforce terminators
        Buffer[BytesReturned] := #0;
        Buffer[BytesReturned + 1] := #0;
      end
      else
        Buffer := AllocMem((BytesReturned + 2) * SizeOf(Char));
      try
        SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, RegDataType, PByte(@Buffer[0]),
          BytesReturned, BytesReturned);
        P := @Buffer[0];
        while P[0] <> #0 do
        begin
          Result.Add(P);
          P := P + StrLen(P) + 1;
        end;
      finally
        if Buffer <> @StackBuffer then
          FreeMem(Buffer);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TJvHidPnPInfo.GetRegistryPropertyDWord(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Result := 0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, PBYTE(@Result), SizeOf(Result), BytesReturned);
end;

function TJvHidPnPInfo.GetRegistryPropertyGuid(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): TGuid;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Result := GUID_NULL;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, RegDataType,
                                   PByte(@Result), SizeOf(Result), BytesReturned);
end;

//=== { TJvHidDevice } =======================================================

// dummy constructor to catch invalid Create calls

constructor TJvHidDevice.Create;
begin
  inherited Create;
  FHidFileHandle := INVALID_HANDLE_VALUE;
  FHidOverlappedRead := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite := INVALID_HANDLE_VALUE;
  raise EControllerError.CreateRes(@RsEDirectHidDeviceCreationNotAllowed);
end;

// create and fill in a HidDevice object
// the constructor is only accessible from TJvHidController
// PnPInfo contains all info the JvHidDeviceController collected
// Controller is the devices controller object to be referenced
// internally

constructor TJvHidDevice.CtlCreate(const APnPInfo: TJvHidPnPInfo; const Controller: TJvHidDeviceController);
begin
  inherited Create;

  // initialize private data
  FMyController := Controller;
  FIsPluggedIn := True;
  FIsCheckedOut := False;
  FIsEnumerated := False;
  FHidOverlappedRead := INVALID_HANDLE_VALUE;
  FHidOverlappedWrite := INVALID_HANDLE_VALUE;
  FVendorName := '';
  FProductName := '';
  FPreparsedData := nil;
  SetLength(FPhysicalDescriptor, 0);
  FSerialNumber := '';
  FLanguageStrings := TStringList.Create;
  FNumInputBuffers := 0;
  FNumOverlappedBuffers := 0;
  SetLength(FLinkCollection, 0);
  FMaxDataListLength := 0;
  FMaxUsageListLength := 0;
  FMaxButtonListLength := 0;
  FReportTypeParam := HIDP_Input;
  FPollingDelayTime := Controller.DevPollingDelayTime;
  FThreadSleepTime := Controller.DevThreadSleepTime;
  FUsagePageParam := 0;
  FLinkCollectionParam := 0;
  FUsageParam := 0;
  FDataThread := nil;
  OnData := Controller.OnDeviceData;
  OnUnplug := Controller.OnDeviceUnplug;
  OnDataError := Controller.OnDeviceDataError;

  FHidFileHandle := CreateFile(PChar(APnPInfo.DevicePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  FHasReadWriteAccess := HidFileHandle <> INVALID_HANDLE_VALUE;
  // Win2000 hack
  if not HasReadWriteAccess then
    FHidFileHandle := CreateFile(PChar(APnPInfo.DevicePath), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if HidFileHandle <> INVALID_HANDLE_VALUE then
  begin
    FAttributes.Size := SizeOf(THIDDAttributes);
    if not HidD_GetAttributes(HidFileHandle, FAttributes) then
      raise EControllerError.CreateRes(@RsEDeviceCannotBeIdentified);
  end
  else
    raise EControllerError.CreateRes(@RsEDeviceCannotBeOpened);
  FPnPInfo := APnPInfo;
  // the file is closed to stop using up resources
  CloseFile;
end;

// If a TJvHidDevice is destroyed the TJvHidController has to be informed.
// If the device is plugged in this TJvHidDevice instance is destroyed,
// but another instance is created in the controller list to replace it.

destructor TJvHidDevice.Destroy;
var
  I: Integer;
  TmpOnData: TJvHidDataEvent;
  TmpOnUnplug: TJvHidUnplugEvent;
  Dev: TJvHidDevice;
begin
  // if we need to clone the object
  TmpOnData := OnData;
  TmpOnUnplug := OnUnplug;
  // to prevent strange problems
  OnData := nil;
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
            Dev := nil;
            try
              Dev := TJvHidDevice.CtlCreate(FPnPInfo, FMyController);
              // make it a complete clone
              Dev.OnData := TmpOnData;
              Dev.OnUnplug := TmpOnUnplug;
              Dev.PollingDelayTime := PollingDelayTime;
              Dev.ThreadSleepTime := ThreadSleepTime;
              FList.Items[I] := Dev;
              // the FPnPInfo has been handed over to the new object
              FPnPInfo := nil;
              if IsCheckedOut then
              begin
                Dec(FNumCheckedOutDevices);
                Inc(FNumCheckedInDevices);
              end;
            except
              on EControllerError do
              begin
                FList.Delete(I);
                Dev.Free;
                Dec(FNumUnpluggedDevices);
              end;
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

  FPnPInfo.Free;
  inherited Destroy;
end;

// if check changes change check only here

function TJvHidDevice.IsAccessible: Boolean;
begin
  Result := IsPluggedIn and (IsCheckedOut or FIsEnumerated);
end;

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

// implement OnUnplug event

procedure TJvHidDevice.DoUnplug;
begin
  CloseFile;
  CloseFileEx(omhRead);
  CloseFileEx(omhWrite);
  FIsPluggedIn := False;
  // event even for checked in devices
  if Assigned(FOnUnplug) then
    FOnUnplug(Self);
  // guarantees that event is only called once
  OnUnplug := nil;
end;

// implementing indexed properties read

function TJvHidDevice.GetDeviceString(Idx: Byte): string;
var
  Buffer: array [0..253] of WideChar;
begin
  Result := '';
  if Idx <> 0 then
    if OpenFile then
      if HidD_GetIndexedString(HidFileHandle, Idx, Buffer, SizeOf(Buffer)) then
        Result := WideCharToString(Buffer);
end;

{$IFNDEF UNICODE}
function TJvHidDevice.GetDeviceStringWideString(Idx: Byte): WideString;
var
  Buffer: array [0..253] of WideChar;
begin
  Result := '';
  if Idx <> 0 then
    if OpenFile then
      if HidD_GetIndexedString(HidFileHandle, Idx, Buffer, SizeOf(Buffer)) then
        Result := Buffer;
end;
{$ENDIF ~UNICODE}

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
  if Idx < Length(FLinkCollection) then
    Result := FLinkCollection[Idx];
end;

// implementing properties write

procedure TJvHidDevice.SetNumInputBuffers(const Num: Integer);
begin
  if (Num <> FNumInputBuffers) and OpenFile then
  begin
    HidD_SetNumInputBuffers(HidFileHandle, Num);
    HidD_GetNumInputBuffers(HidFileHandle, FNumInputBuffers);
  end;
end;

procedure TJvHidDevice.SetNumOverlappedBuffers(const Num: Integer);
begin
  if (Num <> FNumOverlappedBuffers) and OpenFileEx(omhRead) then
  begin
    HidD_SetNumInputBuffers(HidOverlappedRead, Num);
    HidD_GetNumInputBuffers(HidOverlappedRead, FNumOverlappedBuffers);
  end;
end;

// internal helper for the following functions

procedure TJvHidDevice.GetMax;
begin
  if IsAccessible then
  begin
    FMaxDataListLength := HidP_MaxDataListLength(ReportTypeParam, PreparsedData);
    FMaxUsageListLength := HidP_MaxUsageListLength(ReportTypeParam, UsagePageParam, PreparsedData);
    FMaxButtonListLength := HidP_MaxButtonListLength(ReportTypeParam, UsagePageParam, PreparsedData);
  end;
end;

procedure TJvHidDevice.SetReportTypeParam(const ReportType: THIDPReportType);
begin
  FReportTypeParam := ReportType;
  GetMax;
end;

procedure TJvHidDevice.SetPollingDelayTime(const DelayTime: Integer);
begin
  // limit to 0 sec .. 10 sec
  if (DelayTime >= 0) and (DelayTime <= 10000) then
    FPollingDelayTime := DelayTime;
end;

procedure TJvHidDevice.SetThreadSleepTime(const SleepTime: Integer);
begin
  // limit to 10 msec .. 10 sec
  if (SleepTime >= 10) and (SleepTime <= 10000) then
    FThreadSleepTime := SleepTime;
end;

procedure TJvHidDevice.SetUsagePageParam(const UsagePage: TUsage);
begin
  FUsagePageParam := UsagePage;
  GetMax;
end;

function TJvHidDevice.GetConfiguration: THIDDConfiguration;
begin
  Result.cookie := nil;
  Result.size := 0;
  Result.RingBufferSize := 0;
  if OpenFile then
    HidD_GetConfiguration(HidFileHandle, Result, SizeOf(THIDDConfiguration));
end;

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

function TJvHidDevice.GetCaps: THIDPCaps;
begin
  FillChar(Result, SizeOf(THIDPCaps), #0);
  HidP_GetCaps(PreparsedData, Result);
end;

function TJvHidDevice.GetVendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
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

function TJvHidDevice.GetProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
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

function TJvHidDevice.GetSerialNumber: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
var
  I: Integer;
  Len: Integer;
  IDs: array [0..253] of WORD;
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
              Len := I + 1;
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

function TJvHidDevice.GetPhysicalDescriptor: TJvPhysicalDescriptor;
var
  I: Integer;
begin
  if Length(FPhysicalDescriptor) = 0 then
    if OpenFile then
    begin
      I := 0;
      SetLength(FPhysicalDescriptor, 2048);
      while not HidD_GetPhysicalDescriptor(HidFileHandle, FPhysicalDescriptor[0], I * SizeOf(WORD)) do
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

function TJvHidDevice.GetLanguageStrings: TStrings;
var
  I: Integer;
  Len: Integer;
  IDs: array [0..253] of WORD;
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
              Len := I + 1;
            Break;
          end;
      // transform id into localized language name
      for I := 0 to Len - 1 do
      begin
        Name[0] := #0;
        if GetLocaleInfo(WORD(IDs[I]), LOCALE_SLANGUAGE, Name, SizeOf(Name)) <> 0 then
          FLanguageStrings.Add(Name)
        else
          FLanguageStrings.Add(Format(RsUnknownLocaleIDFmt, [WORD(IDs[I])]));
      end;
      CloseFile;
    end;
  Result := FLanguageStrings;
end;

function TJvHidDevice.GetOverlappedReadResult: DWORD;
begin
  Result := 0;
  if HidOverlappedRead <> INVALID_HANDLE_VALUE then
    if not GetOverlappedResult(HidOverlappedRead, FOvlRead, Result, False) then
      Result := 0;
end;

function TJvHidDevice.GetOverlappedWriteResult: DWORD;
begin
  Result := 0;
  if HidOverlappedWrite <> INVALID_HANDLE_VALUE then
    if not GetOverlappedResult(HidOverlappedWrite, FOvlWrite, Result, False) then
      Result := 0;
end;

procedure TJvHidDevice.SetConfiguration(const Config: THIDDConfiguration);
begin
  if OpenFile then
    HidD_SetConfiguration(HidFileHandle, Config, SizeOf(THIDDConfiguration));
end;

procedure TJvHidDevice.SetOnData(const DataEvent: TJvHidDataEvent);
begin
  // this assignment is a bit tricky because a thread may be running
  // kill the thread with the old event still in effect
  if not Assigned(DataEvent) then
    StopThread;
  // assign the new event and start the thread if needed
  FOnData := DataEvent;
  StartThread;
end;

procedure TJvHidDevice.StartThread;
begin
  if Assigned(FOnData) and IsPluggedIn and IsCheckedOut and
    HasReadWriteAccess and not Assigned(FDataThread) then
  begin
    FDataThread := TJvHidDeviceReadThread.CtlCreate(Self);
  end;
end;

procedure TJvHidDevice.StopThread;
begin
  if Assigned(FDataThread) then
  begin
    FDataThread.Terminate;
    FDataThread.WaitFor;
    FDataThread.Free;
    FDataThread := nil;
  end;
end;

// TJvHidDevice methods:
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
    hKernel: HMODULE;
    CancelIOFunc: TCancelIOFunc;
  begin
    hKernel := GetModuleHandle(kernel32);
    Result := hKernel <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      @CancelIOFunc := GetProcAddress(hKernel, 'CancelIO');
      if Assigned(CancelIOFunc) then
        Result := CancelIOFunc(Handle)
      else
        Result := False;
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

// close the device "file"
// if you want to open the file directly close this
// to get undisturbed access

procedure TJvHidDevice.CloseFile;
begin
  if HidFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(HidFileHandle);
  FNumInputBuffers := 0;
  FHidFileHandle := INVALID_HANDLE_VALUE;
end;

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

// all the methods which directly map to a HID-function

function TJvHidDevice.FlushQueue: Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_FlushQueue(HidFileHandle);
end;

function TJvHidDevice.GetFeature(var Report; const Size: Integer): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_GetFeature(HidFileHandle, Report, Size);
end;

function TJvHidDevice.SetFeature(var Report; const Size: Integer): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := HidD_SetFeature(HidFileHandle, Report, Size);
end;

function TJvHidDevice.GetSpecificButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetSpecificButtonCaps(ReportTypeParam, UsagePageParam,
      LinkCollectionParam, UsageParam, ButtonCaps, Count, PreparsedData);
end;

function TJvHidDevice.GetButtonCaps(ButtonCaps: PHIDPButtonCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtonCaps_(ReportTypeParam, ButtonCaps, Count, PreparsedData);
end;

function TJvHidDevice.GetSpecificValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetSpecificValueCaps(ReportTypeParam, UsagePageParam,
      LinkCollectionParam, UsageParam, ValueCaps, Count, PreparsedData);
end;

function TJvHidDevice.GetValueCaps(ValueCaps: PHIDPValueCaps; var Count: WORD): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetValueCaps_(ReportTypeParam, ValueCaps, Count, PreparsedData);
end;

function TJvHidDevice.GetData(DataList: PHIDPData; var DataLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetData(ReportTypeParam, DataList, DataLength, PreparsedData,
      Report, ReportLength);
end;

function TJvHidDevice.SetData(DataList: PHIDPData; var DataLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetData(ReportTypeParam, DataList, DataLength, PreparsedData,
      Report, ReportLength);
end;

function TJvHidDevice.GetUsages(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetButtons(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetUsagesEx(UsageList: PUsageAndPage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsagesEx(ReportTypeParam, LinkCollectionParam, UsageList,
      UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetButtonsEx(UsageList: PUsageAndPage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetButtonsEx(ReportTypeParam, LinkCollectionParam, UsageList,
      UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.SetUsages(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.SetButtons(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.UnsetUsages(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_UnsetUsages(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.UnsetButtons(UsageList: PUsage; var UsageLength: ULONG;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_UnsetButtons(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageList, UsageLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetUsageValue(var UsageValue: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetScaledUsageValue(var UsageValue: Integer;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetScaledUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.GetUsageValueArray(UsageValue: PAnsiChar;
  UsageValueByteLength: WORD; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_GetUsageValueArray(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, UsageValueByteLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.SetUsageValue(UsageValue: ULONG; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.SetScaledUsageValue(UsageValue: Integer; var Report;
  ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetScaledUsageValue(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.SetUsageValueArray(UsageValue: PAnsiChar;
  UsageValueByteLength: WORD; var Report; ReportLength: ULONG): NTSTATUS;
begin
  Result := HIDP_STATUS_NULL; // for not plugged in
  if IsAccessible then
    Result := HidP_SetUsageValueArray(ReportTypeParam, UsagePageParam, LinkCollectionParam,
      UsageParam, UsageValue, UsageValueByteLength, PreparsedData, Report, ReportLength);
end;

function TJvHidDevice.DeviceIoControl(IoControlCode: DWORD; InBuffer: Pointer; InSize: DWORD;
  OutBuffer: Pointer; OutSize: DWORD; var BytesReturned: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.DeviceIoControl(HidFileHandle, IoControlCode, InBuffer, InSize,
      OutBuffer, OutSize, BytesReturned, nil);
end;

function TJvHidDevice.ReadFile(var Report; ToRead: DWORD; var BytesRead: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.ReadFile(HidFileHandle, Report, ToRead, BytesRead, nil);
end;

function TJvHidDevice.WriteFile(var Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
begin
  Result := False;
  if OpenFile then
    Result := Windows.WriteFile(HidFileHandle, Report, ToWrite, BytesWritten, nil);
end;

// the TOverlapped structure is not needed externally
// the hEvent element is used to transport the device object
// to the callback function
// Better not implement a Delphi event with that

function TJvHidDevice.ReadFileEx(var Report; ToRead: DWORD;
  CallBack: TPROverlappedCompletionRoutine): Boolean;
begin
  Result := False;
  if OpenFileEx(omhRead) then
  begin
    FillChar(FOvlRead, SizeOf(TOverlapped), #0);
    FOvlRead.hEvent := DWORD(Self);
    Result := JvHidControllerClass.ReadFileEx(HidOverlappedRead, Report, ToRead, FOvlRead, CallBack);
  end;
end;

function TJvHidDevice.WriteFileEx(var Report; ToWrite: DWORD;
  CallBack: TPROverlappedCompletionRoutine): Boolean;
begin
  Result := False;
  if OpenFileEx(omhWrite) then
  begin
    FillChar(FOvlWrite, SizeOf(TOverlapped), #0);
    FOvlWrite.hEvent := DWORD(Self);
    Result := JvHidControllerClass.WriteFileEx(HidOverlappedWrite, Report, ToWrite, FOvlWrite, CallBack);
  end;
end;

function TJvHidDevice.CheckOut: Boolean;
begin
  Result := Assigned(FMyController) and IsPluggedIn and not IsCheckedOut;
  if Result then
  begin
    FIsCheckedOut := True;
    Inc(FMyController.FNumCheckedOutDevices);
    Dec(FMyController.FNumCheckedInDevices);
    StartThread;
  end;
end;

function  TJvHidDevice.GetExtendedAttributes(ReportType: THIDPReportType; DataIndex: Word;
  Attributes: PHIDPExtendedAttributes; var LengthAttributes: ULONG): NTSTATUS;
begin
  if Assigned(HidP_GetExtendedAttributes) then
    Result := HidP_GetExtendedAttributes(ReportType, DataIndex, FPreparsedData,
      Attributes, LengthAttributes)
  else
    Result := HIDP_STATUS_NOT_IMPLEMENTED;
end;

function TJvHidDevice.InitializeReportForID(ReportType: THIDPReportType; ReportID: Byte;
  var Report; ReportLength: ULONG): NTSTATUS;
begin
  if Assigned(HidP_InitializeReportForID) then
    Result := HidP_InitializeReportForID(ReportType, ReportID, FPreparsedData, Report, ReportLength)
  else
    Result := HIDP_STATUS_NOT_IMPLEMENTED;
end;

function TJvHidDevice.GetInputReport(var Report; const Size: ULONG): Boolean;
begin
  Result := False;
  if Assigned(HidD_GetInputReport) then
    if OpenFile then
      Result := HidD_GetInputReport(FHidFileHandle, @Report, Size);
end;

function TJvHidDevice.SetOutputReport(var Report; const Size: ULONG): Boolean;
begin
  Result := False;
  if Assigned(HidD_SetOutputReport) then
    if OpenFile then
      Result := HidD_SetOutputReport(FHidFileHandle, @Report, Size);
end;

//=== { TJvHidDeviceController } =============================================

constructor TJvHidDeviceController.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  Method.Code := nil;
  Method.Data := nil;
  
  Create(AOwner, TJvHidDeviceCreateError(Method)); // work around compiler issue with method overloading and method pointers
end;

constructor TJvHidDeviceController.Create(AOwner: TComponent;
  AOnHidCtlDeviceCreateError: TJvHidDeviceCreateError; AOnDeviceChange: TNotifyEvent);
const
  cHidGuid: TGUID = '{4d1e55b2-f16f-11cf-88cb-001111000030}';
begin
  inherited Create(AOwner);
  FDevThreadSleepTime := 100;
  FVersion := cHidControllerClassVersion;

  FList := TList.Create;

  if LoadSetupApi then
    LoadHid;

  SetOnDeviceChange(AOnDeviceChange);
  FOnDeviceCreateError := AOnHidCtlDeviceCreateError;

  if IsHidLoaded then
  begin
    HidD_GetHidGuid(FHidGuid);
    // only hook messages if there is a HID DLL
    FHWnd := AllocateHWnd(EventPipe);
    // this one executes after Create completed which ensures
    // that all global elements like Application.MainForm are initialized
    PostMessage(FHWnd, WM_DEVICECHANGE, DBT_DEVNODES_CHANGED, -1);
  end
  else
    FHidGuid := cHidGuid;
end;

// unplug or kill all controlled TJvHidDevices on controller destruction

destructor TJvHidDeviceController.Destroy;
var
  I: Integer;
  HidDev: TJvHidDevice;
begin
  // to prevent strange problems
  SetOnDeviceChange(nil);
  SetOnDeviceUnplug(nil);
  FOnEnumerate := nil;
  // unhook event pipe
  if IsHidLoaded then
    DeallocateHWnd(FHWnd);

  for I := 0 to FList.Count - 1 do
  begin
    HidDev := FList.Items[I];
    with HidDev do
    begin
      // set to uncontrolled
      FMyController := nil;
      if IsCheckedOut then
        DoUnplug; // pull the plug for checked out TJvHidDevices

      Free; // Always free, which will kill TJvHidDevices which are not checked out
    end;
  end;
  FList.Free;

  if IsHidLoaded then
    UnloadSetupApi;
  UnloadHid;

  inherited Destroy;
end;

procedure TJvHidDeviceController.DoArrival(HidDev: TJvHidDevice);
begin
  if Assigned(FOnArrival) then
  begin
    HidDev.FIsEnumerated := True;
    FOnArrival(HidDev);
    HidDev.FIsEnumerated := False;
  end;
end;

procedure TJvHidDeviceController.DoRemoval(HidDev: TJvHidDevice);
begin
  if Assigned(FOnRemoval) then
  begin
    HidDev.FIsEnumerated := True;
    FOnRemoval(HidDev);
    HidDev.FIsEnumerated := False;
  end;
end;

// implement OnDeviceChange event

procedure TJvHidDeviceController.DoDeviceChange;
begin
  if Assigned(FOnDeviceChange) then
    FOnDeviceChange(Self);
end;

// gets all the Windows events/messages directly

procedure TJvHidDeviceController.EventPipe(var Msg: TMessage);
begin
  // sort out WM_DEVICECHANGE : DBT_DEVNODES_CHANGED
  if not (csDestroying in ComponentState) and
   (Msg.Msg = WM_DEVICECHANGE) and (TWMDeviceChange(Msg).Event = DBT_DEVNODES_CHANGED) then
    if not FInDeviceChange then
    begin
      FLParam := Msg.LParam;
      FInDeviceChange := True;
      DeviceChange;
      FInDeviceChange := False;
    end;
  Msg.Result := DefWindowProc(FHWnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

// implements OnDeviceChange event
// it is published to allow calling at design time

procedure TJvHidDeviceController.DeviceChange;
var
  I: Integer;
  J: Integer;
  HidDev: TJvHidDevice;
  Changed: Boolean;
  NewList: TList;

  // internal worker function to find all HID devices and create their objects

  procedure FillInList;
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
    Handled: Boolean;
    RetryCreate: Boolean;
    DevicePath: string;
  begin
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
        BytesReturned := 0;
        //evalue size needed to store the detailed interface data in FunctionClassDeviceData
        SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, nil, 0, BytesReturned, @DevData);
        if (BytesReturned <> 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
        begin
          FunctionClassDeviceData := AllocMem(BytesReturned);
          try
            FunctionClassDeviceData^.cbSize := SizeOf(TSPDeviceInterfaceDetailData);
            if SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData,
              FunctionClassDeviceData, BytesReturned, BytesReturned, @DevData) then
            begin
              // Win64: Don't include the padding bytes into the string length calculation
              SetString(DevicePath, PChar(@FunctionClassDeviceData.DevicePath), (BytesReturned - (SizeOf(FunctionClassDeviceData.cbSize) + SizeOf(FunctionClassDeviceData.DevicePath))) div SizeOf(Char));
              // fill in PnPInfo of device
              PnPInfo := TJvHidPnPInfo.Create(PnPHandle, DevData, DevicePath);
              // create HID device object and add it to the device list
              RetryCreate := False;
              HidDev := nil;
              repeat
                try
                  HidDev := TJvHidDevice.CtlCreate(PnPInfo, Self);
                except
                  on EControllerError do
                    if Assigned(OnDeviceCreateError) then
                    begin
                      Handled := False;
                      OnDeviceCreateError(Self, PnPInfo, Handled, RetryCreate);
                      if not Handled then
                      Begin
                        FreeAndNil(PnPInfo);
                        raise;
                      end;
                    end
                    else
                    Begin
                      FreeAndNil(PnPInfo);
                      raise;
                    end;
                end;
              until not RetryCreate;
              
              if Assigned(HidDev) then
                NewList.Add(HidDev);
            end;
          finally
            FreeMem(FunctionClassDeviceData);
          end;
        end;
      end;

      Inc(Devn);
    until not Success;
    SetupDiDestroyDeviceInfoList(PnPHandle);
  end;

begin
  // initial auto message always triggers OnDeviceChange event
  Changed := (FLParam = -1);
  // get new device list
  NewList := TList.Create;
  FillInList;

  // unplug devices in FList which are not in NewList
  for I := FList.Count - 1 downto 0 do
  begin
    HidDev := FList.Items[I];
    for J := NewList.Count - 1 downto 0 do
      if (TJvHidDevice(NewList.Items[J]).PnPInfo.DeviceID = HidDev.PnPInfo.DeviceID) and
        HidDev.IsPluggedIn then
      begin
        HidDev := nil;
        Break;
      end;
    if HidDev <> nil then
    begin
      HidDev.DoUnplug;
      DoRemoval(HidDev);
      // delete from list
      if not HidDev.IsCheckedOut then
      begin
        FList.Delete(I);
        HidDev.Free;
      end;
      Changed := True;
    end;
  end;

  // delete devices from NewList which are in FList
  for I := 0 to NewList.Count - 1 do
    for J := 0 to FList.Count - 1 do
      if (TJvHidDevice(NewList[I]).PnPInfo.DeviceID = TJvHidDevice(FList[J]).PnPInfo.DeviceID) and
        TJvHidDevice(FList[J]).IsPluggedIn then
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
      DoArrival(TJvHidDevice(NewList[I]));
    end;

  // throw away helper list
  NewList.Free;

  // recount the devices
  FNumCheckedInDevices := 0;
  FNumCheckedOutDevices := 0;
  FNumUnpluggedDevices := 0;
  for I := 0 to FList.Count - 1 do
  begin
    HidDev := FList.Items[I];
    Inc(FNumCheckedInDevices, Ord(not HidDev.IsCheckedOut));
    Inc(FNumCheckedOutDevices, Ord(HidDev.IsCheckedOut));
    Inc(FNumUnpluggedDevices, Ord(not HidDev.IsPluggedIn));
  end;
  FNumCheckedOutDevices := FNumCheckedOutDevices - FNumUnpluggedDevices;

  if Changed then
    DoDeviceChange;
end;

class function TJvHidDeviceController.HidVersion: string;
var
  Dummy: DWORD;
  Size: UINT;
  Buf: array of Byte;
  Value: PChar;
begin
  Result := '';
  Size := GetFileVersionInfoSize(HidModuleName, Dummy);
  if Size > 0 then
  begin
    SetLength(Buf, Size);
    GetFileVersionInfo(HidModuleName, DWORD(INVALID_HANDLE_VALUE), Size, @Buf[0]);
    if VerQueryValue(@Buf[0], 'StringFileInfo\040904E4\FileVersion', Pointer(Value), Size) then
      Result := Value;
  end;
end;

// assign OnDeviceChange and immediately fire it

procedure TJvHidDeviceController.SetOnDeviceChange(const Notifier: TNotifyEvent);
begin
  if (TMethod(Notifier).Code <> TMethod(FOnDeviceChange).Code) or
     (TMethod(Notifier).Data <> TMethod(FOnDeviceChange).Data) then
  begin
    FOnDeviceChange := Notifier;
    if not (csLoading in ComponentState) then
      DeviceChange;
  end;
end;

// implement OnEnumerate event

function TJvHidDeviceController.DoEnumerate(HidDev: TJvHidDevice; Idx: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnEnumerate) then
  begin
    HidDev.FIsEnumerated := True;
    Result := FOnEnumerate(HidDev, Idx);
    HidDev.FIsEnumerated := False;
    if not HidDev.IsCheckedOut then
    begin
      HidDev.CloseFile;
      HidDev.CloseFileEx(omhRead);
      HidDev.CloseFileEx(omhWrite);
    end;
  end;
end;

// assign DevPollingDelayTime

procedure TJvHidDeviceController.SetDevPollingDelayTime(const DevTime: Integer);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if DevTime <> FDevPollingDelayTime then
  begin
    // change all DevPollingDelayTime with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if Dev.PollingDelayTime = FDevPollingDelayTime then
        Dev.PollingDelayTime := DevTime;
    end;
    FDevPollingDelayTime := DevTime;
  end;
end;

// assign DevThreadSleepTime

procedure TJvHidDeviceController.SetDevThreadSleepTime(const DevTime: Integer);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if DevTime <> FDevThreadSleepTime then
  begin
    // change all DevThreadSleepTime with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if Dev.ThreadSleepTime = FDevThreadSleepTime then
        Dev.ThreadSleepTime := DevTime;
    end;
    FDevThreadSleepTime := DevTime;
  end;
end;

// assign OnOnDeviceData event

procedure TJvHidDeviceController.SetOnDeviceData(const DataEvent: TJvHidDataEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if (TMethod(DataEvent).Code <> TMethod(FOnDeviceData).Code) or
     (TMethod(DataEvent).Data <> TMethod(FOnDeviceData).Data) then
  begin
    // change all OnData events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if (TMethod(Dev.OnData).Code = TMethod(FOnDeviceData).Code) and
         (TMethod(Dev.OnData).Data = TMethod(FOnDeviceData).Data) then
        Dev.OnData := DataEvent;
    end;
    FOnDeviceData := DataEvent;
  end;
end;

// assign OnDeviceDataError event

procedure TJvHidDeviceController.SetOnDeviceDataError(const DataErrorEvent: TJvHidDataErrorEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if (TMethod(DataErrorEvent).Code <> TMethod(FOnDeviceDataError).Code) or
     (TMethod(DataErrorEvent).Data <> TMethod(FOnDeviceDataError).Data) then
  begin
    // change all OnDataError events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if (TMethod(Dev.OnDataError).Code = TMethod(FOnDeviceDataError).Code) and
         (TMethod(Dev.OnDataError).Data = TMethod(FOnDeviceDataError).Data) then
        Dev.OnDataError := DataErrorEvent;
    end;
    FOnDeviceDataError := DataErrorEvent;
  end;
end;

// assign OnDeviceUnplug event

procedure TJvHidDeviceController.SetOnDeviceUnplug(const Unplugger: TJvHidUnplugEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if (TMethod(Unplugger).Code <> TMethod(FOnDeviceUnplug).Code) or
     (TMethod(Unplugger).Data <> TMethod(FOnDeviceUnplug).Data) then
  begin
    // change all OnUnplug events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := FList.Items[I];
      if (TMethod(Dev.OnUnplug).Code = TMethod(FOnDeviceUnplug).Code) and
         (TMethod(Dev.OnUnplug).Data = TMethod(FOnDeviceUnplug).Data) then
        Dev.OnUnplug := Unplugger;
    end;
    FOnDeviceUnplug := Unplugger;
  end;
end;

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
    HidDev.StartThread;
  end;
end;

// method CheckOutByProductName hands out the first HidDevice with a matching ProductName

function TJvHidDeviceController.CheckOutByProductName(var HidDev: TJvHidDevice;
  const ProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Boolean;
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

// method CheckOutByVendorName hands out the first HidDevice with a matching VendorName

function TJvHidDeviceController.CheckOutByVendorName(var HidDev: TJvHidDevice;
  const VendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Boolean;
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

// method CheckOutByCallback hands out the first HidDevice which is accepted by the Check function
// only checked in devices are presented to the Check function
// the device object is usable like during Enumerate

function TJvHidDeviceController.CheckOutByCallback(var HidDev: TJvHidDevice;
  Check: TJvHidCheckCallback): Boolean;
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Dev := FList[I];
    if not Dev.IsCheckedOut then
    begin
      Dev.FIsEnumerated := True;
      Result := CheckThisOut(HidDev, I, Check(Dev));
      Dev.FIsEnumerated := False;
      if not Result then
      begin
        Dev.CloseFile;
        Dev.CloseFileEx(omhRead);
        Dev.CloseFileEx(omhWrite);
      end;
      if Result then
        Break;
    end;
  end;
end;

// method CheckOutByClass hands out the first HidDevice with a matching Class
// Class comes from the registry (examples: 'Mouse', 'Keyboard')

function TJvHidDeviceController.CheckOutByClass(var HidDev: TJvHidDevice;
  const ClassName: string): Boolean;
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

// method CheckOutByID hands out the first HidDevice with a matching VendorID and ProductID
// Pid = -1 matches all ProductIDs

function TJvHidDeviceController.CheckOutByID(var HidDev: TJvHidDevice;
  const Vid, Pid: Integer): Boolean;
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

// method CheckOutByIndex hands out the HidDevice in the list with the named index
// this is mainly for check out during OnEnumerate

function TJvHidDeviceController.CheckOutByIndex(var HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
begin
  Result := False;
  HidDev := nil;
  if (Idx >= 0) and (Idx < FList.Count) then
    Result := CheckThisOut(HidDev, Idx, True);
end;

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

// method CheckIn hands a checked out HidDevice back in

procedure TJvHidDeviceController.CheckIn(var HidDev: TJvHidDevice);
begin
  if HidDev <> nil then
  begin
    HidDev.StopThread;
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

function TJvHidDeviceController.CountByClass(const ClassName: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and
      (ClassName = TJvHidDevice(FList[I]).PnPInfo.ClassDescr) then
      Inc(Result);
end;

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

function TJvHidDeviceController.CountByProductName(const ProductName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and
      (ProductName = TJvHidDevice(FList[I]).ProductName) then
      Inc(Result);
end;

function TJvHidDeviceController.CountByVendorName(const VendorName: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn and
      (VendorName = TJvHidDevice(FList[I]).VendorName) then
      Inc(Result);
end;

function TJvHidDeviceController.CountByCallback(Check: TJvHidCheckCallback): Integer;
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
  begin
    if TJvHidDevice(FList[I]).IsPluggedIn then
    begin
      Dev := FList[I];
      Dev.FIsEnumerated := True;
      if Check(Dev) then
        Inc(Result);
      Dev.FIsEnumerated := False;
      if not Dev.IsCheckedOut then
      begin
        Dev.CloseFile;
        Dev.CloseFileEx(omhRead);
        Dev.CloseFileEx(omhWrite);
      end;
    end;
  end;
end;

//============================================================================

// a helper function to check the return values just
// like Win32Check
// the functions return the parameter to be transparent

function HidCheck(const RetVal: NTSTATUS): NTSTATUS;
begin
  if RetVal <> HIDP_STATUS_SUCCESS then
    HidError(RetVal);
  Result := RetVal;
end;

function HidCheck(const RetVal: LongBool): LongBool;
begin
  if not RetVal then
    raise EHidClientError.CreateRes(@RsEHIDBooleanError);
  Result := RetVal;
end;

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

function HidErrorString(const RetVal: NTSTATUS): string;
begin
  Result := '';
  // only check HID errors
  if ((RetVal and NTSTATUS($00FF0000)) = HIDP_STATUS_SUCCESS) and
    ((RetVal and NTSTATUS($C0000000)) <> 0) then
  begin
    case RetVal of
      HIDP_STATUS_NULL:
        Result := RsHIDP_STATUS_NULL;
      HIDP_STATUS_INVALID_PREPARSED_DATA:
        Result := RsHIDP_STATUS_INVALID_PREPARSED_DATA;
      HIDP_STATUS_INVALID_REPORT_TYPE:
        Result := RsHIDP_STATUS_INVALID_REPORT_TYPE;
      HIDP_STATUS_INVALID_REPORT_LENGTH:
        Result := RsHIDP_STATUS_INVALID_REPORT_LENGTH;
      HIDP_STATUS_USAGE_NOT_FOUND:
        Result := RsHIDP_STATUS_USAGE_NOT_FOUND;
      HIDP_STATUS_VALUE_OUT_OF_RANGE:
        Result := RsHIDP_STATUS_VALUE_OUT_OF_RANGE;
      HIDP_STATUS_BAD_LOG_PHY_VALUES:
        Result := RsHIDP_STATUS_BAD_LOG_PHY_VALUES;
      HIDP_STATUS_BUFFER_TOO_SMALL:
        Result := RsHIDP_STATUS_BUFFER_TOO_SMALL;
      HIDP_STATUS_INTERNAL_ERROR:
        Result := RsHIDP_STATUS_INTERNAL_ERROR;
      HIDP_STATUS_I8042_TRANS_UNKNOWN:
        Result := RsHIDP_STATUS_I8042_TRANS_UNKNOWN;
      HIDP_STATUS_INCOMPATIBLE_REPORT_ID:
        Result := RsHIDP_STATUS_INCOMPATIBLE_REPORT_ID;
      HIDP_STATUS_NOT_VALUE_ARRAY:
        Result := RsHIDP_STATUS_NOT_VALUE_ARRAY;
      HIDP_STATUS_IS_VALUE_ARRAY:
        Result := RsHIDP_STATUS_IS_VALUE_ARRAY;
      HIDP_STATUS_DATA_INDEX_NOT_FOUND:
        Result := RsHIDP_STATUS_DATA_INDEX_NOT_FOUND;
      HIDP_STATUS_DATA_INDEX_OUT_OF_RANGE:
        Result := RsHIDP_STATUS_DATA_INDEX_OUT_OF_RANGE;
      HIDP_STATUS_BUTTON_NOT_PRESSED:
        Result := RsHIDP_STATUS_BUTTON_NOT_PRESSED;
      HIDP_STATUS_REPORT_DOES_NOT_EXIST:
        Result := RsHIDP_STATUS_REPORT_DOES_NOT_EXIST;
      HIDP_STATUS_NOT_IMPLEMENTED:
        Result := RsHIDP_STATUS_NOT_IMPLEMENTED;
    else
      Result := Format(RsUnknownHIDFmt, [RetVal]);
    end;
    Result := RsHIDErrorPrefix + Result;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
