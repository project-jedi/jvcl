{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComputerInfoEx.PAS, released on 2004-03-07.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
André Snepvangers - better TimeRunning

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description
  A read-only computer info component. Subproperties are created as needed so loading
  at run-time should be pretty fast.

  This component replaces JvComputerInfo, JvKeyboardStates, JvDeviceChanged, JvDirectories
  and JvSystemColors.

Known Issues:
  * ADO version info might not be correct/available on all systems (depending on the ADO version installed)
  * ResetSystemIcons only tested on W2k

-----------------------------------------------------------------------------}
// $Id$

unit JvComputerInfoEx;

{$I jvcl.inc}
{$I windowsonly.inc}

{$HPPEMIT '#pragma link "wininet.lib"'}

interface

uses
  Windows, Messages, SysUtils, ShlObj, ShellAPI,
  {$IFDEF VisualCLX}
  Qt,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls,
  JclWin32, JclSysInfo,
  JvJVCLUtils, JvComponent, JvTypes;

{$HPPEMIT '#include <dbt.h>'}
// these are defined here to avoid including DBT.pas
const
  {$EXTERNALSYM DBT_DEVICEARRIVAL}
  DBT_DEVICEARRIVAL = $8000; { system detected a new device }
  {$EXTERNALSYM DBT_DEVICEREMOVECOMPLETE}
  DBT_DEVICEREMOVECOMPLETE = $8004; { device is gone }
  {$EXTERNALSYM DBT_DEVTYP_VOLUME}
  DBT_DEVTYP_VOLUME = $00000002; { logical volume }
  {$EXTERNALSYM DBTF_MEDIA}
  DBTF_MEDIA = $0001; { media comings and goings }

  // new params for SystemParametersInfo not defined in Windows
  SPI_GETDESKWALLPAPER = $0073;
  {$EXTERNALSYM SPI_GETDESKWALLPAPER}
  SPI_GETMOUSESONAR = $101C;
  {$EXTERNALSYM SPI_GETMOUSESONAR}
  SPI_SETMOUSESONAR = $101D;
  {$EXTERNALSYM SPI_SETMOUSESONAR}
  SPI_GETMOUSECLICKLOCK = $101E;
  {$EXTERNALSYM SPI_GETMOUSECLICKLOCK}
  SPI_SETMOUSECLICKLOCK = $101F;
  {$EXTERNALSYM SPI_SETMOUSECLICKLOCK}
  SPI_GETMOUSEVANISH = $1020;
  {$EXTERNALSYM SPI_GETMOUSEVANISH}
  SPI_SETMOUSEVANISH = $1021;
  {$EXTERNALSYM SPI_SETMOUSEVANISH}
  SPI_GETFLATMENU = $1022;
  {$EXTERNALSYM SPI_GETFLATMENU}
  SPI_SETFLATMENU = $1023;
  {$EXTERNALSYM SPI_SETFLATMENU}
  SPI_GETDROPSHADOW = $1024;
  {$EXTERNALSYM SPI_GETDROPSHADOW}
  SPI_SETDROPSHADOW = $1025;
  {$EXTERNALSYM SPI_SETDROPSHADOW}

  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  {$EXTERNALSYM SPI_GETFOREGROUNDLOCKTIMEOUT}
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
  {$EXTERNALSYM SPI_SETFOREGROUNDLOCKTIMEOUT}
  SPI_GETACTIVEWNDTRKTIMEOUT = $2002;
  {$EXTERNALSYM SPI_GETACTIVEWNDTRKTIMEOUT}
  SPI_SETACTIVEWNDTRKTIMEOUT = $2003;
  {$EXTERNALSYM SPI_SETACTIVEWNDTRKTIMEOUT}
  SPI_GETFOREGROUNDFLASHCOUNT = $2004;
  {$EXTERNALSYM SPI_GETFOREGROUNDFLASHCOUNT}
  SPI_SETFOREGROUNDFLASHCOUNT = $2005;
  {$EXTERNALSYM SPI_SETFOREGROUNDFLASHCOUNT}
  SPI_GETCARETWIDTH = $2006;
  {$EXTERNALSYM SPI_GETCARETWIDTH}
  SPI_SETCARETWIDTH = $2007;
  {$EXTERNALSYM SPI_SETCARETWIDTH}

  SPI_GETMOUSECLICKLOCKTIME = $2008;
  {$EXTERNALSYM SPI_GETMOUSECLICKLOCKTIME}
  SPI_SETMOUSECLICKLOCKTIME = $2009;
  {$EXTERNALSYM SPI_SETMOUSECLICKLOCKTIME}
  SPI_GETFONTSMOOTHINGTYPE = $200A;
  {$EXTERNALSYM SPI_GETFONTSMOOTHINGTYPE}
  SPI_SETFONTSMOOTHINGTYPE = $200B;
  {$EXTERNALSYM SPI_SETFONTSMOOTHINGTYPE}

  { constants for SPI_GETFONTSMOOTHINGTYPE and SPI_SETFONTSMOOTHINGTYPE: }
  FE_FONTSMOOTHINGSTANDARD = $0001;
  {$EXTERNALSYM FE_FONTSMOOTHINGSTANDARD}
  FE_FONTSMOOTHINGCLEARTYPE = $0002;
  {$EXTERNALSYM FE_FONTSMOOTHINGCLEARTYPE}
  FE_FONTSMOOTHINGDOCKING = $8000;
  {$EXTERNALSYM FE_FONTSMOOTHINGDOCKING}

  SPI_GETFONTSMOOTHINGCONTRAST = $200C;
  {$EXTERNALSYM SPI_GETFONTSMOOTHINGCONTRAST}
  SPI_SETFONTSMOOTHINGCONTRAST = $200D;
  {$EXTERNALSYM SPI_SETFONTSMOOTHINGCONTRAST}

  SPI_GETFOCUSBORDERWIDTH = $200E;
  {$EXTERNALSYM SPI_GETFOCUSBORDERWIDTH}
  SPI_SETFOCUSBORDERWIDTH = $200F;
  {$EXTERNALSYM SPI_SETFOCUSBORDERWIDTH}
  SPI_GETFOCUSBORDERHEIGHT = $2010;
  {$EXTERNALSYM SPI_GETFOCUSBORDERHEIGHT}
  SPI_SETFOCUSBORDERHEIGHT = $2011;
  {$EXTERNALSYM SPI_SETFOCUSBORDERHEIGHT}

  {$IFDEF COMPILER5} // This is because there's no point in redefining symbols already defined
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
  {$ENDIF COMPILER5}

type
  EJVCLComputerInfoEx = class(EJVCLException);

  {$EXTERNALSYM PDevBroadcastHdr}
  PDevBroadcastHdr = ^TDevBroadcastHdr;
  {$EXTERNALSYM DEV_BROADCAST_HDR}
  DEV_BROADCAST_HDR = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  TDevBroadcastHdr = DEV_BROADCAST_HDR;

  {$EXTERNALSYM PDevBroadcastVolume}
  PDevBroadcastVolume = ^TDevBroadcastVolume;
  {$EXTERNALSYM DEV_BROADCAST_VOLUME}
  DEV_BROADCAST_VOLUME = packed record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: Word;
  end;
  TDevBroadcastVolume = DEV_BROADCAST_VOLUME;

  TWMDeviceChange = record
    Msg: Cardinal;
    Event: UINT;
    dwData: Pointer;
    Result: Longint;
  end;

  // TJvReadOnlyInfo doesn't have any writeable properties
  TJvReadOnlyInfo = class(TPersistent);

  // TJvWriteableInfo have at least one writeable property
  // and also have a ReadOnly property that controls whether the properties
  // are allowed to change
  TJvWriteableInfo = class(TPersistent)
  private
    FReadOnly: Boolean;
  protected
    property ReadOnly: Boolean read FReadOnly write FReadOnly default True;
  public
    constructor Create;
  end;

  TJvAPMInfo = class(TJvReadOnlyInfo)
  private
    function GetAPMBatteryFlag: TAPMBatteryFlag;
    function GetAPMBatteryFullLifeTime: DWORD;
    function GetAPMBatteryLifePercent: Integer;
    function GetAPMBatteryLifeTime: DWORD;
    function GetAPMLineStatus: TAPMLineStatus;
    procedure SetAPMBatteryFlag(const Value: TAPMBatteryFlag);
    procedure SetAPMBatteryFullLifeTime(const Value: DWORD);
    procedure SetAPMBatteryLifePercent(const Value: Integer);
    procedure SetAPMBatteryLifeTime(const Value: DWORD);
    procedure SetAPMLineStatus(const Value: TAPMLineStatus);
  published
    property LineStatus: TAPMLineStatus read GetAPMLineStatus write SetAPMLineStatus stored False;
    property BatteryFlag: TAPMBatteryFlag read GetAPMBatteryFlag write SetAPMBatteryFlag stored False;
    property BatteryLifePercent: Integer read GetAPMBatteryLifePercent write SetAPMBatteryLifePercent stored False;
    property BatteryLifeTime: DWORD read GetAPMBatteryLifeTime write SetAPMBatteryLifeTime stored False;
    property BatteryFullLifeTime: DWORD read GetAPMBatteryFullLifeTime write SetAPMBatteryFullLifeTime stored False;
  end;

  TJvOSVersionInfo = class(TJvReadOnlyInfo)
  private
    function GetProductType: TNtProductType;
    function GetWinProductID: string;
    function GetWinProductName: string;
    function GetWinVersionBuild: DWORD;
    function GetWinVersionMajor: DWORD;
    function GetWinVersionMinor: DWORD;
    function GetWinVersionCSDString: string;
    procedure SetProductType(const Value: TNtProductType);
    procedure SetWinProductID(const Value: string);
    procedure SetWinProductName(const Value: string);
    procedure SetWinVersionBuild(const Value: DWORD);
    procedure SetWinVersionMajor(const Value: DWORD);
    procedure SetWinVersionMinor(const Value: DWORD);
    procedure SetWinVersionCSDString(const Value: string);
    function GetWinServicePackVersion: DWORD;
    procedure SetWinServicePackVersion(const Value: DWORD);
    function GetOSVersion: TWindowsVersion;
    procedure SetOSVersion(const Value: TWindowsVersion);
  published
    property OSVersion: TWindowsVersion read GetOSVersion write SetOSVersion stored False;
    property ProductType: TNtProductType read GetProductType write SetProductType stored False;
    property ProductID: string read GetWinProductID write SetWinProductID stored False;
    property ProductName: string read GetWinProductName write SetWinProductName stored False;
    property ServicePackVersion: DWORD read GetWinServicePackVersion write SetWinServicePackVersion stored False;
    property VersionBuild: DWORD read GetWinVersionBuild write SetWinVersionBuild stored False;
    property VersionMajor: DWORD read GetWinVersionMajor write SetWinVersionMajor stored False;
    property VersionMinor: DWORD read GetWinVersionMinor write SetWinVersionMinor stored False;
    property VersionCSDString: string read GetWinVersionCSDString write SetWinVersionCSDString stored False;
  end;

  TJvCPUType = (cpuUnknown, cpuIntel, cpuCyrix, cpuAMD, cpuCrusoe);

  TJvCPUInfo = class(TJvReadOnlyInfo)
  private
    function GetCPUInfo: TCPUInfo;
    function GetCPUSpeed: TFreqInfo;
    function GetCPUName: string;
    function GetCPUType: TJvCPUType;
    function GetExTicks: Cardinal;
    function GetFamily: Byte;
    function GetFeatures: Cardinal;
    function GetHasCacheInfo: Boolean;
    function GetHasExtendedInfo: Boolean;
    function GetHasInstruction: Boolean;
    function GetInCycles: Cardinal;
    function GetIsFDIVOK: Boolean;
    function GetManufacturer: string;
    function GetMMX: Boolean;
    function GetModel: Byte;
    function GetNormFreq: Cardinal;
    function GetRawFreq: Cardinal;
    function GetStepping: Byte;
    function GetVendorIDString: string;
    procedure SetCPUName(const Value: string);
    procedure SetCPUType(const Value: TJvCPUType);
    procedure SetExTicks(const Value: Cardinal);
    procedure SetFamily(const Value: Byte);
    procedure SetFeatures(const Value: Cardinal);
    procedure SetHasCacheInfo(const Value: Boolean);
    procedure SetHasExtendedInfo(const Value: Boolean);
    procedure SetHasInstruction(const Value: Boolean);
    procedure SetInCycles(const Value: Cardinal);
    procedure SetIsFDIVOK(const Value: Boolean);
    procedure SetManufacturer(const Value: string);
    procedure SetMMX(const Value: Boolean);
    procedure SetModel(const Value: Byte);
    procedure SetNormFreq(const Value: Cardinal);
    procedure SetRawFreq(const Value: Cardinal);
    procedure SetStepping(const Value: Byte);
    procedure SetVendorIDString(const Value: string);
    function GetProcessorCount: Integer;
    procedure SetProcessorCount(const Value: Integer);
  public
    function IntelSpecific: TIntelSpecific;
    function CyrixSpecific: TCyrixSpecific;
    function AMDSpecific: TAMDSpecific;
  public
    property Features: Cardinal read GetFeatures write SetFeatures stored False;
    property TotalCycles: Cardinal read GetInCycles write SetInCycles stored False;
    property TotalTicks: Cardinal read GetExTicks write SetExTicks stored False;
  published
    // CPUInfo
    property HasInstruction: Boolean read GetHasInstruction write SetHasInstruction stored False;
    property MMX: Boolean read GetMMX write SetMMX stored False;
    property IsFDIVOK: Boolean read GetIsFDIVOK write SetIsFDIVOK stored False;
    property HasCacheInfo: Boolean read GetHasCacheInfo write SetHasCacheInfo stored False;
    property HasExtendedInfo: Boolean read GetHasExtendedInfo write SetHasExtendedInfo stored False;
    property CPUType: TJvCPUType read GetCPUType write SetCPUType stored False;
    property Family: Byte read GetFamily write SetFamily stored False;
    property Model: Byte read GetModel write SetModel stored False;
    property Stepping: Byte read GetStepping write SetStepping stored False;
    property VendorIDString: string read GetVendorIDString write SetVendorIDString stored False;
    property Manufacturer: string read GetManufacturer write SetManufacturer stored False;
    property Name: string read GetCPUName write SetCPUName stored False;
    property ProcessorCount: Integer read GetProcessorCount write SetProcessorCount stored False;
    // FreqInfo
    property RawFreq: Cardinal read GetRawFreq write SetRawFreq stored False;
    property NormFreq: Cardinal read GetNormFreq write SetNormFreq stored False;
  end;

  TJvBIOSInfo = class(TJvReadOnlyInfo)
  private
    function GetBIOSCopyright: string;
    function GetBIOSDate: TDateTime;
    function GetBIOSExtendedInfo: string;
    function GetBIOSName: string;
    procedure SetBIOSCopyright(const Value: string);
    procedure SetBIOSDate(const Value: TDateTime);
    procedure SetBIOSExtendedInfo(const Value: string);
    procedure SetBIOSName(const Value: string);
  published
    property Name: string read GetBIOSName write SetBIOSName stored False;
    property Copyright: string read GetBIOSCopyright write SetBIOSCopyright stored False;
    property ExtendedInfo: string read GetBIOSExtendedInfo write SetBIOSExtendedInfo stored False;
    property Date: TDateTime read GetBIOSDate write SetBIOSDate stored False;
  end;

  TJvSystemFolders = class(TJvWriteableInfo)
    // writeable: Current
  private
    FTrailingPathDelimiter: Boolean;
    function GetCurrent: string;
    function Get(const Index: Integer): string;
    function GetProgramFiles: string;
    function GetWindows: string;
    function GetSystem: string;
    function GetTemp: string;
    procedure SetCommonFiles(const Value: string);
    procedure SetCurrent(const Value: string);
    procedure Put(const Index: Integer; const Value: string);
    procedure SetProgramFiles(const Value: string);
    procedure SetWindows(const Value: string);
    procedure SetSystem(const Value: string);
    procedure SetTemp(const Value: string);
    function GetCommonFiles: string;
    function AdjustPathDelimiter(const S: string): string;
  published
    property TrailingPathDelimiter: Boolean read FTrailingPathDelimiter write FTrailingPathDelimiter default False;
    property CommonFiles: string read GetCommonFiles write SetCommonFiles stored False;
    property Current: string read GetCurrent write SetCurrent stored False;
    property ProgramFiles: string read GetProgramFiles write SetProgramFiles stored False;
    property Windows: string read GetWindows write SetWindows stored False;
    property System: string read GetSystem write SetSystem stored False;
    property Temp: string read GetTemp write SetTemp stored False;
    property Desktop: string index CSIDL_DESKTOP read Get write Put stored False;
    property Programs: string index CSIDL_PROGRAMS read Get write Put stored False;
    property Personal: string index CSIDL_PERSONAL read Get write Put stored False;
    property Favorites: string index CSIDL_FAVORITES read Get write Put stored False;
    property Startup: string index CSIDL_STARTUP read Get write Put stored False;
    property Recent: string index CSIDL_RECENT read Get write Put stored False;
    property SendTo: string index CSIDL_SENDTO read Get write Put stored False;
    property StartMenu: string index CSIDL_STARTMENU read Get write Put stored False;
    property DesktopDirectory: string index CSIDL_DESKTOPDIRECTORY read Get write Put stored False;
    property Nethood: string index CSIDL_NETHOOD read Get write Put stored False;
    property Fonts: string index CSIDL_FONTS read Get write Put stored False;
    property CommonStartmenu: string index CSIDL_COMMON_STARTMENU read Get write Put stored False;
    property CommonPrograms: string index CSIDL_COMMON_PROGRAMS read Get write Put stored False;
    property CommonStartup: string index CSIDL_COMMON_STARTUP read Get write Put stored False;
    property CommonDesktopDirectory: string index CSIDL_COMMON_DESKTOPDIRECTORY read Get write Put stored False;
    property CommonAppData: string index CSIDL_COMMON_APPDATA read Get write Put stored False;
    property AppData: string index CSIDL_APPDATA read Get write Put stored False;
    property Printhood: string index CSIDL_PRINTHOOD read Get write Put stored False;
    property CommonFavorites: string index CSIDL_COMMON_FAVORITES read Get write Put stored False;
    property Templates: string index CSIDL_TEMPLATES read Get write Put stored False;
    property InternetCache: string index CSIDL_INTERNET_CACHE read Get write Put stored False;
    property Cookies: string index CSIDL_COOKIES read Get write Put stored False;
    property History: string index CSIDL_HISTORY read Get write Put stored False;
    property Profile: string index CSIDL_PROFILE read Get write Put stored False;
  end;

  TJvMemInfo = class(TJvReadOnlyInfo)
  private
    function GetFreePageFileMemory: Int64;
    function GetFreePhysicalMemory: Int64;
    function GetFreeVirtualMemory: Int64;
    function GetMaxAppAddress: Integer;
    function GetMemoryLoad: Int64;
    function GetMinAppAddress: Integer;
    function GetSwapFileSize: Int64;
    function GetSwapFileUsage: Integer;
    function GetTotalPageFileMemory: Int64;
    function GetTotalPhysicalMemory: Int64;
    function GetTotalVirtualMemory: Int64;
    procedure SetFreePageFileMemory(const Value: Int64);
    procedure SetFreePhysicalMemory(const Value: Int64);
    procedure SetFreeVirtualMemory(const Value: Int64);
    procedure SetMaxAppAddress(const Value: Integer);
    procedure SetMemoryLoad(const Value: Int64);
    procedure SetMinAppAddress(const Value: Integer);
    procedure SetSwapFileSize(const Value: Int64);
    procedure SetSwapFileUsage(const Value: Integer);
    procedure SetTotalPageFileMemory(const Value: Int64);
    procedure SetTotalPhysicalMemory(const Value: Int64);
    procedure SetTotalVirtualMemory(const Value: Int64);
  published
    property MaxAppAddress: Integer read GetMaxAppAddress write SetMaxAppAddress stored False;
    property MinAppAddress: Integer read GetMinAppAddress write SetMinAppAddress stored False;
    property MemoryLoad: Int64 read GetMemoryLoad write SetMemoryLoad stored False;
    property SwapFileSize: Int64 read GetSwapFileSize write SetSwapFileSize stored False;
    property SwapFileUsage: Integer read GetSwapFileUsage write SetSwapFileUsage stored False;
    property TotalPhysicalMemory: Int64 read GetTotalPhysicalMemory write SetTotalPhysicalMemory stored False;
    property FreePhysicalMemory: Int64 read GetFreePhysicalMemory write SetFreePhysicalMemory stored False;
    property TotalPageFileMemory: Int64 read GetTotalPageFileMemory write SetTotalPageFileMemory stored False;
    property FreePageFileMemory: Int64 read GetFreePageFileMemory write SetFreePageFileMemory stored False;
    property TotalVirtualMemory: Int64 read GetTotalVirtualMemory write SetTotalVirtualMemory stored False;
    property FreeVirtualMemory: Int64 read GetFreeVirtualMemory write SetFreeVirtualMemory stored False;
  end;

  TJvKeyInfo = class(TJvWriteableInfo)
    // writeable: KeyState[], NumLock, ScrollLock, CapsLock
  private
    function GetCapsLockKeyState: Boolean;
    function GetKeyState(const VirtualKey: Cardinal): Boolean;
    function GetNumLockKeyState: Boolean;
    function GetScrollLockKeyState: Boolean;
    procedure SetCapsLockKeyState(const Value: Boolean);
    procedure SetKeyState(const VirtualKey: Cardinal; const Value: Boolean);
    procedure SetNumLockKeyState(const Value: Boolean);
    procedure SetScrollLockKeyState(const Value: Boolean);
  public
    property KeyState[const VirtualKey: Cardinal]: Boolean read GetKeyState write SetKeyState;
  published
    property NumLock: Boolean read GetNumLockKeyState write SetNumLockKeyState stored False;
    property ScrollLock: Boolean read GetScrollLockKeyState write SetScrollLockKeyState stored False;
    property CapsLock: Boolean read GetCapsLockKeyState write SetCapsLockKeyState stored False;
  end;

  TJvIdentification = class(TJvWriteableInfo)
    // writeable: ComputerName (reboot needed), RegisteredCompany, RegisteredOwner, Comment (Win95, 98 and XP only)
  private
    function GetDomainName: string;
    function GetLocalComputerName: string;
    function GetLocalUserName: string;
    function GetLocalWorkgroup: string;
    function GetRegisteredCompany: string;
    function GetRegisteredOwner: string;
    function GetHostIPAddress(const HostName: string): string;
    function GetUserDomainName(const CurUser: string): string;
    function GetVolumeFileSystem(const Drive: string): string;
    function GetVolumeName(const Drive: string): string;
    function GetVolumeSerialNumber(const Drive: string): string;
    function GetIPAddress: string;
    procedure SetDomainName(const Value: string);
    procedure SetLocalComputerName(const Value: string);
    procedure SetLocalUserName(const Value: string);
    procedure SetLocalWorkgroup(const Value: string);
    procedure SetRegisteredCompany(const Value: string);
    procedure SetRegisteredOwner(const Value: string);
    procedure SetIPAddress(const Value: string);
    function GetComment: string;
    procedure SetComment(const Value: string);
  public
    property VolumeName[const Drive: string]: string read GetVolumeName;
    property VolumeSerialNumber[const Drive: string]: string read GetVolumeSerialNumber;
    property VolumeFileSystem[const Drive: string]: string read GetVolumeFileSystem;
    property HostIPAddress[const HostName: string]: string read GetHostIPAddress;
    property UserDomainName[const CurUser: string]: string read GetUserDomainName;
  published
    property IPAddress: string read GetIPAddress write SetIPAddress;
    property LocalComputerName: string read GetLocalComputerName write SetLocalComputerName stored False;
    property LocalUserName: string read GetLocalUserName write SetLocalUserName stored False;
    property LocalWorkgroup: string read GetLocalWorkgroup write SetLocalWorkgroup stored False;
    property DomainName: string read GetDomainName write SetDomainName stored False;
    property RegisteredCompany: string read GetRegisteredCompany write SetRegisteredCompany stored False;
    property RegisteredOwner: string read GetRegisteredOwner write SetRegisteredOwner stored False;
    // NB!!! "Comment" property only supported on Win95, 96 and some NT OS's!
    property Comment: string read GetComment write SetComment stored False;
  end;

  TJvDisplayFlags = set of (dmGrayScale, dmInterlaced);
  TJvScreenMode = class(TJvReadOnlyInfo)
  private
    FHz: DWORD;
    FBitsPerPixel: DWORD;
    FWidth: Integer;
    FHeight: Integer;
    FFlags: TJvDisplayFlags;
    procedure SetBitsPerPixel(const Value: DWORD);
    procedure SetFlags(const Value: TJvDisplayFlags);
    procedure SetHeight(const Value: Integer);
    procedure SetHz(const Value: DWORD);
    procedure SetWidth(const Value: Integer);
  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property BitsPerPixel: DWORD read FBitsPerPixel write SetBitsPerPixel;
    property Hz: DWORD read FHz write SetHz;
    property Flags: TJvDisplayFlags read FFlags write SetFlags;
  end;

  TJvScreenModes = class(TJvReadOnlyInfo)
  private
    FItems: TList;
    FDefaultMode: TJvScreenMode;
    function GetItems(Index: Integer): TJvScreenMode;
    procedure SetItems(Index: Integer; const Value: TJvScreenMode);
    function GetCount: Integer;
  protected
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    property Items[Index: Integer]: TJvScreenMode read GetItems write SetItems; default;
    property Count: Integer read GetCount;
  end;

  TJvScreenInfo = class(TJvWriteableInfo)
    // writeable: BitsPerPixel, Flags, Width, Height, Hz
  private
    FScreenModes: TJvScreenModes;
    function GetBitsPerPixel: DWORD;
    function GetScreenResolution: TPoint;
    function GetScreenFrequency: DWORD;
    function GetScreenHeight: DWORD;
    function GetScreenWidth: DWORD;
    procedure SetBitsPerPixel(const Value: DWORD);
    procedure SetScreenResolution(const Value: TPoint);
    procedure SetScreenFrequency(const Value: DWORD);
    procedure SetScreenHeight(const Value: DWORD);
    procedure SetScreenWidth(const Value: DWORD);
    function GetFlags: TJvDisplayFlags;
    procedure SetFlags(const Value: TJvDisplayFlags);
    function GetScreenModes: TJvScreenModes;
    procedure SetScreenModes(const Value: TJvScreenModes);
  protected
    function GetCurrentMode: TDeviceMode;
    procedure SetCurrentMode(ADeviceMode: TDeviceMode; Flags: DWORD);
  public
    destructor Destroy; override;
    property ScreenResolution: TPoint read GetScreenResolution write SetScreenResolution stored False;
    property ScreenModes: TJvScreenModes read GetScreenModes write SetScreenModes stored False;
  published
    property BitsPerPixel: DWORD read GetBitsPerPixel write SetBitsPerPixel stored False;
    property Flags: TJvDisplayFlags read GetFlags write SetFlags;
    property Width: DWORD read GetScreenWidth write SetScreenWidth stored False;
    property Height: DWORD read GetScreenHeight write SetScreenHeight stored False;
    property Hz: DWORD read GetScreenFrequency write SetScreenFrequency stored False;
  end;

  TJvAppVersions = class(TJvReadOnlyInfo)
  private
    function GetADOVersion: string;
    function GetBDELocation: string;
    function GetBDEVersion: string;
    function GetIEVersion: string;
    function GetOpenGLVersion: string;
    function GetDirectXVersion: string;
    procedure SetADOVersion(const Value: string);
    procedure SetBDEVersion(const Value: string);
    procedure SetIEVersion(const Value: string);
    procedure SetOpenGLVersion(const Value: string);
    procedure SetDirectXVersion(const Value: string);
  published
    property DirectX: string read GetDirectXVersion write SetDirectXVersion stored False;
    property OpenGL: string read GetOpenGLVersion write SetOpenGLVersion stored False;
    property BDE: string read GetBDEVersion write SetBDEVersion stored False;
    property ADO: string read GetADOVersion write SetADOVersion stored False;
    property InternetExplorer: string read GetIEVersion write SetIEVersion stored False;
  end;

  TJvHWDockInfo = set of (diDocked, diUndocked, diUserSupplied, diUserDocked, diUserUndocked);

  TJvHardwareProfile = class(TJvReadOnlyInfo)
  private
    function GetDockInfo: TJvHWDockInfo;
    function GetGUID: string;
    function GetName: string;
    function GetNativeType: HW_PROFILE_INFO;
    procedure SetDockInfo(const Value: TJvHWDockInfo);
    procedure SetGUID(const Value: string);
    procedure SetName(const Value: string);
  public
    property NativeType: HW_PROFILE_INFO read GetNativeType;
  published
    property GUID: string read GetGUID write SetGUID stored False;
    property Name: string read GetName write SetName stored False;
    property DockInfo: TJvHWDockInfo read GetDockInfo write SetDockInfo stored False;
  end;

  TJvWallpaperStyle = (wsCenter, wsUnused, wsStretch);
  TJvMiscInfo = class(TJvWriteableInfo)
    // writeable: ScreenSaver, Pattern, Wallpaper, WallpaperStyle, WallpaperTiled
    // CurrentColorScheme
  private
    FVersions: TJvAppVersions;
    FHardwareProfile: TJvHardwareProfile;
    FColorSchemes: TStrings;
    function GetIsOnline: Boolean;
    function GetScreenSaver: string;
    function GetDVDRegion: Integer;
    function GetTimeRunning: Int64;
    function GetTimeRunningAsString: string;
    function GetNetBIOS: Boolean;
    function GetVersions: TJvAppVersions;
    function GetHardwareProfile: TJvHardwareProfile;
    function GetPattern: string;
    function GetWallpaper: string;
    function GetWallpaperStyle: TJvWallpaperStyle;
    function GetWallpaperTiled: Boolean;
    function GetColorSchemes: TStrings;
    function GetCurrentColorScheme: string;
    procedure SetIsOnline(const Value: Boolean);
    procedure SetScreenSaver(const Value: string);
    procedure SetDVDRegion(const Value: Integer);
    procedure SetTimeRunning(const Value: Int64);
    procedure SetTimeRunningAsString(const Value: string);
    procedure SetNetBIOS(const Value: Boolean);
    procedure SetVersions(const Value: TJvAppVersions);
    procedure SetHardwareProfile(const Value: TJvHardwareProfile);
    procedure SetPattern(const Value: string);
    procedure SetWallpaper(const Value: string);
    procedure SetWallpaperStyle(const Value: TJvWallpaperStyle);
    procedure SetWallpaperTiled(const Value: Boolean);
    procedure SetColorSchemes(const Value: TStrings);
    procedure SetCurrentColorScheme(const Value: string);
  public
    destructor Destroy; override;
  published
    property TimeRunning: Int64 read GetTimeRunning write SetTimeRunning stored False;
    property TimeRunningAsString: string read GetTimeRunningAsString write SetTimeRunningAsString stored False;
    property Online: Boolean read GetIsOnline write SetIsOnline stored False;
    property ScreenSaver: string read GetScreenSaver write SetScreenSaver stored False;
    property DVDRegion: Integer read GetDVDRegion write SetDVDRegion stored False;
    property NetBIOS: Boolean read GetNetBIOS write SetNetBIOS stored False;
    property WallpaperTiled: Boolean read GetWallpaperTiled write SetWallpaperTiled stored False;
    property Wallpaper: string read GetWallpaper write SetWallpaper stored False;
    property WallpaperStyle: TJvWallpaperStyle read GetWallpaperStyle write SetWallpaperStyle stored False;
    property Pattern: string read GetPattern write SetPattern stored False;
    property ColorSchemes: TStrings read GetColorSchemes write SetColorSchemes stored False;
    property CurrentColorScheme: string read GetCurrentColorScheme write SetCurrentColorScheme stored False;

    property Versions: TJvAppVersions read GetVersions write SetVersions stored False;
    property HardwareProfile: TJvHardwareProfile read GetHardwareProfile write SetHardwareProfile stored False;
  end;

  TJvCleanBoot = (cbNormal, cbFailSafe, cbFailSafeNetwork);
  TJvWindowsArrange = set of (waDown, waLeft, waRight, waUp, waBottomLeft,
    waBottomRight, waHide, waTopLeft, waTopRight);

  TJvMetricsInfo = class(TJvWriteableInfo)
    // writeable: CursorX, CursorY, CaretX, CaretY
  private
    function GetBoolMetrics(const Index: Integer): Boolean;
    function GetMetrics(const Index: Integer): Integer;
    function GetArrange: TJvWindowsArrange;
    function GetCleanBoot: TJvCleanBoot;
    function GetCaretBlinkTime: DWORD;
    function GetCaretPos(const Index: Integer): Integer;
    function GetCursorPos(const Index: Integer): Integer;
    procedure SetBoolMetrics(const Index: Integer; const Value: Boolean);
    procedure SetMetrics(const Index, Value: Integer);
    procedure SetArrange(const Value: TJvWindowsArrange);
    procedure SetCleanBoot(const Value: TJvCleanBoot);
    procedure SetCaretBlinkTime(const Value: DWORD);
    procedure SetCaretPos(const Index, Value: Integer);
    procedure SetCursorPos(const Index, Value: Integer);
    function GetDialogBaseUnits: Integer;
    procedure SetDialogBaseUnits(const Value: Integer);
    function GetACP: Integer;
    function GetDoubleClickTime: Integer;
    function GetOEMCP: Integer;
    procedure SetACP(const Value: Integer);
    procedure SetDoubleClickTime(const Value: Integer);
    procedure SetOEMCP(const Value: Integer);
  published
    property Arrange: TJvWindowsArrange read GetArrange write SetArrange stored False;
    property CleanBoot: TJvCleanBoot read GetCleanBoot write SetCleanBoot stored False;
    property MouseButtons: Integer index SM_CMOUSEBUTTONS read GetMetrics write SetMetrics stored False;
    property BorderWidth: Integer index SM_CXBORDER read GetMetrics write SetMetrics stored False;
    property BorderHeight: Integer index SM_CYBORDER read GetMetrics write SetMetrics stored False;
    property CursorWidth: Integer index SM_CXCURSOR read GetMetrics write SetMetrics stored False;
    property CursorHeight: Integer index SM_CYCURSOR read GetMetrics write SetMetrics stored False;
    property CaretBlinkTime: LongWord read GetCaretBlinkTime write SetCaretBlinkTime stored False;
    property CaretX: Integer index 0 read GetCaretPos write SetCaretPos stored False;
    property CaretY: Integer index 1 read GetCaretPos write SetCaretPos stored False;
    property CursorX: Integer index 0 read GetCursorPos write SetCursorPos stored False;
    property CursorY: Integer index 1 read GetCursorPos write SetCursorPos stored False;
    property CodePageANSI: Integer read GetACP write SetACP stored False;
    property CodePageOEM: Integer read GetOEMCP write SetOEMCP stored False;
    property DialogBaseUnits: Integer read GetDialogBaseUnits write SetDialogBaseUnits stored False;
    property DialogFrameWidth: Integer index SM_CXDLGFRAME read GetMetrics write SetMetrics stored False;
    property DialogFrameHeight: Integer index SM_CYDLGFRAME read GetMetrics write SetMetrics stored False;
    property DoubleClickWidth: Integer index SM_CXDOUBLECLK read GetMetrics write SetMetrics stored False;
    property DoubleClickHeight: Integer index SM_CYDOUBLECLK read GetMetrics write SetMetrics stored False;
    property DoubleClickTime: Integer read GetDoubleClickTime write SetDoubleClickTime stored False;
    property DragWidth: Integer index SM_CXDRAG read GetMetrics write SetMetrics stored False;
    property DragHeight: Integer index SM_CYDRAG read GetMetrics write SetMetrics stored False;
    property EdgeWidth: Integer index SM_CXEDGE read GetMetrics write SetMetrics stored False;
    property EdgeHeight: Integer index SM_CYEDGE read GetMetrics write SetMetrics stored False;
    property FixedFrameWidth: Integer index SM_CXFIXEDFRAME read GetMetrics write SetMetrics stored False;
    property FixedFrameHeight: Integer index SM_CYFIXEDFRAME read GetMetrics write SetMetrics stored False;
    property FrameWidth: Integer index SM_CXFRAME read GetMetrics write SetMetrics stored False;
    property FrameHeight: Integer index SM_CYFRAME read GetMetrics write SetMetrics stored False;
    property ScreenClientWidth: Integer index SM_CXFULLSCREEN read GetMetrics write SetMetrics stored False;
    property ScreenClientHeight: Integer index SM_CYFULLSCREEN read GetMetrics write SetMetrics stored False;
    property ScreenWidth: Integer index SM_CXSCREEN read GetMetrics write SetMetrics stored False;
    property ScreenHeight: Integer index SM_CYSCREEN read GetMetrics write SetMetrics stored False;
    property ScrollArrowWidth: Integer index SM_CXHSCROLL read GetMetrics write SetMetrics stored False;
    property ScrollArrowHeight: Integer index SM_CYHSCROLL read GetMetrics write SetMetrics stored False;
    property ScrollThumbWidth: Integer index SM_CXHTHUMB read GetMetrics write SetMetrics stored False;
    property ScrollThumbHeight: Integer index SM_CYVTHUMB read GetMetrics write SetMetrics stored False;
    property ScrollWidth: Integer index SM_CXVSCROLL read GetMetrics write SetMetrics stored False;
    property ScrollHeight: Integer index SM_CYVSCROLL read GetMetrics write SetMetrics stored False;
    property IconWidth: Integer index SM_CXICON read GetMetrics write SetMetrics stored False;
    property IconHeight: Integer index SM_CYICON read GetMetrics write SetMetrics stored False;
    property SmallIconWidth: Integer index SM_CXSMICON read GetMetrics write SetMetrics stored False;
    property SmallIconHeight: Integer index SM_CYSMICON read GetMetrics write SetMetrics stored False;
    property IconSpacingWidth: Integer index SM_CXICONSPACING read GetMetrics write SetMetrics stored False;
    property IconSpacingHeight: Integer index SM_CYICONSPACING read GetMetrics write SetMetrics stored False;
    property MaximizedWindowWidth: Integer index SM_CXMAXIMIZED read GetMetrics write SetMetrics stored False;
    property MaximizedWindowHeight: Integer index SM_CYMAXIMIZED read GetMetrics write SetMetrics stored False;
    property MinimizedWindowWidth: Integer index SM_CXMINIMIZED read GetMetrics write SetMetrics stored False;
    property MinimizedWindowHeight: Integer index SM_CYMINIMIZED read GetMetrics write SetMetrics stored False;
    property MaxDragWindowWidth: Integer index SM_CXMAXTRACK read GetMetrics write SetMetrics stored False;
    property MaxDragWindowHeight: Integer index SM_CYMAXTRACK read GetMetrics write SetMetrics stored False;
    property MinDragWindowWidth: Integer index SM_CXMINTRACK read GetMetrics write SetMetrics stored False;
    property MinDragWindowHeight: Integer index SM_CYMINTRACK read GetMetrics write SetMetrics stored False;
    property MinWindowWidth: Integer index SM_CXMIN read GetMetrics write SetMetrics stored False;
    property MinWindowHeight: Integer index SM_CYMIN read GetMetrics write SetMetrics stored False;
    property MenuCheckWidth: Integer index SM_CXMENUCHECK read GetMetrics write SetMetrics stored False;
    property MenuCheckHeight: Integer index SM_CYMENUCHECK read GetMetrics write SetMetrics stored False;
    property MenuButtonWidth: Integer index SM_CXMENUSIZE read GetMetrics write SetMetrics stored False;
    property MenuButtonHeight: Integer index SM_CYMENUSIZE read GetMetrics write SetMetrics stored False;
    property MinimizedWindowSpacingWidth: Integer index SM_CXMINSPACING read GetMetrics write SetMetrics stored False;
    property MinimizedWindowSpacingHeight: Integer index SM_CYMINSPACING read GetMetrics write SetMetrics stored False;
    property CaptionButtonWidth: Integer index SM_CXSIZE read GetMetrics write SetMetrics stored False;
    property CaptionButtonheight: Integer index SM_CYSIZE read GetMetrics write SetMetrics stored False;
    property ResizeBorderWidth: Integer index SM_CXSIZEFRAME read GetMetrics write SetMetrics stored False;
    property ResizeBorderHeight: Integer index SM_CYSIZEFRAME read GetMetrics write SetMetrics stored False;
    property SmallCaptionButtonWidth: Integer index SM_CXSMSIZE read GetMetrics write SetMetrics stored False;
    property SmallCaptionButtonHeight: Integer index SM_CYSMSIZE read GetMetrics write SetMetrics stored False;
    property WindowCaptionHeight: Integer index SM_CYCAPTION read GetMetrics write SetMetrics stored False;
    property SmallWindowCaptionHeight: Integer index SM_CYSMCAPTION read GetMetrics write SetMetrics stored False;
    property KanjiWindowHeight: Integer index SM_CYKANJIWINDOW read GetMetrics write SetMetrics stored False;
    property MenuItemHeight: Integer index SM_CYMENU read GetMetrics write SetMetrics stored False;
    property DBCSEnabled: Boolean index SM_DBCSENABLED read GetBoolMetrics write SetBoolMetrics stored False;
    property Debug: Boolean index SM_DEBUG read GetBoolMetrics write SetBoolMetrics stored False;
    property MenuRightAligned: Boolean index SM_MENUDROPALIGNMENT read GetBoolMetrics write SetBoolMetrics stored False;
    property MidEastEnabled: Boolean index SM_MIDEASTENABLED read GetBoolMetrics write SetBoolMetrics stored False;
    property MousePresent: Boolean index SM_MOUSEPRESENT read GetBoolMetrics write SetBoolMetrics stored False;
    property MouseWheelPresent: Boolean index SM_MOUSEWHEELPRESENT read GetBoolMetrics write SetBoolMetrics stored
      False;
    property Networked: Boolean index SM_NETWORK read GetBoolMetrics write SetBoolMetrics stored False;
    property PenWindows: Boolean index SM_PENWINDOWS read GetBoolMetrics write SetBoolMetrics stored False;
    property Secure: Boolean index SM_SECURE read GetBoolMetrics write SetBoolMetrics stored False;
    property ShowSounds: Boolean index SM_SHOWSOUNDS read GetBoolMetrics write SetBoolMetrics stored False;
    property SlowMachine: Boolean index SM_SLOWMACHINE read GetBoolMetrics write SetBoolMetrics stored False;
    property MouseButtonsSwapped: Boolean index SM_SWAPBUTTON read GetBoolMetrics write SetBoolMetrics stored False;
  end;

  TJvAccessTimeOutFlags = set of (atfOnOffFeedback, atfTimeOutOn);

  TJvAccessTimeOut = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetFlags: TJvAccessTimeOutFlags;
    function GetNativeType: ACCESSTIMEOUT;
    function GetTimeOutMS: DWORD;
    procedure SetFlags(const Value: TJvAccessTimeOutFlags);
    procedure SetTimeOutMS(const Value: DWORD);
    procedure SetNativeType(Value: ACCESSTIMEOUT);
  public
    property NativeType: ACCESSTIMEOUT read GetNativeType write SetNativeType;
  published
    property TimeOutMS: DWORD read GetTimeOutMS write SetTimeOutMS stored False;
    property Flags: TJvAccessTimeOutFlags read GetFlags write SetFlags stored False;
  end;

  TJvFilterKeyFlags = set of (fkfAvailable, fkfClickOn, fkfFilterKeysOn, fkfHotkeyActive,
    fkfHotkeySound, fkfConfirmHotkey, fkfIndicator);

  TJvFilterKeys = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetBounceMSec: DWORD;
    function GetDelayMSec: DWORD;
    function GetFlags: TJvFilterKeyFlags;
    function GetNativeType: FILTERKEYS;
    function GetRepeatMSec: DWORD;
    function GetWaitMSec: DWORD;
    procedure SetBounceMSec(const Value: DWORD);
    procedure SetDelayMSec(const Value: DWORD);
    procedure SetFlags(const Value: TJvFilterKeyFlags);
    procedure SetNativeType(Value: FILTERKEYS);
    procedure SetRepeatMSec(const Value: DWORD);
    procedure SetWaitMSec(const Value: DWORD);
  public
    property NativeType: FILTERKEYS read GetNativeType write SetNativeType;
  published
    property Flags: TJvFilterKeyFlags read GetFlags write SetFlags stored False;
    property WaitMSec: DWORD read GetWaitMSec write SetWaitMSec stored False;
    property DelayMSec: DWORD read GetDelayMSec write SetDelayMSec stored False;
    property RepeatMSec: DWORD read GetRepeatMSec write SetRepeatMSec stored False;
    property BounceMSec: DWORD read GetBounceMSec write SetBounceMSec stored False;
  end;

  TJvHighContrastFlags = set of (hcfAvailable, hcfConfirmHotKey, hcfHighContrastOn,
    hcfHotkeyActive, hcfHotkeyAvailable, hcfHotkeySound, hcfIndicator);

  TJvHighContrast = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetDefaultScheme: string;
    function GetFlags: TJvHighContrastFlags;
    function GetNativeType: HIGHCONTRAST;
    procedure SetDefaultScheme(const Value: string);
    procedure SetFlags(const Value: TJvHighContrastFlags);
    procedure SetNativeType(Value: HIGHCONTRAST);
  public
    property NativeType: HIGHCONTRAST read GetNativeType write SetNativeType;
  published
    property Flags: TJvHighContrastFlags read GetFlags write SetFlags stored False;
    property DefaultScheme: string read GetDefaultScheme write SetDefaultScheme stored False;
  end;

  TJvIconMetrics = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    FFont: TFont;
    function GetFont: TFont;
    function GetHorzSpacing: Integer;
    function GetNativeType: ICONMETRICS;
    function GetTitleWrap: Boolean;
    function GetVertSpacing: Integer;
    procedure SetFont(const Value: TFont);
    procedure SetHorzSpacing(const Value: Integer);
    procedure SetNativeType(Value: ICONMETRICS);
    procedure SetTitleWrap(const Value: Boolean);
    procedure SetVertSpacing(const Value: Integer);
  public
    destructor Destroy; override;
    property NativeType: ICONMETRICS read GetNativeType write SetNativeType;
  published
    property VertSpacing: Integer read GetVertSpacing write SetVertSpacing stored False;
    property HorzSpacing: Integer read GetHorzSpacing write SetHorzSpacing stored False;
    property TitleWrap: Boolean read GetTitleWrap write SetTitleWrap stored False;
    property Font: TFont read GetFont write SetFont stored False;
  end;

  TJvMinimizedMetrics = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetArrange: TJvWindowsArrange;
    function GetHorzGap: Integer;
    function GetNativeType: MINIMIZEDMETRICS;
    function GetVertGap: Integer;
    function GetWidth: Integer;
    procedure SetArrange(const Value: TJvWindowsArrange);
    procedure SetHorzGap(const Value: Integer);
    procedure SetNativeType(Value: MINIMIZEDMETRICS);
    procedure SetVertGap(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    property NativeType: MINIMIZEDMETRICS read GetNativeType write SetNativeType;
  published
    property Width: Integer read GetWidth write SetWidth stored False;
    property HorzGap: Integer read GetHorzGap write SetHorzGap stored False;
    property VertGap: Integer read GetVertGap write SetVertGap stored False;
    property Arrange: TJvWindowsArrange read GetArrange write SetArrange stored False;
  end;

  TJvMouseKeysFlags = set of (mkfAvailable, mkfConfirmHotKey, mkfHotkeyActive, mkfHotkeySound, mkfIndicator,
    mkfMouseKeysOn, mkfModifiers, mkfReplaceNumbers);

  TJvMouseKeys = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetCtrlSpeed: DWORD;
    function GetFlags: TJvMouseKeysFlags;
    function GetMaxSpeed: DWORD;
    function GetNativeType: MOUSEKEYS;
    function GetTimeToMaxSpeed: DWORD;
    procedure SetCtrlSpeed(const Value: DWORD);
    procedure SetFlags(const Value: TJvMouseKeysFlags);
    procedure SetMaxSpeed(const Value: DWORD);
    procedure SetNativeType(Value: MOUSEKEYS);
    procedure SetTimeToMaxSpeed(const Value: DWORD);
  public
    property NativeType: MOUSEKEYS read GetNativeType write SetNativeType;
  published
    property Flags: TJvMouseKeysFlags read GetFlags write SetFlags stored False;
    property MaxSpeed: DWORD read GetMaxSpeed write SetMaxSpeed stored False;
    property TimeToMaxSpeed: DWORD read GetTimeToMaxSpeed write SetTimeToMaxSpeed stored False;
    property CtrlSpeed: DWORD read GetCtrlSpeed write SetCtrlSpeed stored False;
  end;

  TJvNonClientMetrics = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    FCaptionFont: TFont;
    FMenuFont: TFont;
    FMessageFont: TFont;
    FStatusFont: TFont;
    FSmallCaptionFont: TFont;
    function GetBorderWidth: Integer;
    function GetCaptionFont: TFont;
    function GetCaptionHeight: Integer;
    function GetCaptionWidth: Integer;
    function GetMenuFont: TFont;
    function GetMenuHeight: Integer;
    function GetMenuWidth: Integer;
    function GetMessageFont: TFont;
    function GetNativeType: NONCLIENTMETRICS;
    function GetScrollHeight: Integer;
    function GetScrollWidth: Integer;
    function GetSmallCaptionFont: TFont;
    function GetSmallCaptionHeight: Integer;
    function GetSmallCaptionWidth: Integer;
    function GetStatusFont: TFont;
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionHeight(const Value: Integer);
    procedure SetCaptionWidth(const Value: Integer);
    procedure SetMenuFont(const Value: TFont);
    procedure SetMenuHeight(const Value: Integer);
    procedure SetMenuWidth(const Value: Integer);
    procedure SetMessageFont(const Value: TFont);
    procedure SetNativeType(Value: NONCLIENTMETRICS);
    procedure SetScrollHeight(const Value: Integer);
    procedure SetScrollWidth(const Value: Integer);
    procedure SetSmallCaptionFont(const Value: TFont);
    procedure SetSmallCaptionHeight(const Value: Integer);
    procedure SetSmallCaptionWidth(const Value: Integer);
    procedure SetStatusFont(const Value: TFont);
  public
    property NativeType: NONCLIENTMETRICS read GetNativeType write SetNativeType;
    destructor Destroy; override;
  published
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth stored False;
    property ScrollWidth: Integer read GetScrollWidth write SetScrollWidth stored False;
    property ScrollHeight: Integer read GetScrollHeight write SetScrollHeight stored False;
    property CaptionWidth: Integer read GetCaptionWidth write SetCaptionWidth stored False;
    property CaptionHeight: Integer read GetCaptionHeight write SetCaptionHeight stored False;
    property CaptionFont: TFont read GetCaptionFont write SetCaptionFont stored False;
    property SmallCaptionWidth: Integer read GetSmallCaptionWidth write SetSmallCaptionWidth stored False;
    property SmallCaptionHeight: Integer read GetSmallCaptionHeight write SetSmallCaptionHeight stored False;
    property SmallCaptionFont: TFont read GetSmallCaptionFont write SetSmallCaptionFont stored False;
    property MenuWidth: Integer read GetMenuWidth write SetMenuWidth stored False;
    property MenuHeight: Integer read GetMenuHeight write SetMenuHeight stored False;
    property MenuFont: TFont read GetMenuFont write SetMenuFont stored False;
    property StatusFont: TFont read GetStatusFont write SetStatusFont stored False;
    property MessageFont: TFont read GetMessageFont write SetMessageFont stored False;
  end;

  TJvSerialKeysFlags = set of (serkfAvailable, serkfIndicator, serkfSerialKeysOn);
  TJvSerialKeysPortState = (psSerialKeysIgnored, psSerialKeysAware, psSerialKeysAlways);
  TJvSerialKeys = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetActivePort: string;
    function GetBaudRate: DWORD;
    function GetFlags: TJvSerialKeysFlags;
    function GetNativeType: SERIALKEYS;
    function GetPortState: TJvSerialKeysPortState;
    procedure SetActivePort(const Value: string);
    procedure SetBaudRate(const Value: DWORD);
    procedure SetFlags(const Value: TJvSerialKeysFlags);
    procedure SetNativeType(Value: SERIALKEYS);
    procedure SetPortState(const Value: TJvSerialKeysPortState);
    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    property NativeType: SERIALKEYS read GetNativeType write SetNativeType;
  published
    property Active: Boolean read GetActive write SetActive stored False;
    property Flags: TJvSerialKeysFlags read GetFlags write SetFlags stored False;
    property ActivePort: string read GetActivePort write SetActivePort stored False;
    property Port: string read GetPort write SetPort stored False;
    property BaudRate: DWORD read GetBaudRate write SetBaudRate stored False;
    property PortState: TJvSerialKeysPortState read GetPortState write SetPortState stored False;
  end;

  TJvSoundSentryFlags = set of (ssfAvailable, ssfSoundSentryOn, ssfIndicator);
  TJvSoundSentryTextEffect = (sstfNone, sstfChars, sstfBorder, sstfDisplay);
  TJvSoundSentryGrafEffect = (ssgfNone, ssgfDisplay);
  TJvSoundSentryWindowsEffect = (sswfNone, sswfTitle, sswfWindow, sswfDisplay, sswfCustom);

  TJvSoundSentry = class(TJvWriteableInfo)
    // writeable: all (using NativeType recommended)
  private
    function GetFlags: TJvSoundSentryFlags;
    function GetGrafEffect: TJvSoundSentryGrafEffect;
    function GetGrafEffectColor: TColor;
    function GetGrafEffectMSec: DWORD;
    function GetNativeType: SOUNDSENTRY;
    function GetTextEffect: TJvSoundSentryTextEffect;
    function GetTextEffectColor: TColor;
    function GetTextEffectMSec: DWORD;
    function GetWindowsEffect: TJvSoundSentryWindowsEffect;
    function GetWindowsEffectDLL: string;
    function GetWindowsEffectMSec: DWORD;
    procedure SetFlags(const Value: TJvSoundSentryFlags);
    procedure SetGrafEffect(const Value: TJvSoundSentryGrafEffect);
    procedure SetGrafEffectColor(const Value: TColor);
    procedure SetGrafEffectMSec(const Value: DWORD);
    procedure SetNativeType(Value: SOUNDSENTRY);
    procedure SetTextEffect(const Value: TJvSoundSentryTextEffect);
    procedure SetTextEffectColor(const Value: TColor);
    procedure SetTextEffectMSec(const Value: DWORD);
    procedure SetWindowsEffect(const Value: TJvSoundSentryWindowsEffect);
    procedure SetWindowsEffectDLL(const Value: string);
    procedure SetWindowsEffectMSec(const Value: DWORD);
  public
    property NativeType: SOUNDSENTRY read GetNativeType write SetNativeType;
  published
    property Flags: TJvSoundSentryFlags read GetFlags write SetFlags stored False;
    property TextEffect: TJvSoundSentryTextEffect read GetTextEffect write SetTextEffect stored False;
    property TextEffectMSec: DWORD read GetTextEffectMSec write SetTextEffectMSec stored False;
    property TextEffectColor: TColor read GetTextEffectColor write SetTextEffectColor stored False;
    property GrafEffect: TJvSoundSentryGrafEffect read GetGrafEffect write SetGrafEffect stored False;
    property GrafEffectMSec: DWORD read GetGrafEffectMSec write SetGrafEffectMSec stored False;
    property GrafEffectColor: TColor read GetGrafEffectColor write SetGrafEffectColor stored False;
    property WindowsEffect: TJvSoundSentryWindowsEffect read GetWindowsEffect write SetWindowsEffect stored False;
    property WindowsEffectMSec: DWORD read GetWindowsEffectMSec write SetWindowsEffectMSec stored False;
    property WindowsEffectDLL: string read GetWindowsEffectDLL write SetWindowsEffectDLL stored False;
  end;

  TJvStickyKeysFlags = set of (skfStickyKeysOn, skfAvailable, skfHotkeyActive, skfConfirmHotkey,
    skfHotkeySound, skfIndicator, skfAudibleFeedback, skfTriState, skfTwoKeysOff,
    skfLeftAltLatched, skfLeftCtrlLatched, skfLeftShiftLatched,
    skfRightAltLatched, skfRightCtrlLatched, skfRightShiftLatched,
    skfLeftWinLatched, skfRightWinLatched,
    skfLeftAltLocked, skfLeftCtrlLocked, skfLeftShiftLocked,
    skfRightAltLocked, skfRightCtrlLocked, skfRightShiftLocked,
    skfLeftWinLocked, skfRightWinLocked
    );
  TJvToggleKeysFlags = set of (tkfAvailable, tkfConfirmHotkey, tkfHotkeyActive, tkfHotkeySound, tkfToggleKeysOn);
  TJvFontSmoothingType = (fstStandard, fstClearType, fstDocking);

  TJvSystemParametersInfo = class(TJvWriteableInfo)
    // writeable: all except ScreenSaverRunning, WindowsExtension
  private
    FAccessTimeOut: TJvAccessTimeOut;
    FFilterKeys: TJvFilterKeys;
    FHighContrast: TJvHighContrast;
    FIconMetrics: TJvIconMetrics;
    FMinimizedMetrics: TJvMinimizedMetrics;
    FMouseKeys: TJvMouseKeys;
    FNonClientMetrics: TJvNonClientMetrics;
    FSerialKeys: TJvSerialKeys;
    FSoundSentry: TJvSoundSentry;
    FIconTitleFont: TFont;
    FWorkArea: TJvRect;
    FMap: array of TPoint;
    function GetAccessTimeOut: TJvAccessTimeOut;
    function GetFilterKeys: TJvFilterKeys;
    function GetHighContrast: TJvHighContrast;
    function GetIconMetrics: TJvIconMetrics;
    function GetIconTitleFont: TFont;
    function GetMinimizedMetrics: TJvMinimizedMetrics;
    function GetMouseKeys: TJvMouseKeys;
    function GetNonClientMetrics: TJvNonClientMetrics;
    function GetSerialKeys: TJvSerialKeys;
    function GetSoundSentry: TJvSoundSentry;
    function GetStickyKeys: TJvStickyKeysFlags;
    function GetToggleKeys: TJvToggleKeysFlags;
    function GetWorkArea: TJvRect;
    function GetIntInfo(const Index: Integer): Integer;
    function GetBoolInfo(const Index: Integer): Boolean;
    function GetMouseInfo(const Index: Integer): Integer;
    function GetAnimationInfo: Boolean;
    function GetKeyboardLayoutName: string;
    function GetDeskWallpaper: string;
    function GetFontSmoothingType: TJvFontSmoothingType;
    function GetIconSpacing(const Index: Integer): Integer;
    procedure SetBoolInfo(const Index: Integer; const Value: Boolean);
    procedure SetIntInfo(const Index, Value: Integer);
    procedure SetAccessTimeOut(const Value: TJvAccessTimeOut);
    procedure SetFilterKeys(const Value: TJvFilterKeys);
    procedure SetHighContrast(const Value: TJvHighContrast);
    procedure SetIconMetrics(const Value: TJvIconMetrics);
    procedure SetIconTitleFont(const Value: TFont);
    procedure SetMinimizedMetrics(const Value: TJvMinimizedMetrics);
    procedure SetMouseKeys(const Value: TJvMouseKeys);
    procedure SetNonClientMetrics(const Value: TJvNonClientMetrics);
    procedure SetSerialKeys(const Value: TJvSerialKeys);
    procedure SetSoundSentry(const Value: TJvSoundSentry);
    procedure SetStickyKeys(const Value: TJvStickyKeysFlags);
    procedure SetToggleKeys(const Value: TJvToggleKeysFlags);
    procedure SetWorkArea(const Value: TJvRect);
    procedure SetMouseInfo(const Index, Value: Integer);
    procedure SetAnimationInfo(const Value: Boolean);
    procedure SetKeyboardLayoutName(const Value: string);
    procedure SetDeskWallpaper(const Value: string);
    procedure SetFontSmoothingType(const Value: TJvFontSmoothingType);
    procedure SetIconSpacing(const Index, Value: Integer);
    procedure InitMap;
    function MapToSet(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AccessTimeOut: TJvAccessTimeOut read GetAccessTimeOut write SetAccessTimeOut stored False;
    property Animation: Boolean read GetAnimationInfo write SetAnimationInfo stored False;
    property Beep: Boolean index SPI_GETBEEP read GetBoolInfo write SetBoolInfo stored False;
    property BorderMultiplier: Integer index SPI_GETBORDER read GetIntInfo write SetIntInfo stored False;
    property DefaultInputLanguage: Integer index SPI_GETDEFAULTINPUTLANG read GetIntInfo write SetIntInfo stored False;
    property DragFullWindows: Boolean index SPI_GETDRAGFULLWINDOWS read GetBoolInfo write SetBoolInfo stored False;
    property FilterKeys: TJvFilterKeys read GetFilterKeys write SetFilterKeys stored False;
    property FontSmoothing: Boolean index SPI_GETFONTSMOOTHING read GetBoolInfo write SetBoolInfo stored False;
    property GridGranularity: Integer index SPI_GETGRIDGRANULARITY read GetIntInfo write SetIntInfo stored False;
    property HighContrast: TJvHighContrast read GetHighContrast write SetHighContrast stored False;
    property IconMetrics: TJvIconMetrics read GetIconMetrics write SetIconMetrics stored False;
    property IconTitleFont: TFont read GetIconTitleFont write SetIconTitleFont stored False;
    property IconTitleWrap: Boolean index SPI_GETICONTITLEWRAP read GetBoolInfo write SetBoolInfo stored False;
    property KeyboardDelay: Integer index SPI_GETKEYBOARDDELAY read GetIntInfo write SetIntInfo stored False;
    property KeyboardPreferred: Boolean index SPI_GETKEYBOARDPREF read GetBoolInfo write SetBoolInfo stored False;
    property KeyboardSpeed: Integer index SPI_GETKEYBOARDSPEED read GetIntInfo write SetIntInfo stored False;
    property KeyboardLayoutName: string read GetKeyboardLayoutName write SetKeyboardLayoutName stored False;
    property LowPowerActive: Boolean index SPI_GETLOWPOWERACTIVE read GetBoolInfo write SetBoolInfo stored False;
    property LowPowerTimeOut: Integer index SPI_GETLOWPOWERTIMEOUT read GetIntInfo write SetIntInfo stored False;
    property MenuLeftAligned: Boolean index SPI_GETMENUDROPALIGNMENT read GetBoolInfo write SetBoolInfo stored False;
    property MinimizedMetrics: TJvMinimizedMetrics read GetMinimizedMetrics write SetMinimizedMetrics stored False;
    property MouseSpeed: Integer index SPI_GETMOUSESPEED read GetIntInfo write SetIntInfo stored False;
    property MouseThreshold1: Integer index 0 read GetMouseInfo write SetMouseInfo stored False;
    property MouseThreshold2: Integer index 1 read GetMouseInfo write SetMouseInfo stored False;
    property MouseHoverHeight: Integer index SPI_GETMOUSEHOVERHEIGHT read GetIntInfo write SetIntInfo stored False;
    property MouseHoverTime: Integer index SPI_GETMOUSEHOVERTIME read GetIntInfo write SetIntInfo stored False;
    property MouseHoverWidth: Integer index SPI_GETMOUSEHOVERWIDTH read GetIntInfo write SetIntInfo stored False;
    property MouseKeys: TJvMouseKeys read GetMouseKeys write SetMouseKeys stored False;
    property MouseTrails: Integer index SPI_GETMOUSETRAILS read GetIntInfo write SetIntInfo stored False;
    property NonClientMetrics: TJvNonClientMetrics read GetNonClientMetrics write SetNonClientMetrics stored False;
    property PowerOffActive: Boolean index SPI_GETPOWEROFFACTIVE read GetBoolInfo write SetBoolInfo stored False;
    property PowerOffTimeout: Integer index SPI_GETPOWEROFFTIMEOUT read GetIntInfo write SetIntInfo stored False;
    property ScreenReader: Boolean index SPI_GETSCREENREADER read GetBoolInfo write SetBoolInfo stored False;
    property ScreenSaverActive: Boolean index SPI_GETSCREENSAVEACTIVE read GetBoolInfo write SetBoolInfo stored False;
    property ScreenSaveTimeOut: Integer index SPI_GETSCREENSAVETIMEOUT read GetIntInfo write SetIntInfo stored False;
    property SerialKeys: TJvSerialKeys read GetSerialKeys write SetSerialKeys stored False;
    property ShowSounds: Boolean index SPI_GETSHOWSOUNDS read GetBoolInfo write SetBoolInfo stored False;
    property SnapToDefaultButton: Boolean index SPI_GETSNAPTODEFBUTTON read GetBoolInfo write SetBoolInfo stored False;
    property SoundSentry: TJvSoundSentry read GetSoundSentry write SetSoundSentry stored False;
    property StickyKeys: TJvStickyKeysFlags read GetStickyKeys write SetStickyKeys stored False;
    property ToggleKeys: TJvToggleKeysFlags read GetToggleKeys write SetToggleKeys stored False;
    property WheelScrollLines: Integer index SPI_GETWHEELSCROLLLINES read GetIntInfo write SetIntInfo stored False;
    property WindowsExtensions: Boolean index SPI_GETWINDOWSEXTENSION read GetBoolInfo write SetBoolInfo stored False;
    property WorkArea: TJvRect read GetWorkArea write SetWorkArea stored False;
    property ScreenSaverRunning: Boolean index SPI_GETSCREENSAVERRUNNING read GetBoolInfo write SetBoolInfo stored
      False;
    // New (W2k, XP and up)
    property FocusBorderHeight: Integer index SPI_GETFOCUSBORDERHEIGHT read GetIntInfo write SetIntInfo stored False;
    property FocusBorderWidth: Integer index SPI_GETFOCUSBORDERWIDTH read GetIntInfo write SetIntInfo stored False;
    property MouseClickLock: Boolean index SPI_GETMOUSECLICKLOCK read GetBoolInfo write SetBoolInfo stored False;
    property MouseClickLockTime: Integer index SPI_GETMOUSECLICKLOCKTIME read GetIntInfo write SetIntInfo stored False;
    property MouseSonar: Boolean index SPI_GETMOUSESONAR read GetBoolInfo write SetBoolInfo stored False;
    property MouseVanish: Boolean index SPI_GETMOUSEVANISH read GetBoolInfo write SetBoolInfo stored False;
    property DeskWallpaper: string read GetDeskWallpaper write SetDeskWallpaper stored False;
    property DropShadow: Boolean index SPI_GETDROPSHADOW read GetBoolInfo write SetBoolInfo stored False;
    property FlatMenu: Boolean index SPI_GETFLATMENU read GetBoolInfo write SetBoolInfo stored False;
    property FontSmoothingContrast: Integer index SPI_GETFONTSMOOTHINGCONTRAST read GetIntInfo write SetIntInfo stored
      False;
    property FontSmoothingType: TJvFontSmoothingType read GetFontSmoothingType write SetFontSmoothingType stored False;
    property MenuShowDelay: Integer index SPI_GETMENUSHOWDELAY read GetIntInfo write SetIntInfo stored False;
    property ShowIMEUI: Boolean index SPI_GETSHOWIMEUI read GetBoolInfo write SetBoolInfo stored False;
    property ActiveWindowTracking: Boolean index SPI_GETACTIVEWINDOWTRACKING read GetBoolInfo write SetBoolInfo stored
      False;
    property MenuAnimation: Boolean index SPI_GETMENUANIMATION read GetBoolInfo write SetBoolInfo stored False;
    property ComboboxAnimation: Boolean index SPI_GETCOMBOBOXANIMATION read GetBoolInfo write SetBoolInfo stored False;
    property ListboxSmoothScrolling: Boolean index SPI_GETLISTBOXSMOOTHSCROLLING read GetBoolInfo write SetBoolInfo
      stored False;
    property GradientCaptions: Boolean index SPI_GETGRADIENTCAPTIONS read GetBoolInfo write SetBoolInfo stored False;
    property MenuUnderLines: Boolean index SPI_GETMENUUNDERLINES read GetBoolInfo write SetBoolInfo stored False;
    property ActiveWindowTrackZOrder: Boolean index SPI_GETACTIVEWNDTRKZORDER read GetBoolInfo write SetBoolInfo stored
      False;
    property HotTracking: Boolean index SPI_GETHOTTRACKING read GetBoolInfo write SetBoolInfo stored False;
    property MenuFade: Boolean index SPI_GETMENUFADE read GetBoolInfo write SetBoolInfo stored False;
    property SelectionFade: Boolean index SPI_GETSELECTIONFADE read GetBoolInfo write SetBoolInfo stored False;
    property ToolTipAnimation: Boolean index SPI_GETTOOLTIPANIMATION read GetBoolInfo write SetBoolInfo stored False;
    property ToolTipFade: Boolean index SPI_GETTOOLTIPFADE read GetBoolInfo write SetBoolInfo stored False;
    property CursorShadow: Boolean index SPI_GETCURSORSHADOW read GetBoolInfo write SetBoolInfo stored False;
    property UIEffects: Boolean index SPI_GETUIEFFECTS read GetBoolInfo write SetBoolInfo stored False;
    property ForegroundLockTimeOut: Integer index SPI_GETFOREGROUNDLOCKTIMEOUT read GetIntInfo write SetIntInfo stored
      False;
    property ActiveWindowTrackTimeOut: Integer index SPI_GETACTIVEWNDTRKTIMEOUT read GetIntInfo write SetIntInfo stored
      False;
    property ForegroundFlashCount: Integer index SPI_GETFOREGROUNDFLASHCOUNT read GetIntInfo write SetIntInfo stored
      False;
    property CaretWidth: Integer index SPI_GETCARETWIDTH read GetIntInfo write SetIntInfo stored False;
    property IconHorizontalSpacing: Integer index 0 read GetIconSpacing write SetIconSpacing stored False;
    property IconVerticalSpacing: Integer index 1 read GetIconSpacing write SetIconSpacing stored False;
  end;

  TJvSystemColorsInfo = class(TJvWriteableInfo)
    // writeable: all
  private
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  published
    property Color3DHighlight: TColor index COLOR_3DHILIGHT read GetColor write SetColor stored False;
    property Color3DLight: TColor index COLOR_3DLIGHT read GetColor write SetColor stored False;
    property Color3DShadow: TColor index COLOR_3DSHADOW read GetColor write SetColor stored False;
    property Color3DDarkShadow: TColor index COLOR_3DDKSHADOW read GetColor write SetColor stored False;
    property Color3DFace: TColor index COLOR_3DFACE read GetColor write SetColor stored False;
    property ColorActiveBorder: TColor index COLOR_ACTIVEBORDER read GetColor write SetColor stored False;
    property ColorActiveCaption: TColor index COLOR_ACTIVECAPTION read GetColor write SetColor stored False;
    property ColorAppWorkspace: TColor index COLOR_APPWORKSPACE read GetColor write SetColor stored False;
    property ColorBackground: TColor index COLOR_BACKGROUND read GetColor write SetColor stored False;
    property ColorBtnFace: TColor index COLOR_BTNFACE read GetColor write SetColor stored False;
    property ColorBtnText: TColor index COLOR_BTNTEXT read GetColor write SetColor stored False;
    property ColorCaptionText: TColor index COLOR_CAPTIONTEXT read GetColor write SetColor stored False;
    property ColorGrayText: TColor index COLOR_GRAYTEXT read GetColor write SetColor stored False;
    property ColorHighlight: TColor index COLOR_HIGHLIGHT read GetColor write SetColor stored False;
    property ColorHighlightText: TColor index COLOR_HIGHLIGHTTEXT read GetColor write SetColor stored False;
    property ColorInactiveBorder: TColor index COLOR_INACTIVEBORDER read GetColor write SetColor stored False;
    property ColorInactiveCaption: TColor index COLOR_INACTIVECAPTION read GetColor write SetColor stored False;
    property ColorInactiveCaptionText: TColor index COLOR_INACTIVECAPTIONTEXT read GetColor write SetColor stored False;
    property ColorInfoBk: TColor index COLOR_INFOBK read GetColor write SetColor stored False;
    property ColorInfoText: TColor index COLOR_INFOTEXT read GetColor write SetColor stored False;
    property ColorMenu: TColor index COLOR_MENU read GetColor write SetColor stored False;
    property ColorMenuText: TColor index COLOR_MENUTEXT read GetColor write SetColor stored False;
    property ColorScrollBar: TColor index COLOR_SCROLLBAR read GetColor write SetColor stored False;
    property ColorWindow: TColor index COLOR_WINDOW read GetColor write SetColor stored False;
    property ColorWindowFrame: TColor index COLOR_WINDOWFRAME read GetColor write SetColor stored False;
    property ColorWindowText: TColor index COLOR_WINDOWTEXT read GetColor write SetColor stored False;
    property ColorHotLight: TColor index COLOR_HOTLIGHT read GetColor write SetColor stored False;
    property ColorGradientActiveCaption: TColor index COLOR_GRADIENTACTIVECAPTION read GetColor write SetColor stored
      False;
    property ColorGradientInactiveCaption: TColor index COLOR_GRADIENTINACTIVECAPTION read GetColor write SetColor stored
      False;
    property ColorMenuHighlight: TColor index COLOR_MENUHILIGHT read GetColor write SetColor stored False;
    property ColorMenuBar: TColor index COLOR_MENUBAR read GetColor write SetColor stored False;
  end;

  TJvExeType = (etNone, etMSDos, etWin16, etWin32, etConsole);
  TJvIconModifier = (imNormal, imOverlay, imSelected, imOpen, imShellSize, imSmall);
  TJvIconModifiers = set of TJvIconModifier;

  TJvFileInfo = class(TJvWriteableInfo)
  // writeable: FileName, Modifier
  private
    FLargeImages: TImageList;
    FSmallImages: TImageList;
    FFileName: TFileName;
    FModifiers: TJvIconModifiers;
    FIcon: TIcon;
    function GetSmallImages: TImageList;
    function GetLargeImages: TImageList;
    procedure SetExeDummy(const Value: TJvExeType);
    procedure SetIconDummy(const Value: TIcon);
    procedure SetIntDummy(const Value: Integer);
    procedure SetStrDummy(const Value: string);
  protected
    function GetIconIndex: Integer;
    function GetDisplayName: string;
    function GetExeType: TJvExeType;
    function GetAttributes: Integer;
    function GetIconLocation: string;
    function GetTypeString: string;
    function GetIconHandle: THandle;
    function GetAttrString: string;
    procedure SetFileName(Value: TFileName);
    procedure SetModifiers(Value: TJvIconModifiers);
  public
    property LargeImages: TImageList read FLargeImages;
    property SmallImages: TImageList read FSmallImages;
    property IconHandle: THandle read GetIconHandle stored False;
    property Attributes: Integer read GetAttributes stored False;
    function GetFileInfo(const FileName: string; Attributes: Cardinal; out Info: ShFileInfo; Flags: Cardinal): Cardinal;
  published
    constructor Create;
    destructor Destroy; override;
    property FileName: TFileName read FFileName write SetFileName stored False;
    property Modifiers: TJvIconModifiers read FModifiers write SetModifiers default [imNormal];
    property IconIndex: Integer read GetIconIndex write SetIntDummy stored False;
    property DisplayName: string read GetDisplayName write SetStrDummy stored False;
    property ExeType: TJvExeType read GetExeType write SetExeDummy stored False;
    property AttrString: string read GetAttrString write SetStrDummy stored False;
    property IconLocation: string read GetIconLocation write SetStrDummy stored False;
    property TypeString: string read GetTypeString write SetStrDummy stored False;
    property Icon: TIcon read FIcon write SetIconDummy stored False;
  end;

  TJvDriveChangeEvent = procedure(Sender: TObject; Drive: Char) of object;
  TJvCompactingEvent = procedure(Sender: TObject; Ratio: Integer) of object;
  TJvPowerBroadcastEvent = procedure(Sender: TObject; Event, Data: Integer) of object;
  TJvDeviceChangeEvent = procedure(Sender: TObject; Event: UINT; Data: Pointer) of object;
  TJvDevModeChangeEvent = procedure(Sender: TObject; Device: string) of object;
  TJvDisplayChangeEvent = procedure(Sender: TObject; BitsPerPixel, ScreenWidth, ScreenHeight: Integer) of object;
  TJvSettingChangeEvent = procedure(Sender: TObject; Flag: Integer; const Section: string) of object;
  TJvSpoolerChangeEvent = procedure(Sender: TObject; JobStatus, JobsLeft: Integer) of object;
  TJvPaletteChangeEvent = procedure(Sender: TObject; Wnd: HWND) of object;

  TJvComputerInfoEx = class(TJvComponent)
  private
    FAPMInfo: TJvAPMInfo;
    FBIOSInfo: TJvBIOSInfo;
    FCPUInfo: TJvCPUInfo;
    FIdentification: TJvIdentification;
    FKeyInfo: TJvKeyInfo;
    FMemoryInfo: TJvMemInfo;
    FMiscInfo: TJvMiscInfo;
    FOSVersionInfo: TJvOSVersionInfo;
    FScreenInfo: TJvScreenInfo;
    FSystemFolders: TJvSystemFolders;
    FDeviceHandle: Integer;
    FOnDeviceAdded: TJvDriveChangeEvent;
    FOnDeviceRemoved: TJvDriveChangeEvent;
    FMetrics: TJvMetricsInfo;
    FSystem: TJvSystemParametersInfo;
    FColors: TJvSystemColorsInfo;
    FOnSettingChange: TJvSettingChangeEvent;
    FOnCompacting: TJvCompactingEvent;
    FOnPowerBroadcast: TJvPowerBroadcastEvent;
    FOnUserChanged: TNotifyEvent;
    FOnDeviceChange: TJvDeviceChangeEvent;
    FOnDeviceModeChange: TJvDevModeChangeEvent;
    FOnDisplayChange: TJvDisplayChangeEvent;
    FOnTimeChange: TNotifyEvent;
    FOnFontChange: TNotifyEvent;
    FOnSysColorChange: TNotifyEvent;
    FOnSpoolerStatusChange: TJvSpoolerChangeEvent;
    FOnPaletteChanging: TJvPaletteChangeEvent;
    FOnPaletteChanged: TJvPaletteChangeEvent;
    FReadOnly: Boolean;
    FFileInfo: TJvFileInfo;
    procedure SetAPMInfo(const Value: TJvAPMInfo);
    procedure SetBIOSInfo(const Value: TJvBIOSInfo);
    procedure SetCPUInfo(const Value: TJvCPUInfo);
    procedure SetIdentification(const Value: TJvIdentification);
    procedure SetKeyInfo(const Value: TJvKeyInfo);
    procedure SetMemoryInfo(const Value: TJvMemInfo);
    procedure SetMiscInfo(const Value: TJvMiscInfo);
    procedure SetOSVersionInfo(const Value: TJvOSVersionInfo);
    procedure SetScreenInfo(const Value: TJvScreenInfo);
    procedure SetSystemFolders(const Value: TJvSystemFolders);
    procedure SetMetrics(const Value: TJvMetricsInfo);
    procedure SetSystem(const Value: TJvSystemParametersInfo);
    procedure SetColors(const Value: TJvSystemColorsInfo);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetFileInfo(const Value: TJvFileInfo);
    function GetAPMInfo: TJvAPMInfo;
    function GetBIOSInfo: TJvBIOSInfo;
    function GetCPUInfo: TJvCPUInfo;
    function GetIdentification: TJvIdentification;
    function GetKeyInfo: TJvKeyInfo;
    function GetMemoryInfo: TJvMemInfo;
    function GetMetrics: TJvMetricsInfo;
    function GetMiscInfo: TJvMiscInfo;
    function GetOSVersionInfo: TJvOSVersionInfo;
    function GetScreenInfo: TJvScreenInfo;
    function GetSystem: TJvSystemParametersInfo;
    function GetSystemFolders: TJvSystemFolders;
    function GetColors: TJvSystemColorsInfo;
    function GetFileInfo: TJvFileInfo;
  protected
    function FirstDrive(AMask: Longint): Char;
    procedure WMDeviceChange(var Msg: TWMDeviceChange);
    procedure WMDisplayChange(var Msg: TWMDisplayChange);
    procedure DoSettingChange(Flag: Integer; Section: string); dynamic;
    procedure DoDriveChange(Drive: Char; Removed: Boolean); dynamic;
    procedure DoCompacting(Ratio: Integer); dynamic;
    procedure DoPowerBroadcast(Event, Data: Integer); dynamic;
    procedure DoUserChanged; dynamic;
    procedure DoDeviceChange(Event: UINT; dwData: Pointer); dynamic;
    procedure DoDevModeChange(const Device: PChar); dynamic;
    procedure DoTimeChange; dynamic;
    procedure DoFontChange; dynamic;
    procedure DoSysColorChange; dynamic;
    procedure DoSpoolerStatus(JobStatus, JobsLeft: Integer); dynamic;
    procedure DoPaletteChanging(Wnd: HWND); dynamic;
    procedure DoPaletteChanged(Wnd: HWND); dynamic;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ResetSystemCursors: Boolean;
    function ResetSystemIcons: Boolean;
  published
    property APM: TJvAPMInfo read GetAPMInfo write SetAPMInfo stored False;
    property BIOS: TJvBIOSInfo read GetBIOSInfo write SetBIOSInfo stored False;
    property Colors: TJvSystemColorsInfo read GetColors write SetColors stored False;
    property CPU: TJvCPUInfo read GetCPUInfo write SetCPUInfo stored False;
    property FileInfo: TJvFileInfo read GetFileInfo write SetFileInfo stored False;
    property Folders: TJvSystemFolders read GetSystemFolders write SetSystemFolders stored False;
    property Identification: TJvIdentification read GetIdentification write SetIdentification stored False;
    property Keyboard: TJvKeyInfo read GetKeyInfo write SetKeyInfo stored False;
    property Memory: TJvMemInfo read GetMemoryInfo write SetMemoryInfo stored False;
    property Metrics: TJvMetricsInfo read GetMetrics write SetMetrics;
    property Misc: TJvMiscInfo read GetMiscInfo write SetMiscInfo stored False;
    property OS: TJvOSVersionInfo read GetOSVersionInfo write SetOSVersionInfo stored False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default True;
    property Screen: TJvScreenInfo read GetScreenInfo write SetScreenInfo stored False;
    property System: TJvSystemParametersInfo read GetSystem write SetSystem stored False;
    property OnDeviceAdded: TJvDriveChangeEvent read FOnDeviceAdded write FOnDeviceAdded;
    property OnDeviceRemoved: TJvDriveChangeEvent read FOnDeviceRemoved write FOnDeviceRemoved;
    property OnSettingChange: TJvSettingChangeEvent read FOnSettingChange write FOnSettingChange;
    property OnCompacting: TJvCompactingEvent read FOnCompacting write FOnCompacting;
    property OnPowerBroadcast: TJvPowerBroadcastEvent read FOnPowerBroadcast write FOnPowerBroadcast;
    property OnUserChanged: TNotifyEvent read FOnUserChanged write FOnUserChanged;
    property OnDeviceChange: TJvDeviceChangeEvent read FOnDeviceChange write FOnDeviceChange;
    property OnDeviceModeChange: TJvDevModeChangeEvent read FOnDeviceModeChange write FOnDeviceModeChange;
    property OnDisplayChange: TJvDisplayChangeEvent read FOnDisplayChange write FOnDisplayChange;
    property OnTimeChange: TNotifyEvent read FOnTimeChange write FOnTimeChange;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
    property OnSysColorChange: TNotifyEvent read FOnSysColorChange write FOnSysColorChange;
    property OnSpoolerStatusChange: TJvSpoolerChangeEvent read FOnSpoolerStatusChange write FOnSpoolerStatusChange;
    property OnPaletteChanging: TJvPaletteChangeEvent read FOnPaletteChanging write FOnPaletteChanging;
    property OnPaletteChanged: TJvPaletteChangeEvent read FOnPaletteChanged write FOnPaletteChanged;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  WinInet, Registry,
  JclShell, JclRegistry, JclFileUtils,
  {$IFDEF COMPILER5}
  JvJCLUtils, // Include/ExcludeTrailingPathDelimiter
  {$ENDIF COMPILER5}
  JvResources;

var
  IsDesigning: Boolean = False;

const
  DEFAULT_SPIF_SENDCHANGE = SPIF_UPDATEINIFILE or SPIF_SENDCHANGE;

procedure UpdateFromLogFont(AFont: TFont; const LogFont: TLogFont);
var
  Style: TFontStyles;
begin
  with LogFont do
  begin
    AFont.Name := lfFaceName;
    AFont.Height := lfHeight;
    AFont.Charset := TFontCharset(lfCharSet);
    Style := [];
    if lfWeight > FW_REGULAR then
      Include(Style, fsBold);
    if lfItalic <> 0 then
      Include(Style, fsItalic);
    if lfUnderline <> 0 then
      Include(Style, fsUnderline);
    if lfStrikeOut <> 0 then
      Include(Style, fsStrikeOut);
    AFont.Style := Style;
  end;
end;

procedure UpdateToLogFont(AFont: TFont; var LogFont: TLogFont);
begin
  {$IFDEF VCL}
  with LogFont do
  begin
    StrCopy(lfFaceName, PChar(AFont.Name));
    lfHeight := AFont.Height;
    lfCharSet := AFont.Charset;
    if fsBold in AFont.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Ord(fsItalic in AFont.Style);
    lfUnderline := Ord(fsUnderline in AFont.Style);
    lfStrikeOut := Ord(fsStrikeOut in AFont.Style);
  end;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  GetObject(QFont_handle(AFont.Handle), SizeOf(LogFont), @LogFont);
  {$ENDIF VisualCLX}
end;

procedure RaiseReadOnly(AlwaysRaise: Boolean = False);
begin
  if not IsDesigning or AlwaysRaise then
    raise EJVCLComputerInfoEx.CreateRes(@RsEReadOnlyProperty);
end;

function ArrangeToWindowsArrange(Value: DWORD): TJvWindowsArrange;
begin
  Result := [];
  if Value and ARW_HIDE = ARW_HIDE then
    Include(Result, waHide)
  else
  if Value and ARW_BOTTOMLEFT = ARW_BOTTOMLEFT then
  begin
    Include(Result, waBottomLeft);
    if Value and ARW_UP = ARW_UP then
      Include(Result, waUp)
    else
      Include(Result, waRight);
  end
  else
  if Value and ARW_BOTTOMRIGHT = ARW_BOTTOMRIGHT then
  begin
    Include(Result, waBottomRight);
    if Value and ARW_UP = ARW_UP then
      Include(Result, waUp)
    else
      Include(Result, waLeft);
  end
  else
  if Value and ARW_TOPLEFT = ARW_TOPLEFT then
  begin
    Include(Result, waTopLeft);
    if Value and ARW_DOWN = ARW_DOWN then
      Include(Result, waDown)
    else
      Include(Result, waRight);
  end
  else
  if Value and ARW_TOPRIGHT = ARW_TOPRIGHT then
  begin
    Include(Result, waTopRight);
    if Value and ARW_DOWN = ARW_DOWN then
      Include(Result, waDown)
    else
      Include(Result, waLeft);
  end;
end;

function WindowsArrangeToArrange(Value: TJvWindowsArrange): DWORD;
begin
  Result := 0;
  // NB! no error checking - trust the user (bad idea?)
  if waHide in Value then
    Result := Result or ARW_HIDE;
  if waBottomLeft in Value then
    Result := Result or ARW_BOTTOMLEFT;
  if waBottomRight in Value then
    Result := Result or ARW_BOTTOMRIGHT;
  if waUp in Value then
    Result := Result or ARW_UP;
  if waTopLeft in Value then
    Result := Result or ARW_TOPLEFT;
  if waDown in Value then
    Result := Result or ARW_DOWN;
  if waTopRight in Value then
    Result := Result or ARW_TOPRIGHT;
end;

//=== { TJvWriteableInfo } ===================================================

constructor TJvWriteableInfo.Create;
begin
  inherited Create;
  FReadOnly := True;
end;

//=== { TJvAPMInfo } =========================================================

function TJvAPMInfo.GetAPMBatteryFlag: TAPMBatteryFlag;
begin
  Result := JclSysInfo.GetAPMBatteryFlag;
end;

function TJvAPMInfo.GetAPMBatteryFullLifeTime: DWORD;
begin
  if BatteryFlag = abfNoBattery then
    Result := 0
  else
    Result := JclSysInfo.GetAPMBatteryFullLifeTime;
end;

function TJvAPMInfo.GetAPMBatteryLifePercent: Integer;
begin
  if BatteryFlag = abfNoBattery then
    Result := 0
  else
    Result := JclSysInfo.GetAPMBatteryLifePercent;
end;

function TJvAPMInfo.GetAPMBatteryLifeTime: DWORD;
begin
  if BatteryFlag = abfNoBattery then
    Result := 0
  else
    Result := JclSysInfo.GetAPMBatteryLifeTime;
end;

function TJvAPMInfo.GetAPMLineStatus: TAPMLineStatus;
begin
  if BatteryFlag = abfNoBattery then
    Result := alsUnknown
  else
    Result := JclSysInfo.GetAPMLineStatus;
end;

procedure TJvAPMInfo.SetAPMBatteryFlag(const Value: TAPMBatteryFlag);
begin
  RaiseReadOnly;
end;

procedure TJvAPMInfo.SetAPMBatteryFullLifeTime(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvAPMInfo.SetAPMBatteryLifePercent(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvAPMInfo.SetAPMBatteryLifeTime(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvAPMInfo.SetAPMLineStatus(const Value: TAPMLineStatus);
begin
  RaiseReadOnly;
end;

//=== { TJvOSVersionInfo } ===================================================

const
  HKLM_CURRENT_VERSION_WINDOWS = 'Software\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT = 'Software\Microsoft\Windows NT\CurrentVersion';

function REG_CURRENT_VERSION: string;
begin
  if IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

function TJvOSVersionInfo.GetOSVersion: TWindowsVersion;
begin
  Result := JclSysInfo.GetWindowsVersion;
end;

function TJvOSVersionInfo.GetProductType: TNtProductType;
begin
  Result := JclSysInfo.NtProductType;
end;

function TJvOSVersionInfo.GetWinProductID: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'ProductID', '');
end;

function TJvOSVersionInfo.GetWinProductName: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'ProductName', '');
end;

function TJvOSVersionInfo.GetWinServicePackVersion: DWORD;
begin
  Result := JclSysInfo.GetWindowsServicePackVersion;
end;

function TJvOSVersionInfo.GetWinVersionBuild: DWORD;
begin
  Result := Win32BuildNumber;
end;

function TJvOSVersionInfo.GetWinVersionMajor: DWORD;
begin
  Result := Win32MajorVersion;
end;

function TJvOSVersionInfo.GetWinVersionMinor: DWORD;
begin
  Result := Win32MinorVersion;
end;

function TJvOSVersionInfo.GetWinVersionCSDString: string;
begin
  Result := Win32CSDVersion;
end;

procedure TJvOSVersionInfo.SetOSVersion(const Value: TWindowsVersion);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetProductType(const Value: TNtProductType);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinProductID(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinProductName(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinServicePackVersion(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinVersionBuild(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinVersionMajor(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinVersionMinor(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvOSVersionInfo.SetWinVersionCSDString(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvCPUInfo } =========================================================

function TJvCPUInfo.AMDSpecific: TAMDSpecific;
begin
  Result := GetCPUInfo.AMDSpecific;
end;

function TJvCPUInfo.CyrixSpecific: TCyrixSpecific;
begin
  Result := GetCPUInfo.CyrixSpecific;
end;

function TJvCPUInfo.GetCPUInfo: TCPUInfo;
begin
  JclSysInfo.GetCPUInfo(Result);
end;

function TJvCPUInfo.GetCPUName: string;
begin
  Result := GetCPUInfo.CpuName;
end;

function TJvCPUInfo.GetCPUSpeed: TFreqInfo;
begin
  Result := GetCPUInfo.FrequencyInfo;
end;

function TJvCPUInfo.GetCPUType: TJvCPUType;
begin
  Result := TJvCPUType(GetCPUInfo.CpuType);
end;

function TJvCPUInfo.GetExTicks: Cardinal;
begin
  Result := GetCPUSpeed.ExTicks;
end;

function TJvCPUInfo.GetFamily: Byte;
begin
  Result := GetCPUInfo.Family;
end;

function TJvCPUInfo.GetFeatures: Cardinal;
begin
  Result := GetCPUInfo.Features;
end;

function TJvCPUInfo.GetHasCacheInfo: Boolean;
begin
  Result := GetCPUInfo.HasCacheInfo;
end;

function TJvCPUInfo.GetHasExtendedInfo: Boolean;
begin
  Result := GetCPUInfo.HasExtendedInfo;
end;

function TJvCPUInfo.GetHasInstruction: Boolean;
begin
  Result := GetCPUInfo.HasInstruction;
end;

function TJvCPUInfo.GetInCycles: Cardinal;
begin
  Result := GetCPUSpeed.InCycles;
end;

function TJvCPUInfo.GetIsFDIVOK: Boolean;
begin
  Result := GetCPUInfo.IsFDIVOK;
end;

function TJvCPUInfo.GetManufacturer: string;
begin
  Result := GetCPUInfo.Manufacturer;
end;

function TJvCPUInfo.GetMMX: Boolean;
begin
  Result := GetCPUInfo.MMX;
end;

function TJvCPUInfo.GetModel: Byte;
begin
  Result := GetCPUInfo.Model;
end;

function TJvCPUInfo.GetNormFreq: Cardinal;
begin
  Result := GetCPUSpeed.NormFreq;
end;

function TJvCPUInfo.GetProcessorCount: Integer;
begin
  Result := JclSysInfo.ProcessorCount;
end;

function TJvCPUInfo.GetRawFreq: Cardinal;
begin
  Result := GetCPUSpeed.RawFreq;
end;

function TJvCPUInfo.GetStepping: Byte;
begin
  Result := GetCPUInfo.Stepping;
end;

function TJvCPUInfo.GetVendorIDString: string;
begin
  Result := GetCPUInfo.VendorIDString;
end;

function TJvCPUInfo.IntelSpecific: TIntelSpecific;
begin
  Result := GetCPUInfo.IntelSpecific;
end;

procedure TJvCPUInfo.SetCPUName(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetCPUType(const Value: TJvCPUType);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetExTicks(const Value: Cardinal);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetFamily(const Value: Byte);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetFeatures(const Value: Cardinal);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetHasCacheInfo(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetHasExtendedInfo(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetHasInstruction(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetInCycles(const Value: Cardinal);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetIsFDIVOK(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetManufacturer(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetMMX(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetModel(const Value: Byte);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetNormFreq(const Value: Cardinal);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetProcessorCount(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetRawFreq(const Value: Cardinal);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetStepping(const Value: Byte);
begin
  RaiseReadOnly;
end;

procedure TJvCPUInfo.SetVendorIDString(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvBIOSInfo } ========================================================

function TJvBIOSInfo.GetBIOSCopyright: string;
begin
  Result := JclSysInfo.GetBIOSCopyright;
end;

function TJvBIOSInfo.GetBIOSDate: TDateTime;
begin
  Result := JclSysInfo.GetBIOSDate;
end;

function TJvBIOSInfo.GetBIOSExtendedInfo: string;
begin
  Result := JclSysInfo.GetBIOSExtendedInfo;
end;

function TJvBIOSInfo.GetBIOSName: string;
begin
  Result := JclSysInfo.GetBIOSName;
end;

procedure TJvBIOSInfo.SetBIOSCopyright(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvBIOSInfo.SetBIOSDate(const Value: TDateTime);
begin
  RaiseReadOnly;
end;

procedure TJvBIOSInfo.SetBIOSExtendedInfo(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvBIOSInfo.SetBIOSName(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvSystemFolders } ===================================================

function TJvSystemFolders.AdjustPathDelimiter(const S: string): string;
begin
  if TrailingPathDelimiter then
    Result := IncludeTrailingPathDelimiter(S)
  else
    Result := ExcludeTrailingPathDelimiter(S);
end;

function TJvSystemFolders.GetCommonFiles: string;
begin
  Result := AdjustPathDelimiter(JclSysInfo.GetCommonFilesFolder);
end;

function TJvSystemFolders.GetCurrent: string;
begin
  Result := AdjustPathDelimiter(JclSysInfo.GetCurrentFolder);
end;

function TJvSystemFolders.Get(const Index: Integer): string;
begin
  Result := AdjustPathDelimiter(JclShell.GetSpecialFolderLocation(Index));
end;

function TJvSystemFolders.GetProgramFiles: string;
begin
  Result := AdjustPathDelimiter(JclSysInfo.GetProgramFilesFolder);
end;

function TJvSystemFolders.GetWindows: string;
begin
  Result := AdjustPathDelimiter(JclSysInfo.GetWindowsFolder);
end;

function TJvSystemFolders.GetSystem: string;
begin
  Result := AdjustPathDelimiter(JclSysInfo.GetWindowsSystemFolder);
end;

function TJvSystemFolders.GetTemp: string;
begin
  if not GetEnvironmentVar('TMP', Result, False) or not GetEnvironmentVar('TEMP', Result, False) then
    Result := GetCurrentDir;
  if Result <> '' then
    // the temp folder is usually in 8.3 format, so try to convert
    Result := AdjustPathDelimiter(PathGetLongName(Result));
end;

procedure TJvSystemFolders.SetCommonFiles(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvSystemFolders.SetCurrent(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    SetCurrentDir(Value)
  else
    RaiseReadOnly;
end;

procedure TJvSystemFolders.Put(const Index: Integer; const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvSystemFolders.SetProgramFiles(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvSystemFolders.SetWindows(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvSystemFolders.SetSystem(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvSystemFolders.SetTemp(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvMemInfo } =========================================================

function GetMemoryStatus: TMemoryStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(Result);
end;

function TJvMemInfo.GetFreePageFileMemory: Int64;
begin
  Result := GetMemoryStatus.dwAvailPageFile;
end;

function TJvMemInfo.GetFreePhysicalMemory: Int64;
begin
  Result := GetMemoryStatus.dwAvailPhys;
end;

function TJvMemInfo.GetFreeVirtualMemory: Int64;
begin
  Result := GetMemoryStatus.dwAvailVirtual;
end;

function TJvMemInfo.GetMaxAppAddress: Integer;
begin
  Result := JclSysInfo.GetMaxAppAddress;
end;

function TJvMemInfo.GetMemoryLoad: Int64;
begin
  Result := GetMemoryStatus.dwMemoryLoad;
end;

function TJvMemInfo.GetMinAppAddress: Integer;
begin
  Result := JclSysInfo.GetMinAppAddress;
end;

function TJvMemInfo.GetSwapFileSize: Int64;
var
  MemInfo: TMemoryStatus;
begin
  // (rom) avoid possible inconsistencies when calling GetMemoryStatus twice
  MemInfo := GetMemoryStatus;
  Result := MemInfo.dwTotalPageFile - MemInfo.dwAvailPageFile;
end;

function TJvMemInfo.GetSwapFileUsage: Integer;
var
  MemInfo: TMemoryStatus;
begin
  // (rom) avoid possible inconsistencies when calling GetMemoryStatus several times
  MemInfo := GetMemoryStatus;
  with MemInfo do
    if dwTotalPageFile > 0 then
      Result := 100 - Trunc(dwAvailPageFile / dwTotalPageFile * 100)
    else
      Result := 0;
end;

function TJvMemInfo.GetTotalPageFileMemory: Int64;
begin
  Result := GetMemoryStatus.dwTotalPageFile;
end;

function TJvMemInfo.GetTotalPhysicalMemory: Int64;
begin
  Result := GetMemoryStatus.dwTotalPhys;
end;

function TJvMemInfo.GetTotalVirtualMemory: Int64;
begin
  Result := GetMemoryStatus.dwTotalVirtual;
end;

procedure TJvMemInfo.SetFreePageFileMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetFreePhysicalMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetFreeVirtualMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetMaxAppAddress(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetMemoryLoad(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetMinAppAddress(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetSwapFileSize(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetSwapFileUsage(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetTotalPageFileMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetTotalPhysicalMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMemInfo.SetTotalVirtualMemory(const Value: Int64);
begin
  RaiseReadOnly;
end;

//=== { TJvKeyInfo } =========================================================

function TJvKeyInfo.GetCapsLockKeyState: Boolean;
begin
  Result := JclSysInfo.GetCapsLockKeyState;
end;

function TJvKeyInfo.GetKeyState(const VirtualKey: Cardinal): Boolean;
begin
  Result := JclSysInfo.GetKeyState(VirtualKey);
end;

function TJvKeyInfo.GetNumLockKeyState: Boolean;
begin
  Result := JclSysInfo.GetNumLockKeyState;
end;

function TJvKeyInfo.GetScrollLockKeyState: Boolean;
begin
  Result := JclSysInfo.GetScrollLockKeyState;
end;

procedure TJvKeyInfo.SetCapsLockKeyState(const Value: Boolean);
var
  Keys: TKeyboardState;
begin
  if not IsDesigning and not ReadOnly then
  begin
    GetKeyboardState(Keys);
    Keys[VK_CAPITAL] := Ord(Value);
    SetKeyboardState(Keys);
  end
  else
    RaiseReadOnly;
end;

procedure TJvKeyInfo.SetKeyState(const VirtualKey: Cardinal; const Value: Boolean);
var
  Keys: TKeyboardState;
begin
  if not IsDesigning and not ReadOnly then
  begin
    GetKeyboardState(Keys);
    Keys[VirtualKey] := Ord(Value) * $80;
    SetKeyboardState(Keys);
  end
  else
    RaiseReadOnly;
end;

procedure TJvKeyInfo.SetNumLockKeyState(const Value: Boolean);
var
  Keys: TKeyboardState;
begin
  if not IsDesigning and not ReadOnly then
  begin
    GetKeyboardState(Keys);
    Keys[VK_NUMLOCK] := Ord(Value);
    SetKeyboardState(Keys);
  end
  else
    RaiseReadOnly;
end;

procedure TJvKeyInfo.SetScrollLockKeyState(const Value: Boolean);
var
  Keys: TKeyboardState;
begin
  if not IsDesigning and not ReadOnly then
  begin
    GetKeyboardState(Keys);
    Keys[VK_SCROLL] := Ord(Value);
    SetKeyboardState(Keys);
  end
  else
    RaiseReadOnly;
end;

//=== { TJvIdentification } ==================================================

const
  cCommentRegPath = 'System\CurrentControlSet\Services\VxD\VNETSUP';
  cCommentRegPathNT = 'SYSTEM\CurrentControlSet\Services\lanmanserver\parameters';

function TJvIdentification.GetComment: string;
begin
  if IsWinNT then
    // (p3) should return empty string on unsupported NT OS's
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, cCommentRegPathNT, 'srvcomment', '')
  else
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, cCommentRegPath, 'Comment', '')
end;

function TJvIdentification.GetDomainName: string;
begin
  Result := JclSysInfo.GetDomainName;
end;

function TJvIdentification.GetHostIPAddress(const HostName: string): string;
begin
  Result := JclSysInfo.GetIPAddress(HostName);
end;

function TJvIdentification.GetIPAddress: string;
begin
  Result := HostIPAddress[''];
end;

function TJvIdentification.GetLocalComputerName: string;
begin
  Result := JclSysInfo.GetLocalComputerName;
end;

function TJvIdentification.GetLocalUserName: string;
begin
  Result := JclSysInfo.GetLocalUserName;
end;

// avoid using LM.pas
type
  PWkstaInfo100 = ^TWkstaInfo100;
  _WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computername: LPWSTR;
    wki100_langroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
  {$EXTERNALSYM _WKSTA_INFO_100}
  TWkstaInfo100 = _WKSTA_INFO_100;

const
  netapi32lib = 'netapi32.dll';

var
  _NetWkstaGetInfo: function(servername: LPWSTR; level: DWORD; bufptr: Pointer): DWORD; stdcall = nil;
  _NetApiBufferFree: function(Buffer: Pointer): DWORD; stdcall = nil;
  LibHandle: Cardinal = 0;

function LoadNetLib: Boolean;
begin
  if LibHandle = 0 then
  begin
    LibHandle := LoadLibrary(netapi32lib);
    if LibHandle <> 0 then
    begin
      @_NetWkstaGetInfo := GetProcAddress(LibHandle, 'NetWkstaGetInfo');
      @_NetApiBufferFree := GetProcAddress(LibHandle, 'NetApiBufferFree');
    end;
  end;
  Result := LibHandle <> 0;
end;

procedure UnloadNetLib;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
  LibHandle := 0;
end;

function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall;
begin
  if LoadNetLib and Assigned(_NetApiBufferFree) then
    Result := _NetApiBufferFree(Buffer)
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

function NetWkstaGetInfo(servername: LPWSTR; level: DWORD; bufptr: Pointer): DWORD;
begin
  if LoadNetLib and Assigned(_NetWkstaGetInfo) then
    Result := _NetWkstaGetInfo(servername, level, bufptr)
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

function TJvIdentification.GetLocalWorkgroup: string;
var
  LanInfo: PWkstaInfo100;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Services\Vxd\VNETSUP', 'Workgroup', '');
  if (Result = '') and IsWinNT then
  begin
    LanInfo := nil;
    if (NetWkstaGetInfo(nil, 100, @LanInfo) = 0) and (LanInfo <> nil) then
    begin
      Result := LanInfo^.wki100_langroup;
      NetApiBufferFree(LanInfo);
    end;
  end;
end;

function TJvIdentification.GetRegisteredCompany: string;
begin
  Result := JclSysInfo.GetRegisteredCompany;
end;

function TJvIdentification.GetRegisteredOwner: string;
begin
  Result := JclSysInfo.GetRegisteredOwner;
end;

function TJvIdentification.GetUserDomainName(const CurUser: string): string;
begin
  Result := JclSysInfo.GetUserDomainName(CurUser);
end;

function TJvIdentification.GetVolumeFileSystem(const Drive: string): string;
begin
  Result := JclSysInfo.GetVolumeFileSystem(Drive);
end;

function TJvIdentification.GetVolumeName(const Drive: string): string;
begin
  Result := JclSysInfo.GetVolumeName(Drive);
end;

function TJvIdentification.GetVolumeSerialNumber(const Drive: string): string;
begin
  Result := JclSysInfo.GetVolumeSerialNumber(Drive);
end;

procedure TJvIdentification.SetComment(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
  begin
    // (p3) implementation dilemma: either allow the user to write the value regardless of
    // whether the OS supports it or not, or only write value if it is known to be supported?
    // Currently, only allow to write if known to be supported and raise error if not supported,
    // but maybe that's a bad idea?
    if IsWinXP then // "srvcomment" property only supported on WinXP AFAIK
      RegWriteString(HKEY_LOCAL_MACHINE, cCommentRegPathNT, 'srvcomment', Value)
    else
    if not IsWinNT then
      // Win95/98 both support Comment
      RegWriteString(HKEY_LOCAL_MACHINE, cCommentRegPath, 'Comment', Value)
    else
      RaiseReadOnly; // ?? - or just let it pass unnoticed?
  end
  else
    RaiseReadOnly;
end;

procedure TJvIdentification.SetDomainName(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvIdentification.SetIPAddress(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvIdentification.SetLocalComputerName(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
  begin
    if not SetComputerName(PChar(Value)) then
      RaiseLastOSError;
  end
  else
    RaiseReadOnly;
end;

procedure TJvIdentification.SetLocalUserName(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvIdentification.SetLocalWorkgroup(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvIdentification.SetRegisteredCompany(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', Value)
  else
    RaiseReadOnly;
end;

procedure TJvIdentification.SetRegisteredOwner(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', Value)
  else
    RaiseReadOnly;
end;

//=== { TJvScreenMode } ======================================================

function DisplayFlagsToSet(Flags: DWORD): TJvDisplayFlags;
begin
  Result := [];
  if Flags and DM_GRAYSCALE = DM_GRAYSCALE then
    Include(Result, dmGrayScale);
  if Flags and DM_INTERLACED = DM_INTERLACED then
    Include(Result, dmInterlaced);
end;

function SetToDisplayFlags(Value: TJvDisplayFlags): DWORD;
begin
  Result := 0;
  if dmGrayScale in Value then
    Result := Result or DM_GRAYSCALE;
  if dmInterlaced in Value then
    Result := Result or DM_INTERLACED;
end;

procedure TJvScreenMode.SetBitsPerPixel(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvScreenMode.SetFlags(const Value: TJvDisplayFlags);
begin
  RaiseReadOnly;
end;

procedure TJvScreenMode.SetHeight(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvScreenMode.SetHz(const Value: DWORD);
begin
  RaiseReadOnly;
end;

procedure TJvScreenMode.SetWidth(const Value: Integer);
begin
  RaiseReadOnly;
end;

//=== { TJvScreenInfo } ======================================================

destructor TJvScreenInfo.Destroy;
begin
  FScreenModes.Free;
  inherited Destroy;
end;

function TJvScreenInfo.GetCurrentMode: TDeviceMode;
const
  cCurrentSettings = $FFFFFFFE;
begin
  Result.dmSize := SizeOf(Result);
  EnumDisplaySettings(nil, cCurrentSettings, Result);
end;

function TJvScreenInfo.GetBitsPerPixel: DWORD;
begin
  Result := GetCurrentMode.dmBitsPerPel; // ( 2^Result = number of colors
end;

function TJvScreenInfo.GetScreenFrequency: DWORD;
begin
  Result := GetCurrentMode.dmDisplayFrequency;
  if Result in [0, 1] then
    Result := 60; // - default screen frequency but don't know how to get it...
end;

function TJvScreenInfo.GetScreenHeight: DWORD;
begin
  Result := ScreenResolution.Y;
end;

function TJvScreenInfo.GetScreenResolution: TPoint;
begin
  with GetCurrentMode do
    Result := Point(dmPelsWidth, dmPelsHeight);
end;

function TJvScreenInfo.GetScreenWidth: DWORD;
begin
  Result := ScreenResolution.X;
end;

procedure TJvScreenInfo.SetBitsPerPixel(const Value: DWORD);
var
  DevMode: TDeviceMode;
begin
  if not IsDesigning and not ReadOnly then
  begin
    DevMode := GetCurrentMode;
    if DevMode.dmBitsPerPel <> Value then
      DevMode.dmBitsPerPel := Value;
    DevMode.dmFields := DM_BITSPERPEL;
    SetCurrentMode(DevMode, CDS_UPDATEREGISTRY);
  end
  else
    RaiseReadOnly;
end;

procedure TJvScreenInfo.SetScreenFrequency(const Value: DWORD);
var
  DevMode: TDeviceMode;
begin
  if not IsDesigning and not ReadOnly then
  begin
    DevMode := GetCurrentMode;
    if DevMode.dmDisplayFrequency <> Value then
      DevMode.dmDisplayFrequency := Value;
    DevMode.dmFields := DM_DISPLAYFREQUENCY;
    SetCurrentMode(DevMode, CDS_UPDATEREGISTRY);
  end
  else
    RaiseReadOnly;
end;

procedure TJvScreenInfo.SetScreenHeight(const Value: DWORD);
var
  DevMode: TDeviceMode;
begin
  if not IsDesigning and not ReadOnly then
  begin
    DevMode := GetCurrentMode;
    if DevMode.dmPelsHeight <> Value then
      DevMode.dmPelsHeight := Value;
    DevMode.dmFields := DM_PELSHEIGHT;
    SetCurrentMode(DevMode, CDS_UPDATEREGISTRY);
  end
  else
    RaiseReadOnly;
end;

procedure TJvScreenInfo.SetScreenResolution(const Value: TPoint);
begin
  RaiseReadOnly;
end;

procedure TJvScreenInfo.SetScreenWidth(const Value: DWORD);
var
  DevMode: TDeviceMode;
begin
  if not IsDesigning and not ReadOnly then
  begin
    DevMode := GetCurrentMode;
    if DevMode.dmPelsWidth <> Value then
      DevMode.dmPelsWidth := Value;
    DevMode.dmFields := DM_PELSWIDTH;
    SetCurrentMode(DevMode, CDS_UPDATEREGISTRY);
  end
  else
    RaiseReadOnly;
end;

function TJvScreenInfo.GetFlags: TJvDisplayFlags;
begin
  Result := DisplayFlagsToSet(GetCurrentMode.dmDisplayFlags);
end;

procedure TJvScreenInfo.SetFlags(const Value: TJvDisplayFlags);
var
  DevMode: TDeviceMode;
  Flags: DWORD;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Flags := SetToDisplayFlags(Value);
    DevMode := GetCurrentMode;
    if DevMode.dmDisplayFlags <> Flags then
      DevMode.dmDisplayFlags := Flags;
    DevMode.dmFields := DM_DISPLAYFLAGS;
    SetCurrentMode(DevMode, CDS_UPDATEREGISTRY);
  end
  else
    RaiseReadOnly;
end;

function TJvScreenInfo.GetScreenModes: TJvScreenModes;
begin
  if FScreenModes = nil then
    FScreenModes := TJvScreenModes.Create;
  FScreenModes.Refresh;
  Result := FScreenModes;
end;

procedure TJvScreenInfo.SetScreenModes(const Value: TJvScreenModes);
begin
  //
end;

procedure TJvScreenInfo.SetCurrentMode(ADeviceMode: TDeviceMode; Flags: DWORD);
begin
  if not IsDesigning and not ReadOnly then
  begin
    if ChangeDisplaySettings(ADeviceMode, CDS_TEST) = DISP_CHANGE_SUCCESSFUL then
      ChangeDisplaySettings(ADeviceMode, Flags);
    //    else
    //      RaiseLastOSError;
  end
  else
    RaiseReadOnly;
end;

//=== { TJvAppVersions } =====================================================

function TJvAppVersions.GetADOVersion: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\DataAccess', 'Version', '');
end;

function TJvAppVersions.GetBDELocation: string;
begin
  Result := ExcludeTrailingPathDelimiter(RegReadStringDef(HKEY_LOCAL_MACHINE,
    '\SOFTWARE\Borland\Database Engine', 'DLLPATH', ''));
end;

function TJvAppVersions.GetBDEVersion: string;
begin
  Result := IncludeTrailingPathDelimiter(GetBDELocation) + 'idapi32.dll';
  if not VersionResourceAvailable(Result) then
    Result := IncludeTrailingPathDelimiter(GetBDELocation) + 'bdeadmin.exe';

  if VersionResourceAvailable(Result) then
  begin
    with TJclFileVersionInfo.Create(Result) do
    try
      Result := FileVersion;
    finally
      Free;
    end;
  end
  else
    Result := '';
end;

function TJvAppVersions.GetIEVersion: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\Internet Explorer', 'Version', '');
end;

function TJvAppVersions.GetOpenGLVersion: string;
var
  AVendor: string;
begin
  if not JclSysInfo.GetOpenGLVersion(GetActiveWindow, Result, AVendor) then
    Result := '';
end;

function TJvAppVersions.GetDirectXVersion: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\DirectX', 'Version', '');
end;

procedure TJvAppVersions.SetADOVersion(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvAppVersions.SetBDEVersion(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvAppVersions.SetIEVersion(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvAppVersions.SetOpenGLVersion(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvAppVersions.SetDirectXVersion(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvHardwareProfile } =================================================

function TJvHardwareProfile.GetDockInfo: TJvHWDockInfo;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwDockInfo;
  Result := [];
  if Flags and DOCKINFO_DOCKED = DOCKINFO_DOCKED then
    Include(Result, diDocked);
  if Flags and DOCKINFO_UNDOCKED = DOCKINFO_UNDOCKED then
    Include(Result, diUndocked);
  if Flags and DOCKINFO_USER_SUPPLIED = DOCKINFO_USER_SUPPLIED then
    Include(Result, diUserSupplied);
  if Flags and DOCKINFO_USER_DOCKED = DOCKINFO_USER_DOCKED then
    Include(Result, diUserDocked);
  if Flags and DOCKINFO_USER_UNDOCKED = DOCKINFO_USER_UNDOCKED then
    Include(Result, diUserUndocked);
end;

function TJvHardwareProfile.GetGUID: string;
begin
  Result := NativeType.szHwProfileGuid;
end;

function TJvHardwareProfile.GetName: string;
begin
  Result := NativeType.szHwProfileName;
end;

function TJvHardwareProfile.GetNativeType: HW_PROFILE_INFO;
type
  GetCurrentHwProfileFunc = function(var lpHwProfileInfo: HW_PROFILE_INFO): BOOL; stdcall;
var
  GetCurrentHwProfile: GetCurrentHwProfileFunc;
  LibHandle: THandle;
begin
  FillChar(Result, SizeOf(Result), 0);
  // GetCurrentHwProfile is not available on all Win95's
  LibHandle := LoadLibrary('advapi32.dll');
  if LibHandle <> 0 then
  try
    @GetCurrentHwProfile := GetProcAddress(LibHandle, 'GetCurrentHwProfileA');
    if Assigned(GetCurrentHwProfile) then
      GetCurrentHwProfile(Result);
  finally
    FreeLibrary(LibHandle);
  end;
end;

procedure TJvHardwareProfile.SetDockInfo(const Value: TJvHWDockInfo);
begin
  RaiseReadOnly;
end;

procedure TJvHardwareProfile.SetGUID(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvHardwareProfile.SetName(const Value: string);
begin
  RaiseReadOnly;
end;

//=== { TJvScreenModes } =====================================================

constructor TJvScreenModes.Create;
begin
  FItems := TList.Create;
  FDefaultMode := TJvScreenMode.Create;
end;

destructor TJvScreenModes.Destroy;
begin
  Clear;
  FItems.Free;
  FDefaultMode.Free;
  inherited Destroy;
end;

procedure TJvScreenModes.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TObject(FItems[I]).Free;
  FItems.Clear;
end;

function TJvScreenModes.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvScreenModes.GetItems(Index: Integer): TJvScreenMode;
begin
  if Index < 0 then
    Result := FDefaultMode
  else
  if (Index >= 0) and (Index < Count) then
    Result := TJvScreenMode(FItems[Index])
  else
    Result := nil;
end;

procedure TJvScreenModes.Refresh;
const
  cCurrentSettings = $FFFFFFFE;
var
  I: Integer;
  DevMode: TDeviceMode;
  Item: TJvScreenMode;
begin
  FItems.Clear;
  DevMode.dmSize := SizeOf(DevMode);
  I := 0;
  EnumDisplaySettings(nil, cCurrentSettings, DevMode);
  if DevMode.dmDisplayFrequency < 2 then
    FDefaultMode.FHz := 60
  else
    FDefaultMode.FHz := DevMode.dmDisplayFrequency;
  FDefaultMode.FWidth := DevMode.dmPelsWidth;
  FDefaultMode.FHeight := DevMode.dmPelsHeight;
  FDefaultMode.FBitsPerPixel := DevMode.dmBitsPerPel;
  FDefaultMode.FFlags := DisplayFlagsToSet(DevMode.dmDisplayFlags);

  while EnumDisplaySettings(nil, I, DevMode) do
  begin
    Item := TJvScreenMode.Create;
    if DevMode.dmDisplayFrequency < 2 then
      Item.FHz := 60 // pure guess work
    else
      Item.FHz := DevMode.dmDisplayFrequency;
    Item.FWidth := DevMode.dmPelsWidth;
    Item.FHeight := DevMode.dmPelsHeight;
    Item.FBitsPerPixel := DevMode.dmBitsPerPel;
    Item.FFlags := DisplayFlagsToSet(DevMode.dmDisplayFlags);
    FItems.Add(Item);
    Inc(I);
  end;
end;

procedure TJvScreenModes.SetItems(Index: Integer; const Value: TJvScreenMode);
begin
  RaiseReadOnly;
end;

//=== { TJvMiscInfo } ========================================================

const
  HKCU_CONTROL_PANEL_DESKTOP = '\Control Panel\Desktop';

destructor TJvMiscInfo.Destroy;
begin
  FVersions.Free;
  FColorSchemes.Free;
  inherited Destroy;
end;

function TJvMiscInfo.GetColorSchemes: TStrings;
begin
  if FColorSchemes = nil then
    FColorSchemes := TStringlist.Create;
  FColorSchemes.Clear;
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly('\Control Panel\Appearance\Schemes') then
      GetValueNames(FColorSchemes);
  finally
    Free;
  end;
  TStringlist(FColorSchemes).Sort;
  Result := FColorSchemes;
end;

function TJvMiscInfo.GetCurrentColorScheme: string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, '\Control Panel\Current', 'Color Schemes', '');
end;

function TJvMiscInfo.GetDVDRegion: Integer;
begin
  Result := RegReadIntegerDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'DVD_Region', -1);
end;

function TJvMiscInfo.GetHardwareProfile: TJvHardwareProfile;
begin
  if FHardwareProfile = nil then
    FHardwareProfile := TJvHardwareProfile.Create;
  Result := FHardwareProfile;
end;

function TJvMiscInfo.GetIsOnline: Boolean;
const
  INTERNET_CONNECTION_OFFLINE = $20;
var
  ConnectFlag: DWORD;
begin
  { TODO : Load dynamically (not all have WinInet) }
  Result := InternetGetConnectedState(@ConnectFlag, 0) and (ConnectFlag <> INTERNET_CONNECTION_OFFLINE);
end;

function TJvMiscInfo.GetNetBIOS: Boolean;
var
  P: _NCB;
begin
  Result := RtdlNetBios(@P) <> NRC_OPENERR;
end;

function TJvMiscInfo.GetPattern: string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'Pattern', '');
end;

function TJvMiscInfo.GetScreenSaver: string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'SCRNSAVE.EXE', '');
  if Result <> '' then
    // screen saver is usually returned in 8.3 format
    Result := PathGetLongName(Result);
end;

function TJvMiscInfo.GetTimeRunning: Int64;
begin
  Result := GetTickCount64;
end;
(*
Added to JvJVCLUtils:

function GetTickCount64: Int64;
var
  QFreq, QCount:Int64;
begin
   Result := GetTickCount;
   if QueryPerformanceFrequency(QFreq) then
   begin
     QueryPerformanceCounter(QCount);
     if QFreq <> 0 then
       Result := (QCount div QFreq) * 1000;
  end;
end;
*)

function TJvMiscInfo.GetTimeRunningAsString: string;
var
  DateTime: TDateTime;
begin
  DateTime := GetTimeRunning / 86400000;
  Result := Format('%d %s', [Trunc(DateTime), TimeToStr(DateTime)]);
end;

function TJvMiscInfo.GetVersions: TJvAppVersions;
begin
  if FVersions = nil then
    FVersions := TJvAppVersions.Create;
  Result := FVersions;
end;

function TJvMiscInfo.GetWallpaper: string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'Wallpaper', '');
end;

function TJvMiscInfo.GetWallpaperStyle: TJvWallpaperStyle;
begin
  Result := TJvWallpaperStyle(RegReadIntegerDef(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'WallpaperStyle', 0));
end;

function TJvMiscInfo.GetWallpaperTiled: Boolean;
begin
  Result := RegReadIntegerDef(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'TileWallpaper', 0) <> 0;
end;

procedure TJvMiscInfo.SetColorSchemes(const Value: TStrings);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetCurrentColorScheme(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_CURRENT_USER, '\Control Panel\Current', 'Color Schemes', Value)
  else
    RaiseReadOnly;
end;

procedure TJvMiscInfo.SetDVDRegion(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetHardwareProfile(const Value: TJvHardwareProfile);
begin
  //
end;

procedure TJvMiscInfo.SetIsOnline(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetNetBIOS(const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetPattern(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'Pattern', Value)
  else
    RaiseReadOnly;
end;

procedure TJvMiscInfo.SetScreenSaver(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'SCRSAVE.EXE', Value)
  else
    RaiseReadOnly;
end;

procedure TJvMiscInfo.SetTimeRunning(const Value: Int64);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetTimeRunningAsString(const Value: string);
begin
  RaiseReadOnly;
end;

procedure TJvMiscInfo.SetVersions(const Value: TJvAppVersions);
begin
  //
end;

procedure TJvMiscInfo.SetWallpaper(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteString(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'Wallpaper', Value)
  else
    RaiseReadOnly;
end;

procedure TJvMiscInfo.SetWallpaperStyle(const Value: TJvWallpaperStyle);
begin
  if not IsDesigning and not ReadOnly then
  begin
    if Value <> wsUnused then
      RegWriteInteger(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'WallpaperStyle', Ord(Value));
  end
  else
    RaiseReadOnly;
end;

procedure TJvMiscInfo.SetWallpaperTiled(const Value: Boolean);
begin
  if not IsDesigning and not ReadOnly then
    RegWriteInteger(HKEY_CURRENT_USER, HKCU_CONTROL_PANEL_DESKTOP, 'TileWallpaper', Ord(Value))
  else
    RaiseReadOnly;
end;

//=== { TJvMetricsInfo } =====================================================

function TJvMetricsInfo.GetACP: Integer;
begin
  Result := Windows.GetACP;
end;

function TJvMetricsInfo.GetArrange: TJvWindowsArrange;
begin
  Result := ArrangeToWindowsArrange(GetSystemMetrics(SM_ARRANGE));
end;

function TJvMetricsInfo.GetBoolMetrics(const Index: Integer): Boolean;
begin
  Result := GetSystemMetrics(Index) <> 0;
end;

function TJvMetricsInfo.GetCaretBlinkTime: DWORD;
begin
  Result := Windows.GetCaretBlinkTime;
end;

function TJvMetricsInfo.GetCaretPos(const Index: Integer): Integer;
var
  P: TPoint;
begin
  Result := -1;
  if not Windows.GetCaretPos(P) then
    P := Point(-1, -1);
  case Index of
    0:
      Result := P.X;
    1:
      Result := P.Y;
  end;
end;

function TJvMetricsInfo.GetCleanBoot: TJvCleanBoot;
begin
  Result := TJvCleanBoot(GetSystemMetrics(SM_CLEANBOOT));
end;

function TJvMetricsInfo.GetCursorPos(const Index: Integer): Integer;
var
  P: TPoint;
begin
  Result := -1;
  if not Windows.GetCursorPos(P) then
    P := Point(-1, -1);
  case Index of
    0:
      Result := P.X;
    1:
      Result := P.Y;
  end;
end;

function TJvMetricsInfo.GetDialogBaseUnits: Integer;
begin
  Result := Windows.GetDialogBaseUnits;
end;

function TJvMetricsInfo.GetDoubleClickTime: Integer;
begin
  Result := Windows.GetDoubleClickTime;
end;

function TJvMetricsInfo.GetMetrics(const Index: Integer): Integer;
begin
  Result := GetSystemMetrics(Index);
end;

function TJvMetricsInfo.GetOEMCP: Integer;
begin
  Result := Windows.GetOEMCP;
end;

procedure TJvMetricsInfo.SetACP(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetArrange(const Value: TJvWindowsArrange);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetBoolMetrics(const Index: Integer;
  const Value: Boolean);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetCaretBlinkTime(const Value: DWORD);
begin
  if not IsDesigning and not ReadOnly then
    Windows.SetCaretBlinkTime(Value)
  else
    RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetCaretPos(const Index, Value: Integer);
var
  P: TPoint;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Windows.GetCaretPos(P);
    case Index of
      0:
        P.X := Value;
      1:
        P.Y := Value;
    end;
    Windows.SetCaretPos(P.X, P.Y);
  end
  else
    RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetCleanBoot(const Value: TJvCleanBoot);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetCursorPos(const Index, Value: Integer);
var
  P: TPoint;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Windows.GetCursorPos(P);
    case Index of
      0:
        P.X := Value;
      1:
        P.Y := Value;
    end;
    Windows.SetCursorPos(P.X, P.Y);
  end
  else
    RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetDialogBaseUnits(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetDoubleClickTime(const Value: Integer);
begin

end;

procedure TJvMetricsInfo.SetMetrics(const Index, Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvMetricsInfo.SetOEMCP(const Value: Integer);
begin
  RaiseReadOnly;
end;

//=== { TJvAccessTimeOut } ===================================================

function TJvAccessTimeOut.GetFlags: TJvAccessTimeOutFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and ATF_ONOFFFEEDBACK = ATF_ONOFFFEEDBACK then
    Include(Result, atfOnOffFeedback);
  if Flags and ATF_TIMEOUTON = ATF_TIMEOUTON then
    Include(Result, atfTimeOutOn);
end;

function TJvAccessTimeOut.GetNativeType: ACCESSTIMEOUT;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETACCESSTIMEOUT, SizeOf(Result), @Result, 0) then
  begin
    Result.dwFlags := 0;
    Result.iTimeOutMSec := 0;
  end;
end;

function TJvAccessTimeOut.GetTimeOutMS: DWORD;
begin
  Result := NativeType.iTimeOutMSec;
end;

procedure TJvAccessTimeOut.SetFlags(const Value: TJvAccessTimeOutFlags);
var
  Native: ACCESSTIMEOUT;
  Flags: DWORD;
begin
  Flags := 0;
  if atfOnOffFeedback in Value then
    Flags := Flags or ATF_ONOFFFEEDBACK;
  if atfTimeOutOn in Value then
    Flags := Flags or ATF_TIMEOUTON;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvAccessTimeOut.SetNativeType(Value: ACCESSTIMEOUT);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETACCESSTIMEOUT, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvAccessTimeOut.SetTimeOutMS(const Value: DWORD);
var
  Native: ACCESSTIMEOUT;
begin
  Native := NativeType;
  if Native.iTimeOutMSec <> Value then
  begin
    Native.iTimeOutMSec := Value;
    NativeType := Native;
  end;
end;

//=== { TJvFilterKeys } ======================================================

function TJvFilterKeys.GetBounceMSec: DWORD;
begin
  Result := NativeType.iBounceMSec
end;

function TJvFilterKeys.GetDelayMSec: DWORD;
begin
  Result := NativeType.iDelayMSec;
end;

function TJvFilterKeys.GetFlags: TJvFilterKeyFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and FKF_AVAILABLE = FKF_AVAILABLE then
    Include(Result, fkfAvailable);
  if Flags and FKF_CLICKON = FKF_CLICKON then
    Include(Result, fkfClickOn);
  if Flags and FKF_FILTERKEYSON = FKF_FILTERKEYSON then
    Include(Result, fkfFilterKeysOn);
  if Flags and FKF_HOTKEYACTIVE = FKF_HOTKEYACTIVE then
    Include(Result, fkfHotkeyActive);
  if Flags and FKF_HOTKEYSOUND = FKF_HOTKEYSOUND then
    Include(Result, fkfHotkeySound);
  if Flags and FKF_CONFIRMHOTKEY = FKF_CONFIRMHOTKEY then
    Include(Result, fkfConfirmHotkey);
  if Flags and FKF_INDICATOR = FKF_INDICATOR then
    Include(Result, fkfIndicator);
end;

function TJvFilterKeys.GetNativeType: FILTERKEYS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETFILTERKEYS, SizeOf(Result), @Result, 0) then
  begin
    Result.dwFlags := 0;
    Result.iWaitMSec := 0;
    Result.iDelayMSec := 0;
    Result.iRepeatMSec := 0;
    Result.iBounceMSec := 0;
  end;
end;

function TJvFilterKeys.GetRepeatMSec: DWORD;
begin
  Result := NativeType.iRepeatMSec;
end;

function TJvFilterKeys.GetWaitMSec: DWORD;
begin
  Result := NativeType.iWaitMSec;
end;

procedure TJvFilterKeys.SetBounceMSec(const Value: DWORD);
var
  Native: FILTERKEYS;
begin
  Native := NativeType;
  if NativeType.iBounceMSec <> Value then
  begin
    Native.iBounceMSec := Value;
    NativeType := Native;
  end;
end;

procedure TJvFilterKeys.SetDelayMSec(const Value: DWORD);
var
  Native: FILTERKEYS;
begin
  Native := NativeType;
  if NativeType.iDelayMSec <> Value then
  begin
    Native.iDelayMSec := Value;
    NativeType := Native;
  end;
end;

procedure TJvFilterKeys.SetFlags(const Value: TJvFilterKeyFlags);
var
  Native: FILTERKEYS;
  Flags: DWORD;
begin
  Flags := 0;
  if fkfAvailable in Value then
    Flags := Flags or FKF_AVAILABLE;
  if fkfClickOn in Value then
    Flags := Flags or FKF_CLICKON;
  if fkfFilterKeysOn in Value then
    Flags := Flags or FKF_FILTERKEYSON;
  if fkfHotkeyActive in Value then
    Flags := Flags or FKF_HOTKEYACTIVE;
  if fkfHotkeySound in Value then
    Flags := Flags or FKF_HOTKEYSOUND;
  if fkfConfirmHotkey in Value then
    Flags := Flags or FKF_CONFIRMHOTKEY;
  if fkfIndicator in Value then
    Flags := Flags or FKF_INDICATOR;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvFilterKeys.SetNativeType(Value: FILTERKEYS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETFILTERKEYS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvFilterKeys.SetRepeatMSec(const Value: DWORD);
var
  Native: FILTERKEYS;
begin
  Native := NativeType;
  if NativeType.iRepeatMSec <> Value then
  begin
    Native.iRepeatMSec := Value;
    NativeType := Native;
  end;
end;

procedure TJvFilterKeys.SetWaitMSec(const Value: DWORD);
var
  Native: FILTERKEYS;
begin
  Native := NativeType;
  if NativeType.iWaitMSec <> Value then
  begin
    Native.iWaitMSec := Value;
    NativeType := Native;
  end;
end;

//=== { TJvHighContrast } ====================================================

function TJvHighContrast.GetDefaultScheme: string;
begin
  Result := NativeType.lpszDefaultScheme;
end;

function TJvHighContrast.GetFlags: TJvHighContrastFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and HCF_AVAILABLE = HCF_AVAILABLE then
    Include(Result, hcfAvailable);
  if Flags and HCF_CONFIRMHOTKEY = HCF_CONFIRMHOTKEY then
    Include(Result, hcfConfirmHotKey);
  if Flags and HCF_HIGHCONTRASTON = HCF_HIGHCONTRASTON then
    Include(Result, hcfHighContrastOn);
  if Flags and HCF_HOTKEYACTIVE = HCF_HOTKEYACTIVE then
    Include(Result, hcfHotkeyActive);
  if Flags and HCF_HOTKEYAVAILABLE = HCF_HOTKEYAVAILABLE then
    Include(Result, hcfHotkeyAvailable);
  if Flags and HCF_HOTKEYSOUND = HCF_HOTKEYSOUND then
    Include(Result, hcfHotkeySound);
  if Flags and HCF_INDICATOR = HCF_INDICATOR then
    Include(Result, hcfIndicator);
end;

function TJvHighContrast.GetNativeType: HIGHCONTRAST;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETHIGHCONTRAST, SizeOf(Result), @Result, 0) then
  begin
    Result.dwFlags := 0;
    Result.lpszDefaultScheme := '';
  end;
end;

procedure TJvHighContrast.SetDefaultScheme(const Value: string);
var
  Native: HIGHCONTRAST;
begin
  Native := NativeType;
  if not AnsiSameText(Native.lpszDefaultScheme, Value) then
  begin
    Native.lpszDefaultScheme := PChar(Value);
    NativeType := Native;
  end;
end;

procedure TJvHighContrast.SetFlags(const Value: TJvHighContrastFlags);
var
  Native: HIGHCONTRAST;
  Flags: DWORD;
begin
  Flags := 0;
  if hcfAvailable in Value then
    Flags := Flags or HCF_AVAILABLE;
  if hcfConfirmHotKey in Value then
    Flags := Flags or HCF_CONFIRMHOTKEY;
  if hcfHighContrastOn in Value then
    Flags := Flags or HCF_HIGHCONTRASTON;
  if hcfHotkeyActive in Value then
    Flags := Flags or HCF_HOTKEYACTIVE;
  if hcfHotkeyAvailable in Value then
    Flags := Flags or HCF_HOTKEYAVAILABLE;
  if hcfHotkeySound in Value then
    Flags := Flags or HCF_HOTKEYSOUND;
  if hcfIndicator in Value then
    Flags := Flags or HCF_INDICATOR;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvHighContrast.SetNativeType(Value: HIGHCONTRAST);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETHIGHCONTRAST, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

//=== { TJvIconMetrics } =====================================================

destructor TJvIconMetrics.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

function TJvIconMetrics.GetFont: TFont;
begin
  if FFont = nil then
    FFont := TFont.Create;
  UpdateFromLogFont(FFont, NativeType.lfFont);
  Result := FFont;
end;

function TJvIconMetrics.GetHorzSpacing: Integer;
begin
  Result := NativeType.iHorzSpacing;
end;

function TJvIconMetrics.GetNativeType: ICONMETRICS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETICONMETRICS, SizeOf(Result), @Result, 0) then
  begin
    Result.iHorzSpacing := 0;
    Result.iVertSpacing := 0;
    Result.iTitleWrap := 0;
    //    Result.lfFont  := ?:
  end;
end;

function TJvIconMetrics.GetTitleWrap: Boolean;
begin
  Result := NativeType.iTitleWrap <> 0;
end;

function TJvIconMetrics.GetVertSpacing: Integer;
begin
  Result := NativeType.iVertSpacing;
end;

procedure TJvIconMetrics.SetFont(const Value: TFont);
var
  Native: ICONMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfFont);
  NativeType := Native;
end;

procedure TJvIconMetrics.SetHorzSpacing(const Value: Integer);
var
  Native: ICONMETRICS;
begin
  Native := NativeType;
  if Native.iHorzSpacing <> Value then
  begin
    Native.iHorzSpacing := Value;
    NativeType := Native;
  end;
end;

procedure TJvIconMetrics.SetNativeType(Value: ICONMETRICS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETICONMETRICS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvIconMetrics.SetTitleWrap(const Value: Boolean);
var
  Native: ICONMETRICS;
begin
  Native := NativeType;
  if Native.iTitleWrap <> Ord(Value) then
  begin
    Native.iTitleWrap := Ord(Value);
    NativeType := Native;
  end;
end;

procedure TJvIconMetrics.SetVertSpacing(const Value: Integer);
var
  Native: ICONMETRICS;
begin
  Native := NativeType;
  if Native.iVertSpacing <> Value then
  begin
    Native.iVertSpacing := Value;
    NativeType := Native;
  end;
end;

//=== { TJvMinimizedMetrics } ================================================

function TJvMinimizedMetrics.GetArrange: TJvWindowsArrange;
begin
  Result := ArrangeToWindowsArrange(NativeType.iArrange);
end;

function TJvMinimizedMetrics.GetHorzGap: Integer;
begin
  Result := NativeType.iHorzGap;
end;

function TJvMinimizedMetrics.GetNativeType: MINIMIZEDMETRICS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETMINIMIZEDMETRICS, SizeOf(Result), @Result, 0) then
  begin
    Result.iWidth := 0;
    Result.iHorzGap := 0;
    Result.iVertGap := 0;
    Result.iArrange := 0;
  end;
end;

function TJvMinimizedMetrics.GetVertGap: Integer;
begin
  Result := NativeType.iVertGap;
end;

function TJvMinimizedMetrics.GetWidth: Integer;
begin
  Result := NativeType.iWidth;
end;

procedure TJvMinimizedMetrics.SetArrange(const Value: TJvWindowsArrange);
var
  Native: MINIMIZEDMETRICS;
  WArrange: Integer;
begin
  Native := NativeType;
  WArrange := WindowsArrangeToArrange(Value);
  if Native.iArrange <> WArrange then
  begin
    Native.iArrange := WArrange;
    NativeType := Native;
  end;
end;

procedure TJvMinimizedMetrics.SetHorzGap(const Value: Integer);
var
  Native: MINIMIZEDMETRICS;
begin
  Native := NativeType;
  if Native.iHorzGap <> Value then
  begin
    Native.iHorzGap := Value;
    NativeType := Native;
  end;
end;

procedure TJvMinimizedMetrics.SetNativeType(Value: MINIMIZEDMETRICS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETMINIMIZEDMETRICS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvMinimizedMetrics.SetVertGap(const Value: Integer);
var
  Native: MINIMIZEDMETRICS;
begin
  Native := NativeType;
  if Native.iVertGap <> Value then
  begin
    Native.iVertGap := Value;
    NativeType := Native;
  end;
end;

procedure TJvMinimizedMetrics.SetWidth(const Value: Integer);
var
  Native: MINIMIZEDMETRICS;
begin
  Native := NativeType;
  if Native.iWidth <> Value then
  begin
    Native.iWidth := Value;
    NativeType := Native;
  end;
end;

//=== { TJvMouseKeys } =======================================================

function TJvMouseKeys.GetCtrlSpeed: DWORD;
begin
  Result := NativeType.iCtrlSpeed;
end;

function TJvMouseKeys.GetFlags: TJvMouseKeysFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and MKF_AVAILABLE = MKF_AVAILABLE then
    Include(Result, mkfAvailable);
  if Flags and MKF_CONFIRMHOTKEY = MKF_CONFIRMHOTKEY then
    Include(Result, mkfConfirmHotKey);
  if Flags and MKF_HOTKEYACTIVE = MKF_HOTKEYACTIVE then
    Include(Result, mkfHotkeyActive);
  if Flags and MKF_HOTKEYSOUND = MKF_HOTKEYSOUND then
    Include(Result, mkfHotkeySound);
  if Flags and MKF_INDICATOR = MKF_INDICATOR then
    Include(Result, mkfIndicator);
  if Flags and MKF_MOUSEKEYSON = MKF_MOUSEKEYSON then
    Include(Result, mkfMouseKeysOn);
  if Flags and MKF_MODIFIERS = MKF_MODIFIERS then
    Include(Result, mkfModifiers);
  if Flags and MKF_REPLACENUMBERS = MKF_REPLACENUMBERS then
    Include(Result, mkfReplaceNumbers);
end;

function TJvMouseKeys.GetMaxSpeed: DWORD;
begin
  Result := NativeType.iMaxSpeed;
end;

function TJvMouseKeys.GetNativeType: MOUSEKEYS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETMOUSEKEYS, SizeOf(Result), @Result, 0) then
  begin
    Result.dwFlags := 0;
    Result.iMaxSpeed := 0;
    Result.iTimeToMaxSpeed := 0;
    Result.iCtrlSpeed := 0;
    Result.dwReserved1 := 0;
    Result.dwReserved2 := 0;
  end;
end;

function TJvMouseKeys.GetTimeToMaxSpeed: DWORD;
begin
  Result := NativeType.iTimeToMaxSpeed;
end;

procedure TJvMouseKeys.SetCtrlSpeed(const Value: DWORD);
var
  Native: MOUSEKEYS;
begin
  Native := NativeType;
  if Native.iCtrlSpeed <> Value then
  begin
    Native.iCtrlSpeed := Value;
    NativeType := Native;
  end;
end;

procedure TJvMouseKeys.SetFlags(const Value: TJvMouseKeysFlags);
var
  Native: MOUSEKEYS;
  Flags: DWORD;
begin
  Flags := 0;
  if mkfAvailable in Value then
    Flags := Flags or MKF_AVAILABLE;
  if mkfConfirmHotKey in Value then
    Flags := Flags or MKF_CONFIRMHOTKEY;
  if mkfHotkeyActive in Value then
    Flags := Flags or MKF_HOTKEYACTIVE;
  if mkfHotkeySound in Value then
    Flags := Flags or MKF_HOTKEYSOUND;
  if mkfIndicator in Value then
    Flags := Flags or MKF_INDICATOR;
  if mkfMouseKeysOn in Value then
    Flags := Flags or MKF_MOUSEKEYSON;
  if mkfModifiers in Value then
    Flags := Flags or MKF_MODIFIERS;
  if mkfReplaceNumbers in Value then
    Flags := Flags or MKF_REPLACENUMBERS;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvMouseKeys.SetMaxSpeed(const Value: DWORD);
var
  Native: MOUSEKEYS;
begin
  Native := NativeType;
  if Native.iMaxSpeed <> Value then
  begin
    Native.iMaxSpeed := Value;
    NativeType := Native;
  end;
end;

procedure TJvMouseKeys.SetNativeType(Value: MOUSEKEYS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETMOUSEKEYS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvMouseKeys.SetTimeToMaxSpeed(const Value: DWORD);
var
  Native: MOUSEKEYS;
begin
  Native := NativeType;
  if Native.iTimeToMaxSpeed <> Value then
  begin
    Native.iTimeToMaxSpeed := Value;
    NativeType := Native;
  end;
end;

//=== { TJvNonClientMetrics } ================================================

destructor TJvNonClientMetrics.Destroy;
begin
  FCaptionFont.Free;
  FMenuFont.Free;
  FMessageFont.Free;
  FStatusFont.Free;
  inherited Destroy;
end;

function TJvNonClientMetrics.GetBorderWidth: Integer;
begin
  Result := NativeType.iBorderWidth;
end;

function TJvNonClientMetrics.GetCaptionFont: TFont;
begin
  if FCaptionFont = nil then
    FCaptionFont := TFont.Create;
  UpdateFromLogFont(FCaptionFont, NativeType.lfCaptionFont);
  Result := FCaptionFont;
end;

function TJvNonClientMetrics.GetCaptionHeight: Integer;
begin
  Result := NativeType.iCaptionHeight;
end;

function TJvNonClientMetrics.GetCaptionWidth: Integer;
begin
  Result := NativeType.iCaptionWidth;
end;

function TJvNonClientMetrics.GetMenuFont: TFont;
begin
  if FMenuFont = nil then
    FMenuFont := TFont.Create;
  UpdateFromLogFont(FMenuFont, NativeType.lfMenuFont);
  Result := FMenuFont;
end;

function TJvNonClientMetrics.GetMenuHeight: Integer;
begin
  Result := NativeType.iMenuHeight;
end;

function TJvNonClientMetrics.GetMenuWidth: Integer;
begin
  Result := NativeType.iMenuWidth;
end;

function TJvNonClientMetrics.GetMessageFont: TFont;
begin
  if FMessageFont = nil then
    FMessageFont := TFont.Create;
  UpdateFromLogFont(FMessageFont, NativeType.lfMessageFont);
  Result := FMessageFont;
end;

function TJvNonClientMetrics.GetNativeType: NONCLIENTMETRICS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Result), @Result, 0) then
  begin
    Result.iBorderWidth := 0;
    Result.iScrollWidth := 0;
    Result.iScrollHeight := 0;
    Result.iCaptionWidth := 0;
    Result.iCaptionHeight := 0;
    //    Result.lfCaptionFont := ?:
    Result.iSmCaptionWidth := 0;
    Result.iSmCaptionHeight := 0;
    //    Result.lfSmCaptionFont  := ?:
    Result.iMenuWidth := 0;
    Result.iMenuHeight := 0;
    //    Result.lfMenuFont := ?:
    //    Result.lfStatusFont := ?;
    //    Result.lfMessageFont := ?;
  end;
end;

function TJvNonClientMetrics.GetScrollHeight: Integer;
begin
  Result := NativeType.iScrollHeight;
end;

function TJvNonClientMetrics.GetScrollWidth: Integer;
begin
  Result := NativeType.iScrollWidth;
end;

function TJvNonClientMetrics.GetSmallCaptionFont: TFont;
begin
  if FSmallCaptionFont = nil then
    FSmallCaptionFont := TFont.Create;
  UpdateFromLogFont(FSmallCaptionFont, NativeType.lfSmCaptionFont);
  Result := FSmallCaptionFont;
end;

function TJvNonClientMetrics.GetSmallCaptionHeight: Integer;
begin
  Result := NativeType.iSmCaptionHeight;
end;

function TJvNonClientMetrics.GetSmallCaptionWidth: Integer;
begin
  Result := NativeType.iSmCaptionWidth;
end;

function TJvNonClientMetrics.GetStatusFont: TFont;
begin
  if FStatusFont = nil then
    FStatusFont := TFont.Create;
  UpdateFromLogFont(FStatusFont, NativeType.lfStatusFont);
  Result := FStatusFont;
end;

procedure TJvNonClientMetrics.SetBorderWidth(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iBorderWidth <> Value then
  begin
    Native.iBorderWidth := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetCaptionFont(const Value: TFont);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfCaptionFont);
  NativeType := Native;
end;

procedure TJvNonClientMetrics.SetCaptionHeight(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iCaptionHeight <> Value then
  begin
    Native.iCaptionHeight := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetCaptionWidth(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iCaptionWidth <> Value then
  begin
    Native.iCaptionWidth := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetMenuFont(const Value: TFont);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfMenuFont);
  NativeType := Native;
end;

procedure TJvNonClientMetrics.SetMenuHeight(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iMenuHeight <> Value then
  begin
    Native.iMenuHeight := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetMenuWidth(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iMenuWidth <> Value then
  begin
    Native.iMenuWidth := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetMessageFont(const Value: TFont);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfMessageFont);
  NativeType := Native;
end;

procedure TJvNonClientMetrics.SetNativeType(Value: NONCLIENTMETRICS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETNONCLIENTMETRICS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvNonClientMetrics.SetScrollHeight(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iScrollHeight <> Value then
  begin
    Native.iScrollHeight := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetScrollWidth(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iScrollWidth <> Value then
  begin
    Native.iScrollWidth := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetSmallCaptionFont(const Value: TFont);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfSmCaptionFont);
  NativeType := Native;
end;

procedure TJvNonClientMetrics.SetSmallCaptionHeight(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iSmCaptionHeight <> Value then
  begin
    Native.iSmCaptionHeight := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetSmallCaptionWidth(const Value: Integer);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  if Native.iSmCaptionWidth <> Value then
  begin
    Native.iSmCaptionWidth := Value;
    NativeType := Native;
  end;
end;

procedure TJvNonClientMetrics.SetStatusFont(const Value: TFont);
var
  Native: NONCLIENTMETRICS;
begin
  Native := NativeType;
  UpdateToLogFont(Value, Native.lfStatusFont);
  NativeType := Native;
end;

//== TJvSerialKeys ===========================================================

function TJvSerialKeys.GetActive: Boolean;
begin
  Result := NativeType.iActive <> 0;
end;

function TJvSerialKeys.GetActivePort: string;
begin
  Result := NativeType.lpszActivePort;
end;

function TJvSerialKeys.GetBaudRate: DWORD;
begin
  Result := NativeType.iBaudRate;
end;

function TJvSerialKeys.GetFlags: TJvSerialKeysFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and SERKF_AVAILABLE = SERKF_AVAILABLE then
    Include(Result, serkfAvailable);
  if Flags and SERKF_INDICATOR = SERKF_INDICATOR then
    Include(Result, serkfIndicator);
  if Flags and SERKF_SERIALKEYSON = SERKF_SERIALKEYSON then
    Include(Result, serkfSerialKeysOn);
end;

function TJvSerialKeys.GetNativeType: SERIALKEYS;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETSERIALKEYS, SizeOf(Result), @Result, 0) then
  begin
    Result.lpszActivePort := '';
    Result.lpszPort := '';
    Result.dwFlags := 0;
    Result.iBaudRate := 0;
    Result.iPortState := 0;
    Result.iActive := 0;
  end;
end;

function TJvSerialKeys.GetPort: string;
begin
  Result := NativeType.lpszPort;
end;

function TJvSerialKeys.GetPortState: TJvSerialKeysPortState;
begin
  Result := TJvSerialKeysPortState(NativeType.iPortState);
end;

procedure TJvSerialKeys.SetActive(const Value: Boolean);
var
  Native: SERIALKEYS;
begin
  Native := NativeType;
  if Integer(Native.iActive) <> Ord(Value) then
  begin
    Native.iActive := Ord(Value);
    NativeType := Native;
  end;
end;

procedure TJvSerialKeys.SetActivePort(const Value: string);
var
  Native: SERIALKEYS;
begin
  Native := NativeType;
  if not AnsiSameText(Native.lpszActivePort, Value) then
  begin
    Native.lpszActivePort := PChar(Value);
    NativeType := Native;
  end;
end;

procedure TJvSerialKeys.SetBaudRate(const Value: DWORD);
var
  Native: SERIALKEYS;
begin
  Native := NativeType;
  if Native.iBaudRate <> Value then
  begin
    Native.iBaudRate := Value;
    NativeType := Native;
  end;
end;

procedure TJvSerialKeys.SetFlags(const Value: TJvSerialKeysFlags);
var
  Native: SERIALKEYS;
  Flags: DWORD;
begin
  Flags := 0;
  if serkfAvailable in Value then
    Flags := Flags or SERKF_AVAILABLE;
  if serkfIndicator in Value then
    Flags := Flags or SERKF_INDICATOR;
  if serkfSerialKeysOn in Value then
    Flags := Flags or SERKF_SERIALKEYSON;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvSerialKeys.SetNativeType(Value: SERIALKEYS);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETSERIALKEYS, SizeOf(Value), @Value, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSerialKeys.SetPort(const Value: string);
var
  Native: SERIALKEYS;
begin
  Native := NativeType;
  if not AnsiSameText(Native.lpszPort, Value) then
  begin
    Native.lpszPort := PChar(Value);
    NativeType := Native;
  end;
end;

procedure TJvSerialKeys.SetPortState(const Value: TJvSerialKeysPortState);
var
  Native: SERIALKEYS;
begin
  Native := NativeType;
  if Integer(Native.iPortState) <> Ord(Value) then
  begin
    Native.iPortState := Ord(Value);
    NativeType := Native;
  end;
end;

//=== { TJvSoundSentry } =====================================================

function TJvSoundSentry.GetFlags: TJvSoundSentryFlags;
var
  Flags: DWORD;
begin
  Flags := NativeType.dwFlags;
  Result := [];
  if Flags and SSF_AVAILABLE = SSF_AVAILABLE then
    Include(Result, ssfAvailable);
  if Flags and SSF_SOUNDSENTRYON = SSF_SOUNDSENTRYON then
    Include(Result, ssfSoundSentryOn);
  if Flags and SSF_INDICATOR = SSF_INDICATOR then
    Include(Result, ssfIndicator);
end;

function TJvSoundSentry.GetGrafEffect: TJvSoundSentryGrafEffect;
begin
  if NativeType.iFSGrafEffect = 3 then
    Result := ssgfDisplay
  else
    Result := ssgfNone;
end;

function TJvSoundSentry.GetGrafEffectColor: TColor;
begin
  Result := TColor(NativeType.iFSGrafEffectColor);
end;

function TJvSoundSentry.GetGrafEffectMSec: DWORD;
begin
  Result := NativeType.iFSGrafEffectMSec;
end;

function TJvSoundSentry.GetNativeType: SOUNDSENTRY;
begin
  Result.cbSize := SizeOf(Result);
  if not SystemParametersInfo(SPI_GETSOUNDSENTRY, SizeOf(Result), @Result, 0) then
  begin
    Result.dwFlags := 0;
    Result.iFSTextEffect := 0;
    Result.iFSTextEffectMSec := 0;
    Result.iFSTextEffectColorBits := 0;
    Result.iFSGrafEffect := 0;
    Result.iFSGrafEffectMSec := 0;
    Result.iFSGrafEffectColor := 0;
    Result.iWindowsEffect := 0;
    Result.iWindowsEffectMSec := 0;
    Result.lpszWindowsEffectDLL := '';
    Result.iWindowsEffectOrdinal := 0;
  end;
end;

function TJvSoundSentry.GetTextEffect: TJvSoundSentryTextEffect;
begin
  Result := TJvSoundSentryTextEffect(NativeType.iFSTextEffect);
end;

function TJvSoundSentry.GetTextEffectColor: TColor;
begin
  Result := TColor(NativeType.iFSTextEffectColorBits);
end;

function TJvSoundSentry.GetTextEffectMSec: DWORD;
begin
  Result := NativeType.iFSTextEffectMSec;
end;

function TJvSoundSentry.GetWindowsEffect: TJvSoundSentryWindowsEffect;
begin
  Result := TJvSoundSentryWindowsEffect(NativeType.iWindowsEffect);
end;

function TJvSoundSentry.GetWindowsEffectDLL: string;
begin
  Result := NativeType.lpszWindowsEffectDLL;
end;

function TJvSoundSentry.GetWindowsEffectMSec: DWORD;
begin
  Result := NativeType.iWindowsEffectMSec;
end;

procedure TJvSoundSentry.SetFlags(const Value: TJvSoundSentryFlags);
var
  Flags: DWORD;
  Native: SOUNDSENTRY;
begin
  Flags := 0;
  if ssfAvailable in Value then
    Flags := Flags or SSF_AVAILABLE;
  if ssfSoundSentryOn in Value then
    Flags := Flags or SSF_SOUNDSENTRYON;
  if ssfIndicator in Value then
    Flags := Flags or SSF_INDICATOR;
  Native := NativeType;
  if Native.dwFlags <> Flags then
  begin
    Native.dwFlags := Flags;
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetGrafEffect(const Value: TJvSoundSentryGrafEffect);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSGrafEffect <> DWORD(Value) then
  begin
    Native.iFSGrafEffect := DWORD(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetGrafEffectColor(const Value: TColor);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSGrafEffectColor <> DWORD(Value) then
  begin
    Native.iFSGrafEffectColor := DWORD(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetGrafEffectMSec(const Value: DWORD);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSGrafEffectMSec <> Value then
  begin
    Native.iFSGrafEffectMSec := Value;
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetNativeType(Value: SOUNDSENTRY);
begin
  if not IsDesigning and not ReadOnly then
  begin
    Value.cbSize := SizeOf(Value);
    SystemParametersInfo(SPI_SETSOUNDSENTRY, SizeOf(Value), @Value, 0);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSoundSentry.SetTextEffect(const Value: TJvSoundSentryTextEffect);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSTextEffect <> DWORD(Value) then
  begin
    Native.iFSTextEffect := DWORD(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetTextEffectColor(const Value: TColor);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSTextEffectColorBits <> DWORD(Value) then
  begin
    Native.iFSTextEffectColorBits := DWORD(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetTextEffectMSec(const Value: DWORD);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iFSTextEffectMSec <> Value then
  begin
    Native.iFSTextEffectMSec := Value;
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetWindowsEffect(const Value: TJvSoundSentryWindowsEffect);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iWindowsEffect <> DWORD(Value) then
  begin
    Native.iWindowsEffect := Ord(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetWindowsEffectDLL(const Value: string);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if not AnsiSameText(Native.lpszWindowsEffectDLL, Value) then
  begin
    Native.lpszWindowsEffectDLL := PChar(Value);
    NativeType := Native;
  end;
end;

procedure TJvSoundSentry.SetWindowsEffectMSec(const Value: DWORD);
var
  Native: SOUNDSENTRY;
begin
  Native := NativeType;
  if Native.iWindowsEffectMSec <> Value then
  begin
    Native.iWindowsEffectMSec := Value;
    NativeType := Native;
  end;
end;

//=== { TJvSystemParametersInfo } ============================================

constructor TJvSystemParametersInfo.Create;
begin
  inherited Create;
  FWorkArea := TJvRect.Create;
end;

destructor TJvSystemParametersInfo.Destroy;
begin
  FAccessTimeOut.Free;
  FFilterKeys.Free;
  FHighContrast.Free;
  FIconMetrics.Free;
  FMinimizedMetrics.Free;
  FMouseKeys.Free;
  FNonClientMetrics.Free;
  FSerialKeys.Free;
  FSoundSentry.Free;
  FWorkArea.Create;
  inherited Destroy;
end;

procedure TJvSystemParametersInfo.InitMap;
begin
  { TODO: make this a hash table for faster access? }
  if Length(FMap) > 0 then
    Exit;
  SetLength(FMap, 96);
  FMap[0] := Point(SPI_GETBEEP, SPI_SETBEEP);
  FMap[1] := Point(SPI_GETMOUSE, SPI_SETMOUSE);
  FMap[2] := Point(SPI_GETBORDER, SPI_SETBORDER);
  FMap[3] := Point(SPI_GETKEYBOARDSPEED, SPI_SETKEYBOARDSPEED);
  FMap[4] := Point(-1, SPI_LANGDRIVER);
  FMap[5] := Point(SPI_ICONHORIZONTALSPACING, SPI_ICONHORIZONTALSPACING);
  FMap[6] := Point(SPI_GETSCREENSAVETIMEOUT, SPI_SETSCREENSAVETIMEOUT);
  FMap[7] := Point(SPI_GETSCREENSAVEACTIVE, SPI_SETSCREENSAVEACTIVE);
  FMap[8] := Point(SPI_GETGRIDGRANULARITY, SPI_SETGRIDGRANULARITY);
  FMap[9] := Point(-1, SPI_SETDESKPATTERN);
  FMap[10] := Point(SPI_GETKEYBOARDDELAY, SPI_SETKEYBOARDDELAY);
  FMap[11] := Point(SPI_ICONVERTICALSPACING, SPI_ICONVERTICALSPACING);
  FMap[12] := Point(SPI_GETICONTITLEWRAP, SPI_SETICONTITLEWRAP);
  FMap[13] := Point(SPI_GETMENUDROPALIGNMENT, SPI_SETMENUDROPALIGNMENT);
  FMap[14] := Point(-1, SPI_SETDOUBLECLKWIDTH);
  FMap[15] := Point(-1, SPI_SETDOUBLECLKHEIGHT);
  FMap[16] := Point(SPI_GETICONTITLELOGFONT, SPI_SETICONTITLELOGFONT);
  FMap[17] := Point(-1, SPI_SETDOUBLECLICKTIME);
  FMap[18] := Point(-1, SPI_SETMOUSEBUTTONSWAP);
  FMap[19] := Point(-1, SPI_SETDRAGWIDTH);
  FMap[20] := Point(-1, SPI_SETDRAGHEIGHT);
  FMap[21] := Point(-1, SPI_SETHANDHELD);
  FMap[22] := Point(SPI_GETFASTTASKSWITCH, SPI_SETFASTTASKSWITCH);
  FMap[23] := Point(SPI_GETDRAGFULLWINDOWS, SPI_SETDRAGFULLWINDOWS);
  FMap[24] := Point(SPI_GETNONCLIENTMETRICS, SPI_SETNONCLIENTMETRICS);
  FMap[25] := Point(SPI_GETMINIMIZEDMETRICS, SPI_SETMINIMIZEDMETRICS);
  FMap[26] := Point(SPI_GETICONMETRICS, SPI_SETICONMETRICS);
  FMap[27] := Point(SPI_GETWORKAREA, SPI_SETWORKAREA);
  FMap[28] := Point(-1, SPI_SETPENWINDOWS);
  FMap[29] := Point(SPI_GETHIGHCONTRAST, SPI_SETHIGHCONTRAST);
  FMap[30] := Point(SPI_GETKEYBOARDPREF, SPI_SETKEYBOARDPREF);
  FMap[31] := Point(SPI_GETSCREENREADER, SPI_SETSCREENREADER);
  FMap[32] := Point(SPI_GETANIMATION, SPI_SETANIMATION);
  FMap[33] := Point(SPI_GETFONTSMOOTHING, SPI_SETFONTSMOOTHING);
  FMap[34] := Point(SPI_GETLOWPOWERTIMEOUT, SPI_SETPOWEROFFTIMEOUT);
  FMap[35] := Point(SPI_GETPOWEROFFTIMEOUT, SPI_SETLOWPOWERTIMEOUT);
  FMap[36] := Point(SPI_GETLOWPOWERACTIVE, SPI_SETLOWPOWERACTIVE);
  FMap[37] := Point(SPI_GETPOWEROFFACTIVE, SPI_SETPOWEROFFACTIVE);
  FMap[38] := Point(-1, SPI_SETCURSORS);
  FMap[39] := Point(-1, SPI_SETICONS);
  FMap[40] := Point(SPI_GETDEFAULTINPUTLANG, SPI_SETDEFAULTINPUTLANG);
  FMap[41] := Point(-1, SPI_SETLANGTOGGLE);
  FMap[42] := Point(SPI_GETWINDOWSEXTENSION, -1);
  FMap[43] := Point(SPI_GETMOUSETRAILS, SPI_SETMOUSETRAILS);
  FMap[44] := Point(SPI_SCREENSAVERRUNNING, -1);
  FMap[45] := Point(SPI_GETFILTERKEYS, SPI_SETFILTERKEYS);
  FMap[46] := Point(SPI_GETTOGGLEKEYS, SPI_SETTOGGLEKEYS);
  FMap[47] := Point(SPI_GETMOUSEKEYS, SPI_SETMOUSEKEYS);
  FMap[48] := Point(SPI_GETSHOWSOUNDS, SPI_SETSHOWSOUNDS);
  FMap[49] := Point(SPI_GETSTICKYKEYS, SPI_SETSTICKYKEYS);
  FMap[50] := Point(SPI_GETACCESSTIMEOUT, SPI_SETACCESSTIMEOUT);
  FMap[51] := Point(SPI_GETSERIALKEYS, SPI_SETSERIALKEYS);
  FMap[52] := Point(SPI_GETSOUNDSENTRY, SPI_SETSOUNDSENTRY);
  FMap[53] := Point(SPI_GETSNAPTODEFBUTTON, SPI_SETSNAPTODEFBUTTON);
  FMap[54] := Point(SPI_GETMOUSEHOVERWIDTH, SPI_SETMOUSEHOVERWIDTH);
  FMap[55] := Point(SPI_GETMOUSEHOVERHEIGHT, SPI_SETMOUSEHOVERHEIGHT);
  FMap[56] := Point(SPI_GETMOUSEHOVERTIME, SPI_SETMOUSEHOVERTIME);
  FMap[57] := Point(SPI_GETWHEELSCROLLLINES, SPI_SETWHEELSCROLLLINES);
  FMap[58] := Point(SPI_GETMENUSHOWDELAY, SPI_SETMENUSHOWDELAY);
  FMap[59] := Point(SPI_GETSHOWIMEUI, SPI_SETSHOWIMEUI);
  FMap[60] := Point(SPI_GETMOUSESPEED, SPI_SETMOUSESPEED);
  FMap[61] := Point(SPI_GETSCREENSAVERRUNNING, -1);
  FMap[62] := Point(SPI_GETACTIVEWINDOWTRACKING, SPI_SETACTIVEWINDOWTRACKING);
  FMap[63] := Point(SPI_GETMENUANIMATION, SPI_SETMENUANIMATION);
  FMap[64] := Point(SPI_GETCOMBOBOXANIMATION, SPI_SETCOMBOBOXANIMATION);
  FMap[65] := Point(SPI_GETLISTBOXSMOOTHSCROLLING, SPI_SETLISTBOXSMOOTHSCROLLING);
  FMap[66] := Point(SPI_GETGRADIENTCAPTIONS, SPI_SETGRADIENTCAPTIONS);
  FMap[67] := Point(SPI_GETKEYBOARDCUES, SPI_SETKEYBOARDCUES);
  FMap[68] := Point(SPI_GETMENUUNDERLINES, SPI_SETMENUUNDERLINES);
  FMap[69] := Point(SPI_GETACTIVEWNDTRKZORDER, SPI_SETACTIVEWNDTRKZORDER);
  FMap[70] := Point(SPI_GETHOTTRACKING, SPI_SETHOTTRACKING);
  FMap[71] := Point(SPI_GETMENUFADE, SPI_SETMENUFADE);
  FMap[72] := Point(SPI_GETSELECTIONFADE, SPI_SETSELECTIONFADE);
  FMap[73] := Point(SPI_GETTOOLTIPANIMATION, SPI_SETTOOLTIPANIMATION);
  FMap[74] := Point(SPI_GETTOOLTIPFADE, SPI_SETTOOLTIPFADE);
  FMap[75] := Point(SPI_GETCURSORSHADOW, SPI_SETCURSORSHADOW);
  FMap[76] := Point(SPI_GETUIEFFECTS, SPI_SETUIEFFECTS);
  FMap[77] := Point(SPI_GETFOREGROUNDLOCKTIMEOUT, SPI_SETFOREGROUNDLOCKTIMEOUT);
  FMap[78] := Point(SPI_GETACTIVEWNDTRKTIMEOUT, SPI_SETACTIVEWNDTRKTIMEOUT);
  FMap[79] := Point(SPI_GETFOREGROUNDFLASHCOUNT, SPI_SETFOREGROUNDFLASHCOUNT);
  FMap[80] := Point(SPI_GETCARETWIDTH, SPI_SETCARETWIDTH);
  FMap[81] := Point(SPI_GETDESKWALLPAPER, SPI_SETDESKWALLPAPER);
  FMap[82] := Point(SPI_GETMOUSESONAR, SPI_SETMOUSESONAR);
  FMap[83] := Point(SPI_GETMOUSECLICKLOCK, SPI_SETMOUSECLICKLOCK);
  FMap[84] := Point(SPI_GETMOUSEVANISH, SPI_SETMOUSEVANISH);
  FMap[85] := Point(SPI_GETFLATMENU, SPI_SETFLATMENU);
  FMap[86] := Point(SPI_GETDROPSHADOW, SPI_SETDROPSHADOW);
  FMap[87] := Point(SPI_GETFOREGROUNDLOCKTIMEOUT, SPI_SETFOREGROUNDLOCKTIMEOUT);
  FMap[88] := Point(SPI_GETACTIVEWNDTRKTIMEOUT, SPI_SETACTIVEWNDTRKTIMEOUT);
  FMap[89] := Point(SPI_GETFOREGROUNDFLASHCOUNT, SPI_SETFOREGROUNDFLASHCOUNT);
  FMap[90] := Point(SPI_GETCARETWIDTH, SPI_SETCARETWIDTH);
  FMap[91] := Point(SPI_GETMOUSECLICKLOCKTIME, SPI_SETMOUSECLICKLOCKTIME);
  FMap[92] := Point(SPI_GETFONTSMOOTHINGTYPE, SPI_SETFONTSMOOTHINGTYPE);
  FMap[93] := Point(SPI_GETFONTSMOOTHINGCONTRAST, SPI_SETFONTSMOOTHINGCONTRAST);
  FMap[94] := Point(SPI_GETFOCUSBORDERWIDTH, SPI_SETFOCUSBORDERWIDTH);
  FMap[95] := Point(SPI_GETFOCUSBORDERHEIGHT, SPI_SETFOCUSBORDERHEIGHT);
end;

function TJvSystemParametersInfo.MapToSet(Index: Integer): Integer;
var
  I: Integer;
begin
  InitMap;
  for I := 0 to Length(FMap) - 1 do
    if FMap[I].Y = Index then
    begin
      Result := FMap[I].X;
      Exit;
    end;
  Result := -1;
end;

{ (ahuser) make Delphi 5 Compiler happy
function MapToGet(Index: Integer): Integer;
var
  I: Integer;
begin
  InitMap;
  for I := 0 to Length(FMap) - 1 do
    if FMap[I].X = Index then
    begin
      Result := FMap[I].Y;
      Exit;
    end;
  Result := -1;
end;}

function TJvSystemParametersInfo.GetAccessTimeOut: TJvAccessTimeOut;
begin
  if FAccessTimeOut = nil then
    FAccessTimeOut := TJvAccessTimeOut.Create;
  Result := FAccessTimeOut;
end;

function TJvSystemParametersInfo.GetAnimationInfo: Boolean;
var
  Info: ANIMATIONINFO;
begin
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

function TJvSystemParametersInfo.GetBoolInfo(const Index: Integer): Boolean;
var
  Value: Integer;
begin
  if SystemParametersInfo(Index, 0, @Value, 0) then
    Result := Value <> 0
  else
    Result := False;
end;

function TJvSystemParametersInfo.GetDeskWallpaper: string;
begin
  // doesn't work
  // SetLength(Result, MAX_PATH);
  // SystemParametersInfo(SPI_GETDESKWALLPAPER, Length(Result), @Result[1], 0);
  Result := '';
end;

function TJvSystemParametersInfo.GetFilterKeys: TJvFilterKeys;
begin
  if FFilterKeys = nil then
    FFilterKeys := TJvFilterKeys.Create;
  Result := FFilterKeys;
end;

function TJvSystemParametersInfo.GetFontSmoothingType: TJvFontSmoothingType;
var
  Value: Integer;
begin
  Result := fstStandard;
  if SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @Value, 0) then
    case Value of
      //    FE_FONTSMOOTHINGSTANDARD: Result := fstStandard;
      FE_FONTSMOOTHINGCLEARTYPE:
        Result := fstClearType;
      FE_FONTSMOOTHINGDOCKING:
        Result := fstDocking;
    end;
end;

function TJvSystemParametersInfo.GetHighContrast: TJvHighContrast;
begin
  if FHighContrast = nil then
    FHighContrast := TJvHighContrast.Create;
  Result := FHighContrast;
end;

function TJvSystemParametersInfo.GetIconMetrics: TJvIconMetrics;
begin
  if FIconMetrics = nil then
    FIconMetrics := TJvIconMetrics.Create;
  Result := FIconMetrics;
end;

function TJvSystemParametersInfo.GetIconSpacing(const Index: Integer): Integer;
const
  cIconSpacing: array [0..1] of DWORD = (SPI_ICONHORIZONTALSPACING, SPI_ICONVERTICALSPACING);
begin
  SystemParametersInfo(cIconSpacing[Index], 0, @Result, 0);
end;

function TJvSystemParametersInfo.GetIconTitleFont: TFont;
var
  ALogFont: TLogFont;
begin
  if FIconTitleFont = nil then
    FIconTitleFont := TFont.Create;
  SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(ALogFont), @ALogFont, 0);
  UpdateFromLogFont(FIconTitleFont, ALogFont);
  Result := FIconTitleFont;
end;

function TJvSystemParametersInfo.GetIntInfo(const Index: Integer): Integer;
begin
  if not SystemParametersInfo(Index, 0, @Result, 0) then
    Result := 0;
end;

function TJvSystemParametersInfo.GetKeyboardLayoutName: string;
var
  Buf: array [0..8] of Char;
begin
  if Windows.GetKeyboardLayoutName(Buf) then
    Result := Buf
  else
    Result := '';
end;

function TJvSystemParametersInfo.GetMinimizedMetrics: TJvMinimizedMetrics;
begin
  if FMinimizedMetrics = nil then
    FMinimizedMetrics := TJvMinimizedMetrics.Create;
  Result := FMinimizedMetrics;
end;

function TJvSystemParametersInfo.GetMouseInfo(const Index: Integer): Integer;
var
  Mouse: array [0..2] of Integer;
begin
  if (Index in [0..2]) and SystemParametersInfo(SPI_GETMOUSE, SizeOf(Mouse), @Mouse, 0) then
    Result := Mouse[Index]
  else
    Result := 0;
end;

function TJvSystemParametersInfo.GetMouseKeys: TJvMouseKeys;
begin
  if FMouseKeys = nil then
    FMouseKeys := TJvMouseKeys.Create;
  Result := FMouseKeys;
end;

function TJvSystemParametersInfo.GetNonClientMetrics: TJvNonClientMetrics;
begin
  if FNonClientMetrics = nil then
    FNonClientMetrics := TJvNonClientMetrics.Create;
  Result := FNonClientMetrics;
end;

function TJvSystemParametersInfo.GetSerialKeys: TJvSerialKeys;
begin
  if FSerialKeys = nil then
    FSerialKeys := TJvSerialKeys.Create;
  Result := FSerialKeys;
end;

function TJvSystemParametersInfo.GetSoundSentry: TJvSoundSentry;
begin
  if FSoundSentry = nil then
    FSoundSentry := TJvSoundSentry.Create;
  Result := FSoundSentry;
end;

function TJvSystemParametersInfo.GetStickyKeys: TJvStickyKeysFlags;
var
  StickyKeys: TStickyKeys;
  Flags: DWORD;
begin
  StickyKeys.cbSize := SizeOf(StickyKeys);
  if SystemParametersInfo(SPI_GETSTICKYKEYS, SizeOf(StickyKeys), @StickyKeys, 0) then
    Flags := StickyKeys.dwFlags
  else
    Flags := 0;
  Result := [];
  if Flags and SKF_AUDIBLEFEEDBACK = SKF_AUDIBLEFEEDBACK then
    Include(Result, skfAudibleFeedback);
  if Flags and SKF_AVAILABLE = SKF_AVAILABLE then
    Include(Result, skfAvailable);
  if Flags and SKF_CONFIRMHOTKEY = SKF_CONFIRMHOTKEY then
    Include(Result, skfConfirmHotkey);
  if Flags and SKF_HOTKEYACTIVE = SKF_HOTKEYACTIVE then
    Include(Result, skfHotkeyActive);
  if Flags and SKF_HOTKEYSOUND = SKF_HOTKEYSOUND then
    Include(Result, skfHotkeySound);
  if Flags and SKF_INDICATOR = SKF_INDICATOR then
    Include(Result, skfIndicator);
  if Flags and SKF_STICKYKEYSON = SKF_STICKYKEYSON then
    Include(Result, skfStickyKeysOn);
  if Flags and SKF_TRISTATE = SKF_TRISTATE then
    Include(Result, skfTriState);
  if Flags and SKF_TWOKEYSOFF = SKF_TWOKEYSOFF then
    Include(Result, skfTwoKeysOff);
  if Flags and SKF_LALTLATCHED = SKF_LALTLATCHED then
    Include(Result, skfLeftAltLatched);
  if Flags and SKF_LCTLLATCHED = SKF_LCTLLATCHED then
    Include(Result, skfLeftCtrlLatched);
  if Flags and SKF_LSHIFTLATCHED = SKF_LSHIFTLATCHED then
    Include(Result, skfLeftShiftLatched);
  if Flags and SKF_RALTLATCHED = SKF_RALTLATCHED then
    Include(Result, skfRightAltLatched);
  if Flags and SKF_RCTLLATCHED = SKF_RCTLLATCHED then
    Include(Result, skfRightCtrlLatched);
  if Flags and SKF_RSHIFTLATCHED = SKF_RSHIFTLATCHED then
    Include(Result, skfRightShiftLatched);
  if Flags and SKF_LWINLATCHED = SKF_LWINLATCHED then
    Include(Result, skfLeftWinLatched);
  if Flags and SKF_RWINLATCHED = SKF_RWINLATCHED then
    Include(Result, skfRightWinLatched);
  if Flags and SKF_LALTLOCKED = SKF_LALTLOCKED then
    Include(Result, skfLeftAltLocked);
  if Flags and SKF_LCTLLOCKED = SKF_LCTLLOCKED then
    Include(Result, skfLeftCtrlLocked);
  if Flags and SKF_LSHIFTLOCKED = SKF_LSHIFTLOCKED then
    Include(Result, skfLeftShiftLocked);
  if Flags and SKF_RALTLOCKED = SKF_RALTLOCKED then
    Include(Result, skfRightAltLocked);
  if Flags and SKF_RCTLLOCKED = SKF_RCTLLOCKED then
    Include(Result, skfRightCtrlLocked);
  if Flags and SKF_RSHIFTLOCKED = SKF_RSHIFTLOCKED then
    Include(Result, skfRightShiftLocked);
  if Flags and SKF_LWINLOCKED = SKF_LWINLOCKED then
    Include(Result, skfLeftWinLocked);
  if Flags and SKF_RWINLOCKED = SKF_RWINLOCKED then
    Include(Result, skfRightWinLocked);
end;

function TJvSystemParametersInfo.GetToggleKeys: TJvToggleKeysFlags;
var
  ToggleKeys: TToggleKeys;
  Flags: DWORD;
begin
  ToggleKeys.cbSize := SizeOf(ToggleKeys);
  if SystemParametersInfo(SPI_GETTOGGLEKEYS, SizeOf(ToggleKeys), @ToggleKeys, 0) then
    Flags := ToggleKeys.dwFlags
  else
    Flags := 0;
  Result := [];
  if Flags and TKF_AVAILABLE = TKF_AVAILABLE then
    Include(Result, tkfAvailable);
  if Flags and TKF_CONFIRMHOTKEY = TKF_CONFIRMHOTKEY then
    Include(Result, tkfConfirmHotkey);
  if Flags and TKF_HOTKEYACTIVE = TKF_HOTKEYACTIVE then
    Include(Result, tkfHotkeyActive);
  if Flags and TKF_HOTKEYSOUND = TKF_HOTKEYSOUND then
    Include(Result, tkfHotkeySound);
  if Flags and TKF_TOGGLEKEYSON = TKF_TOGGLEKEYSON then
    Include(Result, tkfToggleKeysOn);
end;

function TJvSystemParametersInfo.GetWorkArea: TJvRect;
var
  Value: TRect;
begin
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Value, 0) then
    Value := Rect(0, 0, 0, 0);

  FWorkArea.Top := Value.Top;
  FWorkArea.Left := Value.Left;
  FWorkArea.Bottom := Value.Bottom;
  FWorkArea.Right := Value.Right;
  Result := FWorkArea;
end;

procedure TJvSystemParametersInfo.SetAccessTimeOut(const Value: TJvAccessTimeOut);
begin
  //
end;

procedure TJvSystemParametersInfo.SetAnimationInfo(const Value: Boolean);
var
  Info: ANIMATIONINFO;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Info.cbSize := SizeOf(Info);
    Info.iMinAnimate := Ord(Value);
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetBoolInfo(const Index: Integer;
  const Value: Boolean);
begin
  if not IsDesigning and not ReadOnly and (Index <> SPI_GETSCREENSAVERRUNNING) then
  begin
    if Index >= SPI_GETACTIVEWINDOWTRACKING then // new values use new style
      SystemParametersInfo(MapToSet(Index), 0, @Value, DEFAULT_SPIF_SENDCHANGE)
    else
      SystemParametersInfo(MapToSet(Index), Ord(Value), nil, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetDeskWallpaper(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    SystemParametersInfo(SPI_SETDESKWALLPAPER, Length(Value), Pointer(Value), 0)
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetFilterKeys(const Value: TJvFilterKeys);
begin
  //
end;

procedure TJvSystemParametersInfo.SetFontSmoothingType(const Value: TJvFontSmoothingType);
var
  Smoothing: Integer;
begin
  if not IsDesigning and not ReadOnly then
  begin
    case Value of
      fstClearType:
        Smoothing := FE_FONTSMOOTHINGCLEARTYPE;
      fstDocking:
        Smoothing := FE_FONTSMOOTHINGDOCKING;
    else
      Smoothing := FE_FONTSMOOTHINGSTANDARD;
    end;
    SystemParametersInfo(SPI_SETFONTSMOOTHINGTYPE, Smoothing, nil, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetHighContrast(const Value: TJvHighContrast);
begin
  //
end;

procedure TJvSystemParametersInfo.SetIconMetrics(const Value: TJvIconMetrics);
begin
  //
end;

procedure TJvSystemParametersInfo.SetIconSpacing(const Index, Value: Integer);
const
  cIconSpacing: array [0..1] of DWORD =
    (SPI_ICONHORIZONTALSPACING, SPI_ICONVERTICALSPACING);
begin
  if not IsDesigning and not ReadOnly then
    SystemParametersInfo(cIconSpacing[Index], Value, nil, 0)
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetIconTitleFont(const Value: TFont);
var
  ALogFont: TLogFont;
begin
  if not IsDesigning and not ReadOnly then
  begin
    UpdateToLogFont(Value, ALogFont);
    SystemParametersInfo(SPI_SETICONTITLELOGFONT, SizeOf(ALogFont), @ALogFont, 0);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetIntInfo(const Index, Value: Integer);
begin
  if not IsDesigning and not ReadOnly and (Index <> SPI_GETSCREENSAVERRUNNING) then
  begin
    if Index >= SPI_GETACTIVEWINDOWTRACKING then // new values use new style
      SystemParametersInfo(MapToSet(Index), 0, @Value, DEFAULT_SPIF_SENDCHANGE)
    else
      SystemParametersInfo(MapToSet(Index), Value, nil, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetKeyboardLayoutName(const Value: string);
begin
  if not IsDesigning and not ReadOnly then
    LoadKeyboardLayout(PChar(Value), KLF_ACTIVATE)
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetMinimizedMetrics(const Value: TJvMinimizedMetrics);
begin
  //
end;

procedure TJvSystemParametersInfo.SetMouseInfo(const Index, Value: Integer);
var
  Mouse: array [0..2] of Integer;
begin
  if not IsDesigning and not ReadOnly then
  begin
    if (Index in [0..2]) and SystemParametersInfo(SPI_GETMOUSE, SizeOf(Mouse), @Mouse, 0) then
    begin
      Mouse[Index] := Value;
      SystemParametersInfo(SPI_SETMOUSE, SizeOf(Mouse), @Mouse, DEFAULT_SPIF_SENDCHANGE);
    end;
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetMouseKeys(const Value: TJvMouseKeys);
begin
  //
end;

procedure TJvSystemParametersInfo.SetNonClientMetrics(const Value: TJvNonClientMetrics);
begin
  //
end;

procedure TJvSystemParametersInfo.SetSerialKeys(const Value: TJvSerialKeys);
begin
  //
end;

procedure TJvSystemParametersInfo.SetSoundSentry(const Value: TJvSoundSentry);
begin
  //
end;

procedure TJvSystemParametersInfo.SetStickyKeys(const Value: TJvStickyKeysFlags);
var
  StickyKeys: TStickyKeys;
  Flags: DWORD;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Flags := 0;
    if skfAudibleFeedback in Value then
      Flags := Flags or SKF_AUDIBLEFEEDBACK;
    if skfAvailable in Value then
      Flags := Flags or SKF_AVAILABLE;
    if skfConfirmHotkey in Value then
      Flags := Flags or SKF_CONFIRMHOTKEY;
    if skfHotkeyActive in Value then
      Flags := Flags or SKF_HOTKEYACTIVE;
    if skfHotkeySound in Value then
      Flags := Flags or SKF_HOTKEYSOUND;
    if skfIndicator in Value then
      Flags := Flags or SKF_INDICATOR;
    if skfStickyKeysOn in Value then
      Flags := Flags or SKF_STICKYKEYSON;
    if skfTriState in Value then
      Flags := Flags or SKF_TRISTATE;
    if skfTwoKeysOff in Value then
      Flags := Flags or SKF_TWOKEYSOFF;
    if skfAudibleFeedback in Value then
      Flags := Flags or SKF_AUDIBLEFEEDBACK;
    if skfAvailable in Value then
      Flags := Flags or SKF_AVAILABLE;
    if skfConfirmHotkey in Value then
      Flags := Flags or SKF_CONFIRMHOTKEY;
    if skfHotkeyActive in Value then
      Flags := Flags or SKF_HOTKEYACTIVE;
    if skfHotkeySound in Value then
      Flags := Flags or SKF_HOTKEYSOUND;
    if skfIndicator in Value then
      Flags := Flags or SKF_INDICATOR;
    if skfStickyKeysOn in Value then
      Flags := Flags or SKF_STICKYKEYSON;
    if skfTriState in Value then
      Flags := Flags or SKF_TRISTATE;
    if skfTwoKeysOff in Value then
      Flags := Flags or SKF_TWOKEYSOFF;
    if skfLeftAltLatched in Value then
      Flags := Flags or SKF_LALTLATCHED;
    if skfLeftCtrlLatched in Value then
      Flags := Flags or SKF_LCTLLATCHED;
    if skfLeftShiftLatched in Value then
      Flags := Flags or SKF_LSHIFTLATCHED;
    if skfRightAltLatched in Value then
      Flags := Flags or SKF_RALTLATCHED;
    if skfRightCtrlLatched in Value then
      Flags := Flags or SKF_RCTLLATCHED;
    if skfRightShiftLatched in Value then
      Flags := Flags or SKF_RSHIFTLATCHED;
    if skfLeftWinLatched in Value then
      Flags := Flags or SKF_LWINLATCHED;
    if skfRightWinLatched in Value then
      Flags := Flags or SKF_RWINLATCHED;
    if skfLeftAltLocked in Value then
      Flags := Flags or SKF_LALTLOCKED;
    if skfLeftCtrlLocked in Value then
      Flags := Flags or SKF_LCTLLOCKED;
    if skfLeftShiftLocked in Value then
      Flags := Flags or SKF_LSHIFTLOCKED;
    if skfRightAltLocked in Value then
      Flags := Flags or SKF_RALTLOCKED;
    if skfRightCtrlLocked in Value then
      Flags := Flags or SKF_RCTLLOCKED;
    if skfRightShiftLocked in Value then
      Flags := Flags or SKF_RSHIFTLOCKED;
    if skfLeftWinLocked in Value then
      Flags := Flags or SKF_LWINLOCKED;
    if skfRightWinLocked in Value then
      Flags := Flags or SKF_RWINLOCKED;
    StickyKeys.cbSize := SizeOf(StickyKeys);
    StickyKeys.dwFlags := Flags;
    SystemParametersInfo(SPI_SETSTICKYKEYS, SizeOf(StickyKeys), @StickyKeys, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetToggleKeys(const Value: TJvToggleKeysFlags);
var
  ToggleKeys: TToggleKeys;
  Flags: DWORD;
begin
  if not IsDesigning and not ReadOnly then
  begin
    Flags := 0;
    if tkfAvailable in Value then
      Flags := Flags or TKF_AVAILABLE;
    if tkfConfirmHotkey in Value then
      Flags := Flags or TKF_CONFIRMHOTKEY;
    if tkfHotkeyActive in Value then
      Flags := Flags or TKF_HOTKEYACTIVE;
    if tkfHotkeySound in Value then
      Flags := Flags or TKF_HOTKEYSOUND;
    if tkfToggleKeysOn in Value then
      Flags := Flags or TKF_TOGGLEKEYSON;
    ToggleKeys.cbSize := SizeOf(ToggleKeys);
    ToggleKeys.dwFlags := Flags;
    SystemParametersInfo(SPI_SETTOGGLEKEYS, SizeOf(ToggleKeys), @ToggleKeys, DEFAULT_SPIF_SENDCHANGE);
  end
  else
    RaiseReadOnly;
end;

procedure TJvSystemParametersInfo.SetWorkArea(const Value: TJvRect);
begin
  RaiseReadOnly;
end;

procedure TJvSystemColorsInfo.SetColor(Index: Integer; Value: TColor);
begin
  if not IsDesigning and not ReadOnly then
    SetSysColors(1, Index, Value)
  else
    RaiseReadOnly;
end;

function TJvSystemColorsInfo.GetColor(Index: Integer): TColor;
begin
  Result := GetSysColor(Index);
end;

//=== { TJvFileInfo } ========================================================

constructor TJvFileInfo.Create;
begin
  inherited Create;
  FIcon := TIcon.Create;
  FModifiers := [imNormal];
  GetLargeImages;
  GetSmallImages;
end;

destructor TJvFileInfo.Destroy;
begin
  FLargeImages.Free;
  FSmallImages.Free;
  FIcon.Free;
  inherited Destroy;
end;

function TJvFileInfo.GetLargeImages: TImageList;
var
  SysIL: THandle;
  sfi: TSHFileInfo;
begin
  if not Assigned(FLargeImages) then
    FLargeImages := TImageList.Create(nil);
  SysIL := GetFileInfo('', 0, sfi, SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysIL <> 0 then
    FLargeImages.Handle := SysIL;
  FLargeImages.ShareImages := True;
  Result := FLargeImages;
end;

function TJvFileInfo.GetSmallImages: TImageList;
var
  SysIL: THandle;
  sfi: TSHFileInfo;
begin
  if not Assigned(FSmallImages) then
    FSmallImages := TImageList.Create(nil);
  SysIL := GetFileInfo('', 0, sfi, SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then
    FSmallImages.Handle := SysIL;
  FSmallImages.ShareImages := True;
  Result := FSmallImages;
end;

procedure TJvFileInfo.SetModifiers(Value: TJvIconModifiers);
begin
  if FModifiers <> Value then
  begin
    FModifiers := Value;
    Include(FModifiers, imNormal); // imNormal can never be removed (equals 0)
    GetIconHandle;
  end;
end;

procedure TJvFileInfo.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  GetIconHandle;
end;

procedure TJvFileInfo.SetExeDummy(const Value: TJvExeType);
begin
  RaiseReadOnly;
end;

procedure TJvFileInfo.SetIconDummy(const Value: TIcon);
begin
  RaiseReadOnly;
end;

procedure TJvFileInfo.SetIntDummy(const Value: Integer);
begin
  RaiseReadOnly;
end;

procedure TJvFileInfo.SetStrDummy(const Value: string);
begin
  RaiseReadOnly;
end;

{ returns index of icon for filename in the systemlist }

function TJvFileInfo.GetIconIndex: Integer;
var
  sfi: TSHFileInfo;
begin
  GetFileInfo(FFileName, 0, sfi, SHGFI_SYSICONINDEX);
  Result := sfi.iIcon;
end;

function TJvFileInfo.GetDisplayName: string;
var
  sfi: TSHFileInfo;
begin
  GetFileInfo(FFileName, 0, sfi, SHGFI_DISPLAYNAME);
  Result := sfi.szDisplayName;
end;

function TJvFileInfo.GetExeType: TJvExeType;
var
  sfi: TSHFileInfo;
  Res: Integer;
begin
  Result := etNone;

  Res := GetFileInfo(FFileName, 0, sfi, SHGFI_EXETYPE);
  if Res = 0 then
    Exit;
  case Lo(Res) of
    77:
      Result := etMSDos;
    78:
      Result := etWin16;
    80:
      Result := etWin32;
  else
    Result := etConsole; { ? }
  end;
end;

function TJvFileInfo.GetAttributes: Integer;
// var    sfi: TSHFileInfo;
begin
{ this doesn't work, use "old" method instead }
{
  GetFileInfo(FFileName, 0, sfi, SHGFI_ATTRIBUTES);
  Result := sfi.dwAttributes;}
  Result := GetFileAttributes(PChar(FFileName));
end;

function TJvFileInfo.GetAttrString: string;
var
  I: Integer;
begin
  I := GetAttributes;
  Result := '';
  if (I and FILE_ATTRIBUTE_NORMAL) <> 0 then
    Exit; { no attributes }
  if (I and FILE_ATTRIBUTE_ARCHIVE) <> 0 then
    Result := Result + 'A';
  if (I and FILE_ATTRIBUTE_COMPRESSED) <> 0 then
    Result := Result + 'C';
  if (I and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    Result := Result + 'D';
  if (I and FILE_ATTRIBUTE_HIDDEN) <> 0 then
    Result := Result + 'H';
  if (I and FILE_ATTRIBUTE_READONLY) <> 0 then
    Result := Result + 'R';
  if (I and FILE_ATTRIBUTE_SYSTEM) <> 0 then
    Result := Result + 'S';
end;

function StrTrimAll(const S: string; const Chars: TSysCharSet): string;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not (S[I] in Chars) then
      Result := Result + S[I];
end;

function AddDot(S: string): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> '.') then
    Result := '.' + Result;
end;

function ExpandEnvVar(const Value: string): string;
var
  Dest: array [0..MAX_PATH] of Char;
begin
  ExpandEnvironmentStrings(PChar(Value), Dest, MAX_PATH - 1);
  Result := Dest;
end;

function GetAdvancedIconLocation(const FileName: string; var iIcon: Integer): string;
var
  Reg: TRegistry;
  Ext, sPath, Tmp: string;
  I: Integer;
  sfi: TSHFileInfo;
begin
  // first try the easy way:
  SHGetFileInfo(PChar(FileName), 0, sfi, SizeOf(sfi), SHGFI_ICON or SHGFI_ICONLOCATION);
  Result := sfi.szDisplayName;
  if Result <> '' then
  begin
    iIcon := sfi.iIcon;
    Exit;
  end;

  if Pos('.', FileName) > 0 then
    Ext := ExtractFileExt(StrTrimAll(FileName, ['"', '''']))
  else
    Ext := AddDot(FileName);

  if Length(Ext) = 0 then
    Exit;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    // is the key present ?
    if Reg.OpenKey(Ext, False) then
      // get ID to associated program:
      Result := Reg.ReadString('');
    if Reg.OpenKey('\' + Result + '\DefaultIcon', False) then
      Result := Reg.ReadString(''); // path (and possibly index) to icon location
    if Length(Result) > 0 then
    begin
      if Pos('%1', Result) > 0 then
        Result := FileName; // instance specific icon
      I := Pos(',', Result);
      sPath := '';
      if I > 0 then
      begin
        sPath := Copy(Result, I + 1, MaxInt);
        Result := Copy(Result, 1, I - 1);
      end;
      Tmp := '';
      for I := 1 to Length(sPath) do
        if not (sPath[I] in ['-', '0'..'9']) then
          Continue
        else
          Tmp := Tmp + sPath[I];
      iIcon := Abs(StrToIntDef(Tmp, 0)); // convert to positive index
    end
  finally
    Reg.Free;
  end;
  Result := ExpandEnvVar(Result); // replace any environment variables in path (like %systemroot%)
end;

function TJvFileInfo.GetIconLocation: string;
var
  sfi: TSHFileInfo;
  iIcon: Integer;
begin
  { this doesn't seem to work on files, only on directories (always returns an empty string)... }
  GetFileInfo(FFileName, 0, sfi, SHGFI_ICONLOCATION);
  Result := sfi.szDisplayName;
  if Result = '' then
    Result := StrTrimAll(GetAdvancedIconLocation(FFileName, iIcon), ['"']);
end;

function TJvFileInfo.GetTypeString: string;
var
  sfi: TSHFileInfo;
begin
  GetFileInfo(FFileName, 0, sfi, SHGFI_TYPENAME);
  Result := sfi.szTypeName;
  if Result = '' then
    Result := AnsiUpperCase(Copy(ExtractFileExt(FFileName), 2, MaxInt)) + ' file';
end;

function TJvFileInfo.GetIconHandle: THandle;
const
  Modifier: array [TJvIconModifier] of Integer =
    (0, SHGFI_LINKOVERLAY, SHGFI_SELECTED, SHGFI_OPENICON, SHGFI_SHELLICONSIZE, SHGFI_SMALLICON);
var
  sfi: TSHFileInfo;
  Flags: Integer;
  I: TJvIconModifier;
begin
  Flags := 0;
  for I := Low(TJvIconModifier) to High(TJvIconModifier) do
    if I in Modifiers then
      Flags := Flags or Modifier[I];
  GetFileInfo(FFileName, 0, sfi, SHGFI_SYSICONINDEX or SHGFI_ICON or Flags);
  Result := sfi.hIcon;
  FIcon.Handle := sfi.hIcon;
end;

function TJvFileInfo.GetFileInfo(const FileName: string; Attributes: Cardinal; out Info: ShFileInfo; Flags: Cardinal):
  Cardinal;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := SHGetFileInfo(PChar(FileName), Attributes, Info, SizeOf(Info), Flags);
end;

//=== { TJvComputerInfoEx } ==================================================

constructor TJvComputerInfoEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsDesigning := csDesigning in ComponentState;
  FReadOnly := True;
  if not IsDesigning then
    FDeviceHandle := AllocateHWndEx(WndProc);
end;

destructor TJvComputerInfoEx.Destroy;
begin
  FAPMInfo.Free;
  FBIOSInfo.Free;
  FColors.Free;
  FCPUInfo.Free;
  FIdentification.Free;
  FKeyInfo.Free;
  FMemoryInfo.Free;
  FMiscInfo.Free;
  FOSVersionInfo.Free;
  FScreenInfo.Free;
  FSystemFolders.Free;
  FMetrics.Free;
  FSystem.Free;
  FFileInfo.Free;
  if not IsDesigning and (FDeviceHandle <> 0) then
    DeallocateHWndEx(FDeviceHandle);
  inherited Destroy;
end;

procedure TJvComputerInfoEx.DoDriveChange(Drive: Char; Removed: Boolean);
begin
  if Removed then
  begin
    if Assigned(FOnDeviceRemoved) then
      FOnDeviceRemoved(Self, Drive);
  end
  else
  if Assigned(FOnDeviceAdded) then
    FOnDeviceAdded(Self, Drive);
end;

function TJvComputerInfoEx.FirstDrive(AMask: Integer): Char;
var
  Drive: Shortint;
begin
  Drive := Ord('A');
  while (AMask and 1) = 0 do
  begin
    AMask := AMask shr 1;
    Inc(Drive);
  end;
  Result := Char(Drive);
end;

function TJvComputerInfoEx.GetAPMInfo: TJvAPMInfo;
begin
  if FAPMInfo = nil then
    FAPMInfo := TJvAPMInfo.Create;
  Result := FAPMInfo;
end;

function TJvComputerInfoEx.GetBIOSInfo: TJvBIOSInfo;
begin
  if FBIOSInfo = nil then
    FBIOSInfo := TJvBIOSInfo.Create;
  Result := FBIOSInfo;
end;

function TJvComputerInfoEx.GetColors: TJvSystemColorsInfo;
begin
  if FColors = nil then
  begin
    FColors := TJvSystemColorsInfo.Create;
    FColors.ReadOnly := ReadOnly;
  end;
  Result := FColors;
end;

function TJvComputerInfoEx.GetCPUInfo: TJvCPUInfo;
begin
  if FCPUInfo = nil then
    FCPUInfo := TJvCPUInfo.Create;
  Result := FCPUInfo;
end;

function TJvComputerInfoEx.GetIdentification: TJvIdentification;
begin
  if FIdentification = nil then
  begin
    FIdentification := TJvIdentification.Create;
    FIdentification.ReadOnly := ReadOnly;
  end;
  Result := FIdentification;
end;

function TJvComputerInfoEx.GetKeyInfo: TJvKeyInfo;
begin
  if FKeyInfo = nil then
  begin
    FKeyInfo := TJvKeyInfo.Create;
    FKeyInfo.ReadOnly := ReadOnly;
  end;
  Result := FKeyInfo;
end;

function TJvComputerInfoEx.GetMemoryInfo: TJvMemInfo;
begin
  if FMemoryInfo = nil then
    FMemoryInfo := TJvMemInfo.Create;
  Result := FMemoryInfo;
end;

function TJvComputerInfoEx.GetMetrics: TJvMetricsInfo;
begin
  if FMetrics = nil then
    FMetrics := TJvMetricsInfo.Create;
  Result := FMetrics;
end;

function TJvComputerInfoEx.GetMiscInfo: TJvMiscInfo;
begin
  if FMiscInfo = nil then
  begin
    FMiscInfo := TJvMiscInfo.Create;
    FMiscInfo.ReadOnly := ReadOnly;
  end;
  Result := FMiscInfo;
end;

function TJvComputerInfoEx.GetOSVersionInfo: TJvOSVersionInfo;
begin
  if FOSVersionInfo = nil then
    FOSVersionInfo := TJvOSVersionInfo.Create;
  Result := FOSVersionInfo;
end;

function TJvComputerInfoEx.GetScreenInfo: TJvScreenInfo;
begin
  if FScreenInfo = nil then
  begin
    FScreenInfo := TJvScreenInfo.Create;
    FScreenInfo.ReadOnly := ReadOnly;
  end;
  Result := FScreenInfo;
end;

function TJvComputerInfoEx.GetSystem: TJvSystemParametersInfo;
begin
  if FSystem = nil then
  begin
    FSystem := TJvSystemParametersInfo.Create;
    FSystem.ReadOnly := ReadOnly;
  end;
  Result := FSystem;
end;

function TJvComputerInfoEx.GetSystemFolders: TJvSystemFolders;
begin
  if FSystemFolders = nil then
    FSystemFolders := TJvSystemFolders.Create;
  Result := FSystemFolders;
end;

function TJvComputerInfoEx.GetFileInfo: TJvFileInfo;
begin
  if FFileInfo = nil then
    FFileInfo := TJvFileInfo.Create;
  Result := FFileInfo;
end;

procedure TJvComputerInfoEx.SetAPMInfo(const Value: TJvAPMInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetBIOSInfo(const Value: TJvBIOSInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetColors(const Value: TJvSystemColorsInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetCPUInfo(const Value: TJvCPUInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetIdentification(const Value: TJvIdentification);
begin
  //
end;

procedure TJvComputerInfoEx.SetKeyInfo(const Value: TJvKeyInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetMemoryInfo(const Value: TJvMemInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetMetrics(const Value: TJvMetricsInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetMiscInfo(const Value: TJvMiscInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetOSVersionInfo(const Value: TJvOSVersionInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetScreenInfo(const Value: TJvScreenInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetSystem(const Value: TJvSystemParametersInfo);
begin
  //
end;

procedure TJvComputerInfoEx.SetSystemFolders(const Value: TJvSystemFolders);
begin
  //
end;

procedure TJvComputerInfoEx.SetFileInfo(const Value: TJvFileInfo);
begin
  //
end;

procedure TJvComputerInfoEx.DoSettingChange(Flag: Integer; Section: string);
begin
  if Assigned(FOnSettingChange) then
    FOnSettingChange(Self, Flag, Section);
end;

procedure TJvComputerInfoEx.WMDeviceChange(var Msg: TWMDeviceChange);
var
  lpdb: PDevBroadcastHdr;
  lpdbv: PDevBroadcastVolume;
begin
  lpdb := PDevBroadcastHdr(Msg.dwData);
  case Msg.Event of
    DBT_DEVICEARRIVAL:
      if lpdb^.dbch_devicetype = DBT_DEVTYP_VOLUME then
      begin
        lpdbv := PDevBroadcastVolume(Msg.dwData);
        if (lpdbv^.dbcv_flags and DBTF_MEDIA) = 1 then
          DoDriveChange(FirstDrive(lpdbv^.dbcv_unitmask), False);
      end;
    DBT_DEVICEREMOVECOMPLETE:
      if lpdb^.dbch_devicetype = DBT_DEVTYP_VOLUME then
      begin
        lpdbv := PDevBroadcastVolume(Msg.dwData);
        if (lpdbv^.dbcv_flags and DBTF_MEDIA) = 1 then
          DoDriveChange(FirstDrive(lpdbv^.dbcv_unitmask), True);
      end;
      // (rom) one of the rare occasions where ; before else is correct
  else
    with Msg do
      DoDeviceChange(Event, dwData);
  end
end;

procedure TJvComputerInfoEx.DoCompacting(Ratio: Integer);
begin
  if Assigned(FOnCompacting) then
    FOnCompacting(Self, Ratio);
end;

procedure TJvComputerInfoEx.DoPowerBroadcast(Event, Data: Integer);
begin
  if Assigned(FOnPowerBroadcast) then
    FOnPowerBroadcast(Self, Event, Data);
end;

procedure TJvComputerInfoEx.DoUserChanged;
begin
  if Assigned(FOnUserChanged) then
    FOnUserChanged(Self);
end;

procedure TJvComputerInfoEx.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    case Msg of
      WM_DISPLAYCHANGE:
        WMDisplayChange(TWMDisplayChange(Message));
      WM_DEVMODECHANGE:
        DoDevModeChange(TWMDevModeChange(Message).Device);
      WM_SETTINGCHANGE:
        with TWMSettingChange(Message) do
          DoSettingChange(Flag, Section);
      WM_DEVICECHANGE:
        WMDeviceChange(TWMDeviceChange(Message));
      WM_COMPACTING:
        DoCompacting(wParam);
      WM_POWER, WM_POWERBROADCAST:
        DoPowerBroadcast(wParam, lParam);
      WM_USERCHANGED:
        DoUserChanged;
      WM_TIMECHANGE:
        DoTimeChange;
      WM_FONTCHANGE:
        DoFontChange;
      WM_SYSCOLORCHANGE:
        DoSysColorChange;
      WM_SPOOLERSTATUS:
        with TWMSpoolerStatus(Message) do
          DoSpoolerStatus(JobStatus, JobsLeft);
      WM_PALETTEISCHANGING:
        with TWmPaletteIsChanging(Message) do
          DoPaletteChanging(Realize);
      WM_PALETTECHANGED:
        with TWmPaletteChanged(Message) do
          DoPaletteChanged(PalChg);
    end;
    Result := DefWindowProc(FDeviceHandle, Msg, wParam, lParam);
  end;
end;

procedure TJvComputerInfoEx.DoDevModeChange(const Device: PChar);
begin
  if Assigned(FOnDeviceModeChange) then
    FOnDeviceModeChange(Self, Device);
end;

procedure TJvComputerInfoEx.WMDisplayChange(var Msg: TWMDisplayChange);
begin
  if Assigned(FOnDisplayChange) then
    FOnDisplayChange(Self, Msg.BitsPerPixel, Msg.Width, Msg.Height);
end;

procedure TJvComputerInfoEx.DoDeviceChange(Event: UINT; dwData: Pointer);
begin
  if Assigned(FOnDeviceChange) then
    FOnDeviceChange(Self, Event, dwData);
end;

procedure TJvComputerInfoEx.DoTimeChange;
begin
  if Assigned(FOnTimeChange) then
    FOnTimeChange(Self);
end;

procedure TJvComputerInfoEx.DoFontChange;
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

procedure TJvComputerInfoEx.DoSysColorChange;
begin
  if Assigned(FOnSysColorChange) then
    FOnSysColorChange(Self);
end;

procedure TJvComputerInfoEx.DoSpoolerStatus(JobStatus, JobsLeft: Integer);
begin
  if Assigned(FOnSpoolerStatusChange) then
    FOnSpoolerStatusChange(Self, JobStatus, JobsLeft);
end;

procedure TJvComputerInfoEx.DoPaletteChanging(Wnd: HWND);
begin
  if Assigned(FOnPaletteChanging) then
    FOnPaletteChanging(Self, Wnd);
end;

procedure TJvComputerInfoEx.DoPaletteChanged(Wnd: HWND);
begin
  if Assigned(FOnPaletteChanged) then
    FOnPaletteChanged(Self, Wnd);
end;

procedure TJvComputerInfoEx.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if FColors <> nil then
      FColors.ReadOnly := FReadOnly;
    if FIdentification <> nil then
      FIdentification.ReadOnly := FReadOnly;
    if FKeyInfo <> nil then
      FKeyInfo.ReadOnly := FReadOnly;
    if FMiscInfo <> nil then
      FMiscInfo.ReadOnly := FReadOnly;
    if FScreenInfo <> nil then
      FScreenInfo.ReadOnly := FReadOnly;
    if FSystem <> nil then
      FSystem.ReadOnly := FReadOnly;
  end;
end;

function TJvComputerInfoEx.ResetSystemCursors: Boolean;
begin
  Result := SystemParametersInfo(SPI_SETCURSORS, 0, nil, SPIF_SENDCHANGE);
end;

function TJvComputerInfoEx.ResetSystemIcons: Boolean;
const
  HKCU_WINDOWMETRICS = '\Control Panel\Desktop\WindowMetrics';
  cShellIconSize = 'Shell Icon Size';
var
  DefaultValue: Integer;
begin
  //  Result := SystemParametersInfo(SPI_SETICONS, 0, nil, SPIF_SENDCHANGE);
    // I stole the idea for this from the TortoiseCVS guys (thanks!)
  DefaultValue := StrToIntDef(RegReadStringDef(HKEY_CURRENT_USER, HKCU_WINDOWMETRICS, cShellIconSize, '0'), 0);
  Result := DefaultValue <> 0;
  if Result then
  begin
    RegWriteString(HKEY_CURRENT_USER, HKCU_WINDOWMETRICS, cShellIconSize, IntToStr(Succ(DefaultValue)));
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, 0);
    RegWriteString(HKEY_CURRENT_USER, HKCU_WINDOWMETRICS, cShellIconSize, IntToStr(DefaultValue));
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, 0);
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}


finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadNetLib;

end.

