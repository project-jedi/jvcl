{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ <API> interface unit                                             }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1993-1998 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: dbt.h, released 24 May 1993                }
{ The original Pascal code is: dbt.pas, released 01 Jan 1998       }
{ The initial developer of the Pascal code is Tom Deprez           }
{ (Tom.Deprez@village.uunet.be)                                    }
{                                                                  }
{ Portions created by Tom Deprez are                               }
{ Copyright (C) 1999-2000 Tom Deprez.                              }
{                                                                  }
{ Contributor(s):                                                  }
{     Robert Marquardt : pointed out that TWMDeviceChange          }
{                        is better placed inside dbt.pas           }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit QDBT;

{$WEAKPACKAGEUNIT ON}

interface

(*$HPPEMIT '' *)
(*$HPPEMIT '#include <dbt.h>' *)
(*$HPPEMIT '' *)

uses
  Windows;

{ *
  * BroadcastSpecialMessage constants
  * }

const
  {$EXTERNALSYM WM_DEVICECHANGE}
  WM_DEVICECHANGE = $0219;


{ *
  * Broadcast message and receipient flags.
  *
  * Note that there is a third "flag". If the wParam has:
  *
  *   bit 15 on: lparam is a pointer and bit 14 is meaningfull.
  *   bit 15 off: lparam is just a UNLONG data type.
  *
  *   bit 14 on: lparam is a pointer to an ASCIIZ string.
  *   bit 14 off: lparam is a pointer to a binary struture starting with
  *     a dword describing the length of the structure.
  * }

const
  {$EXTERNALSYM BSF_QUERY}
  BSF_QUERY = $00000001;
  {$EXTERNALSYM BSF_IGNORECURRENTTASK}
  BSF_IGNORECURRENTTASK = $00000002;        { Meaningless for VxDs }
  {$EXTERNALSYM BSF_FLUSHDISK}
  BSF_FLUSHDISK = $00000004;                { Shouldn't be used by VxDs }
  {$EXTERNALSYM BSF_NOHANG}
  BSF_NOHANG = $00000008;
  {$EXTERNALSYM BSF_POSTMESSAGE}
  BSF_POSTMESSAGE = $00000010;
  {$EXTERNALSYM BSF_FORCEIFHUNG}
  BSF_FORCEIFHUNG = $00000020;
  {$EXTERNALSYM BSF_NOTIMEOUTIFNOTHUNG}
  BSF_NOTIMEOUTIFNOTHUNG = $00000040;
  {$EXTERNALSYM BSF_MSGSRV32ISOK}
  BSF_MSGSRV32ISOK = DWORD($80000000);      { Called synchronously from PM API }
  {$EXTERNALSYM BSF_MSGSRV32ISOK_BIT}
  BSF_MSGSRV32ISOK_BIT = 31;                { Called synchronously from PM API }
  {$EXTERNALSYM BSM_ALLCOMPONENTS}
  BSM_ALLCOMPONENTS = $00000000;
  {$EXTERNALSYM BSM_VXDS}
  BSM_VXDS = $00000001;
  {$EXTERNALSYM BSM_NETDRIVER}
  BSM_NETDRIVER = $00000002;
  {$EXTERNALSYM BSM_INSTALLABLEDRIVERS}
  BSM_INSTALLABLEDRIVERS = $00000004;
  {$EXTERNALSYM BSM_APPLICATIONS}
  BSM_APPLICATIONS = $00000008;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_APPYBEGIN
  *   lParam = (not used)
  *
  * 'Appy-time is now available. This message is itself sent
  * at 'Appy-time.
  *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_APPYEND
  *   lParam = (not used)
  *
  * 'Appy-time is no longer available. This message is*NOT* sent
  * at 'Appy-time. (It cannot be, because 'Appy-time is gone.)
  *
  * NOTE! It is possible for DBT_APPYBEGIN and DBT_APPYEND to be sent
  * multiple times during a single Windows session. Each appearance of
  * 'Appy-time is bracketed by these two messages, but 'Appy-time may
  * momentarily become unavailable during otherwise normal Windows
  * processing. The current status of 'Appy-time availability can always
  * be obtained from a call to _SHELL_QueryAppyTimeAvailable.
  * }

const
  {$EXTERNALSYM DBT_APPYBEGIN}
  DBT_APPYBEGIN = $0000;
  {$EXTERNALSYM DBT_APPYEND}
  DBT_APPYEND = $0001;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_DEVNODES_CHANGED
  *   lParam = 0
  *
  * send when configmg finished a process tree batch. Some devnodes
  * may have been added or removed. This is used by ring3 people which
  * need to be refreshed whenever any devnode changed occur (like
  * device manager). People specific to certain devices should use
  * DBT_DEVICE* instead.
  * }

const
  {$EXTERNALSYM DBT_DEVNODES_CHANGED}
  DBT_DEVNODES_CHANGED = $0007;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_QUERYCHANGECONFIG
  *   lParam = 0
  *
  * sent to ask if a config change is allowed
  * }

const
  {$EXTERNALSYM DBT_QUERYCHANGECONFIG}
  DBT_QUERYCHANGECONFIG = $0017;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_CONFIGCHANGED
  *   lParam = 0
  *
  * sent when a config has changed
  * }

const
  {$EXTERNALSYM DBT_CONFIGCHANGED}
  DBT_CONFIGCHANGED = $0018;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_CONFIGCHANGECANCELED
  *   lParam = 0
  *
  * someone cancelled the config change
  * }

const
  {$EXTERNALSYM DBT_CONFIGCHANGECANCELED}
  DBT_CONFIGCHANGECANCELED = $0019;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_MONITORCHANGE
  *   lParam = new resolution to use (LOWORD=x, HIWORD=y)
  * if 0, use the default res for current config
  *
  * this message is sent when the display monitor has changed
  * and the system should change the display mode to match it.
  * }

const
  {$EXTERNALSYM DBT_MONITORCHANGE}
  DBT_MONITORCHANGE = $001B;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_SHELLLOGGEDON
  *   lParam = 0
  *
  * The shell has finished login on: VxD can now do Shell_EXEC.
  * }

const
  {$EXTERNALSYM DBT_SHELLLOGGEDON}
  DBT_SHELLLOGGEDON = $0020;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_CONFIGMGAPI
  *   lParam = CONFIGMG API Packet
  *
  * CONFIGMG ring 3 call.
  * }

const
  {$EXTERNALSYM DBT_CONFIGMGAPI32}
  DBT_CONFIGMGAPI32 = $0022;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_VXDINITCOMPLETE
  *   lParam = 0
  *
  * CONFIGMG ring 3 call.
  * }

const
  {$EXTERNALSYM DBT_VXDINITCOMPLETE}
  DBT_VXDINITCOMPLETE = $0023;


{ *
  * Message = WM_DEVICECHANGE
  * wParam = DBT_VOLLOCK*
  * lParam = pointer to VolLockBroadcast structure described below
  *
  * Messages issued by IFSMGR for volume locking purposes on WM_DEVICECHANGE.
  * All these messages pass a pointer to a struct which has no pointers.
  * }

const
  {$EXTERNALSYM DBT_VOLLOCKQUERYLOCK}
  DBT_VOLLOCKQUERYLOCK = $8041;
  {$EXTERNALSYM DBT_VOLLOCKLOCKTAKEN}
  DBT_VOLLOCKLOCKTAKEN = $8042;
  {$EXTERNALSYM DBT_VOLLOCKLOCKFAILED}
  DBT_VOLLOCKLOCKFAILED = $8043;
  {$EXTERNALSYM DBT_VOLLOCKQUERYUNLOCK}
  DBT_VOLLOCKQUERYUNLOCK = $8044;
  {$EXTERNALSYM DBT_VOLLOCKLOCKRELEASED}
  DBT_VOLLOCKLOCKRELEASED = $8045;
  {$EXTERNALSYM DBT_VOLLOCKUNLOCKFAILED}
  DBT_VOLLOCKUNLOCKFAILED = $8046;


{ *
  * Device broadcast header
  * }

type
  PDevBroadcastHdr = ^TDevBroadcastHdr;
  {$EXTERNALSYM DEV_BROADCAST_HDR}
  DEV_BROADCAST_HDR = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  TDevBroadcastHdr = DEV_BROADCAST_HDR;


{ *
  * Structure for volume lock broadcast
  * }

type
  PVolLockBroadcast = ^TVolLockBroadcast;
  {$EXTERNALSYM VolLockBroadcast}
  VolLockBroadcast = packed record
    vlb_dbh: TDevBroadcastHdr;
    vlb_owner: DWORD;
    vlb_perms: Byte;
    vlb_lockType: Byte;
    vlb_drive: Byte;
    vlb_flags: Byte;
  end;
  TVolLockBroadcast = VolLockBroadcast;

{ *
  * Values for vlb_perms
  * }

const
  {$EXTERNALSYM LOCKP_ALLOW_WRITES}
  LOCKP_ALLOW_WRITES = $01;              { Bit 0 set - allow writes }
  {$EXTERNALSYM LOCKP_FAIL_WRITES}
  LOCKP_FAIL_WRITES = $00;               { Bit 0 clear - fail writes }
  {$EXTERNALSYM LOCKP_FAIL_MEM_MAPPING}
  LOCKP_FAIL_MEM_MAPPING = $02;          { Bit 1 set - fail memory mappings }
  {$EXTERNALSYM LOCKP_ALLOW_MEM_MAPPING}
  LOCKP_ALLOW_MEM_MAPPING = $00;         { Bit 1 clear - allow memory mappings }
  {$EXTERNALSYM LOCKP_USER_MASK}
  LOCKP_USER_MASK = $03;                 { Mask for user lock flags }
  {$EXTERNALSYM LOCKP_LOCK_FOR_FORMAT}
  LOCKP_LOCK_FOR_FORMAT = $04;           { Level 0 lock for format }

{ *
  * Values for vlb_flags
  * }

const
  {$EXTERNALSYM LOCKF_LOGICAL_LOCK}
  LOCKF_LOGICAL_LOCK = $00;              { Bit 0 clear - logical lock }
  {$EXTERNALSYM LOCKF_PHYSICAL_LOCK}
  LOCKF_PHYSICAL_LOCK = $01;             { Bit 0 set - physical lock }

{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_NODISKSPACE
  *   lParam = drive number of drive that is out of disk space (1-based)
  *
  * Message issued by IFS manager when it detects that a drive is run out of
  * free space.
  * }

const
  {$EXTERNALSYM DBT_NO_DISK_SPACE}
  DBT_NO_DISK_SPACE = $0047;


{ *
  * Message = WM_DEVICECHANGE
  *   wParam = DBT_LOW_DISK_SPACE
  *   lParam = drive number of drive that is low on disk space (1-based)
  *
  * Message issued by VFAT when it detects that a drive it has mounted
  * has the remaning free space below a threshold specified by the
  * registry or by a disk space management application.
  * The broadcast is issued by VFAT ONLY when space is either allocated
  * or freed by VFAT.
  * }

const
  {$EXTERNALSYM DBT_LOW_DISK_SPACE}
  DBT_LOW_DISK_SPACE = $0048;

  {$EXTERNALSYM DBT_CONFIGMGPRIVATE}
  DBT_CONFIGMGPRIVATE = $7FFF;


{ *
  * The following messages are for WM_DEVICECHANGE. The immediate list
  * is for the wParam. ALL THESE MESSAGES PASS A POINTER TO A STRUCT
  * STARTING WITH A DWORD SIZE AND HAVING NO POINTER IN THE STRUCT.
  * }

const
  {$EXTERNALSYM DBT_DEVICEARRIVAL}
  DBT_DEVICEARRIVAL = $8000;                   { system detected a new device }
  {$EXTERNALSYM DBT_DEVICEQUERYREMOVE}
  DBT_DEVICEQUERYREMOVE = $8001;               { wants to remove, may fail }
  {$EXTERNALSYM DBT_DEVICEQUERYREMOVEFAILED}
  DBT_DEVICEQUERYREMOVEFAILED = $8002;         { removal aborted }
  {$EXTERNALSYM DBT_DEVICEREMOVEPENDING}
  DBT_DEVICEREMOVEPENDING = $8003;             { about to remove, still avail. }
  {$EXTERNALSYM DBT_DEVICEREMOVECOMPLETE}
  DBT_DEVICEREMOVECOMPLETE = $8004;            { device is gone }
  {$EXTERNALSYM DBT_DEVICETYPESPECIFIC}
  DBT_DEVICETYPESPECIFIC = $8005;              { type specific event }
  {$EXTERNALSYM DBT_CUSTOMEVENT}
  DBT_CUSTOMEVENT = $8006;                     { user-defined event }
  {$EXTERNALSYM DBT_DEVTYP_OEM}
  DBT_DEVTYP_OEM = $00000000;                  { oem-defined device type }
  {$EXTERNALSYM DBT_DEVTYP_DEVNODE}
  DBT_DEVTYP_DEVNODE = $00000001;              { devnode number }
  {$EXTERNALSYM DBT_DEVTYP_VOLUME}
  DBT_DEVTYP_VOLUME = $00000002;               { logical volume }
  {$EXTERNALSYM DBT_DEVTYP_PORT}
  DBT_DEVTYP_PORT = $00000003;                 { serial, parallel }
  {$EXTERNALSYM DBT_DEVTYP_NET}
  DBT_DEVTYP_NET = $00000004;                  { network resource }
  {$EXTERNALSYM DBT_DEVTYP_DEVICEINTERFACE}
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;      { device interface class }
  {$EXTERNALSYM DBT_DEVTYP_HANDLE}
  DBT_DEVTYP_HANDLE = $00000006;               { file system handle }

type
  PDevBroadcastHeader = ^TDevBroadcastHeader;
  // no EXTERNALSYM because this struct is not declared as C type
  DEV_BROADCAST_HEADER = packed record
    dbcd_size: DWORD;
    dbcd_devicetype: DWORD;
    dbcd_reserved: DWORD;
  end;
  TDevBroadcastHeader = DEV_BROADCAST_HEADER;

  PDevBroadcastOem = ^TDevBroadcastOem;
  {$EXTERNALSYM DEV_BROADCAST_OEM}
  DEV_BROADCAST_OEM = packed record
    dbco_size: DWORD;
    dbco_devicetype: DWORD;
    dbco_reserved: DWORD;
    dbco_identifier: DWORD;
    dbco_suppfunc: DWORD;
  end;
  TDevBroadcastOem = DEV_BROADCAST_OEM;

  PDevBroadcastDevNode = ^TDevBroadcastDevNode;
  {$EXTERNALSYM DEV_BROADCAST_DEVNODE}
  DEV_BROADCAST_DEVNODE = packed record
    dbcd_size: DWORD;
    dbcd_devicetype: DWORD;
    dbcd_reserved: DWORD;
    dbcd_devnode: DWORD;
  end;
  TDevBroadcastDevNode = DEV_BROADCAST_DEVNODE;

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

const
  {$EXTERNALSYM DBTF_MEDIA}
  DBTF_MEDIA = $0001;                           { media commings and goings }
  {$EXTERNALSYM DBTF_NET}
  DBTF_NET = $0002;                             { network volume }

type
  PDevBroadCastPortA = ^TDevBroadCastPortA;
  {$EXTERNALSYM DEV_BROADCAST_PORT_A}
  DEV_BROADCAST_PORT_A = packed record
    dbcp_size: DWORD;
    dbcp_devicetype: DWORD;
    dbcp_reserved: DWORD;
    dbcp_name: array[0..0] of AnsiChar;
  end;
  TDevBroadCastPortA = DEV_BROADCAST_PORT_A;
  PDevBroadCastPortW = ^TDevBroadCastPortW;
  {$EXTERNALSYM DEV_BROADCAST_PORT_W}
  DEV_BROADCAST_PORT_W = packed record
    dbcp_size: DWORD;
    dbcp_devicetype: DWORD;
    dbcp_reserved: DWORD;
    dbcp_name: array[0..0] of WideChar;
  end;
  TDevBroadCastPortW = DEV_BROADCAST_PORT_W;
  PDevBroadCastPort = PDevBroadCastPortA;

type
  PDevBroadcastNet = ^TDevBroadcastNet;
  {$EXTERNALSYM DEV_BROADCAST_NET}
  DEV_BROADCAST_NET = packed record
    dbcn_size: DWORD;
    dbcn_devicetype: DWORD;
    dbcn_reserved: DWORD;
    dbcn_resource: DWORD;
    dbcn_flags: DWORD;
  end;
  TDevBroadcastNet = DEV_BROADCAST_NET;

  PDevBroadcastDeviceInterfaceA = ^TDevBroadcastDeviceInterfaceA;
  {$EXTERNALSYM DEV_BROADCAST_DEVICEINTERFACE_A}
  DEV_BROADCAST_DEVICEINTERFACE_A = packed record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: Array[0..0] of AnsiChar;
  end;
  TDevBroadcastDeviceInterfaceA = DEV_BROADCAST_DEVICEINTERFACE_A;
  PDevBroadcastDeviceInterfaceW = ^TDevBroadcastDeviceInterfaceW;
  {$EXTERNALSYM DEV_BROADCAST_DEVICEINTERFACE_W}
  DEV_BROADCAST_DEVICEINTERFACE_W = packed record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: Array[0..0] of WideChar;
  end;
  TDevBroadcastDeviceInterfaceW = DEV_BROADCAST_DEVICEINTERFACE_W;
  PDevBroadcastDeviceInterface = PDevBroadcastDeviceInterfaceA;

  {$EXTERNALSYM DEV_BROADCAST_HANDLE}
  PDevBroadcastHandle = ^TDevBroadcastHandle;
  DEV_BROADCAST_HANDLE = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
    dbch_handle: THandle;            { file handle used in call to RegisterDeviceNotification }
    dbch_hdevnotify: DWORD;          { HDEVNOTIFY returned from RegisterDeviceNotification }

    { The following 3 fields are only valid if wParam is DBT_CUSTOMEVENT. }

    dbch_eventguid: TGUID;
    dbch_nameoffset: DWORD;           { offset (bytes) of variable-length string buffer (-1 if none)}
    dbch_data: Array[0..0] of BYTE;   { variable-sized buffer, potentially containing binary and/or text data }
  end;
  TDevBroadcastHandle = DEV_BROADCAST_HANDLE;

const
  {$EXTERNALSYM DBTF_RESOURCE}
  DBTF_RESOURCE = $00000001;               { network resource }
  {$EXTERNALSYM DBTF_XPORT}
  DBTF_XPORT = $00000002;                  { new transport coming or going }
  {$EXTERNALSYM DBTF_SLOWNET}
  DBTF_SLOWNET = $00000004;                { new incoming transport is slow }
                                           { (dbcn_resource undefined for now) }
  {$EXTERNALSYM DBT_VPOWERDAPI}
  DBT_VPOWERDAPI = $8100;                  { VPOWERD API for Win95 }


{ *
  * User-defined message types all use wParam = 0xFFFF with the
  * lParam a pointer to the structure below.
  *
  * dbud_dbh - DEV_BROADCAST_HEADER must be filled in as usual.
  *
  * dbud_szName contains a case-sensitive ASCIIZ name which names the
  * message. The message name consists of the vendor name, a backslash,
  * then arbitrary user-defined ASCIIZ text. For example:
  *
  * "WidgetWare\QueryScannerShutdown"
  * "WidgetWare\Video Q39S\AdapterReady"
  *
  * After the ASCIIZ name, arbitrary information may be provided.
  * Make sure that dbud_dbh.dbch_size is big enough to encompass
  * all the data. And remember that nothing in the structure may
  * contain pointers.
  * }

const
  {$EXTERNALSYM DBT_USERDEFINED}
  DBT_USERDEFINED = $FFF;

type
  PDevBroadcastUserdefined = ^TDevBroadcastUserdefined;
  // no EXTERNALSYM because this struct is not declared as C type
  DEV_BROADCAST_USERDEFINED = packed record
    dbud_dbh: TDevBroadcastHdr;
    dbud_szName: Array[0..0] of Char; { ASCIIZ name }
   {dbud_rgbUserDefined[]: Byte; // User-defined contents }
  end;
  TDevBroadcastUserdefined = DEV_BROADCAST_USERDEFINED;

{ added own message type for WM_DEVICECHANGE }

type
  TWMDeviceChange = record
   Msg:    Cardinal;
   Event:  UINT;
   dwData: Pointer;
   Result: LongInt;
  end;

implementation

end.

