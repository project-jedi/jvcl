{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Config Manager API interface unit                          }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) Microsoft Corporation.  All rights reserved.       }
{                                                                  }
{ The original file is: cfg.h, released August 2001.               }
{ The original Pascal code is: Cfg.pas, released 5 Nov 2004.       }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (C) 2004 Robert Marquardt.                             }
{                                                                  }
{ Contributor(s):                                                  }
{                                                                  }
{ Obtained through:                                                }
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

unit Cfg;

{$WEAKPACKAGEUNIT ON}

interface

{$WEAKPACKAGEUNIT ON}

uses
  Windows;

{$HPPEMIT '#include "cfg.h"'}

//
// The following definitions are also used by kernel mode code to
// set up the registry.
//
//
// VetoType used in
//      CM_Disable_DevNode
//      CM_Uninstall_DevNode
//      CM_Query_And_Remove_SubTree
//
const
  PNP_VetoTypeUnknown          = 0;   // Name is unspecified
  {$EXTERNALSYM PNP_VetoTypeUnknown}
  PNP_VetoLegacyDevice         = 1;   // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoLegacyDevice}
  PNP_VetoPendingClose         = 2;   // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoPendingClose}
  PNP_VetoWindowsApp           = 3;   // Name is a Module
  {$EXTERNALSYM PNP_VetoWindowsApp}
  PNP_VetoWindowsService       = 4;   // Name is a Service
  {$EXTERNALSYM PNP_VetoWindowsService}
  PNP_VetoOutstandingOpen      = 5;   // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoOutstandingOpen}
  PNP_VetoDevice               = 6;   // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoDevice}
  PNP_VetoDriver               = 7;   // Name is a Driver Service Name
  {$EXTERNALSYM PNP_VetoDriver}
  PNP_VetoIllegalDeviceRequest = 8;   // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoIllegalDeviceRequest}
  PNP_VetoInsufficientPower    = 9;   // Name is unspecified
  {$EXTERNALSYM PNP_VetoInsufficientPower}
  PNP_VetoNonDisableable       = 10;  // Name is an Instance Path
  {$EXTERNALSYM PNP_VetoNonDisableable}
  PNP_VetoLegacyDriver         = 11;  // Name is a Service
  {$EXTERNALSYM PNP_VetoLegacyDriver}
  PNP_VetoInsufficientRights   = 12;  // Name is unspecified
  {$EXTERNALSYM PNP_VetoInsufficientRights}
type
  PPNP_VETO_TYPE = ^PNP_VETO_TYPE;
  {$EXTERNALSYM PPNP_VETO_TYPE}
  PNP_VETO_TYPE = DWORD;
  {$EXTERNALSYM PNP_VETO_TYPE}

const
  //
  // DevInst problem values, returned by call to CM_Get_DevInst_Status
  //
  CM_PROB_NOT_CONFIGURED             = $00000001;   // no config for device
  {$EXTERNALSYM CM_PROB_NOT_CONFIGURED}
  CM_PROB_DEVLOADER_FAILED           = $00000002;   // service load failed
  {$EXTERNALSYM CM_PROB_DEVLOADER_FAILED}
  CM_PROB_OUT_OF_MEMORY              = $00000003;   // out of memory
  {$EXTERNALSYM CM_PROB_OUT_OF_MEMORY}
  CM_PROB_ENTRY_IS_WRONG_TYPE        = $00000004;   //
  {$EXTERNALSYM CM_PROB_ENTRY_IS_WRONG_TYPE}
  CM_PROB_LACKED_ARBITRATOR          = $00000005;   //
  {$EXTERNALSYM CM_PROB_LACKED_ARBITRATOR}
  CM_PROB_BOOT_CONFIG_CONFLICT       = $00000006;   // boot config conflict
  {$EXTERNALSYM CM_PROB_BOOT_CONFIG_CONFLICT}
  CM_PROB_FAILED_FILTER              = $00000007;   //
  {$EXTERNALSYM CM_PROB_FAILED_FILTER}
  CM_PROB_DEVLOADER_NOT_FOUND        = $00000008;   // Devloader not found
  {$EXTERNALSYM CM_PROB_DEVLOADER_NOT_FOUND}
  CM_PROB_INVALID_DATA               = $00000009;   //
  {$EXTERNALSYM CM_PROB_INVALID_DATA}
  CM_PROB_FAILED_START               = $0000000A;   //
  {$EXTERNALSYM CM_PROB_FAILED_START}
  CM_PROB_LIAR                       = $0000000B;   //
  {$EXTERNALSYM CM_PROB_LIAR}
  CM_PROB_NORMAL_CONFLICT            = $0000000C;   // config conflict
  {$EXTERNALSYM CM_PROB_NORMAL_CONFLICT}
  CM_PROB_NOT_VERIFIED               = $0000000D;   //
  {$EXTERNALSYM CM_PROB_NOT_VERIFIED}
  CM_PROB_NEED_RESTART               = $0000000E;   // requires restart
  {$EXTERNALSYM CM_PROB_NEED_RESTART}
  CM_PROB_REENUMERATION              = $0000000F;   //
  {$EXTERNALSYM CM_PROB_REENUMERATION}
  CM_PROB_PARTIAL_LOG_CONF           = $00000010;   //
  {$EXTERNALSYM CM_PROB_PARTIAL_LOG_CONF}
  CM_PROB_UNKNOWN_RESOURCE           = $00000011;   // unknown res type
  {$EXTERNALSYM CM_PROB_UNKNOWN_RESOURCE}
  CM_PROB_REINSTALL                  = $00000012;   //
  {$EXTERNALSYM CM_PROB_REINSTALL}
  CM_PROB_REGISTRY                   = $00000013;   //
  {$EXTERNALSYM CM_PROB_REGISTRY}
  CM_PROB_VXDLDR                     = $00000014;   // WINDOWS 95 ONLY
  {$EXTERNALSYM CM_PROB_VXDLDR}
  CM_PROB_WILL_BE_REMOVED            = $00000015;   // devinst will remove
  {$EXTERNALSYM CM_PROB_WILL_BE_REMOVED}
  CM_PROB_DISABLED                   = $00000016;   // devinst is disabled
  {$EXTERNALSYM CM_PROB_DISABLED}
  CM_PROB_DEVLOADER_NOT_READY        = $00000017;   // Devloader not ready
  {$EXTERNALSYM CM_PROB_DEVLOADER_NOT_READY}
  CM_PROB_DEVICE_NOT_THERE           = $00000018;   // device doesn't exist
  {$EXTERNALSYM CM_PROB_DEVICE_NOT_THERE}
  CM_PROB_MOVED                      = $00000019;   //
  {$EXTERNALSYM CM_PROB_MOVED}
  CM_PROB_TOO_EARLY                  = $0000001A;   //
  {$EXTERNALSYM CM_PROB_TOO_EARLY}
  CM_PROB_NO_VALID_LOG_CONF          = $0000001B;   // no valid log config
  {$EXTERNALSYM CM_PROB_NO_VALID_LOG_CONF}
  CM_PROB_FAILED_INSTALL             = $0000001C;   // install failed
  {$EXTERNALSYM CM_PROB_FAILED_INSTALL}
  CM_PROB_HARDWARE_DISABLED          = $0000001D;   // device disabled
  {$EXTERNALSYM CM_PROB_HARDWARE_DISABLED}
  CM_PROB_CANT_SHARE_IRQ             = $0000001E;   // can't share IRQ
  {$EXTERNALSYM CM_PROB_CANT_SHARE_IRQ}
  CM_PROB_FAILED_ADD                 = $0000001F;   // driver failed add
  {$EXTERNALSYM CM_PROB_FAILED_ADD}
  CM_PROB_DISABLED_SERVICE           = $00000020;   // service's Start = 4
  {$EXTERNALSYM CM_PROB_DISABLED_SERVICE}
  CM_PROB_TRANSLATION_FAILED         = $00000021;   // resource translation failed
  {$EXTERNALSYM CM_PROB_TRANSLATION_FAILED}
  CM_PROB_NO_SOFTCONFIG              = $00000022;   // no soft config
  {$EXTERNALSYM CM_PROB_NO_SOFTCONFIG}
  CM_PROB_BIOS_TABLE                 = $00000023;   // device missing in BIOS table
  {$EXTERNALSYM CM_PROB_BIOS_TABLE}
  CM_PROB_IRQ_TRANSLATION_FAILED     = $00000024;   // IRQ translator failed
  {$EXTERNALSYM CM_PROB_IRQ_TRANSLATION_FAILED}
  CM_PROB_FAILED_DRIVER_ENTRY        = $00000025;   // DriverEntry() failed.
  {$EXTERNALSYM CM_PROB_FAILED_DRIVER_ENTRY}
  CM_PROB_DRIVER_FAILED_PRIOR_UNLOAD = $00000026;   // Driver should have unloaded.
  {$EXTERNALSYM CM_PROB_DRIVER_FAILED_PRIOR_UNLOAD}
  CM_PROB_DRIVER_FAILED_LOAD         = $00000027;   // Driver load unsuccessful.
  {$EXTERNALSYM CM_PROB_DRIVER_FAILED_LOAD}
  CM_PROB_DRIVER_SERVICE_KEY_INVALID = $00000028;   // Error accessing driver's service key
  {$EXTERNALSYM CM_PROB_DRIVER_SERVICE_KEY_INVALID}
  CM_PROB_LEGACY_SERVICE_NO_DEVICES  = $00000029;   // Loaded legacy service created no devices
  {$EXTERNALSYM CM_PROB_LEGACY_SERVICE_NO_DEVICES}
  CM_PROB_DUPLICATE_DEVICE           = $0000002A;   // Two devices were discovered with the same name
  {$EXTERNALSYM CM_PROB_DUPLICATE_DEVICE}
  CM_PROB_FAILED_POST_START          = $0000002B;   // The drivers set the device state to failed
  {$EXTERNALSYM CM_PROB_FAILED_POST_START}
  CM_PROB_HALTED                     = $0000002C;   // This device was failed post start via usermode
  {$EXTERNALSYM CM_PROB_HALTED}
  CM_PROB_PHANTOM                    = $0000002D;   // The devinst currently exists only in the registry
  {$EXTERNALSYM CM_PROB_PHANTOM}
  CM_PROB_SYSTEM_SHUTDOWN            = $0000002E;   // The system is shutting down
  {$EXTERNALSYM CM_PROB_SYSTEM_SHUTDOWN}
  CM_PROB_HELD_FOR_EJECT             = $0000002F;   // The device is offline awaiting removal
  {$EXTERNALSYM CM_PROB_HELD_FOR_EJECT}
  CM_PROB_DRIVER_BLOCKED             = $00000030;   // One or more drivers is blocked from loading
  {$EXTERNALSYM CM_PROB_DRIVER_BLOCKED}
  CM_PROB_REGISTRY_TOO_LARGE         = $00000031;   // System hive has grown too large
  {$EXTERNALSYM CM_PROB_REGISTRY_TOO_LARGE}
  NUM_CM_PROB                        = $00000032;
  {$EXTERNALSYM NUM_CM_PROB}

  //
  // Configuration Manager Global State Flags (returned by CM_Get_Global_State)
  //
  CM_GLOBAL_STATE_CAN_DO_UI            = $00000001; // Can  do UI?
  {$EXTERNALSYM CM_GLOBAL_STATE_CAN_DO_UI}
  CM_GLOBAL_STATE_ON_BIG_STACK         = $00000002; // WINDOWS 95 ONLY
  {$EXTERNALSYM CM_GLOBAL_STATE_ON_BIG_STACK}
  CM_GLOBAL_STATE_SERVICES_AVAILABLE   = $00000004; // CM APIs available?
  {$EXTERNALSYM CM_GLOBAL_STATE_SERVICES_AVAILABLE}
  CM_GLOBAL_STATE_SHUTTING_DOWN        = $00000008; // CM shutting down
  {$EXTERNALSYM CM_GLOBAL_STATE_SHUTTING_DOWN}
  CM_GLOBAL_STATE_DETECTION_PENDING    = $00000010; // detection pending
  {$EXTERNALSYM CM_GLOBAL_STATE_DETECTION_PENDING}

  //
  // Device Instance status flags, returned by call to CM_Get_DevInst_Status
  //
  DN_ROOT_ENUMERATED = $00000001; // Was enumerated by ROOT
  {$EXTERNALSYM DN_ROOT_ENUMERATED}
  DN_DRIVER_LOADED   = $00000002; // Has Register_Device_Driver
  {$EXTERNALSYM DN_DRIVER_LOADED}
  DN_ENUM_LOADED     = $00000004; // Has Register_Enumerator
  {$EXTERNALSYM DN_ENUM_LOADED}
  DN_STARTED         = $00000008; // Is currently configured
  {$EXTERNALSYM DN_STARTED}
  DN_MANUAL          = $00000010; // Manually installed
  {$EXTERNALSYM DN_MANUAL}
  DN_NEED_TO_ENUM    = $00000020; // May need reenumeration
  {$EXTERNALSYM DN_NEED_TO_ENUM}
  DN_NOT_FIRST_TIME  = $00000040; // Has received a config
  {$EXTERNALSYM DN_NOT_FIRST_TIME}
  DN_HARDWARE_ENUM   = $00000080; // Enum generates hardware ID
  {$EXTERNALSYM DN_HARDWARE_ENUM}
  DN_LIAR            = $00000100; // Lied about can reconfig once
  {$EXTERNALSYM DN_LIAR}
  DN_HAS_MARK        = $00000200; // Not CM_Create_DevInst lately
  {$EXTERNALSYM DN_HAS_MARK}
  DN_HAS_PROBLEM     = $00000400; // Need device installer
  {$EXTERNALSYM DN_HAS_PROBLEM}
  DN_FILTERED        = $00000800; // Is filtered
  {$EXTERNALSYM DN_FILTERED}
  DN_MOVED           = $00001000; // Has been moved
  {$EXTERNALSYM DN_MOVED}
  DN_DISABLEABLE     = $00002000; // Can be rebalanced
  {$EXTERNALSYM DN_DISABLEABLE}
  DN_REMOVABLE       = $00004000; // Can be removed
  {$EXTERNALSYM DN_REMOVABLE}
  DN_PRIVATE_PROBLEM = $00008000; // Has a private problem
  {$EXTERNALSYM DN_PRIVATE_PROBLEM}
  DN_MF_PARENT       = $00010000; // Multi function parent
  {$EXTERNALSYM DN_MF_PARENT}
  DN_MF_CHILD        = $00020000; // Multi function child
  {$EXTERNALSYM DN_MF_CHILD}
  DN_WILL_BE_REMOVED = $00040000; // DevInst is being removed
  {$EXTERNALSYM DN_WILL_BE_REMOVED}

  //
  // Windows 4 OPK2 Flags
  //
  DN_NOT_FIRST_TIMEE  = $00080000;  // S: Has received a config enumerate
  {$EXTERNALSYM DN_NOT_FIRST_TIMEE}
  DN_STOP_FREE_RES    = $00100000;  // S: When child is stopped, free resources
  {$EXTERNALSYM DN_STOP_FREE_RES}
  DN_REBAL_CANDIDATE  = $00200000;  // S: Don't skip during rebalance
  {$EXTERNALSYM DN_REBAL_CANDIDATE}
  DN_BAD_PARTIAL      = $00400000;  // S: This devnode's log_confs do not have same resources
  {$EXTERNALSYM DN_BAD_PARTIAL}
  DN_NT_ENUMERATOR    = $00800000;  // S: This devnode's is an NT enumerator
  {$EXTERNALSYM DN_NT_ENUMERATOR}
  DN_NT_DRIVER        = $01000000;  // S: This devnode's is an NT driver
  {$EXTERNALSYM DN_NT_DRIVER}
  //
  // Windows 4.1 Flags
  //
  DN_NEEDS_LOCKING    = $02000000;  // S: Devnode need lock resume processing
  {$EXTERNALSYM DN_NEEDS_LOCKING}
  DN_ARM_WAKEUP       = $04000000;  // S: Devnode can be the wakeup device
  {$EXTERNALSYM DN_ARM_WAKEUP}
  DN_APM_ENUMERATOR   = $08000000;  // S: APM aware enumerator
  {$EXTERNALSYM DN_APM_ENUMERATOR}
  DN_APM_DRIVER       = $10000000;  // S: APM aware driver
  {$EXTERNALSYM DN_APM_DRIVER}
  DN_SILENT_INSTALL   = $20000000;  // S: Silent install
  {$EXTERNALSYM DN_SILENT_INSTALL}
  DN_NO_SHOW_IN_DM    = $40000000;  // S: No show in device manager
  {$EXTERNALSYM DN_NO_SHOW_IN_DM}
  DN_BOOT_LOG_PROB    = $80000000;  // S: Had a problem during preassignment of boot log conf
  {$EXTERNALSYM DN_BOOT_LOG_PROB}

  //
  // Windows NT Flags
  //
  // These are overloaded on top of unused Win 9X flags
  //
  //DN_LIAR             = $00000100;           // Lied about can reconfig once
  DN_NEED_RESTART       = DN_LIAR;             // System needs to be restarted for this Devnode to work properly
  {$EXTERNALSYM DN_NEED_RESTART}
  //DN_NOT_FIRST_TIME   = $00000040;           // Has Register_Enumerator
  DN_DRIVER_BLOCKED     = DN_NOT_FIRST_TIME;   // One or more drivers are blocked from loading for this Devnode
  {$EXTERNALSYM DN_DRIVER_BLOCKED}
  //DN_MOVED            = $00001000;           // Has been moved
  DN_LEGACY_DRIVER      = DN_MOVED;            // This device is using a legacy driver
  {$EXTERNALSYM DN_LEGACY_DRIVER}

  DN_CHANGEABLE_FLAGS   = DWORD(DN_NOT_FIRST_TIME +
    DN_HARDWARE_ENUM + DN_HAS_MARK + DN_DISABLEABLE +
    DN_REMOVABLE + DN_MF_CHILD + DN_MF_PARENT +
    DN_NOT_FIRST_TIMEE + DN_STOP_FREE_RES + DN_REBAL_CANDIDATE +
    DN_NT_ENUMERATOR + DN_NT_DRIVER + DN_SILENT_INSTALL + DN_NO_SHOW_IN_DM);
  {$EXTERNALSYM DN_CHANGEABLE_FLAGS}

  //
  // Logical configuration Priority values
  //
  // These priority values are used in user-mode calls to CM_Add_Empty_Log_Conf.
  // Drivers may also specify priority values for a given IO_RESOURCE_LIST
  // structure by including a ConfigData member union as the first
  // IO_RESOURCE_DESCRIPTOR in the IO_RESOURCE_LIST. In this case, the descriptor
  // type would be CmResourceTypeConfigData.
  //
  LCPRI_FORCECONFIG     = $00000000; // Coming from a forced config
  {$EXTERNALSYM LCPRI_FORCECONFIG}
  LCPRI_BOOTCONFIG      = $00000001; // Coming from a boot config
  {$EXTERNALSYM LCPRI_BOOTCONFIG}
  LCPRI_DESIRED         = $00002000; // Preferable (better performance)
  {$EXTERNALSYM LCPRI_DESIRED}
  LCPRI_NORMAL          = $00003000; // Workable (acceptable performance)
  {$EXTERNALSYM LCPRI_NORMAL}
  LCPRI_LASTBESTCONFIG  = $00003FFF; // CM only--do not use
  {$EXTERNALSYM LCPRI_LASTBESTCONFIG}
  LCPRI_SUBOPTIMAL      = $00005000; // Not desired, but will work
  {$EXTERNALSYM LCPRI_SUBOPTIMAL}
  LCPRI_LASTSOFTCONFIG  = $00007FFF; // CM only--do not use
  {$EXTERNALSYM LCPRI_LASTSOFTCONFIG}
  LCPRI_RESTART         = $00008000; // Need to restart
  {$EXTERNALSYM LCPRI_RESTART}
  LCPRI_REBOOT          = $00009000; // Need to reboot
  {$EXTERNALSYM LCPRI_REBOOT}
  LCPRI_POWEROFF        = $0000A000; // Need to shutdown/power-off
  {$EXTERNALSYM LCPRI_POWEROFF}
  LCPRI_HARDRECONFIG    = $0000C000; // Need to change a jumper
  {$EXTERNALSYM LCPRI_HARDRECONFIG}
  LCPRI_HARDWIRED       = $0000E000; // Cannot be changed
  {$EXTERNALSYM LCPRI_HARDWIRED}
  LCPRI_IMPOSSIBLE      = $0000F000; // Impossible configuration
  {$EXTERNALSYM LCPRI_IMPOSSIBLE}
  LCPRI_DISABLED        = $0000FFFF; // Disabled configuration
  {$EXTERNALSYM LCPRI_DISABLED}
  MAX_LCPRI             = $0000FFFF; // Maximum known LC Priority
  {$EXTERNALSYM MAX_LCPRI}

implementation

end.

