{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Config Manager API interface unit                          }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) Microsoft Corporation.  All rights reserved.       }
{                                                                  }
{ The original file is: cfgmgr32.h, released August 2001.          }
{ The original Pascal code is: CfgMgr32.pas, released 5 Nov 2004.  }
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

unit CfgMgr32;

{$I windowsversion.inc}

interface

{$WEAKPACKAGEUNIT ON}

uses
  Windows,
  Cfg, WinConvTypes, SetupApi;

// (rom) this is the switch to change between static and dynamic linking.
// (rom) it is enabled by default here.
// (rom) To disable simply change the '$' to a '.'.
{$DEFINE CFGMGR32_LINKONREQUEST}

{$HPPEMIT '#include "cfgmgr32.h"'}

//--------------------------------------------------------------
// General size definitions
//--------------------------------------------------------------

const
  MAX_DEVICE_ID_LEN     = 200;
  {$EXTERNALSYM MAX_DEVICE_ID_LEN}
  MAX_DEVNODE_ID_LEN    = MAX_DEVICE_ID_LEN;
  {$EXTERNALSYM MAX_DEVNODE_ID_LEN}

  MAX_GUID_STRING_LEN   = 39;          // 38 chars + terminator null
  {$EXTERNALSYM MAX_GUID_STRING_LEN}
  MAX_CLASS_NAME_LEN    = 32;
  {$EXTERNALSYM MAX_CLASS_NAME_LEN}
  MAX_PROFILE_LEN       = 80;
  {$EXTERNALSYM MAX_PROFILE_LEN}

  MAX_CONFIG_VALUE      = 9999;
  {$EXTERNALSYM MAX_CONFIG_VALUE}
  MAX_INSTANCE_VALUE    = 9999;
  {$EXTERNALSYM MAX_INSTANCE_VALUE}

  MAX_MEM_REGISTERS     = 9;     // Win95 compatibility--not applicable to 32-bit ConfigMgr
  {$EXTERNALSYM MAX_MEM_REGISTERS}
  MAX_IO_PORTS          = 20;    // Win95 compatibility--not applicable to 32-bit ConfigMgr
  {$EXTERNALSYM MAX_IO_PORTS}
  MAX_IRQS              = 7;     // Win95 compatibility--not applicable to 32-bit ConfigMgr
  {$EXTERNALSYM MAX_IRQS}
  MAX_DMA_CHANNELS      = 7;     // Win95 compatibility--not applicable to 32-bit ConfigMgr
  {$EXTERNALSYM MAX_DMA_CHANNELS}

  DWORD_MAX             = $FFFFFFFF;
  {$EXTERNALSYM DWORD_MAX}
  DWORDLONG_MAX         = $FFFFFFFFFFFFFFFF;
  {$EXTERNALSYM DWORDLONG_MAX}

  CONFIGMG_VERSION      = $0400;
  {$EXTERNALSYM CONFIGMG_VERSION}

type
  PDWORDLONG = ^DWORDLONG;
  {$EXTERNALSYM PDWORDLONG}
  DWORDLONG = Int64; // (rom) no unsigned Int64 available in Delphi
  {$EXTERNALSYM DWORDLONG}

//--------------------------------------------------------------
// Data types
//--------------------------------------------------------------

  //
  // Standardized Return Value data type
  //
  RETURN_TYPE = DWORD;
  {$EXTERNALSYM RETURN_TYPE}
  CONFIGRET = RETURN_TYPE;
  {$EXTERNALSYM CONFIGRET}

  //
  // Device Instance Handle data type
  //
  PDEVNODE = ^DEVNODE;
  {$EXTERNALSYM PDEVNODE}
  DEVNODE = DWORD;
  {$EXTERNALSYM DEVNODE}
  PDEVINST = ^DEVINST;
  {$EXTERNALSYM PDEVINST}
  DEVINST = DWORD;
  {$EXTERNALSYM DEVINST}

  //
  // Device Instance Identifier data type
  // The device instance ID specifies the registry path, relative to the
  // Enum key , for a device instance.  For example:  \Root\*PNP0500\0000.
  //
  DEVNODEID_A = PAnsiChar; // Device ID ANSI name.
  {$EXTERNALSYM DEVNODEID_A}
  DEVNODEID_W = PWideChar; // Device ID Unicode name.
  {$EXTERNALSYM DEVNODEID_W}
  DEVINSTID_A = PAnsiChar; // Device ID ANSI name.
  {$EXTERNALSYM DEVINSTID_A}
  DEVINSTID_W = PWideChar; // Device ID Unicode name.
  {$EXTERNALSYM DEVINSTID_W}
  {$IFDEF UNICODE}
  DEVNODEID = DEVNODEID_W;
  DEVINSTID = DEVINSTID_W;
  {$ELSE}
  DEVNODEID = DEVNODEID_A;
  DEVINSTID = DEVINSTID_A;
  {$ENDIF UNICODE}
  {$EXTERNALSYM DEVNODEID}
  {$EXTERNALSYM DEVINSTID}

  //
  // Logical Configuration Handle data type
  //
  LOG_CONF = DWORD_PTR;
  {$EXTERNALSYM LOG_CONF}
  PLOG_CONF = ^LOG_CONF;
  {$EXTERNALSYM PLOG_CONF}

  //
  // Resource Descriptor Handle data type
  //
  RES_DES = DWORD_PTR;
  {$EXTERNALSYM RES_DES}
  PRES_DES = ^RES_DES;
  {$EXTERNALSYM PRES_DES}

  //
  // Resource ID data type (may take any of the ResType_* values)
  //
  RESOURCEID = ULONG;
  {$EXTERNALSYM RESOURCEID}
  PRESOURCEID = ^RESOURCEID;
  {$EXTERNALSYM PRESOURCEID}

  //
  // Priority data type (may take any of the LCPRI_* values)
  //
  PRIORITY = ULONG;
  {$EXTERNALSYM PRIORITY}
  PPRIORITY = ^PRIORITY;
  {$EXTERNALSYM PPRIORITY}

  //
  // Range List Handle data type
  //
  RANGE_LIST = DWORD_PTR;
  {$EXTERNALSYM RANGE_LIST}
  PRANGE_LIST = ^RANGE_LIST;
  {$EXTERNALSYM PRANGE_LIST}

  //
  // Range Element Handle data type
  //
  RANGE_ELEMENT = DWORD_PTR;
  {$EXTERNALSYM RANGE_ELEMENT}
  PRANGE_ELEMENT = ^RANGE_ELEMENT;
  {$EXTERNALSYM PRANGE_ELEMENT}

  //
  // Machine Handle data type
  //
  HMACHINE = THandle;
  {$EXTERNALSYM HMACHINE}
  PHMACHINE = ^HMACHINE;
  {$EXTERNALSYM PHMACHINE}

  //
  // Conflict List data types
  //
  CONFLICT_LIST = ULONG_PTR;
  {$EXTERNALSYM CONFLICT_LIST}
  PCONFLICT_LIST = ^CONFLICT_LIST;
  {$EXTERNALSYM PCONFLICT_LIST}

  PCONFLICT_DETAILS_A = ^CONFLICT_DETAILS_A;
  {$EXTERNALSYM PCONFLICT_DETAILS_A}
  CONFLICT_DETAILS_A = packed record
    CD_ulSize: ULONG;                   // size of structure, ie: sizeof(CONFLICT_DETAILS)
    CD_ulMask: ULONG;                   // indicates what information is required/valid
    CD_dnDevInst: DEVINST;              // filled with DevInst of conflicting device if CM_CDMASK_DEVINST set
    CD_rdResDes: RES_DES;               // filled with a ResDes of conflict if CM_CDMASK_RESDES set
    CD_ulFlags: ULONG;                  // various flags regarding conflict
    CD_szDescription: array [0..MAX_PATH-1] of Char;  // description of conflicting device
  end;
  {$EXTERNALSYM CONFLICT_DETAILS_A}

  PCONFLICT_DETAILS_W = ^CONFLICT_DETAILS_W;
  {$EXTERNALSYM PCONFLICT_DETAILS_W}
  CONFLICT_DETAILS_W = packed record
    CD_ulSize: ULONG;                   // size of structure, ie: sizeof(CONFLICT_DETAILS)
    CD_ulMask: ULONG;                   // indicates what information is required/valid
    CD_dnDevInst: DEVINST;              // filled with DevInst of conflicting device if CM_CDMASK_DEVINST set
    CD_rdResDes: RES_DES;               // filled with a ResDes of conflict if CM_CDMASK_RESDES set
    CD_ulFlags: ULONG;                  // various flags regarding conflict
    CD_szDescription: array [0..MAX_PATH-1] of WideChar;  // description of conflicting device
  end;
  {$EXTERNALSYM CONFLICT_DETAILS_W}

  {$IFDEF UNICODE}
  CONFLICT_DETAILS = CONFLICT_DETAILS_W;
  PCONFLICT_DETAILS = PCONFLICT_DETAILS_W;
  {$ELSE}
  CONFLICT_DETAILS = CONFLICT_DETAILS_A;
  PCONFLICT_DETAILS = PCONFLICT_DETAILS_A;
  {$ENDIF UNICODE}
  {$EXTERNALSYM CONFLICT_DETAILS}
  {$EXTERNALSYM PCONFLICT_DETAILS}

const
  CM_CDMASK_DEVINST      = $00000001;   // mask to retrieve CD_dnDevInst attribute for conflict
  {$EXTERNALSYM CM_CDMASK_DEVINST}
  CM_CDMASK_RESDES       = $00000002;   // mask to retrieve CD_rdResDes attribute for conflict
  {$EXTERNALSYM CM_CDMASK_RESDES}
  CM_CDMASK_FLAGS        = $00000004;   // mask to retrieve CD_ulFlags attribute for conflict
  {$EXTERNALSYM CM_CDMASK_FLAGS}
  CM_CDMASK_DESCRIPTION  = $00000008;   // mask to retrieve CD_szDescription attribute for conflict
  {$EXTERNALSYM CM_CDMASK_DESCRIPTION}
  CM_CDMASK_VALID        = $0000000F;   // valid bits
  {$EXTERNALSYM CM_CDMASK_VALID}

  CM_CDFLAGS_DRIVER      = $00000001;   // CD_ulFlags: CD_szDescription reports back legacy driver name
  {$EXTERNALSYM CM_CDFLAGS_DRIVER}
  CM_CDFLAGS_ROOT_OWNED  = $00000002;   // CD_ulFlags: Root owned device
  {$EXTERNALSYM CM_CDFLAGS_ROOT_OWNED}
  CM_CDFLAGS_RESERVED    = $00000004;   // CD_ulFlags: Specified range is not available for use
  {$EXTERNALSYM CM_CDFLAGS_RESERVED}

type
  REGDISPOSITION = ^ULONG;
  {$EXTERNALSYM REGDISPOSITION}

//--------------------------------------------------------------
// Memory resource
//--------------------------------------------------------------

const
  //
  // Define the attribute flags for memory ranges.  Each bit flag is
  // identified by a constant bitmask.  Following the bitmask definition,
  // are the two possible values.
  //
  mMD_MemoryType              = $1; // Bitmask, whether memory is writable
  {$EXTERNALSYM mMD_MemoryType}
  fMD_MemoryType              = mMD_MemoryType; // compatibility
  {$EXTERNALSYM fMD_MemoryType}
  fMD_ROM                     = $0; // Memory range is read-only
  {$EXTERNALSYM fMD_ROM}
  fMD_RAM                     = $1; // Memory range may be written to
  {$EXTERNALSYM fMD_RAM}

  mMD_32_24                   = $2; // Bitmask, memory is 24 or 32-bit
  {$EXTERNALSYM mMD_32_24}
  fMD_32_24                   = mMD_32_24; // compatibility
  {$EXTERNALSYM fMD_32_24}
  fMD_24                      = $0; // Memory range is 24-bit
  {$EXTERNALSYM fMD_24}
  fMD_32                      = $2; // Memory range is 32-bit
  {$EXTERNALSYM fMD_32}

  mMD_Prefetchable            = $4; // Bitmask,whether memory prefetchable
  {$EXTERNALSYM mMD_Prefetchable}
  fMD_Prefetchable            = mMD_Prefetchable; // compatibility
  {$EXTERNALSYM fMD_Prefetchable}
  fMD_Pref                    = mMD_Prefetchable; // compatibility
  {$EXTERNALSYM fMD_Pref}
  fMD_PrefetchDisallowed      = $0; // Memory range is not prefetchable
  {$EXTERNALSYM fMD_PrefetchDisallowed}
  fMD_PrefetchAllowed         = $4; // Memory range is prefetchable
  {$EXTERNALSYM fMD_PrefetchAllowed}

  mMD_Readable                = $8; // Bitmask,whether memory is readable
  {$EXTERNALSYM mMD_Readable}
  fMD_Readable                = mMD_Readable; // compatibility
  {$EXTERNALSYM fMD_Readable}
  fMD_ReadAllowed             = $0; // Memory range is readable
  {$EXTERNALSYM fMD_ReadAllowed}
  fMD_ReadDisallowed          = $8; // Memory range is write-only
  {$EXTERNALSYM fMD_ReadDisallowed}

  mMD_CombinedWrite           = $10; // Bitmask,supports write-behind
  {$EXTERNALSYM mMD_CombinedWrite}
  fMD_CombinedWrite           = mMD_CombinedWrite; // compatibility
  {$EXTERNALSYM fMD_CombinedWrite}
  fMD_CombinedWriteDisallowed = $0;  // no combined-write caching
  {$EXTERNALSYM fMD_CombinedWriteDisallowed}
  fMD_CombinedWriteAllowed    = $10; // supports combined-write caching
  {$EXTERNALSYM fMD_CombinedWriteAllowed}

  mMD_Cacheable               = $20; // Bitmask,whether memory is cacheable
  {$EXTERNALSYM mMD_Cacheable}
  fMD_NonCacheable            = $0;  // Memory range is non-cacheable
  {$EXTERNALSYM fMD_NonCacheable}
  fMD_Cacheable               = $20; // Memory range is cacheable
  {$EXTERNALSYM fMD_Cacheable}

type
  //
  // MEM_RANGE Structure
  //
  PMEM_RANGE = ^MEM_RANGE;
  {$EXTERNALSYM PMEM_RANGE}
  MEM_RANGE = packed record
    MR_Align: DWORDLONG;     // specifies mask for base alignment
    MR_nBytes: ULONG;        // specifies number of bytes required
    MR_Min: DWORDLONG;       // specifies minimum address of the range
    MR_Max: DWORDLONG;       // specifies maximum address of the range
    MR_Flags: DWORD;         // specifies flags describing range (fMD flags)
    MR_Reserved: DWORD;
  end;
  {$EXTERNALSYM MEM_RANGE}

  //
  // MEM_DES structure
  //
  PMEM_DES = ^MEM_DES;
  {$EXTERNALSYM PMEM_DES}
  MEM_DES = packed record
    MD_Count: DWORD;            // number of MEM_RANGE structs in MEM_RESOURCE
    MD_Type: DWORD;             // size (in bytes) of MEM_RANGE (MType_Range)
    MD_Alloc_Base: DWORDLONG;   // base memory address of range allocated
    MD_Alloc_End: DWORDLONG;    // end of allocated range
    MD_Flags: DWORD;            // flags describing allocated range (fMD flags)
    MD_Reserved: DWORD;
  end;
  {$EXTERNALSYM MEM_DES}

  //
  // MEM_RESOURCE structure
  //
  PMEM_RESOURCE = ^MEM_RESOURCE;
  {$EXTERNALSYM PMEM_RESOURCE}
  MEM_RESOURCE = packed record
    MEM_Header: MEM_DES;                  // info about memory range list
    MEM_Data: array [0..0] of MEM_RANGE;  // list of memory ranges
  end;
  {$EXTERNALSYM MEM_RESOURCE}

const
  //
  // Define the size of each range structure
  //
  MType_Range = SizeOf(MEM_RANGE);
  {$EXTERNALSYM MType_Range}

//--------------------------------------------------------------
// I/O Port Resource
//--------------------------------------------------------------

const
  //
  // Define the attribute flags for port resources.  Each bit flag is
  // identified by a constant bitmask.  Following the bitmask definition,
  // are the two possible values.
  //
  fIOD_PortType   = $1;    // Bitmask,whether port is IO or memory
  {$EXTERNALSYM fIOD_PortType}
  fIOD_Memory     = $0;    // Port resource really uses memory
  {$EXTERNALSYM fIOD_Memory}
  fIOD_IO         = $1;    // Port resource uses IO ports
  {$EXTERNALSYM fIOD_IO}
  fIOD_DECODE     = $00fc; // decode flags
  {$EXTERNALSYM fIOD_DECODE}
  fIOD_10_BIT_DECODE    = $0004;
  {$EXTERNALSYM fIOD_10_BIT_DECODE}
  fIOD_12_BIT_DECODE    = $0008;
  {$EXTERNALSYM fIOD_12_BIT_DECODE}
  fIOD_16_BIT_DECODE    = $0010;
  {$EXTERNALSYM fIOD_16_BIT_DECODE}
  fIOD_POSITIVE_DECODE  = $0020;
  {$EXTERNALSYM fIOD_POSITIVE_DECODE}
  fIOD_PASSIVE_DECODE   = $0040;
  {$EXTERNALSYM fIOD_PASSIVE_DECODE}
  fIOD_WINDOW_DECODE    = $0080;
  {$EXTERNALSYM fIOD_WINDOW_DECODE}

  //
  // these are for compatiblity
  //
  IO_ALIAS_10_BIT_DECODE      = $00000004;
  {$EXTERNALSYM IO_ALIAS_10_BIT_DECODE}
  IO_ALIAS_12_BIT_DECODE      = $00000010;
  {$EXTERNALSYM IO_ALIAS_12_BIT_DECODE}
  IO_ALIAS_16_BIT_DECODE      = $00000000;
  {$EXTERNALSYM IO_ALIAS_16_BIT_DECODE}
  IO_ALIAS_POSITIVE_DECODE    = $000000FF;
  {$EXTERNALSYM IO_ALIAS_POSITIVE_DECODE}

type
  //
  // IO_RANGE structure
  //
  PIO_RANGE = ^IO_RANGE;
  {$EXTERNALSYM PIO_RANGE}
  IO_RANGE = packed record
    IOR_Align: DWORDLONG;      // mask for base alignment
    IOR_nPorts: DWORD;         // number of ports
    IOR_Min: DWORDLONG;        // minimum port address
    IOR_Max: DWORDLONG;        // maximum port address
    IOR_RangeFlags: DWORD;     // flags for this port range
    IOR_Alias: DWORDLONG;      // multiplier that generates aliases for port(s)
  end;
  {$EXTERNALSYM IO_RANGE}

  //
  // IO_DES structure
  //
  PIO_DES = ^IO_DES;
  {$EXTERNALSYM PIO_DES}
  IO_DES = packed record
    IOD_Count: DWORD;          // number of IO_RANGE structs in IO_RESOURCE
    IOD_Type: DWORD;           // size (in bytes) of IO_RANGE (IOType_Range)
    IOD_Alloc_Base: DWORDLONG; // base of allocated port range
    IOD_Alloc_End: DWORDLONG;  // end of allocated port range
    IOD_DesFlags: DWORD;       // flags relating to allocated port range
  end;
  {$EXTERNALSYM IO_DES}

  //
  // IO_RESOURCE
  //
  PIO_RESOURCE = ^IO_RESOURCE;
  {$EXTERNALSYM PIO_RESOURCE}
  IO_RESOURCE = packed record
    IO_Header: IO_DES;                 // info about I/O port range list
    IO_Data: array [0..0] of IO_RANGE; // list of I/O port ranges
  end;
  {$EXTERNALSYM IO_RESOURCE}

const
  IOA_Local = $ff;
  {$EXTERNALSYM IOA_Local}

  //
  // Define the size of each range structure
  //
  IOType_Range = SizeOf(IO_RANGE);
  {$EXTERNALSYM IOType_Range}

//--------------------------------------------------------------
// DMA Resource
//--------------------------------------------------------------

const
  //
  // Define the attribute flags for a DMA resource range.  Each bit flag is
  // identified with a constant bitmask.  Following the bitmask definition
  // are the possible values.
  //
  mDD_Width         = $3;    // Bitmask, width of the DMA channel:
  {$EXTERNALSYM mDD_Width}
  fDD_BYTE          = $0;    //   8-bit DMA channel
  {$EXTERNALSYM fDD_BYTE}
  fDD_WORD          = $1;    //   16-bit DMA channel
  {$EXTERNALSYM fDD_WORD}
  fDD_DWORD         = $2;    //   32-bit DMA channel
  {$EXTERNALSYM fDD_DWORD}
  fDD_BYTE_AND_WORD = $3;    //   8-bit and 16-bit DMA channel
  {$EXTERNALSYM fDD_BYTE_AND_WORD}

  mDD_BusMaster     = $4;    // Bitmask, whether bus mastering is supported
  {$EXTERNALSYM mDD_BusMaster}
  fDD_NoBusMaster   = $0;    //   no bus mastering
  {$EXTERNALSYM fDD_NoBusMaster}
  fDD_BusMaster     = $4;    //   bus mastering
  {$EXTERNALSYM fDD_BusMaster}

  mDD_Type         = $18;    // Bitmask, specifies type of DMA
  {$EXTERNALSYM mDD_Type}
  fDD_TypeStandard = $00;    //   standard DMA
  {$EXTERNALSYM fDD_TypeStandard}
  fDD_TypeA        = $08;    //   Type-A DMA
  {$EXTERNALSYM fDD_TypeA}
  fDD_TypeB        = $10;    //   Type-B DMA
  {$EXTERNALSYM fDD_TypeB}
  fDD_TypeF        = $18;    //   Type-F DMA
  {$EXTERNALSYM fDD_TypeF}

type
  //
  // DMA_RANGE structure
  //
  PDMA_RANGE = ^DMA_RANGE;
  {$EXTERNALSYM PDMA_RANGE}
  DMA_RANGE = packed record
    DR_Min: ULONG;     // minimum DMA port in the range
    DR_Max: ULONG;     // maximum DMA port in the range
    DR_Flags: ULONG;   // flags describing the range (fDD flags)
  end;
  {$EXTERNALSYM DMA_RANGE}

  //
  // DMA_DES structure
  //
  PDMA_DES = ^DMA_DES;
  {$EXTERNALSYM PDMA_DES}
  DMA_DES = packed record
    DD_Count: DWORD;       // number of DMA_RANGE structs in DMA_RESOURCE
    DD_Type: DWORD;        // size (in bytes) of DMA_RANGE struct (DType_Range)
    DD_Flags: DWORD;       // Flags describing DMA channel (fDD flags)
    DD_Alloc_Chan: ULONG;  // Specifies the DMA channel that was allocated
  end;
  {$EXTERNALSYM DMA_DES}

  //
  // DMA_RESOURCE
  //
  PDMA_RESOURCE = ^DMA_RESOURCE;
  {$EXTERNALSYM PDMA_RESOURCE}
  DMA_RESOURCE = packed record
    DMA_Header: DMA_DES;                  // info about DMA channel range list
    DMA_Data: array [0..0] of DMA_RANGE;  // list of DMA ranges
  end;
  {$EXTERNALSYM DMA_RESOURCE}

const
  //
  // Define the size of each range structure
  //
  DType_Range = SizeOf(DMA_RANGE);
  {$EXTERNALSYM DType_Range}

//--------------------------------------------------------------
// Interrupt Resource
//--------------------------------------------------------------

const
  //
  // Define the attribute flags for an interrupt resource range.  Each bit flag
  // is identified with a constant bitmask.  Following the bitmask definition
  // are the possible values.
  //
  mIRQD_Share        = $1; // Bitmask,whether the IRQ may be shared:
  {$EXTERNALSYM mIRQD_Share}
  fIRQD_Exclusive    = $0; //   The IRQ may not be shared
  {$EXTERNALSYM fIRQD_Exclusive}
  fIRQD_Share        = $1; //   The IRQ may be shared
  {$EXTERNALSYM fIRQD_Share}

  fIRQD_Share_Bit    = 0;     // compatibility
  {$EXTERNALSYM fIRQD_Share_Bit}
  fIRQD_Level_Bit    = 1;     // compatibility
  {$EXTERNALSYM fIRQD_Level_Bit}

  //
  // ** NOTE: 16-bit ConfigMgr uses fIRQD_Level_Bit being set to indicate that the
  // ** interrupt is _level-sensitive_.  For 32-bit ConfigMgr, if this bit is set,
  // ** then the interrupt is _edge-sensitive_.
  //
  mIRQD_Edge_Level   = $2; // Bitmask,whether edge or level triggered:
  {$EXTERNALSYM mIRQD_Edge_Level}
  fIRQD_Level        = $0; //   The IRQ is level-sensitive
  {$EXTERNALSYM fIRQD_Level}
  fIRQD_Edge         = $2; //   The IRQ is edge-sensitive
  {$EXTERNALSYM fIRQD_Edge}

type
  //
  // IRQ_RANGE
  //
  PIRQ_RANGE = ^IRQ_RANGE;
  {$EXTERNALSYM PIRQ_RANGE}
  IRQ_RANGE = packed record
    IRQR_Min: ULONG;      // minimum IRQ in the range
    IRQR_Max: ULONG;      // maximum IRQ in the range
    IRQR_Flags: ULONG;    // flags describing the range (fIRQD flags)
  end;
  {$EXTERNALSYM IRQ_RANGE}

  //
  // IRQ_DES structure
  //
  PIRQ_DES_32 = ^IRQ_DES_32;
  {$EXTERNALSYM PIRQ_DES_32}
  IRQ_DES_32 = packed record
    IRQD_Count: DWORD;       // number of IRQ_RANGE structs in IRQ_RESOURCE
    IRQD_Type: DWORD;        // size (in bytes) of IRQ_RANGE (IRQType_Range)
    IRQD_Flags: DWORD;       // flags describing the IRQ (fIRQD flags)
    IRQD_Alloc_Num: ULONG;   // specifies the IRQ that was allocated
    IRQD_Affinity: ULONG32;
  end;
  {$EXTERNALSYM IRQ_DES_32}

  PIRQ_DES_64 = ^IRQ_DES_64;
  {$EXTERNALSYM PIRQ_DES_64}
  IRQ_DES_64 = packed record
    IRQD_Count: DWORD;       // number of IRQ_RANGE structs in IRQ_RESOURCE
    IRQD_Type: DWORD;        // size (in bytes) of IRQ_RANGE (IRQType_Range)
    IRQD_Flags: DWORD;       // flags describing the IRQ (fIRQD flags)
    IRQD_Alloc_Num: ULONG;   // specifies the IRQ that was allocated
    IRQD_Affinity: ULONG64;
  end;
  {$EXTERNALSYM IRQ_DES_64}

  {$IFDEF _WIN64}
  IRQ_DES = IRQ_DES_64;
  PIRQ_DES = PIRQ_DES_64;
  {$ELSE}
  IRQ_DES = IRQ_DES_32;
  PIRQ_DES = PIRQ_DES_32;
  {$ENDIF UNICODE}
  {$EXTERNALSYM IRQ_DES}
  {$EXTERNALSYM PIRQ_DES}

  //
  // IRQ_RESOURCE structure
  //
  PIRQ_RESOURCE_32 = ^IRQ_RESOURCE_32;
  {$EXTERNALSYM PIRQ_RESOURCE_32}
  IRQ_RESOURCE_32 = packed record
    IRQ_Header: IRQ_DES_32;               // info about IRQ range list
    IRQ_Data: array [0..0] of IRQ_RANGE;  // list of IRQ ranges
  end;
  {$EXTERNALSYM IRQ_RESOURCE_32}

  PIRQ_RESOURCE_64 = ^IRQ_RESOURCE_64;
  {$EXTERNALSYM PIRQ_RESOURCE_64}
  IRQ_RESOURCE_64 = packed record
    IRQ_Header: IRQ_DES_64;               // info about IRQ range list
    IRQ_Data: array [0..0] of IRQ_RANGE;  // list of IRQ ranges
  end;
  {$EXTERNALSYM IRQ_RESOURCE_64}

  {$IFDEF _WIN64}
  IRQ_RESOURCE = IRQ_RESOURCE_64;
  PIRQ_RESOURCE = PIRQ_RESOURCE_64;
  {$ELSE}
  IRQ_RESOURCE = IRQ_RESOURCE_32;
  PIRQ_RESOURCE = PIRQ_RESOURCE_32;
  {$ENDIF _WIN64}
  {$EXTERNALSYM IRQ_RESOURCE}
  {$EXTERNALSYM PIRQ_RESOURCE}

const
  //
  // Define the size of each range structure
  //
  IRQType_Range = SizeOf(IRQ_RANGE);
  {$EXTERNALSYM IRQType_Range}

  //
  // Flags for resource descriptor APIs indicating the width of certain
  // variable-size resource descriptor structure fields, where applicable.
  //
  CM_RESDES_WIDTH_DEFAULT = $00000000;  // 32 or 64-bit IRQ_RESOURCE / IRQ_DES, based on client
  {$EXTERNALSYM CM_RESDES_WIDTH_DEFAULT}
  CM_RESDES_WIDTH_32      = $00000001;  // 32-bit IRQ_RESOURCE / IRQ_DES
  {$EXTERNALSYM CM_RESDES_WIDTH_32}
  CM_RESDES_WIDTH_64      = $00000002;  // 64-bit IRQ_RESOURCE / IRQ_DES
  {$EXTERNALSYM CM_RESDES_WIDTH_64}
  CM_RESDES_WIDTH_BITS    = $00000003;
  {$EXTERNALSYM CM_RESDES_WIDTH_BITS}

//--------------------------------------------------------------
// Device Private Resource
//--------------------------------------------------------------

type
  //
  // DEVICEPRIVATE_RANGE structure
  //
  PDEVPRIVATE_RANGE = ^DEVPRIVATE_RANGE;
  {$EXTERNALSYM PDEVPRIVATE_RANGE}
  DEVPRIVATE_RANGE = packed record
    PR_Data1: DWORD;     // mask for base alignment
    PR_Data2: DWORD;     // number of bytes
    PR_Data3: DWORD;     // minimum address
  end;
  {$EXTERNALSYM DEVPRIVATE_RANGE}

  //
  // DEVPRIVATE_DES structure
  //
  PDEVPRIVATE_DES = ^DEVPRIVATE_DES;
  {$EXTERNALSYM PDEVPRIVATE_DES}
  DEVPRIVATE_DES = packed record
    PD_Count: DWORD;
    PD_Type: DWORD;
    PD_Data1: DWORD;
    PD_Data2: DWORD;
    PD_Data3: DWORD;
    PD_Flags: DWORD;
  end;
  {$EXTERNALSYM DEVPRIVATE_DES}

  //
  // DEVPRIVATE_RESOURCE
  //
  PDEVPRIVATE_RESOURCE = ^DEVPRIVATE_RESOURCE;
  {$EXTERNALSYM PDEVPRIVATE_RESOURCE}
  DEVPRIVATE_RESOURCE = packed record
    PRV_Header: DEVPRIVATE_DES;
    PRV_Data: array [0..0] of DEVPRIVATE_RANGE;
  end;
  {$EXTERNALSYM DEVPRIVATE_RESOURCE}

const
  //
  // Define the size of each range structure
  //
  PType_Range = SizeOf(DEVPRIVATE_RANGE);
  {$EXTERNALSYM PType_Range}

//--------------------------------------------------------------
// Class-Specific Resource
//--------------------------------------------------------------

type
  PCS_DES = ^CS_DES;
  {$EXTERNALSYM PCS_DES}
  CS_DES = packed record
    CSD_SignatureLength: DWORD;
    CSD_LegacyDataOffset: DWORD;
    CSD_LegacyDataSize: DWORD;
    CSD_Flags: DWORD;
    CSD_ClassGuid: TGUID;
    CSD_Signature: array [0..0] of Byte;
  end;
  {$EXTERNALSYM CS_DES}

  PCS_RESOURCE = ^CS_RESOURCE;
  {$EXTERNALSYM PCS_RESOURCE}
  CS_RESOURCE = packed record
    CS_Header: CS_DES;
  end;
  {$EXTERNALSYM CS_RESOURCE}

//--------------------------------------------------------------
// PC Card Configuration Resource
//--------------------------------------------------------------

const
  //
  // Define the attribute flags for a PC Card configuration resource descriptor.
  // Each bit flag is identified with a constant bitmask.  Following the bitmask
  // definition are the possible values.
  //
  mPCD_IO_8_16        = $1;   // Bitmask, whether I/O is 8 or 16 bits
  {$EXTERNALSYM mPCD_IO_8_16}
  fPCD_IO_8           = $0;   // I/O is 8-bit
  {$EXTERNALSYM fPCD_IO_8}
  fPCD_IO_16          = $1;   // I/O is 16-bit
  {$EXTERNALSYM fPCD_IO_16}
  mPCD_MEM_8_16       = $2;   // Bitmask, whether MEM is 8 or 16 bits
  {$EXTERNALSYM mPCD_MEM_8_16}
  fPCD_MEM_8          = $0;   // MEM is 8-bit
  {$EXTERNALSYM fPCD_MEM_8}
  fPCD_MEM_16         = $2;   // MEM is 16-bit
  {$EXTERNALSYM fPCD_MEM_16}
  mPCD_MEM_A_C        = $C;   // Bitmask, whether MEMx is Attribute or Common
  {$EXTERNALSYM mPCD_MEM_A_C}
  fPCD_MEM1_A         = $4;   // MEM1 is Attribute
  {$EXTERNALSYM fPCD_MEM1_A}
  fPCD_MEM2_A         = $8;   // MEM2 is Attribute
  {$EXTERNALSYM fPCD_MEM2_A}
  fPCD_IO_ZW_8        = $10;  // zero wait on 8 bit I/O
  {$EXTERNALSYM fPCD_IO_ZW_8}
  fPCD_IO_SRC_16      = $20;  // iosrc 16
  {$EXTERNALSYM fPCD_IO_SRC_16}
  fPCD_IO_WS_16       = $40;  // wait states on 16 bit io
  {$EXTERNALSYM fPCD_IO_WS_16}
  mPCD_MEM_WS         = $300; // Bitmask, for additional wait states on memory windows
  {$EXTERNALSYM mPCD_MEM_WS}
  fPCD_MEM_WS_ONE     = $100; // 1 wait state
  {$EXTERNALSYM fPCD_MEM_WS_ONE}
  fPCD_MEM_WS_TWO     = $200; // 2 wait states
  {$EXTERNALSYM fPCD_MEM_WS_TWO}
  fPCD_MEM_WS_THREE   = $300; // 3 wait states
  {$EXTERNALSYM fPCD_MEM_WS_THREE}

  fPCD_MEM_A          = $4;   // MEM is Attribute
  {$EXTERNALSYM fPCD_MEM_A}

  fPCD_ATTRIBUTES_PER_WINDOW = $8000;
  {$EXTERNALSYM fPCD_ATTRIBUTES_PER_WINDOW}

  fPCD_IO1_16         = $00010000;  // I/O window 1 is 16-bit
  {$EXTERNALSYM fPCD_IO1_16}
  fPCD_IO1_ZW_8       = $00020000;  // I/O window 1 zero wait on 8 bit I/O
  {$EXTERNALSYM fPCD_IO1_ZW_8}
  fPCD_IO1_SRC_16     = $00040000;  // I/O window 1 iosrc 16
  {$EXTERNALSYM fPCD_IO1_SRC_16}
  fPCD_IO1_WS_16      = $00080000;  // I/O window 1 wait states on 16 bit io
  {$EXTERNALSYM fPCD_IO1_WS_16}

  fPCD_IO2_16         = $00100000;  // I/O window 2 is 16-bit
  {$EXTERNALSYM fPCD_IO2_16}
  fPCD_IO2_ZW_8       = $00200000;  // I/O window 2 zero wait on 8 bit I/O
  {$EXTERNALSYM fPCD_IO2_ZW_8}
  fPCD_IO2_SRC_16     = $00400000;  // I/O window 2 iosrc 16
  {$EXTERNALSYM fPCD_IO2_SRC_16}
  fPCD_IO2_WS_16      = $00800000;  // I/O window 2 wait states on 16 bit io
  {$EXTERNALSYM fPCD_IO2_WS_16}

  mPCD_MEM1_WS        = $03000000;  // MEM window 1 Bitmask, for additional wait states on memory windows
  {$EXTERNALSYM mPCD_MEM1_WS}
  fPCD_MEM1_WS_ONE    = $01000000;  // MEM window 1, 1 wait state
  {$EXTERNALSYM fPCD_MEM1_WS_ONE}
  fPCD_MEM1_WS_TWO    = $02000000;  // MEM window 1, 2 wait states
  {$EXTERNALSYM fPCD_MEM1_WS_TWO}
  fPCD_MEM1_WS_THREE  = $03000000;  // MEM window 1, 3 wait states
  {$EXTERNALSYM fPCD_MEM1_WS_THREE}
  fPCD_MEM1_16        = $04000000;  // MEM window 1 is 16-bit
  {$EXTERNALSYM fPCD_MEM1_16}

  mPCD_MEM2_WS        = $30000000;  // MEM window 2 Bitmask, for additional wait states on memory windows
  {$EXTERNALSYM mPCD_MEM2_WS}
  fPCD_MEM2_WS_ONE    = $10000000;  // MEM window 2, 1 wait state
  {$EXTERNALSYM fPCD_MEM2_WS_ONE}
  fPCD_MEM2_WS_TWO    = $20000000;  // MEM window 2, 2 wait states
  {$EXTERNALSYM fPCD_MEM2_WS_TWO}
  fPCD_MEM2_WS_THREE  = $30000000;  // MEM window 2, 3 wait states
  {$EXTERNALSYM fPCD_MEM2_WS_THREE}
  fPCD_MEM2_16        = $40000000;  // MEM window 2 is 16-bit
  {$EXTERNALSYM fPCD_MEM2_16}

  PCD_MAX_MEMORY   = 2;
  {$EXTERNALSYM PCD_MAX_MEMORY}
  PCD_MAX_IO       = 2;
  {$EXTERNALSYM PCD_MAX_IO}

type
  PPCCARD_DES = ^PCCARD_DES;
  {$EXTERNALSYM PPCCARD_DES}
  PCCARD_DES = packed record
    PCD_Count: DWORD;
    PCD_Type: DWORD;
    PCD_Flags: DWORD;
    PCD_ConfigIndex: Byte;
    PCD_Reserved: array [0..2] of Byte;
    PCD_MemoryCardBase1: DWORD;
    PCD_MemoryCardBase2: DWORD;
    PCD_MemoryCardBase: array [0..PCD_MAX_MEMORY-1] of DWORD; // will soon be removed
    PCD_MemoryFlags: array [0..PCD_MAX_MEMORY-1] of WORD;     // will soon be removed
    PCD_IoFlags: array [0..PCD_MAX_IO-1] of Byte;             // will soon be removed
  end;
  {$EXTERNALSYM PCCARD_DES}

  PPCCARD_RESOURCE = ^PCCARD_RESOURCE;
  {$EXTERNALSYM PPCCARD_RESOURCE}
  PCCARD_RESOURCE = packed record
    PcCard_Header: PCCARD_DES;
  end;
  {$EXTERNALSYM PCCARD_RESOURCE}

//--------------------------------------------------------------
// MF (multifunction) PCCard Configuration Resource
//--------------------------------------------------------------

const
  mPMF_AUDIO_ENABLE   = $8;   // Bitmask, whether audio is enabled or not
  {$EXTERNALSYM mPMF_AUDIO_ENABLE}
  fPMF_AUDIO_ENABLE   = $8;   // Audio is enabled
  {$EXTERNALSYM fPMF_AUDIO_ENABLE}

type
  PMFCARD_DES = ^MFCARD_DES;
  {$EXTERNALSYM PMFCARD_DES}
  MFCARD_DES = packed record
    PMF_Count: DWORD;
    PMF_Type: DWORD;
    PMF_Flags: DWORD;
    PMF_ConfigOptions: Byte;
    PMF_IoResourceIndex: Byte;
    PMF_Reserved: array [0..1] of Byte;
    PMF_ConfigRegisterBase: DWORD;
  end;
  {$EXTERNALSYM MFCARD_DES}

  PMFCARD_RESOURCE = ^MFCARD_RESOURCE;
  {$EXTERNALSYM PMFCARD_RESOURCE}
  MFCARD_RESOURCE = packed record
    MfCard_Header: MFCARD_DES;
  end;
  {$EXTERNALSYM MFCARD_RESOURCE}

//--------------------------------------------------------------
// Bus Number Resource
//--------------------------------------------------------------

//
// Define the attribute flags for a Bus Number resource descriptor.
// Each bit flag is identified with a constant bitmask.  Following the bitmask
// definition are the possible values.
//
// Currently unused.
//

  //
  // BUSNUMBER_RANGE
  //
  PBUSNUMBER_RANGE = ^BUSNUMBER_RANGE;
  {$EXTERNALSYM PBUSNUMBER_RANGE}
  BUSNUMBER_RANGE = packed record
    BUSR_Min: ULONG;          // minimum Bus Number in the range
    BUSR_Max: ULONG;          // maximum Bus Number in the range
    BUSR_nBusNumbers: ULONG;  // specifies number of buses required
    BUSR_Flags: ULONG;        // flags describing the range (currently unused)
  end;
  {$EXTERNALSYM BUSNUMBER_RANGE}

  //
  // BUSNUMBER_DES structure
  //
  PBUSNUMBER_DES = ^BUSNUMBER_DES;
  {$EXTERNALSYM PBUSNUMBER_DES}
  BUSNUMBER_DES = packed record
    BUSD_Count: DWORD;       // number of BUSNUMBER_RANGE structs in BUSNUMBER_RESOURCE
    BUSD_Type: DWORD;        // size (in bytes) of BUSNUMBER_RANGE (BusNumberType_Range)
    BUSD_Flags: DWORD;       // flags describing the range (currently unused)
    BUSD_Alloc_Base: ULONG;  // specifies the first Bus that was allocated
    BUSD_Alloc_End: ULONG;   // specifies the last Bus number that was allocated
  end;
  {$EXTERNALSYM BUSNUMBER_DES}

  //
  // BUSNUMBER_RESOURCE structure
  //
  PBUSNUMBER_RESOURCE = ^BUSNUMBER_RESOURCE;
  {$EXTERNALSYM PBUSNUMBER_RESOURCE}
  BUSNUMBER_RESOURCE = packed record
    BusNumber_Header: BUSNUMBER_DES;                  // info about Bus Number range list
    BusNumber_Data: array [0..0] of BUSNUMBER_RANGE;  // list of Bus Number ranges
  end;
  {$EXTERNALSYM BUSNUMBER_RESOURCE}

const
  //
  // Define the size of each range structure
  //
  BusNumberType_Range = SizeOf(BUSNUMBER_RANGE);
  {$EXTERNALSYM BusNumberType_Range}

//--------------------------------------------------------------
// Hardware Profile Information
//--------------------------------------------------------------

const
  //
  // Define flags relating to hardware profiles
  //
  CM_HWPI_NOT_DOCKABLE  = $00000000;   // machine is not dockable
  {$EXTERNALSYM CM_HWPI_NOT_DOCKABLE}
  CM_HWPI_UNDOCKED      = $00000001;   // hw profile for docked config
  {$EXTERNALSYM CM_HWPI_UNDOCKED}
  CM_HWPI_DOCKED        = $00000002;   // hw profile for undocked config
  {$EXTERNALSYM CM_HWPI_DOCKED}

type
  //
  // HWPROFILEINFO structure
  //
  PHWPROFILEINFO_A = ^HWPROFILEINFO_A;
  {$EXTERNALSYM PHWPROFILEINFO_A}
  HWPROFILEINFO_A = packed record
    HWPI_ulHWProfile: ULONG;                      // handle of hw profile
    HWPI_szFriendlyName: array [0..MAX_PROFILE_LEN-1] of Char;  // friendly name of hw profile
    HWPI_dwFlags: DWORD;                          // profile flags (CM_HWPI_*)
  end;
  {$EXTERNALSYM HWPROFILEINFO_A}

  PHWPROFILEINFO_W = ^HWPROFILEINFO_W;
  {$EXTERNALSYM PHWPROFILEINFO_W}
  HWPROFILEINFO_W = packed record
    HWPI_ulHWProfile: ULONG;                      // handle of hw profile
    HWPI_szFriendlyName: array [0..MAX_PROFILE_LEN-1] of Char;  // friendly name of hw profile
    HWPI_dwFlags: DWORD;                          // profile flags (CM_HWPI_*)
  end;
  {$EXTERNALSYM HWPROFILEINFO_W}

  {$IFDEF UNICODE}
  HWPROFILEINFO = HWPROFILEINFO_W;
  PHWPROFILEINFO = PHWPROFILEINFO_W;
  {$ELSE}
  HWPROFILEINFO = HWPROFILEINFO_A;
  PHWPROFILEINFO = PHWPROFILEINFO_A;
  {$ENDIF UNICODE}
  {$EXTERNALSYM HWPROFILEINFO}
  {$EXTERNALSYM PHWPROFILEINFO}

//--------------------------------------------------------------
// Miscellaneous
//--------------------------------------------------------------

const
  //
  // Resource types
  //
  ResType_All           = $00000000;   // Return all resource types
  {$EXTERNALSYM ResType_All}
  ResType_None          = $00000000;   // Arbitration always succeeded
  {$EXTERNALSYM ResType_None}
  ResType_Mem           = $00000001;   // Physical address resource
  {$EXTERNALSYM ResType_Mem}
  ResType_IO            = $00000002;   // Physical I/O address resource
  {$EXTERNALSYM ResType_IO}
  ResType_DMA           = $00000003;   // DMA channels resource
  {$EXTERNALSYM ResType_DMA}
  ResType_IRQ           = $00000004;   // IRQ resource
  {$EXTERNALSYM ResType_IRQ}
  ResType_DoNotUse      = $00000005;   // Used as spacer to sync subsequent ResTypes w/NT
  {$EXTERNALSYM ResType_DoNotUse}
  ResType_BusNumber     = $00000006;   // bus number resource
  {$EXTERNALSYM ResType_BusNumber}
  ResType_MAX           = $00000006;   // Maximum known (arbitrated) ResType
  {$EXTERNALSYM ResType_MAX}
  ResType_Ignored_Bit   = $00008000;   // Ignore this resource
  {$EXTERNALSYM ResType_Ignored_Bit}
  ResType_ClassSpecific = $0000FFFF;   // class-specific resource
  {$EXTERNALSYM ResType_ClassSpecific}
  ResType_Reserved      = $00008000;   // reserved for internal use
  {$EXTERNALSYM ResType_Reserved}
  ResType_DevicePrivate = $00008001;   // device private data
  {$EXTERNALSYM ResType_DevicePrivate}
  ResType_PcCardConfig  = $00008002;   // PC Card configuration data
  {$EXTERNALSYM ResType_PcCardConfig}
  ResType_MfCardConfig  = $00008003;   // MF Card configuration data
  {$EXTERNALSYM ResType_MfCardConfig}

  //
  // Flags specifying options for ranges that conflict with ranges already in
  // the range list (CM_Add_Range)
  //
  CM_ADD_RANGE_ADDIFCONFLICT        = $00000000; // merg with conflicting range
  {$EXTERNALSYM CM_ADD_RANGE_ADDIFCONFLICT}
  CM_ADD_RANGE_DONOTADDIFCONFLICT   = $00000001; // error if range conflicts
  {$EXTERNALSYM CM_ADD_RANGE_DONOTADDIFCONFLICT}
  CM_ADD_RANGE_BITS                 = $00000001;
  {$EXTERNALSYM CM_ADD_RANGE_BITS}

  //
  // Logical Config Flags (specified in call to CM_Get_First_Log_Conf
  //
  BASIC_LOG_CONF    = $00000000;  // Specifies the req list.
  {$EXTERNALSYM BASIC_LOG_CONF}
  FILTERED_LOG_CONF = $00000001;  // Specifies the filtered req list.
  {$EXTERNALSYM FILTERED_LOG_CONF}
  ALLOC_LOG_CONF    = $00000002;  // Specifies the Alloc Element.
  {$EXTERNALSYM ALLOC_LOG_CONF}
  BOOT_LOG_CONF     = $00000003;  // Specifies the RM Alloc Element.
  {$EXTERNALSYM BOOT_LOG_CONF}
  FORCED_LOG_CONF   = $00000004;  // Specifies the Forced Log Conf
  {$EXTERNALSYM FORCED_LOG_CONF}
  OVERRIDE_LOG_CONF = $00000005;  // Specifies the Override req list.
  {$EXTERNALSYM OVERRIDE_LOG_CONF}
  NUM_LOG_CONF      = $00000006;  // Number of Log Conf type
  {$EXTERNALSYM NUM_LOG_CONF}
  LOG_CONF_BITS     = $00000007;  // The bits of the log conf type.
  {$EXTERNALSYM LOG_CONF_BITS}

  PRIORITY_EQUAL_FIRST  = $00000008; // Same priority, new one first
  {$EXTERNALSYM PRIORITY_EQUAL_FIRST}
  PRIORITY_EQUAL_LAST   = $00000000; // Same priority, new one last
  {$EXTERNALSYM PRIORITY_EQUAL_LAST}
  PRIORITY_BIT          = $00000008;
  {$EXTERNALSYM PRIORITY_BIT}

  //
  // Registry disposition values
  // (specified in call to CM_Open_DevNode_Key and CM_Open_Class_Key)
  //
  RegDisposition_OpenAlways   = $00000000;   // open if exists else create
  {$EXTERNALSYM RegDisposition_OpenAlways}
  RegDisposition_OpenExisting = $00000001;   // open key only if exists
  {$EXTERNALSYM RegDisposition_OpenExisting}
  RegDisposition_Bits         = $00000001;
  {$EXTERNALSYM RegDisposition_Bits}

  //
  // ulFlags values for CM API routines
  //

  //
  // Flags for CM_Add_ID
  //
  CM_ADD_ID_HARDWARE   = $00000000;
  {$EXTERNALSYM CM_ADD_ID_HARDWARE}
  CM_ADD_ID_COMPATIBLE = $00000001;
  {$EXTERNALSYM CM_ADD_ID_COMPATIBLE}
  CM_ADD_ID_BITS       = $00000001;
  {$EXTERNALSYM CM_ADD_ID_BITS}

  //
  // Device Node creation flags
  //
  CM_CREATE_DEVNODE_NORMAL          = $00000000;   // install later
  {$EXTERNALSYM CM_CREATE_DEVNODE_NORMAL}
  CM_CREATE_DEVNODE_NO_WAIT_INSTALL = $00000001;   // NOT SUPPORTED ON NT
  {$EXTERNALSYM CM_CREATE_DEVNODE_NO_WAIT_INSTALL}
  CM_CREATE_DEVNODE_PHANTOM         = $00000002;
  {$EXTERNALSYM CM_CREATE_DEVNODE_PHANTOM}
  CM_CREATE_DEVNODE_GENERATE_ID     = $00000004;
  {$EXTERNALSYM CM_CREATE_DEVNODE_GENERATE_ID}
  CM_CREATE_DEVNODE_DO_NOT_INSTALL  = $00000008;
  {$EXTERNALSYM CM_CREATE_DEVNODE_DO_NOT_INSTALL}
  CM_CREATE_DEVNODE_BITS            = $0000000F;
  {$EXTERNALSYM CM_CREATE_DEVNODE_BITS}

  CM_CREATE_DEVINST_NORMAL          = CM_CREATE_DEVNODE_NORMAL;
  {$EXTERNALSYM CM_CREATE_DEVINST_NORMAL}
  CM_CREATE_DEVINST_NO_WAIT_INSTALL = CM_CREATE_DEVNODE_NO_WAIT_INSTALL;
  {$EXTERNALSYM CM_CREATE_DEVINST_NO_WAIT_INSTALL}
  CM_CREATE_DEVINST_PHANTOM         = CM_CREATE_DEVNODE_PHANTOM;
  {$EXTERNALSYM CM_CREATE_DEVINST_PHANTOM}
  CM_CREATE_DEVINST_GENERATE_ID     = CM_CREATE_DEVNODE_GENERATE_ID;
  {$EXTERNALSYM CM_CREATE_DEVINST_GENERATE_ID}
  CM_CREATE_DEVINST_DO_NOT_INSTALL  = CM_CREATE_DEVNODE_DO_NOT_INSTALL;
  {$EXTERNALSYM CM_CREATE_DEVINST_DO_NOT_INSTALL}
  CM_CREATE_DEVINST_BITS            = CM_CREATE_DEVNODE_BITS;
  {$EXTERNALSYM CM_CREATE_DEVINST_BITS}

  //
  // Flags for CM_Delete_Class_Key
  //
  CM_DELETE_CLASS_ONLY        = $00000000;
  {$EXTERNALSYM CM_DELETE_CLASS_ONLY}
  CM_DELETE_CLASS_SUBKEYS     = $00000001;
  {$EXTERNALSYM CM_DELETE_CLASS_SUBKEYS}
  CM_DELETE_CLASS_BITS        = $00000001;
  {$EXTERNALSYM CM_DELETE_CLASS_BITS}

  //
  // Detection reason flags (specified in call to CM_Run_Detection)
  //
  CM_DETECT_NEW_PROFILE       = $00000001; // detection for new hw profile
  {$EXTERNALSYM CM_DETECT_NEW_PROFILE}
  CM_DETECT_CRASHED           = $00000002; // Previous detection crashed
  {$EXTERNALSYM CM_DETECT_CRASHED}
  CM_DETECT_HWPROF_FIRST_BOOT = $00000004;
  {$EXTERNALSYM CM_DETECT_HWPROF_FIRST_BOOT}
  CM_DETECT_RUN               = $80000000;
  {$EXTERNALSYM CM_DETECT_RUN}
  CM_DETECT_BITS              = $80000007;
  {$EXTERNALSYM CM_DETECT_BITS}

  CM_DISABLE_POLITE           = $00000000;    // Ask the driver
  {$EXTERNALSYM CM_DISABLE_POLITE}
  CM_DISABLE_ABSOLUTE         = $00000001;    // Don't ask the driver
  {$EXTERNALSYM CM_DISABLE_ABSOLUTE}
  CM_DISABLE_HARDWARE         = $00000002;    // Don't ask the driver, and won't be restarteable
  {$EXTERNALSYM CM_DISABLE_HARDWARE}
  CM_DISABLE_UI_NOT_OK        = $00000004;    // Don't popup any veto API
  {$EXTERNALSYM CM_DISABLE_UI_NOT_OK}
  CM_DISABLE_BITS             = $00000007;    // The bits for the disable function
  {$EXTERNALSYM CM_DISABLE_BITS}

  //
  // Flags for CM_Get_Device_ID_List, CM_Get_Device_ID_List_Size
  //
  CM_GETIDLIST_FILTER_NONE                = $00000000;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_NONE}
  CM_GETIDLIST_FILTER_ENUMERATOR          = $00000001;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_ENUMERATOR}
  CM_GETIDLIST_FILTER_SERVICE             = $00000002;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_SERVICE}
  CM_GETIDLIST_FILTER_EJECTRELATIONS      = $00000004;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_EJECTRELATIONS}
  CM_GETIDLIST_FILTER_REMOVALRELATIONS    = $00000008;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_REMOVALRELATIONS}
  CM_GETIDLIST_FILTER_POWERRELATIONS      = $00000010;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_POWERRELATIONS}
  CM_GETIDLIST_FILTER_BUSRELATIONS        = $00000020;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_BUSRELATIONS}
  CM_GETIDLIST_DONOTGENERATE              = $10000040;
  {$EXTERNALSYM CM_GETIDLIST_DONOTGENERATE}
  CM_GETIDLIST_FILTER_BITS                = $1000007F;
  {$EXTERNALSYM CM_GETIDLIST_FILTER_BITS}

  //
  // Flags for CM_Get_Device_Interface_List, CM_Get_Device_Interface_List_Size
  //
  CM_GET_DEVICE_INTERFACE_LIST_PRESENT     = $00000000;  // only currently 'live' device interfaces
  {$EXTERNALSYM CM_GET_DEVICE_INTERFACE_LIST_PRESENT}
  CM_GET_DEVICE_INTERFACE_LIST_ALL_DEVICES = $00000001;  // all registered device interfaces, live or not
  {$EXTERNALSYM CM_GET_DEVICE_INTERFACE_LIST_ALL_DEVICES}
  CM_GET_DEVICE_INTERFACE_LIST_BITS        = $00000001;
  {$EXTERNALSYM CM_GET_DEVICE_INTERFACE_LIST_BITS}

  //
  // Registry properties (specified in call to CM_Get_DevInst_Registry_Property or CM_Get_Class_Registry_Property,
  // some are allowed in calls to CM_Set_DevInst_Registry_Property and CM_Set_Class_Registry_Property)
  // CM_DRP_xxxx values should be used for CM_Get_DevInst_Registry_Property / CM_Set_DevInst_Registry_Property
  // CM_CRP_xxxx values should be used for CM_Get_Class_Registry_Property / CM_Set_Class_Registry_Property
  // DRP/CRP values that overlap must have a 1:1 correspondence with each other
  //
  CM_DRP_DEVICEDESC                  = $00000001; // DeviceDesc REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_DEVICEDESC}
  CM_DRP_HARDWAREID                  = $00000002; // HardwareID REG_MULTI_SZ property (RW)
  {$EXTERNALSYM CM_DRP_HARDWAREID}
  CM_DRP_COMPATIBLEIDS               = $00000003; // CompatibleIDs REG_MULTI_SZ property (RW)
  {$EXTERNALSYM CM_DRP_COMPATIBLEIDS}
  CM_DRP_UNUSED0                     = $00000004; // unused
  {$EXTERNALSYM CM_DRP_UNUSED0}
  CM_DRP_SERVICE                     = $00000005; // Service REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_SERVICE}
  CM_DRP_UNUSED1                     = $00000006; // unused
  {$EXTERNALSYM CM_DRP_UNUSED1}
  CM_DRP_UNUSED2                     = $00000007; // unused
  {$EXTERNALSYM CM_DRP_UNUSED2}
  CM_DRP_CLASS                       = $00000008; // Class REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_CLASS}
  CM_DRP_CLASSGUID                   = $00000009; // ClassGUID REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_CLASSGUID}
  CM_DRP_DRIVER                      = $0000000A; // Driver REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_DRIVER}
  CM_DRP_CONFIGFLAGS                 = $0000000B; // ConfigFlags REG_DWORD property (RW)
  {$EXTERNALSYM CM_DRP_CONFIGFLAGS}
  CM_DRP_MFG                         = $0000000C; // Mfg REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_MFG}
  CM_DRP_FRIENDLYNAME                = $0000000D; // FriendlyName REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_FRIENDLYNAME}
  CM_DRP_LOCATION_INFORMATION        = $0000000E; // LocationInformation REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_LOCATION_INFORMATION}
  CM_DRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000F; // PhysicalDeviceObjectName REG_SZ property (R)
  {$EXTERNALSYM CM_DRP_PHYSICAL_DEVICE_OBJECT_NAME}
  CM_DRP_CAPABILITIES                = $00000010; // Capabilities REG_DWORD property (R)
  {$EXTERNALSYM CM_DRP_CAPABILITIES}
  CM_DRP_UI_NUMBER                   = $00000011; // UiNumber REG_DWORD property (R)
  {$EXTERNALSYM CM_DRP_UI_NUMBER}
  CM_DRP_UPPERFILTERS                = $00000012; // UpperFilters REG_MULTI_SZ property (RW)
  {$EXTERNALSYM CM_DRP_UPPERFILTERS}
  CM_DRP_LOWERFILTERS                = $00000013; // LowerFilters REG_MULTI_SZ property (RW)
  {$EXTERNALSYM CM_DRP_LOWERFILTERS}
  CM_DRP_BUSTYPEGUID                 = $00000014; // Bus Type Guid, GUID, (R)
  {$EXTERNALSYM CM_DRP_BUSTYPEGUID}
  CM_DRP_LEGACYBUSTYPE               = $00000015; // Legacy bus type, INTERFACE_TYPE, (R)
  {$EXTERNALSYM CM_DRP_LEGACYBUSTYPE}
  CM_DRP_BUSNUMBER                   = $00000016; // Bus Number, DWORD, (R)
  {$EXTERNALSYM CM_DRP_BUSNUMBER}
  CM_DRP_ENUMERATOR_NAME             = $00000017; // Enumerator Name REG_SZ property (R)
  {$EXTERNALSYM CM_DRP_ENUMERATOR_NAME}
  CM_DRP_SECURITY                    = $00000018; // Security - Device override (RW)
  {$EXTERNALSYM CM_DRP_SECURITY}
  CM_CRP_SECURITY                    = CM_DRP_SECURITY;   // Class default security (RW)
  {$EXTERNALSYM CM_CRP_SECURITY}
  CM_DRP_SECURITY_SDS                = $00000019; // Security - Device override (RW)
  {$EXTERNALSYM CM_DRP_SECURITY_SDS}
  CM_CRP_SECURITY_SDS                = CM_DRP_SECURITY_SDS; // Class default security (RW)
  {$EXTERNALSYM CM_CRP_SECURITY_SDS}
  CM_DRP_DEVTYPE                     = $0000001A; // Device Type - Device override (RW)
  {$EXTERNALSYM CM_DRP_DEVTYPE}
  CM_CRP_DEVTYPE                     = CM_DRP_DEVTYPE;    // Class default Device-type (RW)
  {$EXTERNALSYM CM_CRP_DEVTYPE}
  CM_DRP_EXCLUSIVE                   = $0000001B; // Exclusivity - Device override (RW)
  {$EXTERNALSYM CM_DRP_EXCLUSIVE}
  CM_CRP_EXCLUSIVE                   = CM_DRP_EXCLUSIVE;  // Class default (RW)
  {$EXTERNALSYM CM_CRP_EXCLUSIVE}
  CM_DRP_CHARACTERISTICS             = $0000001C; // Characteristics - Device Override (RW)
  {$EXTERNALSYM CM_DRP_CHARACTERISTICS}
  CM_CRP_CHARACTERISTICS             = CM_DRP_CHARACTERISTICS;  // Class default (RW)
  {$EXTERNALSYM CM_CRP_CHARACTERISTICS}
  CM_DRP_ADDRESS                     = $0000001D; // Device Address (R)
  {$EXTERNALSYM CM_DRP_ADDRESS}
  CM_DRP_UI_NUMBER_DESC_FORMAT       = $0000001E; // UINumberDescFormat REG_SZ property (RW)
  {$EXTERNALSYM CM_DRP_UI_NUMBER_DESC_FORMAT}
  CM_DRP_DEVICE_POWER_DATA           = $0000001F; // CM_POWER_DATA REG_BINARY property (R)
  {$EXTERNALSYM CM_DRP_DEVICE_POWER_DATA}
  CM_DRP_REMOVAL_POLICY              = $00000020; // CM_DEVICE_REMOVAL_POLICY REG_DWORD (R)
  {$EXTERNALSYM CM_DRP_REMOVAL_POLICY}
  CM_DRP_REMOVAL_POLICY_HW_DEFAULT   = $00000021; // CM_DRP_REMOVAL_POLICY_HW_DEFAULT REG_DWORD (R)
  {$EXTERNALSYM CM_DRP_REMOVAL_POLICY_HW_DEFAULT}
  CM_DRP_REMOVAL_POLICY_OVERRIDE     = $00000022; // CM_DRP_REMOVAL_POLICY_OVERRIDE REG_DWORD (RW)
  {$EXTERNALSYM CM_DRP_REMOVAL_POLICY_OVERRIDE}
  CM_DRP_INSTALL_STATE               = $00000023; // CM_DRP_INSTALL_STATE REG_DWORD (R)
  {$EXTERNALSYM CM_DRP_INSTALL_STATE}

  CM_DRP_MIN                         = $00000001; // First device register
  {$EXTERNALSYM CM_DRP_MIN}
  CM_CRP_MIN                         = CM_DRP_MIN;   // First class register
  {$EXTERNALSYM CM_CRP_MIN}
  CM_DRP_MAX                         = $00000023; // Last device register
  {$EXTERNALSYM CM_DRP_MAX}
  CM_CRP_MAX                         = CM_DRP_MAX;   // Last class register
  {$EXTERNALSYM CM_CRP_MAX}

  //
  // Capabilities bits (the capability value is returned from calling
  // CM_Get_DevInst_Registry_Property with CM_DRP_CAPABILITIES property)
  //
  CM_DEVCAP_LOCKSUPPORTED     = $00000001;
  {$EXTERNALSYM CM_DEVCAP_LOCKSUPPORTED}
  CM_DEVCAP_EJECTSUPPORTED    = $00000002;
  {$EXTERNALSYM CM_DEVCAP_EJECTSUPPORTED}
  CM_DEVCAP_REMOVABLE         = $00000004;
  {$EXTERNALSYM CM_DEVCAP_REMOVABLE}
  CM_DEVCAP_DOCKDEVICE        = $00000008;
  {$EXTERNALSYM CM_DEVCAP_DOCKDEVICE}
  CM_DEVCAP_UNIQUEID          = $00000010;
  {$EXTERNALSYM CM_DEVCAP_UNIQUEID}
  CM_DEVCAP_SILENTINSTALL     = $00000020;
  {$EXTERNALSYM CM_DEVCAP_SILENTINSTALL}
  CM_DEVCAP_RAWDEVICEOK       = $00000040;
  {$EXTERNALSYM CM_DEVCAP_RAWDEVICEOK}
  CM_DEVCAP_SURPRISEREMOVALOK = $00000080;
  {$EXTERNALSYM CM_DEVCAP_SURPRISEREMOVALOK}
  CM_DEVCAP_HARDWAREDISABLED  = $00000100;
  {$EXTERNALSYM CM_DEVCAP_HARDWAREDISABLED}
  CM_DEVCAP_NONDYNAMIC        = $00000200;
  {$EXTERNALSYM CM_DEVCAP_NONDYNAMIC}

  //
  // Removal policies (retrievable via CM_Get_DevInst_Registry_Property with
  // the CM_DRP_REMOVAL_POLICY, CM_DRP_REMOVAL_POLICY_OVERRIDE, or
  // CM_DRP_REMOVAL_POLICY_HW_DEFAULT properties)
  //
  CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL             = 1;
  {$EXTERNALSYM CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL}
  CM_REMOVAL_POLICY_EXPECT_ORDERLY_REMOVAL        = 2;
  {$EXTERNALSYM CM_REMOVAL_POLICY_EXPECT_ORDERLY_REMOVAL}
  CM_REMOVAL_POLICY_EXPECT_SURPRISE_REMOVAL       = 3;
  {$EXTERNALSYM CM_REMOVAL_POLICY_EXPECT_SURPRISE_REMOVAL}

  //
  // Device install states (retrievable via CM_Get_DevInst_Registry_Property with
  // the CM_DRP_INSTALL_STATE properties)
  //
  CM_INSTALL_STATE_INSTALLED                      = 0;
  {$EXTERNALSYM CM_INSTALL_STATE_INSTALLED}
  CM_INSTALL_STATE_NEEDS_REINSTALL                = 1;
  {$EXTERNALSYM CM_INSTALL_STATE_NEEDS_REINSTALL}
  CM_INSTALL_STATE_FAILED_INSTALL                 = 2;
  {$EXTERNALSYM CM_INSTALL_STATE_FAILED_INSTALL}
  CM_INSTALL_STATE_FINISH_INSTALL                 = 3;
  {$EXTERNALSYM CM_INSTALL_STATE_FINISH_INSTALL}

  //
  // Flags for CM_Locate_DevNode
  //
  CM_LOCATE_DEVNODE_NORMAL       = $00000000;
  {$EXTERNALSYM CM_LOCATE_DEVNODE_NORMAL}
  CM_LOCATE_DEVNODE_PHANTOM      = $00000001;
  {$EXTERNALSYM CM_LOCATE_DEVNODE_PHANTOM}
  CM_LOCATE_DEVNODE_CANCELREMOVE = $00000002;
  {$EXTERNALSYM CM_LOCATE_DEVNODE_CANCELREMOVE}
  CM_LOCATE_DEVNODE_NOVALIDATION = $00000004;
  {$EXTERNALSYM CM_LOCATE_DEVNODE_NOVALIDATION}
  CM_LOCATE_DEVNODE_BITS         = $00000007;
  {$EXTERNALSYM CM_LOCATE_DEVNODE_BITS}

  CM_LOCATE_DEVINST_NORMAL       = CM_LOCATE_DEVNODE_NORMAL;
  {$EXTERNALSYM CM_LOCATE_DEVINST_NORMAL}
  CM_LOCATE_DEVINST_PHANTOM      = CM_LOCATE_DEVNODE_PHANTOM;
  {$EXTERNALSYM CM_LOCATE_DEVINST_PHANTOM}
  CM_LOCATE_DEVINST_CANCELREMOVE = CM_LOCATE_DEVNODE_CANCELREMOVE;
  {$EXTERNALSYM CM_LOCATE_DEVINST_CANCELREMOVE}
  CM_LOCATE_DEVINST_NOVALIDATION = CM_LOCATE_DEVNODE_NOVALIDATION;
  {$EXTERNALSYM CM_LOCATE_DEVINST_NOVALIDATION}
  CM_LOCATE_DEVINST_BITS         = CM_LOCATE_DEVNODE_BITS;
  {$EXTERNALSYM CM_LOCATE_DEVINST_BITS}

  //
  // Flags for CM_Open_Class_Key
  //
  CM_OPEN_CLASS_KEY_INSTALLER        = $00000000;
  {$EXTERNALSYM CM_OPEN_CLASS_KEY_INSTALLER}
  CM_OPEN_CLASS_KEY_INTERFACE        = $00000001;
  {$EXTERNALSYM CM_OPEN_CLASS_KEY_INTERFACE}
  CM_OPEN_CLASS_KEY_BITS             = $00000001;
  {$EXTERNALSYM CM_OPEN_CLASS_KEY_BITS}

  //
  // Flags for CM_Query_And_Remove_SubTree
  //
  CM_REMOVE_UI_OK             = $00000000;
  {$EXTERNALSYM CM_REMOVE_UI_OK}
  CM_REMOVE_UI_NOT_OK         = $00000001;
  {$EXTERNALSYM CM_REMOVE_UI_NOT_OK}
  CM_REMOVE_NO_RESTART        = $00000002;
  {$EXTERNALSYM CM_REMOVE_NO_RESTART}
  CM_REMOVE_BITS              = $00000003;
  {$EXTERNALSYM CM_REMOVE_BITS}

  //
  // Backward compatibility--do not use
  // (use above CM_REMOVE_* flags instead)
  //
  CM_QUERY_REMOVE_UI_OK       = CM_REMOVE_UI_OK;
  {$EXTERNALSYM CM_QUERY_REMOVE_UI_OK}
  CM_QUERY_REMOVE_UI_NOT_OK   = CM_REMOVE_UI_NOT_OK;
  {$EXTERNALSYM CM_QUERY_REMOVE_UI_NOT_OK}
  CM_QUERY_REMOVE_BITS        = CM_QUERY_REMOVE_UI_OK or CM_QUERY_REMOVE_UI_NOT_OK;
  {$EXTERNALSYM CM_QUERY_REMOVE_BITS}

  //
  // Flags for CM_Reenumerate_DevNode
  //
  CM_REENUMERATE_NORMAL             = $00000000;
  {$EXTERNALSYM CM_REENUMERATE_NORMAL}
  CM_REENUMERATE_SYNCHRONOUS        = $00000001;
  {$EXTERNALSYM CM_REENUMERATE_SYNCHRONOUS}
  CM_REENUMERATE_RETRY_INSTALLATION = $00000002;
  {$EXTERNALSYM CM_REENUMERATE_RETRY_INSTALLATION}
  CM_REENUMERATE_ASYNCHRONOUS       = $00000004;
  {$EXTERNALSYM CM_REENUMERATE_ASYNCHRONOUS}
  CM_REENUMERATE_BITS               = $00000007;
  {$EXTERNALSYM CM_REENUMERATE_BITS}

  //
  // Flags for CM_Register_Device_Driver
  //
  CM_REGISTER_DEVICE_DRIVER_STATIC        = $00000000;
  {$EXTERNALSYM CM_REGISTER_DEVICE_DRIVER_STATIC}
  CM_REGISTER_DEVICE_DRIVER_DISABLEABLE   = $00000001;
  {$EXTERNALSYM CM_REGISTER_DEVICE_DRIVER_DISABLEABLE}
  CM_REGISTER_DEVICE_DRIVER_REMOVABLE     = $00000002;
  {$EXTERNALSYM CM_REGISTER_DEVICE_DRIVER_REMOVABLE}
  CM_REGISTER_DEVICE_DRIVER_BITS          = $00000003;
  {$EXTERNALSYM CM_REGISTER_DEVICE_DRIVER_BITS}

  //
  // Registry Branch Locations (for CM_Open_DevNode_Key)
  //
  CM_REGISTRY_HARDWARE = $00000000;
  {$EXTERNALSYM CM_REGISTRY_HARDWARE}
  CM_REGISTRY_SOFTWARE = $00000001;
  {$EXTERNALSYM CM_REGISTRY_SOFTWARE}
  CM_REGISTRY_USER     = $00000100;
  {$EXTERNALSYM CM_REGISTRY_USER}
  CM_REGISTRY_CONFIG   = $00000200;
  {$EXTERNALSYM CM_REGISTRY_CONFIG}
  CM_REGISTRY_BITS     = $00000301;
  {$EXTERNALSYM CM_REGISTRY_BITS}

  //
  // Flags for CM_Set_DevNode_Problem
  //
  CM_SET_DEVNODE_PROBLEM_NORMAL   = $00000000;  // only set problem if currently no problem
  {$EXTERNALSYM CM_SET_DEVNODE_PROBLEM_NORMAL}
  CM_SET_DEVNODE_PROBLEM_OVERRIDE = $00000001;  // override current problem with new problem
  {$EXTERNALSYM CM_SET_DEVNODE_PROBLEM_OVERRIDE}
  CM_SET_DEVNODE_PROBLEM_BITS     = $00000001;
  {$EXTERNALSYM CM_SET_DEVNODE_PROBLEM_BITS}

  CM_SET_DEVINST_PROBLEM_NORMAL   = CM_SET_DEVNODE_PROBLEM_NORMAL;
  {$EXTERNALSYM CM_SET_DEVINST_PROBLEM_NORMAL}
  CM_SET_DEVINST_PROBLEM_OVERRIDE = CM_SET_DEVNODE_PROBLEM_OVERRIDE;
  {$EXTERNALSYM CM_SET_DEVINST_PROBLEM_OVERRIDE}
  CM_SET_DEVINST_PROBLEM_BITS     = CM_SET_DEVNODE_PROBLEM_BITS;
  {$EXTERNALSYM CM_SET_DEVINST_PROBLEM_BITS}

  //
  // Flags for CM_Set_HW_Prof_Flags
  //
  CM_SET_HW_PROF_FLAGS_UI_NOT_OK = $00000001;    // Don't popup any veto UI
  {$EXTERNALSYM CM_SET_HW_PROF_FLAGS_UI_NOT_OK}
  CM_SET_HW_PROF_FLAGS_BITS      = $00000001;
  {$EXTERNALSYM CM_SET_HW_PROF_FLAGS_BITS}

  //
  // Re-enable and configuration actions (specified in call to CM_Setup_DevInst)
  //
  CM_SETUP_DEVNODE_READY   = $00000000; // Reenable problem devinst
  {$EXTERNALSYM CM_SETUP_DEVNODE_READY}
  CM_SETUP_DEVINST_READY   = CM_SETUP_DEVNODE_READY;
  {$EXTERNALSYM CM_SETUP_DEVINST_READY}
  CM_SETUP_DOWNLOAD        = $00000001; // Get info about devinst
  {$EXTERNALSYM CM_SETUP_DOWNLOAD}
  CM_SETUP_WRITE_LOG_CONFS = $00000002;
  {$EXTERNALSYM CM_SETUP_WRITE_LOG_CONFS}
  CM_SETUP_PROP_CHANGE     = $00000003;
  {$EXTERNALSYM CM_SETUP_PROP_CHANGE}
  CM_SETUP_DEVNODE_RESET   = $00000004; // Reset problem devinst without starting
  {$EXTERNALSYM CM_SETUP_DEVNODE_RESET}
  CM_SETUP_DEVINST_RESET   = CM_SETUP_DEVNODE_RESET;
  {$EXTERNALSYM CM_SETUP_DEVINST_RESET}
  CM_SETUP_BITS            = $00000007;
  {$EXTERNALSYM CM_SETUP_BITS}

  //
  // Flags for CM_Query_Arbitrator_Free_Data and
  // CM_Query_Arbitrator_Free_Data_Size.
  //
  CM_QUERY_ARBITRATOR_RAW        = $00000000;
  {$EXTERNALSYM CM_QUERY_ARBITRATOR_RAW}
  CM_QUERY_ARBITRATOR_TRANSLATED = $00000001;
  {$EXTERNALSYM CM_QUERY_ARBITRATOR_TRANSLATED}
  CM_QUERY_ARBITRATOR_BITS       = $00000001;
  {$EXTERNALSYM CM_QUERY_ARBITRATOR_BITS}

  //
  // Flags for CM_Get_DevNode_Custom_Property
  //
  CM_CUSTOMDEVPROP_MERGE_MULTISZ = $00000001;
  {$EXTERNALSYM CM_CUSTOMDEVPROP_MERGE_MULTISZ}
  CM_CUSTOMDEVPROP_BITS          = $00000001;
  {$EXTERNALSYM CM_CUSTOMDEVPROP_BITS}

//--------------------------------------------------------------
// Function prototypes
//--------------------------------------------------------------

{$IFNDEF CFGMGR32_LINKONREQUEST}

function CM_Add_Empty_Log_Conf(var lcLogConf: LOG_CONF;
  dnDevInst: DEVINST; Priority: PRIORITY; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Add_Empty_Log_Conf_Ex(var lcLogConf: LOG_CONF;
  dnDevInst: DEVINST; Priority: PRIORITY; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Add_IDA(dnDevInst: DEVINST; pszID: PAnsiChar;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Add_IDW(dnDevInst: DEVINST; pszID: PWideChar;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Add_ID(dnDevInst: DEVINST; pszID: PTSTR;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Add_ID_ExA(dnDevInst: DEVINST; pszID: PAnsiChar;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Add_ID_ExW(dnDevInst: DEVINST; pszID: PWideChar;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Add_ID_Ex(dnDevInst: DEVINST; pszID: PTSTR;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Add_Range(ullStartValue: DWORDLONG;
  ullEndValue: DWORDLONG; rlh: RANGE_LIST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Add_Res_Des(var rdResDes: RES_DES; lcLogConf: LOG_CONF;
  ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Add_Res_Des_Ex(var rdResDes: RES_DES;
  lcLogConf: LOG_CONF; ResourceID: RESOURCEID; ResourceData: Pointer;
  ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Connect_MachineA(const UNCServerName: PAnsiChar;
  var hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Connect_MachineW(const UNCServerName: PWideChar;
  var hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Connect_Machine(const UNCServerName: PTSTR;
  var hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Create_DevNodeA(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Create_DevNodeW(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Create_DevNode(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Create_DevNode_ExA(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Create_DevNode_ExW(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Create_DevNode_Ex(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Create_DevInstA(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Create_DevInstW(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Create_DevInst(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
  dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Create_DevInst_ExA(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Create_DevInst_ExW(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Create_DevInst_Ex(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
  dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Create_Range_List(var rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Delete_Class_Key(ClassGuid: PGUID; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Delete_Class_Key_Ex(ClassGuid: PGUID; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Delete_DevNode_Key(dnDevNode: DEVNODE;
  ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Delete_DevNode_Key_Ex(dnDevNode: DEVNODE;
  ulHardwareProfile: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Delete_DevInst_Key(dnDevNode: DEVINST;
  ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Delete_DevInst_Key_Ex(dnDevNode: DEVINST; ulHardwareProfile: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Delete_Range(ullStartValue: DWORDLONG; ullEndValue: DWORDLONG;
  rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Detect_Resource_Conflict(dnDevInst: DEVINST;
  ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
  var bConflictDetected: BOOL; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Detect_Resource_Conflict_Ex(dnDevInst: DEVINST;
  ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
  var bConflictDetected: BOOL; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Disable_DevNode(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Disable_DevNode_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Disable_DevInst(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Disable_DevInst_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Disconnect_Machine(hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Dup_Range_List(rlhOld: RANGE_LIST; rlhNew: RANGE_LIST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Enable_DevNode(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Enable_DevNode_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Enable_DevInst(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Enable_DevInst_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Enumerate_Classes(ulClassIndex: ULONG;
  var ClassGuid: TGUID; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Enumerate_Classes_Ex(ulClassIndex: ULONG; var ClassGuid: TGUID;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Enumerate_EnumeratorsA(ulEnumIndex: ULONG; Buffer: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Enumerate_EnumeratorsW(ulEnumIndex: ULONG; Buffer: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Enumerate_Enumerators(ulEnumIndex: ULONG; Buffer: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Enumerate_Enumerators_ExA(ulEnumIndex: ULONG; Buffer: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Enumerate_Enumerators_ExW(ulEnumIndex: ULONG; Buffer: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Enumerate_Enumerators_Ex(ulEnumIndex: ULONG; Buffer: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Find_Range(var pullStart: DWORDLONG; ullStart: DWORDLONG;
  ulLength: ULONG; ullAlignment: DWORDLONG; ullEnd: DWORDLONG;
  rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_First_Range(rlh: RANGE_LIST; var ullStart: DWORDLONG;
  var ullEnd: DWORDLONG; preElement: PRANGE_ELEMENT;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Free_Log_Conf(lcLogConfToBeFreed: LOG_CONF;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Free_Log_Conf_Ex(lcLogConfToBeFreed: LOG_CONF;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Free_Log_Conf_Handle(lcLogConf: LOG_CONF): CONFIGRET; stdcall;

function CM_Free_Range_List(rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Free_Res_Des(prdResDes: PRES_DES;
  rdResDes: RES_DES; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Free_Res_Des_Ex(prdResDes: PRES_DES; rdResDes: RES_DES;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Free_Res_Des_Handle(rdResDes: RES_DES): CONFIGRET; stdcall;

function CM_Get_Child(var dnDevInstChild: DEVINST;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Child_Ex(var dnDevInstChild: DEVINST; dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Class_NameA(ClassGuid: PGUID; Buffer: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Class_NameW(ClassGuid: PGUID; Buffer: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Class_Name(ClassGuid: PGUID; Buffer: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Class_Name_ExA(ClassGuid: PGUID; Buffer: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Name_ExW(ClassGuid: PGUID; Buffer: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Name_Ex(ClassGuid: PGUID; Buffer: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Class_Key_NameA(ClassGuid: PGUID; pszKeyName: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Class_Key_NameW(ClassGuid: PGUID; pszKeyName: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Class_Key_Name(ClassGuid: PGUID; pszKeyName: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Class_Key_Name_ExA(ClassGuid: PGUID; pszKeyName: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Key_Name_ExW(ClassGuid: PGUID; pszKeyName: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Key_Name_Ex(ClassGuid: PGUID; pszKeyName: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Depth(var ulDepth: ULONG; dnDevInst: DEVINST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Depth_Ex(var ulDepth: ULONG; dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_IDA(dnDevInst: DEVINST; Buffer: PAnsiChar;
  BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_IDW(dnDevInst: DEVINST; Buffer: PWideChar;
  BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_ID(dnDevInst: DEVINST; Buffer: PTSTR;
  BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_ID_ExA(dnDevInst: DEVINST; Buffer: PAnsiChar;
  BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_ExW(dnDevInst: DEVINST; Buffer: PWideChar;
  BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_Ex(dnDevInst: DEVINST; Buffer: PTSTR;
  BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_ID_ListA(const pszFilter: PAnsiChar;      // OPTIONAL
  Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_ID_ListW(const pszFilter: PWideChar;      // OPTIONAL
  Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_ID_List(const pszFilter: PTSTR;           // OPTIONAL
  Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_ID_List_ExA(const pszFilter: PAnsiChar;   // OPTIONAL
  Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_ExW(const pszFilter: PWideChar;   // OPTIONAL
  Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_Ex(const pszFilter: PTSTR;        // OPTIONAL
  Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_ID_List_SizeA(var ulLen: ULONG;
  const pszFilter: PAnsiChar;    // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_SizeW(var ulLen: ULONG;
  const pszFilter: PWideChar;    // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_Size(var pulLen: ULONG;
  const pszFilter: PTSTR;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_ID_List_Size_ExA(var ulLen: ULONG;
  const pszFilter: PAnsiChar;    // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_Size_ExW(var ulLen: ULONG;
  const pszFilter: PWideChar;    // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_ID_List_Size_Ex(var ulLen: ULONG;
  const pszFilter: PTSTR;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_ID_Size(var ulLen: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_ID_Size_Ex(var ulLen: ULONG; dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_DevNode_Registry_PropertyA(dnDevInst: DEVINST;
  ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevNode_Registry_PropertyW(dnDevInst: DEVINST;
  ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevNode_Registry_Property(dnDevInst: DEVINST;
  ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevNode_Registry_Property_ExA(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevNode_Registry_Property_ExW(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevNode_Registry_Property_Ex(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_DevInst_Registry_PropertyA(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevInst_Registry_PropertyW(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevInst_Registry_Property(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevInst_Registry_Property_ExA(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevInst_Registry_Property_ExW(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevInst_Registry_Property_Ex(dnDevInst: DEVINST; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFDEF WINXP_UP}

function CM_Get_DevNode_Custom_PropertyA(dnDevInst: DEVINST;
  const pszCustomPropertyName: PAnsiChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevNode_Custom_PropertyW(dnDevInst: DEVINST;
  const pszCustomPropertyName: PWideChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevNode_Custom_Property(dnDevInst: DEVINST;
  const pszCustomPropertyName: PTSTR;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevNode_Custom_Property_ExA(dnDevInst: DEVINST;
  const pszCustomPropertyName: PAnsiChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevNode_Custom_Property_ExW(dnDevInst: DEVINST;
  const pszCustomPropertyName: PWideChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevNode_Custom_Property_Ex(dnDevInst: DEVINST;
  const pszCustomPropertyName: PTSTR;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_DevInst_Custom_PropertyA(dnDevInst: DEVINST;
  const pszCustomPropertyName: PAnsiChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevInst_Custom_PropertyW(dnDevInst: DEVINST;
  const pszCustomPropertyName: PWideChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_DevInst_Custom_Property(dnDevInst: DEVINST;
  const pszCustomPropertyName: PTSTR;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevInst_Custom_Property_ExA(dnDevInst: DEVINST;
  const pszCustomPropertyName: PAnsiChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevInst_Custom_Property_ExW(dnDevInst: DEVINST;
  const pszCustomPropertyName: PWideChar;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_DevInst_Custom_Property_Ex(dnDevInst: DEVINST;
  const pszCustomPropertyName: PTSTR;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$ENDIF WINXP_UP}

function CM_Get_DevNode_Status(var ulStatus: ULONG; var ulProblemNumber: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevInst_Status(var ulStatus: ULONG; var ulProblemNumber: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_DevNode_Status_Ex(var ulStatus: ULONG; var ulProblemNumber: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_DevInst_Status_Ex(var ulStatus: ULONG; var ulProblemNumber: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_First_Log_Conf(plcLogConf: PLOG_CONF;         // OPTIONAL
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_First_Log_Conf_Ex(plcLogConf: PLOG_CONF;      // OPTIONAL
  dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Global_State(var ulState: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Global_State_Ex(var ulState: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Hardware_Profile_InfoA(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO_A; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Hardware_Profile_InfoW(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO_W; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Hardware_Profile_Info(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Hardware_Profile_Info_ExA(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO_A; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Hardware_Profile_Info_ExW(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO_W; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Hardware_Profile_Info_Ex(ulIndex: ULONG;
  var HWProfileInfo: HWPROFILEINFO; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_HW_Prof_FlagsA(szDevInstName: DEVINSTID_A;
  ulHardwareProfile: ULONG; var ulValue: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_HW_Prof_FlagsW(szDevInstName: DEVINSTID_W;
  ulHardwareProfile: ULONG; var ulValue: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_HW_Prof_Flags(szDevInstName: DEVINSTID;
  ulHardwareProfile: ULONG; var ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_HW_Prof_Flags_ExA(szDevInstName: DEVINSTID_A;
  ulHardwareProfile: ULONG; var ulValue: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_HW_Prof_Flags_ExW(szDevInstName: DEVINSTID_W;
  ulHardwareProfile: ULONG; var ulValue: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_HW_Prof_Flags_Ex(szDevInstName: DEVINSTID;
  ulHardwareProfile: ULONG; var ulValue: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFNDEF WINNT4}

function CM_Get_Device_Interface_AliasA(const pszDeviceInterface: PAnsiChar;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_AliasW(const pszDeviceInterface: PWideChar;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_Alias(const pszDeviceInterface: PTSTR;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_Interface_Alias_ExA(const pszDeviceInterface: PAnsiChar;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PAnsiChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_Alias_ExW(const pszDeviceInterface: PWideChar;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PWideChar;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_Alias_Ex(const pszDeviceInterface: PTSTR;
  AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PTSTR;
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_Interface_ListA(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_ListW(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID;          // OPTIONAL
  Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_Interface_List_ExA(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_ExW(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_Ex(InterfaceClassGuid: PGUID;
  pDeviceID: DEVINSTID;          // OPTIONAL
  Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Device_Interface_List_SizeA(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_SizeW(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_Size(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Device_Interface_List_Size_ExA(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_Size_ExW(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Device_Interface_List_Size_Ex(var ulLen: ULONG;
  InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Log_Conf_Priority(lcLogConf: LOG_CONF;
  var Priority: PRIORITY; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Log_Conf_Priority_Ex(lcLogConf: LOG_CONF;
  var Priority: PRIORITY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$ENDIF !WINNT4}

function CM_Get_Next_Log_Conf(plcLogConf: PLOG_CONF;         // OPTIONAL
  lcLogConf: LOG_CONF; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Next_Log_Conf_Ex(plcLogConf: PLOG_CONF;      // OPTIONAL
  lcLogConf: LOG_CONF; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Parent(var dnDevInstParent: DEVINST;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Parent_Ex(var dnDevInstParent: DEVINST;
  dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Res_Des_Data(rdResDes: RES_DES; Buffer: Pointer;
  BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Res_Des_Data_Ex(rdResDes: RES_DES; Buffer: Pointer;
  BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Res_Des_Data_Size(var ulSize: ULONG; rdResDes: RES_DES;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Res_Des_Data_Size_Ex(var ulSize: ULONG; rdResDes: RES_DES;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Sibling(var dnDevInstSibling: DEVINST;
  DevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Sibling_Ex(var dnDevInstSibling: DEVINST;
  DevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Get_Version: WORD; stdcall;

function CM_Get_Version_Ex(hMachine: HMACHINE): WORD; stdcall;

{$IFDEF WINXP_UP}

function CM_Is_Version_Available(wVersion: WORD): BOOL; stdcall;

function CM_Is_Version_Available_Ex(wVersion: WORD;
  hMachine: HMACHINE): BOOL; stdcall;

{$ENDIF WINXP_UP}

function CM_Intersect_Range_List(rlhOld1: RANGE_LIST;rlhOld2: RANGE_LIST;
  rlhNew: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Invert_Range_List(rlhOld: RANGE_LIST; rlhNew: RANGE_LIST;
  ullMaxValue: DWORDLONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Locate_DevNodeA(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Locate_DevNodeW(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Locate_DevNode(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Locate_DevNode_ExA(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Locate_DevNode_ExW(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Locate_DevNode_Ex(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Locate_DevInstA(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Locate_DevInstW(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Locate_DevInst(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Locate_DevInst_ExA(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_A;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Locate_DevInst_ExW(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID_W;        // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Locate_DevInst_Ex(var dnDevInst: DEVINST;
  pDeviceID: DEVINSTID;          // OPTIONAL
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Merge_Range_List(rlhOld1: RANGE_LIST; rlhOld2: RANGE_LIST;
  rlhNew: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Modify_Res_Des(var rdResDesModified: RES_DES;
  rdResDes: RES_DES; ResourceID: RESOURCEID; ResourceData: Pointer;
  ResourceLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Modify_Res_Des_Ex(var rdResDesModified: RES_DES;
  rdResDes: RES_DES; ResourceID: RESOURCEID; ResourceData: Pointer;
  ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Move_DevNode(dnFromDevInst: DEVINST;
  dnToDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Move_DevInst(dnFromDevInst: DEVINST;
  dnToDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Move_DevNode_Ex(dnFromDevInst: DEVINST;
  dnToDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Move_DevInst_Ex(dnFromDevInst: DEVINST;
  dnToDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Next_Range(var reElement: RANGE_ELEMENT;
  var ullStart: DWORDLONG; var ullEnd: DWORDLONG;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Next_Res_Des(var rdResDesNext: RES_DES;
  rdResDes: RES_DES; ForResource: RESOURCEID; var ResourceID: RESOURCEID;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Get_Next_Res_Des_Ex(var rdResDesNext: RES_DES;
  rdResDes: RES_DES; ForResource: RESOURCEID; var ResourceID: RESOURCEID;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Open_Class_KeyA(ClassGuid: PGUID; // OPTIONAL
  const pszClassName: PAnsiChar;              // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Open_Class_KeyW(ClassGuid: PGUID; // OPTIONAL
  const pszClassName: PWideChar;              // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Open_Class_Key(ClassGuid: PGUID;  // OPTIONAL
  const pszClassName: PTSTR;                  // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Open_Class_Key_ExA(pszClassGuid: PGUID; // OPTIONAL
  const pszClassName: PAnsiChar;                    // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Open_Class_Key_ExW(pszClassGuid: PGUID; // OPTIONAL
  const pszClassName: PWideChar;                    // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Open_Class_Key_Ex(pszClassGuid: PGUID;  // OPTIONAL
  const pszClassName: PTSTR;                        // OPTIONAL
  samDesired: REGSAM; Disposition: REGDISPOSITION;
  var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Open_DevNode_Key(dnDevNode: DEVINST; samDesired: REGSAM;
  ulHardwareProfile: ULONG; Disposition: REGDISPOSITION;
  var hkDevice: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Open_DevInst_Key(dnDevNode: DEVINST; samDesired: REGSAM;
  ulHardwareProfile: ULONG; Disposition: REGDISPOSITION;
  var hkDevice: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Open_DevNode_Key_Ex(dnDevNode: DEVINST; samDesired: REGSAM;
  ulHardwareProfile: ULONG; Disposition: REGDISPOSITION; var hkDevice: HKEY;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Open_DevInst_Key_Ex(dnDevNode: DEVINST; samDesired: REGSAM;
  ulHardwareProfile: ULONG; Disposition: REGDISPOSITION; var hkDevice: HKEY;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Query_Arbitrator_Free_Data(pData: Pointer; DataLen: ULONG;
  dnDevInst: DEVINST; ResourceID: RESOURCEID; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Query_Arbitrator_Free_Data_Ex(pData: Pointer; DataLen: ULONG;
  dnDevInst: DEVINST; ResourceID: RESOURCEID; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Query_Arbitrator_Free_Size(var ulSize: ULONG; dnDevInst: DEVINST;
  ResourceID: RESOURCEID; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Query_Arbitrator_Free_Size_Ex(var ulSize: ULONG; dnDevInst: DEVINST;
  ResourceID: RESOURCEID; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Query_Remove_SubTree(dnAncestor: DEVINST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Query_Remove_SubTree_Ex(dnAncestor: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFDEF WIN2000_UP}

function CM_Query_And_Remove_SubTreeA(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PAnsiChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Query_And_Remove_SubTreeW(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PWideChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Query_And_Remove_SubTree(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PTSTR;            // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Query_And_Remove_SubTree_ExA(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PAnsiChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Query_And_Remove_SubTree_ExW(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PWideChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Query_And_Remove_SubTree_Ex(dnAncestor: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PTSTR;            // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Request_Device_EjectA(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PAnsiChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Request_Device_EjectW(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PWideChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Request_Device_Eject(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PTSTR;            // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Request_Device_Eject_ExA(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PAnsiChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Request_Device_Eject_ExW(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PWideChar;        // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Request_Device_Eject_Ex(dnDevInst: DEVINST;
  pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
  pszVetoName: PTSTR;            // OPTIONAL
  ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$ENDIF WIN2000_UP}

function CM_Reenumerate_DevNode(dnDevInst: DEVINST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Reenumerate_DevInst(dnDevInst: DEVINST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Reenumerate_DevNode_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Reenumerate_DevInst_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFNDEF WINNT4}

function CM_Register_Device_InterfaceA(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PAnsiChar; // OPTIONAL
  pszDeviceInterface: PAnsiChar; var ulLength: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Register_Device_InterfaceW(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PWideChar; // OPTIONAL
  pszDeviceInterface: PWideChar; var ulLength: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Register_Device_Interface(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PTSTR;     // OPTIONAL
  pszDeviceInterface: PTSTR; var ulLength: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Register_Device_Interface_ExA(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PAnsiChar; // OPTIONAL
  pszDeviceInterface: PAnsiChar; var ulLength: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Register_Device_Interface_ExW(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PWideChar; // OPTIONAL
  pszDeviceInterface: PWideChar; var ulLength: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Register_Device_Interface_Ex(dnDevInst: DEVINST;
  InterfaceClassGuid: PGUID;
  const pszReference: PTSTR;     // OPTIONAL
  pszDeviceInterface: PTSTR; var ulLength: ULONG;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_DevNode_Problem_Ex(dnDevInst: DEVINST;
  ulProblem: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_DevInst_Problem_Ex(dnDevInst: DEVINST;
  ulProblem: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_DevNode_Problem(dnDevInst: DEVINST; ulProblem: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Set_DevInst_Problem(dnDevInst: DEVINST; ulProblem: ULONG;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Unregister_Device_InterfaceA(const pszDeviceInterface: PAnsiChar;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Unregister_Device_InterfaceW(const pszDeviceInterface: PWideChar;
  ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Unregister_Device_Interface(const pszDeviceInterface: PTSTR;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Unregister_Device_Interface_ExA(const pszDeviceInterface: PAnsiChar;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Unregister_Device_Interface_ExW(const pszDeviceInterface: PWideChar;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Unregister_Device_Interface_Ex(const pszDeviceInterface: PTSTR;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Register_Device_Driver(dnDevInst: DEVINST;
  ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Register_Device_Driver_Ex(dnDevInst: DEVINST;
  ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$ENDIF !WINNT4}

function CM_Remove_SubTree(dnAncestor: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Remove_SubTree_Ex(dnAncestor: DEVINST; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_DevNode_Registry_PropertyA(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_DevNode_Registry_PropertyW(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_DevNode_Registry_Property(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Set_DevNode_Registry_Property_ExA(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_DevNode_Registry_Property_ExW(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_DevNode_Registry_Property_Ex(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_DevInst_Registry_PropertyA(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_DevInst_Registry_PropertyW(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_DevInst_Registry_Property(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Set_DevInst_Registry_Property_ExA(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_DevInst_Registry_Property_ExW(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_DevInst_Registry_Property_Ex(dnDevInst: DEVINST;
  ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFNDEF WINNT4}
function CM_Is_Dock_Station_Present(var bPresent: BOOL): CONFIGRET; stdcall;
{$ENDIF !WINNT4}

{$IFDEF WIN2000_UP}
function CM_Is_Dock_Station_Present_Ex(var bPresent: BOOL;
  hMachine: HMACHINE): CONFIGRET; stdcall;
{$ENDIF WIN2000_UP}

{$IFNDEF WINNT4}
function CM_Request_Eject_PC: CONFIGRET; stdcall;
{$ENDIF !WINNT4}

{$IFDEF WIN2000_UP}
function CM_Request_Eject_PC_Ex(hMachine: HMACHINE): CONFIGRET; stdcall;
{$ENDIF WIN2000_UP}

function CM_Set_HW_Prof_FlagsA(szDevInstName: DEVINSTID_A;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_HW_Prof_FlagsW(szDevInstName: DEVINSTID_W;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
function CM_Set_HW_Prof_Flags(szDevInstName: DEVINSTID;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Set_HW_Prof_Flags_ExA(szDevInstName: DEVINSTID_A;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_HW_Prof_Flags_ExW(szDevInstName: DEVINSTID_W;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_HW_Prof_Flags_Ex(szDevInstName: DEVINSTID;
  ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Setup_DevNode(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Setup_DevInst(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Setup_DevNode_Ex(dnDevInst: DEVINST; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Setup_DevInst_Ex(dnDevInst: DEVINST; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Test_Range_Available(ullStartValue: DWORDLONG; ullEndValue: DWORDLONG;
  rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Uninstall_DevNode(dnPhantom: DEVNODE; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Uninstall_DevInst(dnPhantom: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Uninstall_DevNode_Ex(dnPhantom: DEVNODE; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Uninstall_DevInst_Ex(dnPhantom: DEVINST; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Run_Detection(ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Run_Detection_Ex(ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_HW_Prof(ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

function CM_Set_HW_Prof_Ex(ulHardwareProfile: ULONG; ulFlags: ULONG;
  hMachine: HMACHINE): CONFIGRET; stdcall;

{$IFDEF WIN2000_UP}

function CM_Query_Resource_Conflict_List(var clConflictList: CONFLICT_LIST;
  dnDevInst: DEVINST; ResourceID: RESOURCEID; ResourceData: Pointer;
  ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Free_Resource_Conflict_Handle(clConflictList: CONFLICT_LIST): CONFIGRET; stdcall;

function CM_Get_Resource_Conflict_Count(clConflictList: CONFLICT_LIST;
  var ulCount: ULONG): CONFIGRET; stdcall;

function CM_Get_Resource_Conflict_DetailsA(clConflictList: CONFLICT_LIST;
  ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS_A): CONFIGRET; stdcall;
function CM_Get_Resource_Conflict_DetailsW(clConflictList: CONFLICT_LIST;
  ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS_W): CONFIGRET; stdcall;
function CM_Get_Resource_Conflict_Details(clConflictList: CONFLICT_LIST;
  ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS): CONFIGRET; stdcall;

function CM_Get_Class_Registry_PropertyA(ClassGUID: PGUID; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Registry_PropertyW(ClassGUID: PGUID; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Get_Class_Registry_Property(ClassGUID: PGUID; ulProperty: ULONG;
  pulRegDataType: PULONG;        // OPTIONAL
  Buffer: Pointer;               // OPTIONAL
  var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_Set_Class_Registry_PropertyA(ClassGUID: PGUID; ulProperty: ULONG;
  Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_Class_Registry_PropertyW(ClassGUID: PGUID; ulProperty: ULONG;
  Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
function CM_Set_Class_Registry_Property(ClassGUID: PGUID; ulProperty: ULONG;
  Buffer: Pointer;               // OPTIONAL
  ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

function CM_WaitNoPendingInstallEvents(dwTimeout: DWORD): DWORD; stdcall;
function CMP_WaitNoPendingInstallEvents(dwTimeout: DWORD): DWORD; stdcall;

{$ENDIF WIN2000_UP}

{$ELSE}

type
  TCM_Add_Empty_Log_Conf = function(var lcLogConf: LOG_CONF;
    dnDevInst: DEVINST; Priority: PRIORITY; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Add_Empty_Log_Conf_Ex = function(var lcLogConf: LOG_CONF;
    dnDevInst: DEVINST; Priority: PRIORITY; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Add_IDA = function(dnDevInst: DEVINST; pszID: PAnsiChar;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Add_IDW = function(dnDevInst: DEVINST; pszID: PWideChar;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Add_ID = function(dnDevInst: DEVINST; pszID: PTSTR;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Add_ID_ExA = function(dnDevInst: DEVINST; pszID: PAnsiChar;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Add_ID_ExW = function(dnDevInst: DEVINST; pszID: PWideChar;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Add_ID_Ex = function(dnDevInst: DEVINST; pszID: PTSTR;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Add_Range = function(ullStartValue: DWORDLONG;
    ullEndValue: DWORDLONG; rlh: RANGE_LIST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Add_Res_Des = function(var rdResDes: RES_DES; lcLogConf: LOG_CONF;
    ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Add_Res_Des_Ex = function(var rdResDes: RES_DES;
    lcLogConf: LOG_CONF; ResourceID: RESOURCEID; ResourceData: Pointer;
    ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Connect_MachineA = function(const UNCServerName: PAnsiChar;
    var hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Connect_MachineW = function(const UNCServerName: PWideChar;
    var hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Connect_Machine = function(const UNCServerName: PTSTR;
    var hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Create_DevNodeA = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Create_DevNodeW = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Create_DevNode = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Create_DevNode_ExA = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Create_DevNode_ExW = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Create_DevNode_Ex = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Create_DevInstA = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Create_DevInstW = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Create_DevInst = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
    dnParent: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Create_DevInst_ExA = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_A;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Create_DevInst_ExW = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID_W;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Create_DevInst_Ex = function(var dnDevInst: DEVINST; pDeviceID: DEVINSTID;
    dnParent: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Create_Range_List = function(var rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Delete_Class_Key = function(ClassGuid: PGUID; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Delete_Class_Key_Ex = function(ClassGuid: PGUID; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Delete_DevNode_Key = function(dnDevNode: DEVNODE;
    ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Delete_DevNode_Key_Ex = function(dnDevNode: DEVNODE;
    ulHardwareProfile: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Delete_DevInst_Key = function(dnDevNode: DEVINST;
    ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Delete_DevInst_Key_Ex = function(dnDevNode: DEVINST; ulHardwareProfile: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Delete_Range = function(ullStartValue: DWORDLONG; ullEndValue: DWORDLONG;
    rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Detect_Resource_Conflict = function(dnDevInst: DEVINST;
    ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
    var bConflictDetected: BOOL; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Detect_Resource_Conflict_Ex = function(dnDevInst: DEVINST;
    ResourceID: RESOURCEID; ResourceData: Pointer; ResourceLen: ULONG;
    var bConflictDetected: BOOL; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Disable_DevNode = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Disable_DevNode_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Disable_DevInst = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Disable_DevInst_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Disconnect_Machine = function(hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Dup_Range_List = function(rlhOld: RANGE_LIST; rlhNew: RANGE_LIST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Enable_DevNode = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Enable_DevNode_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Enable_DevInst = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Enable_DevInst_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Enumerate_Classes = function(ulClassIndex: ULONG;
    var ClassGuid: TGUID; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Enumerate_Classes_Ex = function(ulClassIndex: ULONG; var ClassGuid: TGUID;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Enumerate_EnumeratorsA = function(ulEnumIndex: ULONG; Buffer: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Enumerate_EnumeratorsW = function(ulEnumIndex: ULONG; Buffer: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Enumerate_Enumerators = function(ulEnumIndex: ULONG; Buffer: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Enumerate_Enumerators_ExA = function(ulEnumIndex: ULONG; Buffer: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Enumerate_Enumerators_ExW = function(ulEnumIndex: ULONG; Buffer: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Enumerate_Enumerators_Ex = function(ulEnumIndex: ULONG; Buffer: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Find_Range = function(var pullStart: DWORDLONG; ullStart: DWORDLONG;
    ulLength: ULONG; ullAlignment: DWORDLONG; ullEnd: DWORDLONG;
    rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_First_Range = function(rlh: RANGE_LIST; var ullStart: DWORDLONG;
    var ullEnd: DWORDLONG; preElement: PRANGE_ELEMENT;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Free_Log_Conf = function(lcLogConfToBeFreed: LOG_CONF;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Free_Log_Conf_Ex = function(lcLogConfToBeFreed: LOG_CONF;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Free_Log_Conf_Handle = function(lcLogConf: LOG_CONF): CONFIGRET; stdcall;

  TCM_Free_Range_List = function(rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Free_Res_Des = function(prdResDes: PRES_DES;
    rdResDes: RES_DES; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Free_Res_Des_Ex = function(prdResDes: PRES_DES; rdResDes: RES_DES;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Free_Res_Des_Handle = function(rdResDes: RES_DES): CONFIGRET; stdcall;

  TCM_Get_Child = function(var dnDevInstChild: DEVINST;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Child_Ex = function(var dnDevInstChild: DEVINST; dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Class_NameA = function(ClassGuid: PGUID; Buffer: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Class_NameW = function(ClassGuid: PGUID; Buffer: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Class_Name = function(ClassGuid: PGUID; Buffer: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Class_Name_ExA = function(ClassGuid: PGUID; Buffer: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Name_ExW = function(ClassGuid: PGUID; Buffer: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Name_Ex = function(ClassGuid: PGUID; Buffer: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Class_Key_NameA = function(ClassGuid: PGUID; pszKeyName: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Class_Key_NameW = function(ClassGuid: PGUID; pszKeyName: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Class_Key_Name = function(ClassGuid: PGUID; pszKeyName: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Class_Key_Name_ExA = function(ClassGuid: PGUID; pszKeyName: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Key_Name_ExW = function(ClassGuid: PGUID; pszKeyName: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Key_Name_Ex = function(ClassGuid: PGUID; pszKeyName: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Depth = function(var ulDepth: ULONG; dnDevInst: DEVINST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Depth_Ex = function(var ulDepth: ULONG; dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_IDA = function(dnDevInst: DEVINST; Buffer: PAnsiChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_IDW = function(dnDevInst: DEVINST; Buffer: PWideChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_ID = function(dnDevInst: DEVINST; Buffer: PTSTR;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_ID_ExA = function(dnDevInst: DEVINST; Buffer: PAnsiChar;
    BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_ExW = function(dnDevInst: DEVINST; Buffer: PWideChar;
    BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_Ex = function(dnDevInst: DEVINST; Buffer: PTSTR;
    BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_ID_ListA = function(const pszFilter: PAnsiChar;      // OPTIONAL
    Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_ID_ListW = function(const pszFilter: PWideChar;      // OPTIONAL
    Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List = function(const pszFilter: PTSTR;           // OPTIONAL
    Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_ID_List_ExA = function(const pszFilter: PAnsiChar;   // OPTIONAL
    Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_ExW = function(const pszFilter: PWideChar;   // OPTIONAL
    Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_Ex = function(const pszFilter: PTSTR;        // OPTIONAL
    Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_ID_List_SizeA = function(var ulLen: ULONG;
    const pszFilter: PAnsiChar;    // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_SizeW = function(var ulLen: ULONG;
    const pszFilter: PWideChar;    // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_Size = function(var pulLen: ULONG;
    const pszFilter: PTSTR;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_ID_List_Size_ExA = function(var ulLen: ULONG;
    const pszFilter: PAnsiChar;    // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_Size_ExW = function(var ulLen: ULONG;
    const pszFilter: PWideChar;    // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_ID_List_Size_Ex = function(var ulLen: ULONG;
    const pszFilter: PTSTR;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_ID_Size = function(var ulLen: ULONG;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_ID_Size_Ex = function(var ulLen: ULONG; dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_DevNode_Registry_PropertyA = function(dnDevInst: DEVINST;
    ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevNode_Registry_PropertyW = function(dnDevInst: DEVINST;
    ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevNode_Registry_Property = function(dnDevInst: DEVINST;
    ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevNode_Registry_Property_ExA = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevNode_Registry_Property_ExW = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevNode_Registry_Property_Ex = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_DevInst_Registry_PropertyA = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevInst_Registry_PropertyW = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevInst_Registry_Property = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevInst_Registry_Property_ExA = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevInst_Registry_Property_ExW = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevInst_Registry_Property_Ex = function(dnDevInst: DEVINST; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFDEF WINXP_UP}

  TCM_Get_DevNode_Custom_PropertyA = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PAnsiChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevNode_Custom_PropertyW = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PWideChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevNode_Custom_Property = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PTSTR;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevNode_Custom_Property_ExA = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PAnsiChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevNode_Custom_Property_ExW = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PWideChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevNode_Custom_Property_Ex = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PTSTR;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_DevInst_Custom_PropertyA = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PAnsiChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevInst_Custom_PropertyW = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PWideChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_DevInst_Custom_Property = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PTSTR;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevInst_Custom_Property_ExA = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PAnsiChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevInst_Custom_Property_ExW = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PWideChar;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_DevInst_Custom_Property_Ex = function(dnDevInst: DEVINST;
    const pszCustomPropertyName: PTSTR;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$ENDIF WINXP_UP}

  TCM_Get_DevNode_Status = function(var ulStatus: ULONG; var ulProblemNumber: ULONG;
   dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevInst_Status = function(var ulStatus: ULONG; var ulProblemNumber: ULONG;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_DevNode_Status_Ex = function(var ulStatus: ULONG; var ulProblemNumber: ULONG;
    dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_DevInst_Status_Ex = function(var ulStatus: ULONG; var ulProblemNumber: ULONG;
    dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_First_Log_Conf = function(plcLogConf: PLOG_CONF;         // OPTIONAL
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_First_Log_Conf_Ex = function(plcLogConf: PLOG_CONF;      // OPTIONAL
    dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Global_State = function(var ulState: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Global_State_Ex = function(var ulState: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Hardware_Profile_InfoA = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO_A; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Hardware_Profile_InfoW = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO_W; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Hardware_Profile_Info = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Hardware_Profile_Info_ExA = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO_A; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Hardware_Profile_Info_ExW = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO_W; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Hardware_Profile_Info_Ex = function(ulIndex: ULONG;
    var HWProfileInfo: HWPROFILEINFO; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_HW_Prof_FlagsA = function(szDevInstName: DEVINSTID_A;
    ulHardwareProfile: ULONG; var ulValue: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_HW_Prof_FlagsW = function(szDevInstName: DEVINSTID_W;
    ulHardwareProfile: ULONG; var ulValue: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_HW_Prof_Flags = function(szDevInstName: DEVINSTID;
    ulHardwareProfile: ULONG; var ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_HW_Prof_Flags_ExA = function(szDevInstName: DEVINSTID_A;
    ulHardwareProfile: ULONG; var ulValue: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_HW_Prof_Flags_ExW = function(szDevInstName: DEVINSTID_W;
    ulHardwareProfile: ULONG; var ulValue: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_HW_Prof_Flags_Ex = function(szDevInstName: DEVINSTID;
    ulHardwareProfile: ULONG; var ulValue: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFNDEF WINNT4}

  TCM_Get_Device_Interface_AliasA = function(const pszDeviceInterface: PAnsiChar;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_AliasW = function(const pszDeviceInterface: PWideChar;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_Alias = function(const pszDeviceInterface: PTSTR;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_Alias_ExA = function(const pszDeviceInterface: PAnsiChar;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PAnsiChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_Alias_ExW = function(const pszDeviceInterface: PWideChar;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PWideChar;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_Alias_Ex = function(const pszDeviceInterface: PTSTR;
    AliasInterfaceGuid: PGUID; pszAliasDeviceInterface: PTSTR;
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_ListA = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_ListW = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID;          // OPTIONAL
    Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_List_ExA = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    Buffer: PAnsiChar; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_ExW = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    Buffer: PWideChar; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_Ex = function(InterfaceClassGuid: PGUID;
    pDeviceID: DEVINSTID;          // OPTIONAL
    Buffer: PTSTR; BufferLen: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_List_SizeA = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_SizeW = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_Size = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Device_Interface_List_Size_ExA = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_Size_ExW = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Device_Interface_List_Size_Ex = function(var ulLen: ULONG;
    InterfaceClassGuid: PGUID; pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Log_Conf_Priority = function(lcLogConf: LOG_CONF;
    var Priority: PRIORITY; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Log_Conf_Priority_Ex = function(lcLogConf: LOG_CONF;
    var Priority: PRIORITY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$ENDIF !WINNT4}

  TCM_Get_Next_Log_Conf = function(plcLogConf: PLOG_CONF;         // OPTIONAL
    lcLogConf: LOG_CONF; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Next_Log_Conf_Ex = function(plcLogConf: PLOG_CONF;      // OPTIONAL
    lcLogConf: LOG_CONF; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Parent = function(var dnDevInstParent: DEVINST;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Parent_Ex = function(var dnDevInstParent: DEVINST;
    dnDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Res_Des_Data = function(rdResDes: RES_DES; Buffer: Pointer;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Res_Des_Data_Ex = function(rdResDes: RES_DES; Buffer: Pointer;
    BufferLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Res_Des_Data_Size = function(var ulSize: ULONG; rdResDes: RES_DES;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Res_Des_Data_Size_Ex = function(var ulSize: ULONG; rdResDes: RES_DES;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Sibling = function(var dnDevInstSibling: DEVINST;
    DevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Sibling_Ex = function(var dnDevInstSibling: DEVINST;
    DevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Get_Version = function: WORD; stdcall;

  TCM_Get_Version_Ex = function(hMachine: HMACHINE): WORD; stdcall;

  {$IFDEF WINXP_UP}

  TCM_Is_Version_Available = function(wVersion: WORD): BOOL; stdcall;

  TCM_Is_Version_Available_Ex = function(wVersion: WORD;
    hMachine: HMACHINE): BOOL; stdcall;

  {$ENDIF WINXP_UP}

  TCM_Intersect_Range_List = function(rlhOld1: RANGE_LIST;rlhOld2: RANGE_LIST;
    rlhNew: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Invert_Range_List = function(rlhOld: RANGE_LIST; rlhNew: RANGE_LIST;
    ullMaxValue: DWORDLONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Locate_DevNodeA = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Locate_DevNodeW = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Locate_DevNode = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Locate_DevNode_ExA = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Locate_DevNode_ExW = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Locate_DevNode_Ex = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Locate_DevInstA = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Locate_DevInstW = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Locate_DevInst = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Locate_DevInst_ExA = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_A;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Locate_DevInst_ExW = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID_W;        // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Locate_DevInst_Ex = function(var dnDevInst: DEVINST;
    pDeviceID: DEVINSTID;          // OPTIONAL
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Merge_Range_List = function(rlhOld1: RANGE_LIST; rlhOld2: RANGE_LIST;
    rlhNew: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Modify_Res_Des = function(var rdResDesModified: RES_DES;
    rdResDes: RES_DES; ResourceID: RESOURCEID; ResourceData: Pointer;
    ResourceLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Modify_Res_Des_Ex = function(var rdResDesModified: RES_DES;
    rdResDes: RES_DES; ResourceID: RESOURCEID; ResourceData: Pointer;
    ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Move_DevNode = function(dnFromDevInst: DEVINST;
    dnToDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Move_DevInst = function(dnFromDevInst: DEVINST;
    dnToDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Move_DevNode_Ex = function(dnFromDevInst: DEVINST;
    dnToDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Move_DevInst_Ex = function(dnFromDevInst: DEVINST;
    dnToDevInst: DEVINST; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Next_Range = function(var reElement: RANGE_ELEMENT;
    var ullStart: DWORDLONG; var ullEnd: DWORDLONG;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Next_Res_Des = function(var rdResDesNext: RES_DES;
    rdResDes: RES_DES; ForResource: RESOURCEID; var ResourceID: RESOURCEID;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Get_Next_Res_Des_Ex = function(var rdResDesNext: RES_DES;
    rdResDes: RES_DES; ForResource: RESOURCEID; var ResourceID: RESOURCEID;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Open_Class_KeyA = function(ClassGuid: PGUID; // OPTIONAL
    const pszClassName: PAnsiChar;              // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Open_Class_KeyW = function(ClassGuid: PGUID; // OPTIONAL
    const pszClassName: PWideChar;              // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Open_Class_Key = function(ClassGuid: PGUID;  // OPTIONAL
    const pszClassName: PTSTR;                  // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Open_Class_Key_ExA = function(pszClassGuid: PGUID; // OPTIONAL
    const pszClassName: PAnsiChar;                    // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Open_Class_Key_ExW = function(pszClassGuid: PGUID; // OPTIONAL
    const pszClassName: PWideChar;                    // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Open_Class_Key_Ex = function(pszClassGuid: PGUID;  // OPTIONAL
    const pszClassName: PTSTR;                        // OPTIONAL
    samDesired: REGSAM; Disposition: REGDISPOSITION;
    var hkClass: HKEY; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Open_DevNode_Key = function(dnDevNode: DEVINST; samDesired: REGSAM;
    ulHardwareProfile: ULONG; Disposition: REGDISPOSITION;
    var hkDevice: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Open_DevInst_Key = function(dnDevNode: DEVINST; samDesired: REGSAM;
    ulHardwareProfile: ULONG; Disposition: REGDISPOSITION;
    var hkDevice: HKEY; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Open_DevNode_Key_Ex = function(dnDevNode: DEVINST; samDesired: REGSAM;
    ulHardwareProfile: ULONG; Disposition: REGDISPOSITION; var hkDevice: HKEY;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Open_DevInst_Key_Ex = function(dnDevNode: DEVINST; samDesired: REGSAM;
    ulHardwareProfile: ULONG; Disposition: REGDISPOSITION; var hkDevice: HKEY;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Query_Arbitrator_Free_Data = function(pData: Pointer; DataLen: ULONG;
    dnDevInst: DEVINST; ResourceID: RESOURCEID; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Query_Arbitrator_Free_Data_Ex = function(pData: Pointer; DataLen: ULONG;
    dnDevInst: DEVINST; ResourceID: RESOURCEID; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Query_Arbitrator_Free_Size = function(var ulSize: ULONG; dnDevInst: DEVINST;
    ResourceID: RESOURCEID; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Query_Arbitrator_Free_Size_Ex = function(var ulSize: ULONG; dnDevInst: DEVINST;
    ResourceID: RESOURCEID; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Query_Remove_SubTree = function(dnAncestor: DEVINST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Query_Remove_SubTree_Ex = function(dnAncestor: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFDEF WIN2000_UP}

  TCM_Query_And_Remove_SubTreeA = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Query_And_Remove_SubTreeW = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Query_And_Remove_SubTree = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PTSTR;            // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Query_And_Remove_SubTree_ExA = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Query_And_Remove_SubTree_ExW = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Query_And_Remove_SubTree_Ex = function(dnAncestor: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PTSTR;            // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Request_Device_EjectA = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Request_Device_EjectW = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Request_Device_Eject = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PTSTR;            // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Request_Device_Eject_ExA = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Request_Device_Eject_ExW = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Request_Device_Eject_Ex = function(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PTSTR;            // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$ENDIF WIN2000_UP}

  TCM_Reenumerate_DevNode = function(dnDevInst: DEVINST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Reenumerate_DevInst = function(dnDevInst: DEVINST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Reenumerate_DevNode_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Reenumerate_DevInst_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFNDEF WINNT4}

  TCM_Register_Device_InterfaceA = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PAnsiChar; // OPTIONAL
    pszDeviceInterface: PAnsiChar; var ulLength: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Register_Device_InterfaceW = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PWideChar; // OPTIONAL
    pszDeviceInterface: PWideChar; var ulLength: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Register_Device_Interface = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PTSTR;     // OPTIONAL
    pszDeviceInterface: PTSTR; var ulLength: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Register_Device_Interface_ExA = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PAnsiChar; // OPTIONAL
    pszDeviceInterface: PAnsiChar; var ulLength: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Register_Device_Interface_ExW = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PWideChar; // OPTIONAL
    pszDeviceInterface: PWideChar; var ulLength: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Register_Device_Interface_Ex = function(dnDevInst: DEVINST;
    InterfaceClassGuid: PGUID;
    const pszReference: PTSTR;     // OPTIONAL
    pszDeviceInterface: PTSTR; var ulLength: ULONG;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_DevNode_Problem_Ex = function(dnDevInst: DEVINST;
    ulProblem: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_DevInst_Problem_Ex = function(dnDevInst: DEVINST;
    ulProblem: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_DevNode_Problem = function(dnDevInst: DEVINST; ulProblem: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Set_DevInst_Problem = function(dnDevInst: DEVINST; ulProblem: ULONG;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Unregister_Device_InterfaceA = function(const pszDeviceInterface: PAnsiChar;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Unregister_Device_InterfaceW = function(const pszDeviceInterface: PWideChar;
    ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Unregister_Device_Interface = function(const pszDeviceInterface: PTSTR;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Unregister_Device_Interface_ExA = function(const pszDeviceInterface: PAnsiChar;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Unregister_Device_Interface_ExW = function(const pszDeviceInterface: PWideChar;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Unregister_Device_Interface_Ex = function(const pszDeviceInterface: PTSTR;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Register_Device_Driver = function(dnDevInst: DEVINST;
    ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Register_Device_Driver_Ex = function(dnDevInst: DEVINST;
    ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$ENDIF !WINNT4}

  TCM_Remove_SubTree = function(dnAncestor: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Remove_SubTree_Ex = function(dnAncestor: DEVINST; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_DevNode_Registry_PropertyA = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_DevNode_Registry_PropertyW = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_DevNode_Registry_Property = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Set_DevNode_Registry_Property_ExA = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_DevNode_Registry_Property_ExW = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_DevNode_Registry_Property_Ex = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_DevInst_Registry_PropertyA = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_DevInst_Registry_PropertyW = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_DevInst_Registry_Property = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Set_DevInst_Registry_Property_ExA = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_DevInst_Registry_Property_ExW = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_DevInst_Registry_Property_Ex = function(dnDevInst: DEVINST;
    ulProperty: ULONG; Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFNDEF WINNT4}
  TCM_Is_Dock_Station_Present = function(var bPresent: BOOL): CONFIGRET; stdcall;
  {$ENDIF !WINNT4}

  {$IFDEF WIN2000_UP}
  TCM_Is_Dock_Station_Present_Ex = function(var bPresent: BOOL;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  {$ENDIF WIN2000_UP}

  {$IFNDEF WINNT4}
  TCM_Request_Eject_PC = function: CONFIGRET; stdcall;
  {$ENDIF !WINNT4}

  {$IFDEF WIN2000_UP}
  TCM_Request_Eject_PC_Ex = function(hMachine: HMACHINE): CONFIGRET; stdcall;
  {$ENDIF WIN2000_UP}

  TCM_Set_HW_Prof_FlagsA = function(szDevInstName: DEVINSTID_A;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_HW_Prof_FlagsW = function(szDevInstName: DEVINSTID_W;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
  TCM_Set_HW_Prof_Flags = function(szDevInstName: DEVINSTID;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Set_HW_Prof_Flags_ExA = function(szDevInstName: DEVINSTID_A;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_HW_Prof_Flags_ExW = function(szDevInstName: DEVINSTID_W;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_HW_Prof_Flags_Ex = function(szDevInstName: DEVINSTID;
    ulConfig: ULONG; ulValue: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Setup_DevNode = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Setup_DevInst = function(dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Setup_DevNode_Ex = function(dnDevInst: DEVINST; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Setup_DevInst_Ex = function(dnDevInst: DEVINST; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Test_Range_Available = function(ullStartValue: DWORDLONG; ullEndValue: DWORDLONG;
    rlh: RANGE_LIST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Uninstall_DevNode = function(dnPhantom: DEVNODE; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Uninstall_DevInst = function(dnPhantom: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Uninstall_DevNode_Ex = function(dnPhantom: DEVNODE; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Uninstall_DevInst_Ex = function(dnPhantom: DEVINST; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Run_Detection = function(ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Run_Detection_Ex = function(ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_HW_Prof = function(ulHardwareProfile: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;

  TCM_Set_HW_Prof_Ex = function(ulHardwareProfile: ULONG; ulFlags: ULONG;
    hMachine: HMACHINE): CONFIGRET; stdcall;

  {$IFDEF WIN2000_UP}

  TCM_Query_Resource_Conflict_List = function(var clConflictList: CONFLICT_LIST;
    dnDevInst: DEVINST; ResourceID: RESOURCEID; ResourceData: Pointer;
    ResourceLen: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Free_Resource_Conflict_Handle = function(clConflictList: CONFLICT_LIST): CONFIGRET; stdcall;

  TCM_Get_Resource_Conflict_Count = function(clConflictList: CONFLICT_LIST;
    var ulCount: ULONG): CONFIGRET; stdcall;

  TCM_Get_Resource_Conflict_DetailsA = function(clConflictList: CONFLICT_LIST;
    ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS_A): CONFIGRET; stdcall;
  TCM_Get_Resource_Conflict_DetailsW = function(clConflictList: CONFLICT_LIST;
    ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS_W): CONFIGRET; stdcall;
  TCM_Get_Resource_Conflict_Details = function(clConflictList: CONFLICT_LIST;
    ulIndex: ULONG; var ConflictDetails: CONFLICT_DETAILS): CONFIGRET; stdcall;

  TCM_Get_Class_Registry_PropertyA = function(ClassGUID: PGUID; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Registry_PropertyW = function(ClassGUID: PGUID; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Get_Class_Registry_Property = function(ClassGUID: PGUID; ulProperty: ULONG;
    pulRegDataType: PULONG;        // OPTIONAL
    Buffer: Pointer;               // OPTIONAL
    var ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_Set_Class_Registry_PropertyA = function(ClassGUID: PGUID; ulProperty: ULONG;
    Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_Class_Registry_PropertyW = function(ClassGUID: PGUID; ulProperty: ULONG;
    Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;
  TCM_Set_Class_Registry_Property = function(ClassGUID: PGUID; ulProperty: ULONG;
    Buffer: Pointer;               // OPTIONAL
    ulLength: ULONG; ulFlags: ULONG; hMachine: HMACHINE): CONFIGRET; stdcall;

  TCM_WaitNoPendingInstallEvents = function(dwTimeout: DWORD): DWORD; stdcall;
  TCMP_WaitNoPendingInstallEvents = function(dwTimeout: DWORD): DWORD; stdcall;

  {$ENDIF WIN2000_UP}

var
  CM_Add_Empty_Log_Conf: TCM_Add_Empty_Log_Conf;
  CM_Add_Empty_Log_Conf_Ex: TCM_Add_Empty_Log_Conf_Ex;
  CM_Add_IDA: TCM_Add_IDA;
  CM_Add_IDW: TCM_Add_IDW;
  CM_Add_ID: TCM_Add_ID;
  CM_Add_ID_ExA: TCM_Add_ID_ExA;
  CM_Add_ID_ExW: TCM_Add_ID_ExW;
  CM_Add_ID_Ex: TCM_Add_ID_Ex;
  CM_Add_Range: TCM_Add_Range;
  CM_Add_Res_Des: TCM_Add_Res_Des;
  CM_Add_Res_Des_Ex: TCM_Add_Res_Des_Ex;
  CM_Connect_MachineA: TCM_Connect_MachineA;
  CM_Connect_MachineW: TCM_Connect_MachineW;
  CM_Connect_Machine: TCM_Connect_Machine;
  CM_Create_DevNodeA: TCM_Create_DevNodeA;
  CM_Create_DevNodeW: TCM_Create_DevNodeW;
  CM_Create_DevNode: TCM_Create_DevNode;
  CM_Create_DevNode_ExA: TCM_Create_DevNode_ExA;
  CM_Create_DevNode_ExW: TCM_Create_DevNode_ExW;
  CM_Create_DevNode_Ex: TCM_Create_DevNode_Ex;
  CM_Create_DevInstA: TCM_Create_DevInstA;
  CM_Create_DevInstW: TCM_Create_DevInstW;
  CM_Create_DevInst: TCM_Create_DevInst;
  CM_Create_DevInst_ExA: TCM_Create_DevInst_ExA;
  CM_Create_DevInst_ExW: TCM_Create_DevInst_ExW;
  CM_Create_DevInst_Ex: TCM_Create_DevInst_Ex;
  CM_Create_Range_List: TCM_Create_Range_List;
  CM_Delete_Class_Key: TCM_Delete_Class_Key;
  CM_Delete_Class_Key_Ex: TCM_Delete_Class_Key_Ex;
  CM_Delete_DevNode_Key: TCM_Delete_DevNode_Key;
  CM_Delete_DevNode_Key_Ex: TCM_Delete_DevNode_Key_Ex;
  CM_Delete_DevInst_Key: TCM_Delete_DevInst_Key;
  CM_Delete_DevInst_Key_Ex: TCM_Delete_DevInst_Key_Ex;
  CM_Delete_Range: TCM_Delete_Range;
  CM_Detect_Resource_Conflict: TCM_Detect_Resource_Conflict;
  CM_Detect_Resource_Conflict_Ex: TCM_Detect_Resource_Conflict_Ex;
  CM_Disable_DevNode: TCM_Disable_DevNode;
  CM_Disable_DevNode_Ex: TCM_Disable_DevNode_Ex;
  CM_Disable_DevInst: TCM_Disable_DevInst;
  CM_Disable_DevInst_Ex: TCM_Disable_DevInst_Ex;
  CM_Disconnect_Machine: TCM_Disconnect_Machine;
  CM_Dup_Range_List: TCM_Dup_Range_List;
  CM_Enable_DevNode: TCM_Enable_DevNode;
  CM_Enable_DevNode_Ex: TCM_Enable_DevNode_Ex;
  CM_Enable_DevInst: TCM_Enable_DevInst;
  CM_Enable_DevInst_Ex: TCM_Enable_DevInst_Ex;
  CM_Enumerate_Classes: TCM_Enumerate_Classes;
  CM_Enumerate_Classes_Ex: TCM_Enumerate_Classes_Ex;
  CM_Enumerate_EnumeratorsA: TCM_Enumerate_EnumeratorsA;
  CM_Enumerate_EnumeratorsW: TCM_Enumerate_EnumeratorsW;
  CM_Enumerate_Enumerators: TCM_Enumerate_Enumerators;
  CM_Enumerate_Enumerators_ExA: TCM_Enumerate_Enumerators_ExA;
  CM_Enumerate_Enumerators_ExW: TCM_Enumerate_Enumerators_ExW;
  CM_Enumerate_Enumerators_Ex: TCM_Enumerate_Enumerators_Ex;
  CM_Find_Range: TCM_Find_Range;
  CM_First_Range: TCM_First_Range;
  CM_Free_Log_Conf: TCM_Free_Log_Conf;
  CM_Free_Log_Conf_Ex: TCM_Free_Log_Conf_Ex;
  CM_Free_Log_Conf_Handle: TCM_Free_Log_Conf_Handle;
  CM_Free_Range_List: TCM_Free_Range_List;
  CM_Free_Res_Des: TCM_Free_Res_Des;
  CM_Free_Res_Des_Ex: TCM_Free_Res_Des_Ex;
  CM_Free_Res_Des_Handle: TCM_Free_Res_Des_Handle;
  CM_Get_Child: TCM_Get_Child;
  CM_Get_Child_Ex: TCM_Get_Child_Ex;
  CM_Get_Class_NameA: TCM_Get_Class_NameA;
  CM_Get_Class_NameW: TCM_Get_Class_NameW;
  CM_Get_Class_Name: TCM_Get_Class_Name;
  CM_Get_Class_Name_ExA: TCM_Get_Class_Name_ExA;
  CM_Get_Class_Name_ExW: TCM_Get_Class_Name_ExW;
  CM_Get_Class_Name_Ex: TCM_Get_Class_Name_Ex;
  CM_Get_Class_Key_NameA: TCM_Get_Class_Key_NameA;
  CM_Get_Class_Key_NameW: TCM_Get_Class_Key_NameW;
  CM_Get_Class_Key_Name: TCM_Get_Class_Key_Name;
  CM_Get_Class_Key_Name_ExA: TCM_Get_Class_Key_Name_ExA;
  CM_Get_Class_Key_Name_ExW: TCM_Get_Class_Key_Name_ExW;
  CM_Get_Class_Key_Name_Ex: TCM_Get_Class_Key_Name_Ex;
  CM_Get_Depth: TCM_Get_Depth;
  CM_Get_Depth_Ex: TCM_Get_Depth_Ex;
  CM_Get_Device_IDA: TCM_Get_Device_IDA;
  CM_Get_Device_IDW: TCM_Get_Device_IDW;
  CM_Get_Device_ID: TCM_Get_Device_ID;
  CM_Get_Device_ID_ExA: TCM_Get_Device_ID_ExA;
  CM_Get_Device_ID_ExW: TCM_Get_Device_ID_ExW;
  CM_Get_Device_ID_Ex: TCM_Get_Device_ID_Ex;
  CM_Get_Device_ID_ListA: TCM_Get_Device_ID_ListA;
  CM_Get_Device_ID_ListW: TCM_Get_Device_ID_ListW;
  CM_Get_Device_ID_List: TCM_Get_Device_ID_List;
  CM_Get_Device_ID_List_ExA: TCM_Get_Device_ID_List_ExA;
  CM_Get_Device_ID_List_ExW: TCM_Get_Device_ID_List_ExW;
  CM_Get_Device_ID_List_Ex: TCM_Get_Device_ID_List_Ex;
  CM_Get_Device_ID_List_SizeA: TCM_Get_Device_ID_List_SizeA;
  CM_Get_Device_ID_List_SizeW: TCM_Get_Device_ID_List_SizeW;
  CM_Get_Device_ID_List_Size: TCM_Get_Device_ID_List_Size;
  CM_Get_Device_ID_List_Size_ExA: TCM_Get_Device_ID_List_Size_ExA;
  CM_Get_Device_ID_List_Size_ExW: TCM_Get_Device_ID_List_Size_ExW;
  CM_Get_Device_ID_List_Size_Ex: TCM_Get_Device_ID_List_Size_Ex;
  CM_Get_Device_ID_Size: TCM_Get_Device_ID_Size;
  CM_Get_Device_ID_Size_Ex: TCM_Get_Device_ID_Size_Ex;
  CM_Get_DevNode_Registry_PropertyA: TCM_Get_DevNode_Registry_PropertyA;
  CM_Get_DevNode_Registry_PropertyW: TCM_Get_DevNode_Registry_PropertyW;
  CM_Get_DevNode_Registry_Property: TCM_Get_DevNode_Registry_Property;
  CM_Get_DevNode_Registry_Property_ExA: TCM_Get_DevNode_Registry_Property_ExA;
  CM_Get_DevNode_Registry_Property_ExW: TCM_Get_DevNode_Registry_Property_ExW;
  CM_Get_DevNode_Registry_Property_Ex: TCM_Get_DevNode_Registry_Property_Ex;
  CM_Get_DevInst_Registry_PropertyA: TCM_Get_DevInst_Registry_PropertyA;
  CM_Get_DevInst_Registry_PropertyW: TCM_Get_DevInst_Registry_PropertyW;
  CM_Get_DevInst_Registry_Property: TCM_Get_DevInst_Registry_Property;
  CM_Get_DevInst_Registry_Property_ExA: TCM_Get_DevInst_Registry_Property_ExA;
  CM_Get_DevInst_Registry_Property_ExW: TCM_Get_DevInst_Registry_Property_ExW;
  CM_Get_DevInst_Registry_Property_Ex: TCM_Get_DevInst_Registry_Property_Ex;
  {$IFDEF WINXP_UP}
  CM_Get_DevNode_Custom_PropertyA: TCM_Get_DevNode_Custom_PropertyA;
  CM_Get_DevNode_Custom_PropertyW: TCM_Get_DevNode_Custom_PropertyW;
  CM_Get_DevNode_Custom_Property: TCM_Get_DevNode_Custom_Property;
  CM_Get_DevNode_Custom_Property_ExA: TCM_Get_DevNode_Custom_Property_ExA;
  CM_Get_DevNode_Custom_Property_ExW: TCM_Get_DevNode_Custom_Property_ExW;
  CM_Get_DevNode_Custom_Property_Ex: TCM_Get_DevNode_Custom_Property_Ex;
  CM_Get_DevInst_Custom_PropertyA: TCM_Get_DevInst_Custom_PropertyA;
  CM_Get_DevInst_Custom_PropertyW: TCM_Get_DevInst_Custom_PropertyW;
  CM_Get_DevInst_Custom_Property: TCM_Get_DevInst_Custom_Property;
  CM_Get_DevInst_Custom_Property_ExA: TCM_Get_DevInst_Custom_Property_ExA;
  CM_Get_DevInst_Custom_Property_ExW: TCM_Get_DevInst_Custom_Property_ExW;
  CM_Get_DevInst_Custom_Property_Ex: TCM_Get_DevInst_Custom_Property_Ex;
  {$ENDIF WINXP_UP}
  CM_Get_DevNode_Status: TCM_Get_DevNode_Status;
  CM_Get_DevInst_Status: TCM_Get_DevInst_Status;
  CM_Get_DevNode_Status_Ex: TCM_Get_DevNode_Status_Ex;
  CM_Get_DevInst_Status_Ex: TCM_Get_DevInst_Status_Ex;
  CM_Get_First_Log_Conf: TCM_Get_First_Log_Conf;
  CM_Get_First_Log_Conf_Ex: TCM_Get_First_Log_Conf_Ex;
  CM_Get_Global_State: TCM_Get_Global_State;
  CM_Get_Global_State_Ex: TCM_Get_Global_State_Ex;
  CM_Get_Hardware_Profile_InfoA: TCM_Get_Hardware_Profile_InfoA;
  CM_Get_Hardware_Profile_InfoW: TCM_Get_Hardware_Profile_InfoW;
  CM_Get_Hardware_Profile_Info: TCM_Get_Hardware_Profile_Info;
  CM_Get_Hardware_Profile_Info_ExA: TCM_Get_Hardware_Profile_Info_ExA;
  CM_Get_Hardware_Profile_Info_ExW: TCM_Get_Hardware_Profile_Info_ExW;
  CM_Get_Hardware_Profile_Info_Ex: TCM_Get_Hardware_Profile_Info_Ex;
  CM_Get_HW_Prof_FlagsA: TCM_Get_HW_Prof_FlagsA;
  CM_Get_HW_Prof_FlagsW: TCM_Get_HW_Prof_FlagsW;
  CM_Get_HW_Prof_Flags: TCM_Get_HW_Prof_Flags;
  CM_Get_HW_Prof_Flags_ExA: TCM_Get_HW_Prof_Flags_ExA;
  CM_Get_HW_Prof_Flags_ExW: TCM_Get_HW_Prof_Flags_ExW;
  CM_Get_HW_Prof_Flags_Ex: TCM_Get_HW_Prof_Flags_Ex;
  {$IFNDEF WINNT4}
  CM_Get_Device_Interface_AliasA: TCM_Get_Device_Interface_AliasA;
  CM_Get_Device_Interface_AliasW: TCM_Get_Device_Interface_AliasW;
  CM_Get_Device_Interface_Alias: TCM_Get_Device_Interface_Alias;
  CM_Get_Device_Interface_Alias_ExA: TCM_Get_Device_Interface_Alias_ExA;
  CM_Get_Device_Interface_Alias_ExW: TCM_Get_Device_Interface_Alias_ExW;
  CM_Get_Device_Interface_Alias_Ex: TCM_Get_Device_Interface_Alias_Ex;
  CM_Get_Device_Interface_ListA: TCM_Get_Device_Interface_ListA;
  CM_Get_Device_Interface_ListW: TCM_Get_Device_Interface_ListW;
  CM_Get_Device_Interface_List: TCM_Get_Device_Interface_List;
  CM_Get_Device_Interface_List_ExA: TCM_Get_Device_Interface_List_ExA;
  CM_Get_Device_Interface_List_ExW: TCM_Get_Device_Interface_List_ExW;
  CM_Get_Device_Interface_List_Ex: TCM_Get_Device_Interface_List_Ex;
  CM_Get_Device_Interface_List_SizeA: TCM_Get_Device_Interface_List_SizeA;
  CM_Get_Device_Interface_List_SizeW: TCM_Get_Device_Interface_List_SizeW;
  CM_Get_Device_Interface_List_Size: TCM_Get_Device_Interface_List_Size;
  CM_Get_Device_Interface_List_Size_ExA: TCM_Get_Device_Interface_List_Size_ExA;
  CM_Get_Device_Interface_List_Size_ExW: TCM_Get_Device_Interface_List_Size_ExW;
  CM_Get_Device_Interface_List_Size_Ex: TCM_Get_Device_Interface_List_Size_Ex;
  CM_Get_Log_Conf_Priority: TCM_Get_Log_Conf_Priority;
  CM_Get_Log_Conf_Priority_Ex: TCM_Get_Log_Conf_Priority_Ex;
  {$ENDIF !WINNT4}
  CM_Get_Next_Log_Conf: TCM_Get_Next_Log_Conf;
  CM_Get_Next_Log_Conf_Ex: TCM_Get_Next_Log_Conf_Ex;
  CM_Get_Parent: TCM_Get_Parent;
  CM_Get_Parent_Ex: TCM_Get_Parent_Ex;
  CM_Get_Res_Des_Data: TCM_Get_Res_Des_Data;
  CM_Get_Res_Des_Data_Ex: TCM_Get_Res_Des_Data_Ex;
  CM_Get_Res_Des_Data_Size: TCM_Get_Res_Des_Data_Size;
  CM_Get_Res_Des_Data_Size_Ex: TCM_Get_Res_Des_Data_Size_Ex;
  CM_Get_Sibling: TCM_Get_Sibling;
  CM_Get_Sibling_Ex: TCM_Get_Sibling_Ex;
  CM_Get_Version: TCM_Get_Version;
  CM_Get_Version_Ex: TCM_Get_Version_Ex;
  {$IFDEF WINXP_UP}
  CM_Is_Version_Available: TCM_Is_Version_Available;
  CM_Is_Version_Available_Ex: TCM_Is_Version_Available_Ex;
  {$ENDIF WINXP_UP}
  CM_Intersect_Range_List: TCM_Intersect_Range_List;
  CM_Invert_Range_List: TCM_Invert_Range_List;
  CM_Locate_DevNodeA: TCM_Locate_DevNodeA;
  CM_Locate_DevNodeW: TCM_Locate_DevNodeW;
  CM_Locate_DevNode: TCM_Locate_DevNode;
  CM_Locate_DevNode_ExA: TCM_Locate_DevNode_ExA;
  CM_Locate_DevNode_ExW: TCM_Locate_DevNode_ExW;
  CM_Locate_DevNode_Ex: TCM_Locate_DevNode_Ex;
  CM_Locate_DevInstA: TCM_Locate_DevInstA;
  CM_Locate_DevInstW: TCM_Locate_DevInstW;
  CM_Locate_DevInst: TCM_Locate_DevInst;
  CM_Locate_DevInst_ExA: TCM_Locate_DevInst_ExA;
  CM_Locate_DevInst_ExW: TCM_Locate_DevInst_ExW;
  CM_Locate_DevInst_Ex: TCM_Locate_DevInst_Ex;
  CM_Merge_Range_List: TCM_Merge_Range_List;
  CM_Modify_Res_Des: TCM_Modify_Res_Des;
  CM_Modify_Res_Des_Ex: TCM_Modify_Res_Des_Ex;
  CM_Move_DevNode: TCM_Move_DevNode;
  CM_Move_DevInst: TCM_Move_DevInst;
  CM_Move_DevNode_Ex: TCM_Move_DevNode_Ex;
  CM_Move_DevInst_Ex: TCM_Move_DevInst_Ex;
  CM_Next_Range: TCM_Next_Range;
  CM_Get_Next_Res_Des: TCM_Get_Next_Res_Des;
  CM_Get_Next_Res_Des_Ex: TCM_Get_Next_Res_Des_Ex;
  CM_Open_Class_KeyA: TCM_Open_Class_KeyA;
  CM_Open_Class_KeyW: TCM_Open_Class_KeyW;
  CM_Open_Class_Key: TCM_Open_Class_Key;
  CM_Open_Class_Key_ExA: TCM_Open_Class_Key_ExA;
  CM_Open_Class_Key_ExW: TCM_Open_Class_Key_ExW;
  CM_Open_Class_Key_Ex: TCM_Open_Class_Key_Ex;
  CM_Open_DevNode_Key: TCM_Open_DevNode_Key;
  CM_Open_DevInst_Key: TCM_Open_DevInst_Key;
  CM_Open_DevNode_Key_Ex: TCM_Open_DevNode_Key_Ex;
  CM_Open_DevInst_Key_Ex: TCM_Open_DevInst_Key_Ex;
  CM_Query_Arbitrator_Free_Data: TCM_Query_Arbitrator_Free_Data;
  CM_Query_Arbitrator_Free_Data_Ex: TCM_Query_Arbitrator_Free_Data_Ex;
  CM_Query_Arbitrator_Free_Size: TCM_Query_Arbitrator_Free_Size;
  CM_Query_Arbitrator_Free_Size_Ex: TCM_Query_Arbitrator_Free_Size_Ex;
  CM_Query_Remove_SubTree: TCM_Query_Remove_SubTree;
  CM_Query_Remove_SubTree_Ex: TCM_Query_Remove_SubTree_Ex;
  {$IFDEF WIN2000_UP}
  CM_Query_And_Remove_SubTreeA: TCM_Query_And_Remove_SubTreeA;
  CM_Query_And_Remove_SubTreeW: TCM_Query_And_Remove_SubTreeW;
  CM_Query_And_Remove_SubTree: TCM_Query_And_Remove_SubTree;
  CM_Query_And_Remove_SubTree_ExA: TCM_Query_And_Remove_SubTree_ExA;
  CM_Query_And_Remove_SubTree_ExW: TCM_Query_And_Remove_SubTree_ExW;
  CM_Query_And_Remove_SubTree_Ex: TCM_Query_And_Remove_SubTree_Ex;
  CM_Request_Device_EjectA: TCM_Request_Device_EjectA;
  CM_Request_Device_EjectW: TCM_Request_Device_EjectW;
  CM_Request_Device_Eject: TCM_Request_Device_Eject;
  CM_Request_Device_Eject_ExA: TCM_Request_Device_Eject_ExA;
  CM_Request_Device_Eject_ExW: TCM_Request_Device_Eject_ExW;
  CM_Request_Device_Eject_Ex: TCM_Request_Device_Eject_Ex;
  {$ENDIF WIN2000_UP}
  CM_Reenumerate_DevNode: TCM_Reenumerate_DevNode;
  CM_Reenumerate_DevInst: TCM_Reenumerate_DevInst;
  CM_Reenumerate_DevNode_Ex: TCM_Reenumerate_DevNode_Ex;
  CM_Reenumerate_DevInst_Ex: TCM_Reenumerate_DevInst_Ex;
  {$IFNDEF WINNT4}
  CM_Register_Device_InterfaceA: TCM_Register_Device_InterfaceA;
  CM_Register_Device_InterfaceW: TCM_Register_Device_InterfaceW;
  CM_Register_Device_Interface: TCM_Register_Device_Interface;
  CM_Register_Device_Interface_ExA: TCM_Register_Device_Interface_ExA;
  CM_Register_Device_Interface_ExW: TCM_Register_Device_Interface_ExW;
  CM_Register_Device_Interface_Ex: TCM_Register_Device_Interface_Ex;
  CM_Set_DevNode_Problem_Ex: TCM_Set_DevNode_Problem_Ex;
  CM_Set_DevInst_Problem_Ex: TCM_Set_DevInst_Problem_Ex;
  CM_Set_DevNode_Problem: TCM_Set_DevNode_Problem;
  CM_Set_DevInst_Problem: TCM_Set_DevInst_Problem;
  CM_Unregister_Device_InterfaceA: TCM_Unregister_Device_InterfaceA;
  CM_Unregister_Device_InterfaceW: TCM_Unregister_Device_InterfaceW;
  CM_Unregister_Device_Interface: TCM_Unregister_Device_Interface;
  CM_Unregister_Device_Interface_ExA: TCM_Unregister_Device_Interface_ExA;
  CM_Unregister_Device_Interface_ExW: TCM_Unregister_Device_Interface_ExW;
  CM_Unregister_Device_Interface_Ex: TCM_Unregister_Device_Interface_Ex;
  CM_Register_Device_Driver: TCM_Register_Device_Driver;
  CM_Register_Device_Driver_Ex: TCM_Register_Device_Driver_Ex;
  {$ENDIF !WINNT4}
  CM_Remove_SubTree: TCM_Remove_SubTree;
  CM_Remove_SubTree_Ex: TCM_Remove_SubTree_Ex;
  CM_Set_DevNode_Registry_PropertyA: TCM_Set_DevNode_Registry_PropertyA;
  CM_Set_DevNode_Registry_PropertyW: TCM_Set_DevNode_Registry_PropertyW;
  CM_Set_DevNode_Registry_Property: TCM_Set_DevNode_Registry_Property;
  CM_Set_DevNode_Registry_Property_ExA: TCM_Set_DevNode_Registry_Property_ExA;
  CM_Set_DevNode_Registry_Property_ExW: TCM_Set_DevNode_Registry_Property_ExW;
  CM_Set_DevNode_Registry_Property_Ex: TCM_Set_DevNode_Registry_Property_Ex;
  CM_Set_DevInst_Registry_PropertyA: TCM_Set_DevInst_Registry_PropertyA;
  CM_Set_DevInst_Registry_PropertyW: TCM_Set_DevInst_Registry_PropertyW;
  CM_Set_DevInst_Registry_Property: TCM_Set_DevInst_Registry_Property;
  CM_Set_DevInst_Registry_Property_ExA: TCM_Set_DevInst_Registry_Property_ExA;
  CM_Set_DevInst_Registry_Property_ExW: TCM_Set_DevInst_Registry_Property_ExW;
  CM_Set_DevInst_Registry_Property_Ex: TCM_Set_DevInst_Registry_Property_Ex;
  {$IFNDEF WINNT4}
  CM_Is_Dock_Station_Present: TCM_Is_Dock_Station_Present;
  {$ENDIF !WINNT4}
  {$IFDEF WIN2000_UP}
  CM_Is_Dock_Station_Present_Ex: TCM_Is_Dock_Station_Present_Ex;
  {$ENDIF WIN2000_UP}
  {$IFNDEF WINNT4}
  CM_Request_Eject_PC: TCM_Request_Eject_PC;
  {$ENDIF !WINNT4}
  {$IFDEF WIN2000_UP}
  CM_Request_Eject_PC_Ex: TCM_Request_Eject_PC_Ex;
  {$ENDIF WIN2000_UP}
  CM_Set_HW_Prof_FlagsA: TCM_Set_HW_Prof_FlagsA;
  CM_Set_HW_Prof_FlagsW: TCM_Set_HW_Prof_FlagsW;
  CM_Set_HW_Prof_Flags: TCM_Set_HW_Prof_Flags;
  CM_Set_HW_Prof_Flags_ExA: TCM_Set_HW_Prof_Flags_ExA;
  CM_Set_HW_Prof_Flags_ExW: TCM_Set_HW_Prof_Flags_ExW;
  CM_Set_HW_Prof_Flags_Ex: TCM_Set_HW_Prof_Flags_Ex;
  CM_Setup_DevNode: TCM_Setup_DevNode;
  CM_Setup_DevInst: TCM_Setup_DevInst;
  CM_Setup_DevNode_Ex: TCM_Setup_DevNode_Ex;
  CM_Setup_DevInst_Ex: TCM_Setup_DevInst_Ex;
  CM_Test_Range_Available: TCM_Test_Range_Available;
  CM_Uninstall_DevNode: TCM_Uninstall_DevNode;
  CM_Uninstall_DevInst: TCM_Uninstall_DevInst;
  CM_Uninstall_DevNode_Ex: TCM_Uninstall_DevNode_Ex;
  CM_Uninstall_DevInst_Ex: TCM_Uninstall_DevInst_Ex;
  CM_Run_Detection: TCM_Run_Detection;
  CM_Run_Detection_Ex: TCM_Run_Detection_Ex;
  CM_Set_HW_Prof: TCM_Set_HW_Prof;
  CM_Set_HW_Prof_Ex: TCM_Set_HW_Prof_Ex;
  {$IFDEF WIN2000_UP}
  CM_Query_Resource_Conflict_List: TCM_Query_Resource_Conflict_List;
  CM_Free_Resource_Conflict_Handle: TCM_Free_Resource_Conflict_Handle;
  CM_Get_Resource_Conflict_Count: TCM_Get_Resource_Conflict_Count;
  CM_Get_Resource_Conflict_DetailsA: TCM_Get_Resource_Conflict_DetailsA;
  CM_Get_Resource_Conflict_DetailsW: TCM_Get_Resource_Conflict_DetailsW;
  CM_Get_Resource_Conflict_Details: TCM_Get_Resource_Conflict_Details;
  CM_Get_Class_Registry_PropertyA: TCM_Get_Class_Registry_PropertyA;
  CM_Get_Class_Registry_PropertyW: TCM_Get_Class_Registry_PropertyW;
  CM_Get_Class_Registry_Property: TCM_Get_Class_Registry_Property;
  CM_Set_Class_Registry_PropertyA: TCM_Set_Class_Registry_PropertyA;
  CM_Set_Class_Registry_PropertyW: TCM_Set_Class_Registry_PropertyW;
  CM_Set_Class_Registry_Property: TCM_Set_Class_Registry_Property;
  CM_WaitNoPendingInstallEvents: TCM_WaitNoPendingInstallEvents;
  CMP_WaitNoPendingInstallEvents: TCMP_WaitNoPendingInstallEvents;
  {$ENDIF WIN2000_UP}

{$ENDIF !CFGMGR32_LINKONREQUEST}

//--------------------------------------------------------------
// Configuration Manager return status codes
//--------------------------------------------------------------

const
  CR_SUCCESS                  = $00000000;
  {$EXTERNALSYM CR_SUCCESS}
  CR_DEFAULT                  = $00000001;
  {$EXTERNALSYM CR_DEFAULT}
  CR_OUT_OF_MEMORY            = $00000002;
  {$EXTERNALSYM CR_OUT_OF_MEMORY}
  CR_INVALID_POINTER          = $00000003;
  {$EXTERNALSYM CR_INVALID_POINTER}
  CR_INVALID_FLAG             = $00000004;
  {$EXTERNALSYM CR_INVALID_FLAG}
  CR_INVALID_DEVNODE          = $00000005;
  {$EXTERNALSYM CR_INVALID_DEVNODE}
  CR_INVALID_DEVINST          = CR_INVALID_DEVNODE;
  {$EXTERNALSYM CR_INVALID_DEVINST}
  CR_INVALID_RES_DES          = $00000006;
  {$EXTERNALSYM CR_INVALID_RES_DES}
  CR_INVALID_LOG_CONF         = $00000007;
  {$EXTERNALSYM CR_INVALID_LOG_CONF}
  CR_INVALID_ARBITRATOR       = $00000008;
  {$EXTERNALSYM CR_INVALID_ARBITRATOR}
  CR_INVALID_NODELIST         = $00000009;
  {$EXTERNALSYM CR_INVALID_NODELIST}
  CR_DEVNODE_HAS_REQS         = $0000000A;
  {$EXTERNALSYM CR_DEVNODE_HAS_REQS}
  CR_DEVINST_HAS_REQS         = CR_DEVNODE_HAS_REQS;
  {$EXTERNALSYM CR_DEVINST_HAS_REQS}
  CR_INVALID_RESOURCEID       = $0000000B;
  {$EXTERNALSYM CR_INVALID_RESOURCEID}
  CR_DLVXD_NOT_FOUND          = $0000000C;   // WIN 95 ONLY
  {$EXTERNALSYM CR_DLVXD_NOT_FOUND}
  CR_NO_SUCH_DEVNODE          = $0000000D;
  {$EXTERNALSYM CR_NO_SUCH_DEVNODE}
  CR_NO_SUCH_DEVINST          = CR_NO_SUCH_DEVNODE;
  {$EXTERNALSYM CR_NO_SUCH_DEVINST}
  CR_NO_MORE_LOG_CONF         = $0000000E;
  {$EXTERNALSYM CR_NO_MORE_LOG_CONF}
  CR_NO_MORE_RES_DES          = $0000000F;
  {$EXTERNALSYM CR_NO_MORE_RES_DES}
  CR_ALREADY_SUCH_DEVNODE     = $00000010;
  {$EXTERNALSYM CR_ALREADY_SUCH_DEVNODE}
  CR_ALREADY_SUCH_DEVINST     = CR_ALREADY_SUCH_DEVNODE;
  {$EXTERNALSYM CR_ALREADY_SUCH_DEVINST}
  CR_INVALID_RANGE_LIST       = $00000011;
  {$EXTERNALSYM CR_INVALID_RANGE_LIST}
  CR_INVALID_RANGE            = $00000012;
  {$EXTERNALSYM CR_INVALID_RANGE}
  CR_FAILURE                  = $00000013;
  {$EXTERNALSYM CR_FAILURE}
  CR_NO_SUCH_LOGICAL_DEV      = $00000014;
  {$EXTERNALSYM CR_NO_SUCH_LOGICAL_DEV}
  CR_CREATE_BLOCKED           = $00000015;
  {$EXTERNALSYM CR_CREATE_BLOCKED}
  CR_NOT_SYSTEM_VM            = $00000016;   // WIN 95 ONLY
  {$EXTERNALSYM CR_NOT_SYSTEM_VM}
  CR_REMOVE_VETOED            = $00000017;
  {$EXTERNALSYM CR_REMOVE_VETOED}
  CR_APM_VETOED               = $00000018;
  {$EXTERNALSYM CR_APM_VETOED}
  CR_INVALID_LOAD_TYPE        = $00000019;
  {$EXTERNALSYM CR_INVALID_LOAD_TYPE}
  CR_BUFFER_SMALL             = $0000001A;
  {$EXTERNALSYM CR_BUFFER_SMALL}
  CR_NO_ARBITRATOR            = $0000001B;
  {$EXTERNALSYM CR_NO_ARBITRATOR}
  CR_NO_REGISTRY_HANDLE       = $0000001C;
  {$EXTERNALSYM CR_NO_REGISTRY_HANDLE}
  CR_REGISTRY_ERROR           = $0000001D;
  {$EXTERNALSYM CR_REGISTRY_ERROR}
  CR_INVALID_DEVICE_ID        = $0000001E;
  {$EXTERNALSYM CR_INVALID_DEVICE_ID}
  CR_INVALID_DATA             = $0000001F;
  {$EXTERNALSYM CR_INVALID_DATA}
  CR_INVALID_API              = $00000020;
  {$EXTERNALSYM CR_INVALID_API}
  CR_DEVLOADER_NOT_READY      = $00000021;
  {$EXTERNALSYM CR_DEVLOADER_NOT_READY}
  CR_NEED_RESTART             = $00000022;
  {$EXTERNALSYM CR_NEED_RESTART}
  CR_NO_MORE_HW_PROFILES      = $00000023;
  {$EXTERNALSYM CR_NO_MORE_HW_PROFILES}
  CR_DEVICE_NOT_THERE         = $00000024;
  {$EXTERNALSYM CR_DEVICE_NOT_THERE}
  CR_NO_SUCH_VALUE            = $00000025;
  {$EXTERNALSYM CR_NO_SUCH_VALUE}
  CR_WRONG_TYPE               = $00000026;
  {$EXTERNALSYM CR_WRONG_TYPE}
  CR_INVALID_PRIORITY         = $00000027;
  {$EXTERNALSYM CR_INVALID_PRIORITY}
  CR_NOT_DISABLEABLE          = $00000028;
  {$EXTERNALSYM CR_NOT_DISABLEABLE}
  CR_FREE_RESOURCES           = $00000029;
  {$EXTERNALSYM CR_FREE_RESOURCES}
  CR_QUERY_VETOED             = $0000002A;
  {$EXTERNALSYM CR_QUERY_VETOED}
  CR_CANT_SHARE_IRQ           = $0000002B;
  {$EXTERNALSYM CR_CANT_SHARE_IRQ}
  CR_NO_DEPENDENT             = $0000002C;
  {$EXTERNALSYM CR_NO_DEPENDENT}
  CR_SAME_RESOURCES           = $0000002D;
  {$EXTERNALSYM CR_SAME_RESOURCES}
  CR_NO_SUCH_REGISTRY_KEY     = $0000002E;
  {$EXTERNALSYM CR_NO_SUCH_REGISTRY_KEY}
  CR_INVALID_MACHINENAME      = $0000002F;   // NT ONLY
  {$EXTERNALSYM CR_INVALID_MACHINENAME}
  CR_REMOTE_COMM_FAILURE      = $00000030;   // NT ONLY
  {$EXTERNALSYM CR_REMOTE_COMM_FAILURE}
  CR_MACHINE_UNAVAILABLE      = $00000031;   // NT ONLY
  {$EXTERNALSYM CR_MACHINE_UNAVAILABLE}
  CR_NO_CM_SERVICES           = $00000032;   // NT ONLY
  {$EXTERNALSYM CR_NO_CM_SERVICES}
  CR_ACCESS_DENIED            = $00000033;   // NT ONLY
  {$EXTERNALSYM CR_ACCESS_DENIED}
  CR_CALL_NOT_IMPLEMENTED     = $00000034;
  {$EXTERNALSYM CR_CALL_NOT_IMPLEMENTED}
  CR_INVALID_PROPERTY         = $00000035;
  {$EXTERNALSYM CR_INVALID_PROPERTY}
  CR_DEVICE_INTERFACE_ACTIVE  = $00000036;
  {$EXTERNALSYM CR_DEVICE_INTERFACE_ACTIVE}
  CR_NO_SUCH_DEVICE_INTERFACE = $00000037;
  {$EXTERNALSYM CR_NO_SUCH_DEVICE_INTERFACE}
  CR_INVALID_REFERENCE_STRING = $00000038;
  {$EXTERNALSYM CR_INVALID_REFERENCE_STRING}
  CR_INVALID_CONFLICT_LIST    = $00000039;
  {$EXTERNALSYM CR_INVALID_CONFLICT_LIST}
  CR_INVALID_INDEX            = $0000003A;
  {$EXTERNALSYM CR_INVALID_INDEX}
  CR_INVALID_STRUCTURE_SIZE   = $0000003B;
  {$EXTERNALSYM CR_INVALID_STRUCTURE_SIZE}
  NUM_CR_RESULTS              = $0000003C;
  {$EXTERNALSYM NUM_CR_RESULTS}

function IsConfigManagerApiLoaded: Boolean;
function LoadConfigManagerApi: Boolean;
procedure UnloadConfigManagerApi;

implementation

{$IFDEF CFGMGR32_LINKONREQUEST}
uses
  ModuleLoader;
{$ENDIF CFGMGR32_LINKONREQUEST}

const
  CfgMgrDllName = 'cfgmgr32.dll';
  SetupApiDllName = 'SETUPAPI.DLL';
  {$IFDEF UNICODE}
  NameSuffix = 'W';
  {$ELSE}
  NameSuffix = 'A';
  {$ENDIF UNICODE}

{$IFDEF CFGMGR32_LINKONREQUEST}
var
  CfgMgrApiLib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
  CfgMgrApiLoadCount: Integer = 0;
{$ENDIF CFGMGR32_LINKONREQUEST}

function IsConfigManagerApiLoaded: Boolean;
begin
  {$IFDEF CFGMGR32_LINKONREQUEST}
  Result := CfgMgrApiLib <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE}
  Result := True;
  {$ENDIF CFGMGR32_LINKONREQUEST}
end;

function LoadConfigManagerApi: Boolean;
begin
  Result := LoadSetupApi;
  if not Result then
    Exit;
  {$IFDEF CFGMGR32_LINKONREQUEST}
  Inc(CfgMgrApiLoadCount);
  if CfgMgrApiLoadCount > 1 then
    Exit;
  Result := LoadModule(CfgMgrApiLib, CfgMgrDllName);
  if Result then
  begin
    @CM_Add_Empty_Log_Conf := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_Empty_Log_Conf', Result);
    @CM_Add_Empty_Log_Conf_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_Empty_Log_Conf_Ex', Result);
    @CM_Add_IDA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_IDA', Result);
    @CM_Add_IDW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_IDW', Result);
    @CM_Add_ID := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_ID' + NameSuffix, Result);
    @CM_Add_ID_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_ID_ExA', Result);
    @CM_Add_ID_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_ID_ExW', Result);
    @CM_Add_ID_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_ID_Ex' + NameSuffix, Result);
    @CM_Add_Range := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_Range', Result);
    @CM_Add_Res_Des := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_Res_Des', Result);
    @CM_Add_Res_Des_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Add_Res_Des_Ex', Result);
    @CM_Connect_MachineA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Connect_MachineA', Result);
    @CM_Connect_MachineW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Connect_MachineW', Result);
    @CM_Connect_Machine := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Connect_Machine' + NameSuffix, Result);
    @CM_Create_DevNodeA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNodeA', Result);
    @CM_Create_DevNodeW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNodeW', Result);
    @CM_Create_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode' + NameSuffix, Result);
    @CM_Create_DevNode_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_ExA', Result);
    @CM_Create_DevNode_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_ExW', Result);
    @CM_Create_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_Ex' + NameSuffix, Result);
    @CM_Create_DevInstA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNodeA', Result);
    @CM_Create_DevInstW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNodeW', Result);
    @CM_Create_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode' + NameSuffix, Result);
    @CM_Create_DevInst_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_ExA', Result);
    @CM_Create_DevInst_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_ExW', Result);
    @CM_Create_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_DevNode_Ex' + NameSuffix, Result);
    @CM_Create_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Create_Range_List', Result);
    @CM_Delete_Class_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_Class_Key', Result);
    @CM_Delete_Class_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_Class_Key_Ex', Result);
    @CM_Delete_DevNode_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_DevNode_Key', Result);
    @CM_Delete_DevNode_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_DevNode_Key_Ex', Result);
    @CM_Delete_DevInst_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_DevNode_Key', Result);
    @CM_Delete_DevInst_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_DevNode_Key_Ex', Result);
    @CM_Delete_Range := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Delete_Range', Result);
    @CM_Detect_Resource_Conflict := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Detect_Resource_Conflict', Result);
    @CM_Detect_Resource_Conflict_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Detect_Resource_Conflict_Ex', Result);
    @CM_Disable_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Disable_DevNode', Result);
    @CM_Disable_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Disable_DevNode_Ex', Result);
    @CM_Disable_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Disable_DevNode', Result);
    @CM_Disable_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Disable_DevNode_Ex', Result);
    @CM_Disconnect_Machine := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Disconnect_Machine', Result);
    @CM_Dup_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Dup_Range_List', Result);
    @CM_Enable_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enable_DevNode', Result);
    @CM_Enable_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enable_DevNode_Ex', Result);
    @CM_Enable_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enable_DevNode', Result);
    @CM_Enable_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enable_DevNode_Ex', Result);
    @CM_Enumerate_Classes := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Classes', Result);
    @CM_Enumerate_Classes_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Classes_Ex', Result);
    @CM_Enumerate_EnumeratorsA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_EnumeratorsA', Result);
    @CM_Enumerate_EnumeratorsW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_EnumeratorsW', Result);
    @CM_Enumerate_Enumerators := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Enumerators' + NameSuffix, Result);
    @CM_Enumerate_Enumerators_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Enumerators_ExA', Result);
    @CM_Enumerate_Enumerators_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Enumerators_ExW', Result);
    @CM_Enumerate_Enumerators_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Enumerate_Enumerators_Ex' + NameSuffix, Result);
    @CM_Find_Range := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Find_Range', Result);
    @CM_First_Range := GetModuleSymbolEx(CfgMgrApiLib, 'CM_First_Range', Result);
    @CM_Free_Log_Conf := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Log_Conf', Result);
    @CM_Free_Log_Conf_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Log_Conf_Ex', Result);
    @CM_Free_Log_Conf_Handle := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Log_Conf_Handle', Result);
    @CM_Free_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Range_List', Result);
    @CM_Free_Res_Des := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Res_Des', Result);
    @CM_Free_Res_Des_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Res_Des_Ex', Result);
    @CM_Free_Res_Des_Handle := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Res_Des_Handle', Result);
    @CM_Get_Child := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Child', Result);
    @CM_Get_Child_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Child_Ex', Result);
    @CM_Get_Class_NameA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_NameA', Result);
    @CM_Get_Class_NameW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_NameW', Result);
    @CM_Get_Class_Name := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Name' + NameSuffix, Result);
    @CM_Get_Class_Name_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Name_ExA', Result);
    @CM_Get_Class_Name_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Name_ExW', Result);
    @CM_Get_Class_Name_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Name_Ex' + NameSuffix, Result);
    @CM_Get_Class_Key_NameA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_NameA', Result);
    @CM_Get_Class_Key_NameW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_NameW', Result);
    @CM_Get_Class_Key_Name := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_Name' + NameSuffix, Result);
    @CM_Get_Class_Key_Name_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_Name_ExA', Result);
    @CM_Get_Class_Key_Name_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_Name_ExW', Result);
    @CM_Get_Class_Key_Name_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Key_Name_Ex' + NameSuffix, Result);
    @CM_Get_Depth := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Depth', Result);
    @CM_Get_Depth_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Depth_Ex', Result);
    @CM_Get_Device_IDA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_IDA', Result);
    @CM_Get_Device_IDW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_IDW', Result);
    @CM_Get_Device_ID := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID' + NameSuffix, Result);
    @CM_Get_Device_ID_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_ExA', Result);
    @CM_Get_Device_ID_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_ExW', Result);
    @CM_Get_Device_ID_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_Ex' + NameSuffix, Result);
    @CM_Get_Device_ID_ListA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_ListA', Result);
    @CM_Get_Device_ID_ListW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_ListW', Result);
    @CM_Get_Device_ID_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List' + NameSuffix, Result);
    @CM_Get_Device_ID_List_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_ExA', Result);
    @CM_Get_Device_ID_List_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_ExW', Result);
    @CM_Get_Device_ID_List_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_Ex' + NameSuffix, Result);
    @CM_Get_Device_ID_List_SizeA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_SizeA', Result);
    @CM_Get_Device_ID_List_SizeW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_SizeW', Result);
    @CM_Get_Device_ID_List_Size := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_Size' + NameSuffix, Result);
    @CM_Get_Device_ID_List_Size_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_Size_ExA', Result);
    @CM_Get_Device_ID_List_Size_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_Size_ExW', Result);
    @CM_Get_Device_ID_List_Size_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_List_Size_Ex' + NameSuffix, Result);
    @CM_Get_Device_ID_Size := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_Size', Result);
    @CM_Get_Device_ID_Size_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_ID_Size_Ex', Result);
    @CM_Get_DevNode_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_PropertyA', Result);
    @CM_Get_DevNode_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_PropertyW', Result);
    @CM_Get_DevNode_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property' + NameSuffix, Result);
    @CM_Get_DevNode_Registry_Property_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_ExA', Result);
    @CM_Get_DevNode_Registry_Property_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_ExW', Result);
    @CM_Get_DevNode_Registry_Property_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_Ex' + NameSuffix, Result);
    @CM_Get_DevInst_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_PropertyA', Result);
    @CM_Get_DevInst_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_PropertyW', Result);
    @CM_Get_DevInst_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property' + NameSuffix, Result);
    @CM_Get_DevInst_Registry_Property_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_ExA', Result);
    @CM_Get_DevInst_Registry_Property_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_ExW', Result);
    @CM_Get_DevInst_Registry_Property_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Registry_Property_Ex' + NameSuffix, Result);
    {$IFDEF WINXP_UP}
    @CM_Get_DevNode_Custom_PropertyA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_PropertyA', Result);
    @CM_Get_DevNode_Custom_PropertyW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_PropertyW', Result);
    @CM_Get_DevNode_Custom_Property := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property' + NameSuffix, Result);
    @CM_Get_DevNode_Custom_Property_ExA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_ExA', Result);
    @CM_Get_DevNode_Custom_Property_ExW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_ExW', Result);
    @CM_Get_DevNode_Custom_Property_Ex := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_Ex' + NameSuffix, Result);
    @CM_Get_DevInst_Custom_PropertyA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_PropertyA', Result);
    @CM_Get_DevInst_Custom_PropertyW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_PropertyW', Result);
    @CM_Get_DevInst_Custom_Property := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property' + NameSuffix, Result);
    @CM_Get_DevInst_Custom_Property_ExA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_ExA', Result);
    @CM_Get_DevInst_Custom_Property_ExW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_ExW', Result);
    @CM_Get_DevInst_Custom_Property_Ex := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Get_DevNode_Custom_Property_Ex' + NameSuffix, Result);
    {$ENDIF WINXP_UP}
    @CM_Get_DevNode_Status := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Status', Result);
    @CM_Get_DevInst_Status := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Status', Result);
    @CM_Get_DevNode_Status_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Status_Ex', Result);
    @CM_Get_DevInst_Status_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_DevNode_Status_Ex', Result);
    @CM_Get_First_Log_Conf := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_First_Log_Conf', Result);
    @CM_Get_First_Log_Conf_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_First_Log_Conf_Ex', Result);
    @CM_Get_Global_State := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Global_State', Result);
    @CM_Get_Global_State_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Global_State_Ex', Result);
    @CM_Get_Hardware_Profile_InfoA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_InfoA', Result);
    @CM_Get_Hardware_Profile_InfoW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_InfoW', Result);
    @CM_Get_Hardware_Profile_Info := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_Info' + NameSuffix, Result);
    @CM_Get_Hardware_Profile_Info_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_Info_ExA', Result);
    @CM_Get_Hardware_Profile_Info_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_Info_ExW', Result);
    @CM_Get_Hardware_Profile_Info_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Hardware_Profile_Info_Ex' + NameSuffix, Result);
    @CM_Get_HW_Prof_FlagsA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_FlagsA', Result);
    @CM_Get_HW_Prof_FlagsW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_FlagsW', Result);
    @CM_Get_HW_Prof_Flags := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_Flags' + NameSuffix, Result);
    @CM_Get_HW_Prof_Flags_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_Flags_ExA', Result);
    @CM_Get_HW_Prof_Flags_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_Flags_ExW', Result);
    @CM_Get_HW_Prof_Flags_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_HW_Prof_Flags_Ex' + NameSuffix, Result);
    {$IFNDEF WINNT4}
    @CM_Get_Device_Interface_AliasA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_AliasA', Result);
    @CM_Get_Device_Interface_AliasW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_AliasW', Result);
    @CM_Get_Device_Interface_Alias := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_Alias' + NameSuffix, Result);
    @CM_Get_Device_Interface_Alias_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_Alias_ExA', Result);
    @CM_Get_Device_Interface_Alias_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_Alias_ExW', Result);
    @CM_Get_Device_Interface_Alias_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_Alias_Ex' + NameSuffix, Result);
    @CM_Get_Device_Interface_ListA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_ListA', Result);
    @CM_Get_Device_Interface_ListW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_ListW', Result);
    @CM_Get_Device_Interface_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List' + NameSuffix, Result);
    @CM_Get_Device_Interface_List_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_ExA', Result);
    @CM_Get_Device_Interface_List_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_ExW', Result);
    @CM_Get_Device_Interface_List_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_Ex' + NameSuffix, Result);
    @CM_Get_Device_Interface_List_SizeA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_SizeA', Result);
    @CM_Get_Device_Interface_List_SizeW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_SizeW', Result);
    @CM_Get_Device_Interface_List_Size := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_Size' + NameSuffix, Result);
    @CM_Get_Device_Interface_List_Size_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_Size_ExA', Result);
    @CM_Get_Device_Interface_List_Size_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_Size_ExW', Result);
    @CM_Get_Device_Interface_List_Size_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Device_Interface_List_Size_Ex' + NameSuffix, Result);
    @CM_Get_Log_Conf_Priority := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Log_Conf_Priority', Result);
    @CM_Get_Log_Conf_Priority_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Log_Conf_Priority_Ex', Result);
    {$ENDIF !WINNT4}
    @CM_Get_Next_Log_Conf := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Next_Log_Conf', Result);
    @CM_Get_Next_Log_Conf_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Next_Log_Conf_Ex', Result);
    @CM_Get_Parent := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Parent', Result);
    @CM_Get_Parent_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Parent_Ex', Result);
    @CM_Get_Res_Des_Data := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Res_Des_Data', Result);
    @CM_Get_Res_Des_Data_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Res_Des_Data_Ex', Result);
    @CM_Get_Res_Des_Data_Size := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Res_Des_Data_Size', Result);
    @CM_Get_Res_Des_Data_Size_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Res_Des_Data_Size_Ex', Result);
    @CM_Get_Sibling := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Sibling', Result);
    @CM_Get_Sibling_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Sibling_Ex', Result);
    @CM_Get_Version := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Version', Result);
    @CM_Get_Version_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Version_Ex', Result);
    {$IFDEF WINXP_UP}
    @CM_Is_Version_Available := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Is_Version_Available', Result);
    @CM_Is_Version_Available_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Is_Version_Available_Ex', Result);
    {$ENDIF WINXP_UP}
    @CM_Intersect_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Intersect_Range_List', Result);
    @CM_Invert_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Invert_Range_List', Result);
    @CM_Locate_DevNodeA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNodeA', Result);
    @CM_Locate_DevNodeW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNodeW', Result);
    @CM_Locate_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode' + NameSuffix, Result);
    @CM_Locate_DevNode_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_ExA', Result);
    @CM_Locate_DevNode_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_ExW', Result);
    @CM_Locate_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_Ex' + NameSuffix, Result);
    @CM_Locate_DevInstA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNodeA', Result);
    @CM_Locate_DevInstW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNodeW', Result);
    @CM_Locate_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode' + NameSuffix, Result);
    @CM_Locate_DevInst_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_ExA', Result);
    @CM_Locate_DevInst_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_ExW', Result);
    @CM_Locate_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Locate_DevNode_Ex' + NameSuffix, Result);
    @CM_Merge_Range_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Merge_Range_List', Result);
    @CM_Modify_Res_Des := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Modify_Res_Des', Result);
    @CM_Modify_Res_Des_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Modify_Res_Des_Ex', Result);
    @CM_Move_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Move_DevNode', Result);
    @CM_Move_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Move_DevNode', Result);
    @CM_Move_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Move_DevNode_Ex', Result);
    @CM_Move_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Move_DevNode_Ex', Result);
    @CM_Next_Range := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Next_Range', Result);
    @CM_Get_Next_Res_Des := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Next_Res_Des', Result);
    @CM_Get_Next_Res_Des_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Next_Res_Des_Ex', Result);
    @CM_Open_Class_KeyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_KeyA', Result);
    @CM_Open_Class_KeyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_KeyW', Result);
    @CM_Open_Class_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_Key' + NameSuffix, Result);
    @CM_Open_Class_Key_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_Key_ExA', Result);
    @CM_Open_Class_Key_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_Key_ExW', Result);
    @CM_Open_Class_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_Class_Key_Ex' + NameSuffix, Result);
    @CM_Open_DevNode_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_DevNode_Key', Result);
    @CM_Open_DevInst_Key := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_DevNode_Key', Result);
    @CM_Open_DevNode_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_DevNode_Key_Ex', Result);
    @CM_Open_DevInst_Key_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Open_DevNode_Key_Ex', Result);
    @CM_Query_Arbitrator_Free_Data := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Arbitrator_Free_Data', Result);
    @CM_Query_Arbitrator_Free_Data_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Arbitrator_Free_Data_Ex', Result);
    @CM_Query_Arbitrator_Free_Size := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Arbitrator_Free_Size', Result);
    @CM_Query_Arbitrator_Free_Size_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Arbitrator_Free_Size_Ex', Result);
    @CM_Query_Remove_SubTree := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Remove_SubTree', Result);
    @CM_Query_Remove_SubTree_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Remove_SubTree_Ex', Result);
    {$IFDEF WIN2000_UP}
    @CM_Query_And_Remove_SubTreeA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTreeA', Result);
    @CM_Query_And_Remove_SubTreeW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTreeW', Result);
    @CM_Query_And_Remove_SubTree := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTree' + NameSuffix, Result);
    @CM_Query_And_Remove_SubTree_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTree_ExA', Result);
    @CM_Query_And_Remove_SubTree_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTree_ExW', Result);
    @CM_Query_And_Remove_SubTree_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_And_Remove_SubTree_Ex' + NameSuffix, Result);
    @CM_Request_Device_EjectA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_EjectA', Result);
    @CM_Request_Device_EjectW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_EjectW', Result);
    @CM_Request_Device_Eject := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_Eject' + NameSuffix, Result);
    @CM_Request_Device_Eject_ExA := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_Eject_ExA', Result);
    @CM_Request_Device_Eject_ExW := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_Eject_ExW', Result);
    @CM_Request_Device_Eject_Ex := GetModuleSymbolEx(GetSetupApiModuleHandle, 'CM_Request_Device_Eject_Ex' + NameSuffix, Result);
    {$ENDIF WIN2000_UP}
    @CM_Reenumerate_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Reenumerate_DevNode', Result);
    @CM_Reenumerate_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Reenumerate_DevNode', Result);
    @CM_Reenumerate_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Reenumerate_DevNode_Ex', Result);
    @CM_Reenumerate_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Reenumerate_DevNode_Ex', Result);
    {$IFNDEF WINNT4}
    @CM_Register_Device_InterfaceA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_InterfaceA', Result);
    @CM_Register_Device_InterfaceW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_InterfaceW', Result);
    @CM_Register_Device_Interface := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Interface' + NameSuffix, Result);
    @CM_Register_Device_Interface_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Interface_ExA', Result);
    @CM_Register_Device_Interface_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Interface_ExW', Result);
    @CM_Register_Device_Interface_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Interface_Ex' + NameSuffix, Result);
    @CM_Set_DevNode_Problem_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Problem_Ex', Result);
    @CM_Set_DevInst_Problem_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Problem_Ex', Result);
    @CM_Set_DevNode_Problem := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Problem', Result);
    @CM_Set_DevInst_Problem := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Problem', Result);
    @CM_Unregister_Device_InterfaceA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_InterfaceA', Result);
    @CM_Unregister_Device_InterfaceW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_InterfaceW', Result);
    @CM_Unregister_Device_Interface := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_Interface' + NameSuffix, Result);
    @CM_Unregister_Device_Interface_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_Interface_ExA', Result);
    @CM_Unregister_Device_Interface_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_Interface_ExW', Result);
    @CM_Unregister_Device_Interface_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Unregister_Device_Interface_Ex' + NameSuffix, Result);
    @CM_Register_Device_Driver := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Driver', Result);
    @CM_Register_Device_Driver_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Register_Device_Driver_Ex', Result);
    {$ENDIF !WINNT4}
    @CM_Remove_SubTree := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Remove_SubTree', Result);
    @CM_Remove_SubTree_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Remove_SubTree_Ex', Result);
    @CM_Set_DevNode_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_PropertyA', Result);
    @CM_Set_DevNode_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_PropertyW', Result);
    @CM_Set_DevNode_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property' + NameSuffix, Result);
    @CM_Set_DevNode_Registry_Property_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_ExA', Result);
    @CM_Set_DevNode_Registry_Property_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_ExW', Result);
    @CM_Set_DevNode_Registry_Property_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_Ex' + NameSuffix, Result);
    @CM_Set_DevInst_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_PropertyA', Result);
    @CM_Set_DevInst_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_PropertyW', Result);
    @CM_Set_DevInst_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property' + NameSuffix, Result);
    @CM_Set_DevInst_Registry_Property_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_ExA', Result);
    @CM_Set_DevInst_Registry_Property_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_ExW', Result);
    @CM_Set_DevInst_Registry_Property_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_DevNode_Registry_Property_Ex' + NameSuffix, Result);
    {$IFNDEF WINNT4}
    @CM_Is_Dock_Station_Present := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Is_Dock_Station_Present', Result);
    {$ENDIF !WINNT4}
    {$IFDEF WIN2000_UP}
    @CM_Is_Dock_Station_Present_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Is_Dock_Station_Present_Ex', Result);
    {$ENDIF WIN2000_UP}
    {$IFNDEF WINNT4}
    @CM_Request_Eject_PC := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Request_Eject_PC', Result);
    {$ENDIF !WINNT4}
    {$IFDEF WIN2000_UP}
    @CM_Request_Eject_PC_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Request_Eject_PC_Ex', Result);
    {$ENDIF WIN2000_UP}
    @CM_Set_HW_Prof_FlagsA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_FlagsA', Result);
    @CM_Set_HW_Prof_FlagsW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_FlagsW', Result);
    @CM_Set_HW_Prof_Flags := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_Flags' + NameSuffix, Result);
    @CM_Set_HW_Prof_Flags_ExA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_Flags_ExA', Result);
    @CM_Set_HW_Prof_Flags_ExW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_Flags_ExW', Result);
    @CM_Set_HW_Prof_Flags_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_Flags_Ex' + NameSuffix, Result);
    @CM_Setup_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Setup_DevNode', Result);
    @CM_Setup_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Setup_DevNode', Result);
    @CM_Setup_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Setup_DevNode_Ex', Result);
    @CM_Setup_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Setup_DevNode_Ex', Result);
    @CM_Test_Range_Available := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Test_Range_Available', Result);
    @CM_Uninstall_DevNode := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Uninstall_DevNode', Result);
    @CM_Uninstall_DevInst := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Uninstall_DevNode', Result);
    @CM_Uninstall_DevNode_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Uninstall_DevNode_Ex', Result);
    @CM_Uninstall_DevInst_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Uninstall_DevNode_Ex', Result);
    @CM_Run_Detection := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Run_Detection', Result);
    @CM_Run_Detection_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Run_Detection_Ex', Result);
    @CM_Set_HW_Prof := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof', Result);
    @CM_Set_HW_Prof_Ex := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_HW_Prof_Ex', Result);
    {$IFDEF WIN2000_UP}
    @CM_Query_Resource_Conflict_List := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Query_Resource_Conflict_List', Result);
    @CM_Free_Resource_Conflict_Handle := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Free_Resource_Conflict_Handle', Result);
    @CM_Get_Resource_Conflict_Count := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Resource_Conflict_Count', Result);
    @CM_Get_Resource_Conflict_DetailsA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Resource_Conflict_DetailsA', Result);
    @CM_Get_Resource_Conflict_DetailsW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Resource_Conflict_DetailsW', Result);
    @CM_Get_Resource_Conflict_Details := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Resource_Conflict_Details' + NameSuffix, Result);
    @CM_Get_Class_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Registry_PropertyA', Result);
    @CM_Get_Class_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Registry_PropertyW', Result);
    @CM_Get_Class_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Get_Class_Registry_Property' + NameSuffix, Result);
    @CM_Set_Class_Registry_PropertyA := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_Class_Registry_PropertyA', Result);
    @CM_Set_Class_Registry_PropertyW := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_Class_Registry_PropertyW', Result);
    @CM_Set_Class_Registry_Property := GetModuleSymbolEx(CfgMgrApiLib, 'CM_Set_Class_Registry_Property' + NameSuffix, Result);
    @CM_WaitNoPendingInstallEvents := GetModuleSymbolEx(CfgMgrApiLib, 'CMP_WaitNoPendingInstallEvents', Result);
    @CMP_WaitNoPendingInstallEvents := GetModuleSymbolEx(CfgMgrApiLib, 'CMP_WaitNoPendingInstallEvents', Result);
    {$ENDIF WIN2000_UP}
    if not Result then
      UnloadConfigManagerApi;
  end;
  {$ELSE}
  Result := True;
  {$ENDIF CFGMGR32_LINKONREQUEST}
end;

procedure UnloadConfigManagerApi;
begin
  UnloadSetupApi;
  {$IFDEF CFGMGR32_LINKONREQUEST}
  Dec(CfgMgrApiLoadCount);
  if CfgMgrApiLoadCount > 0 then
    Exit;
  UnloadModule(CfgMgrApiLib);
  CM_Add_Empty_Log_Conf := nil;
  CM_Add_Empty_Log_Conf_Ex := nil;
  CM_Add_IDA := nil;
  CM_Add_IDW := nil;
  CM_Add_ID := nil;
  CM_Add_ID_ExA := nil;
  CM_Add_ID_ExW := nil;
  CM_Add_ID_Ex := nil;
  CM_Add_Range := nil;
  CM_Add_Res_Des := nil;
  CM_Add_Res_Des_Ex := nil;
  CM_Connect_MachineA := nil;
  CM_Connect_MachineW := nil;
  CM_Connect_Machine := nil;
  CM_Create_DevNodeA := nil;
  CM_Create_DevNodeW := nil;
  CM_Create_DevNode := nil;
  CM_Create_DevNode_ExA := nil;
  CM_Create_DevNode_ExW := nil;
  CM_Create_DevNode_Ex := nil;
  CM_Create_DevInstA := nil;
  CM_Create_DevInstW := nil;
  CM_Create_DevInst := nil;
  CM_Create_DevInst_ExA := nil;
  CM_Create_DevInst_ExW := nil;
  CM_Create_DevInst_Ex := nil;
  CM_Create_Range_List := nil;
  CM_Delete_Class_Key := nil;
  CM_Delete_Class_Key_Ex := nil;
  CM_Delete_DevNode_Key := nil;
  CM_Delete_DevNode_Key_Ex := nil;
  CM_Delete_DevInst_Key := nil;
  CM_Delete_DevInst_Key_Ex := nil;
  CM_Delete_Range := nil;
  CM_Detect_Resource_Conflict := nil;
  CM_Detect_Resource_Conflict_Ex := nil;
  CM_Disable_DevNode := nil;
  CM_Disable_DevNode_Ex := nil;
  CM_Disable_DevInst := nil;
  CM_Disable_DevInst_Ex := nil;
  CM_Disconnect_Machine := nil;
  CM_Dup_Range_List := nil;
  CM_Enable_DevNode := nil;
  CM_Enable_DevNode_Ex := nil;
  CM_Enable_DevInst := nil;
  CM_Enable_DevInst_Ex := nil;
  CM_Enumerate_Classes := nil;
  CM_Enumerate_Classes_Ex := nil;
  CM_Enumerate_EnumeratorsA := nil;
  CM_Enumerate_EnumeratorsW := nil;
  CM_Enumerate_Enumerators := nil;
  CM_Enumerate_Enumerators_ExA := nil;
  CM_Enumerate_Enumerators_ExW := nil;
  CM_Enumerate_Enumerators_Ex := nil;
  CM_Find_Range := nil;
  CM_First_Range := nil;
  CM_Free_Log_Conf := nil;
  CM_Free_Log_Conf_Ex := nil;
  CM_Free_Log_Conf_Handle := nil;
  CM_Free_Range_List := nil;
  CM_Free_Res_Des := nil;
  CM_Free_Res_Des_Ex := nil;
  CM_Free_Res_Des_Handle := nil;
  CM_Get_Child := nil;
  CM_Get_Child_Ex := nil;
  CM_Get_Class_NameA := nil;
  CM_Get_Class_NameW := nil;
  CM_Get_Class_Name := nil;
  CM_Get_Class_Name_ExA := nil;
  CM_Get_Class_Name_ExW := nil;
  CM_Get_Class_Name_Ex := nil;
  CM_Get_Class_Key_NameA := nil;
  CM_Get_Class_Key_NameW := nil;
  CM_Get_Class_Key_Name := nil;
  CM_Get_Class_Key_Name_ExA := nil;
  CM_Get_Class_Key_Name_ExW := nil;
  CM_Get_Class_Key_Name_Ex := nil;
  CM_Get_Depth := nil;
  CM_Get_Depth_Ex := nil;
  CM_Get_Device_IDA := nil;
  CM_Get_Device_IDW := nil;
  CM_Get_Device_ID := nil;
  CM_Get_Device_ID_ExA := nil;
  CM_Get_Device_ID_ExW := nil;
  CM_Get_Device_ID_Ex := nil;
  CM_Get_Device_ID_ListA := nil;
  CM_Get_Device_ID_ListW := nil;
  CM_Get_Device_ID_List := nil;
  CM_Get_Device_ID_List_ExA := nil;
  CM_Get_Device_ID_List_ExW := nil;
  CM_Get_Device_ID_List_Ex := nil;
  CM_Get_Device_ID_List_SizeA := nil;
  CM_Get_Device_ID_List_SizeW := nil;
  CM_Get_Device_ID_List_Size := nil;
  CM_Get_Device_ID_List_Size_ExA := nil;
  CM_Get_Device_ID_List_Size_ExW := nil;
  CM_Get_Device_ID_List_Size_Ex := nil;
  CM_Get_Device_ID_Size := nil;
  CM_Get_Device_ID_Size_Ex := nil;
  CM_Get_DevNode_Registry_PropertyA := nil;
  CM_Get_DevNode_Registry_PropertyW := nil;
  CM_Get_DevNode_Registry_Property := nil;
  CM_Get_DevNode_Registry_Property_ExA := nil;
  CM_Get_DevNode_Registry_Property_ExW := nil;
  CM_Get_DevNode_Registry_Property_Ex := nil;
  CM_Get_DevInst_Registry_PropertyA := nil;
  CM_Get_DevInst_Registry_PropertyW := nil;
  CM_Get_DevInst_Registry_Property := nil;
  CM_Get_DevInst_Registry_Property_ExA := nil;
  CM_Get_DevInst_Registry_Property_ExW := nil;
  CM_Get_DevInst_Registry_Property_Ex := nil;
  {$IFDEF WINXP_UP}
  CM_Get_DevNode_Custom_PropertyA := nil;
  CM_Get_DevNode_Custom_PropertyW := nil;
  CM_Get_DevNode_Custom_Property := nil;
  CM_Get_DevNode_Custom_Property_ExA := nil;
  CM_Get_DevNode_Custom_Property_ExW := nil;
  CM_Get_DevNode_Custom_Property_Ex := nil;
  CM_Get_DevInst_Custom_PropertyA := nil;
  CM_Get_DevInst_Custom_PropertyW := nil;
  CM_Get_DevInst_Custom_Property := nil;
  CM_Get_DevInst_Custom_Property_ExA := nil;
  CM_Get_DevInst_Custom_Property_ExW := nil;
  CM_Get_DevInst_Custom_Property_Ex := nil;
  {$ENDIF WINXP_UP}
  CM_Get_DevNode_Status := nil;
  CM_Get_DevInst_Status := nil;
  CM_Get_DevNode_Status_Ex := nil;
  CM_Get_DevInst_Status_Ex := nil;
  CM_Get_First_Log_Conf := nil;
  CM_Get_First_Log_Conf_Ex := nil;
  CM_Get_Global_State := nil;
  CM_Get_Global_State_Ex := nil;
  CM_Get_Hardware_Profile_InfoA := nil;
  CM_Get_Hardware_Profile_InfoW := nil;
  CM_Get_Hardware_Profile_Info := nil;
  CM_Get_Hardware_Profile_Info_ExA := nil;
  CM_Get_Hardware_Profile_Info_ExW := nil;
  CM_Get_Hardware_Profile_Info_Ex := nil;
  CM_Get_HW_Prof_FlagsA := nil;
  CM_Get_HW_Prof_FlagsW := nil;
  CM_Get_HW_Prof_Flags := nil;
  CM_Get_HW_Prof_Flags_ExA := nil;
  CM_Get_HW_Prof_Flags_ExW := nil;
  CM_Get_HW_Prof_Flags_Ex := nil;
  {$IFNDEF WINNT4}
  CM_Get_Device_Interface_AliasA := nil;
  CM_Get_Device_Interface_AliasW := nil;
  CM_Get_Device_Interface_Alias := nil;
  CM_Get_Device_Interface_Alias_ExA := nil;
  CM_Get_Device_Interface_Alias_ExW := nil;
  CM_Get_Device_Interface_Alias_Ex := nil;
  CM_Get_Device_Interface_ListA := nil;
  CM_Get_Device_Interface_ListW := nil;
  CM_Get_Device_Interface_List := nil;
  CM_Get_Device_Interface_List_ExA := nil;
  CM_Get_Device_Interface_List_ExW := nil;
  CM_Get_Device_Interface_List_Ex := nil;
  CM_Get_Device_Interface_List_SizeA := nil;
  CM_Get_Device_Interface_List_SizeW := nil;
  CM_Get_Device_Interface_List_Size := nil;
  CM_Get_Device_Interface_List_Size_ExA := nil;
  CM_Get_Device_Interface_List_Size_ExW := nil;
  CM_Get_Device_Interface_List_Size_Ex := nil;
  CM_Get_Log_Conf_Priority := nil;
  CM_Get_Log_Conf_Priority_Ex := nil;
  {$ENDIF !WINNT4}
  CM_Get_Next_Log_Conf := nil;
  CM_Get_Next_Log_Conf_Ex := nil;
  CM_Get_Parent := nil;
  CM_Get_Parent_Ex := nil;
  CM_Get_Res_Des_Data := nil;
  CM_Get_Res_Des_Data_Ex := nil;
  CM_Get_Res_Des_Data_Size := nil;
  CM_Get_Res_Des_Data_Size_Ex := nil;
  CM_Get_Sibling := nil;
  CM_Get_Sibling_Ex := nil;
  CM_Get_Version := nil;
  CM_Get_Version_Ex := nil;
  {$IFDEF WINXP_UP}
  CM_Is_Version_Available := nil;
  CM_Is_Version_Available_Ex := nil;
  {$ENDIF WINXP_UP}
  CM_Intersect_Range_List := nil;
  CM_Invert_Range_List := nil;
  CM_Locate_DevNodeA := nil;
  CM_Locate_DevNodeW := nil;
  CM_Locate_DevNode := nil;
  CM_Locate_DevNode_ExA := nil;
  CM_Locate_DevNode_ExW := nil;
  CM_Locate_DevNode_Ex := nil;
  CM_Locate_DevInstA := nil;
  CM_Locate_DevInstW := nil;
  CM_Locate_DevInst := nil;
  CM_Locate_DevInst_ExA := nil;
  CM_Locate_DevInst_ExW := nil;
  CM_Locate_DevInst_Ex := nil;
  CM_Merge_Range_List := nil;
  CM_Modify_Res_Des := nil;
  CM_Modify_Res_Des_Ex := nil;
  CM_Move_DevNode := nil;
  CM_Move_DevInst := nil;
  CM_Move_DevNode_Ex := nil;
  CM_Move_DevInst_Ex := nil;
  CM_Next_Range := nil;
  CM_Get_Next_Res_Des := nil;
  CM_Get_Next_Res_Des_Ex := nil;
  CM_Open_Class_KeyA := nil;
  CM_Open_Class_KeyW := nil;
  CM_Open_Class_Key := nil;
  CM_Open_Class_Key_ExA := nil;
  CM_Open_Class_Key_ExW := nil;
  CM_Open_Class_Key_Ex := nil;
  CM_Open_DevNode_Key := nil;
  CM_Open_DevInst_Key := nil;
  CM_Open_DevNode_Key_Ex := nil;
  CM_Open_DevInst_Key_Ex := nil;
  CM_Query_Arbitrator_Free_Data := nil;
  CM_Query_Arbitrator_Free_Data_Ex := nil;
  CM_Query_Arbitrator_Free_Size := nil;
  CM_Query_Arbitrator_Free_Size_Ex := nil;
  CM_Query_Remove_SubTree := nil;
  CM_Query_Remove_SubTree_Ex := nil;
  {$IFDEF WIN2000_UP}
  CM_Query_And_Remove_SubTreeA := nil;
  CM_Query_And_Remove_SubTreeW := nil;
  CM_Query_And_Remove_SubTree := nil;
  CM_Query_And_Remove_SubTree_ExA := nil;
  CM_Query_And_Remove_SubTree_ExW := nil;
  CM_Query_And_Remove_SubTree_Ex := nil;
  CM_Request_Device_EjectA := nil;
  CM_Request_Device_EjectW := nil;
  CM_Request_Device_Eject := nil;
  CM_Request_Device_Eject_ExA := nil;
  CM_Request_Device_Eject_ExW := nil;
  CM_Request_Device_Eject_Ex := nil;
  {$ENDIF WIN2000_UP}
  CM_Reenumerate_DevNode := nil;
  CM_Reenumerate_DevInst := nil;
  CM_Reenumerate_DevNode_Ex := nil;
  CM_Reenumerate_DevInst_Ex := nil;
  {$IFNDEF WINNT4}
  CM_Register_Device_InterfaceA := nil;
  CM_Register_Device_InterfaceW := nil;
  CM_Register_Device_Interface := nil;
  CM_Register_Device_Interface_ExA := nil;
  CM_Register_Device_Interface_ExW := nil;
  CM_Register_Device_Interface_Ex := nil;
  CM_Set_DevNode_Problem_Ex := nil;
  CM_Set_DevInst_Problem_Ex := nil;
  CM_Set_DevNode_Problem := nil;
  CM_Set_DevInst_Problem := nil;
  CM_Unregister_Device_InterfaceA := nil;
  CM_Unregister_Device_InterfaceW := nil;
  CM_Unregister_Device_Interface := nil;
  CM_Unregister_Device_Interface_ExA := nil;
  CM_Unregister_Device_Interface_ExW := nil;
  CM_Unregister_Device_Interface_Ex := nil;
  CM_Register_Device_Driver := nil;
  CM_Register_Device_Driver_Ex := nil;
  {$ENDIF !WINNT4}
  CM_Remove_SubTree := nil;
  CM_Remove_SubTree_Ex := nil;
  CM_Set_DevNode_Registry_PropertyA := nil;
  CM_Set_DevNode_Registry_PropertyW := nil;
  CM_Set_DevNode_Registry_Property := nil;
  CM_Set_DevNode_Registry_Property_ExA := nil;
  CM_Set_DevNode_Registry_Property_ExW := nil;
  CM_Set_DevNode_Registry_Property_Ex := nil;
  CM_Set_DevInst_Registry_PropertyA := nil;
  CM_Set_DevInst_Registry_PropertyW := nil;
  CM_Set_DevInst_Registry_Property := nil;
  CM_Set_DevInst_Registry_Property_ExA := nil;
  CM_Set_DevInst_Registry_Property_ExW := nil;
  CM_Set_DevInst_Registry_Property_Ex := nil;
  {$IFNDEF WINNT4}
  CM_Is_Dock_Station_Present := nil;
  {$ENDIF !WINNT4}
  {$IFDEF WIN2000_UP}
  CM_Is_Dock_Station_Present_Ex := nil;
  {$ENDIF WIN2000_UP}
  {$IFNDEF WINNT4}
  CM_Request_Eject_PC := nil;
  {$ENDIF !WINNT4}
  {$IFDEF WIN2000_UP}
  CM_Request_Eject_PC_Ex := nil;
  {$ENDIF WIN2000_UP}
  CM_Set_HW_Prof_FlagsA := nil;
  CM_Set_HW_Prof_FlagsW := nil;
  CM_Set_HW_Prof_Flags := nil;
  CM_Set_HW_Prof_Flags_ExA := nil;
  CM_Set_HW_Prof_Flags_ExW := nil;
  CM_Set_HW_Prof_Flags_Ex := nil;
  CM_Setup_DevNode := nil;
  CM_Setup_DevInst := nil;
  CM_Setup_DevNode_Ex := nil;
  CM_Setup_DevInst_Ex := nil;
  CM_Test_Range_Available := nil;
  CM_Uninstall_DevNode := nil;
  CM_Uninstall_DevInst := nil;
  CM_Uninstall_DevNode_Ex := nil;
  CM_Uninstall_DevInst_Ex := nil;
  CM_Run_Detection := nil;
  CM_Run_Detection_Ex := nil;
  CM_Set_HW_Prof := nil;
  CM_Set_HW_Prof_Ex := nil;
  {$IFDEF WIN2000_UP}
  CM_Query_Resource_Conflict_List := nil;
  CM_Free_Resource_Conflict_Handle := nil;
  CM_Get_Resource_Conflict_Count := nil;
  CM_Get_Resource_Conflict_DetailsA := nil;
  CM_Get_Resource_Conflict_DetailsW := nil;
  CM_Get_Resource_Conflict_Details := nil;
  CM_Get_Class_Registry_PropertyA := nil;
  CM_Get_Class_Registry_PropertyW := nil;
  CM_Get_Class_Registry_Property := nil;
  CM_Set_Class_Registry_PropertyA := nil;
  CM_Set_Class_Registry_PropertyW := nil;
  CM_Set_Class_Registry_Property := nil;
  CM_WaitNoPendingInstallEvents := nil;
  CMP_WaitNoPendingInstallEvents := nil;
  {$ENDIF WIN2000_UP}
  {$ENDIF CFGMGR32_LINKONREQUEST}
end;

{$IFNDEF CFGMGR32_LINKONREQUEST}

function CM_Add_Empty_Log_Conf; external CfgMgrDllName name 'CM_Add_Empty_Log_Conf';
function CM_Add_Empty_Log_Conf_Ex; external CfgMgrDllName name 'CM_Add_Empty_Log_Conf_Ex';
function CM_Add_ID; external CfgMgrDllName name 'CM_Add_ID' + NameSuffix;
function CM_Add_IDA; external CfgMgrDllName name 'CM_Add_IDA';
function CM_Add_IDW; external CfgMgrDllName name 'CM_Add_IDW';
function CM_Add_ID_Ex; external CfgMgrDllName name 'CM_Add_ID_Ex' + NameSuffix;
function CM_Add_ID_ExA; external CfgMgrDllName name 'CM_Add_ID_ExA';
function CM_Add_ID_ExW; external CfgMgrDllName name 'CM_Add_ID_ExW';
function CM_Add_Range; external CfgMgrDllName name 'CM_Add_Range';
function CM_Add_Res_Des; external CfgMgrDllName name 'CM_Add_Res_Des';
function CM_Add_Res_Des_Ex; external CfgMgrDllName name 'CM_Add_Res_Des_Ex';
function CM_Connect_Machine; external CfgMgrDllName name 'CM_Connect_Machine' + NameSuffix;
function CM_Connect_MachineA; external CfgMgrDllName name 'CM_Connect_MachineA';
function CM_Connect_MachineW; external CfgMgrDllName name 'CM_Connect_MachineW';
function CM_Create_DevNode; external CfgMgrDllName name 'CM_Create_DevNode' + NameSuffix;
function CM_Create_DevNodeA; external CfgMgrDllName name 'CM_Create_DevNodeA';
function CM_Create_DevNodeW; external CfgMgrDllName name 'CM_Create_DevNodeW';
function CM_Create_DevNode_Ex; external CfgMgrDllName name 'CM_Create_DevNode_Ex' + NameSuffix;
function CM_Create_DevNode_ExA; external CfgMgrDllName name 'CM_Create_DevNode_ExA';
function CM_Create_DevNode_ExW; external CfgMgrDllName name 'CM_Create_DevNode_ExW';
function CM_Create_DevInst; external CfgMgrDllName name 'CM_Create_DevNode' + NameSuffix;
function CM_Create_DevInstA; external CfgMgrDllName name 'CM_Create_DevNodeA';
function CM_Create_DevInstW; external CfgMgrDllName name 'CM_Create_DevNodeW';
function CM_Create_DevInst_Ex; external CfgMgrDllName name 'CM_Create_DevNode_Ex' + NameSuffix;
function CM_Create_DevInst_ExA; external CfgMgrDllName name 'CM_Create_DevNode_ExA';
function CM_Create_DevInst_ExW; external CfgMgrDllName name 'CM_Create_DevNode_ExW';
function CM_Create_Range_List; external CfgMgrDllName name 'CM_Create_Range_List';
function CM_Delete_Class_Key; external CfgMgrDllName name 'CM_Delete_Class_Key';
function CM_Delete_Class_Key_Ex; external CfgMgrDllName name 'CM_Delete_Class_Key_Ex';
function CM_Delete_DevNode_Key; external CfgMgrDllName name 'CM_Delete_DevNode_Key';
function CM_Delete_DevNode_Key_Ex; external CfgMgrDllName name 'CM_Delete_DevNode_Key_Ex';
function CM_Delete_DevInst_Key; external CfgMgrDllName name 'CM_Delete_DevNode_Key';
function CM_Delete_DevInst_Key_Ex; external CfgMgrDllName name 'CM_Delete_DevNode_Key_Ex';
function CM_Delete_Range; external CfgMgrDllName name 'CM_Delete_Range';
function CM_Detect_Resource_Conflict; external CfgMgrDllName name 'CM_Detect_Resource_Conflict';
function CM_Detect_Resource_Conflict_Ex; external CfgMgrDllName name 'CM_Detect_Resource_Conflict_Ex';
function CM_Disable_DevNode; external CfgMgrDllName name 'CM_Disable_DevNode';
function CM_Disable_DevNode_Ex; external CfgMgrDllName name 'CM_Disable_DevNode_Ex';
function CM_Disable_DevInst; external CfgMgrDllName name 'CM_Disable_DevNode';
function CM_Disable_DevInst_Ex; external CfgMgrDllName name 'CM_Disable_DevNode_Ex';
function CM_Disconnect_Machine; external CfgMgrDllName name 'CM_Disconnect_Machine';
function CM_Dup_Range_List; external CfgMgrDllName name 'CM_Dup_Range_List';
function CM_Enable_DevNode; external CfgMgrDllName name 'CM_Enable_DevNode';
function CM_Enable_DevNode_Ex; external CfgMgrDllName name 'CM_Enable_DevNode_Ex';
function CM_Enable_DevInst; external CfgMgrDllName name 'CM_Enable_DevNode';
function CM_Enable_DevInst_Ex; external CfgMgrDllName name 'CM_Enable_DevNode_Ex';
function CM_Enumerate_Classes; external CfgMgrDllName name 'CM_Enumerate_Classes';
function CM_Enumerate_Classes_Ex; external CfgMgrDllName name 'CM_Enumerate_Classes_Ex';
function CM_Enumerate_Enumerators; external CfgMgrDllName name 'CM_Enumerate_Enumerators' + NameSuffix;
function CM_Enumerate_EnumeratorsA; external CfgMgrDllName name 'CM_Enumerate_EnumeratorsA';
function CM_Enumerate_EnumeratorsW; external CfgMgrDllName name 'CM_Enumerate_EnumeratorsW';
function CM_Enumerate_Enumerators_Ex; external CfgMgrDllName name 'CM_Enumerate_Enumerators_Ex' + NameSuffix;
function CM_Enumerate_Enumerators_ExA; external CfgMgrDllName name 'CM_Enumerate_Enumerators_ExA';
function CM_Enumerate_Enumerators_ExW; external CfgMgrDllName name 'CM_Enumerate_Enumerators_ExW';
function CM_Find_Range; external CfgMgrDllName name 'CM_Find_Range';
function CM_First_Range; external CfgMgrDllName name 'CM_First_Range';
function CM_Free_Log_Conf; external CfgMgrDllName name 'CM_Free_Log_Conf';
function CM_Free_Log_Conf_Ex; external CfgMgrDllName name 'CM_Free_Log_Conf_Ex';
function CM_Free_Log_Conf_Handle; external CfgMgrDllName name 'CM_Free_Log_Conf_Handle';
function CM_Free_Range_List; external CfgMgrDllName name 'CM_Free_Range_List';
function CM_Free_Res_Des; external CfgMgrDllName name 'CM_Free_Res_Des';
function CM_Free_Res_Des_Ex; external CfgMgrDllName name 'CM_Free_Res_Des_Ex';
function CM_Free_Res_Des_Handle; external CfgMgrDllName name 'CM_Free_Res_Des_Handle';
function CM_Get_Child; external CfgMgrDllName name 'CM_Get_Child';
function CM_Get_Child_Ex; external CfgMgrDllName name 'CM_Get_Child_Ex';
function CM_Get_Class_Name; external CfgMgrDllName name 'CM_Get_Class_Name' + NameSuffix;
function CM_Get_Class_NameA; external CfgMgrDllName name 'CM_Get_Class_NameA';
function CM_Get_Class_NameW; external CfgMgrDllName name 'CM_Get_Class_NameW';
function CM_Get_Class_Name_Ex; external CfgMgrDllName name 'CM_Get_Class_Name_Ex' + NameSuffix;
function CM_Get_Class_Name_ExA; external CfgMgrDllName name 'CM_Get_Class_Name_ExA';
function CM_Get_Class_Name_ExW; external CfgMgrDllName name 'CM_Get_Class_Name_ExW';
function CM_Get_Class_Key_Name; external CfgMgrDllName name 'CM_Get_Class_Key_Name' + NameSuffix;
function CM_Get_Class_Key_NameA; external CfgMgrDllName name 'CM_Get_Class_Key_NameA';
function CM_Get_Class_Key_NameW; external CfgMgrDllName name 'CM_Get_Class_Key_NameW';
function CM_Get_Class_Key_Name_Ex; external CfgMgrDllName name 'CM_Get_Class_Key_Name_Ex' + NameSuffix;
function CM_Get_Class_Key_Name_ExA; external CfgMgrDllName name 'CM_Get_Class_Key_Name_ExA';
function CM_Get_Class_Key_Name_ExW; external CfgMgrDllName name 'CM_Get_Class_Key_Name_ExW';
function CM_Get_Depth; external CfgMgrDllName name 'CM_Get_Depth';
function CM_Get_Depth_Ex; external CfgMgrDllName name 'CM_Get_Depth_Ex';
function CM_Get_Device_ID; external CfgMgrDllName name 'CM_Get_Device_ID' + NameSuffix;
function CM_Get_Device_IDA; external CfgMgrDllName name 'CM_Get_Device_IDA';
function CM_Get_Device_IDW; external CfgMgrDllName name 'CM_Get_Device_IDW';
function CM_Get_Device_ID_Ex; external CfgMgrDllName name 'CM_Get_Device_ID_Ex' + NameSuffix;
function CM_Get_Device_ID_ExA; external CfgMgrDllName name 'CM_Get_Device_ID_ExA';
function CM_Get_Device_ID_ExW; external CfgMgrDllName name 'CM_Get_Device_ID_ExW';
function CM_Get_Device_ID_List; external CfgMgrDllName name 'CM_Get_Device_ID_List' + NameSuffix;
function CM_Get_Device_ID_ListA; external CfgMgrDllName name 'CM_Get_Device_ID_ListA';
function CM_Get_Device_ID_ListW; external CfgMgrDllName name 'CM_Get_Device_ID_ListW';
function CM_Get_Device_ID_List_Ex; external CfgMgrDllName name 'CM_Get_Device_ID_List_Ex' + NameSuffix;
function CM_Get_Device_ID_List_ExA; external CfgMgrDllName name 'CM_Get_Device_ID_List_ExA';
function CM_Get_Device_ID_List_ExW; external CfgMgrDllName name 'CM_Get_Device_ID_List_ExW';
function CM_Get_Device_ID_List_Size; external CfgMgrDllName name 'CM_Get_Device_ID_List_Size' + NameSuffix;
function CM_Get_Device_ID_List_SizeA; external CfgMgrDllName name 'CM_Get_Device_ID_List_SizeA';
function CM_Get_Device_ID_List_SizeW; external CfgMgrDllName name 'CM_Get_Device_ID_List_SizeW';
function CM_Get_Device_ID_List_Size_Ex; external CfgMgrDllName name 'CM_Get_Device_ID_List_Size_Ex' + NameSuffix;
function CM_Get_Device_ID_List_Size_ExA; external CfgMgrDllName name 'CM_Get_Device_ID_List_Size_ExA';
function CM_Get_Device_ID_List_Size_ExW; external CfgMgrDllName name 'CM_Get_Device_ID_List_Size_ExW';
function CM_Get_Device_ID_Size; external CfgMgrDllName name 'CM_Get_Device_ID_Size';
function CM_Get_Device_ID_Size_Ex; external CfgMgrDllName name 'CM_Get_Device_ID_Size_Ex';
function CM_Get_DevNode_Registry_Property; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property' + NameSuffix;
function CM_Get_DevNode_Registry_PropertyA; external CfgMgrDllName name 'CM_Get_DevNode_Registry_PropertyA';
function CM_Get_DevNode_Registry_PropertyW; external CfgMgrDllName name 'CM_Get_DevNode_Registry_PropertyW';
function CM_Get_DevNode_Registry_Property_Ex; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_Ex' + NameSuffix;
function CM_Get_DevNode_Registry_Property_ExA; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_ExA';
function CM_Get_DevNode_Registry_Property_ExW; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_ExW';
function CM_Get_DevInst_Registry_Property; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property' + NameSuffix;
function CM_Get_DevInst_Registry_PropertyA; external CfgMgrDllName name 'CM_Get_DevNode_Registry_PropertyA';
function CM_Get_DevInst_Registry_PropertyW; external CfgMgrDllName name 'CM_Get_DevNode_Registry_PropertyW';
function CM_Get_DevInst_Registry_Property_Ex; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_Ex' + NameSuffix;
function CM_Get_DevInst_Registry_Property_ExA; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_ExA';
function CM_Get_DevInst_Registry_Property_ExW; external CfgMgrDllName name 'CM_Get_DevNode_Registry_Property_ExW';
{$IFDEF WINXP_UP}
function CM_Get_DevNode_Custom_Property; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property' + NameSuffix;
function CM_Get_DevNode_Custom_PropertyA; external SetupApiDllName name 'CM_Get_DevNode_Custom_PropertyA';
function CM_Get_DevNode_Custom_PropertyW; external SetupApiDllName name 'CM_Get_DevNode_Custom_PropertyW';
function CM_Get_DevNode_Custom_Property_Ex; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_Ex' + NameSuffix;
function CM_Get_DevNode_Custom_Property_ExA; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_ExA';
function CM_Get_DevNode_Custom_Property_ExW; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_ExW';
function CM_Get_DevInst_Custom_Property; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property' + NameSuffix;
function CM_Get_DevInst_Custom_PropertyA; external SetupApiDllName name 'CM_Get_DevNode_Custom_PropertyA';
function CM_Get_DevInst_Custom_PropertyW; external SetupApiDllName name 'CM_Get_DevNode_Custom_PropertyW';
function CM_Get_DevInst_Custom_Property_Ex; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_Ex' + NameSuffix;
function CM_Get_DevInst_Custom_Property_ExA; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_ExA';
function CM_Get_DevInst_Custom_Property_ExW; external SetupApiDllName name 'CM_Get_DevNode_Custom_Property_ExW';
{$ENDIF WINXP_UP}
function CM_Get_DevNode_Status; external CfgMgrDllName name 'CM_Get_DevNode_Status';
function CM_Get_DevInst_Status; external CfgMgrDllName name 'CM_Get_DevNode_Status';
function CM_Get_DevNode_Status_Ex; external CfgMgrDllName name 'CM_Get_DevNode_Status_Ex';
function CM_Get_DevInst_Status_Ex; external CfgMgrDllName name 'CM_Get_DevNode_Status_Ex';
function CM_Get_First_Log_Conf; external CfgMgrDllName name 'CM_Get_First_Log_Conf';
function CM_Get_First_Log_Conf_Ex; external CfgMgrDllName name 'CM_Get_First_Log_Conf_Ex';
function CM_Get_Global_State; external CfgMgrDllName name 'CM_Get_Global_State';
function CM_Get_Global_State_Ex; external CfgMgrDllName name 'CM_Get_Global_State_Ex';
function CM_Get_Hardware_Profile_Info; external CfgMgrDllName name 'CM_Get_Hardware_Profile_Info' + NameSuffix;
function CM_Get_Hardware_Profile_InfoA; external CfgMgrDllName name 'CM_Get_Hardware_Profile_InfoA';
function CM_Get_Hardware_Profile_Info_Ex; external CfgMgrDllName name 'CM_Get_Hardware_Profile_Info_Ex' + NameSuffix;
function CM_Get_Hardware_Profile_Info_ExA; external CfgMgrDllName name 'CM_Get_Hardware_Profile_Info_ExA';
function CM_Get_Hardware_Profile_InfoW; external CfgMgrDllName name 'CM_Get_Hardware_Profile_InfoW';
function CM_Get_Hardware_Profile_Info_ExW; external CfgMgrDllName name 'CM_Get_Hardware_Profile_Info_ExW';
function CM_Get_HW_Prof_Flags; external CfgMgrDllName name 'CM_Get_HW_Prof_Flags' + NameSuffix;
function CM_Get_HW_Prof_FlagsA; external CfgMgrDllName name 'CM_Get_HW_Prof_FlagsA';
function CM_Get_HW_Prof_FlagsW; external CfgMgrDllName name 'CM_Get_HW_Prof_FlagsW';
function CM_Get_HW_Prof_Flags_Ex; external CfgMgrDllName name 'CM_Get_HW_Prof_Flags_Ex' + NameSuffix;
function CM_Get_HW_Prof_Flags_ExA; external CfgMgrDllName name 'CM_Get_HW_Prof_Flags_ExA';
function CM_Get_HW_Prof_Flags_ExW; external CfgMgrDllName name 'CM_Get_HW_Prof_Flags_ExW';
{$IFNDEF WINNT4}
function CM_Get_Device_Interface_Alias; external CfgMgrDllName name 'CM_Get_Device_Interface_Alias' + NameSuffix;
function CM_Get_Device_Interface_AliasA; external CfgMgrDllName name 'CM_Get_Device_Interface_AliasA';
function CM_Get_Device_Interface_AliasW; external CfgMgrDllName name 'CM_Get_Device_Interface_AliasW';
function CM_Get_Device_Interface_Alias_Ex; external CfgMgrDllName name 'CM_Get_Device_Interface_Alias_Ex' + NameSuffix;
function CM_Get_Device_Interface_Alias_ExA; external CfgMgrDllName name 'CM_Get_Device_Interface_Alias_ExA';
function CM_Get_Device_Interface_Alias_ExW; external CfgMgrDllName name 'CM_Get_Device_Interface_Alias_ExW';
function CM_Get_Device_Interface_List; external CfgMgrDllName name 'CM_Get_Device_Interface_List' + NameSuffix;
function CM_Get_Device_Interface_ListA; external CfgMgrDllName name 'CM_Get_Device_Interface_ListA';
function CM_Get_Device_Interface_ListW; external CfgMgrDllName name 'CM_Get_Device_Interface_ListW';
function CM_Get_Device_Interface_List_Ex; external CfgMgrDllName name 'CM_Get_Device_Interface_List_Ex' + NameSuffix;
function CM_Get_Device_Interface_List_ExA; external CfgMgrDllName name 'CM_Get_Device_Interface_List_ExA';
function CM_Get_Device_Interface_List_ExW; external CfgMgrDllName name 'CM_Get_Device_Interface_List_ExW';
function CM_Get_Device_Interface_List_Size; external CfgMgrDllName name 'CM_Get_Device_Interface_List_Size' + NameSuffix;
function CM_Get_Device_Interface_List_SizeA; external CfgMgrDllName name 'CM_Get_Device_Interface_List_SizeA';
function CM_Get_Device_Interface_List_SizeW; external CfgMgrDllName name 'CM_Get_Device_Interface_List_SizeW';
function CM_Get_Device_Interface_List_Size_Ex; external CfgMgrDllName name 'CM_Get_Device_Interface_List_Size_Ex' + NameSuffix;
function CM_Get_Device_Interface_List_Size_ExA; external CfgMgrDllName name 'CM_Get_Device_Interface_List_Size_ExA';
function CM_Get_Device_Interface_List_Size_ExW; external CfgMgrDllName name 'CM_Get_Device_Interface_List_Size_ExW';
function CM_Get_Log_Conf_Priority; external CfgMgrDllName name 'CM_Get_Log_Conf_Priority';
function CM_Get_Log_Conf_Priority_Ex; external CfgMgrDllName name 'CM_Get_Log_Conf_Priority_Ex';
{$ENDIF !WINNT4}
function CM_Get_Next_Log_Conf; external CfgMgrDllName name 'CM_Get_Next_Log_Conf';
function CM_Get_Next_Log_Conf_Ex; external CfgMgrDllName name 'CM_Get_Next_Log_Conf_Ex';
function CM_Get_Parent; external CfgMgrDllName name 'CM_Get_Parent';
function CM_Get_Parent_Ex; external CfgMgrDllName name 'CM_Get_Parent_Ex';
function CM_Get_Res_Des_Data; external CfgMgrDllName name 'CM_Get_Res_Des_Data';
function CM_Get_Res_Des_Data_Ex; external CfgMgrDllName name 'CM_Get_Res_Des_Data_Ex';
function CM_Get_Res_Des_Data_Size; external CfgMgrDllName name 'CM_Get_Res_Des_Data_Size';
function CM_Get_Res_Des_Data_Size_Ex; external CfgMgrDllName name 'CM_Get_Res_Des_Data_Size_Ex';
function CM_Get_Sibling; external CfgMgrDllName name 'CM_Get_Sibling';
function CM_Get_Sibling_Ex; external CfgMgrDllName name 'CM_Get_Sibling_Ex';
function CM_Get_Version; external CfgMgrDllName name 'CM_Get_Version';
function CM_Get_Version_Ex; external CfgMgrDllName name 'CM_Get_Version_Ex';
{$IFDEF WINXP_UP}
function CM_Is_Version_Available; external SetupApiDllName name 'CM_Is_Version_Available';
function CM_Is_Version_Available_Ex; external SetupApiDllName name 'CM_Is_Version_Available_Ex';
{$ENDIF WINXP_UP}
function CM_Intersect_Range_List; external CfgMgrDllName name 'CM_Intersect_Range_List';
function CM_Invert_Range_List; external CfgMgrDllName name 'CM_Invert_Range_List';
function CM_Locate_DevNode; external CfgMgrDllName name 'CM_Locate_DevNode' + NameSuffix;
function CM_Locate_DevNodeA; external CfgMgrDllName name 'CM_Locate_DevNodeA';
function CM_Locate_DevNodeW; external CfgMgrDllName name 'CM_Locate_DevNodeW';
function CM_Locate_DevNode_Ex; external CfgMgrDllName name 'CM_Locate_DevNode_Ex' + NameSuffix;
function CM_Locate_DevNode_ExA; external CfgMgrDllName name 'CM_Locate_DevNode_ExA';
function CM_Locate_DevNode_ExW; external CfgMgrDllName name 'CM_Locate_DevNode_ExW';
function CM_Locate_DevInst; external CfgMgrDllName name 'CM_Locate_DevNode' + NameSuffix;
function CM_Locate_DevInstA; external CfgMgrDllName name 'CM_Locate_DevNodeA';
function CM_Locate_DevInstW; external CfgMgrDllName name 'CM_Locate_DevNodeW';
function CM_Locate_DevInst_Ex; external CfgMgrDllName name 'CM_Locate_DevNode_Ex' + NameSuffix;
function CM_Locate_DevInst_ExA; external CfgMgrDllName name 'CM_Locate_DevNode_ExA';
function CM_Locate_DevInst_ExW; external CfgMgrDllName name 'CM_Locate_DevNode_ExW';
function CM_Merge_Range_List; external CfgMgrDllName name 'CM_Merge_Range_List';
function CM_Modify_Res_Des; external CfgMgrDllName name 'CM_Modify_Res_Des';
function CM_Modify_Res_Des_Ex; external CfgMgrDllName name 'CM_Modify_Res_Des_Ex';
function CM_Move_DevNode; external CfgMgrDllName name 'CM_Move_DevNode';
function CM_Move_DevInst; external CfgMgrDllName name 'CM_Move_DevNode';
function CM_Move_DevNode_Ex; external CfgMgrDllName name 'CM_Move_DevNode_Ex';
function CM_Move_DevInst_Ex; external CfgMgrDllName name 'CM_Move_DevNode_Ex';
function CM_Next_Range; external CfgMgrDllName name 'CM_Next_Range';
function CM_Get_Next_Res_Des; external CfgMgrDllName name 'CM_Get_Next_Res_Des';
function CM_Get_Next_Res_Des_Ex; external CfgMgrDllName name 'CM_Get_Next_Res_Des_Ex';
function CM_Open_Class_Key; external CfgMgrDllName name 'CM_Open_Class_Key' + NameSuffix;
function CM_Open_Class_KeyA; external CfgMgrDllName name 'CM_Open_Class_KeyA';
function CM_Open_Class_KeyW; external CfgMgrDllName name 'CM_Open_Class_KeyW';
function CM_Open_Class_Key_Ex; external CfgMgrDllName name 'CM_Open_Class_Key_Ex' + NameSuffix;
function CM_Open_Class_Key_ExA; external CfgMgrDllName name 'CM_Open_Class_Key_ExA';
function CM_Open_Class_Key_ExW; external CfgMgrDllName name 'CM_Open_Class_Key_ExW';
function CM_Open_DevNode_Key; external CfgMgrDllName name 'CM_Open_DevNode_Key';
function CM_Open_DevInst_Key; external CfgMgrDllName name 'CM_Open_DevNode_Key';
function CM_Open_DevNode_Key_Ex; external CfgMgrDllName name 'CM_Open_DevNode_Key_Ex';
function CM_Open_DevInst_Key_Ex; external CfgMgrDllName name 'CM_Open_DevNode_Key_Ex';
function CM_Query_Arbitrator_Free_Data; external CfgMgrDllName name 'CM_Query_Arbitrator_Free_Data';
function CM_Query_Arbitrator_Free_Data_Ex; external CfgMgrDllName name 'CM_Query_Arbitrator_Free_Data_Ex';
function CM_Query_Arbitrator_Free_Size; external CfgMgrDllName name 'CM_Query_Arbitrator_Free_Size';
function CM_Query_Arbitrator_Free_Size_Ex; external CfgMgrDllName name 'CM_Query_Arbitrator_Free_Size_Ex';
function CM_Query_Remove_SubTree; external CfgMgrDllName name 'CM_Query_Remove_SubTree';
function CM_Query_Remove_SubTree_Ex; external CfgMgrDllName name 'CM_Query_Remove_SubTree_Ex';
{$IFDEF WIN2000_UP}
function CM_Query_And_Remove_SubTree; external CfgMgrDllName name 'CM_Query_And_Remove_SubTree' + NameSuffix;
function CM_Query_And_Remove_SubTreeA; external CfgMgrDllName name 'CM_Query_And_Remove_SubTreeA';
function CM_Query_And_Remove_SubTree_Ex; external CfgMgrDllName name 'CM_Query_And_Remove_SubTree_Ex' + NameSuffix;
function CM_Query_And_Remove_SubTree_ExA; external CfgMgrDllName name 'CM_Query_And_Remove_SubTree_ExA';
function CM_Query_And_Remove_SubTreeW; external CfgMgrDllName name 'CM_Query_And_Remove_SubTreeW';
function CM_Query_And_Remove_SubTree_ExW; external CfgMgrDllName name 'CM_Query_And_Remove_SubTree_ExW';
function CM_Request_Device_Eject; external SetupApiDllName name 'CM_Request_Device_Eject' + NameSuffix;
function CM_Request_Device_EjectA; external SetupApiDllName name 'CM_Request_Device_EjectA';
function CM_Request_Device_EjectW; external SetupApiDllName name 'CM_Request_Device_EjectW';
function CM_Request_Device_Eject_Ex; external SetupApiDllName name 'CM_Request_Device_Eject_Ex' + NameSuffix;
function CM_Request_Device_Eject_ExA; external SetupApiDllName name 'CM_Request_Device_Eject_ExA';
function CM_Request_Device_Eject_ExW; external SetupApiDllName name 'CM_Request_Device_Eject_ExW';
{$ENDIF WIN2000_UP}
function CM_Reenumerate_DevNode; external CfgMgrDllName name 'CM_Reenumerate_DevNode';
function CM_Reenumerate_DevInst; external CfgMgrDllName name 'CM_Reenumerate_DevNode';
function CM_Reenumerate_DevNode_Ex; external CfgMgrDllName name 'CM_Reenumerate_DevNode_Ex';
function CM_Reenumerate_DevInst_Ex; external CfgMgrDllName name 'CM_Reenumerate_DevNode_Ex';
{$IFNDEF WINNT4}
function CM_Register_Device_Interface; external CfgMgrDllName name 'CM_Register_Device_Interface' + NameSuffix;
function CM_Register_Device_InterfaceA; external CfgMgrDllName name 'CM_Register_Device_InterfaceA';
function CM_Register_Device_InterfaceW; external CfgMgrDllName name 'CM_Register_Device_InterfaceW';
function CM_Register_Device_Interface_Ex; external CfgMgrDllName name 'CM_Register_Device_Interface_Ex' + NameSuffix;
function CM_Register_Device_Interface_ExA; external CfgMgrDllName name 'CM_Register_Device_Interface_ExA';
function CM_Register_Device_Interface_ExW; external CfgMgrDllName name 'CM_Register_Device_Interface_ExW';
function CM_Set_DevNode_Problem_Ex; external CfgMgrDllName name 'CM_Set_DevNode_Problem_Ex';
function CM_Set_DevInst_Problem_Ex; external CfgMgrDllName name 'CM_Set_DevNode_Problem_Ex';
function CM_Set_DevNode_Problem; external CfgMgrDllName name 'CM_Set_DevNode_Problem';
function CM_Set_DevInst_Problem; external CfgMgrDllName name 'CM_Set_DevNode_Problem';
function CM_Unregister_Device_Interface; external CfgMgrDllName name 'CM_Unregister_Device_Interface' + NameSuffix;
function CM_Unregister_Device_InterfaceA; external CfgMgrDllName name 'CM_Unregister_Device_InterfaceA';
function CM_Unregister_Device_InterfaceW; external CfgMgrDllName name 'CM_Unregister_Device_InterfaceW';
function CM_Unregister_Device_Interface_Ex; external CfgMgrDllName name 'CM_Unregister_Device_Interface_Ex' + NameSuffix;
function CM_Unregister_Device_Interface_ExA; external CfgMgrDllName name 'CM_Unregister_Device_Interface_ExA';
function CM_Unregister_Device_Interface_ExW; external CfgMgrDllName name 'CM_Unregister_Device_Interface_ExW';
function CM_Register_Device_Driver; external CfgMgrDllName name 'CM_Register_Device_Driver';
function CM_Register_Device_Driver_Ex; external CfgMgrDllName name 'CM_Register_Device_Driver_Ex';
{$ENDIF !WINNT4}
function CM_Remove_SubTree; external CfgMgrDllName name 'CM_Remove_SubTree';
function CM_Remove_SubTree_Ex; external CfgMgrDllName name 'CM_Remove_SubTree_Ex';
function CM_Set_DevNode_Registry_Property; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property' + NameSuffix;
function CM_Set_DevNode_Registry_PropertyA; external CfgMgrDllName name 'CM_Set_DevNode_Registry_PropertyA';
function CM_Set_DevNode_Registry_PropertyW; external CfgMgrDllName name 'CM_Set_DevNode_Registry_PropertyW';
function CM_Set_DevNode_Registry_Property_Ex; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_Ex' + NameSuffix;
function CM_Set_DevNode_Registry_Property_ExA; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_ExA';
function CM_Set_DevNode_Registry_Property_ExW; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_ExW';
function CM_Set_DevInst_Registry_Property; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property' + NameSuffix;
function CM_Set_DevInst_Registry_PropertyA; external CfgMgrDllName name 'CM_Set_DevNode_Registry_PropertyA';
function CM_Set_DevInst_Registry_PropertyW; external CfgMgrDllName name 'CM_Set_DevNode_Registry_PropertyW';
function CM_Set_DevInst_Registry_Property_Ex; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_Ex' + NameSuffix;
function CM_Set_DevInst_Registry_Property_ExA; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_ExA';
function CM_Set_DevInst_Registry_Property_ExW; external CfgMgrDllName name 'CM_Set_DevNode_Registry_Property_ExW';
{$IFNDEF WINNT4}
function CM_Is_Dock_Station_Present; external CfgMgrDllName name 'CM_Is_Dock_Station_Present';
{$ENDIF !WINNT4}
{$IFDEF WIN2000_UP}
function CM_Is_Dock_Station_Present_Ex; external CfgMgrDllName name 'CM_Is_Dock_Station_Present_Ex';
{$ENDIF WIN2000_UP}
{$IFNDEF WINNT4}
function CM_Request_Eject_PC; external CfgMgrDllName name 'CM_Request_Eject_PC';
{$ENDIF !WINNT4}
{$IFDEF WIN2000_UP}
function CM_Request_Eject_PC_Ex; external CfgMgrDllName name 'CM_Request_Eject_PC_Ex';
{$ENDIF WIN2000_UP}
function CM_Set_HW_Prof_Flags; external CfgMgrDllName name 'CM_Set_HW_Prof_Flags' + NameSuffix;
function CM_Set_HW_Prof_FlagsA; external CfgMgrDllName name 'CM_Set_HW_Prof_FlagsA';
function CM_Set_HW_Prof_FlagsW; external CfgMgrDllName name 'CM_Set_HW_Prof_FlagsW';
function CM_Set_HW_Prof_Flags_Ex; external CfgMgrDllName name 'CM_Set_HW_Prof_Flags_Ex' + NameSuffix;
function CM_Set_HW_Prof_Flags_ExA; external CfgMgrDllName name 'CM_Set_HW_Prof_Flags_ExA';
function CM_Set_HW_Prof_Flags_ExW; external CfgMgrDllName name 'CM_Set_HW_Prof_Flags_ExW';
function CM_Setup_DevNode; external CfgMgrDllName name 'CM_Setup_DevNode';
function CM_Setup_DevInst; external CfgMgrDllName name 'CM_Setup_DevNode';
function CM_Setup_DevNode_Ex; external CfgMgrDllName name 'CM_Setup_DevNode_Ex';
function CM_Setup_DevInst_Ex; external CfgMgrDllName name 'CM_Setup_DevNode_Ex';
function CM_Test_Range_Available; external CfgMgrDllName name 'CM_Test_Range_Available';
function CM_Uninstall_DevNode; external CfgMgrDllName name 'CM_Uninstall_DevNode';
function CM_Uninstall_DevInst; external CfgMgrDllName name 'CM_Uninstall_DevNode';
function CM_Uninstall_DevNode_Ex; external CfgMgrDllName name 'CM_Uninstall_DevNode_Ex';
function CM_Uninstall_DevInst_Ex; external CfgMgrDllName name 'CM_Uninstall_DevNode_Ex';
function CM_Run_Detection; external CfgMgrDllName name 'CM_Run_Detection';
function CM_Run_Detection_Ex; external CfgMgrDllName name 'CM_Run_Detection_Ex';
function CM_Set_HW_Prof; external CfgMgrDllName name 'CM_Set_HW_Prof';
function CM_Set_HW_Prof_Ex; external CfgMgrDllName name 'CM_Set_HW_Prof_Ex';
{$IFDEF WIN2000_UP}
function CM_Query_Resource_Conflict_List; external CfgMgrDllName name 'CM_Query_Resource_Conflict_List';
function CM_Free_Resource_Conflict_Handle; external CfgMgrDllName name 'CM_Free_Resource_Conflict_Handle';
function CM_Get_Resource_Conflict_Count; external CfgMgrDllName name 'CM_Get_Resource_Conflict_Count';
function CM_Get_Resource_Conflict_Details; external CfgMgrDllName name 'CM_Get_Resource_Conflict_Details' + NameSuffix;
function CM_Get_Resource_Conflict_DetailsA; external CfgMgrDllName name 'CM_Get_Resource_Conflict_DetailsA';
function CM_Get_Resource_Conflict_DetailsW; external CfgMgrDllName name 'CM_Get_Resource_Conflict_DetailsW';
function CM_Get_Class_Registry_Property; external CfgMgrDllName name 'CM_Get_Class_Registry_Property' + NameSuffix;
function CM_Get_Class_Registry_PropertyA; external CfgMgrDllName name 'CM_Get_Class_Registry_PropertyA';
function CM_Get_Class_Registry_PropertyW; external CfgMgrDllName name 'CM_Get_Class_Registry_PropertyW';
function CM_Set_Class_Registry_Property; external CfgMgrDllName name 'CM_Set_Class_Registry_Property' + NameSuffix;
function CM_Set_Class_Registry_PropertyA; external CfgMgrDllName name 'CM_Set_Class_Registry_PropertyA';
function CM_Set_Class_Registry_PropertyW; external CfgMgrDllName name 'CM_Set_Class_Registry_PropertyW';
function CM_WaitNoPendingInstallEvents; external CfgMgrDllName name 'CMP_WaitNoPendingInstallEvents';
function CMP_WaitNoPendingInstallEvents; external CfgMgrDllName name 'CMP_WaitNoPendingInstallEvents';
{$ENDIF WIN2000_UP}

{$ENDIF !CFGMGR32_LINKONREQUEST}

end.
