{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Setup and Device Installer API interface unit              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: setupapi.h, released March 1999.           }
{ The original Pascal code is: SetupApi.pas, released 29 Jan 2000. }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (C) 1999 Robert Marquardt.                             }
{                                                                  }
{ Contributor(s): Marcel van Brakel (brakelm att bart dott nl)     }
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
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit QSetupApi;

interface

{$WEAKPACKAGEUNIT ON}

// (rom) this is the switch to change between static and dynamic linking.
// (rom) it is enabled by default here.
// (rom) To disable simply change the '$' to a '.'.
{$DEFINE SETUPAPI_LINKONREQUEST}

(*$HPPEMIT '#include "setupapi.h"'*)

uses
  Windows, CommCtrl;

type
  PPWSTR    = ^PWideChar;
  PPASTR    = ^PAnsiChar;
  PPSTR     = ^PChar;
  PHICON    = ^HICON;
  ULONG_PTR = DWORD;
  {$EXTERNALSYM ULONG_PTR}
  DWORD_PTR = DWORD;
  {$EXTERNALSYM DWORD_PTR}
  UINT_PTR  = DWORD;
  {$EXTERNALSYM UINT_PTR}

const
  ANYSIZE_ARRAY = 1;
  {$EXTERNALSYM ANYSIZE_ARRAY}

//
// Define maximum string length constants as specified by
// Windows 95.
//
const
  LINE_LEN = 256;                 // Win95-compatible maximum for displayable
  {$EXTERNALSYM LINE_LEN}
                                  // strings coming from a device INF.
  MAX_INF_STRING_LENGTH = 4096;   // Actual maximum size of an INF string
  {$EXTERNALSYM MAX_INF_STRING_LENGTH}
                                  // (including string substitutions).
  MAX_TITLE_LEN         = 60;
  {$EXTERNALSYM MAX_TITLE_LEN}
  MAX_INSTRUCTION_LEN   = 256;
  {$EXTERNALSYM MAX_INSTRUCTION_LEN}
  MAX_LABEL_LEN         = 30;
  {$EXTERNALSYM MAX_LABEL_LEN}
  MAX_SERVICE_NAME_LEN  = 256;
  {$EXTERNALSYM MAX_SERVICE_NAME_LEN}
  MAX_SUBTITLE_LEN      = 256;
  {$EXTERNALSYM MAX_SUBTITLE_LEN}

//
// Define maximum length of a machine name in the format expected by ConfigMgr32
// CM_Connect_Machine (i.e., "\\\\MachineName\0").
//

  SP_MAX_MACHINENAME_LENGTH = MAX_PATH + 3;
  {$EXTERNALSYM SP_MAX_MACHINENAME_LENGTH}

//
// Define type for reference to loaded inf file
//

type
  HINF = Pointer;
  {$EXTERNALSYM HINF}

//
// Inf context structure. Applications must not interpret or
// overwrite values in these structures.
//
  PInfContext = ^TInfContext;
  INFCONTEXT = packed record
    Inf: Pointer;
    CurrentInf: Pointer;
    Section: UINT;
    Line: UINT;
  end;
  {$EXTERNALSYM INFCONTEXT}
  TInfContext = INFCONTEXT;

//
// Inf file information structure.
//
  PSPInfInformation = ^TSPInfInformation;
  SP_INF_INFORMATION = packed record
    InfStyle: DWORD;
    InfCount: DWORD;
    VersionData: array [0..ANYSIZE_ARRAY - 1] of Byte;
  end;
  {$EXTERNALSYM SP_INF_INFORMATION}
  TSPInfInformation = SP_INF_INFORMATION;

//
// Define structure for passing alternate platform info into
// SetupSetFileQueueAlternatePlatform and SetupQueryInfOriginalFileInformation.
//
  PSPAltPlatformInfoV2 = ^SP_ALTPLATFORM_INFO_V2;
  SP_ALTPLATFORM_INFO_V2 = packed record
    cbSize: DWORD;
    //
    // platform to use (VER_PLATFORM_WIN32_WINDOWS or VER_PLATFORM_WIN32_NT)
    //
    Platform: DWORD;
    //
    // major and minor version numbers to use
    //
    MajorVersion: DWORD;
    MinorVersion: DWORD;
    //
    // processor architecture to use (PROCESSOR_ARCHITECTURE_INTEL,
    // PROCESSOR_ARCHITECTURE_ALPHA, PROCESSOR_ARCHITECTURE_IA64, or
    // PROCESSOR_ARCHITECTURE_ALPHA64)
    //
    ProcessorArchitecture: WORD;

    Flags: WORD;
    (*
    union {
        WORD  Reserved; // for compatibility with V1 structure
        WORD  Flags;    // indicates validity of non V1 fields
    };
    *)

    //
    // specify SP_ALTPLATFORM_FLAGS_VERSION_RANGE in Flags
    // to use FirstValidatedMajorVersion and FirstValidatedMinorVersion
    //
    // Major and minor versions of the oldest previous OS for which this
    // package's digital signature may be considered valid.  For example, say
    // the alternate platform is VER_PLATFORM_WIN32_NT, version 5.1.  However,
    // it is wished that driver packages signed with a 5.0 osattr also be
    // considered valid.  In this case, you'd have a  MajorVersion/MinorVersion
    // of 5.1, and a FirstValidatedMajorVersion/FirstValidatedMinorVersion of
    // 5.0.  To validate packages signed for any previous OS release, specify
    // 0 for these fields.  To only validate against the target alternate
    // platform, specify the same values as those in the MajorVersion and
    // MinorVersion fields.
    //
    FirstValidatedMajorVersion: DWORD;
    FirstValidatedMinorVersion: DWORD;
  end;
  {$EXTERNALSYM SP_ALTPLATFORM_INFO_V2}
  TSPAltPlatformInfoV2 = SP_ALTPLATFORM_INFO_V2;

  PSPAltPlatformInfoV1 = ^TSPAltPlatformInfoV1;
  SP_ALTPLATFORM_INFO_V1 = packed record
    cbSize: DWORD;
    //
    // platform to use (VER_PLATFORM_WIN32_WINDOWS or VER_PLATFORM_WIN32_NT)
    //
    Platform: DWORD;
    //
    // major and minor version numbers to use
    //
    MajorVersion: DWORD;
    MinorVersion: DWORD;
    //
    // processor architecture to use (PROCESSOR_ARCHITECTURE_INTEL,
    // PROCESSOR_ARCHITECTURE_ALPHA, PROCESSOR_ARCHITECTURE_IA64, or
    // PROCESSOR_ARCHITECTURE_ALPHA64)
    //
    ProcessorArchitecture: Word;
    Reserved: Word; // must be zero.
  end;
  {$EXTERNALSYM SP_ALTPLATFORM_INFO_V1}
  TSPAltPlatformInfoV1 = SP_ALTPLATFORM_INFO_V1;

  {$IFDEF WINXP}
  PSPAltPlatformInfo = PSPAltPlatformInfoV2;
  TSPAltPlatformInfo = TSPAltPlatformInfoV2;
  {$ELSE}
  PSPAltPlatformInfo = PSPAltPlatformInfoV1;
  TSPAltPlatformInfo = TSPAltPlatformInfoV1;
  {$ENDIF WINXP}

//
// the following flags are available to SP_ALTPLATFORM_INFO_V2
//
const
  SP_ALTPLATFORM_FLAGS_VERSION_RANGE = $0001;  // FirstValidatedMajor/MinorVersion
  {$EXTERNALSYM SP_ALTPLATFORM_FLAGS_VERSION_RANGE}

//
// Define structure that is filled in by SetupQueryInfOriginalFileInformation
// to indicate the INF's original name and the original name of the (potentially
// platform-specific) catalog file specified by that INF.
//
type
  PSPOriginalFileInfoA = ^TSPOriginalFileInfoA;
  PSPOriginalFileInfoW = ^TSPOriginalFileInfoW;
  PSPOriginalFileInfo = PSPOriginalFileInfoA;
  SP_ORIGINAL_FILE_INFO_A = packed record
    cbSize: DWORD;
    OriginalInfName: array [0..MAX_PATH - 1] of AnsiChar;
    OriginalCatalogName: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_ORIGINAL_FILE_INFO_A}
  SP_ORIGINAL_FILE_INFO_W = packed record
    cbSize: DWORD;
    OriginalInfName: array [0..MAX_PATH - 1] of WideChar;
    OriginalCatalogName: array [0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_ORIGINAL_FILE_INFO_W}
  TSPOriginalFileInfoA = SP_ORIGINAL_FILE_INFO_A;
  TSPOriginalFileInfoW = SP_ORIGINAL_FILE_INFO_W;
  TSPOriginalFileInfo = TSPOriginalFileInfoA;

//
// SP_INF_INFORMATION.InfStyle values
//
const
  INF_STYLE_NONE  = $00000000; // unrecognized or non-existent
  {$EXTERNALSYM INF_STYLE_NONE}
  INF_STYLE_OLDNT = $00000001; // winnt 3.x
  {$EXTERNALSYM INF_STYLE_OLDNT}
  INF_STYLE_WIN4  = $00000002; // Win95
  {$EXTERNALSYM INF_STYLE_WIN4}

//
// Additional InfStyle flags that may be specified when calling SetupOpenInfFile.
//
//
  INF_STYLE_CACHE_ENABLE  = $00000010; // always cache INF, even outside of %windir%\Inf
  {$EXTERNALSYM INF_STYLE_CACHE_ENABLE}
  INF_STYLE_CACHE_DISABLE = $00000020; // delete cached INF information
  {$EXTERNALSYM INF_STYLE_CACHE_DISABLE}

//
// Target directory specs.
//
  DIRID_ABSOLUTE       = DWORD(-1); // real 32-bit -1
  {$EXTERNALSYM DIRID_ABSOLUTE}
  DIRID_ABSOLUTE_16BIT = $FFFF; // 16-bit -1 for compat w/setupx
  {$EXTERNALSYM DIRID_ABSOLUTE_16BIT}
  DIRID_NULL           = 0;
  {$EXTERNALSYM DIRID_NULL}
  DIRID_SRCPATH        = 1;
  {$EXTERNALSYM DIRID_SRCPATH}
  DIRID_WINDOWS        = 10;
  {$EXTERNALSYM DIRID_WINDOWS}
  DIRID_SYSTEM         = 11; // system32
  {$EXTERNALSYM DIRID_SYSTEM}
  DIRID_DRIVERS        = 12;
  {$EXTERNALSYM DIRID_DRIVERS}
  DIRID_IOSUBSYS       = DIRID_DRIVERS;
  {$EXTERNALSYM DIRID_IOSUBSYS}
  DIRID_INF            = 17;
  {$EXTERNALSYM DIRID_INF}
  DIRID_HELP           = 18;
  {$EXTERNALSYM DIRID_HELP}
  DIRID_FONTS          = 20;
  {$EXTERNALSYM DIRID_FONTS}
  DIRID_VIEWERS        = 21;
  {$EXTERNALSYM DIRID_VIEWERS}
  DIRID_COLOR          = 23;
  {$EXTERNALSYM DIRID_COLOR}
  DIRID_APPS           = 24;
  {$EXTERNALSYM DIRID_APPS}
  DIRID_SHARED         = 25;
  {$EXTERNALSYM DIRID_SHARED}
  DIRID_BOOT           = 30;
  {$EXTERNALSYM DIRID_BOOT}

  DIRID_SYSTEM16       = 50;
  {$EXTERNALSYM DIRID_SYSTEM16}
  DIRID_SPOOL          = 51;
  {$EXTERNALSYM DIRID_SPOOL}
  DIRID_SPOOLDRIVERS   = 52;
  {$EXTERNALSYM DIRID_SPOOLDRIVERS}
  DIRID_USERPROFILE    = 53;
  {$EXTERNALSYM DIRID_USERPROFILE}
  DIRID_LOADER         = 54;
  {$EXTERNALSYM DIRID_LOADER}
  DIRID_PRINTPROCESSOR = 55;
  {$EXTERNALSYM DIRID_PRINTPROCESSOR}

  DIRID_DEFAULT        = DIRID_SYSTEM;
  {$EXTERNALSYM DIRID_DEFAULT}

//
// The following DIRIDs are for commonly-used shell "special folders".  The
// complete list of such folders is contained in shlobj.h.  In that headerfile,
// each folder is assigned a CSIDL_* value.  The DIRID values below are created
// by taking the CSIDL value in shlobj.h and OR'ing it with 0x4000.  Thus, if
// an INF needs to reference other special folders not defined below, it may
// generate one using the above mechanism, and setupapi will automatically deal
// with it and use the corresponding shell's path where appropriate.  (Remember
// that DIRIDs must be specified in decimal, not hex, in an INF when used for
// string substitution.)
//
  DIRID_COMMON_STARTMENU        = 16406; // All Users\Start Menu
  {$EXTERNALSYM DIRID_COMMON_STARTMENU}
  DIRID_COMMON_PROGRAMS         = 16407; // All Users\Start Menu\Programs
  {$EXTERNALSYM DIRID_COMMON_PROGRAMS}
  DIRID_COMMON_STARTUP          = 16408; // All Users\Start Menu\Programs\Startup
  {$EXTERNALSYM DIRID_COMMON_STARTUP}
  DIRID_COMMON_DESKTOPDIRECTORY = 16409; // All Users\Desktop
  {$EXTERNALSYM DIRID_COMMON_DESKTOPDIRECTORY}
  DIRID_COMMON_FAVORITES        = 16415; // All Users\Favorites
  {$EXTERNALSYM DIRID_COMMON_FAVORITES}
  DIRID_COMMON_APPDATA          = 16419; // All Users\Application Data
  {$EXTERNALSYM DIRID_COMMON_APPDATA}

  DIRID_PROGRAM_FILES           = 16422; // Program Files
  {$EXTERNALSYM DIRID_PROGRAM_FILES}
  DIRID_SYSTEM_X86              = 16425; // system32 on RISC
  {$EXTERNALSYM DIRID_SYSTEM_X86}
  DIRID_PROGRAM_FILES_X86       = 16426; // Program Files on RISC
  {$EXTERNALSYM DIRID_PROGRAM_FILES_X86}
  DIRID_PROGRAM_FILES_COMMON    = 16427; // Program Files\Common
  {$EXTERNALSYM DIRID_PROGRAM_FILES_COMMON}
  DIRID_PROGRAM_FILES_COMMONX86 = 16428; // x86 Program Files\Common on RISC
  {$EXTERNALSYM DIRID_PROGRAM_FILES_COMMONX86}

  DIRID_COMMON_TEMPLATES        = 16429; // All Users\Templates
  {$EXTERNALSYM DIRID_COMMON_TEMPLATES}
  DIRID_COMMON_DOCUMENTS        = 16430; // All Users\Documents
  {$EXTERNALSYM DIRID_COMMON_DOCUMENTS}

//
// First user-definable dirid. See SetupSetDirectoryId().
//
  DIRID_USER = $8000;
  {$EXTERNALSYM DIRID_USER}

//
// Setup callback notification routine type
//
type
  TSPFileCallbackA = function(Context: Pointer; Notification: UINT;
    Param1, Param2: UINT_PTR): UINT; stdcall;
  TSPFileCallbackW = function(Context: Pointer; Notification: UINT;
    Param1, Param2: UINT_PTR): UINT; stdcall;
  TSPFileCallback = TSPFileCallbackA;

//
// Operation/queue start/end notification. These are ordinal values.
//
const
  SPFILENOTIFY_STARTQUEUE    = $00000001;
  {$EXTERNALSYM SPFILENOTIFY_STARTQUEUE}
  SPFILENOTIFY_ENDQUEUE      = $00000002;
  {$EXTERNALSYM SPFILENOTIFY_ENDQUEUE}
  SPFILENOTIFY_STARTSUBQUEUE = $00000003;
  {$EXTERNALSYM SPFILENOTIFY_STARTSUBQUEUE}
  SPFILENOTIFY_ENDSUBQUEUE   = $00000004;
  {$EXTERNALSYM SPFILENOTIFY_ENDSUBQUEUE}
  SPFILENOTIFY_STARTDELETE   = $00000005;
  {$EXTERNALSYM SPFILENOTIFY_STARTDELETE}
  SPFILENOTIFY_ENDDELETE     = $00000006;
  {$EXTERNALSYM SPFILENOTIFY_ENDDELETE}
  SPFILENOTIFY_DELETEERROR   = $00000007;
  {$EXTERNALSYM SPFILENOTIFY_DELETEERROR}
  SPFILENOTIFY_STARTRENAME   = $00000008;
  {$EXTERNALSYM SPFILENOTIFY_STARTRENAME}
  SPFILENOTIFY_ENDRENAME     = $00000009;
  {$EXTERNALSYM SPFILENOTIFY_ENDRENAME}
  SPFILENOTIFY_RENAMEERROR   = $0000000a;
  {$EXTERNALSYM SPFILENOTIFY_RENAMEERROR}
  SPFILENOTIFY_STARTCOPY     = $0000000b;
  {$EXTERNALSYM SPFILENOTIFY_STARTCOPY}
  SPFILENOTIFY_ENDCOPY       = $0000000c;
  {$EXTERNALSYM SPFILENOTIFY_ENDCOPY}
  SPFILENOTIFY_COPYERROR     = $0000000d;
  {$EXTERNALSYM SPFILENOTIFY_COPYERROR}
  SPFILENOTIFY_NEEDMEDIA     = $0000000e;
  {$EXTERNALSYM SPFILENOTIFY_NEEDMEDIA}
  SPFILENOTIFY_QUEUESCAN     = $0000000f;
  {$EXTERNALSYM SPFILENOTIFY_QUEUESCAN}

//
// These are used with SetupIterateCabinet().
//
  SPFILENOTIFY_CABINETINFO    = $00000010;
  {$EXTERNALSYM SPFILENOTIFY_CABINETINFO}
  SPFILENOTIFY_FILEINCABINET  = $00000011;
  {$EXTERNALSYM SPFILENOTIFY_FILEINCABINET}
  SPFILENOTIFY_NEEDNEWCABINET = $00000012;
  {$EXTERNALSYM SPFILENOTIFY_NEEDNEWCABINET}
  SPFILENOTIFY_FILEEXTRACTED  = $00000013;
  {$EXTERNALSYM SPFILENOTIFY_FILEEXTRACTED}
  SPFILENOTIFY_FILEOPDELAYED  = $00000014;
  {$EXTERNALSYM SPFILENOTIFY_FILEOPDELAYED}

//
// These are used for backup operations
//
  SPFILENOTIFY_STARTBACKUP = $00000015;
  {$EXTERNALSYM SPFILENOTIFY_STARTBACKUP}
  SPFILENOTIFY_BACKUPERROR = $00000016;
  {$EXTERNALSYM SPFILENOTIFY_BACKUPERROR}
  SPFILENOTIFY_ENDBACKUP   = $00000017;
  {$EXTERNALSYM SPFILENOTIFY_ENDBACKUP}

//
// Extended notification for SetupScanFileQueue(Flags=SPQ_SCAN_USE_CALLBACKEX)
//
  SPFILENOTIFY_QUEUESCAN_EX = $00000018;
  {$EXTERNALSYM SPFILENOTIFY_QUEUESCAN_EX}

  SPFILENOTIFY_STARTREGISTRATION = $00000019;
  SPFILENOTIFY_ENDREGISTRATION   = $00000020;

//
// Extended notification for SetupScanFileQueue(Flags=SPQ_SCAN_USE_CALLBACK_SIGNERINFO)
//
  SPFILENOTIFY_QUEUESCAN_SIGNERINFO = $00000040;

//
// Copy notification. These are bit flags that may be combined.
//
  SPFILENOTIFY_LANGMISMATCH = $00010000;
  {$EXTERNALSYM SPFILENOTIFY_LANGMISMATCH}
  SPFILENOTIFY_TARGETEXISTS = $00020000;
  {$EXTERNALSYM SPFILENOTIFY_TARGETEXISTS}
  SPFILENOTIFY_TARGETNEWER  = $00040000;
  {$EXTERNALSYM SPFILENOTIFY_TARGETNEWER}

//
// File operation codes and callback outcomes.
//
  FILEOP_COPY   = 0;
  {$EXTERNALSYM FILEOP_COPY}
  FILEOP_RENAME = 1;
  {$EXTERNALSYM FILEOP_RENAME}
  FILEOP_DELETE = 2;
  {$EXTERNALSYM FILEOP_DELETE}
  FILEOP_BACKUP = 3;
  {$EXTERNALSYM FILEOP_BACKUP}

  FILEOP_ABORT   = 0;
  {$EXTERNALSYM FILEOP_ABORT}
  FILEOP_DOIT    = 1;
  {$EXTERNALSYM FILEOP_DOIT}
  FILEOP_SKIP    = 2;
  {$EXTERNALSYM FILEOP_SKIP}
  FILEOP_RETRY   = FILEOP_DOIT;
  {$EXTERNALSYM FILEOP_RETRY}
  FILEOP_NEWPATH = 4;
  {$EXTERNALSYM FILEOP_NEWPATH}

//
// Flags in inf copy sections
//
  COPYFLG_WARN_IF_SKIP         = $00000001; // warn if user tries to skip file
  {$EXTERNALSYM COPYFLG_WARN_IF_SKIP}
  COPYFLG_NOSKIP               = $00000002; // disallow skipping this file
  {$EXTERNALSYM COPYFLG_NOSKIP}
  COPYFLG_NOVERSIONCHECK       = $00000004; // ignore versions and overwrite target
  {$EXTERNALSYM COPYFLG_NOVERSIONCHECK}
  COPYFLG_FORCE_FILE_IN_USE    = $00000008; // force file-in-use behavior
  {$EXTERNALSYM COPYFLG_FORCE_FILE_IN_USE}
  COPYFLG_NO_OVERWRITE         = $00000010; // do not copy if file exists on target
  {$EXTERNALSYM COPYFLG_NO_OVERWRITE}
  COPYFLG_NO_VERSION_DIALOG    = $00000020; // do not copy if target is newer
  {$EXTERNALSYM COPYFLG_NO_VERSION_DIALOG}
  COPYFLG_OVERWRITE_OLDER_ONLY = $00000040; // leave target alone if version same as source
  {$EXTERNALSYM COPYFLG_OVERWRITE_OLDER_ONLY}
  COPYFLG_REPLACEONLY          = $00000400; // copy only if file exists on target
  {$EXTERNALSYM COPYFLG_REPLACEONLY}
  COPYFLG_NODECOMP             = $00000800; // don't attempt to decompress file; copy as-is
  {$EXTERNALSYM COPYFLG_NODECOMP}
  COPYFLG_REPLACE_BOOT_FILE    = $00001000; // file must be present upon reboot (i.e., it's
  {$EXTERNALSYM COPYFLG_REPLACE_BOOT_FILE}  // needed by the loader); this flag implies a reboot
  COPYFLG_NOPRUNE              = $00002000; // never prune this file
  {$EXTERNALSYM COPYFLG_NOPRUNE}

//
// Flags in inf delete sections
// New flags go in high word
//
  DELFLG_IN_USE  = $00000001; // queue in-use file for delete
  {$EXTERNALSYM DELFLG_IN_USE}
  DELFLG_IN_USE1 = $00010000; // high-word version of DELFLG_IN_USE
  {$EXTERNALSYM DELFLG_IN_USE1}

//
// Source and file paths. Used when notifying queue callback
// of SPFILENOTIFY_STARTxxx, SPFILENOTIFY_ENDxxx, and SPFILENOTIFY_xxxERROR.
//
type
  PFilePathsA = ^TFilePathsA;
  PFilePathsW = ^TFilePathsW;
  PFilePaths = PFilePathsA;
  FILEPATHS_A = packed record
    Target: PAnsiChar;
    Source: PAnsiChar; // not used for delete operations
    Win32Error: UINT;
    Flags: DWORD; // such as SP_COPY_NOSKIP for copy errors
  end;
  {$EXTERNALSYM FILEPATHS_A}
  FILEPATHS_W = packed record
    Target: PWideChar;
    Source: PWideChar; // not used for delete operations
    Win32Error: UINT;
    Flags: DWORD; // such as SP_COPY_NOSKIP for copy errors
  end;
  {$EXTERNALSYM FILEPATHS_W}
  TFilePathsA = FILEPATHS_A;
  TFilePathsW = FILEPATHS_W;
  TFilePaths = TFilePathsA;

  {$IFDEF WINXP}
  PFilePathsSignerInfoA = ^TFilePathsSignerInfoA;
  PFilePathsSignerInfoW = ^TFilePathsSignerInfoW;
  PFilePathsSignerInfo = PFilePathsSignerInfoA;
  FILEPATHS_SIGNERINFO_A = packed record
    Target: PChar;
    Source: PChar;  // not used for delete operations
    Win32Error: UINT;
    Flags: DWORD;   // such as SP_COPY_NOSKIP for copy errors
    DigitalSigner: PChar;
    Version: PChar;
    CatalogFile: PChar;
  end;
  {$EXTERNALSYM FILEPATHS_SIGNERINFO_A}
  FILEPATHS_SIGNERINFO_W = packed record
    Target: PWideChar;
    Source: PWideChar;  // not used for delete operations
    Win32Error: UINT;
    Flags: DWORD;   // such as SP_COPY_NOSKIP for copy errors
    DigitalSigner: PWideChar;
    Version: PWideChar;
    CatalogFile: PWideChar;
  end;
  {$EXTERNALSYM FILEPATHS_SIGNERINFO_W}
  TFilePathsSignerInfoA = FILEPATHS_SIGNERINFO_A;
  TFilePathsSignerInfoW = FILEPATHS_SIGNERINFO_W;
  TFilePathsSignerInfo = TFilePathsPathsSignerA;
  {$ENDIF WINXP}

//
// Structure used with SPFILENOTIFY_NEEDMEDIA
//
  PSourceMediaA = ^TSourceMediaA;
  PSourceMediaW = ^TSourceMediaW;
  PSourceMedia = PSourceMediaA;
  SOURCE_MEDIA_A = packed record
    Reserved: PAnsiChar;
    Tagfile: PAnsiChar; // may be NULL
    Description: PAnsiChar;
    //
    // Pathname part and filename part of source file
    // that caused us to need the media.
    //
    SourcePath: PAnsiChar;
    SourceFile: PAnsiChar;
    Flags: DWORD; // subset of SP_COPY_xxx
  end;
  {$EXTERNALSYM SOURCE_MEDIA_A}
  SOURCE_MEDIA_W = packed record
    Reserved: PWideChar;
    Tagfile: PWideChar; // may be NULL
    Description: PWideChar;
    //
    // Pathname part and filename part of source file
    // that caused us to need the media.
    //
    SourcePath: PWideChar;
    SourceFile: PWideChar;
    Flags: DWORD; // subset of SP_COPY_xxx
  end;
  {$EXTERNALSYM SOURCE_MEDIA_W}
  TSourceMediaA = SOURCE_MEDIA_A;
  TSourceMediaW = SOURCE_MEDIA_W;
  TSourceMedia = TSourceMediaA;

//
// Structure used with SPFILENOTIFY_CABINETINFO and
// SPFILENOTIFY_NEEDNEWCABINET
//
  PCabinetInfoA = ^TCabinetInfoA;
  PCabinetInfoW = ^TCabinetInfoW;
  PCabinetInfo = PCabinetInfoA;
  CABINET_INFO_A = packed record
    CabinetPath: PAnsiChar;
    CabinetFile: PAnsiChar;
    DiskName: PAnsiChar;
    SetId: Word;
    CabinetNumber: Word;
  end;
  {$EXTERNALSYM CABINET_INFO_A}
  CABINET_INFO_W = packed record
    CabinetPath: PWideChar;
    CabinetFile: PWideChar;
    DiskName: PWideChar;
    SetId: Word;
    CabinetNumber: Word;
  end;
  {$EXTERNALSYM CABINET_INFO_W}
  TCabinetInfoA = CABINET_INFO_A;
  TCabinetInfoW = CABINET_INFO_W;
  TCabinetInfo = TCabinetInfoA;

//
// Structure used with SPFILENOTIFY_FILEINCABINET
//
  PFileInCabinetInfoA = ^TFileInCabinetInfoA;
  PFileInCabinetInfoW = ^TFileInCabinetInfoW;
  PFileInCabinetInfo = PFileInCabinetInfoA;
  FILE_IN_CABINET_INFO_A = packed record
    NameInCabinet: PAnsiChar;
    FileSize: DWORD;
    Win32Error: DWORD;
    DosDate: Word;
    DosTime: Word;
    DosAttribs: Word;
    FullTargetName: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM FILE_IN_CABINET_INFO_A}
  FILE_IN_CABINET_INFO_W = packed record
    NameInCabinet: PWideChar;
    FileSize: DWORD;
    Win32Error: DWORD;
    DosDate: Word;
    DosTime: Word;
    DosAttribs: Word;
    FullTargetName: array [0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM FILE_IN_CABINET_INFO_W}
  TFileInCabinetInfoA = FILE_IN_CABINET_INFO_A;
  TFileInCabinetInfoW = FILE_IN_CABINET_INFO_W;
  TFileInCabinetInfo = TFileInCabinetInfoA;

  //
  // Structure used for SPFILENOTIFY_***REGISTRATION
  // callback
  //
  {$IFDEF WINXP}
  PSPRegisterControlStatusA = ^TSPRegisterControlStatusA;
  PSPRegisterControlStatusW = ^TSPRegisterControlStatusW;
  PSPRegisterControlStatus = PSPRegisterControlStatusA;
  SP_REGISTER_CONTROL_STATUSA = packed record
    cbSize: DWORD;
    FileName: PChar;
    Win32Error: DWORD;
    FailureCode: DWORD;
  end;
  {$EXTERNALSYM SP_REGISTER_CONTROL_STATUSA}
  SP_REGISTER_CONTROL_STATUSW = packed record
    cbSize: DWORD;
    FileName: PWideChar;
    Win32Error: DWORD;
    FailureCode: DWORD;
  end;
  {$EXTERNALSYM SP_REGISTER_CONTROL_STATUSW}
  TSPRegisterControlStatusA = SP_REGISTER_CONTROL_STATUSA;
  TSPRegisterControlStatusW = SP_REGISTER_CONTROL_STATUSW;
  TSPRegisterControlStatus = TSPRegisterControlStatusA;
  {$ENDIF WINXP}

//
// valid values for SP_REGISTER_CONTROL_STATUS.FailureCode field
//
const
  SPREG_SUCCESS     = $00000000;
  {$EXTERNALSYM SPREG_SUCCESS}
  SPREG_LOADLIBRARY = $00000001;
  {$EXTERNALSYM SPREG_LOADLIBRARY}
  SPREG_GETPROCADDR = $00000002;
  {$EXTERNALSYM SPREG_GETPROCADDR}
  SPREG_REGSVR      = $00000003;
  {$EXTERNALSYM SPREG_REGSVR}
  SPREG_DLLINSTALL  = $00000004;
  {$EXTERNALSYM SPREG_DLLINSTALL}
  SPREG_TIMEOUT     = $00000005;
  {$EXTERNALSYM SPREG_TIMEOUT}
  SPREG_UNKNOWN     = $FFFFFFFF;
  {$EXTERNALSYM SPREG_UNKNOWN}

//
// Define type for setup file queue
//
type
  HSPFILEQ = Pointer;
  {$EXTERNALSYM HSPFILEQ}

//
// Structure used with SetupQueueCopyIndirect
//
  PSPFileCopyParamsA = ^TSPFileCopyParamsA;
  PSPFileCopyParamsW = ^TSPFileCopyParamsW;
  PSPFileCopyParams = PSPFileCopyParamsA;
  SP_FILE_COPY_PARAMS_A = packed record
    cbSize: DWORD;
    QueueHandle: HSPFILEQ;
    SourceRootPath: PAnsiChar;
    SourcePath: PAnsiChar;
    SourceFilename: PAnsiChar;
    SourceDescription: PAnsiChar;
    SourceTagfile: PAnsiChar;
    TargetDirectory: PAnsiChar;
    TargetFilename: PAnsiChar;
    CopyStyle: DWORD;
    LayoutInf: HINF;
    SecurityDescriptor: PAnsiChar;
  end;
  {$EXTERNALSYM SP_FILE_COPY_PARAMS_A}
  SP_FILE_COPY_PARAMS_W = packed record
    cbSize: DWORD;
    QueueHandle: HSPFILEQ;
    SourceRootPath: PWideChar;
    SourcePath: PWideChar;
    SourceFilename: PWideChar;
    SourceDescription: PWideChar;
    SourceTagfile: PWideChar;
    TargetDirectory: PWideChar;
    TargetFilename: PWideChar;
    CopyStyle: DWORD;
    LayoutInf: HINF;
    SecurityDescriptor: PWideChar;
  end;
  {$EXTERNALSYM SP_FILE_COPY_PARAMS_W}
  TSPFileCopyParamsA = SP_FILE_COPY_PARAMS_A;
  TSPFileCopyParamsW = SP_FILE_COPY_PARAMS_W;
  TSPFileCopyParams = TSPFileCopyParamsA;

//
// Define type for setup disk space list
//
  HDSKSPC = Pointer;
  {$EXTERNALSYM HDSKSPC}

//
// Define type for reference to device information set
//
  HDEVINFO = Pointer;
  {$EXTERNALSYM HDEVINFO}

//
// Device information structure (references a device instance
// that is a member of a device information set)
//
  PSPDevInfoData = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: ULONG_PTR;
  end;
  {$EXTERNALSYM SP_DEVINFO_DATA}
  TSPDevInfoData = SP_DEVINFO_DATA;

//
// Device interface information structure (references a device
// interface that is associated with the device information
// element that owns it).
//
  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  {$EXTERNALSYM SP_DEVICE_INTERFACE_DATA}
  TSPDeviceInterfaceData = SP_DEVICE_INTERFACE_DATA;

//
// Flags for SP_DEVICE_INTERFACE_DATA.Flags field.
//
const
  SPINT_ACTIVE  = $00000001;
  {$EXTERNALSYM SPINT_ACTIVE}
  SPINT_DEFAULT = $00000002;
  {$EXTERNALSYM SPINT_DEFAULT}
  SPINT_REMOVED = $00000004;
  {$EXTERNALSYM SPINT_REMOVED}

//
// Backward compatibility--do not use.
//

type
  TSPInterfaceDeviceData = TSPDeviceInterfaceData;
  PSPInterfaceDeviceData = PSPDeviceInterfaceData;

const
  SPID_ACTIVE  = SPINT_ACTIVE;
  {$EXTERNALSYM SPID_ACTIVE}
  SPID_DEFAULT = SPINT_DEFAULT;
  {$EXTERNALSYM SPID_DEFAULT}
  SPID_REMOVED = SPINT_REMOVED;
  {$EXTERNALSYM SPID_REMOVED}

type
  PSPDeviceInterfaceDetailDataA = ^TSPDeviceInterfaceDetailDataA;
  PSPDeviceInterfaceDetailDataW = ^TSPDeviceInterfaceDetailDataW;
  PSPDeviceInterfaceDetailData = PSPDeviceInterfaceDetailDataA;
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    DevicePath: array [0..ANYSIZE_ARRAY - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DEVICE_INTERFACE_DETAIL_DATA_A}
  SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    DevicePath: array [0..ANYSIZE_ARRAY - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_DEVICE_INTERFACE_DETAIL_DATA_W}
  TSPDeviceInterfaceDetailDataA = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  TSPDeviceInterfaceDetailDataW = SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  TSPDeviceInterfaceDetailData = TSPDeviceInterfaceDetailDataA;

//
// Backward compatibility--do not use.
//

  TSPInterfaceDeviceDetailDataA = TSPDeviceInterfaceDetailDataA;
  TSPInterfaceDeviceDetailDataW = TSPDeviceInterfaceDetailDataW;
  TSPInterfaceDeviceDetailData = TSPInterfaceDeviceDetailDataA;
  PSPInterfaceDeviceDetailDataA = PSPDeviceInterfaceDetailDataA;
  PSPInterfaceDeviceDetailDataW = PSPDeviceInterfaceDetailDataW;
  PSPInterfaceDeviceDetailData = PSPInterfaceDeviceDetailDataA;

//
// Structure for detailed information on a device information set (used for
// SetupDiGetDeviceInfoListDetail which supercedes the functionality of
// SetupDiGetDeviceInfoListClass).
//
  PSPDevInfoListDetailDataA = ^TSPDevInfoListDetailDataA;
  PSPDevInfoListDetailDataW = ^TSPDevInfoListDetailDataW;
  PSPDevInfoListDetailData = PSPDevInfoListDetailDataA;
  SP_DEVINFO_LIST_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    RemoteMachineHandle: THandle;
    RemoteMachineName: array [0..SP_MAX_MACHINENAME_LENGTH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DEVINFO_LIST_DETAIL_DATA_A}
  SP_DEVINFO_LIST_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    RemoteMachineHandle: THandle;
    RemoteMachineName: array [0..SP_MAX_MACHINENAME_LENGTH - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_DEVINFO_LIST_DETAIL_DATA_W}
  TSPDevInfoListDetailDataA = SP_DEVINFO_LIST_DETAIL_DATA_A;
  TSPDevInfoListDetailDataW = SP_DEVINFO_LIST_DETAIL_DATA_W;
  TSPDevInfoListDetailData = TSPDevInfoListDetailDataA;

//
// Class installer function codes
//
const
  DIF_SELECTDEVICE                  = $00000001;
  {$EXTERNALSYM DIF_SELECTDEVICE}
  DIF_INSTALLDEVICE                 = $00000002;
  {$EXTERNALSYM DIF_INSTALLDEVICE}
  DIF_ASSIGNRESOURCES               = $00000003;
  {$EXTERNALSYM DIF_ASSIGNRESOURCES}
  DIF_PROPERTIES                    = $00000004;
  {$EXTERNALSYM DIF_PROPERTIES}
  DIF_REMOVE                        = $00000005;
  {$EXTERNALSYM DIF_REMOVE}
  DIF_FIRSTTIMESETUP                = $00000006;
  {$EXTERNALSYM DIF_FIRSTTIMESETUP}
  DIF_FOUNDDEVICE                   = $00000007;
  {$EXTERNALSYM DIF_FOUNDDEVICE}
  DIF_SELECTCLASSDRIVERS            = $00000008;
  {$EXTERNALSYM DIF_SELECTCLASSDRIVERS}
  DIF_VALIDATECLASSDRIVERS          = $00000009;
  {$EXTERNALSYM DIF_VALIDATECLASSDRIVERS}
  DIF_INSTALLCLASSDRIVERS           = $0000000A;
  {$EXTERNALSYM DIF_INSTALLCLASSDRIVERS}
  DIF_CALCDISKSPACE                 = $0000000B;
  {$EXTERNALSYM DIF_CALCDISKSPACE}
  DIF_DESTROYPRIVATEDATA            = $0000000C;
  {$EXTERNALSYM DIF_DESTROYPRIVATEDATA}
  DIF_VALIDATEDRIVER                = $0000000D;
  {$EXTERNALSYM DIF_VALIDATEDRIVER}
  DIF_MOVEDEVICE                    = $0000000E;
  {$EXTERNALSYM DIF_MOVEDEVICE}
  DIF_DETECT                        = $0000000F;
  {$EXTERNALSYM DIF_DETECT}
  DIF_INSTALLWIZARD                 = $00000010;
  {$EXTERNALSYM DIF_INSTALLWIZARD}
  DIF_DESTROYWIZARDDATA             = $00000011;
  {$EXTERNALSYM DIF_DESTROYWIZARDDATA}
  DIF_PROPERTYCHANGE                = $00000012;
  {$EXTERNALSYM DIF_PROPERTYCHANGE}
  DIF_ENABLECLASS                   = $00000013;
  {$EXTERNALSYM DIF_ENABLECLASS}
  DIF_DETECTVERIFY                  = $00000014;
  {$EXTERNALSYM DIF_DETECTVERIFY}
  DIF_INSTALLDEVICEFILES            = $00000015;
  {$EXTERNALSYM DIF_INSTALLDEVICEFILES}
  DIF_UNREMOVE                      = $00000016;
  {$EXTERNALSYM DIF_UNREMOVE}
  DIF_SELECTBESTCOMPATDRV           = $00000017;
  {$EXTERNALSYM DIF_SELECTBESTCOMPATDRV}
  DIF_ALLOW_INSTALL                 = $00000018;
  {$EXTERNALSYM DIF_ALLOW_INSTALL}
  DIF_REGISTERDEVICE                = $00000019;
  {$EXTERNALSYM DIF_REGISTERDEVICE}
  DIF_NEWDEVICEWIZARD_PRESELECT     = $0000001A;
  {$EXTERNALSYM DIF_NEWDEVICEWIZARD_PRESELECT}
  DIF_NEWDEVICEWIZARD_SELECT        = $0000001B;
  {$EXTERNALSYM DIF_NEWDEVICEWIZARD_SELECT}
  DIF_NEWDEVICEWIZARD_PREANALYZE    = $0000001C;
  {$EXTERNALSYM DIF_NEWDEVICEWIZARD_PREANALYZE}
  DIF_NEWDEVICEWIZARD_POSTANALYZE   = $0000001D;
  {$EXTERNALSYM DIF_NEWDEVICEWIZARD_POSTANALYZE}
  DIF_NEWDEVICEWIZARD_FINISHINSTALL = $0000001E;
  {$EXTERNALSYM DIF_NEWDEVICEWIZARD_FINISHINSTALL}
  DIF_UNUSED1                       = $0000001F;
  {$EXTERNALSYM DIF_UNUSED1}
  DIF_INSTALLINTERFACES             = $00000020;
  {$EXTERNALSYM DIF_INSTALLINTERFACES}
  DIF_DETECTCANCEL                  = $00000021;
  {$EXTERNALSYM DIF_DETECTCANCEL}
  DIF_REGISTER_COINSTALLERS         = $00000022;
  {$EXTERNALSYM DIF_REGISTER_COINSTALLERS}
  DIF_ADDPROPERTYPAGE_ADVANCED      = $00000023;
  {$EXTERNALSYM DIF_ADDPROPERTYPAGE_ADVANCED}
  DIF_ADDPROPERTYPAGE_BASIC         = $00000024;
  {$EXTERNALSYM DIF_ADDPROPERTYPAGE_BASIC}
  DIF_RESERVED1                     = $00000025;
  {$EXTERNALSYM DIF_RESERVED1}
  DIF_TROUBLESHOOTER                = $00000026;
  {$EXTERNALSYM DIF_TROUBLESHOOTER}
  DIF_POWERMESSAGEWAKE              = $00000027;
  {$EXTERNALSYM DIF_POWERMESSAGEWAKE}
  DIF_ADDREMOTEPROPERTYPAGE_ADVANCED = $00000028;
  {$EXTERNALSYM DIF_ADDREMOTEPROPERTYPAGE_ADVANCED}
  DIF_UPDATEDRIVER_UI                = $00000029;
  {$EXTERNALSYM DIF_UPDATEDRIVER_UI}
  DIF_RESERVED2                      = $00000030;
  {$EXTERNALSYM DIF_RESERVED2}

type
  DI_FUNCTION = UINT;    // Function type for device installer
  {$EXTERNALSYM DI_FUNCTION}

//
// Device installation parameters structure (associated with a
// particular device information element, or globally with a device
// information set)
//
  PSPDevInstallParamsA = ^TSPDevInstallParamsA;
  PSPDevInstallParamsW = ^TSPDevInstallParamsW;
  PSPDevInstallParams = PSPDevInstallParamsA;
  SP_DEVINSTALL_PARAMS_A = packed record
    cbSize: DWORD;
    Flags: DWORD;
    FlagsEx: DWORD;
    hwndParent: HWND;
    InstallMsgHandler: TSPFileCallback;
    InstallMsgHandlerContext: Pointer;
    FileQueue: HSPFILEQ;
    ClassInstallReserved: ULONG_PTR;
    Reserved: DWORD;
    DriverPath: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DEVINSTALL_PARAMS_A}
  SP_DEVINSTALL_PARAMS_W = packed record
    cbSize: DWORD;
    Flags: DWORD;
    FlagsEx: DWORD;
    hwndParent: HWND;
    InstallMsgHandler: TSPFileCallback;
    InstallMsgHandlerContext: Pointer;
    FileQueue: HSPFILEQ;
    ClassInstallReserved: ULONG_PTR;
    Reserved: DWORD;
    DriverPath: array [0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_DEVINSTALL_PARAMS_W}
  TSPDevInstallParamsA = SP_DEVINSTALL_PARAMS_A;
  TSPDevInstallParamsW = SP_DEVINSTALL_PARAMS_W;
  TSPDevInstallParams = TSPDevInstallParamsA;

//
// SP_DEVINSTALL_PARAMS.Flags values
//
// Flags for choosing a device
//
const
  DI_SHOWOEM       = $00000001; // support Other... button
  {$EXTERNALSYM DI_SHOWOEM}
  DI_SHOWCOMPAT    = $00000002; // show compatibility list
  {$EXTERNALSYM DI_SHOWCOMPAT}
  DI_SHOWCLASS     = $00000004; // show class list
  {$EXTERNALSYM DI_SHOWCLASS}
  DI_SHOWALL       = $00000007; // both class & compat list shown
  {$EXTERNALSYM DI_SHOWALL}
  DI_NOVCP         = $00000008; // don't create a new copy queue--use
  {$EXTERNALSYM DI_NOVCP}     // caller-supplied FileQueue
  DI_DIDCOMPAT     = $00000010; // Searched for compatible devices
  {$EXTERNALSYM DI_DIDCOMPAT}
  DI_DIDCLASS      = $00000020; // Searched for class devices
  {$EXTERNALSYM DI_DIDCLASS}
  DI_AUTOASSIGNRES = $00000040; // No UI for resources if possible
  {$EXTERNALSYM DI_AUTOASSIGNRES}

// flags returned by DiInstallDevice to indicate need to reboot/restart
  DI_NEEDRESTART = $00000080; // Reboot required to take effect
  {$EXTERNALSYM DI_NEEDRESTART}
  DI_NEEDREBOOT  = $00000100; // ""
  {$EXTERNALSYM DI_NEEDREBOOT}

// flags for device installation
  DI_NOBROWSE = $00000200; // no Browse... in InsertDisk
  {$EXTERNALSYM DI_NOBROWSE}

// Flags set by DiBuildDriverInfoList
  DI_MULTMFGS = $00000400;   // Set if multiple manufacturers in
  {$EXTERNALSYM DI_MULTMFGS} // class driver list

// Flag indicates that device is disabled
  DI_DISABLED = $00000800; // Set if device disabled
  {$EXTERNALSYM DI_DISABLED}

// Flags for Device/Class Properties
  DI_GENERALPAGE_ADDED  = $00001000;
  {$EXTERNALSYM DI_GENERALPAGE_ADDED}
  DI_RESOURCEPAGE_ADDED = $00002000;
  {$EXTERNALSYM DI_RESOURCEPAGE_ADDED}

// Flag to indicate the setting properties for this Device (or class) caused a change
// so the Dev Mgr UI probably needs to be updatd.
  DI_PROPERTIES_CHANGE = $00004000;
  {$EXTERNALSYM DI_PROPERTIES_CHANGE}

// Flag to indicate that the sorting from the INF file should be used.
  DI_INF_IS_SORTED = $00008000;
  {$EXTERNALSYM DI_INF_IS_SORTED}

// Flag to indicate that only the the INF specified by SP_DEVINSTALL_PARAMS.DriverPath
// should be searched.
  DI_ENUMSINGLEINF = $00010000;
  {$EXTERNALSYM DI_ENUMSINGLEINF}

// Flag that prevents ConfigMgr from removing/re-enumerating devices during device
// registration, installation, and deletion.
  DI_DONOTCALLCONFIGMG = $00020000;
  {$EXTERNALSYM DI_DONOTCALLCONFIGMG}

// The following flag can be used to install a device disabled
  DI_INSTALLDISABLED = $00040000;
  {$EXTERNALSYM DI_INSTALLDISABLED}

// Flag that causes SetupDiBuildDriverInfoList to build a device's compatible driver
// list from its existing class driver list, instead of the normal INF search.
  DI_COMPAT_FROM_CLASS = $00080000;
  {$EXTERNALSYM DI_COMPAT_FROM_CLASS}

// This flag is set if the Class Install params should be used.
  DI_CLASSINSTALLPARAMS = $00100000;
  {$EXTERNALSYM DI_CLASSINSTALLPARAMS}

// This flag is set if the caller of DiCallClassInstaller does NOT
// want the internal default action performed if the Class installer
// returns ERROR_DI_DO_DEFAULT.
  DI_NODI_DEFAULTACTION = $00200000;
  {$EXTERNALSYM DI_NODI_DEFAULTACTION}

// The setupx flag, DI_NOSYNCPROCESSING (0x00400000L) is not support in the Setup APIs.

// flags for device installation
  DI_QUIETINSTALL        = $00800000; // don't confuse the user with
  {$EXTERNALSYM DI_QUIETINSTALL}      // questions or excess info
  DI_NOFILECOPY          = $01000000; // No file Copy necessary
  {$EXTERNALSYM DI_NOFILECOPY}
  DI_FORCECOPY           = $02000000; // Force files to be copied from install path
  {$EXTERNALSYM DI_FORCECOPY}
  DI_DRIVERPAGE_ADDED    = $04000000; // Prop provider added Driver page.
  {$EXTERNALSYM DI_DRIVERPAGE_ADDED}
  DI_USECI_SELECTSTRINGS = $08000000; // Use Class Installer Provided strings in the Select Device Dlg
  {$EXTERNALSYM DI_USECI_SELECTSTRINGS}
  DI_OVERRIDE_INFFLAGS   = $10000000; // Override INF flags
  {$EXTERNALSYM DI_OVERRIDE_INFFLAGS}
  DI_PROPS_NOCHANGEUSAGE = $20000000; // No Enable/Disable in General Props
  {$EXTERNALSYM DI_PROPS_NOCHANGEUSAGE}

  DI_NOSELECTICONS       = $40000000; // No small icons in select device dialogs
  {$EXTERNALSYM DI_NOSELECTICONS}

  DI_NOWRITE_IDS         = DWORD($80000000); // Don't write HW & Compat IDs on install
  {$EXTERNALSYM DI_NOWRITE_IDS}

//
// SP_DEVINSTALL_PARAMS.FlagsEx values
//
  DI_FLAGSEX_USEOLDINFSEARCH          = $00000001; // Inf Search functions should not use Index Search
  {$EXTERNALSYM DI_FLAGSEX_USEOLDINFSEARCH}
  DI_FLAGSEX_AUTOSELECTRANK0          = $00000002; // SetupDiSelectDevice doesn't prompt user if rank 0 match
  {$EXTERNALSYM DI_FLAGSEX_AUTOSELECTRANK0}
  DI_FLAGSEX_CI_FAILED                = $00000004; // Failed to Load/Call class installer
  {$EXTERNALSYM DI_FLAGSEX_CI_FAILED}

  DI_FLAGSEX_DIDINFOLIST              = $00000010; // Did the Class Info List
  {$EXTERNALSYM DI_FLAGSEX_DIDINFOLIST}
  DI_FLAGSEX_DIDCOMPATINFO            = $00000020; // Did the Compat Info List
  {$EXTERNALSYM DI_FLAGSEX_DIDCOMPATINFO}

  DI_FLAGSEX_FILTERCLASSES            = $00000040;
  {$EXTERNALSYM DI_FLAGSEX_FILTERCLASSES}
  DI_FLAGSEX_SETFAILEDINSTALL         = $00000080;
  {$EXTERNALSYM DI_FLAGSEX_SETFAILEDINSTALL}
  DI_FLAGSEX_DEVICECHANGE             = $00000100;
  {$EXTERNALSYM DI_FLAGSEX_DEVICECHANGE}
  DI_FLAGSEX_ALWAYSWRITEIDS           = $00000200;
  {$EXTERNALSYM DI_FLAGSEX_ALWAYSWRITEIDS}
  DI_FLAGSEX_PROPCHANGE_PENDING       = $00000400; // One or more device property sheets have had changes made
  {$EXTERNALSYM DI_FLAGSEX_PROPCHANGE_PENDING}     // to them, and need to have a DIF_PROPERTYCHANGE occur.

  DI_FLAGSEX_ALLOWEXCLUDEDDRVS        = $00000800;
  {$EXTERNALSYM DI_FLAGSEX_ALLOWEXCLUDEDDRVS}
  DI_FLAGSEX_NOUIONQUERYREMOVE        = $00001000;
  {$EXTERNALSYM DI_FLAGSEX_NOUIONQUERYREMOVE}
  DI_FLAGSEX_USECLASSFORCOMPAT        = $00002000; // Use the device's class when building compat drv list.
  {$EXTERNALSYM DI_FLAGSEX_USECLASSFORCOMPAT}      // (Ignored if DI_COMPAT_FROM_CLASS flag is specified.)
  DI_FLAGSEX_OLDINF_IN_CLASSLIST      = $00004000; // Search legacy INFs when building class driver list.
  {$EXTERNALSYM DI_FLAGSEX_OLDINF_IN_CLASSLIST}
  DI_FLAGSEX_NO_DRVREG_MODIFY         = $00008000; // Don't run AddReg and DelReg for device's software (driver) key.
  {$EXTERNALSYM DI_FLAGSEX_NO_DRVREG_MODIFY}
  DI_FLAGSEX_IN_SYSTEM_SETUP          = $00010000; // Installation is occurring during initial system setup.
  {$EXTERNALSYM DI_FLAGSEX_IN_SYSTEM_SETUP}
  DI_FLAGSEX_INET_DRIVER              = $00020000; // Driver came from Windows Update
  {$EXTERNALSYM DI_FLAGSEX_INET_DRIVER}
  DI_FLAGSEX_APPENDDRIVERLIST         = $00040000; // Cause SetupDiBuildDriverInfoList to append
  {$EXTERNALSYM DI_FLAGSEX_APPENDDRIVERLIST}       // a new driver list to an existing list.
  DI_FLAGSEX_PREINSTALLBACKUP         = $00080000; // backup all files required by old inf before install
  {$EXTERNALSYM DI_FLAGSEX_PREINSTALLBACKUP}
  DI_FLAGSEX_BACKUPONREPLACE          = $00100000; // backup files required by old inf as they are replaced
  {$EXTERNALSYM DI_FLAGSEX_BACKUPONREPLACE}
  DI_FLAGSEX_DRIVERLIST_FROM_URL      = $00200000; // build driver list from INF(s) retrieved from URL specified
  {$EXTERNALSYM DI_FLAGSEX_DRIVERLIST_FROM_URL}
                                                   // in SP_DEVINSTALL_PARAMS.DriverPath (empty string means
                                                   // Windows Update website)
  DI_FLAGSEX_RESERVED1                = $00400000;
  {$EXTERNALSYM DI_FLAGSEX_RESERVED1}
  DI_FLAGSEX_EXCLUDE_OLD_INET_DRIVERS = $00800000; // Don't include old Internet drivers when building
  {$EXTERNALSYM DI_FLAGSEX_EXCLUDE_OLD_INET_DRIVERS}
                                                   // a driver list.
  DI_FLAGSEX_POWERPAGE_ADDED          = $01000000; // class installer added their own power page
  {$EXTERNALSYM DI_FLAGSEX_POWERPAGE_ADDED}

  DI_FLAGSEX_FILTERSIMILARDRIVERS     = $02000000;  // only include similar drivers in class list
  {$EXTERNALSYM DI_FLAGSEX_FILTERSIMILARDRIVERS}
  DI_FLAGSEX_INSTALLEDDRIVER          = $04000000;  // only add the installed driver to the class or compat
  {$EXTERNALSYM DI_FLAGSEX_INSTALLEDDRIVER}
                                                    // driver list.  Used in calls to SetupDiBuildDriverInfoList
  DI_FLAGSEX_NO_CLASSLIST_NODE_MERGE  = $08000000;  // Don't remove identical driver nodes from the class list
  {$EXTERNALSYM DI_FLAGSEX_NO_CLASSLIST_NODE_MERGE}
  DI_FLAGSEX_ALTPLATFORM_DRVSEARCH    = $10000000;  // Build driver list based on alternate platform information
  {$EXTERNALSYM DI_FLAGSEX_ALTPLATFORM_DRVSEARCH}
                                                    // specified in associated file queue
  DI_FLAGSEX_RESTART_DEVICE_ONLY      = $20000000;  // only restart the device drivers are being installed on as
  {$EXTERNALSYM DI_FLAGSEX_RESTART_DEVICE_ONLY}

//
// Class installation parameters header.  This must be the first field of any
// class install parameter structure.  The InstallFunction field must be set to
// the function code corresponding to the structure, and the cbSize field must
// be set to the size of the header structure.  E.g.,
//
// SP_ENABLECLASS_PARAMS EnableClassParams;
//
// EnableClassParams.ClassInstallHeader.cbSize = sizeof(SP_CLASSINSTALL_HEADER);
// EnableClassParams.ClassInstallHeader.InstallFunction = DIF_ENABLECLASS;
//
type
  PSPClassInstallHeader = ^TSPClassInstallHeader;
  SP_CLASSINSTALL_HEADER = packed record
    cbSize: DWORD;
    InstallFunction: DI_FUNCTION;
  end;
  {$EXTERNALSYM SP_CLASSINSTALL_HEADER}
  TSPClassInstallHeader = SP_CLASSINSTALL_HEADER;

//
// Structure corresponding to a DIF_ENABLECLASS install function.
//
  PSPEnableClassParams = ^TSPEnableClassParams;
  SP_ENABLECLASS_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    ClassGuid: TGUID;
    EnableMessage: DWORD;
  end;
  {$EXTERNALSYM SP_ENABLECLASS_PARAMS}
  TSPEnableClassParams = SP_ENABLECLASS_PARAMS;

const
  ENABLECLASS_QUERY   = 0;
  {$EXTERNALSYM ENABLECLASS_QUERY}
  ENABLECLASS_SUCCESS = 1;
  {$EXTERNALSYM ENABLECLASS_SUCCESS}
  ENABLECLASS_FAILURE = 2;
  {$EXTERNALSYM ENABLECLASS_FAILURE}

//
// Structure corresponding to a DIF_MOVEDEVICE install function.
//
type
  PSPMoveDevParams = ^TSPMoveDevParams;
  SP_MOVEDEV_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    SourceDeviceInfoData: TSPDevInfoData;
  end;
  {$EXTERNALSYM SP_MOVEDEV_PARAMS}
  TSPMoveDevParams = SP_MOVEDEV_PARAMS;

//
// Values indicating a change in a device's state
//
const
  DICS_ENABLE     = $00000001;
  {$EXTERNALSYM DICS_ENABLE}
  DICS_DISABLE    = $00000002;
  {$EXTERNALSYM DICS_DISABLE}
  DICS_PROPCHANGE = $00000003;
  {$EXTERNALSYM DICS_PROPCHANGE}
  DICS_START      = $00000004;
  {$EXTERNALSYM DICS_START}
  DICS_STOP       = $00000005;
  {$EXTERNALSYM DICS_STOP}

//
// Values specifying the scope of a device property change
//
  DICS_FLAG_GLOBAL         = $00000001;  // make change in all hardware profiles
  {$EXTERNALSYM DICS_FLAG_GLOBAL}
  DICS_FLAG_CONFIGSPECIFIC = $00000002;  // make change in specified profile only
  {$EXTERNALSYM DICS_FLAG_CONFIGSPECIFIC}
  DICS_FLAG_CONFIGGENERAL  = $00000004;  // 1 or more hardware profile-specific
  {$EXTERNALSYM DICS_FLAG_CONFIGGENERAL} // changes to follow.

//
// Structure corresponding to a DIF_PROPERTYCHANGE install function.
//
type
  PSPPropChangeParams = ^TSPPropChangeParams;
  SP_PROPCHANGE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    StateChange: DWORD;
    Scope: DWORD;
    HwProfile: DWORD;
  end;
  {$EXTERNALSYM SP_PROPCHANGE_PARAMS}
  TSPPropChangeParams = SP_PROPCHANGE_PARAMS;

//
// Structure corresponding to a DIF_REMOVE install function.
//
  PSPRemoveDeviceParams = ^TSPRemoveDeviceParams;
  SP_REMOVEDEVICE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Scope: DWORD;
    HwProfile: DWORD;
  end;
  {$EXTERNALSYM SP_REMOVEDEVICE_PARAMS}
  TSPRemoveDeviceParams = SP_REMOVEDEVICE_PARAMS;

const
  DI_REMOVEDEVICE_GLOBAL         = $00000001;
  {$EXTERNALSYM DI_REMOVEDEVICE_GLOBAL}
  DI_REMOVEDEVICE_CONFIGSPECIFIC = $00000002;
  {$EXTERNALSYM DI_REMOVEDEVICE_CONFIGSPECIFIC}

//
// Structure corresponding to a DIF_UNREMOVE install function.
//
type
  PSPUnremoveDeviceParams = ^TSPUnremoveDeviceParams;
  SP_UNREMOVEDEVICE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Scope: DWORD;
    HwProfile: DWORD;
  end;
  {$EXTERNALSYM SP_UNREMOVEDEVICE_PARAMS}
  TSPUnremoveDeviceParams = SP_UNREMOVEDEVICE_PARAMS;

const
  DI_UNREMOVEDEVICE_CONFIGSPECIFIC = $00000002;
  {$EXTERNALSYM DI_UNREMOVEDEVICE_CONFIGSPECIFIC}

//
// Structure corresponding to a DIF_SELECTDEVICE install function.
//
type
  PSPSelectDeviceParamsA = ^TSPSelectDeviceParamsA;
  PSPSelectDeviceParamsW = ^TSPSelectDeviceParamsW;
  PSPSelectDeviceParams = PSPSelectDeviceParamsA;
  SP_SELECTDEVICE_PARAMS_A = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Title: array [0..MAX_TITLE_LEN - 1] of AnsiChar;
    Instructions: array [0..MAX_INSTRUCTION_LEN - 1] of AnsiChar;
    ListLabel: array [0..MAX_LABEL_LEN - 1] of AnsiChar;
    SubTitle: array [0..MAX_SUBTITLE_LEN - 1] of AnsiChar;
    Reserved: array [0..1] of Byte; // DWORD size alignment
  end;
  {$EXTERNALSYM SP_SELECTDEVICE_PARAMS_A}
  SP_SELECTDEVICE_PARAMS_W = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Title: array [0..MAX_TITLE_LEN - 1] of WideChar;
    Instructions: array [0..MAX_INSTRUCTION_LEN - 1] of WideChar;
    ListLabel: array [0..MAX_LABEL_LEN - 1] of WideChar;
    SubTitle: array [0..MAX_SUBTITLE_LEN - 1] of WideChar;
    Reserved: array [0..1] of Byte; // DWORD size alignment
  end;
  {$EXTERNALSYM SP_SELECTDEVICE_PARAMS_W}
  TSPSelectdeviceParamsA = SP_SELECTDEVICE_PARAMS_A;
  TSPSelectdeviceParamsW = SP_SELECTDEVICE_PARAMS_W;
  TSPSelectdeviceParams = TSPSelectdeviceParamsA;

//
// Callback routine for giving progress notification during detection
//
  PDetectProgressNotify = function(ProgressNotifyParam: Pointer; DetectComplete: DWORD): BOOL; stdcall;

// where:
//     ProgressNotifyParam - value supplied by caller requesting detection.
//     DetectComplete - Percent completion, to be incremented by class
//                      installer, as it steps thru its detection.
//
// Return Value - If TRUE, then detection is cancelled.  Allows caller
//                requesting detection to stop detection asap.
//

//
// Structure corresponding to a DIF_DETECT install function.
//
  PSPDetectDeviceParams = ^TSPDetectDeviceParams;
  SP_DETECTDEVICE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    DetectProgressNotify: PDetectProgressNotify;
    ProgressNotifyParam: Pointer;
  end;
  {$EXTERNALSYM SP_DETECTDEVICE_PARAMS}
  TSPDetectDeviceParams = SP_DETECTDEVICE_PARAMS;

//
// 'Add New Device' installation wizard structure (backward-compatibility
// only--respond to DIF_NEWDEVICEWIZARD_* requests instead).
//
// Structure corresponding to a DIF_INSTALLWIZARD install function.
// (NOTE: This structure is also applicable for DIF_DESTROYWIZARDDATA,
// but DIF_INSTALLWIZARD is the associated function code in the class
// installation parameter structure in both cases.)
//
// Define maximum number of dynamic wizard pages that can be added to
// hardware install wizard.
//
const
  MAX_INSTALLWIZARD_DYNAPAGES = 20;
  {$EXTERNALSYM MAX_INSTALLWIZARD_DYNAPAGES}

type
  PSPInstallWizardData = ^TSPInstallWizardData;
  SP_INSTALLWIZARD_DATA = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Flags: DWORD;
    DynamicPages: array [0..MAX_INSTALLWIZARD_DYNAPAGES - 1] of HPROPSHEETPAGE;
    NumDynamicPages: DWORD;
    DynamicPageFlags: DWORD;
    PrivateFlags: DWORD;
    PrivateData: LPARAM;
    hwndWizardDlg: HWND;
  end;
  {$EXTERNALSYM SP_INSTALLWIZARD_DATA}
  TSPInstallWizardData = SP_INSTALLWIZARD_DATA;

//
// SP_INSTALLWIZARD_DATA.Flags values
//
const
  NDW_INSTALLFLAG_DIDFACTDEFS        = $00000001;
  {$EXTERNALSYM NDW_INSTALLFLAG_DIDFACTDEFS}
  NDW_INSTALLFLAG_HARDWAREALLREADYIN = $00000002;
  {$EXTERNALSYM NDW_INSTALLFLAG_HARDWAREALLREADYIN}
  NDW_INSTALLFLAG_NEEDRESTART        = DI_NEEDRESTART;
  {$EXTERNALSYM NDW_INSTALLFLAG_NEEDRESTART}
  NDW_INSTALLFLAG_NEEDREBOOT         = DI_NEEDREBOOT;
  {$EXTERNALSYM NDW_INSTALLFLAG_NEEDREBOOT}
  NDW_INSTALLFLAG_NEEDSHUTDOWN       = $00000200;
  {$EXTERNALSYM NDW_INSTALLFLAG_NEEDSHUTDOWN}
  NDW_INSTALLFLAG_EXPRESSINTRO       = $00000400;
  {$EXTERNALSYM NDW_INSTALLFLAG_EXPRESSINTRO}
  NDW_INSTALLFLAG_SKIPISDEVINSTALLED = $00000800;
  {$EXTERNALSYM NDW_INSTALLFLAG_SKIPISDEVINSTALLED}
  NDW_INSTALLFLAG_NODETECTEDDEVS     = $00001000;
  {$EXTERNALSYM NDW_INSTALLFLAG_NODETECTEDDEVS}
  NDW_INSTALLFLAG_INSTALLSPECIFIC    = $00002000;
  {$EXTERNALSYM NDW_INSTALLFLAG_INSTALLSPECIFIC}
  NDW_INSTALLFLAG_SKIPCLASSLIST      = $00004000;
  {$EXTERNALSYM NDW_INSTALLFLAG_SKIPCLASSLIST}
  NDW_INSTALLFLAG_CI_PICKED_OEM      = $00008000;
  {$EXTERNALSYM NDW_INSTALLFLAG_CI_PICKED_OEM}
  NDW_INSTALLFLAG_PCMCIAMODE         = $00010000;
  {$EXTERNALSYM NDW_INSTALLFLAG_PCMCIAMODE}
  NDW_INSTALLFLAG_PCMCIADEVICE       = $00020000;
  {$EXTERNALSYM NDW_INSTALLFLAG_PCMCIADEVICE}
  NDW_INSTALLFLAG_USERCANCEL         = $00040000;
  {$EXTERNALSYM NDW_INSTALLFLAG_USERCANCEL}
  NDW_INSTALLFLAG_KNOWNCLASS         = $00080000;
  {$EXTERNALSYM NDW_INSTALLFLAG_KNOWNCLASS}

//
// SP_INSTALLWIZARD_DATA.DynamicPageFlags values
//
// This flag is set if a Class installer has added pages to the install wizard.
//
  DYNAWIZ_FLAG_PAGESADDED = $00000001;
  {$EXTERNALSYM DYNAWIZ_FLAG_PAGESADDED}

//
// Set this flag if you jump to the analyze page, and want it to
// handle conflicts for you.  NOTE.  You will not get control back
// in the event of a conflict if you set this flag.
//
  DYNAWIZ_FLAG_ANALYZE_HANDLECONFLICT = $00000008;
  {$EXTERNALSYM DYNAWIZ_FLAG_ANALYZE_HANDLECONFLICT}

//
// The following flags are not used by the Windows NT hardware wizard.
//
  DYNAWIZ_FLAG_INSTALLDET_NEXT = $00000002;
  {$EXTERNALSYM DYNAWIZ_FLAG_INSTALLDET_NEXT}
  DYNAWIZ_FLAG_INSTALLDET_PREV = $00000004;
  {$EXTERNALSYM DYNAWIZ_FLAG_INSTALLDET_PREV}

//
// Reserve a range of wizard page resource IDs for internal use.  Some of
// these IDs are for use by class installers that respond to the obsolete
// DIF_INSTALLWIZARD/DIF_DESTROYWIZARDDATA messages.  These IDs are listed
// below.
//
  MIN_IDD_DYNAWIZ_RESOURCE_ID = 10000;
  {$EXTERNALSYM MIN_IDD_DYNAWIZ_RESOURCE_ID}
  MAX_IDD_DYNAWIZ_RESOURCE_ID = 11000;
  {$EXTERNALSYM MAX_IDD_DYNAWIZ_RESOURCE_ID}

//
// Define wizard page resource IDs to be used when adding custom pages to the
// hardware install wizard via DIF_INSTALLWIZARD.  Pages marked with
// (CLASS INSTALLER PROVIDED) _must_ be supplied by the class installer if it
// responds to the DIF_INSTALLWIZARD request.
//

//
// Resource ID for the first page that the install wizard will go to after
// adding the class installer pages.  (CLASS INSTALLER PROVIDED)
//
  IDD_DYNAWIZ_FIRSTPAGE = 10000;
  {$EXTERNALSYM IDD_DYNAWIZ_FIRSTPAGE}

//
// Resource ID for the page that the Select Device page will go back to.
// (CLASS INSTALLER PROVIDED)
//
  IDD_DYNAWIZ_SELECT_PREVPAGE = 10001;
  {$EXTERNALSYM IDD_DYNAWIZ_SELECT_PREVPAGE}

//
// Resource ID for the page that the Select Device page will go forward to.
// (CLASS INSTALLER PROVIDED)
//
  IDD_DYNAWIZ_SELECT_NEXTPAGE = 10002;
  {$EXTERNALSYM IDD_DYNAWIZ_SELECT_NEXTPAGE}

//
// Resource ID for the page that the Analyze dialog should go back to
// This will only be used in the event that there is a problem, and the user
// selects Back from the analyze proc. (CLASS INSTALLER PROVIDED)
//
  IDD_DYNAWIZ_ANALYZE_PREVPAGE = 10003;
  {$EXTERNALSYM IDD_DYNAWIZ_ANALYZE_PREVPAGE}

//
// Resource ID for the page that the Analyze dialog should go to if it
// continues from the analyze proc. (CLASS INSTALLER PROVIDED)
//
  IDD_DYNAWIZ_ANALYZE_NEXTPAGE = 10004;
  {$EXTERNALSYM IDD_DYNAWIZ_ANALYZE_NEXTPAGE}

//
// Resource ID of the hardware install wizard's select device page.
// This ID can be used to go directly to the hardware install wizard's select
// device page.  (This is the resource ID of the Select Device wizard page
// retrieved via SetupDiGetWizardPage when SPWPT_SELECTDEVICE is the requested
// PageType.)
//
  IDD_DYNAWIZ_SELECTDEV_PAGE = 10009;
  {$EXTERNALSYM IDD_DYNAWIZ_SELECTDEV_PAGE}

//
// Resource ID of the hardware install wizard's device analysis page.
// This ID can be use to go directly to the hardware install wizard's analysis
// page.
//
  IDD_DYNAWIZ_ANALYZEDEV_PAGE = 10010;
  {$EXTERNALSYM IDD_DYNAWIZ_ANALYZEDEV_PAGE}

//
// Resource ID of the hardware install wizard's install detected devices page.
// This ID can be use to go directly to the hardware install wizard's install
// detected devices page.
//
  IDD_DYNAWIZ_INSTALLDETECTEDDEVS_PAGE = 10011;
  {$EXTERNALSYM IDD_DYNAWIZ_INSTALLDETECTEDDEVS_PAGE}

//
// Resource ID of the hardware install wizard's select class page.
// This ID can be use to go directly to the hardware install wizard's select
// class page.
//
  IDD_DYNAWIZ_SELECTCLASS_PAGE = 10012;
  {$EXTERNALSYM IDD_DYNAWIZ_SELECTCLASS_PAGE}

//
// The following class installer-provided wizard page resource IDs are not used
// by the Windows NT hardware wizard.
//
  IDD_DYNAWIZ_INSTALLDETECTED_PREVPAGE = 10006;
  {$EXTERNALSYM IDD_DYNAWIZ_INSTALLDETECTED_PREVPAGE}
  IDD_DYNAWIZ_INSTALLDETECTED_NEXTPAGE = 10007;
  {$EXTERNALSYM IDD_DYNAWIZ_INSTALLDETECTED_NEXTPAGE}
  IDD_DYNAWIZ_INSTALLDETECTED_NODEVS   = 10008;
  {$EXTERNALSYM IDD_DYNAWIZ_INSTALLDETECTED_NODEVS}

//
// Structure corresponding to the following DIF_NEWDEVICEWIZARD_* install
// functions:
//
//     DIF_NEWDEVICEWIZARD_PRESELECT
//     DIF_NEWDEVICEWIZARD_SELECT
//     DIF_NEWDEVICEWIZARD_PREANALYZE
//     DIF_NEWDEVICEWIZARD_POSTANALYZE
//     DIF_NEWDEVICEWIZARD_FINISHINSTALL
//
type
  PSPNewDeviceWizardData = ^TSPNewDeviceWizardData;
  SP_NEWDEVICEWIZARD_DATA = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    Flags: DWORD; // presently unused--must be zero.
    DynamicPages: array [0..MAX_INSTALLWIZARD_DYNAPAGES - 1] of HPROPSHEETPAGE;
    NumDynamicPages: DWORD;
    hwndWizardDlg: HWND;
  end;
  {$EXTERNALSYM SP_NEWDEVICEWIZARD_DATA}
  TSPNewDeviceWizardData = SP_NEWDEVICEWIZARD_DATA;

//
// Structure corresponding to the DIF_TROUBLESHOOTER install function
//
  PSPTroubleShooterParamsA = ^TSPTroubleShooterParamsA;
  PSPTroubleShooterParamsW = ^TSPTroubleShooterParamsW;
  PSPTroubleShooterParams = PSPTroubleShooterParamsA;
  SP_TROUBLESHOOTER_PARAMS_A = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    ChmFile: array [0..MAX_PATH - 1] of AnsiChar;
    HtmlTroubleShooter: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_TROUBLESHOOTER_PARAMS_A}
  SP_TROUBLESHOOTER_PARAMS_W = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    ChmFile: array [0..MAX_PATH - 1] of WideChar;
    HtmlTroubleShooter: array [0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_TROUBLESHOOTER_PARAMS_W}
  TSPTroubleShooterParamsA = SP_TROUBLESHOOTER_PARAMS_A;
  TSPTroubleShooterParamsW = SP_TROUBLESHOOTER_PARAMS_W;
  TSPTroubleShooterParams = TSPTroubleShooterParamsA;

//
// Structure corresponding to the DIF_POWERMESSAGEWAKE install function
//
  PSPPowerMessageWakeParamsA = ^TSPPowerMessageWakeParamsA;
  PSPPowerMessageWakeParamsW = ^TSPPowerMessageWakeParamsW;
  PSPPowerMessageWakeParams = PSPPowerMessageWakeParamsA;
  SP_POWERMESSAGEWAKE_PARAMS_A = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    PowerMessageWake: array [0..(LINE_LEN * 2) - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_POWERMESSAGEWAKE_PARAMS_A}
  SP_POWERMESSAGEWAKE_PARAMS_W = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    PowerMessageWake: array [0..(LINE_LEN * 2) - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_POWERMESSAGEWAKE_PARAMS_W}
  TSPPowerMessageWakeParamsA = SP_POWERMESSAGEWAKE_PARAMS_A;
  TSPPowerMessageWakeParamsW = SP_POWERMESSAGEWAKE_PARAMS_W;
  TSPPowerMessageWakeParams = TSPPowerMessageWakeParamsA;

//
// Driver information structure (member of a driver info list that may be associated
// with a particular device instance, or (globally) with a device information set)
//
  PSPDrvInfoDataV2A = ^TSPDrvInfoDataV2A;
  PSPDrvInfoDataV2W = ^TSPDrvInfoDataV2W;
  PSPDrvInfoDataV2 = PSPDrvInfoDataV2A;
  SP_DRVINFO_DATA_V2_A = packed record
    cbSize: DWORD;
    DriverType: DWORD;
    Reserved: ULONG_PTR;
    Description: array [0..LINE_LEN - 1] of AnsiChar;
    MfgName: array [0..LINE_LEN - 1] of AnsiChar;
    ProviderName: array [0..LINE_LEN - 1] of AnsiChar;
    DriverDate: TFileTime;
    DriverVersion: Int64;
  end;
  {$EXTERNALSYM SP_DRVINFO_DATA_V2_A}
  SP_DRVINFO_DATA_V2_W = packed record
    cbSize: DWORD;
    DriverType: DWORD;
    Reserved: ULONG_PTR;
    Description: array [0..LINE_LEN - 1] of WideChar;
    MfgName: array [0..LINE_LEN - 1] of WideChar;
    ProviderName: array [0..LINE_LEN - 1] of WideChar;
    DriverDate: TFileTime;
    DriverVersion: Int64;
  end;
  {$EXTERNALSYM SP_DRVINFO_DATA_V2_W}
  TSPDrvInfoDataV2A = SP_DRVINFO_DATA_V2_A;
  TSPDrvInfoDataV2W = SP_DRVINFO_DATA_V2_W;
  TSPDrvInfoDataV2 = TSPDrvInfoDataV2A;

//
// Version 1 of the SP_DRVINFO_DATA structures, used only for compatibility
// with Windows NT 4.0/Windows 95/98 SETUPAPI.DLL
//
  PSPDrvInfoDataV1A = ^TSPDrvInfoDataV1A;
  PSPDrvInfoDataV1W = ^TSPDrvInfoDataV1W;
  PSPDrvInfoDataV1 = PSPDrvInfoDataV1A;
  SP_DRVINFO_DATA_V1_A = packed record
    cbSize: DWORD;
    DriverType: DWORD;
    Reserved: ULONG_PTR;
    Description: array [0..LINE_LEN - 1] of AnsiChar;
    MfgName: array [0..LINE_LEN - 1] of AnsiChar;
    ProviderName: array [0..LINE_LEN - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DRVINFO_DATA_V1_A}
  SP_DRVINFO_DATA_V1_W = packed record
    cbSize: DWORD;
    DriverType: DWORD;
    Reserved: ULONG_PTR;
    Description: array [0..LINE_LEN - 1] of WideChar;
    MfgName: array [0..LINE_LEN - 1] of WideChar;
    ProviderName: array [0..LINE_LEN - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_DRVINFO_DATA_V1_W}
  TSPDrvInfoDataV1A = SP_DRVINFO_DATA_V1_A;
  TSPDrvInfoDataV1W = SP_DRVINFO_DATA_V1_W;
  TSPDrvInfoDataV1 = TSPDrvInfoDataV1A;

  {$IFDEF USE_SP_DRVINFO_DATA_V1}
  TSPDrvInfoDataA = TSPDrvInfoDataV1A;
  TSPDrvInfoDataW = TSPDrvInfoDataV1W;
  TSPDrvInfoData = TSPDrvInfoDataA;
  PSPDrvInfoDataA = PSPDrvInfoDataV1A;
  PSPDrvInfoDataW = PSPDrvInfoDataV1W;
  PSPDrvInfoData = PSPDrvInfoDataA;
  {$ELSE}
  TSPDrvInfoDataA = TSPDrvInfoDataV2A;
  TSPDrvInfoDataW = TSPDrvInfoDataV2W;
  TSPDrvInfoData = TSPDrvInfoDataA;
  PSPDrvInfoDataA = PSPDrvInfoDataV2A;
  PSPDrvInfoDataW = PSPDrvInfoDataV2W;
  PSPDrvInfoData = PSPDrvInfoDataA;
  {$ENDIF USE_SP_DRVINFO_DATA_V1}

//
// Driver information details structure (provides detailed information about a
// particular driver information structure)
//
  PSPDrvInfoDetailDataA = ^TSPDrvInfoDetailDataA;
  PSPDrvInfoDetailDataW = ^TSPDrvInfoDetailDataW;
  PSPDrvInfoDetailData = PSPDrvInfoDetailDataA;
  SP_DRVINFO_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    InfDate: TFileTime;
    CompatIDsOffset: DWORD;
    CompatIDsLength: DWORD;
    Reserved: ULONG_PTR;
    SectionName: array [0..LINE_LEN - 1] of AnsiChar;
    InfFileName: array [0..MAX_PATH - 1] of AnsiChar;
    DrvDescription: array [0..LINE_LEN - 1] of AnsiChar;
    HardwareID: array [0..ANYSIZE_ARRAY - 1] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DRVINFO_DETAIL_DATA_A}
  SP_DRVINFO_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    InfDate: TFileTime;
    CompatIDsOffset: DWORD;
    CompatIDsLength: DWORD;
    Reserved: ULONG_PTR;
    SectionName: array [0..LINE_LEN - 1] of WideChar;
    InfFileName: array [0..MAX_PATH - 1] of WideChar;
    DrvDescription: array [0..LINE_LEN - 1] of WideChar;
    HardwareID: array [0..ANYSIZE_ARRAY - 1] of WideChar;
  end;
  {$EXTERNALSYM SP_DRVINFO_DETAIL_DATA_W}
  TSPDrvInfoDetailDataA = SP_DRVINFO_DETAIL_DATA_A;
  TSPDrvInfoDetailDataW = SP_DRVINFO_DETAIL_DATA_W;
  TSPDrvInfoDetailData = TSPDrvInfoDetailDataA;

//
// Driver installation parameters (associated with a particular driver
// information element)
//
  PSPDrvInstallParams = ^TSPDrvInstallParams;
  SP_DRVINSTALL_PARAMS = packed record
    cbSize: DWORD;
    Rank: DWORD;
    Flags: DWORD;
    PrivateData: DWORD_PTR;
    Reserved: DWORD;
  end;
  {$EXTERNALSYM SP_DRVINSTALL_PARAMS}
  TSPDrvInstallParams = SP_DRVINSTALL_PARAMS;

//
// SP_DRVINSTALL_PARAMS.Flags values
//
const
  DNF_DUPDESC           = $00000001; // Multiple providers have same desc
  {$EXTERNALSYM DNF_DUPDESC}
  DNF_OLDDRIVER         = $00000002; // Driver node specifies old/current driver
  {$EXTERNALSYM DNF_OLDDRIVER}
  DNF_EXCLUDEFROMLIST   = $00000004; // If set, this driver node will not be
  {$EXTERNALSYM DNF_EXCLUDEFROMLIST} // displayed in any driver select dialogs.
  DNF_NODRIVER          = $00000008; // if we want to install no driver
  {$EXTERNALSYM DNF_NODRIVER}        // (e.g no mouse drv)
  DNF_LEGACYINF         = $00000010; // this driver node comes from an old-style INF
  {$EXTERNALSYM DNF_LEGACYINF}
  DNF_CLASS_DRIVER      = $00000020; // Driver node represents a class driver
  {$EXTERNALSYM DNF_CLASS_DRIVER}
  DNF_COMPATIBLE_DRIVER = $00000040; // Driver node represents a compatible driver
  {$EXTERNALSYM DNF_COMPATIBLE_DRIVER}
  DNF_INET_DRIVER       = $00000080; // Driver comes from an internet source
  {$EXTERNALSYM DNF_INET_DRIVER}
  DNF_UNUSED1           = $00000100;
  {$EXTERNALSYM DNF_UNUSED1}
  DNF_INDEXED_DRIVER    = $00000200; // Driver is contained in the Windows Driver Index
  {$EXTERNALSYM DNF_INDEXED_DRIVER}
  DNF_OLD_INET_DRIVER   = $00000400; // Driver came from the Internet, but we don't currently
  {$EXTERNALSYM DNF_OLD_INET_DRIVER} // have access to it's source files.  Never attempt to
                                     // install a driver with this flag!
  DNF_BAD_DRIVER        = $00000800; // Driver node should not be used at all
  {$EXTERNALSYM DNF_BAD_DRIVER}
  DNF_DUPPROVIDER       = $00001000; // Multiple drivers have the same provider and desc
  {$EXTERNALSYM DNF_DUPPROVIDER}

  DNF_INF_IS_SIGNED     = $00002000;  // If file is digitally signed
  {$EXTERNALSYM DNF_INF_IS_SIGNED}
  DNF_OEM_F6_INF        = $00004000;  // INF specified from F6 during textmode setup.
  {$EXTERNALSYM DNF_OEM_F6_INF}
  DNF_DUPDRIVERVER      = $00008000;  // Multipe drivers have the same desc, provider, and DriverVer values
  {$EXTERNALSYM DNF_DUPDRIVERVER}
  DNF_BASIC_DRIVER      = $00010000;  // Driver provides basic functionality, but should
  {$EXTERNALSYM DNF_BASIC_DRIVER}
                                      // not be chosen if other signed drivers exist.
//
// Rank values (the lower the Rank number, the better the Rank)
//
  DRIVER_HARDWAREID_RANK = $00000FFF;   // Any rank less than or equal to
  {$EXTERNALSYM DRIVER_HARDWAREID_RANK} // this value is a trusted
                                        // HardwareID match

  DRIVER_COMPATID_RANK   = $00003FFF;  // Any rank less than or equal to
  {$EXTERNALSYM DRIVER_COMPATID_RANK}  // this (and greater than
                                       // DRIVER_HARDWAREID_RANK) is a
                                       // trusted CompatibleID match

  DRIVER_UNTRUSTED_RANK  = $00008000;  // Any rank with this bit set is an
  {$EXTERNALSYM DRIVER_UNTRUSTED_RANK} // "untrusted" rank, meaning that
                                       // the INF was unsigned.

  DRIVER_UNTRUSTED_HARDWAREID_RANK   = $00008FFF;  // Any rank less than or equal to
  {$EXTERNALSYM DRIVER_UNTRUSTED_HARDWAREID_RANK}  // this value (and greater than
                                                   // or equal to DRIVER_UNTRUSTED_RANK)
                                                   // is an untrusted HardwareID match

  DRIVER_UNTRUSTED_COMPATID_RANK     = $0000BFFF;  // Any rank less than or equal to
  {$EXTERNALSYM DRIVER_UNTRUSTED_COMPATID_RANK}    // this value (and greater than
                                                   // DRIVER_UNTRUSTED_HARDWAREID_RANK)
                                                   // is an untrusted CompatibleID match

  DRIVER_W9X_SUSPECT_RANK            = $0000C000; // Any rank that is greater than
  {$EXTERNALSYM DRIVER_W9X_SUSPECT_RANK}          // or equal to this value, and lesser
                                                  // than or equal to 0xFFFF is suspected
                                                  // to be a Win9x-only driver, because
                                                  // (a) it isn't signed, and (b) there
                                                  // is no NT-specific decoration to
                                                  // explicitly indicate that the INF
                                                  // supports Windows NT/200x

  DRIVER_W9X_SUSPECT_HARDWAREID_RANK = $0000CFFF; // Any rank less than or equal to this
  {$EXTERNALSYM DRIVER_W9X_SUSPECT_HARDWAREID_RANK} // (and greater than or equal to
                                                  // DRIVER_W9X_SUSPECT_RANK) is a
                                                  // hardware ID match suspected of being
                                                  // only for Windows 9x platforms.

  DRIVER_W9X_SUSPECT_COMPATID_RANK   = $0000FFFF; // Any rank less than or equal to
  {$EXTERNALSYM DRIVER_W9X_SUSPECT_COMPATID_RANK} // this (and greater than
                                                  // DRIVER_W9X_SUSPECT_HARDWAREID_RANK)
                                                  // is a compatible ID match suspected
                                                  // of being only for Windows 9x
                                                  // platforms.

//
// Setup callback routine for comparing detection signatures
//
type
  TSPDetsigCmpProc = function(DeviceInfoSet: HDEVINFO; NewDeviceData,
    ExistingDeviceData: PSPDevInfoData; CompareContext: Pointer): DWORD; stdcall;

//
// Define context structure handed to co-installers
//
  PCoInstallerContextData = ^TCoInstallerContextData;
  COINSTALLER_CONTEXT_DATA = packed record
    PostProcessing: BOOL;
    InstallResult: DWORD;
    PrivateData: Pointer;
  end;
  {$EXTERNALSYM COINSTALLER_CONTEXT_DATA}
  TCoInstallerContextData = COINSTALLER_CONTEXT_DATA;

//
// Structure containing class image list information.
//
  PSPClassImageListData = ^TSPClassImageListData;
  SP_CLASSIMAGELIST_DATA = packed record
    cbSize: DWORD;
    ImageList: HIMAGELIST;
    Reserved: ULONG_PTR;
  end;
  {$EXTERNALSYM SP_CLASSIMAGELIST_DATA}
  TSPClassImageListData = SP_CLASSIMAGELIST_DATA;

//
// Structure to be passed as first parameter (LPVOID lpv) to ExtensionPropSheetPageProc
// entry point in setupapi.dll or to "EnumPropPages32" or "BasicProperties32" entry
// points provided by class/device property page providers.  Used to retrieve a handle
// (or, potentially, multiple handles) to property pages for a specified property page type.
//
  PSPPropSheetPageRequest = ^TSPPropSheetPageRequest;
  SP_PROPSHEETPAGE_REQUEST = packed record
    cbSize: DWORD;
    PageRequested: DWORD;
    DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData;
  end;
  {$EXTERNALSYM SP_PROPSHEETPAGE_REQUEST}
  TSPPropSheetPageRequest = SP_PROPSHEETPAGE_REQUEST;

//
// Property sheet codes used in SP_PROPSHEETPAGE_REQUEST.PageRequested
//
const
  SPPSR_SELECT_DEVICE_RESOURCES      = 1; // supplied by setupapi.dll
  {$EXTERNALSYM SPPSR_SELECT_DEVICE_RESOURCES}
  SPPSR_ENUM_BASIC_DEVICE_PROPERTIES = 2; // supplied by device's BasicProperties32 provider
  {$EXTERNALSYM SPPSR_ENUM_BASIC_DEVICE_PROPERTIES}
  SPPSR_ENUM_ADV_DEVICE_PROPERTIES   = 3; // supplied by class and/or device's EnumPropPages32 provider
  {$EXTERNALSYM SPPSR_ENUM_ADV_DEVICE_PROPERTIES}

//
// Structure used with SetupGetBackupInformation/SetupSetBackupInformation
//
type
  PSPBackupQueueParamsV2A = ^TSPBackupQueueParamsV2A;
  PSPBackupQueueParamsV2W = ^TSPBackupQueueParamsV2W;
  PSPBackupQueueParamsV2 = PSPBackupQueueParamsV2A;
  SP_BACKUP_QUEUE_PARAMS_V2_A = packed record
    cbSize: DWORD;
    FullInfPath: array [0..MAX_PATH - 1] of AnsiChar; // buffer to hold ANSI pathname of INF file
    FilenameOffset: Integer; // offset in CHAR's of filename part (after '\')
    ReinstallInstance: array [0..MAX_PATH - 1] of AnsiChar;  // Instance ID (if present)
  end;
  {$EXTERNALSYM SP_BACKUP_QUEUE_PARAMS_V2_A}
  SP_BACKUP_QUEUE_PARAMS_V2_W = packed record
    cbSize: DWORD;
    FullInfPath: array [0..MAX_PATH - 1] of WideChar;  // buffer to hold ANSI pathname of INF file
    FilenameOffset: Integer; // offset in CHAR's of filename part (after '\')
    ReinstallInstance: array [0..MAX_PATH - 1] of WideChar;  // Instance ID (if present)
  end;
  {$EXTERNALSYM SP_BACKUP_QUEUE_PARAMS_V2_W}
  TSPBackupQueueParamsV2A = SP_BACKUP_QUEUE_PARAMS_V2_A;
  TSPBackupQueueParamsV2W = SP_BACKUP_QUEUE_PARAMS_V2_W;
  TSPBackupQueueParamsV2 = TSPBackupQueueParamsV2A;

  PSPBackupQueueParamsV1A = ^TSPBackupQueueParamsV1A;
  PSPBackupQueueParamsV1W = ^TSPBackupQueueParamsV1W;
  PSPBackupQueueParamsV1 = PSPBackupQueueParamsV1A;
  SP_BACKUP_QUEUE_PARAMS_V1_A = packed record
    cbSize: DWORD;
    FullInfPath: array [0..MAX_PATH - 1] of AnsiChar; // buffer to hold ANSI pathname of INF file
    FilenameOffset: Integer; // offset in CHAR's of filename part (after '\')
  end;
  {$EXTERNALSYM SP_BACKUP_QUEUE_PARAMS_V1_A}
  SP_BACKUP_QUEUE_PARAMS_V1_W = packed record
    cbSize: DWORD;
    FullInfPath: array [0..MAX_PATH - 1] of WideChar; // buffer to hold ANSI pathname of INF file
    FilenameOffset: Integer; // offset in CHAR's of filename part (after '\')
  end;
  {$EXTERNALSYM SP_BACKUP_QUEUE_PARAMS_V1_W}
  TSPBackupQueueParamsV1A = SP_BACKUP_QUEUE_PARAMS_V1_A;
  TSPBackupQueueParamsV1W = SP_BACKUP_QUEUE_PARAMS_V1_W;
  TSPBackupQueueParamsV1 = TSPBackupQueueParamsV1A;

  {$IFDEF USE_SP_BACKUP_QUEUE_PARAMS_V1}
  TSPBackupQueueParamsA = TSPBackupQueueParamsV1A;
  TSPBackupQueueParamsW = TSPBackupQueueParamsV1W;
  TSPBackupQueueParams = TSPBackupQueueParamsV1;
  PSPBackupQueueParamsA = PSPBackupQueueParamsV1A;
  PSPBackupQueueParamsW = PSPBackupQueueParamsV1W;
  PSPBackupQueueParams = PSPBackupQueueParamsV1;
  {$ELSE}
  TSPBackupQueueParamsA = TSPBackupQueueParamsV2A;
  TSPBackupQueueParamsW = TSPBackupQueueParamsV2W;
  TSPBackupQueueParams = TSPBackupQueueParamsV2;
  PSPBackupQueueParamsA = PSPBackupQueueParamsV2A;
  PSPBackupQueueParamsW = PSPBackupQueueParamsV2W;
  PSPBackupQueueParams = PSPBackupQueueParamsV2;
  {$ENDIF USE_SP_BACKUP_QUEUE_PARAMS_V1}

//
// Setupapi-specific error codes
//
// Inf parse outcomes
//
const
  APPLICATION_ERROR_MASK = DWORD($20000000); // from WINNT.h
  {$EXTERNALSYM APPLICATION_ERROR_MASK}
  ERROR_SEVERITY_ERROR   = DWORD($C0000000); // from WINNT.h
  {$EXTERNALSYM ERROR_SEVERITY_ERROR}

  ERROR_EXPECTED_SECTION_NAME       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or 0);
  {$EXTERNALSYM ERROR_EXPECTED_SECTION_NAME}
  ERROR_BAD_SECTION_NAME_LINE       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or 1);
  {$EXTERNALSYM ERROR_BAD_SECTION_NAME_LINE}
  ERROR_SECTION_NAME_TOO_LONG       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or 2);
  {$EXTERNALSYM ERROR_SECTION_NAME_TOO_LONG}
  ERROR_GENERAL_SYNTAX              = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or 3);
  {$EXTERNALSYM ERROR_GENERAL_SYNTAX}

//
// Inf runtime errors
//
  ERROR_WRONG_INF_STYLE             = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $100);
  {$EXTERNALSYM ERROR_WRONG_INF_STYLE}
  ERROR_SECTION_NOT_FOUND           = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $101);
  {$EXTERNALSYM ERROR_SECTION_NOT_FOUND}
  ERROR_LINE_NOT_FOUND              = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $102);
  {$EXTERNALSYM ERROR_LINE_NOT_FOUND}
  ERROR_NO_BACKUP                   = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $103);
  {$EXTERNALSYM ERROR_NO_BACKUP}

//
// Device Installer/other errors
//
  ERROR_NO_ASSOCIATED_CLASS         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $200);
  {$EXTERNALSYM ERROR_NO_ASSOCIATED_CLASS}
  ERROR_CLASS_MISMATCH              = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $201);
  {$EXTERNALSYM ERROR_CLASS_MISMATCH}
  ERROR_DUPLICATE_FOUND             = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $202);
  {$EXTERNALSYM ERROR_DUPLICATE_FOUND}
  ERROR_NO_DRIVER_SELECTED          = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $203);
  {$EXTERNALSYM ERROR_NO_DRIVER_SELECTED}
  ERROR_KEY_DOES_NOT_EXIST          = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $204);
  {$EXTERNALSYM ERROR_KEY_DOES_NOT_EXIST}
  ERROR_INVALID_DEVINST_NAME        = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $205);
  {$EXTERNALSYM ERROR_INVALID_DEVINST_NAME}
  ERROR_INVALID_CLASS               = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $206);
  {$EXTERNALSYM ERROR_INVALID_CLASS}
  ERROR_DEVINST_ALREADY_EXISTS      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $207);
  {$EXTERNALSYM ERROR_DEVINST_ALREADY_EXISTS}
  ERROR_DEVINFO_NOT_REGISTERED      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $208);
  {$EXTERNALSYM ERROR_DEVINFO_NOT_REGISTERED}
  ERROR_INVALID_REG_PROPERTY        = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $209);
  {$EXTERNALSYM ERROR_INVALID_REG_PROPERTY}
  ERROR_NO_INF                      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20A);
  {$EXTERNALSYM ERROR_NO_INF}
  ERROR_NO_SUCH_DEVINST             = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20B);
  {$EXTERNALSYM ERROR_NO_SUCH_DEVINST}
  ERROR_CANT_LOAD_CLASS_ICON        = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20C);
  {$EXTERNALSYM ERROR_CANT_LOAD_CLASS_ICON}
  ERROR_INVALID_CLASS_INSTALLER     = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20D);
  {$EXTERNALSYM ERROR_INVALID_CLASS_INSTALLER}
  ERROR_DI_DO_DEFAULT               = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20E);
  {$EXTERNALSYM ERROR_DI_DO_DEFAULT}
  ERROR_DI_NOFILECOPY               = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $20F);
  {$EXTERNALSYM ERROR_DI_NOFILECOPY}
  ERROR_INVALID_HWPROFILE           = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $210);
  {$EXTERNALSYM ERROR_INVALID_HWPROFILE}
  ERROR_NO_DEVICE_SELECTED          = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $211);
  {$EXTERNALSYM ERROR_NO_DEVICE_SELECTED}
  ERROR_DEVINFO_LIST_LOCKED         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $212);
  {$EXTERNALSYM ERROR_DEVINFO_LIST_LOCKED}
  ERROR_DEVINFO_DATA_LOCKED         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $213);
  {$EXTERNALSYM ERROR_DEVINFO_DATA_LOCKED}
  ERROR_DI_BAD_PATH                 = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $214);
  {$EXTERNALSYM ERROR_DI_BAD_PATH}
  ERROR_NO_CLASSINSTALL_PARAMS      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $215);
  {$EXTERNALSYM ERROR_NO_CLASSINSTALL_PARAMS}
  ERROR_FILEQUEUE_LOCKED            = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $216);
  {$EXTERNALSYM ERROR_FILEQUEUE_LOCKED}
  ERROR_BAD_SERVICE_INSTALLSECT     = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $217);
  {$EXTERNALSYM ERROR_BAD_SERVICE_INSTALLSECT}
  ERROR_NO_CLASS_DRIVER_LIST        = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $218);
  {$EXTERNALSYM ERROR_NO_CLASS_DRIVER_LIST}
  ERROR_NO_ASSOCIATED_SERVICE       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $219);
  {$EXTERNALSYM ERROR_NO_ASSOCIATED_SERVICE}
  ERROR_NO_DEFAULT_DEVICE_INTERFACE = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21A);
  {$EXTERNALSYM ERROR_NO_DEFAULT_DEVICE_INTERFACE}
  ERROR_DEVICE_INTERFACE_ACTIVE     = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21B);
  {$EXTERNALSYM ERROR_DEVICE_INTERFACE_ACTIVE}
  ERROR_DEVICE_INTERFACE_REMOVED    = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21C);
  {$EXTERNALSYM ERROR_DEVICE_INTERFACE_REMOVED}
  ERROR_BAD_INTERFACE_INSTALLSECT   = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21D);
  {$EXTERNALSYM ERROR_BAD_INTERFACE_INSTALLSECT}
  ERROR_NO_SUCH_INTERFACE_CLASS     = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21E);
  {$EXTERNALSYM ERROR_NO_SUCH_INTERFACE_CLASS}
  ERROR_INVALID_REFERENCE_STRING    = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $21F);
  {$EXTERNALSYM ERROR_INVALID_REFERENCE_STRING}
  ERROR_INVALID_MACHINENAME         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $220);
  {$EXTERNALSYM ERROR_INVALID_MACHINENAME}
  ERROR_REMOTE_COMM_FAILURE         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $221);
  {$EXTERNALSYM ERROR_REMOTE_COMM_FAILURE}
  ERROR_MACHINE_UNAVAILABLE         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $222);
  {$EXTERNALSYM ERROR_MACHINE_UNAVAILABLE}
  ERROR_NO_CONFIGMGR_SERVICES       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $223);
  {$EXTERNALSYM ERROR_NO_CONFIGMGR_SERVICES}
  ERROR_INVALID_PROPPAGE_PROVIDER   = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $224);
  {$EXTERNALSYM ERROR_INVALID_PROPPAGE_PROVIDER}
  ERROR_NO_SUCH_DEVICE_INTERFACE    = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $225);
  {$EXTERNALSYM ERROR_NO_SUCH_DEVICE_INTERFACE}
  ERROR_DI_POSTPROCESSING_REQUIRED  = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $226);
  {$EXTERNALSYM ERROR_DI_POSTPROCESSING_REQUIRED}
  ERROR_INVALID_COINSTALLER         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $227);
  {$EXTERNALSYM ERROR_INVALID_COINSTALLER}
  ERROR_NO_COMPAT_DRIVERS           = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $228);
  {$EXTERNALSYM ERROR_NO_COMPAT_DRIVERS}
  ERROR_NO_DEVICE_ICON              = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $229);
  {$EXTERNALSYM ERROR_NO_DEVICE_ICON}
  ERROR_INVALID_INF_LOGCONFIG       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22A);
  {$EXTERNALSYM ERROR_INVALID_INF_LOGCONFIG}
  ERROR_DI_DONT_INSTALL             = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22B);
  {$EXTERNALSYM ERROR_DI_DONT_INSTALL}
  ERROR_INVALID_FILTER_DRIVER       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22C);
  {$EXTERNALSYM ERROR_INVALID_FILTER_DRIVER}
  ERROR_NON_WINDOWS_NT_DRIVER       = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22D);
  {$EXTERNALSYM ERROR_NON_WINDOWS_NT_DRIVER}
  ERROR_NON_WINDOWS_DRIVER          = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22E);
  {$EXTERNALSYM ERROR_NON_WINDOWS_DRIVER}
  ERROR_NO_CATALOG_FOR_OEM_INF      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $22F);
  {$EXTERNALSYM ERROR_NO_CATALOG_FOR_OEM_INF}
  ERROR_DEVINSTALL_QUEUE_NONNATIVE  = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $230);
  {$EXTERNALSYM ERROR_DEVINSTALL_QUEUE_NONNATIVE}
  ERROR_NOT_DISABLEABLE             = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $231);
  {$EXTERNALSYM ERROR_NOT_DISABLEABLE}
  ERROR_CANT_REMOVE_DEVINST         = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $232);
  {$EXTERNALSYM ERROR_CANT_REMOVE_DEVINST}
  ERROR_INVALID_TARGET              = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $233);
  {$EXTERNALSYM ERROR_INVALID_TARGET}
  ERROR_DRIVER_NONNATIVE            = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $234);
  {$EXTERNALSYM ERROR_DRIVER_NONNATIVE}
  ERROR_IN_WOW64                    = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $235);
  {$EXTERNALSYM ERROR_IN_WOW64}
  ERROR_SET_SYSTEM_RESTORE_POINT    = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $236);
  {$EXTERNALSYM ERROR_SET_SYSTEM_RESTORE_POINT}
  ERROR_INCORRECTLY_COPIED_INF      = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $237);
  {$EXTERNALSYM ERROR_INCORRECTLY_COPIED_INF}
  ERROR_SCE_DISABLED                = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $238);
  {$EXTERNALSYM ERROR_SCE_DISABLED}

//
// Backward compatibility--do not use.
//
  ERROR_NO_DEFAULT_INTERFACE_DEVICE = ERROR_NO_DEFAULT_DEVICE_INTERFACE;
  {$EXTERNALSYM ERROR_NO_DEFAULT_INTERFACE_DEVICE}
  ERROR_INTERFACE_DEVICE_ACTIVE     = ERROR_DEVICE_INTERFACE_ACTIVE;
  {$EXTERNALSYM ERROR_INTERFACE_DEVICE_ACTIVE}
  ERROR_INTERFACE_DEVICE_REMOVED    = ERROR_DEVICE_INTERFACE_REMOVED;
  {$EXTERNALSYM ERROR_INTERFACE_DEVICE_REMOVED}
  ERROR_NO_SUCH_INTERFACE_DEVICE    = ERROR_NO_SUCH_DEVICE_INTERFACE;
  {$EXTERNALSYM ERROR_NO_SUCH_INTERFACE_DEVICE}

//
// Win9x migration DLL error code
//
  ERROR_NOT_INSTALLED = DWORD(APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR or $1000);
  {$EXTERNALSYM ERROR_NOT_INSTALLED}

// (rom) moved may consts here to allow for dynamic linking

//
// SearchControl flags for SetupGetInfInformation
//
const
  INFINFO_INF_SPEC_IS_HINF       = 1;
  {$EXTERNALSYM INFINFO_INF_SPEC_IS_HINF}
  INFINFO_INF_NAME_IS_ABSOLUTE   = 2;
  {$EXTERNALSYM INFINFO_INF_NAME_IS_ABSOLUTE}
  INFINFO_DEFAULT_SEARCH         = 3;
  {$EXTERNALSYM INFINFO_DEFAULT_SEARCH}
  INFINFO_REVERSE_DEFAULT_SEARCH = 4;
  {$EXTERNALSYM INFINFO_REVERSE_DEFAULT_SEARCH}
  INFINFO_INF_PATH_LIST_SEARCH   = 5;
  {$EXTERNALSYM INFINFO_INF_PATH_LIST_SEARCH}

//
// Compression types
//
const
  FILE_COMPRESSION_NONE   = 0;
  {$EXTERNALSYM FILE_COMPRESSION_NONE}
  FILE_COMPRESSION_WINLZA = 1;
  {$EXTERNALSYM FILE_COMPRESSION_WINLZA}
  FILE_COMPRESSION_MSZIP  = 2;
  {$EXTERNALSYM FILE_COMPRESSION_MSZIP}
  FILE_COMPRESSION_NTCAB  = 3;
  {$EXTERNALSYM FILE_COMPRESSION_NTCAB}

//
// Define flags for SourceList APIs.
//
const
  SRCLIST_TEMPORARY       = $00000001;
  {$EXTERNALSYM SRCLIST_TEMPORARY}
  SRCLIST_NOBROWSE        = $00000002;
  {$EXTERNALSYM SRCLIST_NOBROWSE}
  SRCLIST_SYSTEM          = $00000010;
  {$EXTERNALSYM SRCLIST_SYSTEM}
  SRCLIST_USER            = $00000020;
  {$EXTERNALSYM SRCLIST_USER}
  SRCLIST_SYSIFADMIN      = $00000040;
  {$EXTERNALSYM SRCLIST_SYSIFADMIN}
  SRCLIST_SUBDIRS         = $00000100;
  {$EXTERNALSYM SRCLIST_SUBDIRS}
  SRCLIST_APPEND          = $00000200;
  {$EXTERNALSYM SRCLIST_APPEND}
  SRCLIST_NOSTRIPPLATFORM = $00000400;
  {$EXTERNALSYM SRCLIST_NOSTRIPPLATFORM}

//
// Styles for SetupPromptForDisk, SetupCopyError,
// SetupRenameError, SetupDeleteError
//
const
  IDF_NOBROWSE               = $00000001;
  {$EXTERNALSYM IDF_NOBROWSE}
  IDF_NOSKIP                 = $00000002;
  {$EXTERNALSYM IDF_NOSKIP}
  IDF_NODETAILS              = $00000004;
  {$EXTERNALSYM IDF_NODETAILS}
  IDF_NOCOMPRESSED           = $00000008;
  {$EXTERNALSYM IDF_NOCOMPRESSED}
  IDF_CHECKFIRST             = $00000100;
  {$EXTERNALSYM IDF_CHECKFIRST}
  IDF_NOBEEP                 = $00000200;
  {$EXTERNALSYM IDF_NOBEEP}
  IDF_NOFOREGROUND           = $00000400;
  {$EXTERNALSYM IDF_NOFOREGROUND}
  IDF_WARNIFSKIP             = $00000800;
  {$EXTERNALSYM IDF_WARNIFSKIP}
  IDF_NOREMOVABLEMEDIAPROMPT = $00001000;
  {$EXTERNALSYM IDF_NOREMOVABLEMEDIAPROMPT}
  IDF_USEDISKNAMEASPROMPT    = $00002000;
  {$EXTERNALSYM IDF_USEDISKNAMEASPROMPT}
  IDF_OEMDISK                = DWORD($80000000);
  {$EXTERNALSYM IDF_OEMDISK}

//
// Return values for SetupPromptForDisk, SetupCopyError,
// SetupRenameError, SetupDeleteError, SetupBackupError
//
const
  DPROMPT_SUCCESS        = 0;
  {$EXTERNALSYM DPROMPT_SUCCESS}
  DPROMPT_CANCEL         = 1;
  {$EXTERNALSYM DPROMPT_CANCEL}
  DPROMPT_SKIPFILE       = 2;
  {$EXTERNALSYM DPROMPT_SKIPFILE}
  DPROMPT_BUFFERTOOSMALL = 3;
  {$EXTERNALSYM DPROMPT_BUFFERTOOSMALL}
  DPROMPT_OUTOFMEMORY    = 4;
  {$EXTERNALSYM DPROMPT_OUTOFMEMORY}

//
// Flags for SetupSetDirectoryIdEx
//
const
  SETDIRID_NOT_FULL_PATH = $00000001;
  {$EXTERNALSYM SETDIRID_NOT_FULL_PATH}

//
// InfoDesired values for SetupGetSourceInfo
//
const
  SRCINFO_PATH        = 1;
  {$EXTERNALSYM SRCINFO_PATH}
  SRCINFO_TAGFILE     = 2;
  {$EXTERNALSYM SRCINFO_TAGFILE}
  SRCINFO_DESCRIPTION = 3;
  {$EXTERNALSYM SRCINFO_DESCRIPTION}
  SRCINFO_FLAGS       = 4;
  {$EXTERNALSYM SRCINFO_FLAGS}
  // SRC_FLAGS allow special treatment of source
  // lower 4 bits are reserved for OS use
  // the flags may determine what other parameters exist
  //
  SRCINFO_TAGFILE2    = 5;  // alternate tagfile, when SRCINFO_TAGFILE is a cabfile
  {$EXTERNALSYM SRCINFO_TAGFILE2}
  SRC_FLAGS_CABFILE   = $0010; // if set, treat SRCINFO_TAGFILE as a cabfile and specify alternate tagfile
  {$EXTERNALSYM SRC_FLAGS_CABFILE}

//
// CopyStyle values for copy and queue-related APIs
//
const
  SP_COPY_DELETESOURCE        = $0000001; // delete source file on successful copy
  {$EXTERNALSYM SP_COPY_DELETESOURCE}
  SP_COPY_REPLACEONLY         = $0000002; // copy only if target file already present
  {$EXTERNALSYM SP_COPY_REPLACEONLY}
  SP_COPY_NEWER               = $0000004; // copy only if source newer than or same as target
  {$EXTERNALSYM SP_COPY_NEWER}
  SP_COPY_NEWER_OR_SAME       = SP_COPY_NEWER;
  {$EXTERNALSYM SP_COPY_NEWER_OR_SAME}
  SP_COPY_NOOVERWRITE         = $0000008; // copy only if target doesn't exist
  {$EXTERNALSYM SP_COPY_NOOVERWRITE}
  SP_COPY_NODECOMP            = $0000010; // don't decompress source file while copying
  {$EXTERNALSYM SP_COPY_NODECOMP}
  SP_COPY_LANGUAGEAWARE       = $0000020; // don't overwrite file of different language
  {$EXTERNALSYM SP_COPY_LANGUAGEAWARE}
  SP_COPY_SOURCE_ABSOLUTE     = $0000040; // SourceFile is a full source path
  {$EXTERNALSYM SP_COPY_SOURCE_ABSOLUTE}
  SP_COPY_SOURCEPATH_ABSOLUTE = $0000080; // SourcePathRoot is the full path
  {$EXTERNALSYM SP_COPY_SOURCEPATH_ABSOLUTE}
  SP_COPY_IN_USE_NEEDS_REBOOT = $0000100; // System needs reboot if file in use
  {$EXTERNALSYM SP_COPY_IN_USE_NEEDS_REBOOT}
  SP_COPY_FORCE_IN_USE        = $0000200; // Force target-in-use behavior
  {$EXTERNALSYM SP_COPY_FORCE_IN_USE}
  SP_COPY_NOSKIP              = $0000400; // Skip is disallowed for this file or section
  {$EXTERNALSYM SP_COPY_NOSKIP}
  SP_FLAG_CABINETCONTINUATION = $0000800; // Used with need media notification
  {$EXTERNALSYM SP_FLAG_CABINETCONTINUATION}
  SP_COPY_FORCE_NOOVERWRITE   = $0001000; // like NOOVERWRITE but no callback nofitication
  {$EXTERNALSYM SP_COPY_FORCE_NOOVERWRITE}
  SP_COPY_FORCE_NEWER         = $0002000; // like NEWER but no callback nofitication
  {$EXTERNALSYM SP_COPY_FORCE_NEWER}
  SP_COPY_WARNIFSKIP          = $0004000; // system critical file: warn if user tries to skip
  {$EXTERNALSYM SP_COPY_WARNIFSKIP}
  SP_COPY_NOBROWSE            = $0008000; // Browsing is disallowed for this file or section
  {$EXTERNALSYM SP_COPY_NOBROWSE}
  SP_COPY_NEWER_ONLY          = $0010000; // copy only if source file newer than target
  {$EXTERNALSYM SP_COPY_NEWER_ONLY}
  SP_COPY_SOURCE_SIS_MASTER   = $0020000; // source is single-instance store master
  {$EXTERNALSYM SP_COPY_SOURCE_SIS_MASTER}
  SP_COPY_OEMINF_CATALOG_ONLY = $0040000; // (SetupCopyOEMInf only) don't copy INF--just catalog
  {$EXTERNALSYM SP_COPY_OEMINF_CATALOG_ONLY}
  SP_COPY_REPLACE_BOOT_FILE   = $0080000; // file must be present upon reboot (i.e., it's
  {$EXTERNALSYM SP_COPY_REPLACE_BOOT_FILE}// needed by the loader); this flag implies a reboot
  SP_COPY_NOPRUNE             = $0100000; // never prune this file
  {$EXTERNALSYM SP_COPY_NOPRUNE}
  SP_COPY_OEM_F6_INF           = $0200000;   // Used when calling SetupCopyOemInf
  {$EXTERNALSYM SP_COPY_OEM_F6_INF}
  //
  // Flags passed to Backup notification
  //
  SP_BACKUP_BACKUPPASS          = $00000001;  // file backed up during backup pass
  {$EXTERNALSYM SP_BACKUP_BACKUPPASS}
  SP_BACKUP_DEMANDPASS          = $00000002;  // file backed up on demand
  {$EXTERNALSYM SP_BACKUP_DEMANDPASS}
  SP_BACKUP_SPECIAL             = $00000004;  // if set, special type of backup
  {$EXTERNALSYM SP_BACKUP_SPECIAL}
  SP_BACKUP_BOOTFILE            = $00000008;  // file marked with COPYFLG_REPLACE_BOOT_FILE
  {$EXTERNALSYM SP_BACKUP_BOOTFILE}

//
// Define flags for SetupScanFileQueue.
//
const
  SPQ_SCAN_FILE_PRESENCE           = $00000001;
  {$EXTERNALSYM SPQ_SCAN_FILE_PRESENCE}
  SPQ_SCAN_FILE_VALIDITY           = $00000002;
  {$EXTERNALSYM SPQ_SCAN_FILE_VALIDITY}
  SPQ_SCAN_USE_CALLBACK            = $00000004;
  {$EXTERNALSYM SPQ_SCAN_USE_CALLBACK}
  SPQ_SCAN_USE_CALLBACKEX          = $00000008;
  {$EXTERNALSYM SPQ_SCAN_USE_CALLBACKEX}
  SPQ_SCAN_INFORM_USER             = $00000010;
  {$EXTERNALSYM SPQ_SCAN_INFORM_USER}
  SPQ_SCAN_PRUNE_COPY_QUEUE        = $00000020;
  {$EXTERNALSYM SPQ_SCAN_PRUNE_COPY_QUEUE}
  SPQ_SCAN_USE_CALLBACK_SIGNERINFO = $00000040;
  {$EXTERNALSYM SPQ_SCAN_USE_CALLBACK_SIGNERINFO}
  SPQ_SCAN_PRUNE_DELREN            = $00000080; // remote Delete/Rename queue
  {$EXTERNALSYM SPQ_SCAN_PRUNE_DELREN}

//
// Define flags used with Param2 for SPFILENOTIFY_QUEUESCAN
//
  SPQ_DELAYED_COPY                 = $00000001; // file was in use; registered for delayed copy
  {$EXTERNALSYM SPQ_DELAYED_COPY}

//
// Flags/FlagMask for use with SetupSetFileQueueFlags and returned by SetupGetFileQueueFlags
//
const
  SPQ_FLAG_BACKUP_AWARE      = $00000001;  // If set, SetupCommitFileQueue will
  {$EXTERNALSYM SPQ_FLAG_BACKUP_AWARE}     // issue backup notifications.

  SPQ_FLAG_ABORT_IF_UNSIGNED = $00000002;  // If set, SetupCommitFileQueue will
  {$EXTERNALSYM SPQ_FLAG_ABORT_IF_UNSIGNED}// fail with ERROR_SET_SYSTEM_RESTORE_POINT
                                           // if the user elects to proceed with an
                                           // unsigned queue committal.  This allows
                                           // the caller to set a system restore point,
                                           // then re-commit the file queue.

  SPQ_FLAG_FILES_MODIFIED    = $00000004;  // If set, at least one file was
  {$EXTERNALSYM SPQ_FLAG_FILES_MODIFIED}   // replaced by a different version

  SPQ_FLAG_VALID             = $00000007;  // mask of valid flags (can be passed as FlagMask)
  {$EXTERNALSYM SPQ_FLAG_VALID}

//
// Define OEM Source Type values for use in SetupCopyOEMInf.
//
  SPOST_NONE = 0;
  {$EXTERNALSYM SPOST_NONE}
  SPOST_PATH = 1;
  {$EXTERNALSYM SPOST_PATH}
  SPOST_URL  = 2;
  {$EXTERNALSYM SPOST_URL}
  SPOST_MAX  = 3;
  {$EXTERNALSYM SPOST_MAX}

//
// Flags used by SetupUninstallOEMInf
//
  SUOI_FORCEDELETE = $00000001;
//
// Flags for SetupCreateDiskSpaceList
//
const
  SPDSL_IGNORE_DISK              = $00000001; // ignore deletes and on-disk files in copies
  {$EXTERNALSYM SPDSL_IGNORE_DISK}
  SPDSL_DISALLOW_NEGATIVE_ADJUST = $00000002;
  {$EXTERNALSYM SPDSL_DISALLOW_NEGATIVE_ADJUST}

//
// Define flags that are returned by SetupPromptReboot
//
const
  SPFILEQ_FILE_IN_USE        = $00000001;
  {$EXTERNALSYM SPFILEQ_FILE_IN_USE}
  SPFILEQ_REBOOT_RECOMMENDED = $00000002;
  {$EXTERNALSYM SPFILEQ_REBOOT_RECOMMENDED}
  SPFILEQ_REBOOT_IN_PROGRESS = $00000004;
  {$EXTERNALSYM SPFILEQ_REBOOT_IN_PROGRESS}

//
// Flags for AddReg section lines in INF.  The corresponding value
// is <ValueType> in the AddReg line format given below:
//
// <RegRootString>,<SubKey>,<ValueName>,<ValueType>,<Value>...
//
// The low word contains basic flags concerning the general data type
// and AddReg action. The high word contains values that more specifically
// identify the data type of the registry value.  The high word is ignored
// by the 16-bit Windows 95 SETUPX APIs.
//
// If <ValueType> has FLG_ADDREG_DELREG_BIT set, it will be ignored by AddReg
// (not supported by SetupX).
//
const
  FLG_ADDREG_DELREG_BIT     = $00008000;   // if set, interpret as DELREG, see below
  {$EXTERNALSYM FLG_ADDREG_DELREG_BIT}
  FLG_ADDREG_BINVALUETYPE   = $00000001;
  {$EXTERNALSYM FLG_ADDREG_BINVALUETYPE}
  FLG_ADDREG_NOCLOBBER      = $00000002;
  {$EXTERNALSYM FLG_ADDREG_NOCLOBBER}
  FLG_ADDREG_DELVAL         = $00000004;
  {$EXTERNALSYM FLG_ADDREG_DELVAL}
  FLG_ADDREG_APPEND         = $00000008;   // Currently supported only
  {$EXTERNALSYM FLG_ADDREG_APPEND}         // for REG_MULTI_SZ values.
  FLG_ADDREG_KEYONLY        = $00000010;   // Just create the key, ignore value
  {$EXTERNALSYM FLG_ADDREG_KEYONLY}
  FLG_ADDREG_OVERWRITEONLY  = $00000020;   // Set only if value already exists
  {$EXTERNALSYM FLG_ADDREG_OVERWRITEONLY}
  FLG_ADDREG_64BITKEY       = $00001000;   // make this change in the 64 bit registry.
  {$EXTERNALSYM FLG_ADDREG_64BITKEY}
  FLG_ADDREG_KEYONLY_COMMON = $00002000;   // same as FLG_ADDREG_KEYONLY but also works for DELREG
  {$EXTERNALSYM FLG_ADDREG_KEYONLY_COMMON}
  FLG_ADDREG_32BITKEY       = $00004000;   // make this change in the 32 bit registry.
  {$EXTERNALSYM FLG_ADDREG_OVERWRITEONLY}
  //
  // The INF may supply any arbitrary data type ordinal in the highword except
  // for the following: REG_NONE, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ.  If this
  // technique is used, then the data is given in binary format, one byte per
  // field.
  //
  FLG_ADDREG_TYPE_MASK      = DWORD($FFFF0000 or FLG_ADDREG_BINVALUETYPE);
  {$EXTERNALSYM FLG_ADDREG_TYPE_MASK}
  FLG_ADDREG_TYPE_SZ        = $00000000;
  {$EXTERNALSYM FLG_ADDREG_TYPE_SZ}
  FLG_ADDREG_TYPE_MULTI_SZ  = $00010000;
  {$EXTERNALSYM FLG_ADDREG_TYPE_MULTI_SZ}
  FLG_ADDREG_TYPE_EXPAND_SZ = $00020000;
  {$EXTERNALSYM FLG_ADDREG_TYPE_EXPAND_SZ}
  FLG_ADDREG_TYPE_BINARY    = $00000000 or FLG_ADDREG_BINVALUETYPE;
  {$EXTERNALSYM FLG_ADDREG_TYPE_BINARY}
  FLG_ADDREG_TYPE_DWORD     = $00010000 or FLG_ADDREG_BINVALUETYPE;
  {$EXTERNALSYM FLG_ADDREG_TYPE_DWORD}
  FLG_ADDREG_TYPE_NONE      = $00020000 or FLG_ADDREG_BINVALUETYPE;
  {$EXTERNALSYM FLG_ADDREG_TYPE_NONE}

  //
  // Flags for DelReg section lines in INF.  The corresponding value
  // is <Operation> in the extended DelReg line format given below:
  //
  // <RegRootString>,<SubKey>,<ValueName>,<Operation>[,...]
  //
  // In SetupX and some versions of SetupAPI, <Operation> will be ignored and <ValueName> will
  // be deleted. Use with care.
  //
  // The bits determined by mask FLG_DELREG_TYPE_MASK indicates type of data expected.
  // <Operation> must have FLG_ADDREG_DELREG_BIT set, otherwise it is ignored and specified
  // value will be deleted (allowing an AddReg section to also be used as a DelReg section)
  // if <Operation> is not specified, <ValueName> will be deleted (if specified) otherwise
  // <SubKey> will be deleted.
  //
  // the compatability flag
  //
  FLG_DELREG_VALUE            = $00000000;
  {$EXTERNALSYM FLG_DELREG_VALUE}

  FLG_DELREG_TYPE_MASK        = FLG_ADDREG_TYPE_MASK;        // 0xFFFF0001
  {$EXTERNALSYM FLG_DELREG_TYPE_MASK}
  FLG_DELREG_TYPE_SZ          = FLG_ADDREG_TYPE_SZ;          // 0x00000000
  {$EXTERNALSYM FLG_DELREG_TYPE_SZ}
  FLG_DELREG_TYPE_MULTI_SZ    = FLG_ADDREG_TYPE_MULTI_SZ;    // 0x00010000
  {$EXTERNALSYM FLG_DELREG_TYPE_MULTI_SZ}
  FLG_DELREG_TYPE_EXPAND_SZ   = FLG_ADDREG_TYPE_EXPAND_SZ;   // 0x00020000
  {$EXTERNALSYM FLG_DELREG_TYPE_EXPAND_SZ}
  FLG_DELREG_TYPE_BINARY      = FLG_ADDREG_TYPE_BINARY;      // 0x00000001
  {$EXTERNALSYM FLG_DELREG_TYPE_BINARY}
  FLG_DELREG_TYPE_DWORD       = FLG_ADDREG_TYPE_DWORD;       // 0x00010001
  {$EXTERNALSYM FLG_DELREG_TYPE_DWORD}
  FLG_DELREG_TYPE_NONE        = FLG_ADDREG_TYPE_NONE;        // 0x00020001
  {$EXTERNALSYM FLG_DELREG_TYPE_NONE}
  FLG_DELREG_64BITKEY         = FLG_ADDREG_64BITKEY;         // 0x00001000
  {$EXTERNALSYM FLG_DELREG_64BITKEY}
  FLG_DELREG_KEYONLY_COMMON   = FLG_ADDREG_KEYONLY_COMMON;   // 0x00002000
  {$EXTERNALSYM FLG_DELREG_KEYONLY_COMMON}
  FLG_DELREG_32BITKEY         = FLG_ADDREG_32BITKEY;         // 0x00004000
  {$EXTERNALSYM FLG_DELREG_32BITKEY}

  //
  // <Operation> = FLG_DELREG_MULTI_SZ_DELSTRING
  //               <RegRootString>,<SubKey>,<ValueName>,0x00018002,<String>
  //               removes all entries matching <String> (case ignored) from multi-sz registry value
  //
  FLG_DELREG_OPERATION_MASK = $000000FE;
  {$EXTERNALSYM FLG_DELREG_OPERATION_MASK}
  FLG_DELREG_MULTI_SZ_DELSTRING = FLG_DELREG_TYPE_MULTI_SZ or FLG_ADDREG_DELREG_BIT or $00000002;  // 0x00018002
  {$EXTERNALSYM FLG_DELREG_MULTI_SZ_DELSTRING}

//
// Flags for BitReg section lines in INF.
//
  FLG_BITREG_CLEARBITS = $00000000;
  {$EXTERNALSYM FLG_BITREG_CLEARBITS}
  FLG_BITREG_SETBITS   = $00000001;
  {$EXTERNALSYM FLG_BITREG_SETBITS}
  FLG_BITREG_64BITKEY  = $00001000;
  {$EXTERNALSYM FLG_BITREG_64BITKEY}
  FLG_BITREG_32BITKEY  = $00004000;
  {$EXTERNALSYM FLG_BITREG_32BITKEY}

//
// Flags for Ini2Reg section lines in INF.
//
  FLG_INI2REG_64BITKEY = $00001000;
  {$EXTERNALSYM FLG_INI2REG_64BITKEY}
  FLG_INI2REG_32BITKEY = $00004000;
  {$EXTERNALSYM FLG_INI2REG_32BITKEY}

//
// Flags for RegSvr section lines in INF
//
  FLG_REGSVR_DLLREGISTER = $00000001;
  {$EXTERNALSYM FLG_REGSVR_DLLREGISTER}
  FLG_REGSVR_DLLINSTALL  = $00000002;
  {$EXTERNALSYM FLG_REGSVR_DLLINSTALL}

// Flags for RegSvr section lines in INF
//
  FLG_PROFITEM_CURRENTUSER = $00000001;
  {$EXTERNALSYM FLG_PROFITEM_CURRENTUSER}
  FLG_PROFITEM_DELETE      = $00000002;
  {$EXTERNALSYM FLG_PROFITEM_DELETE}
  FLG_PROFITEM_GROUP       = $00000004;
  {$EXTERNALSYM FLG_PROFITEM_GROUP}
  FLG_PROFITEM_CSIDL       = $00000008;
  {$EXTERNALSYM FLG_PROFITEM_CSIDL}

//
// Flags for SetupInstallFromInfSection
//
const
  SPINST_LOGCONFIG                = $00000001;
  {$EXTERNALSYM SPINST_LOGCONFIG}
  SPINST_INIFILES                 = $00000002;
  {$EXTERNALSYM SPINST_INIFILES}
  SPINST_REGISTRY                 = $00000004;
  {$EXTERNALSYM SPINST_REGISTRY}
  SPINST_INI2REG                  = $00000008;
  {$EXTERNALSYM SPINST_INI2REG}
  SPINST_FILES                    = $00000010;
  {$EXTERNALSYM SPINST_FILES}
  SPINST_BITREG                   = $00000020;
  {$EXTERNALSYM SPINST_BITREG}
  SPINST_REGSVR                   = $00000040;
  {$EXTERNALSYM SPINST_REGSVR}
  SPINST_UNREGSVR                 = $00000080;
  {$EXTERNALSYM SPINST_UNREGSVR}
  SPINST_PROFILEITEMS             = $00000100;
  {$EXTERNALSYM SPINST_PROFILEITEMS}
  {$IFDEF WINXP}
  SPINST_COPYINF                  = $00000200;
  {$EXTERNALSYM SPINST_COPYINF}
  SPINST_ALL                      = $000003ff;
  {$EXTERNALSYM SPINST_ALL}
  {$ELSE}
  SPINST_ALL                      = $000001ff;
  {$EXTERNALSYM SPINST_ALL}
  {$ENDIF WINXP}
  SPINST_SINGLESECTION            = $00010000;
  {$EXTERNALSYM SPINST_SINGLESECTION}
  SPINST_LOGCONFIG_IS_FORCED      = $00020000;
  {$EXTERNALSYM SPINST_LOGCONFIG_IS_FORCED}
  SPINST_LOGCONFIGS_ARE_OVERRIDES = $00040000;
  {$EXTERNALSYM SPINST_LOGCONFIGS_ARE_OVERRIDES}
  SPINST_REGISTERCALLBACKAWARE    = $00080000;
  {$EXTERNALSYM SPINST_REGISTERCALLBACKAWARE}

//
// Flags for SetupInstallServicesFromInfSection(Ex).  These flags are also used
// in the flags field of AddService or DelService lines in a device INF.  Some
// of these flags are not permitted in the non-Ex API.  These flags are marked
// as such below.
//

//
// (AddService) move service's tag to front of its group order list
//
const
  SPSVCINST_TAGTOFRONT = $00000001;
  {$EXTERNALSYM SPSVCINST_TAGTOFRONT}

//
// (AddService) **Ex API only** mark this service as the function driver for the
// device being installed
//
  SPSVCINST_ASSOCSERVICE = $00000002;
  {$EXTERNALSYM SPSVCINST_ASSOCSERVICE}

//
// (DelService) delete the associated event log entry for a service specified in
// a DelService entry
//
  SPSVCINST_DELETEEVENTLOGENTRY = $00000004;
  {$EXTERNALSYM SPSVCINST_DELETEEVENTLOGENTRY}

//
// (AddService) don't overwrite display name if it already exists
//
  SPSVCINST_NOCLOBBER_DISPLAYNAME = $00000008;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_DISPLAYNAME}

//
// (AddService) don't overwrite start type value if service already exists
//
  SPSVCINST_NOCLOBBER_STARTTYPE = $00000010;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_STARTTYPE}

//
// (AddService) don't overwrite error control value if service already exists
//
  SPSVCINST_NOCLOBBER_ERRORCONTROL = $00000020;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_ERRORCONTROL}

//
// (AddService) don't overwrite load order group if it already exists
//
  SPSVCINST_NOCLOBBER_LOADORDERGROUP = $00000040;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_LOADORDERGROUP}

//
// (AddService) don't overwrite dependencies list if it already exists
//
  SPSVCINST_NOCLOBBER_DEPENDENCIES = $00000080;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_DEPENDENCIES}

//
// (AddService) don't overwrite description if it already exists
//
  SPSVCINST_NOCLOBBER_DESCRIPTION = $00000100;
  {$EXTERNALSYM SPSVCINST_NOCLOBBER_DESCRIPTION}

//
// (DelService) stop the associated service specified in
// a DelService entry before deleting the service
//
  SPSVCINST_STOPSERVICE = $00000200;
  {$EXTERNALSYM SPSVCINST_STOPSERVICE}

//
// (AddService) force overwrite of security settings
//
  SPSVCINST_CLOBBER_SECURITY = $00000400;
  {$EXTERNALSYM SPSVCINST_CLOBBER_SECURITY}

//
// Flags for SetupInitializeFileLog
//
const
  SPFILELOG_SYSTEMLOG = $00000001; // use system log -- must be Administrator
  {$EXTERNALSYM SPFILELOG_SYSTEMLOG}
  SPFILELOG_FORCENEW  = $00000002; // not valid with SPFILELOG_SYSTEMLOG
  {$EXTERNALSYM SPFILELOG_FORCENEW}
  SPFILELOG_QUERYONLY = $00000004; // allows non-administrators to read system log
  {$EXTERNALSYM SPFILELOG_QUERYONLY}

//
// Flags for SetupLogFile
//
const
  SPFILELOG_OEMFILE = $00000001;
  {$EXTERNALSYM SPFILELOG_OEMFILE}

//
// Items retrievable from SetupQueryFileLog()
//
const
  SetupFileLogSourceFilename  = $00000000;
  {$EXTERNALSYM SetupFileLogSourceFilename}
  SetupFileLogChecksum        = $00000001;
  {$EXTERNALSYM SetupFileLogChecksum}
  SetupFileLogDiskTagfile     = $00000002;
  {$EXTERNALSYM SetupFileLogDiskTagfile}
  SetupFileLogDiskDescription = $00000003;
  {$EXTERNALSYM SetupFileLogDiskDescription}
  SetupFileLogOtherInfo       = $00000004;
  {$EXTERNALSYM SetupFileLogOtherInfo}
  SetupFileLogMax             = $00000005;
  {$EXTERNALSYM SetupFileLogMax}
type
  SetupFileLogInfo = DWORD;
  {$EXTERNALSYM SetupFileLogInfo}

const
  LogSevInformation = $00000000;
  {$EXTERNALSYM LogSevInformation}
  LogSevWarning     = $00000001;
  {$EXTERNALSYM LogSevWarning}
  LogSevError       = $00000002;
  {$EXTERNALSYM LogSevError}
  LogSevFatalError  = $00000003;
  {$EXTERNALSYM LogSevFatalError}
  LogSevMaximum     = $00000004;
  {$EXTERNALSYM LogSevMaximum}
type
  LogSeverity = DWORD;
  {$EXTERNALSYM LogSeverity}

//
// Flags for SetupDiCreateDeviceInfo
//
const
  DICD_GENERATE_ID       = $00000001;
  {$EXTERNALSYM DICD_GENERATE_ID}
  DICD_INHERIT_CLASSDRVS = $00000002;
  {$EXTERNALSYM DICD_INHERIT_CLASSDRVS}

//
// Flags for SetupDiOpenDeviceInfo
//
const
  DIOD_INHERIT_CLASSDRVS = $00000002;
  {$EXTERNALSYM DIOD_INHERIT_CLASSDRVS}
  DIOD_CANCEL_REMOVE     = $00000004;
  {$EXTERNALSYM DIOD_CANCEL_REMOVE}

//
// Flags for SetupDiOpenDeviceInterface
//
const
  DIODI_NO_ADD = $00000001;
  {$EXTERNALSYM DIODI_NO_ADD}

//
// Flags for SetupDiRegisterDeviceInfo
//
const
  SPRDI_FIND_DUPS = $00000001;
  {$EXTERNALSYM SPRDI_FIND_DUPS}

//
// Ordinal values distinguishing between class drivers and
// device drivers.
// (Passed in 'DriverType' parameter of driver information list APIs)
//
const
  SPDIT_NODRIVER     = $00000000;
  {$EXTERNALSYM SPDIT_NODRIVER}
  SPDIT_CLASSDRIVER  = $00000001;
  {$EXTERNALSYM SPDIT_CLASSDRIVER}
  SPDIT_COMPATDRIVER = $00000002;
  {$EXTERNALSYM SPDIT_COMPATDRIVER}

//
// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
//
const
  DIGCF_DEFAULT         = $00000001; // only valid with DIGCF_DEVICEINTERFACE
  {$EXTERNALSYM DIGCF_DEFAULT}
  DIGCF_PRESENT         = $00000002;
  {$EXTERNALSYM DIGCF_PRESENT}
  DIGCF_ALLCLASSES      = $00000004;
  {$EXTERNALSYM DIGCF_ALLCLASSES}
  DIGCF_PROFILE         = $00000008;
  {$EXTERNALSYM DIGCF_PROFILE}
  DIGCF_DEVICEINTERFACE = $00000010;
  {$EXTERNALSYM DIGCF_DEVICEINTERFACE}

//
// Backward compatibility--do not use.
//

const
  DIGCF_INTERFACEDEVICE = DIGCF_DEVICEINTERFACE;
{$EXTERNALSYM DIGCF_INTERFACEDEVICE}

//
// Flags controlling exclusion from the class information list built
// by SetupDiBuildClassInfoList(Ex)
//
const
  DIBCI_NOINSTALLCLASS = $00000001;
  {$EXTERNALSYM DIBCI_NOINSTALLCLASS}
  DIBCI_NODISPLAYCLASS = $00000002;
  {$EXTERNALSYM DIBCI_NODISPLAYCLASS}

//
// Flags for SetupDiOpenClassRegKeyEx
//
const
  DIOCR_INSTALLER = $00000001; // class installer registry branch
  {$EXTERNALSYM DIOCR_INSTALLER}
  DIOCR_INTERFACE = $00000002; // interface class registry branch
  {$EXTERNALSYM DIOCR_INTERFACE}

//
// KeyType values for SetupDiCreateDevRegKey, SetupDiOpenDevRegKey, and
// SetupDiDeleteDevRegKey.
//
const
  DIREG_DEV  = $00000001; // Open/Create/Delete device key
  {$EXTERNALSYM DIREG_DEV}
  DIREG_DRV  = $00000002; // Open/Create/Delete driver key
  {$EXTERNALSYM DIREG_DRV}
  DIREG_BOTH = $00000004; // Delete both driver and Device key
  {$EXTERNALSYM DIREG_BOTH}

//
// Device registry property codes
// (Codes marked as read-only (R) may only be used for
// SetupDiGetDeviceRegistryProperty)
//
// These values should cover the same set of registry properties
// as defined by the CM_DRP codes in cfgmgr32.h.
//
// Note that SPDRP codes are zero based while CM_DRP codes are one based!
//
const
  SPDRP_DEVICEDESC                  = $00000000; // DeviceDesc (R/W)
  {$EXTERNALSYM SPDRP_DEVICEDESC}
  SPDRP_HARDWAREID                  = $00000001; // HardwareID (R/W)
  {$EXTERNALSYM SPDRP_HARDWAREID}
  SPDRP_COMPATIBLEIDS               = $00000002; // CompatibleIDs (R/W)
  {$EXTERNALSYM SPDRP_COMPATIBLEIDS}
  SPDRP_UNUSED0                     = $00000003; // unused
  {$EXTERNALSYM SPDRP_UNUSED0}
  SPDRP_SERVICE                     = $00000004; // Service (R/W)
  {$EXTERNALSYM SPDRP_SERVICE}
  SPDRP_UNUSED1                     = $00000005; // unused
  {$EXTERNALSYM SPDRP_UNUSED1}
  SPDRP_UNUSED2                     = $00000006; // unused
  {$EXTERNALSYM SPDRP_UNUSED2}
  SPDRP_CLASS                       = $00000007; // Class (R--tied to ClassGUID)
  {$EXTERNALSYM SPDRP_CLASS}
  SPDRP_CLASSGUID                   = $00000008; // ClassGUID (R/W)
  {$EXTERNALSYM SPDRP_CLASSGUID}
  SPDRP_DRIVER                      = $00000009; // Driver (R/W)
  {$EXTERNALSYM SPDRP_DRIVER}
  SPDRP_CONFIGFLAGS                 = $0000000A; // ConfigFlags (R/W)
  {$EXTERNALSYM SPDRP_CONFIGFLAGS}
  SPDRP_MFG                         = $0000000B; // Mfg (R/W)
  {$EXTERNALSYM SPDRP_MFG}
  SPDRP_FRIENDLYNAME                = $0000000C; // FriendlyName (R/W)
  {$EXTERNALSYM SPDRP_FRIENDLYNAME}
  SPDRP_LOCATION_INFORMATION        = $0000000D; // LocationInformation (R/W)
  {$EXTERNALSYM SPDRP_LOCATION_INFORMATION}
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E; // PhysicalDeviceObjectName (R)
  {$EXTERNALSYM SPDRP_PHYSICAL_DEVICE_OBJECT_NAME}
  SPDRP_CAPABILITIES                = $0000000F; // Capabilities (R)
  {$EXTERNALSYM SPDRP_CAPABILITIES}
  SPDRP_UI_NUMBER                   = $00000010; // UiNumber (R)
  {$EXTERNALSYM SPDRP_UI_NUMBER}
  SPDRP_UPPERFILTERS                = $00000011; // UpperFilters (R/W)
  {$EXTERNALSYM SPDRP_UPPERFILTERS}
  SPDRP_LOWERFILTERS                = $00000012; // LowerFilters (R/W)
  {$EXTERNALSYM SPDRP_LOWERFILTERS}
  SPDRP_BUSTYPEGUID                 = $00000013; // BusTypeGUID (R)
  {$EXTERNALSYM SPDRP_BUSTYPEGUID}
  SPDRP_LEGACYBUSTYPE               = $00000014; // LegacyBusType (R)
  {$EXTERNALSYM SPDRP_LEGACYBUSTYPE}
  SPDRP_BUSNUMBER                   = $00000015; // BusNumber (R)
  {$EXTERNALSYM SPDRP_BUSNUMBER}
  SPDRP_ENUMERATOR_NAME             = $00000016; // Enumerator Name (R)
  {$EXTERNALSYM SPDRP_ENUMERATOR_NAME}
  SPDRP_SECURITY                    = $00000017; // Security (R/W, binary form)
  {$EXTERNALSYM SPDRP_SECURITY}
  SPDRP_SECURITY_SDS                = $00000018; // Security (W, SDS form)
  {$EXTERNALSYM SPDRP_SECURITY_SDS}
  SPDRP_DEVTYPE                     = $00000019; // Device Type (R/W)
  {$EXTERNALSYM SPDRP_DEVTYPE}
  SPDRP_EXCLUSIVE                   = $0000001A; // Device is exclusive-access (R/W)
  {$EXTERNALSYM SPDRP_EXCLUSIVE}
  SPDRP_CHARACTERISTICS             = $0000001B; // Device Characteristics (R/W)
  {$EXTERNALSYM SPDRP_CHARACTERISTICS}
  SPDRP_ADDRESS                     = $0000001C; // Device Address (R)
  {$EXTERNALSYM SPDRP_ADDRESS}
  {$IFDEF WINXP}
  SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001D;  // UiNumberDescFormat (R/W)
  {$EXTERNALSYM SPDRP_UI_NUMBER_DESC_FORMAT}
  SPDRP_DEVICE_POWER_DATA           = $0000001E;  // Device Power Data (R)
  {$EXTERNALSYM SPDRP_DEVICE_POWER_DATA}
  SPDRP_REMOVAL_POLICY              = $0000001F;  // Removal Policy (R)
  {$EXTERNALSYM SPDRP_REMOVAL_POLICY}
  SPDRP_REMOVAL_POLICY_HW_DEFAULT   = $00000020;  // Hardware Removal Policy (R)
  {$EXTERNALSYM SPDRP_REMOVAL_POLICY_HW_DEFAULT}
  SPDRP_REMOVAL_POLICY_OVERRIDE     = $00000021;  // Removal Policy Override (RW)
  {$EXTERNALSYM SPDRP_REMOVAL_POLICY_OVERRIDE}
  SPDRP_INSTALL_STATE               = $00000022;  // Device Install State (R)
  {$EXTERNALSYM SPDRP_INSTALL_STATE}

  SPDRP_MAXIMUM_PROPERTY            = $00000023;  // Upper bound on ordinals
  {$EXTERNALSYM SPDRP_MAXIMUM_PROPERTY}
  {$ELSE}
  SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001E; // UiNumberDescFormat (R/W)
  {$EXTERNALSYM SPDRP_UI_NUMBER_DESC_FORMAT}
  SPDRP_MAXIMUM_PROPERTY            = $0000001F; // Upper bound on ordinals
  {$EXTERNALSYM SPDRP_MAXIMUM_PROPERTY}
  {$ENDIF WINXP}
//
// Class registry property codes
// (Codes marked as read-only (R) may only be used for
// SetupDiGetClassRegistryProperty)
//
// These values should cover the same set of registry properties
// as defined by the CM_CRP codes in cfgmgr32.h.
// they should also have a 1:1 correspondence with Device registers, where applicable
// but no overlap otherwise
//
  SPCRP_SECURITY         = $00000017; // Security (R/W, binary form)
  {$EXTERNALSYM SPCRP_SECURITY}
  SPCRP_SECURITY_SDS     = $00000018; // Security (W, SDS form)
  {$EXTERNALSYM SPCRP_SECURITY_SDS}
  SPCRP_DEVTYPE          = $00000019; // Device Type (R/W)
  {$EXTERNALSYM SPCRP_DEVTYPE}
  SPCRP_EXCLUSIVE        = $0000001A; // Device is exclusive-access (R/W)
  {$EXTERNALSYM SPCRP_EXCLUSIVE}
  SPCRP_CHARACTERISTICS  = $0000001B; // Device Characteristics (R/W)
  {$EXTERNALSYM SPCRP_CHARACTERISTICS}
  SPCRP_MAXIMUM_PROPERTY = $0000001C; // Upper bound on ordinals
  {$EXTERNALSYM SPCRP_MAXIMUM_PROPERTY}

//
// Flags controlling the drawing of mini-icons
//
const
  DMI_MASK    = $00000001;
  {$EXTERNALSYM DMI_MASK}
  DMI_BKCOLOR = $00000002;
  {$EXTERNALSYM DMI_BKCOLOR}
  DMI_USERECT = $00000004;
  {$EXTERNALSYM DMI_USERECT}

//
// PropertySheetType values for the SetupDiGetClassDevPropertySheets API
//
const
  DIGCDP_FLAG_BASIC           = $00000001;
  {$EXTERNALSYM DIGCDP_FLAG_BASIC}
  DIGCDP_FLAG_ADVANCED        = $00000002;
  {$EXTERNALSYM DIGCDP_FLAG_ADVANCED}
  DIGCDP_FLAG_REMOTE_BASIC    = $00000003;
  {$EXTERNALSYM DIGCDP_FLAG_REMOTE_BASIC}
  DIGCDP_FLAG_REMOTE_ADVANCED = $00000004;
  {$EXTERNALSYM DIGCDP_FLAG_REMOTE_ADVANCED}

//
// Define ICON IDs publicly exposed from setupapi.
//
const
  IDI_RESOURCEFIRST        = 159;
  {$EXTERNALSYM IDI_RESOURCEFIRST}
  IDI_RESOURCE             = 159;
  {$EXTERNALSYM IDI_RESOURCE}
  IDI_RESOURCELAST         = 161;
  {$EXTERNALSYM IDI_RESOURCELAST}
  IDI_RESOURCEOVERLAYFIRST = 161;
  {$EXTERNALSYM IDI_RESOURCEOVERLAYFIRST}
  IDI_RESOURCEOVERLAYLAST  = 161;
  {$EXTERNALSYM IDI_RESOURCEOVERLAYLAST}
  IDI_CONFLICT             = 161;
  {$EXTERNALSYM IDI_CONFLICT}

  IDI_CLASSICON_OVERLAYFIRST = 500;
  {$EXTERNALSYM IDI_CLASSICON_OVERLAYFIRST}
  IDI_CLASSICON_OVERLAYLAST  = 502;
  {$EXTERNALSYM IDI_CLASSICON_OVERLAYLAST}
  IDI_PROBLEM_OVL            = 500;
  {$EXTERNALSYM IDI_PROBLEM_OVL}
  IDI_DISABLED_OVL           = 501;
  {$EXTERNALSYM IDI_DISABLED_OVL}
  IDI_FORCED_OVL             = 502;
  {$EXTERNALSYM IDI_FORCED_OVL}

//
// PageType values for SetupDiGetWizardPage API
//
const
  SPWPT_SELECTDEVICE = $00000001;
  {$EXTERNALSYM SPWPT_SELECTDEVICE}

//
// Flags for SetupDiGetWizardPage API
//
  SPWP_USE_DEVINFO_DATA = $00000001;
{$EXTERNALSYM SPWP_USE_DEVINFO_DATA}

{$IFDEF WINXP}
type
  PSPInfSignerInfoA = TSPInfSignerInfoA;
  PSPInfSignerInfoW = TSPInfSignerInfoW;
  PSPInfSignerInfo = TSPInfSignerInfoA;
  SP_INF_SIGNER_INFO_A = packed record
    cbSize: DWORD;
    CatalogFile: array [0..MAX_PATH - 1] of AnsiChar;
    DigitalSigner: array [0..MAX_PATH - 1] of AnsiChar;
    DigitalSignerVersion: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  SP_INF_SIGNER_INFO_W = packed record
    cbSize: DWORD;
    CatalogFile: array [0..MAX_PATH - 1] of WideChar;
    DigitalSigner: array [0..MAX_PATH - 1] of WideChar;
    DigitalSignerVersion: array [0..MAX_PATH - 1] of WideChar;
  end;
  TSPInfSignerInfoA = SP_INF_SIGNER_INFO_A;
  TSPInfSignerInfoW = SP_INF_SIGNER_INFO_W;
  TSPInfSignerInfo = TSPOriginalFileInfoA;

//
// Flags for use by SetupDiGetCustomDeviceProperty
//
const
  DICUSTOMDEVPROP_MERGE_MULTISZ = $00000001;
{$ENDIF WINXP}

{$IFNDEF SETUPAPI_LINKONREQUEST}

{$IFDEF WINXP}
function SetupGetFileQueueCount(FileQueue: HSPFILEQ; SubQueueFileOp: UINT; var NumOperations: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetFileQueueCount}
function SetupGetFileQueueFlags(FileQueue: HSPFILEQ; var Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetFileQueueFlags}
function SetupSetFileQueueFlags(FileQueue: HSPFILEQ; FlagMask: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupSetFileQueueFlags}
{$ENDIF WINXP}
function SetupGetInfInformationA(InfSpec: Pointer; SearchControl: DWORD;
  ReturnBuffer: PSPInfInformation; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfInformationA}
function SetupGetInfInformationW(InfSpec: Pointer; SearchControl: DWORD;
  ReturnBuffer: PSPInfInformation; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfInformationW}
function SetupGetInfInformation(InfSpec: Pointer; SearchControl: DWORD;
  ReturnBuffer: PSPInfInformation; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfInformation}

function SetupQueryInfFileInformationA(var InfInformation: TSPInfInformation;
  InfIndex: UINT; ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfFileInformationA}
function SetupQueryInfFileInformationW(var InfInformation: TSPInfInformation;
  InfIndex: UINT; ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfFileInformationW}
function SetupQueryInfFileInformation(var InfInformation: TSPInfInformation;
  InfIndex: UINT; ReturnBuffer: PChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfFileInformation}

{$IFDEF WIN2000}
function SetupQueryInfOriginalFileInformationA(var InfInformation: TSPInfInformation;
  InfIndex: UINT; AlternatePlatformInfo: PSPAltPlatformInfo;
  var OriginalFileInfo: TSPOriginalFileInfoA): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfOriginalFileInformationA}
function SetupQueryInfOriginalFileInformationW(var InfInformation: TSPInfInformation;
  InfIndex: UINT; AlternatePlatformInfo: PSPAltPlatformInfo;
  var OriginalFileInfo: TSPOriginalFileInfoW): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfOriginalFileInformationW}
function SetupQueryInfOriginalFileInformation(var InfInformation: TSPInfInformation;
  InfIndex: UINT; AlternatePlatformInfo: PSPAltPlatformInfo;
  var OriginalFileInfo: TSPOriginalFileInfoA): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfOriginalFileInformation}
{$ENDIF WIN2000}

function SetupQueryInfVersionInformationA(var InfInformation: TSPInfInformation;
  InfIndex: UINT; const Key, ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfVersionInformationA}
function SetupQueryInfVersionInformationW(var InfInformation: TSPInfInformation;
  InfIndex: UINT; const Key, ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfVersionInformationW}
function SetupQueryInfVersionInformation(var InfInformation: TSPInfInformation;
  InfIndex: UINT; const Key, ReturnBuffer: PChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryInfVersionInformation}

function SetupGetInfFileListA(const DirectoryPath: PAnsiChar; InfStyle: DWORD;
  ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfFileListA}
function SetupGetInfFileListW(const DirectoryPath: PWideChar; InfStyle: DWORD;
  ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfFileListW}
function SetupGetInfFileList(const DirectoryPath: PChar; InfStyle: DWORD;
  ReturnBuffer: PChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetInfFileList}

function SetupOpenInfFileA(const FileName: PAnsiChar; const InfClass: PAnsiChar;
  InfStyle: DWORD; ErrorLine: PUINT): HINF; stdcall;
{$EXTERNALSYM SetupOpenInfFileA}
function SetupOpenInfFileW(const FileName: PWideChar; const InfClass: PWideChar;
  InfStyle: DWORD; ErrorLine: PUINT): HINF; stdcall;
{$EXTERNALSYM SetupOpenInfFileW}
function SetupOpenInfFile(const FileName: PChar; const InfClass: PChar;
  InfStyle: DWORD; ErrorLine: PUINT): HINF; stdcall;
{$EXTERNALSYM SetupOpenInfFile}

function SetupOpenMasterInf: HINF; stdcall;
{$EXTERNALSYM SetupOpenMasterInf}

function SetupOpenAppendInfFileA(const FileName: PAnsiChar; InfHandle: HINF;
  ErrorLine: PUINT): BOOL; stdcall;
{$EXTERNALSYM SetupOpenAppendInfFileA}
function SetupOpenAppendInfFileW(const FileName: PWideChar; InfHandle: HINF;
  ErrorLine: PUINT): BOOL; stdcall;
{$EXTERNALSYM SetupOpenAppendInfFileW}
function SetupOpenAppendInfFile(const FileName: PChar; InfHandle: HINF;
  ErrorLine: PUINT): BOOL; stdcall;
{$EXTERNALSYM SetupOpenAppendInfFile}

procedure SetupCloseInfFile(InfHandle: HINF); stdcall;
{$EXTERNALSYM SetupCloseInfFile}

function SetupFindFirstLineA(InfHandle: HINF; Section, Key: PAnsiChar;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindFirstLineA}
function SetupFindFirstLineW(InfHandle: HINF; Section, Key: PWideChar;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindFirstLineW}
function SetupFindFirstLine(InfHandle: HINF; Section, Key: PChar;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindFirstLine}

function SetupFindNextLine(var ContextIn, ContextOut: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindNextLine}

function SetupFindNextMatchLineA(var ContextIn: TInfContext; Key: PAnsiChar;
  var ContextOut: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindNextMatchLineA}
function SetupFindNextMatchLineW(var ContextIn: TInfContext; Key: PWideChar;
  var ContextOut: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindNextMatchLineW}
function SetupFindNextMatchLine(var ContextIn: TInfContext; Key: PChar;
  var ContextOut: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupFindNextMatchLine}

function SetupGetLineByIndexA(InfHandle: HINF; Section: PAnsiChar; Index: DWORD;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineByIndexA}
function SetupGetLineByIndexW(InfHandle: HINF; Section: PWideChar; Index: DWORD;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineByIndexW}
function SetupGetLineByIndex(InfHandle: HINF; Section: PChar; Index: DWORD;
  var Context: TInfContext): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineByIndex}

function SetupGetLineCountA(InfHandle: HINF; Section: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM SetupGetLineCountA}
function SetupGetLineCountW(InfHandle: HINF; Section: PWideChar): Integer; stdcall;
{$EXTERNALSYM SetupGetLineCountW}
function SetupGetLineCount(InfHandle: HINF; Section: PChar): Integer; stdcall;
{$EXTERNALSYM SetupGetLineCount}

function SetupGetLineTextA(Context: PInfContext; InfHandle: HINF; Section,
  Key, ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineTextA}
function SetupGetLineTextW(Context: PInfContext; InfHandle: HINF; Section,
  Key, ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineTextW}
function SetupGetLineText(Context: PInfContext; InfHandle: HINF; Section,
  Key, ReturnBuffer: PChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetLineText}

function SetupGetFieldCount(var Context: TInfContext): DWORD; stdcall;
{$EXTERNALSYM SetupGetFieldCount}

function SetupGetStringFieldA(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetStringFieldA}
function SetupGetStringFieldW(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetStringFieldW}
function SetupGetStringField(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetStringField}

function SetupGetIntField(var Context: TInfContext; FieldIndex: DWORD;
  var IntegerValue: Integer): BOOL; stdcall;
{$EXTERNALSYM SetupGetIntField}

function SetupGetMultiSzFieldA(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetMultiSzFieldA}
function SetupGetMultiSzFieldW(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetMultiSzFieldW}
function SetupGetMultiSzField(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetMultiSzField}

function SetupGetBinaryField(var Context: TInfContext; FieldIndex: DWORD;
  ReturnBuffer: PBYTE; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetBinaryField}

//
// SetupGetFileCompressionInfo is depreciated
// use SetupGetFileCompressionInfoEx instead
//
// ActualSourceFileName returned by SetupGetFileCompressionInfo
// must be freed by the export setupapi!MyFree (NT4+ Win95+)
// or LocalFree (Win2k+)
//
function SetupGetFileCompressionInfoA(const SourceFileName: PAnsiChar;
  var ActualSourceFileName: PAnsiChar; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfoA}
function SetupGetFileCompressionInfoW(const SourceFileName: PWideChar;
  var ActualSourceFileName: PWideChar; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfoW}
function SetupGetFileCompressionInfo(const SourceFileName: PChar;
  var ActualSourceFileName: PChar; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfo}

{$IFDEF WINXP}
//
// SetupGetFileCompressionInfoEx is the preferred API over
// SetupGetFileCompressionInfo. It follows the normal
// conventions of returning BOOL and writing to user-supplied
// buffer.
//
function SetupGetFileCompressionInfoExA(const SourceFileName: PAnsiChar;
  ActualSourceFileNameBuffer: PAnsiChar; var ActualSourceFileNameBufferLen: DWORD;
  RequiredBufferLen: PDWORD; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfoExA}
function SetupGetFileCompressionInfoExW(const SourceFileName: PWideChar;
  ActualSourceFileNameBuffer: PWideChar; var ActualSourceFileNameBufferLen: DWORD;
  RequiredBufferLen: PDWORD; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfoExW}
function SetupGetFileCompressionInfoEx(const SourceFileName: PAnsiChar;
  ActualSourceFileNameBuffer: PAnsiChar; var ActualSourceFileNameBufferLen: DWORD;
  RequiredBufferLen: PDWORD; var SourceFileSize: DWORD;
  var TargetFileSize: DWORD; var CompressionType: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetFileCompressionInfoEx}
{$ENDIF WINXP}

function SetupDecompressOrCopyFileA(const SourceFileName, TargetFileName: PAnsiChar;
  var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupDecompressOrCopyFileA}
function SetupDecompressOrCopyFileW(const SourceFileName, TargetFileName: PWideChar;
  var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupDecompressOrCopyFileW}
function SetupDecompressOrCopyFile(const SourceFileName, TargetFileName: PChar;
  var CompressionType: UINT): DWORD; stdcall;
{$EXTERNALSYM SetupDecompressOrCopyFile}

function SetupGetSourceFileLocationA(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PAnsiChar; var SourceId: UINT; ReturnBuffer: PAnsiChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileLocationA}
function SetupGetSourceFileLocationW(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PWideChar; var SourceId: UINT; ReturnBuffer: PWideChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileLocationW}
function SetupGetSourceFileLocation(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PChar; var SourceId: UINT; ReturnBuffer: PChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileLocation}

function SetupGetSourceFileSizeA(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PAnsiChar; const Section: PAnsiChar; var FileSize: DWORD;
  RoundingFactor: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileSizeA}
function SetupGetSourceFileSizeW(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PWideChar; const Section: PWideChar; var FileSize: DWORD;
  RoundingFactor: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileSizeW}
function SetupGetSourceFileSize(InfHandle: HINF; InfContext: PInfContext;
  const FileName: PChar; const Section: PChar; var FileSize: DWORD;
  RoundingFactor: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceFileSize}

function SetupGetTargetPathA(InfHandle: HINF; InfContext: PInfContext;
  const Section: PAnsiChar; ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetTargetPathA}
function SetupGetTargetPathW(InfHandle: HINF; InfContext: PInfContext;
  const Section: PWideChar; ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetTargetPathW}
function SetupGetTargetPath(InfHandle: HINF; InfContext: PInfContext;
  const Section: PChar; ReturnBuffer: PChar; ReturnBufferSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetTargetPath}

function SetupSetSourceListA(Flags: DWORD; SourceList: PPASTR;
  SourceCount: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupSetSourceListA}
function SetupSetSourceListW(Flags: DWORD; SourceList: PPWSTR;
  SourceCount: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupSetSourceListW}
function SetupSetSourceList(Flags: DWORD; SourceList: PPSTR;
  SourceCount: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupSetSourceList}

function SetupCancelTemporarySourceList: BOOL; stdcall;
{$EXTERNALSYM SetupCancelTemporarySourceList}

function SetupAddToSourceListA(Flags: DWORD; const Source: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupAddToSourceListA}
function SetupAddToSourceListW(Flags: DWORD; const Source: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupAddToSourceListW}
function SetupAddToSourceList(Flags: DWORD; const Source: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupAddToSourceList}

function SetupRemoveFromSourceListA(Flags: DWORD; const Source: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromSourceListA}
function SetupRemoveFromSourceListW(Flags: DWORD; const Source: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromSourceListW}
function SetupRemoveFromSourceList(Flags: DWORD; const Source: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromSourceList}

function SetupQuerySourceListA(Flags: DWORD; var List: PPASTR;
  var Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySourceListA}
function SetupQuerySourceListW(Flags: DWORD; var List: PPWSTR;
  var Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySourceListW}
function SetupQuerySourceList(Flags: DWORD; var List: PPSTR;
  var Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySourceList}

function SetupFreeSourceListA(var List: PPWSTR; Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupFreeSourceListA}
function SetupFreeSourceListW(var List: PPASTR; Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupFreeSourceListW}
function SetupFreeSourceList(var List: PPSTR; Count: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupFreeSourceList}

function SetupPromptForDiskA(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, FileSought, TagFile: PAnsiChar; DiskPromptStyle: DWORD;
  PathBuffer: PAnsiChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupPromptForDiskA}
function SetupPromptForDiskW(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, FileSought, TagFile: PWideChar; DiskPromptStyle: DWORD;
  PathBuffer: PWideChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupPromptForDiskW}
function SetupPromptForDisk(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, FileSought, TagFile: PChar; DiskPromptStyle: DWORD;
  PathBuffer: PChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupPromptForDisk}

function SetupCopyErrorA(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, SourceFile, TargetPathFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD;
  PathBuffer: PAnsiChar; PathBufferSize: DWORD; PathRequiredSize: PDWORD): UINT; stdcall;
{$EXTERNALSYM SetupCopyErrorA}
function SetupCopyErrorW(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, SourceFile, TargetPathFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD;
  PathBuffer: PWideChar; PathBufferSize: DWORD; PathRequiredSize: PDWORD): UINT; stdcall;
{$EXTERNALSYM SetupCopyErrorW}
function SetupCopyError(hwndParent: HWND; const DialogTitle, DiskName,
  PathToSource, SourceFile, TargetPathFile: PChar; Win32ErrorCode: UINT; Style: DWORD;
  PathBuffer: PChar; PathBufferSize: DWORD; PathRequiredSize: PDWORD): UINT; stdcall;
{$EXTERNALSYM SetupCopyError}

function SetupRenameErrorA(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupRenameErrorA}
function SetupRenameErrorW(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupRenameErrorW}
function SetupRenameError(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupRenameError}

function SetupDeleteErrorA(hwndParent: HWND; const DialogTitle, File_: PAnsiChar;
  Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupDeleteErrorA}
function SetupDeleteErrorW(hwndParent: HWND; const DialogTitle, File_: PWideChar;
  Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupDeleteErrorW}
function SetupDeleteError(hwndParent: HWND; const DialogTitle, File_: PChar;
  Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupDeleteError}

{$IFDEF WIN2000}
function SetupBackupErrorA(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupBackupErrorA}
function SetupBackupErrorW(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupBackupErrorW}
function SetupBackupError(hwndParent: HWND; const DialogTitle, SourceFile,
  TargetFile: PChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
{$EXTERNALSYM SetupBackupError}
{$ENDIF WIN2000}

function SetupSetDirectoryIdA(InfHandle: HINF; Id: DWORD; const Directory: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryIdA}
function SetupSetDirectoryIdW(InfHandle: HINF; Id: DWORD; const Directory: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryIdW}
function SetupSetDirectoryId(InfHandle: HINF; Id: DWORD; const Directory: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryId}

function SetupSetDirectoryIdExA(InfHandle: HINF; Id: DWORD; const Directory: PAnsiChar;
  Flags: DWORD; Reserved1: DWORD; Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryIdExA}
function SetupSetDirectoryIdExW(InfHandle: HINF; Id: DWORD; const Directory: PWideChar;
  Flags: DWORD; Reserved1: DWORD; Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryIdExW}
function SetupSetDirectoryIdEx(InfHandle: HINF; Id: DWORD; const Directory: PChar;
  Flags: DWORD; Reserved1: DWORD; Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupSetDirectoryIdEx}

function SetupGetSourceInfoA(InfHandle: HINF; SourceId, InfoDesired: UINT;
  ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceInfoA}
function SetupGetSourceInfoW(InfHandle: HINF; SourceId, InfoDesired: UINT;
  ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceInfoW}
function SetupGetSourceInfo(InfHandle: HINF; SourceId, InfoDesired: UINT;
  ReturnBuffer: PChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupGetSourceInfo}

function SetupInstallFileA(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PAnsiChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFileA}
function SetupInstallFileW(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PWideChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFileW}
function SetupInstallFile(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFile}

function SetupInstallFileExA(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PAnsiChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackA; Context: Pointer; var FileWasInUse: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFileExA}
function SetupInstallFileExW(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PWideChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackW; Context: Pointer; var FileWasInUse: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFileExW}
function SetupInstallFileEx(InfHandle: HINF; InfContext: PInfContext;
  const SourceFile, SourcePathRoot, DestinationName: PChar; CopyStyle: DWORD;
  CopyMsgHandler: TSPFileCallbackA; Context: Pointer; var FileWasInUse: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFileEx}

function SetupOpenFileQueue: HSPFILEQ; stdcall;
{$EXTERNALSYM SetupOpenFileQueue}

function SetupCloseFileQueue(QueueHandle: HSPFILEQ): BOOL; stdcall;
{$EXTERNALSYM SetupCloseFileQueue}

{$IFDEF WIN2000}
function SetupSetFileQueueAlternatePlatformA(QueueHandle: HSPFILEQ;
  AlternatePlatformInfo: PSPAltPlatformInfo;
  const AlternateDefaultCatalogFile: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetFileQueueAlternatePlatformA}
function SetupSetFileQueueAlternatePlatformW(QueueHandle: HSPFILEQ;
  AlternatePlatformInfo: PSPAltPlatformInfo;
  const AlternateDefaultCatalogFile: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetFileQueueAlternatePlatformW}
function SetupSetFileQueueAlternatePlatform(QueueHandle: HSPFILEQ;
  AlternatePlatformInfo: PSPAltPlatformInfo;
  const AlternateDefaultCatalogFile: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetFileQueueAlternatePlatform}
{$ENDIF WIN2000}

function SetupSetPlatformPathOverrideA(const Override_: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetPlatformPathOverrideA}
function SetupSetPlatformPathOverrideW(const Override_: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetPlatformPathOverrideW}
function SetupSetPlatformPathOverride(const Override_: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupSetPlatformPathOverride}

function SetupQueueCopyA(QueueHandle: HSPFILEQ; const SourceRootPath, SourcePath,
  SourceFilename, SourceDescription, SourceTagfile, TargetDirectory,
  TargetFilename: PAnsiChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopyA}
function SetupQueueCopyW(QueueHandle: HSPFILEQ; const SourceRootPath, SourcePath,
  SourceFilename, SourceDescription, SourceTagfile, TargetDirectory,
  TargetFilename: PWideChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopyW}
function SetupQueueCopy(QueueHandle: HSPFILEQ; const SourceRootPath, SourcePath,
  SourceFilename, SourceDescription, SourceTagfile, TargetDirectory,
  TargetFilename: PChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopy}

{$IFDEF WIN2000}
function SetupQueueCopyIndirectA(var CopyParams: TSPFileCopyParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopyIndirectA}
function SetupQueueCopyIndirectW(var CopyParams: TSPFileCopyParamsW): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopyIndirectW}
function SetupQueueCopyIndirect(var CopyParams: TSPFileCopyParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopyIndirect}
{$ENDIF WIN2000}

function SetupQueueDefaultCopyA(QueueHandle: HSPFILEQ; InfHandle: HINF;
  const SourceRootPath, SourceFilename, TargetFilename: PAnsiChar;
  CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDefaultCopyA}
function SetupQueueDefaultCopyW(QueueHandle: HSPFILEQ; InfHandle: HINF;
  const SourceRootPath, SourceFilename, TargetFilename: PWideChar;
  CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDefaultCopyW}
function SetupQueueDefaultCopy(QueueHandle: HSPFILEQ; InfHandle: HINF;
  const SourceRootPath, SourceFilename, TargetFilename: PChar;
  CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDefaultCopy}

function SetupQueueCopySectionA(QueueHandle: HSPFILEQ; const SourceRootPath: PAnsiChar;
  InfHandle: HINF; ListInfHandle: HINF; const Section: PAnsiChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopySectionA}
function SetupQueueCopySectionW(QueueHandle: HSPFILEQ; const SourceRootPath: PWideChar;
  InfHandle: HINF; ListInfHandle: HINF; const Section: PWideChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopySectionW}
function SetupQueueCopySection(QueueHandle: HSPFILEQ; const SourceRootPath: PChar;
  InfHandle: HINF; ListInfHandle: HINF; const Section: PChar; CopyStyle: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueueCopySection}

function SetupQueueDeleteA(QueueHandle: HSPFILEQ; const PathPart1, PathPart2: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDeleteA}
function SetupQueueDeleteW(QueueHandle: HSPFILEQ; const PathPart1, PathPart2: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDeleteW}
function SetupQueueDelete(QueueHandle: HSPFILEQ; const PathPart1, PathPart2: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDelete}

function SetupQueueDeleteSectionA(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDeleteSectionA}
function SetupQueueDeleteSectionW(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDeleteSectionW}
function SetupQueueDeleteSection(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueDeleteSection}

function SetupQueueRenameA(QueueHandle: HSPFILEQ; const SourcePath,
  SourceFilename, TargetPath, TargetFilename: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRenameA}
function SetupQueueRenameW(QueueHandle: HSPFILEQ; const SourcePath,
  SourceFilename, TargetPath, TargetFilename: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRenameW}
function SetupQueueRename(QueueHandle: HSPFILEQ; const SourcePath,
  SourceFilename, TargetPath, TargetFilename: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRename}

function SetupQueueRenameSectionA(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRenameSectionA}
function SetupQueueRenameSectionW(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRenameSectionW}
function SetupQueueRenameSection(QueueHandle: HSPFILEQ; InfHandle: HINF;
  ListInfHandle: HINF; const Section: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupQueueRenameSection}

function SetupCommitFileQueueA(Owner: HWND; QueueHandle: HSPFILEQ;
  MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupCommitFileQueueA}
function SetupCommitFileQueueW(Owner: HWND; QueueHandle: HSPFILEQ;
  MsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupCommitFileQueueW}
function SetupCommitFileQueue(Owner: HWND; QueueHandle: HSPFILEQ;
  MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupCommitFileQueue}

function SetupScanFileQueueA(FileQueue: HSPFILEQ; Flags: DWORD; Window: HWND;
  CallbackRoutine: TSPFileCallbackA; CallbackContext: Pointer; var Result: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupScanFileQueueA}
function SetupScanFileQueueW(FileQueue: HSPFILEQ; Flags: DWORD; Window: HWND;
  CallbackRoutine: TSPFileCallbackW; CallbackContext: Pointer; var Result: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupScanFileQueueW}
function SetupScanFileQueue(FileQueue: HSPFILEQ; Flags: DWORD; Window: HWND;
  CallbackRoutine: TSPFileCallbackA; CallbackContext: Pointer; var Result: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupScanFileQueue}

function SetupCopyOEMInfA(const SourceInfFileName, OEMSourceMediaLocation: PAnsiChar;
  OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PAnsiChar;
  DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
  DestinationInfFileNameComponent: PPASTR): BOOL; stdcall;
{$EXTERNALSYM SetupCopyOEMInfA}
function SetupCopyOEMInfW(const SourceInfFileName, OEMSourceMediaLocation: PWideChar;
  OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PWideChar;
  DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
  DestinationInfFileNameComponent: PPWSTR): BOOL; stdcall;
{$EXTERNALSYM SetupCopyOEMInfW}
function SetupCopyOEMInf(const SourceInfFileName, OEMSourceMediaLocation: PChar;
  OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PChar;
  DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
  DestinationInfFileNameComponent: PPSTR): BOOL; stdcall;
{$EXTERNALSYM SetupCopyOEMInf}

{$IFDEF WINXP}
function SetupUninstallOEMInfA(const InfFileName: PAnsiChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupUninstallOEMInfA}
function SetupUninstallOEMInfW(const InfFileName: PWideChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupUninstallOEMInfW}
function SetupUninstallOEMInf(const InfFileName: PAnsiChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupUninstallOEMInf}
function SetupUninstallNewlyCopiedInfs(FileQueue: HSPFILEQ; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupUninstallNewlyCopiedInfs}
{$ENDIF WINXP}

//
// Disk space list APIs
//
function SetupCreateDiskSpaceListA(Reserved1: Pointer; Reserved2: DWORD;
  Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupCreateDiskSpaceListA}
function SetupCreateDiskSpaceListW(Reserved1: Pointer; Reserved2: DWORD;
  Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupCreateDiskSpaceListW}
function SetupCreateDiskSpaceList(Reserved1: Pointer; Reserved2: DWORD;
  Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupCreateDiskSpaceList}

function SetupDuplicateDiskSpaceListA(DiskSpace: HDSKSPC; Reserved1: Pointer;
  Reserved2: DWORD; Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupDuplicateDiskSpaceListA}
function SetupDuplicateDiskSpaceListW(DiskSpace: HDSKSPC; Reserved1: Pointer;
  Reserved2: DWORD; Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupDuplicateDiskSpaceListW}
function SetupDuplicateDiskSpaceList(DiskSpace: HDSKSPC; Reserved1: Pointer;
  Reserved2: DWORD; Flags: UINT): HDSKSPC; stdcall;
{$EXTERNALSYM SetupDuplicateDiskSpaceList}

function SetupDestroyDiskSpaceList(DiskSpace: HDSKSPC): BOOL; stdcall;
{$EXTERNALSYM SetupDestroyDiskSpaceList}

function SetupQueryDrivesInDiskSpaceListA(DiskSpace: HDSKSPC; ReturnBuffer: PAnsiChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryDrivesInDiskSpaceListA}
function SetupQueryDrivesInDiskSpaceListW(DiskSpace: HDSKSPC; ReturnBuffer: PWideChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryDrivesInDiskSpaceListW}
function SetupQueryDrivesInDiskSpaceList(DiskSpace: HDSKSPC; ReturnBuffer: PChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryDrivesInDiskSpaceList}

function SetupQuerySpaceRequiredOnDriveA(DiskSpace: HDSKSPC; const DriveSpec: PAnsiChar;
  var SpaceRequired: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySpaceRequiredOnDriveA}
function SetupQuerySpaceRequiredOnDriveW(DiskSpace: HDSKSPC; const DriveSpec: PWideChar;
  var SpaceRequired: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySpaceRequiredOnDriveW}
function SetupQuerySpaceRequiredOnDrive(DiskSpace: HDSKSPC; const DriveSpec: PChar;
  var SpaceRequired: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupQuerySpaceRequiredOnDrive}

function SetupAdjustDiskSpaceListA(DiskSpace: HDSKSPC; const DriveRoot: PAnsiChar;
  Amount: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAdjustDiskSpaceListA}
function SetupAdjustDiskSpaceListW(DiskSpace: HDSKSPC; const DriveRoot: PWideChar;
  Amount: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAdjustDiskSpaceListW}
function SetupAdjustDiskSpaceList(DiskSpace: HDSKSPC; const DriveRoot: PChar;
  Amount: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAdjustDiskSpaceList}

function SetupAddToDiskSpaceListA(DiskSpace: HDSKSPC; const TargetFilespec: PAnsiChar;
  FileSize: Int64; Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddToDiskSpaceListA}
function SetupAddToDiskSpaceListW(DiskSpace: HDSKSPC; const TargetFilespec: PWideChar;
  FileSize: Int64; Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddToDiskSpaceListW}
function SetupAddToDiskSpaceList(DiskSpace: HDSKSPC; const TargetFilespec: PChar;
  FileSize: Int64; Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddToDiskSpaceList}

function SetupAddSectionToDiskSpaceListA(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PAnsiChar; Operation: UINT;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddSectionToDiskSpaceListA}
function SetupAddSectionToDiskSpaceListW(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PWideChar; Operation: UINT;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddSectionToDiskSpaceListW}
function SetupAddSectionToDiskSpaceList(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PChar; Operation: UINT;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddSectionToDiskSpaceList}

function SetupAddInstallSectionToDiskSpaceListA( DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PAnsiChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddInstallSectionToDiskSpaceListA}
function SetupAddInstallSectionToDiskSpaceListW( DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PWideChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddInstallSectionToDiskSpaceListW}
function SetupAddInstallSectionToDiskSpaceList( DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupAddInstallSectionToDiskSpaceList}

function SetupRemoveFromDiskSpaceListA(DiskSpace: HDSKSPC; const TargetFilespec: PAnsiChar;
  Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromDiskSpaceListA}
function SetupRemoveFromDiskSpaceListW(DiskSpace: HDSKSPC; const TargetFilespec: PWideChar;
  Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromDiskSpaceListW}
function SetupRemoveFromDiskSpaceList(DiskSpace: HDSKSPC; const TargetFilespec: PChar;
  Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFromDiskSpaceList}

function SetupRemoveSectionFromDiskSpaceListA(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PAnsiChar; Operation: UINT; Reserved1: Pointer;
  Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveSectionFromDiskSpaceListA}
function SetupRemoveSectionFromDiskSpaceListW(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PWideChar; Operation: UINT; Reserved1: Pointer;
  Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveSectionFromDiskSpaceListW}
function SetupRemoveSectionFromDiskSpaceList(DiskSpace: HDSKSPC; InfHandle: HINF;
  ListInfHandle: HINF; const SectionName: PChar; Operation: UINT; Reserved1: Pointer;
  Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveSectionFromDiskSpaceList}

function SetupRemoveInstallSectionFromDiskSpaceListA(DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PAnsiChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveInstallSectionFromDiskSpaceListA}
function SetupRemoveInstallSectionFromDiskSpaceListW(DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PWideChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveInstallSectionFromDiskSpaceListW}
function SetupRemoveInstallSectionFromDiskSpaceList(DiskSpace: HDSKSPC;
  InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PChar;
  Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveInstallSectionFromDiskSpaceList}

//
// Cabinet APIs
//

function SetupIterateCabinetA(const CabinetFile: PAnsiChar; Reserved: DWORD;
  MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupIterateCabinetA}
function SetupIterateCabinetW(const CabinetFile: PWideChar; Reserved: DWORD;
  MsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupIterateCabinetW}
function SetupIterateCabinet(const CabinetFile: PChar; Reserved: DWORD;
  MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupIterateCabinet}

function SetupPromptReboot(FileQueue: HSPFILEQ; Owner: HWND; ScanOnly: BOOL): Integer; stdcall;
{$EXTERNALSYM SetupPromptReboot}

function SetupInitDefaultQueueCallback(OwnerWindow: HWND): Pointer; stdcall;
{$EXTERNALSYM SetupInitDefaultQueueCallback}

function SetupInitDefaultQueueCallbackEx(OwnerWindow: HWND; AlternateProgressWindow: HWND;
  ProgressMessage: UINT; Reserved1: DWORD; Reserved2: Pointer): Pointer; stdcall;
{$EXTERNALSYM SetupInitDefaultQueueCallbackEx}

procedure SetupTermDefaultQueueCallback(Context: Pointer); stdcall;
{$EXTERNALSYM SetupTermDefaultQueueCallback}

function SetupDefaultQueueCallbackA(Context: Pointer; Notification: UINT;
  Param1, Param2: UINT_PTR): UINT; stdcall;
{$EXTERNALSYM SetupDefaultQueueCallbackA}
function SetupDefaultQueueCallbackW(Context: Pointer; Notification: UINT;
  Param1, Param2: UINT_PTR): UINT; stdcall;
{$EXTERNALSYM SetupDefaultQueueCallbackW}
function SetupDefaultQueueCallback(Context: Pointer; Notification: UINT;
  Param1, Param2: UINT_PTR): UINT; stdcall;
{$EXTERNALSYM SetupDefaultQueueCallback}

//
// The INF may supply any arbitrary data type ordinal in the highword except
// for the following: REG_NONE, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ.  If this
// technique is used, then the data is given in binary format, one byte per
// field.
//

function SetupInstallFromInfSectionA(Owner: HWND; InfHandle: HINF;
  const SectionName: PAnsiChar; Flags: UINT; RelativeKeyRoot: HKEY;
  const SourceRootPath: PAnsiChar; CopyFlags: UINT; MsgHandler: TSPFileCallbackA;
  Context: Pointer; DeviceInfoSet: HDEVINFO; DeviceIn: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFromInfSectionA}
function SetupInstallFromInfSectionW(Owner: HWND; InfHandle: HINF;
  const SectionName: PWideChar; Flags: UINT; RelativeKeyRoot: HKEY;
  const SourceRootPath: PWideChar; CopyFlags: UINT; MsgHandler: TSPFileCallbackW;
  Context: Pointer; DeviceInfoSet: HDEVINFO; DeviceIn: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFromInfSectionW}
function SetupInstallFromInfSection(Owner: HWND; InfHandle: HINF;
  const SectionName: PChar; Flags: UINT; RelativeKeyRoot: HKEY;
  const SourceRootPath: PChar; CopyFlags: UINT; MsgHandler: TSPFileCallbackA;
  Context: Pointer; DeviceInfoSet: HDEVINFO; DeviceIn: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFromInfSection}

function SetupInstallFilesFromInfSectionA(InfHandle: HINF; LayoutInfHandle: HINF;
  FileQueue: HSPFILEQ; const SectionName, SourceRootPath: PAnsiChar;
  CopyFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFilesFromInfSectionA}
function SetupInstallFilesFromInfSectionW(InfHandle: HINF; LayoutInfHandle: HINF;
  FileQueue: HSPFILEQ; const SectionName, SourceRootPath: PWideChar;
  CopyFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFilesFromInfSectionW}
function SetupInstallFilesFromInfSection(InfHandle: HINF; LayoutInfHandle: HINF;
  FileQueue: HSPFILEQ; const SectionName, SourceRootPath: PChar;
  CopyFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SetupInstallFilesFromInfSection}

function SetupInstallServicesFromInfSectionA(InfHandle: HINF;
  const SectionName: PAnsiChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSectionA}
function SetupInstallServicesFromInfSectionW(InfHandle: HINF;
  const SectionName: PWideChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSectionW}
function SetupInstallServicesFromInfSection(InfHandle: HINF;
  const SectionName: PChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSection}

function SetupInstallServicesFromInfSectionExA(InfHandle: HINF;
  const SectionName: PAnsiChar; Flags: DWORD; DeviceInfoSet: HDEVINFO;
  DeviceInfoData: TSPDevInfoData; Reserved1, Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSectionExA}
function SetupInstallServicesFromInfSectionExW(InfHandle: HINF;
  const SectionName: PWideChar; Flags: DWORD; DeviceInfoSet: HDEVINFO;
  DeviceInfoData: TSPDevInfoData; Reserved1, Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSectionExW}
function SetupInstallServicesFromInfSectionEx(InfHandle: HINF;
  const SectionName: PChar; Flags: DWORD; DeviceInfoSet: HDEVINFO;
  DeviceInfoData: TSPDevInfoData; Reserved1, Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupInstallServicesFromInfSectionEx}

{$IFDEF WINXP}
//
// High level routine, usually used via rundll32.dll
// to perform right-click install action on INFs
// May be called directly:
//
// wsprintf(CmdLineBuffer,TEXT("DefaultInstall 132 %s"),InfPath);
// InstallHinfSection(NULL,NULL,CmdLineBuffer,0);
//
procedure InstallHinfSectionA(Window: HWND; ModuleHandle: HINSTANCE;
  CommandLine: PAnsiChar; ShowCommand: Integer); stdcall;
{$EXTERNALSYM InstallHinfSectionA}
procedure InstallHinfSectionW(Window: HWND; ModuleHandle: HINSTANCE;
  CommandLine: PWideChar; ShowCommand: Integer); stdcall;
{$EXTERNALSYM InstallHinfSectionW}
procedure InstallHinfSection(Window: HWND; ModuleHandle: HINSTANCE;
  CommandLine: PAnsiChar; ShowCommand: Integer); stdcall;
{$EXTERNALSYM InstallHinfSection}
{$ENDIF WINXP}

//
// Define handle type for Setup file log.
//

type
  HSPFILELOG = Pointer;
  {$EXTERNALSYM HSPFILELOG}

function SetupInitializeFileLogA(const LogFileName: PAnsiChar; Flags: DWORD): HSPFILELOG; stdcall;
{$EXTERNALSYM SetupInitializeFileLogA}
function SetupInitializeFileLogW(const LogFileName: PWideChar; Flags: DWORD): HSPFILELOG; stdcall;
{$EXTERNALSYM SetupInitializeFileLogW}
function SetupInitializeFileLog(const LogFileName: PChar; Flags: DWORD): HSPFILELOG; stdcall;
{$EXTERNALSYM SetupInitializeFileLog}

function SetupTerminateFileLog(FileLogHandle: HSPFILELOG): BOOL; stdcall;
{$EXTERNALSYM SetupTerminateFileLog}

function SetupLogFileA(FileLogHandle: HSPFILELOG; const LogSectionName,
  SourceFilename, TargetFilename: PAnsiChar; Checksum: DWORD; DiskTagfile,
  DiskDescription, OtherInfo: PAnsiChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupLogFileA}
function SetupLogFileW(FileLogHandle: HSPFILELOG; const LogSectionName,
  SourceFilename, TargetFilename: PWideChar; Checksum: DWORD; DiskTagfile,
  DiskDescription, OtherInfo: PWideChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupLogFileW}
function SetupLogFile(FileLogHandle: HSPFILELOG; const LogSectionName,
  SourceFilename, TargetFilename: PChar; Checksum: DWORD; DiskTagfile,
  DiskDescription, OtherInfo: PChar; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupLogFile}

function SetupRemoveFileLogEntryA(FileLogHandle: HSPFILELOG;
  const LogSectionName: PAnsiChar; const TargetFilename: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFileLogEntryA}
function SetupRemoveFileLogEntryW(FileLogHandle: HSPFILELOG;
  const LogSectionName: PWideChar; const TargetFilename: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFileLogEntryW}
function SetupRemoveFileLogEntry(FileLogHandle: HSPFILELOG;
  const LogSectionName: PChar; const TargetFilename: PChar): BOOL; stdcall;
{$EXTERNALSYM SetupRemoveFileLogEntry}

function SetupQueryFileLogA(FileLogHandle: HSPFILELOG; const LogSectionName,
  TargetFilename: PAnsiChar; DesiredInfo: SETUPFILELOGINFO; DataOut: PAnsiChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryFileLogA}
function SetupQueryFileLogW(FileLogHandle: HSPFILELOG; const LogSectionName,
  TargetFilename: PWideChar; DesiredInfo: SETUPFILELOGINFO; DataOut: PWideChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryFileLogW}
function SetupQueryFileLog(FileLogHandle: HSPFILELOG; const LogSectionName,
  TargetFilename: PChar; DesiredInfo: SETUPFILELOGINFO; DataOut: PChar;
  ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupQueryFileLog}

//
// Text logging APIs
//

function SetupOpenLog(Erase: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetupOpenLog}

function SetupLogErrorA(const MessageString: PAnsiChar; Severity: LOGSEVERITY): BOOL; stdcall;
{$EXTERNALSYM SetupLogErrorA}
function SetupLogErrorW(const MessageString: PWideChar; Severity: LOGSEVERITY): BOOL; stdcall;
{$EXTERNALSYM SetupLogErrorW}
function SetupLogError(const MessageString: PChar; Severity: LOGSEVERITY): BOOL; stdcall;
{$EXTERNALSYM SetupLogError}

procedure SetupCloseLog; stdcall;
{$EXTERNALSYM SetupCloseLog}

//
// Backup Information API's
//

{$IFDEF WIN2000}
function SetupGetBackupInformationA(QueueHandle: HSPFILEQ;
  var BackupParams: TSPBackupQueueParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupGetBackupInformationA}
function SetupGetBackupInformationW(QueueHandle: HSPFILEQ;
  var BackupParams: TSPBackupQueueParamsW): BOOL; stdcall;
{$EXTERNALSYM SetupGetBackupInformationW}
function SetupGetBackupInformation(QueueHandle: HSPFILEQ;
  var BackupParams: TSPBackupQueueParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupGetBackupInformation}
{$ENDIF WIN2000}

{$IFDEF WINXP}
function SetupPrepareQueueForRestoreA(QueueHandle: HSPFILEQ;
  BackupPath: PAnsiChar; RestoreFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupPrepareQueueForRestoreA}
function SetupPrepareQueueForRestoreW(QueueHandle: HSPFILEQ;
  BackupPath: PWideChar; RestoreFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupPrepareQueueForRestoreW}
function SetupPrepareQueueForRestore(QueueHandle: HSPFILEQ;
  BackupPath: PAnsiChar; RestoreFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupPrepareQueueForRestore}

//
// Control forcing of Non-Interactive Mode
// Overridden if SetupAPI is run in non-interactive window session
//

function SetupSetNonInteractiveMode(NonInteractiveFlag: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetupSetNonInteractiveMode}
function SetupGetNonInteractiveMode: BOOL; stdcall;
{$EXTERNALSYM SetupGetNonInteractiveMode}
{$ENDIF WINXP}

//
// Device Installer APIs
//

function SetupDiCreateDeviceInfoList(ClassGuid: PGUID; hwndParent: HWND): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoList}

function SetupDiCreateDeviceInfoListExA(ClassGuid: PGUID; hwndParent: HWND;
  const MachineName: PAnsiChar; Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoListExA}
function SetupDiCreateDeviceInfoListExW(ClassGuid: PGUID; hwndParent: HWND;
  const MachineName: PWideChar; Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoListExW}
function SetupDiCreateDeviceInfoListEx(ClassGuid: PGUID; hwndParent: HWND;
  const MachineName: PChar; Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoListEx}

function SetupDiGetDeviceInfoListClass(DeviceInfoSet: HDEVINFO;
  var ClassGuid: TGUID): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInfoListClass}

function SetupDiGetDeviceInfoListDetailA(DeviceInfoSet: HDEVINFO;
  var DeviceInfoSetDetailData: TSPDevInfoListDetailDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInfoListDetailA}
function SetupDiGetDeviceInfoListDetailW(DeviceInfoSet: HDEVINFO;
  var DeviceInfoSetDetailData: TSPDevInfoListDetailDataW): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInfoListDetailW}
function SetupDiGetDeviceInfoListDetail(DeviceInfoSet: HDEVINFO;
  var DeviceInfoSetDetailData: TSPDevInfoListDetailDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInfoListDetail}

function SetupDiCreateDeviceInfoA(DeviceInfoSet: HDEVINFO; const DeviceName: PAnsiChar;
  var ClassGuid: TGUID; const DeviceDescription: PAnsiChar; hwndParent: HWND;
  CreationFlags: DWORD; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoA}

function SetupDiCreateDeviceInfoW(DeviceInfoSet: HDEVINFO; const DeviceName: PWideChar;
  var ClassGuid: TGUID; const DeviceDescription: PWideChar; hwndParent: HWND;
  CreationFlags: DWORD; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfoW}

function SetupDiCreateDeviceInfo(DeviceInfoSet: HDEVINFO; const DeviceName: PChar;
  var ClassGuid: TGUID; const DeviceDescription: PChar; hwndParent: HWND;
  CreationFlags: DWORD; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInfo}


function SetupDiOpenDeviceInfoA(DeviceInfoSet: HDEVINFO;
  const DeviceInstanceId: PAnsiChar; hwndParent: HWND; OpenFlags: DWORD;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInfoA}
function SetupDiOpenDeviceInfoW(DeviceInfoSet: HDEVINFO;
  const DeviceInstanceId: PWideChar; hwndParent: HWND; OpenFlags: DWORD;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInfoW}
function SetupDiOpenDeviceInfo(DeviceInfoSet: HDEVINFO;
  const DeviceInstanceId: PChar; hwndParent: HWND; OpenFlags: DWORD;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInfo}

function SetupDiGetDeviceInstanceIdA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DeviceInstanceId: PAnsiChar;
  DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstanceIdA}
function SetupDiGetDeviceInstanceIdW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DeviceInstanceId: PWideChar;
  DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstanceIdW}
function SetupDiGetDeviceInstanceId(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DeviceInstanceId: PChar;
  DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstanceId}

function SetupDiDeleteDeviceInfo(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteDeviceInfo}

function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO;
  MemberIndex: DWORD; var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDeviceInfo}

function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): BOOL; stdcall;
{$EXTERNALSYM SetupDiDestroyDeviceInfoList}

function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
  MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDeviceInterfaces}

//
// Backward compatibility--do not use
//

function SetupDiEnumInterfaceDevice(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
  MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDeviceInterfaces}

function SetupDiCreateDeviceInterfaceA(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PAnsiChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterfaceA}
function SetupDiCreateDeviceInterfaceW(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PWideChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterfaceW}
function SetupDiCreateDeviceInterface(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterface}

//
// Backward compatibility--do not use.
//

function SetupDiCreateInterfaceDeviceA(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PAnsiChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDeviceA}
function SetupDiCreateInterfaceDeviceW(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PWideChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDeviceW}
function SetupDiCreateInterfaceDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
  const ReferenceString: PChar; CreationFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDevice}

function SetupDiOpenDeviceInterfaceA(DeviceInfoSet: HDEVINFO;
  const DevicePath: PAnsiChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInterfaceA}
function SetupDiOpenDeviceInterfaceW(DeviceInfoSet: HDEVINFO;
  const DevicePath: PWideChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInterfaceW}
function SetupDiOpenDeviceInterface(DeviceInfoSet: HDEVINFO;
  const DevicePath: PChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInterface}

//
// Backward compatibility--do not use
//

function SetupDiOpenInterfaceDeviceA(DeviceInfoSet: HDEVINFO;
  const DevicePath: PAnsiChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenInterfaceDeviceA}
function SetupDiOpenInterfaceDeviceW(DeviceInfoSet: HDEVINFO;
  const DevicePath: PWideChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenInterfaceDeviceW}
function SetupDiOpenInterfaceDevice(DeviceInfoSet: HDEVINFO;
  const DevicePath: PChar; OpenFlags: DWORD;
  DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiOpenInterfaceDevice}

function SetupDiGetDeviceInterfaceAlias(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; var AliasInterfaceClassGuid: TGUID;
  var AliasDeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInterfaceAlias}

//
// Backward compatibility--do not use.
//

function SetupDiGetInterfaceDeviceAlias(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData;
  var AliasInterfaceClassGuid: TGUID;
  var AliasDeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetInterfaceDeviceAlias}

function SetupDiDeleteDeviceInterfaceData(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteDeviceInterfaceData}

//
// Backward compatibility--do not use.
//

function SetupDiDeleteInterfaceDeviceData(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteInterfaceDeviceData}

function SetupDiRemoveDeviceInterface(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiRemoveDeviceInterface}

//
// Backward compatibility--do not use.
//

function SetupDiRemoveInterfaceDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
{$EXTERNALSYM SetupDiRemoveInterfaceDevice}

function SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
  DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInterfaceDetailA}
function SetupDiGetDeviceInterfaceDetailW(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
  DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInterfaceDetailW}
function SetupDiGetDeviceInterfaceDetail(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
  DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInterfaceDetail}

//
// Backward compatibility--do not use.
//

function SetupDiGetInterfaceDeviceDetailA(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
  DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetInterfaceDeviceDetailA}
function SetupDiGetInterfaceDeviceDetailW(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
  DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetInterfaceDeviceDetailW}
function SetupDiGetInterfaceDeviceDetail(DeviceInfoSet: HDEVINFO;
  DeviceInterfaceData: PSPDeviceInterfaceData;
  DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
  DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
  Device: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetInterfaceDeviceDetail}

//
// Default install handler for DIF_INSTALLINTERFACES.
//

function SetupDiInstallDeviceInterfaces(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallDeviceInterfaces}

//
// Backward compatibility--do not use.
//

function SetupDiInstallInterfaceDevices(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallInterfaceDevices}

{$IFDEF WINXP}
function SetupDiSetDeviceInterfaceDefault(DeviceInfoSet: HDEVINFO
  var DeviceInterfaceData: TSPDeviceInterfaceData; Flags: DWORD;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceInterfaceDefault}
{$ENDIF WINXP}

//
// Default install handler for DIF_REGISTERDEVICE
//

function SetupDiRegisterDeviceInfo(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Flags: DWORD; CompareProc: TSPDetSigCmpProc;
  CompareContext: Pointer; DupDeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiRegisterDeviceInfo}

function SetupDiBuildDriverInfoList(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiBuildDriverInfoList}

function SetupDiCancelDriverInfoSearch(DeviceInfoSet: HDEVINFO): BOOL; stdcall;
{$EXTERNALSYM SetupDiCancelDriverInfoSearch}

function SetupDiEnumDriverInfoA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverType: DWORD; MemberIndex: DWORD;
  var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDriverInfoA}
function SetupDiEnumDriverInfoW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverType: DWORD; MemberIndex: DWORD;
  var DriverInfoData: TSPDrvInfoDataW): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDriverInfoW}
function SetupDiEnumDriverInfo(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverType: DWORD; MemberIndex: DWORD;
  var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiEnumDriverInfo}

function SetupDiGetSelectedDriverA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetSelectedDriverA}
function SetupDiGetSelectedDriverW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetSelectedDriverW}
function SetupDiGetSelectedDriver(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetSelectedDriver}

function SetupDiSetSelectedDriverA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverInfoData: PSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetSelectedDriverA}
function SetupDiSetSelectedDriverW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverInfoData: PSPDrvInfoDataW): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetSelectedDriverW}
function SetupDiSetSelectedDriver(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverInfoData: PSPDrvInfoDataA): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetSelectedDriver}

function SetupDiGetDriverInfoDetailA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  DriverInfoDetailData: PSPDrvInfoDetailDataA; DriverInfoDetailDataSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInfoDetailA}
function SetupDiGetDriverInfoDetailW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
  DriverInfoDetailData: PSPDrvInfoDetailDataW; DriverInfoDetailDataSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInfoDetailW}
function SetupDiGetDriverInfoDetail(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  DriverInfoDetailData: PSPDrvInfoDetailDataA; DriverInfoDetailDataSize: DWORD;
  RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInfoDetail}

function SetupDiDestroyDriverInfoList(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; DriverType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiDestroyDriverInfoList}

function SetupDiGetClassDevsA(ClassGuid: PGUID; const Enumerator: PAnsiChar;
  hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevsA}
function SetupDiGetClassDevsW(ClassGuid: PGUID; const Enumerator: PWideChar;
  hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevsW}
function SetupDiGetClassDevs(ClassGuid: PGUID; const Enumerator: PChar;
  hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevs}

function SetupDiGetClassDevsExA(ClassGuid: PGUID; const Enumerator: PAnsiChar;
  hwndParent: HWND; Flags: DWORD; DeviceInfoSet: HDEVINFO; const MachineName: PAnsiChar;
  Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevsExA}
function SetupDiGetClassDevsExW(ClassGuid: PGUID; const Enumerator: PWideChar;
  hwndParent: HWND; Flags: DWORD; DeviceInfoSet: HDEVINFO; const MachineName: PWideChar;
  Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevsExW}
function SetupDiGetClassDevsEx(ClassGuid: PGUID; const Enumerator: PChar;
  hwndParent: HWND; Flags: DWORD; DeviceInfoSet: HDEVINFO; const MachineName: PChar;
  Reserved: Pointer): HDEVINFO; stdcall;
{$EXTERNALSYM SetupDiGetClassDevsEx}

function SetupDiGetINFClassA(const InfName: PAnsiChar; var ClassGuid: TGUID;
  ClassName: PAnsiChar; ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetINFClassA}
function SetupDiGetINFClassW(const InfName: PWideChar; var ClassGuid: TGUID;
  ClassName: PWideChar; ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetINFClassW}
function SetupDiGetINFClass(const InfName: PChar; var ClassGuid: TGUID;
  ClassName: PChar; ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetINFClass}

function SetupDiBuildClassInfoList(Flags: DWORD; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiBuildClassInfoList}

function SetupDiBuildClassInfoListExA(Flags: DWORD; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PAnsiChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiBuildClassInfoListExA}
function SetupDiBuildClassInfoListExW(Flags: DWORD; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PWideChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiBuildClassInfoListExW}
function SetupDiBuildClassInfoListEx(Flags: DWORD; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiBuildClassInfoListEx}

function SetupDiGetClassDescriptionA(var ClassGuid: TGUID; ClassDescription: PAnsiChar;
  ClassDescriptionSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescriptionA}
function SetupDiGetClassDescriptionW(var ClassGuid: TGUID; ClassDescription: PWideChar;
  ClassDescriptionSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescriptionW}
function SetupDiGetClassDescription(var ClassGuid: TGUID; ClassDescription: PChar;
  ClassDescriptionSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescription}

function SetupDiGetClassDescriptionExA(var ClassGuid: TGUID;
  ClassDescription: PAnsiChar; ClassDescriptionSize: DWORD; var RequiredSize: DWORD;
  const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescriptionExA}
function SetupDiGetClassDescriptionExW(var ClassGuid: TGUID;
  ClassDescription: PWideChar; ClassDescriptionSize: DWORD; var RequiredSize: DWORD;
  const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescriptionExW}
function SetupDiGetClassDescriptionEx(var ClassGuid: TGUID;
  ClassDescription: PChar; ClassDescriptionSize: DWORD; var RequiredSize: DWORD;
  const MachineName: PChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDescriptionEx}

function SetupDiCallClassInstaller(InstallFunction: DI_FUNCTION;
  DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiCallClassInstaller}

//
// Default install handler for DIF_SELECTDEVICE
//

function SetupDiSelectDevice(DeviceInfoSet:  HDEVINFO;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiSelectDevice}

//
// Default install handler for DIF_SELECTBESTCOMPATDRV
//

function SetupDiSelectBestCompatDrv(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiSelectBestCompatDrv}

//
// Default install handler for DIF_INSTALLDEVICE
//
function SetupDiInstallDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallDevice}

//
// Default install handler for DIF_INSTALLDEVICEFILES
//

function SetupDiInstallDriverFiles(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallDriverFiles}

//
// Default install handler for DIF_REGISTER_COINSTALLERS
//
function SetupDiRegisterCoDeviceInstallers(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiRegisterCoDeviceInstallers}

//
// Default install handler for DIF_REMOVE
//

function SetupDiRemoveDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiRemoveDevice}

//
// Default install handler for DIF_UNREMOVE
//

function SetupDiUnremoveDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiUnremoveDevice}

//
// Default install handler for DIF_MOVEDEVICE
//
function SetupDiMoveDuplicateDevice(DeviceInfoSet: HDEVINFO;
  var DestinationDeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiMoveDuplicateDevice}

//
// Default install handler for DIF_PROPERTYCHANGE
//
function SetupDiChangeState(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiChangeState}

function SetupDiInstallClassA(hwndParent: HWND; const InfFileName: PAnsiChar;
  Flags: DWORD; FileQueue: HSPFILEQ): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClassA}
function SetupDiInstallClassW(hwndParent: HWND; const InfFileName: PWideChar;
  Flags: DWORD; FileQueue: HSPFILEQ): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClassW}
function SetupDiInstallClass(hwndParent: HWND; const InfFileName: PChar;
  Flags: DWORD; FileQueue: HSPFILEQ): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClass}

function SetupDiInstallClassExA(hwndParent: HWND; const InfFileName: PAnsiChar;
  Flags: DWORD; FileQueue: HSPFILEQ; InterfaceClassGuid: PGUID; Reserved1,
  Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClassExA}
function SetupDiInstallClassExW(hwndParent: HWND; const InfFileName: PWideChar;
  Flags: DWORD; FileQueue: HSPFILEQ; InterfaceClassGuid: PGUID; Reserved1,
  Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClassExW}
function SetupDiInstallClassEx(hwndParent: HWND; const InfFileName: PChar;
  Flags: DWORD; FileQueue: HSPFILEQ; InterfaceClassGuid: PGUID; Reserved1,
  Reserved2: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiInstallClassEx}

function SetupDiOpenClassRegKey(ClassGuid: PGUID; samDesired: REGSAM): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenClassRegKey}

function SetupDiOpenClassRegKeyExA(ClassGuid: PGUID; samDesired: REGSAM;
  Flags: DWORD; const MachineName: PAnsiChar; Reserved: Pointer): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenClassRegKeyExA}
function SetupDiOpenClassRegKeyExW(ClassGuid: PGUID; samDesired: REGSAM;
  Flags: DWORD; const MachineName: PWideChar; Reserved: Pointer): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenClassRegKeyExW}
function SetupDiOpenClassRegKeyEx(ClassGuid: PGUID; samDesired: REGSAM;
  Flags: DWORD; const MachineName: PChar; Reserved: Pointer): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenClassRegKeyEx}

function SetupDiCreateDeviceInterfaceRegKeyA(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterfaceRegKeyA}
function SetupDiCreateDeviceInterfaceRegKeyW(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterfaceRegKeyW}
function SetupDiCreateDeviceInterfaceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDeviceInterfaceRegKey}

//
// Backward compatibility--do not use.
//

function SetupDiCreateInterfaceDeviceRegKeyA(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDeviceRegKeyA}
function SetupDiCreateInterfaceDeviceRegKeyW(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDeviceRegKeyW}
function SetupDiCreateInterfaceDeviceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateInterfaceDeviceRegKey}

function SetupDiOpenDeviceInterfaceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenDeviceInterfaceRegKey}

//
// Backward compatibility--do not use.
//

function SetupDiOpenInterfaceDeviceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
  samDesired: REGSAM): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenInterfaceDeviceRegKey}

function SetupDiDeleteDeviceInterfaceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteDeviceInterfaceRegKey}

//
// Backward compatibility--do not use.
//

function SetupDiDeleteInterfaceDeviceRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteInterfaceDeviceRegKey}

function SetupDiCreateDevRegKeyA(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
  InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDevRegKeyA}

function SetupDiCreateDevRegKeyW(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
  InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDevRegKeyW}

function SetupDiCreateDevRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
  InfHandle: HINF; const InfSectionName: PChar): HKEY; stdcall;
{$EXTERNALSYM SetupDiCreateDevRegKey}


function SetupDiOpenDevRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
  samDesired: REGSAM): HKEY; stdcall;
{$EXTERNALSYM SetupDiOpenDevRegKey}

function SetupDiDeleteDevRegKey(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Scope, HwProfile,
  KeyType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiDeleteDevRegKey}

function SetupDiGetHwProfileList(HwProfileList: PDWORD; HwProfileListSize: DWORD;
  var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileList}

function SetupDiGetHwProfileListExA(HwProfileList: PDWORD;
  HwProfileListSize: DWORD; var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD;
  const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileListExA}
function SetupDiGetHwProfileListExW(HwProfileList: PDWORD;
  HwProfileListSize: DWORD; var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD;
  const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileListExW}
function SetupDiGetHwProfileListEx(HwProfileList: PDWORD;
  HwProfileListSize: DWORD; var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD;
  const MachineName: PChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileListEx}

function SetupDiGetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceRegistryPropertyA}
function SetupDiGetDeviceRegistryPropertyW(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceRegistryPropertyW}
function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceRegistryProperty}

{$IFDEF WIN2000}
function SetupDiGetClassRegistryPropertyA(const ClassGuid: TGUID;
  Property_: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassRegistryPropertyA}
function SetupDiGetClassRegistryPropertyW(const ClassGuid: TGUID;
  Property_: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassRegistryPropertyW}
function SetupDiGetClassRegistryProperty(const ClassGuid: TGUID;
  Property_: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD; const MachineName: PChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassRegistryProperty}
{$ENDIF WIN2000}

function SetupDiSetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceRegistryPropertyA}
function SetupDiSetDeviceRegistryPropertyW(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceRegistryPropertyW}
function SetupDiSetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData; Property_: DWORD;
  const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceRegistryProperty}

{$IFDEF WIN2000}
function SetupDiSetClassRegistryPropertyA(const ClassGuid: TGUID;
  Property_: DWORD; const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassRegistryPropertyA}
function SetupDiSetClassRegistryPropertyW(const ClassGuid: TGUID;
  Property_: DWORD; const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassRegistryPropertyW}
function SetupDiSetClassRegistryProperty(const ClassGuid: TGUID;
  Property_: DWORD; const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
  const MachineName: PChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassRegistryProperty}
{$ENDIF WIN2000}

function SetupDiGetDeviceInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstallParamsA}

function SetupDiGetDeviceInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsW): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstallParamsW}

function SetupDiGetDeviceInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDeviceInstallParams}


function SetupDiGetClassInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassInstallParamsA}
function SetupDiGetClassInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassInstallParamsW}
function SetupDiGetClassInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassInstallParams}

function SetupDiSetDeviceInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceInstallParamsA}
function SetupDiSetDeviceInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsW): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceInstallParamsW}
function SetupDiSetDeviceInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData;
  var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDeviceInstallParams}

function SetupDiSetClassInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassInstallParamsA}
function SetupDiSetClassInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassInstallParamsW}
function SetupDiSetClassInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
  ClassInstallParamsSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetClassInstallParams}

function SetupDiGetDriverInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInstallParamsA}
function SetupDiGetDriverInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInstallParamsW}
function SetupDiGetDriverInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetDriverInstallParams}

function SetupDiSetDriverInstallParamsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDriverInstallParamsA}
function SetupDiSetDriverInstallParamsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDriverInstallParamsW}
function SetupDiSetDriverInstallParams(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
  var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetDriverInstallParams}

function SetupDiLoadClassIcon(var ClassGuid: TGUID; LargeIcon: PHICON;
  MiniIconIndex: PINT): BOOL; stdcall;
{$EXTERNALSYM SetupDiLoadClassIcon}

function SetupDiDrawMiniIcon(hdc: HDC; rc: TRect; MiniIconIndex: Integer;
  Flags: DWORD): Integer; stdcall;
{$EXTERNALSYM SetupDiDrawMiniIcon}

function SetupDiGetClassBitmapIndex(ClassGuid: PGUID;
  var MiniIconIndex: Integer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassBitmapIndex}

function SetupDiGetClassImageList(
  var ClassImageListData: TSPClassImageListData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassImageList}

function SetupDiGetClassImageListExA(var ClassImageListData: TSPClassImageListData;
  const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassImageListExA}
function SetupDiGetClassImageListExW(var ClassImageListData: TSPClassImageListData;
  const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassImageListExW}
function SetupDiGetClassImageListEx(var ClassImageListData: TSPClassImageListData;
  const MachineName: PChar; Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassImageListEx}

function SetupDiGetClassImageIndex(var ClassImageListData: TSPClassImageListData;
  var ClassGuid: TGUID; var ImageIndex: Integer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassImageIndex}

function SetupDiDestroyClassImageList(
  var ClassImageListData: TSPClassImageListData): BOOL; stdcall;
{$EXTERNALSYM SetupDiDestroyClassImageList}

function SetupDiGetClassDevPropertySheetsA(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var PropertySheetHeader: TPropSheetHeaderA;
  PropertySheetHeaderPageListSize: DWORD; RequiredSize: PDWORD;
  PropertySheetType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDevPropertySheetsA}
function SetupDiGetClassDevPropertySheetsW(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var PropertySheetHeader: TPropSheetHeaderW;
  PropertySheetHeaderPageListSize: DWORD; RequiredSize: PDWORD;
  PropertySheetType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDevPropertySheetsW}
function SetupDiGetClassDevPropertySheets(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var PropertySheetHeader: TPropSheetHeaderA;
  PropertySheetHeaderPageListSize: DWORD; RequiredSize: PDWORD;
  PropertySheetType: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetClassDevPropertySheets}

function SetupDiAskForOEMDisk(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiAskForOEMDisk}

function SetupDiSelectOEMDrv(hwndParent: HWND; DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiSelectOEMDrv}

function SetupDiClassNameFromGuidA(var ClassGuid: TGUID; ClassName: PAnsiChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuidA}
function SetupDiClassNameFromGuidW(var ClassGuid: TGUID; ClassName: PWideChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuidW}
function SetupDiClassNameFromGuid(var ClassGuid: TGUID; ClassName: PChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuid}

function SetupDiClassNameFromGuidExA(var ClassGuid: TGUID; ClassName: PAnsiChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuidExA}
function SetupDiClassNameFromGuidExW(var ClassGuid: TGUID; ClassName: PWideChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuidExW}
function SetupDiClassNameFromGuidEx(var ClassGuid: TGUID; ClassName: PChar;
  ClassNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassNameFromGuidEx}

function SetupDiClassGuidsFromNameA(const ClassName: PAnsiChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromNameA}
function SetupDiClassGuidsFromNameW(const ClassName: PWideChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromNameW}
function SetupDiClassGuidsFromName(const ClassName: PChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromName}

function SetupDiClassGuidsFromNameExA(const ClassName: PAnsiChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PAnsiChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromNameExA}
function SetupDiClassGuidsFromNameExW(const ClassName: PWideChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PWideChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromNameExW}
function SetupDiClassGuidsFromNameEx(const ClassName: PChar; ClassGuidList: PGUID;
  ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiClassGuidsFromNameEx}

function SetupDiGetHwProfileFriendlyNameA(HwProfile: DWORD; FriendlyName: PAnsiChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyNameA}
function SetupDiGetHwProfileFriendlyNameW(HwProfile: DWORD; FriendlyName: PWideChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyNameW}
function SetupDiGetHwProfileFriendlyName(HwProfile: DWORD; FriendlyName: PChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyName}

function SetupDiGetHwProfileFriendlyNameExA(HwProfile: DWORD; FriendlyName: PAnsiChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyNameExA}
function SetupDiGetHwProfileFriendlyNameExW(HwProfile: DWORD; FriendlyName: PWideChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyNameExW}
function SetupDiGetHwProfileFriendlyNameEx(HwProfile: DWORD; FriendlyName: PChar;
  FriendlyNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PChar;
  Reserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetHwProfileFriendlyNameEx}

function SetupDiGetWizardPage(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; var InstallWizardData: TSPInstallWizardData;
  PageType: DWORD; Flags: DWORD): HPROPSHEETPAGE; stdcall;
{$EXTERNALSYM SetupDiGetWizardPage}

function SetupDiGetSelectedDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetSelectedDevice}

function SetupDiSetSelectedDevice(DeviceInfoSet: HDEVINFO;
  var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
{$EXTERNALSYM SetupDiSetSelectedDevice}

function SetupDiGetActualSectionToInstallA(InfHandle: HINF;
  const InfSectionName: PAnsiChar; InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPASTR): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetActualSectionToInstallA}
function SetupDiGetActualSectionToInstallW(InfHandle: HINF;
  const InfSectionName: PWideChar; InfSectionWithExt: PWideChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPWSTR): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetActualSectionToInstallW}
function SetupDiGetActualSectionToInstall(InfHandle: HINF;
  const InfSectionName: PChar; InfSectionWithExt: PChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPSTR): BOOL; stdcall;
{$EXTERNALSYM SetupDiGetActualSectionToInstall}

{$IFDEF WINXP}
function SetupDiGetActualSectionToInstallExA(InfHandle: HINF;
  InfSectionName: PAnsiChar; AlternatePlatformInfo: PSPAltPlatformInfo;
  InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPAnsiChar; Reserved: Pointer): BOOL; stdcall;
function SetupDiGetActualSectionToInstallExW(InfHandle: HINF;
  InfSectionName: PWideChar; AlternatePlatformInfo: PSPAltPlatformInfo;
  InfSectionWithExt: PWideChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPWideChar; Reserved: Pointer): BOOL; stdcall;
function SetupDiGetActualSectionToInstallEx(InfHandle: HINF;
  InfSectionName: PAnsiChar; AlternatePlatformInfo: PSPAltPlatformInfo;
  InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
  RequiredSize: PDWORD; Extension: PPAnsiChar; Reserved: Pointer): BOOL; stdcall;

//
// SetupEnumInfSections is for low-level parsing of an INF
//
function SetupEnumInfSectionsA(InfHandle: HINF; Index: UINT;
  Buffer: PAnsiChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;
function SetupEnumInfSectionsW(InfHandle: HINF; Index: UINT;
  Buffer: PWideChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;
function SetupEnumInfSections(InfHandle: HINF; Index: UINT;
  Buffer: PAnsiChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;

function SetupVerifyInfFileA(InfName: PAnsiChar; AltPlatformInfo: PSPAltPlatformInfo;
  var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;
function SetupVerifyInfFileW(InfName: PWideChar; AltPlatformInfo: PSPAltPlatformInfo;
  var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;
function SetupVerifyInfFile(InfName: PAnsiChar; AltPlatformInfo: PSPAltPlatformInfo;
  var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;

function SetupDiGetCustomDevicePropertyA(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PAnsiChar;
  Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
function SetupDiGetCustomDevicePropertyW(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PWideChar;
  Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
function SetupDiGetCustomDeviceProperty(DeviceInfoSet: HDEVINFO;
  const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PAnsiChar;
  Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
{$ENDIF WINXP}

{$ELSE}

// (rom) remove all #defines Microsoft generated in SetupApi.h
// (rom) to handle A/W functions

(*$HPPEMIT '#undef SetupGetInfInformation'*)
(*$HPPEMIT '#undef SetupQueryInfFileInformation'*)
(*$HPPEMIT '#undef SetupQueryInfOriginalFileInformation'*)
(*$HPPEMIT '#undef SetupQueryInfVersionInformation'*)
(*$HPPEMIT '#undef SetupGetInfFileList'*)
(*$HPPEMIT '#undef SetupOpenInfFile'*)
(*$HPPEMIT '#undef SetupOpenAppendInfFile'*)
(*$HPPEMIT '#undef SetupFindFirstLine'*)
(*$HPPEMIT '#undef SetupFindNextMatchLine'*)
(*$HPPEMIT '#undef SetupGetLineByIndex'*)
(*$HPPEMIT '#undef SetupGetLineCount'*)
(*$HPPEMIT '#undef SetupGetLineText'*)
(*$HPPEMIT '#undef SetupGetStringField'*)
(*$HPPEMIT '#undef SetupGetMultiSzField'*)
(*$HPPEMIT '#undef SetupGetFileCompressionInfo'*)
{$IFDEF WINXP}
(*$HPPEMIT '#undef SetupGetFileCompressionInfoEx'*)
{$ENDIF WINXP}
(*$HPPEMIT '#undef SetupDecompressOrCopyFile'*)
(*$HPPEMIT '#undef SetupGetSourceFileLocation'*)
(*$HPPEMIT '#undef SetupGetSourceFileSize'*)
(*$HPPEMIT '#undef SetupGetTargetPath'*)
(*$HPPEMIT '#undef SetupSetSourceList'*)
(*$HPPEMIT '#undef SetupAddToSourceList'*)
(*$HPPEMIT '#undef SetupRemoveFromSourceList'*)
(*$HPPEMIT '#undef SetupQuerySourceList'*)
(*$HPPEMIT '#undef SetupFreeSourceList'*)
(*$HPPEMIT '#undef SetupPromptForDisk'*)
(*$HPPEMIT '#undef SetupCopyError'*)
(*$HPPEMIT '#undef SetupRenameError'*)
(*$HPPEMIT '#undef SetupDeleteError'*)
(*$HPPEMIT '#undef SetupBackupError'*)
(*$HPPEMIT '#undef SetupSetDirectoryId'*)
(*$HPPEMIT '#undef SetupSetDirectoryIdEx'*)
(*$HPPEMIT '#undef SetupGetSourceInfo'*)
(*$HPPEMIT '#undef SetupInstallFile'*)
(*$HPPEMIT '#undef SetupInstallFileEx'*)
(*$HPPEMIT '#undef SetupSetFileQueueAlternatePlatform'*)
(*$HPPEMIT '#undef SetupSetPlatformPathOverride'*)
(*$HPPEMIT '#undef SetupQueueCopy'*)
(*$HPPEMIT '#undef SetupQueueCopyIndirect'*)
(*$HPPEMIT '#undef SetupQueueDefaultCopy'*)
(*$HPPEMIT '#undef SetupQueueCopySection'*)
(*$HPPEMIT '#undef SetupQueueDelete'*)
(*$HPPEMIT '#undef SetupQueueDeleteSection'*)
(*$HPPEMIT '#undef SetupQueueRename'*)
(*$HPPEMIT '#undef SetupQueueRenameSection'*)
(*$HPPEMIT '#undef SetupCommitFileQueue'*)
(*$HPPEMIT '#undef SetupScanFileQueue'*)
(*$HPPEMIT '#undef SetupCopyOEMInf'*)
{$IFDEF WINXP}
(*$HPPEMIT '#undef SetupUninstallOEMInf'*)
{$ENDIF WINXP}
(*$HPPEMIT '#undef SetupCreateDiskSpaceList'*)
(*$HPPEMIT '#undef SetupDuplicateDiskSpaceList'*)
(*$HPPEMIT '#undef SetupQueryDrivesInDiskSpaceList'*)
(*$HPPEMIT '#undef SetupQuerySpaceRequiredOnDrive'*)
(*$HPPEMIT '#undef SetupAdjustDiskSpaceList'*)
(*$HPPEMIT '#undef SetupAddToDiskSpaceList'*)
(*$HPPEMIT '#undef SetupAddSectionToDiskSpaceList'*)
(*$HPPEMIT '#undef SetupAddInstallSectionToDiskSpaceList'*)
(*$HPPEMIT '#undef SetupRemoveFromDiskSpaceList'*)
(*$HPPEMIT '#undef SetupRemoveSectionFromDiskSpaceList'*)
(*$HPPEMIT '#undef SetupRemoveInstallSectionFromDiskSpaceList'*)
(*$HPPEMIT '#undef SetupIterateCabinet'*)
(*$HPPEMIT '#undef SetupDefaultQueueCallback'*)
(*$HPPEMIT '#undef SetupInstallFromInfSection'*)
(*$HPPEMIT '#undef SetupInstallFilesFromInfSection'*)
(*$HPPEMIT '#undef SetupInstallServicesFromInfSection'*)
(*$HPPEMIT '#undef SetupInstallServicesFromInfSectionEx'*)
{$IFDEF WINXP}
(*$HPPEMIT '#undef InstallHinfSection'*)
{$ENDIF WINXP}
(*$HPPEMIT '#undef SetupInitializeFileLog'*)
(*$HPPEMIT '#undef SetupLogFile'*)
(*$HPPEMIT '#undef SetupRemoveFileLogEntry'*)
(*$HPPEMIT '#undef SetupQueryFileLog'*)
(*$HPPEMIT '#undef SetupLogError'*)
(*$HPPEMIT '#undef SetupGetBackupInformation'*)
{$IFDEF WINXP}
(*$HPPEMIT '#undef SetupPrepareQueueForRestore'*)
{$ENDIF WINXP}
(*$HPPEMIT '#undef SetupDiCreateDeviceInfoListEx'*)
(*$HPPEMIT '#undef SetupDiGetDeviceInfoListDetail'*)
(*$HPPEMIT '#undef SetupDiCreateDeviceInfo'*)
(*$HPPEMIT '#undef SetupDiOpenDeviceInfo'*)
(*$HPPEMIT '#undef SetupDiGetDeviceInstanceId'*)
(*$HPPEMIT '#undef SetupDiEnumInterfaceDevice'*)
(*$HPPEMIT '#undef SetupDiCreateDeviceInterface'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDeviceA'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDeviceW'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDevice'*)
(*$HPPEMIT '#undef SetupDiOpenDeviceInterface'*)
(*$HPPEMIT '#undef SetupDiOpenInterfaceDeviceA'*)
(*$HPPEMIT '#undef SetupDiOpenInterfaceDeviceW'*)
(*$HPPEMIT '#undef SetupDiOpenInterfaceDevice'*)
(*$HPPEMIT '#undef SetupDiGetInterfaceDeviceAlias'*)
(*$HPPEMIT '#undef SetupDiDeleteInterfaceDeviceData'*)
(*$HPPEMIT '#undef SetupDiRemoveInterfaceDevice'*)
(*$HPPEMIT '#undef SetupDiGetDeviceInterfaceDetail'*)
(*$HPPEMIT '#undef SetupDiGetInterfaceDeviceDetailA'*)
(*$HPPEMIT '#undef SetupDiGetInterfaceDeviceDetailW'*)
(*$HPPEMIT '#undef SetupDiGetInterfaceDeviceDetail'*)
(*$HPPEMIT '#undef SetupDiInstallInterfaceDevices'*)
(*$HPPEMIT '#undef SetupDiEnumDriverInfo'*)
(*$HPPEMIT '#undef SetupDiGetSelectedDriver'*)
(*$HPPEMIT '#undef SetupDiSetSelectedDriver'*)
(*$HPPEMIT '#undef SetupDiGetDriverInfoDetail'*)
(*$HPPEMIT '#undef SetupDiGetClassDevs'*)
(*$HPPEMIT '#undef SetupDiGetClassDevsEx'*)
(*$HPPEMIT '#undef SetupDiGetINFClass'*)
(*$HPPEMIT '#undef SetupDiBuildClassInfoListEx'*)
(*$HPPEMIT '#undef SetupDiGetClassDescription'*)
(*$HPPEMIT '#undef SetupDiGetClassDescriptionEx'*)
(*$HPPEMIT '#undef SetupDiInstallClass'*)
(*$HPPEMIT '#undef SetupDiInstallClassEx'*)
(*$HPPEMIT '#undef SetupDiOpenClassRegKeyEx'*)
(*$HPPEMIT '#undef SetupDiCreateDeviceInterfaceRegKey'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDeviceRegKeyA'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDeviceRegKeyW'*)
(*$HPPEMIT '#undef SetupDiCreateInterfaceDeviceRegKey'*)
(*$HPPEMIT '#undef SetupDiOpenInterfaceDeviceRegKey'*)
(*$HPPEMIT '#undef SetupDiDeleteInterfaceDeviceRegKey'*)
(*$HPPEMIT '#undef SetupDiCreateDevRegKey'*)
(*$HPPEMIT '#undef SetupDiGetHwProfileListEx'*)
(*$HPPEMIT '#undef SetupDiGetDeviceRegistryProperty'*)
(*$HPPEMIT '#undef SetupDiGetClassRegistryProperty'*)
(*$HPPEMIT '#undef SetupDiSetDeviceRegistryProperty'*)
(*$HPPEMIT '#undef SetupDiSetClassRegistryProperty'*)
(*$HPPEMIT '#undef SetupDiGetDeviceInstallParams'*)
(*$HPPEMIT '#undef SetupDiGetClassInstallParams'*)
(*$HPPEMIT '#undef SetupDiSetDeviceInstallParams'*)
(*$HPPEMIT '#undef SetupDiSetClassInstallParams'*)
(*$HPPEMIT '#undef SetupDiGetDriverInstallParams'*)
(*$HPPEMIT '#undef SetupDiSetDriverInstallParams'*)
(*$HPPEMIT '#undef SetupDiGetClassImageListEx'*)
(*$HPPEMIT '#undef SetupDiGetClassDevPropertySheets'*)
(*$HPPEMIT '#undef SetupDiClassNameFromGuid'*)
(*$HPPEMIT '#undef SetupDiClassNameFromGuidEx'*)
(*$HPPEMIT '#undef SetupDiClassGuidsFromName'*)
(*$HPPEMIT '#undef SetupDiClassGuidsFromNameEx'*)
(*$HPPEMIT '#undef SetupDiGetHwProfileFriendlyName'*)
(*$HPPEMIT '#undef SetupDiGetHwProfileFriendlyNameEx'*)
(*$HPPEMIT '#undef SetupDiGetActualSectionToInstall'*)
{$IFDEF WINXP}
(*$HPPEMIT '#undef SetupDiGetActualSectionToInstallEx'*)
(*$HPPEMIT '#undef SetupEnumInfSections'*)
(*$HPPEMIT '#undef SetupVerifyInfFile'*)
(*$HPPEMIT '#undef SetupDiGetCustomDeviceProperty'*)
{$ENDIF WINXP}

type
  {$IFDEF WINXP}
  TSetupGetFileQueueCount = function(FileQueue: HSPFILEQ; SubQueueFileOp: UINT; var NumOperations: UINT): BOOL; stdcall;
  TSetupGetFileQueueFlags = function(FileQueue: HSPFILEQ; var Flags: DWORD): BOOL; stdcall;
  TSetupSetFileQueueFlags = function(FileQueue: HSPFILEQ; FlagMask: DWORD; Flags: DWORD): BOOL; stdcall;
  {$ENDIF WINXP}
  TSetupGetInfInformationA = function(InfSpec: Pointer; SearchControl: DWORD;
    ReturnBuffer: PSPInfInformation; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetInfInformationW = function(InfSpec: Pointer; SearchControl: DWORD;
    ReturnBuffer: PSPInfInformation; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetInfInformation = TSetupGetInfInformationA;

  TSetupQueryInfFileInformationA = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryInfFileInformationW = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryInfFileInformation = TSetupQueryInfFileInformationA;

  {$IFDEF WIN2000}
  TSetupQueryInfOriginalFileInformationA = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; AlternatePlatformInfo: PSPAltPlatformInfo;
    var OriginalFileInfo: TSPOriginalFileInfoA): BOOL; stdcall;
  TSetupQueryInfOriginalFileInformationW = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; AlternatePlatformInfo: PSPAltPlatformInfo;
    var OriginalFileInfo: TSPOriginalFileInfoW): BOOL; stdcall;
  TSetupQueryInfOriginalFileInformation = TSetupQueryInfOriginalFileInformationA;
  {$ENDIF WIN2000}

  TSetupQueryInfVersionInformationA = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; const Key, ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryInfVersionInformationW = function(var InfInformation: TSPInfInformation;
    InfIndex: UINT; const Key, ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryInfVersionInformation = TSetupQueryInfVersionInformationA;

  TSetupGetInfFileListA = function(const DirectoryPath: PAnsiChar; InfStyle: DWORD;
    ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetInfFileListW = function(const DirectoryPath: PWideChar; InfStyle: DWORD;
    ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetInfFileList = TSetupGetInfFileListA;

  TSetupOpenInfFileA = function(const FileName: PAnsiChar; const InfClass: PAnsiChar;
    InfStyle: DWORD; ErrorLine: PUINT): HINF; stdcall;
  TSetupOpenInfFileW = function(const FileName: PWideChar; const InfClass: PWideChar;
    InfStyle: DWORD; ErrorLine: PUINT): HINF; stdcall;
  TSetupOpenInfFile = TSetupOpenInfFileA;

  TSetupOpenMasterInf = function: HINF; stdcall;

  TSetupOpenAppendInfFileA = function(const FileName: PAnsiChar; InfHandle: HINF;
    ErrorLine: PUINT): BOOL; stdcall;
  TSetupOpenAppendInfFileW = function(const FileName: PWideChar; InfHandle: HINF;
    ErrorLine: PUINT): BOOL; stdcall;
  TSetupOpenAppendInfFile = TSetupOpenAppendInfFileA;

  TSetupCloseInfFile = procedure(InfHandle: HINF); stdcall;

  TSetupFindFirstLineA = function(InfHandle: HINF; Section, Key: PAnsiChar;
    var Context: TInfContext): BOOL; stdcall;
  TSetupFindFirstLineW = function(InfHandle: HINF; Section, Key: PWideChar;
    var Context: TInfContext): BOOL; stdcall;
  TSetupFindFirstLine = TSetupFindFirstLineA;

  TSetupFindNextLine = function(var ContextIn, ContextOut: TInfContext): BOOL; stdcall;

  TSetupFindNextMatchLineA = function(var ContextIn: TInfContext; Key: PAnsiChar;
    var ContextOut: TInfContext): BOOL; stdcall;
  TSetupFindNextMatchLineW = function(var ContextIn: TInfContext; Key: PWideChar;
    var ContextOut: TInfContext): BOOL; stdcall;
  TSetupFindNextMatchLine = TSetupFindNextMatchLineA;

  TSetupGetLineByIndexA = function(InfHandle: HINF; Section: PAnsiChar; Index: DWORD;
    var Context: TInfContext): BOOL; stdcall;
  TSetupGetLineByIndexW = function(InfHandle: HINF; Section: PWideChar; Index: DWORD;
    var Context: TInfContext): BOOL; stdcall;
  TSetupGetLineByIndex = TSetupGetLineByIndexA;

  TSetupGetLineCountA = function(InfHandle: HINF; Section: PAnsiChar): Integer; stdcall;
  TSetupGetLineCountW = function(InfHandle: HINF; Section: PWideChar): Integer; stdcall;
  TSetupGetLineCount = TSetupGetLineCountA;

  TSetupGetLineTextA = function(Context: PInfContext; InfHandle: HINF; Section,
    Key, ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetLineTextW = function(Context: PInfContext; InfHandle: HINF; Section,
    Key, ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetLineText = TSetupGetLineTextA;

  TSetupGetFieldCount = function(var Context: TInfContext): DWORD; stdcall;

  TSetupGetStringFieldA = function(var Context: TInfContext; FieldIndex: DWORD;
    ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetStringFieldW = function(var Context: TInfContext; FieldIndex: DWORD;
    ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetStringField = TSetupGetStringFieldA;

  TSetupGetIntField = function(var Context: TInfContext; FieldIndex: DWORD;
    var IntegerValue: Integer): BOOL; stdcall;

  TSetupGetMultiSzFieldA = function(var Context: TInfContext; FieldIndex: DWORD;
    ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetMultiSzFieldW = function(var Context: TInfContext; FieldIndex: DWORD;
    ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetMultiSzField = TSetupGetMultiSzFieldA;

  TSetupGetBinaryField = function(var Context: TInfContext; FieldIndex: DWORD;
    ReturnBuffer: PBYTE; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;

  TSetupGetFileCompressionInfoA = function(const SourceFileName: PAnsiChar;
    var ActualSourceFileName: PAnsiChar; var SourceFileSize: DWORD;
    var TargetFileSize: DWORD; var CompressionType: UINT): DWORD; stdcall;
  TSetupGetFileCompressionInfoW = function(const SourceFileName: PWideChar;
    var ActualSourceFileName: PWideChar; var SourceFileSize: DWORD;
    var TargetFileSize: DWORD; var CompressionType: UINT): DWORD; stdcall;
  TSetupGetFileCompressionInfo = TSetupGetFileCompressionInfoA;

  {$IFDEF WINXP}
  TSetupGetFileCompressionInfoExA = function(const SourceFileName: PAnsiChar;
    ActualSourceFileNameBuffer: PAnsiChar; var ActualSourceFileNameBufferLen: DWORD;
    RequiredBufferLen: PDWORD; var SourceFileSize: DWORD;
    var TargetFileSize: DWORD; var CompressionType: UINT): BOOL; stdcall;
  TSetupGetFileCompressionInfoExW = function(const SourceFileName: PWideChar;
    ActualSourceFileNameBuffer: PWideChar; var ActualSourceFileNameBufferLen: DWORD;
    RequiredBufferLen: PDWORD; var SourceFileSize: DWORD;
    var TargetFileSize: DWORD; var CompressionType: UINT): BOOL; stdcall;
  TSetupGetFileCompressionInfoEx = TSetupGetFileCompressionInfoExA;
  {$ENDIF WINXP}

  TSetupDecompressOrCopyFileA = function(const SourceFileName, TargetFileName: PAnsiChar;
    var CompressionType: UINT): DWORD; stdcall;
  TSetupDecompressOrCopyFileW = function(const SourceFileName, TargetFileName: PWideChar;
    var CompressionType: UINT): DWORD; stdcall;
  TSetupDecompressOrCopyFile = TSetupDecompressOrCopyFileA;

  TSetupGetSourceFileLocationA = function(InfHandle: HINF; InfContext: PInfContext;
    const FileName: PAnsiChar; var SourceId: UINT; ReturnBuffer: PAnsiChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetSourceFileLocationW = function(InfHandle: HINF; InfContext: PInfContext;
    const FileName: PWideChar; var SourceId: UINT; ReturnBuffer: PWideChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetSourceFileLocation = TSetupGetSourceFileLocationA;

  TSetupGetSourceFileSizeA = function(InfHandle: HINF; InfContext: PInfContext;
    const FileName: PAnsiChar; const Section: PAnsiChar; var FileSize: DWORD;
    RoundingFactor: UINT): BOOL; stdcall;
  TSetupGetSourceFileSizeW = function(InfHandle: HINF; InfContext: PInfContext;
    const FileName: PWideChar; const Section: PWideChar; var FileSize: DWORD;
    RoundingFactor: UINT): BOOL; stdcall;
  TSetupGetSourceFileSize = TSetupGetSourceFileSizeA;

  TSetupGetTargetPathA = function(InfHandle: HINF; InfContext: PInfContext;
    const Section: PAnsiChar; ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetTargetPathW = function(InfHandle: HINF; InfContext: PInfContext;
    const Section: PWideChar; ReturnBuffer: PWideChar; ReturnBufferSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetTargetPath = TSetupGetTargetPathA;

  TSetupSetSourceListA = function(Flags: DWORD; SourceList: PPASTR;
    SourceCount: UINT): BOOL; stdcall;
  TSetupSetSourceListW = function(Flags: DWORD; SourceList: PPWSTR;
    SourceCount: UINT): BOOL; stdcall;
  TSetupSetSourceList = function(Flags: DWORD; SourceList: PPSTR;
    SourceCount: UINT): BOOL; stdcall;

  TSetupCancelTemporarySourceList = function: BOOL; stdcall;

  TSetupAddToSourceListA = function(Flags: DWORD; const Source: PAnsiChar): BOOL; stdcall;
  TSetupAddToSourceListW = function(Flags: DWORD; const Source: PWideChar): BOOL; stdcall;
  TSetupAddToSourceList = TSetupAddToSourceListA;

  TSetupRemoveFromSourceListA = function(Flags: DWORD; const Source: PAnsiChar): BOOL; stdcall;
  TSetupRemoveFromSourceListW = function(Flags: DWORD; const Source: PWideChar): BOOL; stdcall;
  TSetupRemoveFromSourceList = TSetupRemoveFromSourceListA;

  TSetupQuerySourceListA = function(Flags: DWORD; var List: PPASTR;
    var Count: UINT): BOOL; stdcall;
  TSetupQuerySourceListW = function(Flags: DWORD; var List: PPWSTR;
    var Count: UINT): BOOL; stdcall;
  TSetupQuerySourceList = function(Flags: DWORD; var List: PPSTR;
    var Count: UINT): BOOL; stdcall;

  TSetupFreeSourceListA = function(var List: PPWSTR; Count: UINT): BOOL; stdcall;
  TSetupFreeSourceListW = function(var List: PPASTR; Count: UINT): BOOL; stdcall;
  TSetupFreeSourceList = function(var List: PPSTR; Count: UINT): BOOL; stdcall;

  TSetupPromptForDiskA = function(hwndParent: HWND; const DialogTitle, DiskName,
    PathToSource, FileSought, TagFile: PAnsiChar; DiskPromptStyle: DWORD;
    PathBuffer: PAnsiChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;
  TSetupPromptForDiskW = function(hwndParent: HWND; const DialogTitle, DiskName,
    PathToSource, FileSought, TagFile: PWideChar; DiskPromptStyle: DWORD;
    PathBuffer: PWideChar; PathBufferSize: DWORD; var PathRequiredSize: DWORD): UINT; stdcall;
  TSetupPromptForDisk = TSetupPromptForDiskA;

  TSetupCopyErrorA = function(hwndParent: HWND; const DialogTitle, DiskName,
    PathToSource, SourceFile, TargetPathFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD;
    PathBuffer: PAnsiChar; PathBufferSize: DWORD; PathRequiredSize: PDWORD): UINT; stdcall;
  TSetupCopyErrorW = function(hwndParent: HWND; const DialogTitle, DiskName,
    PathToSource, SourceFile, TargetPathFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD;
    PathBuffer: PWideChar; PathBufferSize: DWORD; PathRequiredSize: PDWORD): UINT; stdcall;
  TSetupCopyError = TSetupCopyErrorA;

  TSetupRenameErrorA = function(hwndParent: HWND; const DialogTitle, SourceFile,
    TargetFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupRenameErrorW = function(hwndParent: HWND; const DialogTitle, SourceFile,
    TargetFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupRenameError = TSetupRenameErrorA;

  TSetupDeleteErrorA = function(hwndParent: HWND; const DialogTitle, File_: PAnsiChar;
    Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupDeleteErrorW = function(hwndParent: HWND; const DialogTitle, File_: PWideChar;
    Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupDeleteError = TSetupDeleteErrorA;

  {$IFDEF WIN2000}
  TSetupBackupErrorA = function(hwndParent: HWND; const DialogTitle, BackupFile,
    TargetFile: PAnsiChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupBackupErrorW = function(hwndParent: HWND; const DialogTitle, BackupFile,
    TargetFile: PWideChar; Win32ErrorCode: UINT; Style: DWORD): UINT; stdcall;
  TSetupBackupError = TSetupBackupErrorA;
  {$ENDIF WIN2000}

  TSetupSetDirectoryIdA = function(InfHandle: HINF; Id: DWORD; const Directory: PAnsiChar): BOOL; stdcall;
  TSetupSetDirectoryIdW = function(InfHandle: HINF; Id: DWORD; const Directory: PWideChar): BOOL; stdcall;
  TSetupSetDirectoryId = TSetupSetDirectoryIdA;

  TSetupSetDirectoryIdExA = function(InfHandle: HINF; Id: DWORD; const Directory: PAnsiChar;
    Flags: DWORD; Reserved1: DWORD; Reserved2: Pointer): BOOL; stdcall;
  TSetupSetDirectoryIdExW = function(InfHandle: HINF; Id: DWORD; const Directory: PWideChar;
    Flags: DWORD; Reserved1: DWORD; Reserved2: Pointer): BOOL; stdcall;
  TSetupSetDirectoryIdEx = TSetupSetDirectoryIdExA;

  TSetupGetSourceInfoA = function(InfHandle: HINF; SourceId, InfoDesired: UINT;
    ReturnBuffer: PAnsiChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetSourceInfoW = function(InfHandle: HINF; SourceId, InfoDesired: UINT;
    ReturnBuffer: PWideChar; ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupGetSourceInfo = TSetupGetSourceInfoA;

  TSetupInstallFileA = function(InfHandle: HINF; InfContext: PInfContext;
    const SourceFile, SourcePathRoot, DestinationName: PAnsiChar; CopyStyle: DWORD;
    CopyMsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
  TSetupInstallFileW = function(InfHandle: HINF; InfContext: PInfContext;
    const SourceFile, SourcePathRoot, DestinationName: PWideChar; CopyStyle: DWORD;
    CopyMsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
  TSetupInstallFile = TSetupInstallFileA;

  TSetupInstallFileExA = function(InfHandle: HINF; InfContext: PInfContext;
    const SourceFile, SourcePathRoot, DestinationName: PAnsiChar; CopyStyle: DWORD;
    CopyMsgHandler: TSPFileCallbackA; Context: Pointer; var FileWasInUse: BOOL): BOOL; stdcall;
  TSetupInstallFileExW = function(InfHandle: HINF; InfContext: PInfContext;
    const SourceFile, SourcePathRoot, DestinationName: PWideChar; CopyStyle: DWORD;
    CopyMsgHandler: TSPFileCallbackW; Context: Pointer; var FileWasInUse: BOOL): BOOL; stdcall;
  TSetupInstallFileEx = TSetupInstallFileExA;

  TSetupOpenFileQueue = function: HSPFILEQ; stdcall;

  TSetupCloseFileQueue = function(QueueHandle: HSPFILEQ): BOOL; stdcall;

  {$IFDEF WIN2000}
  TSetupSetFileQueueAlternatePlatformA = function(QueueHandle: HSPFILEQ;
    AlternatePlatformInfo: PSPAltPlatformInfo;
    const AlternateDefaultCatalogFile: PAnsiChar): BOOL; stdcall;
  TSetupSetFileQueueAlternatePlatformW = function(QueueHandle: HSPFILEQ;
    AlternatePlatformInfo: PSPAltPlatformInfo;
    const AlternateDefaultCatalogFile: PWideChar): BOOL; stdcall;
  TSetupSetFileQueueAlternatePlatform = TSetupSetFileQueueAlternatePlatformA;
  {$ENDIF WIN2000}

  TSetupSetPlatformPathOverrideA = function(const Override_: PAnsiChar): BOOL; stdcall;
  TSetupSetPlatformPathOverrideW = function(const Override_: PWideChar): BOOL; stdcall;
  TSetupSetPlatformPathOverride = TSetupSetPlatformPathOverrideA;

  TSetupQueueCopyA = function(QueueHandle: HSPFILEQ; const SourceRootPath, SourcePath,
    SourceFilename, SourceDescription, SourceTagfile, TargetDirectory,
    TargetFilename: PAnsiChar; CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueCopyW = function(QueueHandle: HSPFILEQ; const SourceRootPath, SourcePath,
    SourceFilename, SourceDescription, SourceTagfile, TargetDirectory,
    TargetFilename: PWideChar; CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueCopy = TSetupQueueCopyA;

  {$IFDEF WIN2000}
  TSetupQueueCopyIndirectA = function(var CopyParams: TSPFileCopyParamsA): BOOL; stdcall;
  TSetupQueueCopyIndirectW = function(var CopyParams: TSPFileCopyParamsW): BOOL; stdcall;
  TSetupQueueCopyIndirect = TSetupQueueCopyIndirectA;
  {$ENDIF WIN2000}

  TSetupQueueDefaultCopyA = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    const SourceRootPath, SourceFilename, TargetFilename: PAnsiChar;
    CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueDefaultCopyW = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    const SourceRootPath, SourceFilename, TargetFilename: PWideChar;
    CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueDefaultCopy = TSetupQueueDefaultCopyA;

  TSetupQueueCopySectionA = function(QueueHandle: HSPFILEQ; const SourceRootPath: PAnsiChar;
    InfHandle: HINF; ListInfHandle: HINF; const Section: PAnsiChar; CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueCopySectionW = function(QueueHandle: HSPFILEQ; const SourceRootPath: PWideChar;
    InfHandle: HINF; ListInfHandle: HINF; const Section: PWideChar; CopyStyle: DWORD): BOOL; stdcall;
  TSetupQueueCopySection = TSetupQueueCopySectionA;

  TSetupQueueDeleteA = function(QueueHandle: HSPFILEQ; const PathPart1, PathPart2: PAnsiChar): BOOL; stdcall;
  TSetupQueueDeleteW = function(QueueHandle: HSPFILEQ; const PathPart1, PathPart2: PWideChar): BOOL; stdcall;
  TSetupQueueDelete = TSetupQueueDeleteA;

  TSetupQueueDeleteSectionA = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    ListInfHandle: HINF; const Section: PAnsiChar): BOOL; stdcall;
  TSetupQueueDeleteSectionW = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    ListInfHandle: HINF; const Section: PWideChar): BOOL; stdcall;
  TSetupQueueDeleteSection = TSetupQueueDeleteSectionA;

  TSetupQueueRenameA = function(QueueHandle: HSPFILEQ; const SourcePath,
    SourceFilename, TargetPath, TargetFilename: PAnsiChar): BOOL; stdcall;
  TSetupQueueRenameW = function(QueueHandle: HSPFILEQ; const SourcePath,
    SourceFilename, TargetPath, TargetFilename: PWideChar): BOOL; stdcall;
  TSetupQueueRename = TSetupQueueRenameA;

  TSetupQueueRenameSectionA = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    ListInfHandle: HINF; const Section: PAnsiChar): BOOL; stdcall;
  TSetupQueueRenameSectionW = function(QueueHandle: HSPFILEQ; InfHandle: HINF;
    ListInfHandle: HINF; const Section: PWideChar): BOOL; stdcall;
  TSetupQueueRenameSection = TSetupQueueRenameSectionA;

  TSetupCommitFileQueueA = function(Owner: HWND; QueueHandle: HSPFILEQ;
    MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
  TSetupCommitFileQueueW = function(Owner: HWND; QueueHandle: HSPFILEQ;
    MsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
  TSetupCommitFileQueue = TSetupCommitFileQueueA;

  TSetupScanFileQueueA = function(FileQueue: HSPFILEQ; Flags: DWORD; Window: HWND;
    CallbackRoutine: TSPFileCallbackA; CallbackContext: Pointer; var Result: DWORD): BOOL; stdcall;
  TSetupScanFileQueueW = function(FileQueue: HSPFILEQ; Flags: DWORD; Window: HWND;
    CallbackRoutine: TSPFileCallbackW; CallbackContext: Pointer; var Result: DWORD): BOOL; stdcall;
  TSetupScanFileQueue = TSetupScanFileQueueA;

  TSetupCopyOEMInfA = function(const SourceInfFileName, OEMSourceMediaLocation: PAnsiChar;
    OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PAnsiChar;
    DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
    DestinationInfFileNameComponent: PPASTR): BOOL; stdcall;
  TSetupCopyOEMInfW = function(const SourceInfFileName, OEMSourceMediaLocation: PWideChar;
    OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PWideChar;
    DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
    DestinationInfFileNameComponent: PPWSTR): BOOL; stdcall;
  TSetupCopyOEMInf = function(const SourceInfFileName, OEMSourceMediaLocation: PChar;
    OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: PChar;
    DestinationInfFileNameSize: DWORD; RequiredSize: PDWORD;
    DestinationInfFileNameComponent: PPSTR): BOOL; stdcall;

  {$IFDEF WINXP}
  TSetupUninstallOEMInfA = function(const InfFileName: PAnsiChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
  TSetupUninstallOEMInfW = function(const InfFileName: PWideChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
  TSetupUninstallOEMInf = function(const InfFileName: PAnsiChar; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
  TSetupUninstallNewlyCopiedInfs = function(FileQueue: HSPFILEQ; Flags: DWORD; Reserved: Pointer): BOOL; stdcall;
  {$ENDIF WINXP}

//
// Disk space list APIs
//
  TSetupCreateDiskSpaceListA = function(Reserved1: Pointer; Reserved2: DWORD;
    Flags: UINT): HDSKSPC; stdcall;
  TSetupCreateDiskSpaceListW = function(Reserved1: Pointer; Reserved2: DWORD;
    Flags: UINT): HDSKSPC; stdcall;
  TSetupCreateDiskSpaceList = TSetupCreateDiskSpaceListA;

  TSetupDuplicateDiskSpaceListA = function(DiskSpace: HDSKSPC; Reserved1: Pointer;
    Reserved2: DWORD; Flags: UINT): HDSKSPC; stdcall;
  TSetupDuplicateDiskSpaceListW = function(DiskSpace: HDSKSPC; Reserved1: Pointer;
    Reserved2: DWORD; Flags: UINT): HDSKSPC; stdcall;
  TSetupDuplicateDiskSpaceList = TSetupDuplicateDiskSpaceListA;

  TSetupDestroyDiskSpaceList = function(DiskSpace: HDSKSPC): BOOL; stdcall;

  TSetupQueryDrivesInDiskSpaceListA = function(DiskSpace: HDSKSPC; ReturnBuffer: PAnsiChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryDrivesInDiskSpaceListW = function(DiskSpace: HDSKSPC; ReturnBuffer: PWideChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryDrivesInDiskSpaceList = TSetupQueryDrivesInDiskSpaceListA;

  TSetupQuerySpaceRequiredOnDriveA = function(DiskSpace: HDSKSPC; const DriveSpec: PAnsiChar;
    var SpaceRequired: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupQuerySpaceRequiredOnDriveW = function(DiskSpace: HDSKSPC; const DriveSpec: PWideChar;
    var SpaceRequired: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupQuerySpaceRequiredOnDrive = TSetupQuerySpaceRequiredOnDriveA;

  TSetupAdjustDiskSpaceListA = function(DiskSpace: HDSKSPC; const DriveRoot: PAnsiChar;
    Amount: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAdjustDiskSpaceListW = function(DiskSpace: HDSKSPC; const DriveRoot: PWideChar;
    Amount: Int64; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAdjustDiskSpaceList = TSetupAdjustDiskSpaceListA;

  TSetupAddToDiskSpaceListA = function(DiskSpace: HDSKSPC; const TargetFilespec: PAnsiChar;
    FileSize: Int64; Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddToDiskSpaceListW = function(DiskSpace: HDSKSPC; const TargetFilespec: PWideChar;
    FileSize: Int64; Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddToDiskSpaceList = TSetupAddToDiskSpaceListA;

  TSetupAddSectionToDiskSpaceListA = function(DiskSpace: HDSKSPC; InfHandle: HINF;
    ListInfHandle: HINF; const SectionName: PAnsiChar; Operation: UINT;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddSectionToDiskSpaceListW = function(DiskSpace: HDSKSPC; InfHandle: HINF;
    ListInfHandle: HINF; const SectionName: PWideChar; Operation: UINT;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddSectionToDiskSpaceList = TSetupAddSectionToDiskSpaceListA;

  TSetupAddInstallSectionToDiskSpaceListA = function( DiskSpace: HDSKSPC;
    InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PAnsiChar;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddInstallSectionToDiskSpaceListW = function( DiskSpace: HDSKSPC;
    InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PWideChar;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupAddInstallSectionToDiskSpaceList = TSetupAddInstallSectionToDiskSpaceListA;

  TSetupRemoveFromDiskSpaceListA = function(DiskSpace: HDSKSPC; const TargetFilespec: PAnsiChar;
    Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveFromDiskSpaceListW = function(DiskSpace: HDSKSPC; const TargetFilespec: PWideChar;
    Operation: UINT; Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveFromDiskSpaceList = TSetupRemoveFromDiskSpaceListA;

  TSetupRemoveSectionFromDiskSpaceListA = function(DiskSpace: HDSKSPC; InfHandle: HINF;
    ListInfHandle: HINF; const SectionName: PAnsiChar; Operation: UINT; Reserved1: Pointer;
    Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveSectionFromDiskSpaceListW = function(DiskSpace: HDSKSPC; InfHandle: HINF;
    ListInfHandle: HINF; const SectionName: PWideChar; Operation: UINT; Reserved1: Pointer;
    Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveSectionFromDiskSpaceList = TSetupRemoveSectionFromDiskSpaceListA;

  TSetupRemoveInstallSectionFromDiskSpaceListA = function(DiskSpace: HDSKSPC;
    InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PAnsiChar;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveInstallSectionFromDiskSpaceListW = function(DiskSpace: HDSKSPC;
    InfHandle: HINF; LayoutInfHandle: HINF; const SectionName: PWideChar;
    Reserved1: Pointer; Reserved2: UINT): BOOL; stdcall;
  TSetupRemoveInstallSectionFromDiskSpaceList = TSetupRemoveInstallSectionFromDiskSpaceListA;

//
// Cabinet APIs
//

  TSetupIterateCabinetA = function(const CabinetFile: PAnsiChar; Reserved: DWORD;
    MsgHandler: TSPFileCallbackA; Context: Pointer): BOOL; stdcall;
  TSetupIterateCabinetW = function(const CabinetFile: PWideChar; Reserved: DWORD;
    MsgHandler: TSPFileCallbackW; Context: Pointer): BOOL; stdcall;
  TSetupIterateCabinet = TSetupIterateCabinetA;

  TSetupPromptReboot = function(FileQueue: HSPFILEQ; Owner: HWND; ScanOnly: BOOL): Integer; stdcall;

  TSetupInitDefaultQueueCallback = function(OwnerWindow: HWND): Pointer; stdcall;

  TSetupInitDefaultQueueCallbackEx = function(OwnerWindow: HWND; AlternateProgressWindow: HWND;
    ProgressMessage: UINT; Reserved1: DWORD; Reserved2: Pointer): Pointer; stdcall;

  TSetupTermDefaultQueueCallback = procedure(Context: Pointer); stdcall;

  TSetupDefaultQueueCallbackA = function(Context: Pointer; Notification: UINT;
    Param1, Param2: UINT_PTR): UINT; stdcall;
  TSetupDefaultQueueCallbackW = function(Context: Pointer; Notification: UINT;
    Param1, Param2: UINT_PTR): UINT; stdcall;
  TSetupDefaultQueueCallback = TSetupDefaultQueueCallbackA;

//
// The INF may supply any arbitrary data type ordinal in the highword except
// for the following: REG_NONE, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ.  If this
// technique is used, then the data is given in binary format, one byte per
// field.
//

  TSetupInstallFromInfSectionA = function(Owner: HWND; InfHandle: HINF;
    const SectionName: PAnsiChar; Flags: UINT; RelativeKeyRoot: HKEY;
    const SourceRootPath: PAnsiChar; CopyFlags: UINT; MsgHandler: TSPFileCallbackA;
    Context: Pointer; DeviceInfoSet: HDEVINFO; DeviceIn: PSPDevInfoData): BOOL; stdcall;
  TSetupInstallFromInfSectionW = function(Owner: HWND; InfHandle: HINF;
    const SectionName: PWideChar; Flags: UINT; RelativeKeyRoot: HKEY;
    const SourceRootPath: PWideChar; CopyFlags: UINT; MsgHandler: TSPFileCallbackW;
    Context: Pointer; DeviceInfoSet: HDEVINFO; DeviceIn: PSPDevInfoData): BOOL; stdcall;
  TSetupInstallFromInfSection = TSetupInstallFromInfSectionA;

  TSetupInstallFilesFromInfSectionA = function(InfHandle: HINF; LayoutInfHandle: HINF;
    FileQueue: HSPFILEQ; const SectionName, SourceRootPath: PAnsiChar;
    CopyFlags: UINT): BOOL; stdcall;
  TSetupInstallFilesFromInfSectionW = function(InfHandle: HINF; LayoutInfHandle: HINF;
    FileQueue: HSPFILEQ; const SectionName, SourceRootPath: PWideChar;
    CopyFlags: UINT): BOOL; stdcall;
  TSetupInstallFilesFromInfSection = TSetupInstallFilesFromInfSectionA;

  TSetupInstallServicesFromInfSectionA = function(InfHandle: HINF;
    const SectionName: PAnsiChar; Flags: DWORD): BOOL; stdcall;
  TSetupInstallServicesFromInfSectionW = function(InfHandle: HINF;
    const SectionName: PWideChar; Flags: DWORD): BOOL; stdcall;
  TSetupInstallServicesFromInfSection = TSetupInstallServicesFromInfSectionA;

  TSetupInstallServicesFromInfSectionExA = function(InfHandle: HINF;
    const SectionName: PAnsiChar; Flags: DWORD; DeviceInfoSet: HDEVINFO;
    DeviceInfoData: TSPDevInfoData; Reserved1, Reserved2: Pointer): BOOL; stdcall;
  TSetupInstallServicesFromInfSectionExW = function(InfHandle: HINF;
    const SectionName: PWideChar; Flags: DWORD; DeviceInfoSet: HDEVINFO;
    DeviceInfoData: TSPDevInfoData; Reserved1, Reserved2: Pointer): BOOL; stdcall;
  TSetupInstallServicesFromInfSectionEx = TSetupInstallServicesFromInfSectionExA;

  {$IFDEF WINXP}
  TInstallHinfSectionA = procedure(Window: HWND; ModuleHandle: HINSTANCE;
    CommandLine: PAnsiChar; ShowCommand: Integer); stdcall;
  TInstallHinfSectionW = procedure(Window: HWND; ModuleHandle: HINSTANCE;
    CommandLine: PWideChar; ShowCommand: Integer); stdcall;
  TInstallHinfSection = procedure (Window: HWND; ModuleHandle: HINSTANCE;
    CommandLine: PAnsiChar; ShowCommand: Integer); stdcall;
  {$ENDIF WINXP}

//
// Define handle type for Setup file log.
//

type
  HSPFILELOG = Pointer;
  {$EXTERNALSYM HSPFILELOG}

  TSetupInitializeFileLogA = function(const LogFileName: PAnsiChar; Flags: DWORD): HSPFILELOG; stdcall;
  TSetupInitializeFileLogW = function(const LogFileName: PWideChar; Flags: DWORD): HSPFILELOG; stdcall;
  TSetupInitializeFileLog = TSetupInitializeFileLogA;

  TSetupTerminateFileLog = function(FileLogHandle: HSPFILELOG): BOOL; stdcall;

  TSetupLogFileA = function(FileLogHandle: HSPFILELOG; const LogSectionName,
    SourceFilename, TargetFilename: PAnsiChar; Checksum: DWORD; DiskTagfile,
    DiskDescription, OtherInfo: PAnsiChar; Flags: DWORD): BOOL; stdcall;
  TSetupLogFileW = function(FileLogHandle: HSPFILELOG; const LogSectionName,
    SourceFilename, TargetFilename: PWideChar; Checksum: DWORD; DiskTagfile,
    DiskDescription, OtherInfo: PWideChar; Flags: DWORD): BOOL; stdcall;
  TSetupLogFile = TSetupLogFileA;

  TSetupRemoveFileLogEntryA = function(FileLogHandle: HSPFILELOG;
    const LogSectionName: PAnsiChar; const TargetFilename: PAnsiChar): BOOL; stdcall;
  TSetupRemoveFileLogEntryW = function(FileLogHandle: HSPFILELOG;
    const LogSectionName: PWideChar; const TargetFilename: PWideChar): BOOL; stdcall;
  TSetupRemoveFileLogEntry = TSetupRemoveFileLogEntryA;

  TSetupQueryFileLogA = function(FileLogHandle: HSPFILELOG; const LogSectionName,
    TargetFilename: PAnsiChar; DesiredInfo: SETUPFILELOGINFO; DataOut: PAnsiChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryFileLogW = function(FileLogHandle: HSPFILELOG; const LogSectionName,
    TargetFilename: PWideChar; DesiredInfo: SETUPFILELOGINFO; DataOut: PWideChar;
    ReturnBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupQueryFileLog = TSetupQueryFileLogA;

//
// Text logging APIs
//

  TSetupOpenLog = function(Erase: BOOL): BOOL; stdcall;

  TSetupLogErrorA = function(const MessageString: PAnsiChar; Severity: LOGSEVERITY): BOOL; stdcall;
  TSetupLogErrorW = function(const MessageString: PWideChar; Severity: LOGSEVERITY): BOOL; stdcall;
  TSetupLogError = TSetupLogErrorA;

  TSetupCloseLog = procedure; stdcall;

//
// Backup Information API
//

  {$IFDEF WIN2000}
  TSetupGetBackupInformationA = function(QueueHandle: HSPFILEQ;
    var BackupParams: TSPBackupQueueParamsA): BOOL; stdcall;
  TSetupGetBackupInformationW = function(QueueHandle: HSPFILEQ;
    var BackupParams: TSPBackupQueueParamsW): BOOL; stdcall;
  TSetupGetBackupInformation = TSetupGetBackupInformationA;
  {$ENDIF WIN2000}

  {$IFDEF WINXP}
  TSetupPrepareQueueForRestoreA = function(QueueHandle: HSPFILEQ;
    BackupPath: PAnsiChar; RestoreFlags: DWORD): BOOL; stdcall;
  TSetupPrepareQueueForRestoreW = function(QueueHandle: HSPFILEQ;
    BackupPath: PWideChar; RestoreFlags: DWORD): BOOL; stdcall;
  TSetupPrepareQueueForRestore = function(QueueHandle: HSPFILEQ;
    BackupPath: PAnsiChar; RestoreFlags: DWORD): BOOL; stdcall;

  TSetupSetNonInteractiveMode = function(NonInteractiveFlag: BOOL): BOOL; stdcall;
  TSetupGetNonInteractiveMode = function: BOOL; stdcall;
  {$ENDIF WINXP}

//
// Device Installer APIs
//

  TSetupDiCreateDeviceInfoList = function(ClassGuid: PGUID; hwndParent: HWND): HDEVINFO; stdcall;

  TSetupDiCreateDeviceInfoListExA = function(ClassGuid: PGUID; hwndParent: HWND;
    const MachineName: PAnsiChar; Reserved: Pointer): HDEVINFO; stdcall;
  TSetupDiCreateDeviceInfoListExW = function(ClassGuid: PGUID; hwndParent: HWND;
    const MachineName: PWideChar; Reserved: Pointer): HDEVINFO; stdcall;
  TSetupDiCreateDeviceInfoListEx = TSetupDiCreateDeviceInfoListExA;

  TSetupDiGetDeviceInfoListClass = function(DeviceInfoSet: HDEVINFO;
    var ClassGuid: TGUID): BOOL; stdcall;

  TSetupDiGetDeviceInfoListDetailA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoSetDetailData: TSPDevInfoListDetailDataA): BOOL; stdcall;
  TSetupDiGetDeviceInfoListDetailW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoSetDetailData: TSPDevInfoListDetailDataW): BOOL; stdcall;
  TSetupDiGetDeviceInfoListDetail = TSetupDiGetDeviceInfoListDetailA;

  TSetupDiCreateDeviceInfoA = function(DeviceInfoSet: HDEVINFO; const DeviceName: PAnsiChar;
    var ClassGuid: TGUID; const DeviceDescription: PAnsiChar; hwndParent: HWND;
    CreationFlags: DWORD; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiCreateDeviceInfoW = function(DeviceInfoSet: HDEVINFO; const DeviceName: PWideChar;
    var ClassGuid: TGUID; const DeviceDescription: PWideChar; hwndParent: HWND;
    CreationFlags: DWORD; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiCreateDeviceInfo = TSetupDiCreateDeviceInfoA;

  TSetupDiOpenDeviceInfoA = function(DeviceInfoSet: HDEVINFO;
    const DeviceInstanceId: PAnsiChar; hwndParent: HWND; OpenFlags: DWORD;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
  TSetupDiOpenDeviceInfoW = function(DeviceInfoSet: HDEVINFO;
    const DeviceInstanceId: PWideChar; hwndParent: HWND; OpenFlags: DWORD;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
  TSetupDiOpenDeviceInfo = TSetupDiOpenDeviceInfoA;

  TSetupDiGetDeviceInstanceIdA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DeviceInstanceId: PAnsiChar;
    DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetDeviceInstanceIdW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DeviceInstanceId: PWideChar;
    DeviceInstanceIdSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetDeviceInstanceId = TSetupDiGetDeviceInstanceIdA;

  TSetupDiDeleteDeviceInfo = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiEnumDeviceInfo = function(DeviceInfoSet: HDEVINFO;
    MemberIndex: DWORD; var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

  TSetupDiDestroyDeviceInfoList = function(DeviceInfoSet: HDEVINFO): BOOL; stdcall;

  TSetupDiEnumDeviceInterfaces = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
    MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

//
// Backward compatibility--do not use
//

  TSetupDiEnumInterfaceDevice = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var InterfaceClassGuid: TGUID;
    MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

  TSetupDiCreateDeviceInterfaceA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
    const ReferenceString: PAnsiChar; CreationFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiCreateDeviceInterfaceW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
    const ReferenceString: PWideChar; CreationFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiCreateDeviceInterface = TSetupDiCreateDeviceInterfaceA;

//
// Backward compatibility--do not use.
//

  TSetupDiCreateInterfaceDeviceA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
    const ReferenceString: PAnsiChar; CreationFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiCreateInterfaceDeviceW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
    const ReferenceString: PWideChar; CreationFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiCreateInterfaceDevice = TSetupDiCreateInterfaceDeviceA;

  TSetupDiOpenDeviceInterfaceA = function(DeviceInfoSet: HDEVINFO;
    const DevicePath: PAnsiChar; OpenFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiOpenDeviceInterfaceW = function(DeviceInfoSet: HDEVINFO;
    const DevicePath: PWideChar; OpenFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiOpenDeviceInterface = TSetupDiOpenDeviceInterfaceA;

//
// Backward compatibility--do not use
//

  TSetupDiOpenInterfaceDeviceA = function(DeviceInfoSet: HDEVINFO;
    const DevicePath: PAnsiChar; OpenFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiOpenInterfaceDeviceW = function(DeviceInfoSet: HDEVINFO;
    const DevicePath: PWideChar; OpenFlags: DWORD;
    DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
  TSetupDiOpenInterfaceDevice = TSetupDiOpenInterfaceDeviceA;

  TSetupDiGetDeviceInterfaceAlias = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; var AliasInterfaceClassGuid: TGUID;
    var AliasDeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiGetInterfaceDeviceAlias = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData;
    var AliasInterfaceClassGuid: TGUID;
    var AliasDeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

  TSetupDiDeleteDeviceInterfaceData = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiDeleteInterfaceDeviceData = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

  TSetupDiRemoveDeviceInterface = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiRemoveInterfaceDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

  TSetupDiGetDeviceInterfaceDetailA = function(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSPDeviceInterfaceData;
    DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
    DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
    Device: PSPDevInfoData): BOOL; stdcall;
  TSetupDiGetDeviceInterfaceDetailW = function(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSPDeviceInterfaceData;
    DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
    DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
    Device: PSPDevInfoData): BOOL; stdcall;
  TSetupDiGetDeviceInterfaceDetail = TSetupDiGetDeviceInterfaceDetailA;

//
// Backward compatibility--do not use.
//

  TSetupDiGetInterfaceDeviceDetailA = function(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSPDeviceInterfaceData;
    DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
    DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
    Device: PSPDevInfoData): BOOL; stdcall;
  TSetupDiGetInterfaceDeviceDetailW = function(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSPDeviceInterfaceData;
    DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
    DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD;
    Device: PSPDevInfoData): BOOL; stdcall;
  TSetupDiGetInterfaceDeviceDetail = TSetupDiGetInterfaceDeviceDetailA;

//
// Default install handler for DIF_INSTALLINTERFACES.
//

  TSetupDiInstallDeviceInterfaces = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiInstallInterfaceDevices = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

  {$IFDEF WINXP}
  TSetupDiSetDeviceInterfaceDefault = function(DeviceInfoSet: HDEVINFO
    var DeviceInterfaceData: TSPDeviceInterfaceData; Flags: DWORD;
    Reserved: Pointer): BOOL; stdcall;
  {$ENDIF WINXP}

//
// Default install handler for DIF_REGISTERDEVICE
//

  TSetupDiRegisterDeviceInfo = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Flags: DWORD; CompareProc: TSPDetSigCmpProc;
    CompareContext: Pointer; DupDeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiBuildDriverInfoList = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverType: DWORD): BOOL; stdcall;

  TSetupDiCancelDriverInfoSearch = function(DeviceInfoSet: HDEVINFO): BOOL; stdcall;

  TSetupDiEnumDriverInfoA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverType: DWORD; MemberIndex: DWORD;
    var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
  TSetupDiEnumDriverInfoW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverType: DWORD; MemberIndex: DWORD;
    var DriverInfoData: TSPDrvInfoDataW): BOOL; stdcall;
  TSetupDiEnumDriverInfo = TSetupDiEnumDriverInfoA;

  TSetupDiGetSelectedDriverA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA): BOOL; stdcall;
  TSetupDiGetSelectedDriverW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW): BOOL; stdcall;
  TSetupDiGetSelectedDriver = TSetupDiGetSelectedDriverA;

  TSetupDiSetSelectedDriverA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverInfoData: PSPDrvInfoDataA): BOOL; stdcall;
  TSetupDiSetSelectedDriverW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverInfoData: PSPDrvInfoDataW): BOOL; stdcall;
  TSetupDiSetSelectedDriver = TSetupDiSetSelectedDriverA;

  TSetupDiGetDriverInfoDetailA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
    DriverInfoDetailData: PSPDrvInfoDetailDataA; DriverInfoDetailDataSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetDriverInfoDetailW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
    DriverInfoDetailData: PSPDrvInfoDetailDataW; DriverInfoDetailDataSize: DWORD;
    RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetDriverInfoDetail = TSetupDiGetDriverInfoDetailA;

  TSetupDiDestroyDriverInfoList = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; DriverType: DWORD): BOOL; stdcall;

  TSetupDiGetClassDevsA = function(ClassGuid: PGUID; const Enumerator: PAnsiChar;
    hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
  TSetupDiGetClassDevsW = function(ClassGuid: PGUID; const Enumerator: PWideChar;
    hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
  TSetupDiGetClassDevs = TSetupDiGetClassDevsA;

  TSetupDiGetClassDevsExA = function(ClassGuid: PGUID; const Enumerator: PAnsiChar;
    hwndParent: HWND; Flags: DWORD; DeviceInfoSet: HDEVINFO; const MachineName: PAnsiChar;
    Reserved: Pointer): HDEVINFO; stdcall;
  TSetupDiGetClassDevsExW = function(ClassGuid: PGUID; const Enumerator: PWideChar;
    hwndParent: HWND; Flags: DWORD; DeviceInfoSet: HDEVINFO; const MachineName: PWideChar;
    Reserved: Pointer): HDEVINFO; stdcall;
  TSetupDiGetClassDevsEx = TSetupDiGetClassDevsExA;

  TSetupDiGetINFClassA = function(const InfName: PAnsiChar; var ClassGuid: TGUID;
    ClassName: PAnsiChar; ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetINFClassW = function(const InfName: PWideChar; var ClassGuid: TGUID;
    ClassName: PWideChar; ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetINFClass = TSetupDiGetINFClassA;

  TSetupDiBuildClassInfoList = function(Flags: DWORD; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;

  TSetupDiBuildClassInfoListExA = function(Flags: DWORD; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PAnsiChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiBuildClassInfoListExW = function(Flags: DWORD; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PWideChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiBuildClassInfoListEx = TSetupDiBuildClassInfoListExA;

  TSetupDiGetClassDescriptionA = function(var ClassGuid: TGUID; ClassDescription: PAnsiChar;
    ClassDescriptionSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetClassDescriptionW = function(var ClassGuid: TGUID; ClassDescription: PWideChar;
    ClassDescriptionSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetClassDescription = TSetupDiGetClassDescriptionA;

  TSetupDiGetClassDescriptionExA = function(var ClassGuid: TGUID;
    ClassDescription: PAnsiChar; ClassDescriptionSize: DWORD; var RequiredSize: DWORD;
    const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassDescriptionExW = function(var ClassGuid: TGUID;
    ClassDescription: PWideChar; ClassDescriptionSize: DWORD; var RequiredSize: DWORD;
    const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassDescriptionEx = TSetupDiGetClassDescriptionExA;

  TSetupDiCallClassInstaller = function(InstallFunction: DI_FUNCTION;
    DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_SELECTDEVICE
//

  TSetupDiSelectDevice = function(DeviceInfoSet:  HDEVINFO;
    DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_SELECTBESTCOMPATDRV
//

  TSetupDiSelectBestCompatDrv = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_INSTALLDEVICE
//
  TSetupDiInstallDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_INSTALLDEVICEFILES
//

  TSetupDiInstallDriverFiles = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_REGISTER_COINSTALLERS
//
  TSetupDiRegisterCoDeviceInstallers = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_REMOVE
//

  TSetupDiRemoveDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_UNREMOVE
//

  TSetupDiUnremoveDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_MOVEDEVICE
//
  TSetupDiMoveDuplicateDevice = function(DeviceInfoSet: HDEVINFO;
    var DestinationDeviceInfoData: TSPDevInfoData): BOOL; stdcall;

//
// Default install handler for DIF_PROPERTYCHANGE
//
  TSetupDiChangeState = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

  TSetupDiInstallClassA = function(hwndParent: HWND; const InfFileName: PAnsiChar;
    Flags: DWORD; FileQueue: HSPFILEQ): BOOL; stdcall;
  TSetupDiInstallClassW = function(hwndParent: HWND; const InfFileName: PWideChar;
    Flags: DWORD; FileQueue: HSPFILEQ): BOOL; stdcall;
  TSetupDiInstallClass = TSetupDiInstallClassA;

  TSetupDiInstallClassExA = function(hwndParent: HWND; const InfFileName: PAnsiChar;
    Flags: DWORD; FileQueue: HSPFILEQ; InterfaceClassGuid: PGUID; Reserved1,
    Reserved2: Pointer): BOOL; stdcall;
  TSetupDiInstallClassExW = function(hwndParent: HWND; const InfFileName: PWideChar;
    Flags: DWORD; FileQueue: HSPFILEQ; InterfaceClassGuid: PGUID; Reserved1,
    Reserved2: Pointer): BOOL; stdcall;
  TSetupDiInstallClassEx = TSetupDiInstallClassExA;

  TSetupDiOpenClassRegKey = function(ClassGuid: PGUID; samDesired: REGSAM): HKEY; stdcall;

  TSetupDiOpenClassRegKeyExA = function(ClassGuid: PGUID; samDesired: REGSAM;
    Flags: DWORD; const MachineName: PAnsiChar; Reserved: Pointer): HKEY; stdcall;
  TSetupDiOpenClassRegKeyExW = function(ClassGuid: PGUID; samDesired: REGSAM;
    Flags: DWORD; const MachineName: PWideChar; Reserved: Pointer): HKEY; stdcall;
  TSetupDiOpenClassRegKeyEx = TSetupDiOpenClassRegKeyExA;

  TSetupDiCreateDeviceInterfaceRegKeyA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;
  TSetupDiCreateDeviceInterfaceRegKeyW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;
  TSetupDiCreateDeviceInterfaceRegKey = TSetupDiCreateDeviceInterfaceRegKeyA;

//
// Backward compatibility--do not use.
//

  TSetupDiCreateInterfaceDeviceRegKeyA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;
  TSetupDiCreateInterfaceDeviceRegKeyW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM; InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;
  TSetupDiCreateInterfaceDeviceRegKey = TSetupDiCreateInterfaceDeviceRegKeyA;

  TSetupDiOpenDeviceInterfaceRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM): HKEY; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiOpenInterfaceDeviceRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD;
    samDesired: REGSAM): HKEY; stdcall;

  TSetupDiDeleteDeviceInterfaceRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD): BOOL; stdcall;

//
// Backward compatibility--do not use.
//

  TSetupDiDeleteInterfaceDeviceRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInterfaceData: TSPDeviceInterfaceData; Reserved: DWORD): BOOL; stdcall;

  TSetupDiCreateDevRegKeyA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
    InfHandle: HINF; const InfSectionName: PAnsiChar): HKEY; stdcall;

  TSetupDiCreateDevRegKeyW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
    InfHandle: HINF; const InfSectionName: PWideChar): HKEY; stdcall;

  TSetupDiCreateDevRegKey = TSetupDiCreateDevRegKeyA;

  TSetupDiOpenDevRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Scope, HwProfile, KeyType: DWORD;
    samDesired: REGSAM): HKEY; stdcall;

  TSetupDiDeleteDevRegKey = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Scope, HwProfile,
    KeyType: DWORD): BOOL; stdcall;

  TSetupDiGetHwProfileList = function(HwProfileList: PDWORD; HwProfileListSize: DWORD;
    var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD): BOOL; stdcall;

  TSetupDiGetHwProfileListExA = function(HwProfileList: PDWORD;
    HwProfileListSize: DWORD; var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD;
    const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetHwProfileListExW = function(HwProfileList: PDWORD;
    HwProfileListSize: DWORD; var RequiredSize: DWORD; CurrentlyActiveIndex: PDWORD;
    const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetHwProfileListEx = TSetupDiGetHwProfileListExA;

  TSetupDiGetDeviceRegistryPropertyA = function(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetDeviceRegistryPropertyW = function(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetDeviceRegistryProperty = TSetupDiGetDeviceRegistryPropertyA;

  {$IFDEF WIN2000}
  TSetupDiGetClassRegistryPropertyA = function(var ClassGuid: TGUID;
    Property_: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE;
    PropertyBufferSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassRegistryPropertyW = function(var ClassGuid: TGUID;
    Property_: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE;
    PropertyBufferSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassRegistryProperty = TSetupDiGetClassRegistryPropertyA;
  {$ENDIF WIN2000}

  TSetupDiSetDeviceRegistryPropertyA = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD): BOOL; stdcall;
  TSetupDiSetDeviceRegistryPropertyW = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD): BOOL; stdcall;
  TSetupDiSetDeviceRegistryProperty = TSetupDiSetDeviceRegistryPropertyA;

  {$IFDEF WIN2000}
  TSetupDiSetClassRegistryPropertyA = function(var ClassGuid: TGUID;
    Property_: DWORD; const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiSetClassRegistryPropertyW = function(var ClassGuid: TGUID;
    Property_: DWORD; const PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiSetClassRegistryProperty = TSetupDiSetClassRegistryPropertyA;
  {$ENDIF WIN2000}

  TSetupDiGetDeviceInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData;
    var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;

  TSetupDiGetDeviceInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData;
    var DeviceInstallParams: TSPDevInstallParamsW): BOOL; stdcall;

  TSetupDiGetDeviceInstallParams = TSetupDiGetDeviceInstallParamsA;

  TSetupDiGetClassInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
    ClassInstallParamsSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetClassInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
    ClassInstallParamsSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetClassInstallParams = TSetupDiGetClassInstallParamsA;

  TSetupDiSetDeviceInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData;
    var DeviceInstallParams: TSPDevInstallParamsA): BOOL; stdcall;
  TSetupDiSetDeviceInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData;
    var DeviceInstallParams: TSPDevInstallParamsW): BOOL; stdcall;
  TSetupDiSetDeviceInstallParams = TSetupDiSetDeviceInstallParamsA;

  TSetupDiSetClassInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
    ClassInstallParamsSize: DWORD): BOOL; stdcall;
  TSetupDiSetClassInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader;
    ClassInstallParamsSize: DWORD): BOOL; stdcall;
  TSetupDiSetClassInstallParams = TSetupDiSetClassInstallParamsA;

  TSetupDiGetDriverInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
    var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
  TSetupDiGetDriverInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
    var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
  TSetupDiGetDriverInstallParams = TSetupDiGetDriverInstallParamsA;

  TSetupDiSetDriverInstallParamsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataA;
    var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
  TSetupDiSetDriverInstallParamsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var DriverInfoData: TSPDrvInfoDataW;
    var DriverInstallParams: TSPDrvInstallParams): BOOL; stdcall;
  TSetupDiSetDriverInstallParams = TSetupDiSetDriverInstallParamsA;

  TSetupDiLoadClassIcon = function(var ClassGuid: TGUID; LargeIcon: PHICON;
    MiniIconIndex: PINT): BOOL; stdcall;

  TSetupDiDrawMiniIcon = function(hdc: HDC; rc: TRect; MiniIconIndex: Integer;
    Flags: DWORD): Integer; stdcall;

  TSetupDiGetClassBitmapIndex = function(ClassGuid: PGUID;
    var MiniIconIndex: Integer): BOOL; stdcall;

  TSetupDiGetClassImageList = function(
    var ClassImageListData: TSPClassImageListData): BOOL; stdcall;

  TSetupDiGetClassImageListExA = function(var ClassImageListData: TSPClassImageListData;
    const MachineName: PAnsiChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassImageListExW = function(var ClassImageListData: TSPClassImageListData;
    const MachineName: PWideChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetClassImageListEx = TSetupDiGetClassImageListExA;

  TSetupDiGetClassImageIndex = function(var ClassImageListData: TSPClassImageListData;
    var ClassGuid: TGUID; var ImageIndex: Integer): BOOL; stdcall;

  TSetupDiDestroyClassImageList = function(
    var ClassImageListData: TSPClassImageListData): BOOL; stdcall;

  TSetupDiGetClassDevPropertySheetsA = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var PropertySheetHeader: TPropSheetHeaderA;
    PropertySheetHeaderPageListSize: DWORD; RequiredSize: PDWORD;
    PropertySheetType: DWORD): BOOL; stdcall;
  TSetupDiGetClassDevPropertySheetsW = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var PropertySheetHeader: TPropSheetHeaderW;
    PropertySheetHeaderPageListSize: DWORD; RequiredSize: PDWORD;
    PropertySheetType: DWORD): BOOL; stdcall;
  TSetupDiGetClassDevPropertySheets = TSetupDiGetClassDevPropertySheetsA;

  TSetupDiAskForOEMDisk = function(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiSelectOEMDrv = function(hwndParent: HWND; DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData): BOOL; stdcall;

  TSetupDiClassNameFromGuidA = function(var ClassGuid: TGUID; ClassName: PAnsiChar;
    ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiClassNameFromGuidW = function(var ClassGuid: TGUID; ClassName: PWideChar;
    ClassNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiClassNameFromGuid = TSetupDiClassNameFromGuidA;

  TSetupDiClassNameFromGuidExA = function(var ClassGuid: TGUID; ClassName: PAnsiChar;
    ClassNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiClassNameFromGuidExW = function(var ClassGuid: TGUID; ClassName: PWideChar;
    ClassNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiClassNameFromGuidEx = TSetupDiClassNameFromGuidExA;

  TSetupDiClassGuidsFromNameA = function(const ClassName: PAnsiChar; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiClassGuidsFromNameW = function(const ClassName: PWideChar; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiClassGuidsFromName = TSetupDiClassGuidsFromNameA;

  TSetupDiClassGuidsFromNameExA = function(const ClassName: PAnsiChar; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PAnsiChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiClassGuidsFromNameExW = function(const ClassName: PWideChar; ClassGuidList: PGUID;
    ClassGuidListSize: DWORD; var RequiredSize: DWORD; const MachineName: PWideChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiClassGuidsFromNameEx = TSetupDiClassGuidsFromNameExA;

  TSetupDiGetHwProfileFriendlyNameA = function(HwProfile: DWORD; FriendlyName: PAnsiChar;
    FriendlyNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetHwProfileFriendlyNameW = function(HwProfile: DWORD; FriendlyName: PWideChar;
    FriendlyNameSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetHwProfileFriendlyName = TSetupDiGetHwProfileFriendlyNameA;

  TSetupDiGetHwProfileFriendlyNameExA = function(HwProfile: DWORD; FriendlyName: PAnsiChar;
    FriendlyNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PAnsiChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetHwProfileFriendlyNameExW = function(HwProfile: DWORD; FriendlyName: PWideChar;
    FriendlyNameSize: DWORD; RequiredSize: PDWORD; const MachineName: PWideChar;
    Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetHwProfileFriendlyNameEx = TSetupDiGetHwProfileFriendlyNameExA;

  TSetupDiGetWizardPage = function(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; var InstallWizardData: TSPInstallWizardData;
    PageType: DWORD; Flags: DWORD): HPROPSHEETPAGE; stdcall;

  TSetupDiGetSelectedDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

  TSetupDiSetSelectedDevice = function(DeviceInfoSet: HDEVINFO;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

  TSetupDiGetActualSectionToInstallA = function(InfHandle: HINF;
    const InfSectionName: PAnsiChar; InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPASTR): BOOL; stdcall;
  TSetupDiGetActualSectionToInstallW = function(InfHandle: HINF;
    const InfSectionName: PWideChar; InfSectionWithExt: PWideChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPWSTR): BOOL; stdcall;
  TSetupDiGetActualSectionToInstall = function(InfHandle: HINF;
    const InfSectionName: PChar; InfSectionWithExt: PChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPSTR): BOOL; stdcall;

  {$IFDEF WINXP}
  TSetupDiGetActualSectionToInstallExA = function(InfHandle: HINF;
    InfSectionName: PAnsiChar; AlternatePlatformInfo: PSPAltPlatformInfo;
    InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPAnsiChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetActualSectionToInstallExW = function(InfHandle: HINF;
    InfSectionName: PWideChar; AlternatePlatformInfo: PSPAltPlatformInfo;
    InfSectionWithExt: PWideChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPWideChar; Reserved: Pointer): BOOL; stdcall;
  TSetupDiGetActualSectionToInstallEx = function(InfHandle: HINF;
    InfSectionName: PAnsiChar; AlternatePlatformInfo: PSPAltPlatformInfo;
    InfSectionWithExt: PAnsiChar; InfSectionWithExtSize: DWORD;
    RequiredSize: PDWORD; Extension: PPAnsiChar; Reserved: Pointer): BOOL; stdcall;

  TSetupEnumInfSectionsA = function(InfHandle: HINF; Index: UINT;
    Buffer: PAnsiChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;
  TSetupEnumInfSectionsW = function(InfHandle: HINF; Index: UINT;
    Buffer: PWideChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;
  TSetupEnumInfSections = function(InfHandle: HINF; Index: UINT;
    Buffer: PAnsiChar; Size: UINT; SizeNeeded: PUINT): BOOL; stdcall;

  TSetupVerifyInfFileA = function(InfName: PAnsiChar; AltPlatformInfo: PSPAltPlatformInfo;
    var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;
  TSetupVerifyInfFileW = function(InfName: PWideChar; AltPlatformInfo: PSPAltPlatformInfo;
    var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;
  TSetupVerifyInfFile = function(InfName: PAnsiChar; AltPlatformInfo: PSPAltPlatformInfo;
    var InfSignerInfo: TSPInfSignerInfo): BOOL; stdcall;

  TSetupDiGetCustomDevicePropertyA = function(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PAnsiChar;
    Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
    PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetCustomDevicePropertyW = function(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PWideChar;
    Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
    PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  TSetupDiGetCustomDeviceProperty = function(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; CustomPropertyName: PAnsiChar;
    Flags: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PByte;
    PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall;
  {$ENDIF WINXP}

var
  {$IFDEF WINXP}
  SetupGetFileQueueCount: TSetupGetFileQueueCount;
  SetupGetFileQueueFlags: TSetupGetFileQueueFlags;
  SetupSetFileQueueFlags: TSetupSetFileQueueFlags;
  {$ENDIF WINXP}
  SetupGetInfInformationA: TSetupGetInfInformationA;
  SetupGetInfInformationW: TSetupGetInfInformationW;
  SetupGetInfInformation: TSetupGetInfInformation;
  SetupQueryInfFileInformationA: TSetupQueryInfFileInformationA;
  SetupQueryInfFileInformationW: TSetupQueryInfFileInformationW;
  SetupQueryInfFileInformation: TSetupQueryInfFileInformation;
  {$IFDEF WIN2000}
  SetupQueryInfOriginalFileInformationA: TSetupQueryInfOriginalFileInformationA;
  SetupQueryInfOriginalFileInformationW: TSetupQueryInfOriginalFileInformationW;
  SetupQueryInfOriginalFileInformation: TSetupQueryInfOriginalFileInformation;
  {$ENDIF WIN2000}
  SetupQueryInfVersionInformationA: TSetupQueryInfVersionInformationA;
  SetupQueryInfVersionInformationW: TSetupQueryInfVersionInformationW;
  SetupQueryInfVersionInformation: TSetupQueryInfVersionInformation;
  SetupGetInfFileListA: TSetupGetInfFileListA;
  SetupGetInfFileListW: TSetupGetInfFileListW;
  SetupGetInfFileList: TSetupGetInfFileList;
  SetupOpenInfFileA: TSetupOpenInfFileA;
  SetupOpenInfFileW: TSetupOpenInfFileW;
  SetupOpenInfFile: TSetupOpenInfFile;
  SetupOpenMasterInf: TSetupOpenMasterInf;
  SetupOpenAppendInfFileA: TSetupOpenAppendInfFileA;
  SetupOpenAppendInfFileW: TSetupOpenAppendInfFileW;
  SetupOpenAppendInfFile: TSetupOpenAppendInfFile;
  SetupCloseInfFile: TSetupCloseInfFile;
  SetupFindFirstLineA: TSetupFindFirstLineA;
  SetupFindFirstLineW: TSetupFindFirstLineW;
  SetupFindFirstLine: TSetupFindFirstLine;
  SetupFindNextLine: TSetupFindNextLine;
  SetupFindNextMatchLineA: TSetupFindNextMatchLineA;
  SetupFindNextMatchLineW: TSetupFindNextMatchLineW;
  SetupFindNextMatchLine: TSetupFindNextMatchLine;
  SetupGetLineByIndexA: TSetupGetLineByIndexA;
  SetupGetLineByIndexW: TSetupGetLineByIndexW;
  SetupGetLineByIndex: TSetupGetLineByIndex;
  SetupGetLineCountA: TSetupGetLineCountA;
  SetupGetLineCountW: TSetupGetLineCountW;
  SetupGetLineCount: TSetupGetLineCount;
  SetupGetLineTextA: TSetupGetLineTextA;
  SetupGetLineTextW: TSetupGetLineTextW;
  SetupGetLineText: TSetupGetLineText;
  SetupGetFieldCount: TSetupGetFieldCount;
  SetupGetStringFieldA: TSetupGetStringFieldA;
  SetupGetStringFieldW: TSetupGetStringFieldW;
  SetupGetStringField: TSetupGetStringField;
  SetupGetIntField: TSetupGetIntField;
  SetupGetMultiSzFieldA: TSetupGetMultiSzFieldA;
  SetupGetMultiSzFieldW: TSetupGetMultiSzFieldW;
  SetupGetMultiSzField: TSetupGetMultiSzField;
  SetupGetBinaryField: TSetupGetBinaryField;
  SetupGetFileCompressionInfoA: TSetupGetFileCompressionInfoA;
  SetupGetFileCompressionInfoW: TSetupGetFileCompressionInfoW;
  SetupGetFileCompressionInfo: TSetupGetFileCompressionInfo;
  {$IFDEF WINXP}
  SetupGetFileCompressionInfoExA: TSetupGetFileCompressionInfoExA;
  SetupGetFileCompressionInfoExW: TSetupGetFileCompressionInfoExW;
  SetupGetFileCompressionInfoEx: TSetupGetFileCompressionInfoEx;
  {$ENDIF WINXP}
  SetupDecompressOrCopyFileA: TSetupDecompressOrCopyFileA;
  SetupDecompressOrCopyFileW: TSetupDecompressOrCopyFileW;
  SetupDecompressOrCopyFile: TSetupDecompressOrCopyFile;
  SetupGetSourceFileLocationA: TSetupGetSourceFileLocationA;
  SetupGetSourceFileLocationW: TSetupGetSourceFileLocationW;
  SetupGetSourceFileLocation: TSetupGetSourceFileLocation;
  SetupGetSourceFileSizeA: TSetupGetSourceFileSizeA;
  SetupGetSourceFileSizeW: TSetupGetSourceFileSizeW;
  SetupGetSourceFileSize: TSetupGetSourceFileSize;
  SetupGetTargetPathA: TSetupGetTargetPathA;
  SetupGetTargetPathW: TSetupGetTargetPathW;
  SetupGetTargetPath: TSetupGetTargetPath;
  SetupSetSourceListA: TSetupSetSourceListA;
  SetupSetSourceListW: TSetupSetSourceListW;
  SetupSetSourceList: TSetupSetSourceList;
  SetupCancelTemporarySourceList: TSetupCancelTemporarySourceList;
  SetupAddToSourceListA: TSetupAddToSourceListA;
  SetupAddToSourceListW: TSetupAddToSourceListW;
  SetupAddToSourceList: TSetupAddToSourceList;
  SetupRemoveFromSourceListA: TSetupRemoveFromSourceListA;
  SetupRemoveFromSourceListW: TSetupRemoveFromSourceListW;
  SetupRemoveFromSourceList: TSetupRemoveFromSourceList;
  SetupQuerySourceListA: TSetupQuerySourceListA;
  SetupQuerySourceListW: TSetupQuerySourceListW;
  SetupQuerySourceList: TSetupQuerySourceList;
  SetupFreeSourceListA: TSetupFreeSourceListA;
  SetupFreeSourceListW: TSetupFreeSourceListW;
  SetupFreeSourceList: TSetupFreeSourceList;
  SetupPromptForDiskA: TSetupPromptForDiskA;
  SetupPromptForDiskW: TSetupPromptForDiskW;
  SetupPromptForDisk: TSetupPromptForDisk;
  SetupCopyErrorA: TSetupCopyErrorA;
  SetupCopyErrorW: TSetupCopyErrorW;
  SetupCopyError: TSetupCopyError;
  SetupRenameErrorA: TSetupRenameErrorA;
  SetupRenameErrorW: TSetupRenameErrorW;
  SetupRenameError: TSetupRenameError;
  SetupDeleteErrorA: TSetupDeleteErrorA;
  SetupDeleteErrorW: TSetupDeleteErrorW;
  SetupDeleteError: TSetupDeleteError;
  {$IFDEF WIN2000}
  SetupBackupErrorA: TSetupBackupErrorA;
  SetupBackupErrorW: TSetupBackupErrorW;
  SetupBackupError: TSetupBackupError;
  {$ENDIF WIN2000}
  SetupSetDirectoryIdA: TSetupSetDirectoryIdA;
  SetupSetDirectoryIdW: TSetupSetDirectoryIdW;
  SetupSetDirectoryId: TSetupSetDirectoryId;
  SetupSetDirectoryIdExA: TSetupSetDirectoryIdExA;
  SetupSetDirectoryIdExW: TSetupSetDirectoryIdExW;
  SetupSetDirectoryIdEx: TSetupSetDirectoryIdEx;
  SetupGetSourceInfoA: TSetupGetSourceInfoA;
  SetupGetSourceInfoW: TSetupGetSourceInfoW;
  SetupGetSourceInfo: TSetupGetSourceInfo;
  SetupInstallFileA: TSetupInstallFileA;
  SetupInstallFileW: TSetupInstallFileW;
  SetupInstallFile: TSetupInstallFile;
  SetupInstallFileExA: TSetupInstallFileExA;
  SetupInstallFileExW: TSetupInstallFileExW;
  SetupInstallFileEx: TSetupInstallFileEx;
  SetupOpenFileQueue: TSetupOpenFileQueue;
  SetupCloseFileQueue: TSetupCloseFileQueue;
  {$IFDEF WIN2000}
  SetupSetFileQueueAlternatePlatformA: TSetupSetFileQueueAlternatePlatformA;
  SetupSetFileQueueAlternatePlatformW: TSetupSetFileQueueAlternatePlatformW;
  SetupSetFileQueueAlternatePlatform: TSetupSetFileQueueAlternatePlatform;
  {$ENDIF WIN2000}
  SetupSetPlatformPathOverrideA: TSetupSetPlatformPathOverrideA;
  SetupSetPlatformPathOverrideW: TSetupSetPlatformPathOverrideW;
  SetupSetPlatformPathOverride: TSetupSetPlatformPathOverride;
  SetupQueueCopyA: TSetupQueueCopyA;
  SetupQueueCopyW: TSetupQueueCopyW;
  SetupQueueCopy: TSetupQueueCopy;
  {$IFDEF WIN2000}
  SetupQueueCopyIndirectA: TSetupQueueCopyIndirectA;
  SetupQueueCopyIndirectW: TSetupQueueCopyIndirectW;
  SetupQueueCopyIndirect: TSetupQueueCopyIndirect;
  {$ENDIF WIN2000}
  SetupQueueDefaultCopyA: TSetupQueueDefaultCopyA;
  SetupQueueDefaultCopyW: TSetupQueueDefaultCopyW;
  SetupQueueDefaultCopy: TSetupQueueDefaultCopy;
  SetupQueueCopySectionA: TSetupQueueCopySectionA;
  SetupQueueCopySectionW: TSetupQueueCopySectionW;
  SetupQueueCopySection: TSetupQueueCopySection;
  SetupQueueDeleteA: TSetupQueueDeleteA;
  SetupQueueDeleteW: TSetupQueueDeleteW;
  SetupQueueDelete: TSetupQueueDelete;
  SetupQueueDeleteSectionA: TSetupQueueDeleteSectionA;
  SetupQueueDeleteSectionW: TSetupQueueDeleteSectionW;
  SetupQueueDeleteSection: TSetupQueueDeleteSection;
  SetupQueueRenameA: TSetupQueueRenameA;
  SetupQueueRenameW: TSetupQueueRenameW;
  SetupQueueRename: TSetupQueueRename;
  SetupQueueRenameSectionA: TSetupQueueRenameSectionA;
  SetupQueueRenameSectionW: TSetupQueueRenameSectionW;
  SetupQueueRenameSection: TSetupQueueRenameSection;
  SetupCommitFileQueueA: TSetupCommitFileQueueA;
  SetupCommitFileQueueW: TSetupCommitFileQueueW;
  SetupCommitFileQueue: TSetupCommitFileQueue;
  SetupScanFileQueueA: TSetupScanFileQueueA;
  SetupScanFileQueueW: TSetupScanFileQueueW;
  SetupScanFileQueue: TSetupScanFileQueue;
  SetupCopyOEMInfA: TSetupCopyOEMInfA;
  SetupCopyOEMInfW: TSetupCopyOEMInfW;
  SetupCopyOEMInf: TSetupCopyOEMInf;
  {$IFDEF WINXP}
  SetupUninstallOEMInfA: TSetupUninstallOEMInfA;
  SetupUninstallOEMInfW: TSetupUninstallOEMInfW;
  SetupUninstallOEMInf: TSetupUninstallOEMInf;
  SetupUninstallNewlyCopiedInfs: TSetupUninstallNewlyCopiedInfs;
  {$ENDIF WINXP}
  SetupCreateDiskSpaceListA: TSetupCreateDiskSpaceListA;
  SetupCreateDiskSpaceListW: TSetupCreateDiskSpaceListW;
  SetupCreateDiskSpaceList: TSetupCreateDiskSpaceList;
  SetupDuplicateDiskSpaceListA: TSetupDuplicateDiskSpaceListA;
  SetupDuplicateDiskSpaceListW: TSetupDuplicateDiskSpaceListW;
  SetupDuplicateDiskSpaceList: TSetupDuplicateDiskSpaceList;
  SetupDestroyDiskSpaceList: TSetupDestroyDiskSpaceList;
  SetupQueryDrivesInDiskSpaceListA: TSetupQueryDrivesInDiskSpaceListA;
  SetupQueryDrivesInDiskSpaceListW: TSetupQueryDrivesInDiskSpaceListW;
  SetupQueryDrivesInDiskSpaceList: TSetupQueryDrivesInDiskSpaceList;
  SetupQuerySpaceRequiredOnDriveA: TSetupQuerySpaceRequiredOnDriveA;
  SetupQuerySpaceRequiredOnDriveW: TSetupQuerySpaceRequiredOnDriveW;
  SetupQuerySpaceRequiredOnDrive: TSetupQuerySpaceRequiredOnDrive;
  SetupAdjustDiskSpaceListA: TSetupAdjustDiskSpaceListA;
  SetupAdjustDiskSpaceListW: TSetupAdjustDiskSpaceListW;
  SetupAdjustDiskSpaceList: TSetupAdjustDiskSpaceList;
  SetupAddToDiskSpaceListA: TSetupAddToDiskSpaceListA;
  SetupAddToDiskSpaceListW: TSetupAddToDiskSpaceListW;
  SetupAddToDiskSpaceList: TSetupAddToDiskSpaceList;
  SetupAddSectionToDiskSpaceListA: TSetupAddSectionToDiskSpaceListA;
  SetupAddSectionToDiskSpaceListW: TSetupAddSectionToDiskSpaceListW;
  SetupAddSectionToDiskSpaceList: TSetupAddSectionToDiskSpaceList;
  SetupAddInstallSectionToDiskSpaceListA: TSetupAddInstallSectionToDiskSpaceListA;
  SetupAddInstallSectionToDiskSpaceListW: TSetupAddInstallSectionToDiskSpaceListW;
  SetupAddInstallSectionToDiskSpaceList: TSetupAddInstallSectionToDiskSpaceList;
  SetupRemoveFromDiskSpaceListA: TSetupRemoveFromDiskSpaceListA;
  SetupRemoveFromDiskSpaceListW: TSetupRemoveFromDiskSpaceListW;
  SetupRemoveFromDiskSpaceList: TSetupRemoveFromDiskSpaceList;
  SetupRemoveSectionFromDiskSpaceListA: TSetupRemoveSectionFromDiskSpaceListA;
  SetupRemoveSectionFromDiskSpaceListW: TSetupRemoveSectionFromDiskSpaceListW;
  SetupRemoveSectionFromDiskSpaceList: TSetupRemoveSectionFromDiskSpaceList;
  SetupRemoveInstallSectionFromDiskSpaceListA: TSetupRemoveInstallSectionFromDiskSpaceListA;
  SetupRemoveInstallSectionFromDiskSpaceListW: TSetupRemoveInstallSectionFromDiskSpaceListW;
  SetupRemoveInstallSectionFromDiskSpaceList: TSetupRemoveInstallSectionFromDiskSpaceList;
  SetupIterateCabinetA: TSetupIterateCabinetA;
  SetupIterateCabinetW: TSetupIterateCabinetW;
  SetupIterateCabinet: TSetupIterateCabinet;
  SetupPromptReboot: TSetupPromptReboot;
  SetupInitDefaultQueueCallback: TSetupInitDefaultQueueCallback;
  SetupInitDefaultQueueCallbackEx: TSetupInitDefaultQueueCallbackEx;
  SetupTermDefaultQueueCallback: TSetupTermDefaultQueueCallback;
  SetupDefaultQueueCallbackA: TSetupDefaultQueueCallbackA;
  SetupDefaultQueueCallbackW: TSetupDefaultQueueCallbackW;
  SetupDefaultQueueCallback: TSetupDefaultQueueCallback;
  SetupInstallFromInfSectionA: TSetupInstallFromInfSectionA;
  SetupInstallFromInfSectionW: TSetupInstallFromInfSectionW;
  SetupInstallFromInfSection: TSetupInstallFromInfSection;
  SetupInstallFilesFromInfSectionA: TSetupInstallFilesFromInfSectionA;
  SetupInstallFilesFromInfSectionW: TSetupInstallFilesFromInfSectionW;
  SetupInstallFilesFromInfSection: TSetupInstallFilesFromInfSection;
  SetupInstallServicesFromInfSectionA: TSetupInstallServicesFromInfSectionA;
  SetupInstallServicesFromInfSectionW: TSetupInstallServicesFromInfSectionW;
  SetupInstallServicesFromInfSection: TSetupInstallServicesFromInfSection;
  SetupInstallServicesFromInfSectionExA: TSetupInstallServicesFromInfSectionExA;
  SetupInstallServicesFromInfSectionExW: TSetupInstallServicesFromInfSectionExW;
  SetupInstallServicesFromInfSectionEx: TSetupInstallServicesFromInfSectionEx;
  {$IFDEF WINXP}
  InstallHinfSectionA: TInstallHinfSectionA;
  InstallHinfSectionW: TInstallHinfSectionW;
  InstallHinfSection: TInstallHinfSection;
  {$ENDIF WINXP}
  SetupInitializeFileLogA: TSetupInitializeFileLogA;
  SetupInitializeFileLogW: TSetupInitializeFileLogW;
  SetupInitializeFileLog: TSetupInitializeFileLog;
  SetupTerminateFileLog: TSetupTerminateFileLog;
  SetupLogFileA: TSetupLogFileA;
  SetupLogFileW: TSetupLogFileW;
  SetupLogFile: TSetupLogFile;
  SetupRemoveFileLogEntryA: TSetupRemoveFileLogEntryA;
  SetupRemoveFileLogEntryW: TSetupRemoveFileLogEntryW;
  SetupRemoveFileLogEntry: TSetupRemoveFileLogEntry;
  SetupQueryFileLogA: TSetupQueryFileLogA;
  SetupQueryFileLogW: TSetupQueryFileLogW;
  SetupQueryFileLog: TSetupQueryFileLog;
  SetupOpenLog: TSetupOpenLog;
  SetupLogErrorA: TSetupLogErrorA;
  SetupLogErrorW: TSetupLogErrorW;
  SetupLogError: TSetupLogError;
  SetupCloseLog: TSetupCloseLog;
  {$IFDEF WIN2000}
  SetupGetBackupInformationA: TSetupGetBackupInformationA;
  SetupGetBackupInformationW: TSetupGetBackupInformationW;
  SetupGetBackupInformation: TSetupGetBackupInformation;
  {$ENDIF WIN2000}
  {$IFDEF WINXP}
  SetupPrepareQueueForRestoreA: TSetupPrepareQueueForRestoreA;
  SetupPrepareQueueForRestoreW: TSetupPrepareQueueForRestoreW;
  SetupPrepareQueueForRestore: TSetupPrepareQueueForRestore;
  SetupSetNonInteractiveMode: TSetupSetNonInteractiveMode;
  SetupGetNonInteractiveMode: TSetupGetNonInteractiveMode;
  {$ENDIF WINXP}
  SetupDiCreateDeviceInfoList: TSetupDiCreateDeviceInfoList;
  SetupDiCreateDeviceInfoListExA: TSetupDiCreateDeviceInfoListExA;
  SetupDiCreateDeviceInfoListExW: TSetupDiCreateDeviceInfoListExW;
  SetupDiCreateDeviceInfoListEx: TSetupDiCreateDeviceInfoListEx;
  SetupDiGetDeviceInfoListClass: TSetupDiGetDeviceInfoListClass;
  SetupDiGetDeviceInfoListDetailA: TSetupDiGetDeviceInfoListDetailA;
  SetupDiGetDeviceInfoListDetailW: TSetupDiGetDeviceInfoListDetailW;
  SetupDiGetDeviceInfoListDetail: TSetupDiGetDeviceInfoListDetail;
  SetupDiCreateDeviceInfoA: TSetupDiCreateDeviceInfoA;
  SetupDiCreateDeviceInfoW: TSetupDiCreateDeviceInfoW;
  SetupDiCreateDeviceInfo: TSetupDiCreateDeviceInfo;
  SetupDiOpenDeviceInfoA: TSetupDiOpenDeviceInfoA;
  SetupDiOpenDeviceInfoW: TSetupDiOpenDeviceInfoW;
  SetupDiOpenDeviceInfo: TSetupDiOpenDeviceInfo;
  SetupDiGetDeviceInstanceIdA: TSetupDiGetDeviceInstanceIdA;
  SetupDiGetDeviceInstanceIdW: TSetupDiGetDeviceInstanceIdW;
  SetupDiGetDeviceInstanceId: TSetupDiGetDeviceInstanceId;
  SetupDiDeleteDeviceInfo: TSetupDiDeleteDeviceInfo;
  SetupDiEnumDeviceInfo: TSetupDiEnumDeviceInfo;
  SetupDiDestroyDeviceInfoList: TSetupDiDestroyDeviceInfoList;
  SetupDiEnumDeviceInterfaces: TSetupDiEnumDeviceInterfaces;
  SetupDiEnumInterfaceDevice: TSetupDiEnumDeviceInterfaces;
  SetupDiCreateDeviceInterfaceA: TSetupDiCreateDeviceInterfaceA;
  SetupDiCreateInterfaceDeviceA: TSetupDiCreateDeviceInterfaceA;
  SetupDiCreateDeviceInterfaceW: TSetupDiCreateDeviceInterfaceW;
  SetupDiCreateInterfaceDeviceW: TSetupDiCreateDeviceInterfaceW;
  SetupDiCreateDeviceInterface: TSetupDiCreateDeviceInterface;
  SetupDiOpenDeviceInterfaceA: TSetupDiOpenDeviceInterfaceA;
  SetupDiOpenInterfaceDeviceA: TSetupDiOpenDeviceInterfaceA;
  SetupDiOpenDeviceInterfaceW: TSetupDiOpenDeviceInterfaceW;
  SetupDiOpenInterfaceDeviceW: TSetupDiOpenDeviceInterfaceW;
  SetupDiOpenDeviceInterface: TSetupDiOpenDeviceInterface;
  SetupDiGetDeviceInterfaceAlias: TSetupDiGetDeviceInterfaceAlias;
  SetupDiGetInterfaceDeviceAlias: TSetupDiGetDeviceInterfaceAlias;
  SetupDiDeleteDeviceInterfaceData: TSetupDiDeleteDeviceInterfaceData;
  SetupDiDeleteInterfaceDeviceData: TSetupDiDeleteDeviceInterfaceData;
  SetupDiRemoveDeviceInterface: TSetupDiRemoveDeviceInterface;
  SetupDiRemoveInterfaceDevice: TSetupDiRemoveDeviceInterface;
  SetupDiGetDeviceInterfaceDetailA: TSetupDiGetDeviceInterfaceDetailA;
  SetupDiGetInterfaceDeviceDetailA: TSetupDiGetDeviceInterfaceDetailA;
  SetupDiGetDeviceInterfaceDetailW: TSetupDiGetDeviceInterfaceDetailW;
  SetupDiGetInterfaceDeviceDetailW: TSetupDiGetDeviceInterfaceDetailW;
  SetupDiGetDeviceInterfaceDetail: TSetupDiGetDeviceInterfaceDetail;
  SetupDiInstallDeviceInterfaces: TSetupDiInstallDeviceInterfaces;
  SetupDiInstallInterfaceDevices: TSetupDiInstallDeviceInterfaces;
  {$IFDEF WINXP}
  SetupDiSetDeviceInterfaceDefault: TSetupDiSetDeviceInterfaceDefault;
  {$ENDIF WINXP}
  SetupDiRegisterDeviceInfo: TSetupDiRegisterDeviceInfo;
  SetupDiBuildDriverInfoList: TSetupDiBuildDriverInfoList;
  SetupDiCancelDriverInfoSearch: TSetupDiCancelDriverInfoSearch;
  SetupDiEnumDriverInfoA: TSetupDiEnumDriverInfoA;
  SetupDiEnumDriverInfoW: TSetupDiEnumDriverInfoW;
  SetupDiEnumDriverInfo: TSetupDiEnumDriverInfo;
  SetupDiGetSelectedDriverA: TSetupDiGetSelectedDriverA;
  SetupDiGetSelectedDriverW: TSetupDiGetSelectedDriverW;
  SetupDiGetSelectedDriver: TSetupDiGetSelectedDriver;
  SetupDiSetSelectedDriverA: TSetupDiSetSelectedDriverA;
  SetupDiSetSelectedDriverW: TSetupDiSetSelectedDriverW;
  SetupDiSetSelectedDriver: TSetupDiSetSelectedDriver;
  SetupDiGetDriverInfoDetailA: TSetupDiGetDriverInfoDetailA;
  SetupDiGetDriverInfoDetailW: TSetupDiGetDriverInfoDetailW;
  SetupDiGetDriverInfoDetail: TSetupDiGetDriverInfoDetail;
  SetupDiDestroyDriverInfoList: TSetupDiDestroyDriverInfoList;
  SetupDiGetClassDevsA: TSetupDiGetClassDevsA;
  SetupDiGetClassDevsW: TSetupDiGetClassDevsW;
  SetupDiGetClassDevs: TSetupDiGetClassDevs;
  SetupDiGetClassDevsExA: TSetupDiGetClassDevsExA;
  SetupDiGetClassDevsExW: TSetupDiGetClassDevsExW;
  SetupDiGetClassDevsEx: TSetupDiGetClassDevsEx;
  SetupDiGetINFClassA: TSetupDiGetINFClassA;
  SetupDiGetINFClassW: TSetupDiGetINFClassW;
  SetupDiGetINFClass: TSetupDiGetINFClass;
  SetupDiBuildClassInfoList: TSetupDiBuildClassInfoList;
  SetupDiBuildClassInfoListExA: TSetupDiBuildClassInfoListExA;
  SetupDiBuildClassInfoListExW: TSetupDiBuildClassInfoListExW;
  SetupDiBuildClassInfoListEx: TSetupDiBuildClassInfoListEx;
  SetupDiGetClassDescriptionA: TSetupDiGetClassDescriptionA;
  SetupDiGetClassDescriptionW: TSetupDiGetClassDescriptionW;
  SetupDiGetClassDescription: TSetupDiGetClassDescription;
  SetupDiGetClassDescriptionExA: TSetupDiGetClassDescriptionExA;
  SetupDiGetClassDescriptionExW: TSetupDiGetClassDescriptionExW;
  SetupDiGetClassDescriptionEx: TSetupDiGetClassDescriptionEx;
  SetupDiCallClassInstaller: TSetupDiCallClassInstaller;
  SetupDiSelectDevice: TSetupDiSelectDevice;
  SetupDiSelectBestCompatDrv: TSetupDiSelectBestCompatDrv;
  SetupDiInstallDevice: TSetupDiInstallDevice;
  SetupDiInstallDriverFiles: TSetupDiInstallDriverFiles;
  SetupDiRegisterCoDeviceInstallers: TSetupDiRegisterCoDeviceInstallers;
  SetupDiRemoveDevice: TSetupDiRemoveDevice;
  SetupDiUnremoveDevice: TSetupDiUnremoveDevice;
  SetupDiMoveDuplicateDevice: TSetupDiMoveDuplicateDevice;
  SetupDiChangeState: TSetupDiChangeState;
  SetupDiInstallClassA: TSetupDiInstallClassA;
  SetupDiInstallClassW: TSetupDiInstallClassW;
  SetupDiInstallClass: TSetupDiInstallClass;
  SetupDiInstallClassExA: TSetupDiInstallClassExA;
  SetupDiInstallClassExW: TSetupDiInstallClassExW;
  SetupDiInstallClassEx: TSetupDiInstallClassEx;
  SetupDiOpenClassRegKey: TSetupDiOpenClassRegKey;
  SetupDiOpenClassRegKeyExA: TSetupDiOpenClassRegKeyExA;
  SetupDiOpenClassRegKeyExW: TSetupDiOpenClassRegKeyExW;
  SetupDiOpenClassRegKeyEx: TSetupDiOpenClassRegKeyEx;
  SetupDiCreateDeviceInterfaceRegKeyA: TSetupDiCreateDeviceInterfaceRegKeyA;
  SetupDiCreateInterfaceDeviceRegKeyA: TSetupDiCreateDeviceInterfaceRegKeyA;
  SetupDiCreateDeviceInterfaceRegKeyW: TSetupDiCreateDeviceInterfaceRegKeyW;
  SetupDiCreateInterfaceDeviceRegKeyW: TSetupDiCreateDeviceInterfaceRegKeyW;
  SetupDiCreateDeviceInterfaceRegKey: TSetupDiCreateDeviceInterfaceRegKey;
  SetupDiOpenDeviceInterfaceRegKey: TSetupDiOpenDeviceInterfaceRegKey;
  SetupDiOpenInterfaceDeviceRegKey: TSetupDiOpenDeviceInterfaceRegKey;
  SetupDiDeleteDeviceInterfaceRegKey: TSetupDiDeleteDeviceInterfaceRegKey;
  SetupDiDeleteInterfaceDeviceRegKey: TSetupDiDeleteDeviceInterfaceRegKey;
  SetupDiCreateDevRegKeyA: TSetupDiCreateDevRegKeyA;
  SetupDiCreateDevRegKeyW: TSetupDiCreateDevRegKeyW;
  SetupDiCreateDevRegKey: TSetupDiCreateDevRegKey;
  SetupDiOpenDevRegKey: TSetupDiOpenDevRegKey;
  SetupDiDeleteDevRegKey: TSetupDiDeleteDevRegKey;
  SetupDiGetHwProfileList: TSetupDiGetHwProfileList;
  SetupDiGetHwProfileListExA: TSetupDiGetHwProfileListExA;
  SetupDiGetHwProfileListExW: TSetupDiGetHwProfileListExW;
  SetupDiGetHwProfileListEx: TSetupDiGetHwProfileListEx;
  SetupDiGetDeviceRegistryPropertyA: TSetupDiGetDeviceRegistryPropertyA;
  SetupDiGetDeviceRegistryPropertyW: TSetupDiGetDeviceRegistryPropertyW;
  SetupDiGetDeviceRegistryProperty: TSetupDiGetDeviceRegistryProperty;
  {$IFDEF WIN2000}
  SetupDiGetClassRegistryPropertyA: TSetupDiGetClassRegistryPropertyA;
  SetupDiGetClassRegistryPropertyW: TSetupDiGetClassRegistryPropertyW;
  SetupDiGetClassRegistryProperty: TSetupDiGetClassRegistryProperty;
  {$ENDIF WIN2000}
  SetupDiSetDeviceRegistryPropertyA: TSetupDiSetDeviceRegistryPropertyA;
  SetupDiSetDeviceRegistryPropertyW: TSetupDiSetDeviceRegistryPropertyW;
  SetupDiSetDeviceRegistryProperty: TSetupDiSetDeviceRegistryProperty;
  {$IFDEF WIN2000}
  SetupDiSetClassRegistryPropertyA: TSetupDiSetClassRegistryPropertyA;
  SetupDiSetClassRegistryPropertyW: TSetupDiSetClassRegistryPropertyW;
  SetupDiSetClassRegistryProperty: TSetupDiSetClassRegistryProperty;
  {$ENDIF WIN2000}
  SetupDiGetDeviceInstallParamsA: TSetupDiGetDeviceInstallParamsA;
  SetupDiGetDeviceInstallParamsW: TSetupDiGetDeviceInstallParamsW;
  SetupDiGetDeviceInstallParams: TSetupDiGetDeviceInstallParams;
  SetupDiGetClassInstallParamsA: TSetupDiGetClassInstallParamsA;
  SetupDiGetClassInstallParamsW: TSetupDiGetClassInstallParamsW;
  SetupDiGetClassInstallParams: TSetupDiGetClassInstallParams;
  SetupDiSetDeviceInstallParamsA: TSetupDiSetDeviceInstallParamsA;
  SetupDiSetDeviceInstallParamsW: TSetupDiSetDeviceInstallParamsW;
  SetupDiSetDeviceInstallParams: TSetupDiSetDeviceInstallParams;
  SetupDiSetClassInstallParamsA: TSetupDiSetClassInstallParamsA;
  SetupDiSetClassInstallParamsW: TSetupDiSetClassInstallParamsW;
  SetupDiSetClassInstallParams: TSetupDiSetClassInstallParams;
  SetupDiGetDriverInstallParamsA: TSetupDiGetDriverInstallParamsA;
  SetupDiGetDriverInstallParamsW: TSetupDiGetDriverInstallParamsW;
  SetupDiGetDriverInstallParams: TSetupDiGetDriverInstallParams;
  SetupDiSetDriverInstallParamsA: TSetupDiSetDriverInstallParamsA;
  SetupDiSetDriverInstallParamsW: TSetupDiSetDriverInstallParamsW;
  SetupDiSetDriverInstallParams: TSetupDiSetDriverInstallParams;
  SetupDiLoadClassIcon: TSetupDiLoadClassIcon;
  SetupDiDrawMiniIcon: TSetupDiDrawMiniIcon;
  SetupDiGetClassBitmapIndex: TSetupDiGetClassBitmapIndex;
  SetupDiGetClassImageList: TSetupDiGetClassImageList;
  SetupDiGetClassImageListExA: TSetupDiGetClassImageListExA;
  SetupDiGetClassImageListExW: TSetupDiGetClassImageListExW;
  SetupDiGetClassImageListEx: TSetupDiGetClassImageListEx;
  SetupDiGetClassImageIndex: TSetupDiGetClassImageIndex;
  SetupDiDestroyClassImageList: TSetupDiDestroyClassImageList;
  SetupDiGetClassDevPropertySheetsA: TSetupDiGetClassDevPropertySheetsA;
  SetupDiGetClassDevPropertySheetsW: TSetupDiGetClassDevPropertySheetsW;
  SetupDiGetClassDevPropertySheets: TSetupDiGetClassDevPropertySheets;
  SetupDiAskForOEMDisk: TSetupDiAskForOEMDisk;
  SetupDiSelectOEMDrv: TSetupDiSelectOEMDrv;
  SetupDiClassNameFromGuidA: TSetupDiClassNameFromGuidA;
  SetupDiClassNameFromGuidW: TSetupDiClassNameFromGuidW;
  SetupDiClassNameFromGuid: TSetupDiClassNameFromGuid;
  SetupDiClassNameFromGuidExA: TSetupDiClassNameFromGuidExA;
  SetupDiClassNameFromGuidExW: TSetupDiClassNameFromGuidExW;
  SetupDiClassNameFromGuidEx: TSetupDiClassNameFromGuidEx;
  SetupDiClassGuidsFromNameA: TSetupDiClassGuidsFromNameA;
  SetupDiClassGuidsFromNameW: TSetupDiClassGuidsFromNameW;
  SetupDiClassGuidsFromName: TSetupDiClassGuidsFromName;
  SetupDiClassGuidsFromNameExA: TSetupDiClassGuidsFromNameExA;
  SetupDiClassGuidsFromNameExW: TSetupDiClassGuidsFromNameExW;
  SetupDiClassGuidsFromNameEx: TSetupDiClassGuidsFromNameEx;
  SetupDiGetHwProfileFriendlyNameA: TSetupDiGetHwProfileFriendlyNameA;
  SetupDiGetHwProfileFriendlyNameW: TSetupDiGetHwProfileFriendlyNameW;
  SetupDiGetHwProfileFriendlyName: TSetupDiGetHwProfileFriendlyName;
  SetupDiGetHwProfileFriendlyNameExA: TSetupDiGetHwProfileFriendlyNameExA;
  SetupDiGetHwProfileFriendlyNameExW: TSetupDiGetHwProfileFriendlyNameExW;
  SetupDiGetHwProfileFriendlyNameEx: TSetupDiGetHwProfileFriendlyNameEx;
  SetupDiGetWizardPage: TSetupDiGetWizardPage;
  SetupDiGetSelectedDevice: TSetupDiGetSelectedDevice;
  SetupDiSetSelectedDevice: TSetupDiSetSelectedDevice;
  SetupDiGetActualSectionToInstallA: TSetupDiGetActualSectionToInstallA;
  SetupDiGetActualSectionToInstallW: TSetupDiGetActualSectionToInstallW;
  SetupDiGetActualSectionToInstall: TSetupDiGetActualSectionToInstall;
  {$IFDEF WINXP}
  SetupDiGetActualSectionToInstallExA: TSetupDiGetActualSectionToInstallExA;
  SetupDiGetActualSectionToInstallExW: TSetupDiGetActualSectionToInstallExW;
  SetupDiGetActualSectionToInstallEx: TSetupDiGetActualSectionToInstallEx;
  SetupEnumInfSectionsA: TSetupEnumInfSectionsA;
  SetupEnumInfSectionsW: TSetupEnumInfSectionsW;
  SetupEnumInfSections: TSetupEnumInfSections;
  SetupVerifyInfFileA: TSetupVerifyInfFileA;
  SetupVerifyInfFileW: TSetupVerifyInfFileW;
  SetupVerifyInfFile: TSetupVerifyInfFile;
  SetupDiGetCustomDevicePropertyA: TSetupDiGetCustomDevicePropertyA;
  SetupDiGetCustomDevicePropertyW: TSetupDiGetCustomDevicePropertyW;
  SetupDiGetCustomDeviceProperty: TSetupDiGetCustomDeviceProperty;
  {$ENDIF WINXP}

{$ENDIF SETUPAPI_LINKONREQUEST}

function IsSetupApiLoaded: Boolean;
function LoadSetupApi: Boolean;
procedure UnloadSetupApi;

{$IFDEF SETUPAPI_LINKONREQUEST}
function GetSetupApiHandle:HINST;
{$endif}

implementation

uses
  ModuleLoader;

const
  SetupApiModuleName = 'SetupApi.dll';

{$IFDEF SETUPAPI_LINKONREQUEST}
var
  SetupApiLib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
  SetupApiLoadCount: Integer = 0;
{$ENDIF SETUPAPI_LINKONREQUEST}

{$IFDEF SETUPAPI_LINKONREQUEST}
function GetSetupApiHandle:HINST;
begin
  result := HINST(SetupApiLib);
end;
{$endif}

function IsSetupApiLoaded: Boolean;
begin
  {$IFDEF SETUPAPI_LINKONREQUEST}
  Result := SetupApiLib <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE}
  Result := True;
  {$ENDIF SETUPAPI_LINKONREQUEST}
end;

function LoadSetupApi: Boolean;
begin
  {$IFDEF SETUPAPI_LINKONREQUEST}
  Inc(SetupApiLoadCount);
  if SetupApiLoadCount > 1 then
    Exit;
  Result := LoadModule(SetupApiLib, SetupApiModuleName);
  if Result then
  begin
    {$IFDEF WINXP}
    @SetupGetFileQueueCount := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileQueueCount', Result);
    @SetupGetFileQueueFlags := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileQueueFlags', Result);
    @SetupSetFileQueueFlags := GetModuleSymbolEx(SetupApiLib, 'SetupSetFileQueueFlags', Result);
    {$ENDIF WINXP}
    @SetupGetInfInformationA := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfInformationA', Result);
    @SetupGetInfInformationW := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfInformationW', Result);
    @SetupGetInfInformation := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfInformationA', Result);
    @SetupQueryInfFileInformationA := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfFileInformationA', Result);
    @SetupQueryInfFileInformationW := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfFileInformationW', Result);
    @SetupQueryInfFileInformation := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfFileInformationA', Result);
    {$IFDEF WIN2000}
    @SetupQueryInfOriginalFileInformationA := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfOriginalFileInformationA', Result);
    @SetupQueryInfOriginalFileInformationW := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfOriginalFileInformationW', Result);
    @SetupQueryInfOriginalFileInformation := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfOriginalFileInformationA', Result);
    {$ENDIF WIN2000}
    @SetupQueryInfVersionInformationA := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfVersionInformationA', Result);
    @SetupQueryInfVersionInformationW := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfVersionInformationW', Result);
    @SetupQueryInfVersionInformation := GetModuleSymbolEx(SetupApiLib, 'SetupQueryInfVersionInformationA', Result);
    @SetupGetInfFileListA := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfFileListA', Result);
    @SetupGetInfFileListW := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfFileListW', Result);
    @SetupGetInfFileList := GetModuleSymbolEx(SetupApiLib, 'SetupGetInfFileListA', Result);
    @SetupOpenInfFileA := GetModuleSymbolEx(SetupApiLib, 'SetupOpenInfFileA', Result);
    @SetupOpenInfFileW := GetModuleSymbolEx(SetupApiLib, 'SetupOpenInfFileW', Result);
    @SetupOpenInfFile := GetModuleSymbolEx(SetupApiLib, 'SetupOpenInfFileA', Result);
    @SetupOpenMasterInf := GetModuleSymbolEx(SetupApiLib, 'SetupOpenMasterInf', Result);
    @SetupOpenAppendInfFileA := GetModuleSymbolEx(SetupApiLib, 'SetupOpenAppendInfFileA', Result);
    @SetupOpenAppendInfFileW := GetModuleSymbolEx(SetupApiLib, 'SetupOpenAppendInfFileW', Result);
    @SetupOpenAppendInfFile := GetModuleSymbolEx(SetupApiLib, 'SetupOpenAppendInfFileA', Result);
    @SetupCloseInfFile := GetModuleSymbolEx(SetupApiLib, 'SetupCloseInfFile', Result);
    @SetupFindFirstLineA := GetModuleSymbolEx(SetupApiLib, 'SetupFindFirstLineA', Result);
    @SetupFindFirstLineW := GetModuleSymbolEx(SetupApiLib, 'SetupFindFirstLineW', Result);
    @SetupFindFirstLine := GetModuleSymbolEx(SetupApiLib, 'SetupFindFirstLineA', Result);
    @SetupFindNextLine := GetModuleSymbolEx(SetupApiLib, 'SetupFindNextLine', Result);
    @SetupFindNextMatchLineA := GetModuleSymbolEx(SetupApiLib, 'SetupFindNextMatchLineA', Result);
    @SetupFindNextMatchLineW := GetModuleSymbolEx(SetupApiLib, 'SetupFindNextMatchLineW', Result);
    @SetupFindNextMatchLine := GetModuleSymbolEx(SetupApiLib, 'SetupFindNextMatchLineA', Result);
    @SetupGetLineByIndexA := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineByIndexA', Result);
    @SetupGetLineByIndexW := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineByIndexW', Result);
    @SetupGetLineByIndex := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineByIndexA', Result);
    @SetupGetLineCountA := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineCountA', Result);
    @SetupGetLineCountW := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineCountW', Result);
    @SetupGetLineCount := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineCountA', Result);
    @SetupGetLineTextA := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineTextA', Result);
    @SetupGetLineTextW := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineTextW', Result);
    @SetupGetLineText := GetModuleSymbolEx(SetupApiLib, 'SetupGetLineTextA', Result);
    @SetupGetFieldCount := GetModuleSymbolEx(SetupApiLib, 'SetupGetFieldCount', Result);
    @SetupGetStringFieldA := GetModuleSymbolEx(SetupApiLib, 'SetupGetStringFieldA', Result);
    @SetupGetStringFieldW := GetModuleSymbolEx(SetupApiLib, 'SetupGetStringFieldW', Result);
    @SetupGetStringField := GetModuleSymbolEx(SetupApiLib, 'SetupGetStringFieldA', Result);
    @SetupGetIntField := GetModuleSymbolEx(SetupApiLib, 'SetupGetIntField', Result);
    @SetupGetMultiSzFieldA := GetModuleSymbolEx(SetupApiLib, 'SetupGetMultiSzFieldA', Result);
    @SetupGetMultiSzFieldW := GetModuleSymbolEx(SetupApiLib, 'SetupGetMultiSzFieldW', Result);
    @SetupGetMultiSzField := GetModuleSymbolEx(SetupApiLib, 'SetupGetMultiSzFieldA', Result);
    @SetupGetBinaryField := GetModuleSymbolEx(SetupApiLib, 'SetupGetBinaryField', Result);
    @SetupGetFileCompressionInfoA := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoA', Result);
    @SetupGetFileCompressionInfoW := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoW', Result);
    @SetupGetFileCompressionInfo := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoA', Result);
    {$IFDEF WINXP}
    @SetupGetFileCompressionInfoExA := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoExA', Result);
    @SetupGetFileCompressionInfoExW := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoExW', Result);
    @SetupGetFileCompressionInfoEx := GetModuleSymbolEx(SetupApiLib, 'SetupGetFileCompressionInfoExA', Result);
    {$ENDIF WINXP}
    @SetupDecompressOrCopyFileA := GetModuleSymbolEx(SetupApiLib, 'SetupDecompressOrCopyFileA', Result);
    @SetupDecompressOrCopyFileW := GetModuleSymbolEx(SetupApiLib, 'SetupDecompressOrCopyFileW', Result);
    @SetupDecompressOrCopyFile := GetModuleSymbolEx(SetupApiLib, 'SetupDecompressOrCopyFileA', Result);
    @SetupGetSourceFileLocationA := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileLocationA', Result);
    @SetupGetSourceFileLocationW := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileLocationW', Result);
    @SetupGetSourceFileLocation := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileLocationA', Result);
    @SetupGetSourceFileSizeA := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileSizeA', Result);
    @SetupGetSourceFileSizeW := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileSizeW', Result);
    @SetupGetSourceFileSize := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceFileSizeA', Result);
    @SetupGetTargetPathA := GetModuleSymbolEx(SetupApiLib, 'SetupGetTargetPathA', Result);
    @SetupGetTargetPathW := GetModuleSymbolEx(SetupApiLib, 'SetupGetTargetPathW', Result);
    @SetupGetTargetPath := GetModuleSymbolEx(SetupApiLib, 'SetupGetTargetPathA', Result);
    @SetupSetSourceListA := GetModuleSymbolEx(SetupApiLib, 'SetupSetSourceListA', Result);
    @SetupSetSourceListW := GetModuleSymbolEx(SetupApiLib, 'SetupSetSourceListW', Result);
    @SetupSetSourceList := GetModuleSymbolEx(SetupApiLib, 'SetupSetSourceListA', Result);
    @SetupCancelTemporarySourceList := GetModuleSymbolEx(SetupApiLib, 'SetupCancelTemporarySourceList', Result);
    @SetupAddToSourceListA := GetModuleSymbolEx(SetupApiLib, 'SetupAddToSourceListA', Result);
    @SetupAddToSourceListW := GetModuleSymbolEx(SetupApiLib, 'SetupAddToSourceListW', Result);
    @SetupAddToSourceList := GetModuleSymbolEx(SetupApiLib, 'SetupAddToSourceListA', Result);
    @SetupRemoveFromSourceListA := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromSourceListA', Result);
    @SetupRemoveFromSourceListW := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromSourceListW', Result);
    @SetupRemoveFromSourceList := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromSourceListA', Result);
    @SetupQuerySourceListA := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySourceListA', Result);
    @SetupQuerySourceListW := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySourceListW', Result);
    @SetupQuerySourceList := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySourceListA', Result);
    @SetupFreeSourceListA := GetModuleSymbolEx(SetupApiLib, 'SetupFreeSourceListA', Result);
    @SetupFreeSourceListW := GetModuleSymbolEx(SetupApiLib, 'SetupFreeSourceListW', Result);
    @SetupFreeSourceList := GetModuleSymbolEx(SetupApiLib, 'SetupFreeSourceListA', Result);
    @SetupPromptForDiskA := GetModuleSymbolEx(SetupApiLib, 'SetupPromptForDiskA', Result);
    @SetupPromptForDiskW := GetModuleSymbolEx(SetupApiLib, 'SetupPromptForDiskW', Result);
    @SetupPromptForDisk := GetModuleSymbolEx(SetupApiLib, 'SetupPromptForDiskA', Result);
    @SetupCopyErrorA := GetModuleSymbolEx(SetupApiLib, 'SetupCopyErrorA', Result);
    @SetupCopyErrorW := GetModuleSymbolEx(SetupApiLib, 'SetupCopyErrorW', Result);
    @SetupCopyError := GetModuleSymbolEx(SetupApiLib, 'SetupCopyErrorA', Result);
    @SetupRenameErrorA := GetModuleSymbolEx(SetupApiLib, 'SetupRenameErrorA', Result);
    @SetupRenameErrorW := GetModuleSymbolEx(SetupApiLib, 'SetupRenameErrorW', Result);
    @SetupRenameError := GetModuleSymbolEx(SetupApiLib, 'SetupRenameErrorA', Result);
    @SetupDeleteErrorA := GetModuleSymbolEx(SetupApiLib, 'SetupDeleteErrorA', Result);
    @SetupDeleteErrorW := GetModuleSymbolEx(SetupApiLib, 'SetupDeleteErrorW', Result);
    @SetupDeleteError := GetModuleSymbolEx(SetupApiLib, 'SetupDeleteErrorA', Result);
    {$IFDEF WIN2000}
    @SetupBackupErrorA := GetModuleSymbolEx(SetupApiLib, 'SetupBackupErrorA', Result);
    @SetupBackupErrorW := GetModuleSymbolEx(SetupApiLib, 'SetupBackupErrorW', Result);
    @SetupBackupError := GetModuleSymbolEx(SetupApiLib, 'SetupBackupErrorA', Result);
    {$ENDIF WIN2000}
    @SetupSetDirectoryIdA := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdA', Result);
    @SetupSetDirectoryIdW := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdW', Result);
    @SetupSetDirectoryId := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdA', Result);
    @SetupSetDirectoryIdExA := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdExA', Result);
    @SetupSetDirectoryIdExW := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdExW', Result);
    @SetupSetDirectoryIdEx := GetModuleSymbolEx(SetupApiLib, 'SetupSetDirectoryIdExA', Result);
    @SetupGetSourceInfoA := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceInfoA', Result);
    @SetupGetSourceInfoW := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceInfoW', Result);
    @SetupGetSourceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupGetSourceInfoA', Result);
    @SetupInstallFileA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileA', Result);
    @SetupInstallFileW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileW', Result);
    @SetupInstallFile := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileA', Result);
    @SetupInstallFileExA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileExA', Result);
    @SetupInstallFileExW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileExW', Result);
    @SetupInstallFileEx := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFileExA', Result);
    @SetupOpenFileQueue := GetModuleSymbolEx(SetupApiLib, 'SetupOpenFileQueue', Result);
    @SetupCloseFileQueue := GetModuleSymbolEx(SetupApiLib, 'SetupCloseFileQueue', Result);
    {$IFDEF WIN2000}
    @SetupSetFileQueueAlternatePlatformA := GetModuleSymbolEx(SetupApiLib, 'SetupSetFileQueueAlternatePlatformA', Result);
    @SetupSetFileQueueAlternatePlatformW := GetModuleSymbolEx(SetupApiLib, 'SetupSetFileQueueAlternatePlatformW', Result);
    @SetupSetFileQueueAlternatePlatform := GetModuleSymbolEx(SetupApiLib, 'SetupSetFileQueueAlternatePlatformA', Result);
    {$ENDIF WIN2000}
    @SetupSetPlatformPathOverrideA := GetModuleSymbolEx(SetupApiLib, 'SetupSetPlatformPathOverrideA', Result);
    @SetupSetPlatformPathOverrideW := GetModuleSymbolEx(SetupApiLib, 'SetupSetPlatformPathOverrideW', Result);
    @SetupSetPlatformPathOverride := GetModuleSymbolEx(SetupApiLib, 'SetupSetPlatformPathOverrideA', Result);
    @SetupQueueCopyA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyA', Result);
    @SetupQueueCopyW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyW', Result);
    @SetupQueueCopy := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyA', Result);
    {$IFDEF WIN2000}
    @SetupQueueCopyIndirectA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyIndirectA', Result);
    @SetupQueueCopyIndirectW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyIndirectW', Result);
    @SetupQueueCopyIndirect := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopyIndirectA', Result);
    {$ENDIF WIN2000}
    @SetupQueueDefaultCopyA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDefaultCopyA', Result);
    @SetupQueueDefaultCopyW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDefaultCopyW', Result);
    @SetupQueueDefaultCopy := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDefaultCopyA', Result);
    @SetupQueueCopySectionA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopySectionA', Result);
    @SetupQueueCopySectionW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopySectionW', Result);
    @SetupQueueCopySection := GetModuleSymbolEx(SetupApiLib, 'SetupQueueCopySectionA', Result);
    @SetupQueueDeleteA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteA', Result);
    @SetupQueueDeleteW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteW', Result);
    @SetupQueueDelete := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteA', Result);
    @SetupQueueDeleteSectionA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteSectionA', Result);
    @SetupQueueDeleteSectionW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteSectionW', Result);
    @SetupQueueDeleteSection := GetModuleSymbolEx(SetupApiLib, 'SetupQueueDeleteSectionA', Result);
    @SetupQueueRenameA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameA', Result);
    @SetupQueueRenameW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameW', Result);
    @SetupQueueRename := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameA', Result);
    @SetupQueueRenameSectionA := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameSectionA', Result);
    @SetupQueueRenameSectionW := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameSectionW', Result);
    @SetupQueueRenameSection := GetModuleSymbolEx(SetupApiLib, 'SetupQueueRenameSectionA', Result);
    @SetupCommitFileQueueA := GetModuleSymbolEx(SetupApiLib, 'SetupCommitFileQueueA', Result);
    @SetupCommitFileQueueW := GetModuleSymbolEx(SetupApiLib, 'SetupCommitFileQueueW', Result);
    @SetupCommitFileQueue := GetModuleSymbolEx(SetupApiLib, 'SetupCommitFileQueueA', Result);
    @SetupScanFileQueueA := GetModuleSymbolEx(SetupApiLib, 'SetupScanFileQueueA', Result);
    @SetupScanFileQueueW := GetModuleSymbolEx(SetupApiLib, 'SetupScanFileQueueW', Result);
    @SetupScanFileQueue := GetModuleSymbolEx(SetupApiLib, 'SetupScanFileQueueA', Result);
    @SetupCopyOEMInfA := GetModuleSymbolEx(SetupApiLib, 'SetupCopyOEMInfA', Result);
    @SetupCopyOEMInfW := GetModuleSymbolEx(SetupApiLib, 'SetupCopyOEMInfW', Result);
    @SetupCopyOEMInf := GetModuleSymbolEx(SetupApiLib, 'SetupCopyOEMInfA', Result);
    {$IFDEF WINXP}
    @SetupUninstallOEMInfA := GetModuleSymbolEx(SetupApiLib, 'SetupUninstallOEMInfA', Result);
    @SetupUninstallOEMInfW := GetModuleSymbolEx(SetupApiLib, 'SetupUninstallOEMInfW', Result);
    @SetupUninstallOEMInf := GetModuleSymbolEx(SetupApiLib, 'SetupUninstallOEMInfA', Result);
    @SetupUninstallNewlyCopiedInfs := GetModuleSymbolEx(SetupApiLib, 'SetupUninstallNewlyCopiedInfs', Result);
    {$ENDIF WINXP}
    @SetupCreateDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupCreateDiskSpaceListA', Result);
    @SetupCreateDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupCreateDiskSpaceListW', Result);
    @SetupCreateDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupCreateDiskSpaceListA', Result);
    @SetupDuplicateDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupDuplicateDiskSpaceListA', Result);
    @SetupDuplicateDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupDuplicateDiskSpaceListW', Result);
    @SetupDuplicateDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupDuplicateDiskSpaceListA', Result);
    @SetupDestroyDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupDestroyDiskSpaceList', Result);
    @SetupQueryDrivesInDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupQueryDrivesInDiskSpaceListA', Result);
    @SetupQueryDrivesInDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupQueryDrivesInDiskSpaceListW', Result);
    @SetupQueryDrivesInDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupQueryDrivesInDiskSpaceListA', Result);
    @SetupQuerySpaceRequiredOnDriveA := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySpaceRequiredOnDriveA', Result);
    @SetupQuerySpaceRequiredOnDriveW := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySpaceRequiredOnDriveW', Result);
    @SetupQuerySpaceRequiredOnDrive := GetModuleSymbolEx(SetupApiLib, 'SetupQuerySpaceRequiredOnDriveA', Result);
    @SetupAdjustDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupAdjustDiskSpaceListA', Result);
    @SetupAdjustDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupAdjustDiskSpaceListW', Result);
    @SetupAdjustDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupAdjustDiskSpaceListA', Result);
    @SetupAddToDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupAddToDiskSpaceListA', Result);
    @SetupAddToDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupAddToDiskSpaceListW', Result);
    @SetupAddToDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupAddToDiskSpaceListA', Result);
    @SetupAddSectionToDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupAddSectionToDiskSpaceListA', Result);
    @SetupAddSectionToDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupAddSectionToDiskSpaceListW', Result);
    @SetupAddSectionToDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupAddSectionToDiskSpaceListA', Result);
    @SetupAddInstallSectionToDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupAddInstallSectionToDiskSpaceListA', Result);
    @SetupAddInstallSectionToDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupAddInstallSectionToDiskSpaceListW', Result);
    @SetupAddInstallSectionToDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupAddInstallSectionToDiskSpaceListA', Result);
    @SetupRemoveFromDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromDiskSpaceListA', Result);
    @SetupRemoveFromDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromDiskSpaceListW', Result);
    @SetupRemoveFromDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFromDiskSpaceListA', Result);
    @SetupRemoveSectionFromDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveSectionFromDiskSpaceListA', Result);
    @SetupRemoveSectionFromDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveSectionFromDiskSpaceListW', Result);
    @SetupRemoveSectionFromDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveSectionFromDiskSpaceListA', Result);
    @SetupRemoveInstallSectionFromDiskSpaceListA := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveInstallSectionFromDiskSpaceListA', Result);
    @SetupRemoveInstallSectionFromDiskSpaceListW := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveInstallSectionFromDiskSpaceListW', Result);
    @SetupRemoveInstallSectionFromDiskSpaceList := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveInstallSectionFromDiskSpaceListA', Result);
    @SetupIterateCabinetA := GetModuleSymbolEx(SetupApiLib, 'SetupIterateCabinetA', Result);
    @SetupIterateCabinetW := GetModuleSymbolEx(SetupApiLib, 'SetupIterateCabinetW', Result);
    @SetupIterateCabinet := GetModuleSymbolEx(SetupApiLib, 'SetupIterateCabinetA', Result);
    @SetupPromptReboot := GetModuleSymbolEx(SetupApiLib, 'SetupPromptReboot', Result);
    @SetupInitDefaultQueueCallback := GetModuleSymbolEx(SetupApiLib, 'SetupInitDefaultQueueCallback', Result);
    @SetupInitDefaultQueueCallbackEx := GetModuleSymbolEx(SetupApiLib, 'SetupInitDefaultQueueCallbackEx', Result);
    @SetupTermDefaultQueueCallback := GetModuleSymbolEx(SetupApiLib, 'SetupTermDefaultQueueCallback', Result);
    @SetupDefaultQueueCallbackA := GetModuleSymbolEx(SetupApiLib, 'SetupDefaultQueueCallbackA', Result);
    @SetupDefaultQueueCallbackW := GetModuleSymbolEx(SetupApiLib, 'SetupDefaultQueueCallbackW', Result);
    @SetupDefaultQueueCallback := GetModuleSymbolEx(SetupApiLib, 'SetupDefaultQueueCallbackA', Result);
    @SetupInstallFromInfSectionA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFromInfSectionA', Result);
    @SetupInstallFromInfSectionW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFromInfSectionW', Result);
    @SetupInstallFromInfSection := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFromInfSectionA', Result);
    @SetupInstallFilesFromInfSectionA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFilesFromInfSectionA', Result);
    @SetupInstallFilesFromInfSectionW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFilesFromInfSectionW', Result);
    @SetupInstallFilesFromInfSection := GetModuleSymbolEx(SetupApiLib, 'SetupInstallFilesFromInfSectionA', Result);
    @SetupInstallServicesFromInfSectionA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionA', Result);
    @SetupInstallServicesFromInfSectionW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionW', Result);
    @SetupInstallServicesFromInfSection := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionA', Result);
    @SetupInstallServicesFromInfSectionExA := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionExA', Result);
    @SetupInstallServicesFromInfSectionExW := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionExW', Result);
    @SetupInstallServicesFromInfSectionEx := GetModuleSymbolEx(SetupApiLib, 'SetupInstallServicesFromInfSectionExA', Result);
    {$IFDEF WINXP}
    @InstallHinfSectionA := GetModuleSymbolEx(SetupApiLib, 'InstallHinfSectionA', Result);
    @InstallHinfSectionW := GetModuleSymbolEx(SetupApiLib, 'InstallHinfSectionW', Result);
    @InstallHinfSection := GetModuleSymbolEx(SetupApiLib, 'InstallHinfSectionA', Result);
    {$ENDIF WINXP}
    @SetupInitializeFileLogA := GetModuleSymbolEx(SetupApiLib, 'SetupInitializeFileLogA', Result);
    @SetupInitializeFileLogW := GetModuleSymbolEx(SetupApiLib, 'SetupInitializeFileLogW', Result);
    @SetupInitializeFileLog := GetModuleSymbolEx(SetupApiLib, 'SetupInitializeFileLogA', Result);
    @SetupTerminateFileLog := GetModuleSymbolEx(SetupApiLib, 'SetupTerminateFileLog', Result);
    @SetupLogFileA := GetModuleSymbolEx(SetupApiLib, 'SetupLogFileA', Result);
    @SetupLogFileW := GetModuleSymbolEx(SetupApiLib, 'SetupLogFileW', Result);
    @SetupLogFile := GetModuleSymbolEx(SetupApiLib, 'SetupLogFileA', Result);
    @SetupRemoveFileLogEntryA := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFileLogEntryA', Result);
    @SetupRemoveFileLogEntryW := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFileLogEntryW', Result);
    @SetupRemoveFileLogEntry := GetModuleSymbolEx(SetupApiLib, 'SetupRemoveFileLogEntryA', Result);
    @SetupQueryFileLogA := GetModuleSymbolEx(SetupApiLib, 'SetupQueryFileLogA', Result);
    @SetupQueryFileLogW := GetModuleSymbolEx(SetupApiLib, 'SetupQueryFileLogW', Result);
    @SetupQueryFileLog := GetModuleSymbolEx(SetupApiLib, 'SetupQueryFileLogA', Result);
    @SetupOpenLog := GetModuleSymbolEx(SetupApiLib, 'SetupOpenLog', Result);
    @SetupLogErrorA := GetModuleSymbolEx(SetupApiLib, 'SetupLogErrorA', Result);
    @SetupLogErrorW := GetModuleSymbolEx(SetupApiLib, 'SetupLogErrorW', Result);
    @SetupLogError := GetModuleSymbolEx(SetupApiLib, 'SetupLogErrorA', Result);
    @SetupCloseLog := GetModuleSymbolEx(SetupApiLib, 'SetupCloseLog', Result);
    {$IFDEF WIN2000}
    @SetupGetBackupInformationA := GetModuleSymbolEx(SetupApiLib, 'SetupGetBackupInformationA', Result);
    @SetupGetBackupInformationW := GetModuleSymbolEx(SetupApiLib, 'SetupGetBackupInformationW', Result);
    @SetupGetBackupInformation := GetModuleSymbolEx(SetupApiLib, 'SetupGetBackupInformationA', Result);
    {$ENDIF WIN2000}
    {$IFDEF WINXP}
    @SetupPrepareQueueForRestoreA := GetModuleSymbolEx(SetupApiLib, 'SetupPrepareQueueForRestoreA', Result);
    @SetupPrepareQueueForRestoreW := GetModuleSymbolEx(SetupApiLib, 'SetupPrepareQueueForRestoreW', Result);
    @SetupPrepareQueueForRestore := GetModuleSymbolEx(SetupApiLib, 'SetupPrepareQueueForRestoreA', Result);
    @SetupSetNonInteractiveMode := GetModuleSymbolEx(SetupApiLib, 'SetupSetNonInteractiveMode', Result);
    @SetupGetNonInteractiveMode := GetModuleSymbolEx(SetupApiLib, 'SetupGetNonInteractiveMode', Result);
    {$ENDIF WINXP}
    @SetupDiCreateDeviceInfoList := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoList', Result);
    @SetupDiCreateDeviceInfoListExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoListExA', Result);
    @SetupDiCreateDeviceInfoListExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoListExW', Result);
    @SetupDiCreateDeviceInfoListEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoListExA', Result);
    @SetupDiGetDeviceInfoListClass := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInfoListClass', Result);
    @SetupDiGetDeviceInfoListDetailA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInfoListDetailA', Result);
    @SetupDiGetDeviceInfoListDetailW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInfoListDetailW', Result);
    @SetupDiGetDeviceInfoListDetail := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInfoListDetailA', Result);
    @SetupDiCreateDeviceInfoA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoA', Result);
    @SetupDiCreateDeviceInfoW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoW', Result);
    @SetupDiCreateDeviceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInfoA', Result);
    @SetupDiOpenDeviceInfoA := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInfoA', Result);
    @SetupDiOpenDeviceInfoW := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInfoW', Result);
    @SetupDiOpenDeviceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInfoA', Result);
    @SetupDiGetDeviceInstanceIdA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstanceIdA', Result);
    @SetupDiGetDeviceInstanceIdW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstanceIdW', Result);
    @SetupDiGetDeviceInstanceId := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstanceIdA', Result);
    @SetupDiDeleteDeviceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDeviceInfo', Result);
    @SetupDiEnumDeviceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDeviceInfo', Result);
    @SetupDiDestroyDeviceInfoList := GetModuleSymbolEx(SetupApiLib, 'SetupDiDestroyDeviceInfoList', Result);
    @SetupDiEnumDeviceInterfaces := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDeviceInterfaces', Result);
    @SetupDiEnumInterfaceDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDeviceInterfaces', Result);
    @SetupDiCreateDeviceInterfaceA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceA', Result);
    @SetupDiCreateInterfaceDeviceA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceA', Result);
    @SetupDiCreateDeviceInterfaceW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceW', Result);
    @SetupDiCreateInterfaceDeviceW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceW', Result);
    @SetupDiCreateDeviceInterface := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceA', Result);
    @SetupDiOpenDeviceInterfaceA := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceA', Result);
    @SetupDiOpenInterfaceDeviceA := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceA', Result);
    @SetupDiOpenDeviceInterfaceW := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceW', Result);
    @SetupDiOpenInterfaceDeviceW := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceW', Result);
    @SetupDiOpenDeviceInterface := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceA', Result);
    @SetupDiGetDeviceInterfaceAlias := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceAlias', Result);
    @SetupDiGetInterfaceDeviceAlias := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceAlias', Result);
    @SetupDiDeleteDeviceInterfaceData := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDeviceInterfaceData', Result);
    @SetupDiDeleteInterfaceDeviceData := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDeviceInterfaceData', Result);
    @SetupDiRemoveDeviceInterface := GetModuleSymbolEx(SetupApiLib, 'SetupDiRemoveDeviceInterface', Result);
    @SetupDiRemoveInterfaceDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiRemoveDeviceInterface', Result);
    @SetupDiGetDeviceInterfaceDetailA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceDetailA', Result);
    @SetupDiGetInterfaceDeviceDetailA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceDetailA', Result);
    @SetupDiGetDeviceInterfaceDetailW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceDetailW', Result);
    @SetupDiGetInterfaceDeviceDetailW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceDetailW', Result);
    @SetupDiGetDeviceInterfaceDetail := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInterfaceDetailA', Result);
    @SetupDiInstallDeviceInterfaces := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallDeviceInterfaces', Result);
    @SetupDiInstallInterfaceDevices := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallDeviceInterfaces', Result);
    {$IFDEF WINXP}
    @SetupDiSetDeviceInterfaceDefault := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceInterfaceDefault', Result);
    {$ENDIF WINXP}
    @SetupDiRegisterDeviceInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiRegisterDeviceInfo', Result);
    @SetupDiBuildDriverInfoList := GetModuleSymbolEx(SetupApiLib, 'SetupDiBuildDriverInfoList', Result);
    @SetupDiCancelDriverInfoSearch := GetModuleSymbolEx(SetupApiLib, 'SetupDiCancelDriverInfoSearch', Result);
    @SetupDiEnumDriverInfoA := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDriverInfoA', Result);
    @SetupDiEnumDriverInfoW := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDriverInfoW', Result);
    @SetupDiEnumDriverInfo := GetModuleSymbolEx(SetupApiLib, 'SetupDiEnumDriverInfoA', Result);
    @SetupDiGetSelectedDriverA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetSelectedDriverA', Result);
    @SetupDiGetSelectedDriverW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetSelectedDriverW', Result);
    @SetupDiGetSelectedDriver := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetSelectedDriverA', Result);
    @SetupDiSetSelectedDriverA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetSelectedDriverA', Result);
    @SetupDiSetSelectedDriverW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetSelectedDriverW', Result);
    @SetupDiSetSelectedDriver := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetSelectedDriverA', Result);
    @SetupDiGetDriverInfoDetailA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInfoDetailA', Result);
    @SetupDiGetDriverInfoDetailW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInfoDetailW', Result);
    @SetupDiGetDriverInfoDetail := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInfoDetailA', Result);
    @SetupDiDestroyDriverInfoList := GetModuleSymbolEx(SetupApiLib, 'SetupDiDestroyDriverInfoList', Result);
    @SetupDiGetClassDevsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsA', Result);
    @SetupDiGetClassDevsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsW', Result);
    @SetupDiGetClassDevs := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsA', Result);
    @SetupDiGetClassDevsExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsExA', Result);
    @SetupDiGetClassDevsExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsExW', Result);
    @SetupDiGetClassDevsEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevsExA', Result);
    @SetupDiGetINFClassA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetINFClassA', Result);
    @SetupDiGetINFClassW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetINFClassW', Result);
    @SetupDiGetINFClass := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetINFClassA', Result);
    @SetupDiBuildClassInfoList := GetModuleSymbolEx(SetupApiLib, 'SetupDiBuildClassInfoList', Result);
    @SetupDiBuildClassInfoListExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiBuildClassInfoListExA', Result);
    @SetupDiBuildClassInfoListExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiBuildClassInfoListExW', Result);
    @SetupDiBuildClassInfoListEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiBuildClassInfoListExA', Result);
    @SetupDiGetClassDescriptionA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionA', Result);
    @SetupDiGetClassDescriptionW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionW', Result);
    @SetupDiGetClassDescription := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionA', Result);
    @SetupDiGetClassDescriptionExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionExA', Result);
    @SetupDiGetClassDescriptionExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionExW', Result);
    @SetupDiGetClassDescriptionEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDescriptionExA', Result);
    @SetupDiCallClassInstaller := GetModuleSymbolEx(SetupApiLib, 'SetupDiCallClassInstaller', Result);
    @SetupDiSelectDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiSelectDevice', Result);
    @SetupDiSelectBestCompatDrv := GetModuleSymbolEx(SetupApiLib, 'SetupDiSelectBestCompatDrv', Result);
    @SetupDiInstallDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallDevice', Result);
    @SetupDiInstallDriverFiles := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallDriverFiles', Result);
    @SetupDiRegisterCoDeviceInstallers := GetModuleSymbolEx(SetupApiLib, 'SetupDiRegisterCoDeviceInstallers', Result);
    @SetupDiRemoveDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiRemoveDevice', Result);
    @SetupDiUnremoveDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiUnremoveDevice', Result);
    @SetupDiMoveDuplicateDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiMoveDuplicateDevice', Result);
    @SetupDiChangeState := GetModuleSymbolEx(SetupApiLib, 'SetupDiChangeState', Result);
    @SetupDiInstallClassA := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassA', Result);
    @SetupDiInstallClassW := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassW', Result);
    @SetupDiInstallClass := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassA', Result);
    @SetupDiInstallClassExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassExA', Result);
    @SetupDiInstallClassExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassExW', Result);
    @SetupDiInstallClassEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiInstallClassExA', Result);
    @SetupDiOpenClassRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenClassRegKey', Result);
    @SetupDiOpenClassRegKeyExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenClassRegKeyExA', Result);
    @SetupDiOpenClassRegKeyExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenClassRegKeyExW', Result);
    @SetupDiOpenClassRegKeyEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenClassRegKeyExA', Result);
    @SetupDiCreateDeviceInterfaceRegKeyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceRegKeyA', Result);
    @SetupDiCreateInterfaceDeviceRegKeyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceRegKeyA', Result);
    @SetupDiCreateDeviceInterfaceRegKeyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceRegKeyW', Result);
    @SetupDiCreateInterfaceDeviceRegKeyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceRegKeyW', Result);
    @SetupDiCreateDeviceInterfaceRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDeviceInterfaceRegKeyA', Result);
    @SetupDiOpenDeviceInterfaceRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceRegKey', Result);
    @SetupDiOpenInterfaceDeviceRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDeviceInterfaceRegKey', Result);
    @SetupDiDeleteDeviceInterfaceRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDeviceInterfaceRegKey', Result);
    @SetupDiDeleteInterfaceDeviceRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDeviceInterfaceRegKey', Result);
    @SetupDiCreateDevRegKeyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDevRegKeyA', Result);
    @SetupDiCreateDevRegKeyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDevRegKeyW', Result);
    @SetupDiCreateDevRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiCreateDevRegKeyA', Result);
    @SetupDiOpenDevRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiOpenDevRegKey', Result);
    @SetupDiDeleteDevRegKey := GetModuleSymbolEx(SetupApiLib, 'SetupDiDeleteDevRegKey', Result);
    @SetupDiGetHwProfileList := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileList', Result);
    @SetupDiGetHwProfileListExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileListExA', Result);
    @SetupDiGetHwProfileListExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileListExW', Result);
    @SetupDiGetHwProfileListEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileListExA', Result);
    @SetupDiGetDeviceRegistryPropertyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceRegistryPropertyA', Result);
    @SetupDiGetDeviceRegistryPropertyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceRegistryPropertyW', Result);
    @SetupDiGetDeviceRegistryProperty := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceRegistryPropertyA', Result);
    {$IFDEF WIN2000}
    @SetupDiGetClassRegistryPropertyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassRegistryPropertyA', Result);
    @SetupDiGetClassRegistryPropertyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassRegistryPropertyW', Result);
    @SetupDiGetClassRegistryProperty := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassRegistryPropertyA', Result);
    {$ENDIF WIN2000}
    @SetupDiSetDeviceRegistryPropertyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceRegistryPropertyA', Result);
    @SetupDiSetDeviceRegistryPropertyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceRegistryPropertyW', Result);
    @SetupDiSetDeviceRegistryProperty := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceRegistryPropertyA', Result);
    {$IFDEF WIN2000}
    @SetupDiSetClassRegistryPropertyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassRegistryPropertyA', Result);
    @SetupDiSetClassRegistryPropertyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassRegistryPropertyW', Result);
    @SetupDiSetClassRegistryProperty := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassRegistryPropertyA', Result);
    {$ENDIF WIN2000}
    @SetupDiGetDeviceInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstallParamsA', Result);
    @SetupDiGetDeviceInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstallParamsW', Result);
    @SetupDiGetDeviceInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDeviceInstallParamsA', Result);
    @SetupDiGetClassInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassInstallParamsA', Result);
    @SetupDiGetClassInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassInstallParamsW', Result);
    @SetupDiGetClassInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassInstallParamsA', Result);
    @SetupDiSetDeviceInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceInstallParamsA', Result);
    @SetupDiSetDeviceInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceInstallParamsW', Result);
    @SetupDiSetDeviceInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDeviceInstallParamsA', Result);
    @SetupDiSetClassInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassInstallParamsA', Result);
    @SetupDiSetClassInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassInstallParamsW', Result);
    @SetupDiSetClassInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetClassInstallParamsA', Result);
    @SetupDiGetDriverInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInstallParamsA', Result);
    @SetupDiGetDriverInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInstallParamsW', Result);
    @SetupDiGetDriverInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetDriverInstallParamsA', Result);
    @SetupDiSetDriverInstallParamsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDriverInstallParamsA', Result);
    @SetupDiSetDriverInstallParamsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDriverInstallParamsW', Result);
    @SetupDiSetDriverInstallParams := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetDriverInstallParamsA', Result);
    @SetupDiLoadClassIcon := GetModuleSymbolEx(SetupApiLib, 'SetupDiLoadClassIcon', Result);
    @SetupDiDrawMiniIcon := GetModuleSymbolEx(SetupApiLib, 'SetupDiDrawMiniIcon', Result);
    @SetupDiGetClassBitmapIndex := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassBitmapIndex', Result);
    @SetupDiGetClassImageList := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassImageList', Result);
    @SetupDiGetClassImageListExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassImageListExA', Result);
    @SetupDiGetClassImageListExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassImageListExW', Result);
    @SetupDiGetClassImageListEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassImageListExA', Result);
    @SetupDiGetClassImageIndex := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassImageIndex', Result);
    @SetupDiDestroyClassImageList := GetModuleSymbolEx(SetupApiLib, 'SetupDiDestroyClassImageList', Result);
    @SetupDiGetClassDevPropertySheetsA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevPropertySheetsA', Result);
    @SetupDiGetClassDevPropertySheetsW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevPropertySheetsW', Result);
    @SetupDiGetClassDevPropertySheets := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetClassDevPropertySheetsA', Result);
    @SetupDiAskForOEMDisk := GetModuleSymbolEx(SetupApiLib, 'SetupDiAskForOEMDisk', Result);
    @SetupDiSelectOEMDrv := GetModuleSymbolEx(SetupApiLib, 'SetupDiSelectOEMDrv', Result);
    @SetupDiClassNameFromGuidA := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidA', Result);
    @SetupDiClassNameFromGuidW := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidW', Result);
    @SetupDiClassNameFromGuid := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidA', Result);
    @SetupDiClassNameFromGuidExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidExA', Result);
    @SetupDiClassNameFromGuidExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidExW', Result);
    @SetupDiClassNameFromGuidEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassNameFromGuidExA', Result);
    @SetupDiClassGuidsFromNameA := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameA', Result);
    @SetupDiClassGuidsFromNameW := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameW', Result);
    @SetupDiClassGuidsFromName := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameA', Result);
    @SetupDiClassGuidsFromNameExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameExA', Result);
    @SetupDiClassGuidsFromNameExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameExW', Result);
    @SetupDiClassGuidsFromNameEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiClassGuidsFromNameExA', Result);
    @SetupDiGetHwProfileFriendlyNameA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameA', Result);
    @SetupDiGetHwProfileFriendlyNameW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameW', Result);
    @SetupDiGetHwProfileFriendlyName := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameA', Result);
    @SetupDiGetHwProfileFriendlyNameExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameExA', Result);
    @SetupDiGetHwProfileFriendlyNameExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameExW', Result);
    @SetupDiGetHwProfileFriendlyNameEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetHwProfileFriendlyNameExA', Result);
    @SetupDiGetWizardPage := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetWizardPage', Result);
    @SetupDiGetSelectedDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetSelectedDevice', Result);
    @SetupDiSetSelectedDevice := GetModuleSymbolEx(SetupApiLib, 'SetupDiSetSelectedDevice', Result);
    @SetupDiGetActualSectionToInstallA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallA', Result);
    @SetupDiGetActualSectionToInstallW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallW', Result);
    @SetupDiGetActualSectionToInstall := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallA', Result);
    {$IFDEF WINXP}
    @SetupDiGetActualSectionToInstallExA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallExA', Result);
    @SetupDiGetActualSectionToInstallExW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallExW', Result);
    @SetupDiGetActualSectionToInstallEx := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetActualSectionToInstallExA', Result);
    @SetupEnumInfSectionsA := GetModuleSymbolEx(SetupApiLib, 'SetupEnumInfSectionsA', Result);
    @SetupEnumInfSectionsW := GetModuleSymbolEx(SetupApiLib, 'SetupEnumInfSectionsW', Result);
    @SetupEnumInfSections := GetModuleSymbolEx(SetupApiLib, 'SetupEnumInfSectionsA', Result);
    @SetupVerifyInfFileA := GetModuleSymbolEx(SetupApiLib, 'SetupVerifyInfFileA', Result);
    @SetupVerifyInfFileW := GetModuleSymbolEx(SetupApiLib, 'SetupVerifyInfFileW', Result);
    @SetupVerifyInfFile := GetModuleSymbolEx(SetupApiLib, 'SetupVerifyInfFileA', Result);
    @SetupDiGetCustomDevicePropertyA := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetCustomDevicePropertyA', Result);
    @SetupDiGetCustomDevicePropertyW := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetCustomDevicePropertyW', Result);
    @SetupDiGetCustomDeviceProperty := GetModuleSymbolEx(SetupApiLib, 'SetupDiGetCustomDevicePropertyA', Result);
    {$ENDIF WINXP}
    if not Result then
      UnloadSetupApi;
  end;
  {$ELSE}
  Result := True;
  {$ENDIF SETUPAPI_LINKONREQUEST}
end;

procedure UnloadSetupApi;
begin
  {$IFDEF SETUPAPI_LINKONREQUEST}
  Dec(SetupApiLoadCount);
  if SetupApiLoadCount > 0 then
    Exit;
  UnloadModule(SetupApiLib);
  {$IFDEF WINXP}
  SetupGetFileQueueCount := nil;
  SetupGetFileQueueFlags := nil;
  SetupSetFileQueueFlags := nil;
  {$ENDIF WINXP}
  SetupGetInfInformationA := nil;
  SetupGetInfInformationW := nil;
  SetupGetInfInformation := nil;
  SetupQueryInfFileInformationA := nil;
  SetupQueryInfFileInformationW := nil;
  SetupQueryInfFileInformation := nil;
  {$IFDEF WIN2000}
  SetupQueryInfOriginalFileInformationA := nil;
  SetupQueryInfOriginalFileInformationW := nil;
  SetupQueryInfOriginalFileInformation := nil;
  {$ENDIF WIN2000}
  SetupQueryInfVersionInformationA := nil;
  SetupQueryInfVersionInformationW := nil;
  SetupQueryInfVersionInformation := nil;
  SetupGetInfFileListA := nil;
  SetupGetInfFileListW := nil;
  SetupGetInfFileList := nil;
  SetupOpenInfFileA := nil;
  SetupOpenInfFileW := nil;
  SetupOpenInfFile := nil;
  SetupOpenMasterInf := nil;
  SetupOpenAppendInfFileA := nil;
  SetupOpenAppendInfFileW := nil;
  SetupOpenAppendInfFile := nil;
  SetupCloseInfFile := nil;
  SetupFindFirstLineA := nil;
  SetupFindFirstLineW := nil;
  SetupFindFirstLine := nil;
  SetupFindNextLine := nil;
  SetupFindNextMatchLineA := nil;
  SetupFindNextMatchLineW := nil;
  SetupFindNextMatchLine := nil;
  SetupGetLineByIndexA := nil;
  SetupGetLineByIndexW := nil;
  SetupGetLineByIndex := nil;
  SetupGetLineCountA := nil;
  SetupGetLineCountW := nil;
  SetupGetLineCount := nil;
  SetupGetLineTextA := nil;
  SetupGetLineTextW := nil;
  SetupGetLineText := nil;
  SetupGetFieldCount := nil;
  SetupGetStringFieldA := nil;
  SetupGetStringFieldW := nil;
  SetupGetStringField := nil;
  SetupGetIntField := nil;
  SetupGetMultiSzFieldA := nil;
  SetupGetMultiSzFieldW := nil;
  SetupGetMultiSzField := nil;
  SetupGetBinaryField := nil;
  SetupGetFileCompressionInfoA := nil;
  SetupGetFileCompressionInfoW := nil;
  SetupGetFileCompressionInfo := nil;
  {$IFDEF WINXP}
  SetupGetFileCompressionInfoExA := nil;
  SetupGetFileCompressionInfoExW := nil;
  SetupGetFileCompressionInfoEx := nil;
  {$ENDIF WINXP}
  SetupDecompressOrCopyFileA := nil;
  SetupDecompressOrCopyFileW := nil;
  SetupDecompressOrCopyFile := nil;
  SetupGetSourceFileLocationA := nil;
  SetupGetSourceFileLocationW := nil;
  SetupGetSourceFileLocation := nil;
  SetupGetSourceFileSizeA := nil;
  SetupGetSourceFileSizeW := nil;
  SetupGetSourceFileSize := nil;
  SetupGetTargetPathA := nil;
  SetupGetTargetPathW := nil;
  SetupGetTargetPath := nil;
  SetupSetSourceListA := nil;
  SetupSetSourceListW := nil;
  SetupSetSourceList := nil;
  SetupCancelTemporarySourceList := nil;
  SetupAddToSourceListA := nil;
  SetupAddToSourceListW := nil;
  SetupAddToSourceList := nil;
  SetupRemoveFromSourceListA := nil;
  SetupRemoveFromSourceListW := nil;
  SetupRemoveFromSourceList := nil;
  SetupQuerySourceListA := nil;
  SetupQuerySourceListW := nil;
  SetupQuerySourceList := nil;
  SetupFreeSourceListA := nil;
  SetupFreeSourceListW := nil;
  SetupFreeSourceList := nil;
  SetupPromptForDiskA := nil;
  SetupPromptForDiskW := nil;
  SetupPromptForDisk := nil;
  SetupCopyErrorA := nil;
  SetupCopyErrorW := nil;
  SetupCopyError := nil;
  SetupRenameErrorA := nil;
  SetupRenameErrorW := nil;
  SetupRenameError := nil;
  SetupDeleteErrorA := nil;
  SetupDeleteErrorW := nil;
  SetupDeleteError := nil;
  {$IFDEF WIN2000}
  SetupBackupErrorA := nil;
  SetupBackupErrorW := nil;
  SetupBackupError := nil;
  {$ENDIF WIN2000}
  SetupSetDirectoryIdA := nil;
  SetupSetDirectoryIdW := nil;
  SetupSetDirectoryId := nil;
  SetupSetDirectoryIdExA := nil;
  SetupSetDirectoryIdExW := nil;
  SetupSetDirectoryIdEx := nil;
  SetupGetSourceInfoA := nil;
  SetupGetSourceInfoW := nil;
  SetupGetSourceInfo := nil;
  SetupInstallFileA := nil;
  SetupInstallFileW := nil;
  SetupInstallFile := nil;
  SetupInstallFileExA := nil;
  SetupInstallFileExW := nil;
  SetupInstallFileEx := nil;
  SetupOpenFileQueue := nil;
  SetupCloseFileQueue := nil;
  {$IFDEF WIN2000}
  SetupSetFileQueueAlternatePlatformA := nil;
  SetupSetFileQueueAlternatePlatformW := nil;
  SetupSetFileQueueAlternatePlatform := nil;
  {$ENDIF WIN2000}
  SetupSetPlatformPathOverrideA := nil;
  SetupSetPlatformPathOverrideW := nil;
  SetupSetPlatformPathOverride := nil;
  SetupQueueCopyA := nil;
  SetupQueueCopyW := nil;
  SetupQueueCopy := nil;
  {$IFDEF WIN2000}
  SetupQueueCopyIndirectA := nil;
  SetupQueueCopyIndirectW := nil;
  SetupQueueCopyIndirect := nil;
  {$ENDIF WIN2000}
  SetupQueueDefaultCopyA := nil;
  SetupQueueDefaultCopyW := nil;
  SetupQueueDefaultCopy := nil;
  SetupQueueCopySectionA := nil;
  SetupQueueCopySectionW := nil;
  SetupQueueCopySection := nil;
  SetupQueueDeleteA := nil;
  SetupQueueDeleteW := nil;
  SetupQueueDelete := nil;
  SetupQueueDeleteSectionA := nil;
  SetupQueueDeleteSectionW := nil;
  SetupQueueDeleteSection := nil;
  SetupQueueRenameA := nil;
  SetupQueueRenameW := nil;
  SetupQueueRename := nil;
  SetupQueueRenameSectionA := nil;
  SetupQueueRenameSectionW := nil;
  SetupQueueRenameSection := nil;
  SetupCommitFileQueueA := nil;
  SetupCommitFileQueueW := nil;
  SetupCommitFileQueue := nil;
  SetupScanFileQueueA := nil;
  SetupScanFileQueueW := nil;
  SetupScanFileQueue := nil;
  SetupCopyOEMInfA := nil;
  SetupCopyOEMInfW := nil;
  SetupCopyOEMInf := nil;
  {$IFDEF WINXP}
  SetupUninstallOEMInfA := nil;
  SetupUninstallOEMInfW := nil;
  SetupUninstallOEMInf := nil;
  SetupUninstallNewlyCopiedInfs := nil;
  {$ENDIF WINXP}
  SetupCreateDiskSpaceListA := nil;
  SetupCreateDiskSpaceListW := nil;
  SetupCreateDiskSpaceList := nil;
  SetupDuplicateDiskSpaceListA := nil;
  SetupDuplicateDiskSpaceListW := nil;
  SetupDuplicateDiskSpaceList := nil;
  SetupDestroyDiskSpaceList := nil;
  SetupQueryDrivesInDiskSpaceListA := nil;
  SetupQueryDrivesInDiskSpaceListW := nil;
  SetupQueryDrivesInDiskSpaceList := nil;
  SetupQuerySpaceRequiredOnDriveA := nil;
  SetupQuerySpaceRequiredOnDriveW := nil;
  SetupQuerySpaceRequiredOnDrive := nil;
  SetupAdjustDiskSpaceListA := nil;
  SetupAdjustDiskSpaceListW := nil;
  SetupAdjustDiskSpaceList := nil;
  SetupAddToDiskSpaceListA := nil;
  SetupAddToDiskSpaceListW := nil;
  SetupAddToDiskSpaceList := nil;
  SetupAddSectionToDiskSpaceListA := nil;
  SetupAddSectionToDiskSpaceListW := nil;
  SetupAddSectionToDiskSpaceList := nil;
  SetupAddInstallSectionToDiskSpaceListA := nil;
  SetupAddInstallSectionToDiskSpaceListW := nil;
  SetupAddInstallSectionToDiskSpaceList := nil;
  SetupRemoveFromDiskSpaceListA := nil;
  SetupRemoveFromDiskSpaceListW := nil;
  SetupRemoveFromDiskSpaceList := nil;
  SetupRemoveSectionFromDiskSpaceListA := nil;
  SetupRemoveSectionFromDiskSpaceListW := nil;
  SetupRemoveSectionFromDiskSpaceList := nil;
  SetupRemoveInstallSectionFromDiskSpaceListA := nil;
  SetupRemoveInstallSectionFromDiskSpaceListW := nil;
  SetupRemoveInstallSectionFromDiskSpaceList := nil;
  SetupIterateCabinetA := nil;
  SetupIterateCabinetW := nil;
  SetupIterateCabinet := nil;
  SetupPromptReboot := nil;
  SetupInitDefaultQueueCallback := nil;
  SetupInitDefaultQueueCallbackEx := nil;
  SetupTermDefaultQueueCallback := nil;
  SetupDefaultQueueCallbackA := nil;
  SetupDefaultQueueCallbackW := nil;
  SetupDefaultQueueCallback := nil;
  SetupInstallFromInfSectionA := nil;
  SetupInstallFromInfSectionW := nil;
  SetupInstallFromInfSection := nil;
  SetupInstallFilesFromInfSectionA := nil;
  SetupInstallFilesFromInfSectionW := nil;
  SetupInstallFilesFromInfSection := nil;
  SetupInstallServicesFromInfSectionA := nil;
  SetupInstallServicesFromInfSectionW := nil;
  SetupInstallServicesFromInfSection := nil;
  SetupInstallServicesFromInfSectionExA := nil;
  SetupInstallServicesFromInfSectionExW := nil;
  SetupInstallServicesFromInfSectionEx := nil;
  {$IFDEF WINXP}
  InstallHinfSectionA := nil;
  InstallHinfSectionW := nil;
  InstallHinfSection := nil;
  {$ENDIF WINXP}
  SetupInitializeFileLogA := nil;
  SetupInitializeFileLogW := nil;
  SetupInitializeFileLog := nil;
  SetupTerminateFileLog := nil;
  SetupLogFileA := nil;
  SetupLogFileW := nil;
  SetupLogFile := nil;
  SetupRemoveFileLogEntryA := nil;
  SetupRemoveFileLogEntryW := nil;
  SetupRemoveFileLogEntry := nil;
  SetupQueryFileLogA := nil;
  SetupQueryFileLogW := nil;
  SetupQueryFileLog := nil;
  SetupOpenLog := nil;
  SetupLogErrorA := nil;
  SetupLogErrorW := nil;
  SetupLogError := nil;
  SetupCloseLog := nil;
  {$IFDEF WIN2000}
  SetupGetBackupInformationA := nil;
  SetupGetBackupInformationW := nil;
  SetupGetBackupInformation := nil;
  {$ENDIF WIN2000}
  {$IFDEF WINXP}
  SetupPrepareQueueForRestoreA := nil;
  SetupPrepareQueueForRestoreW := nil;
  SetupPrepareQueueForRestore := nil;
  SetupSetNonInteractiveMode := nil;
  SetupGetNonInteractiveMode := nil;
  {$ENDIF WINXP}
  SetupDiCreateDeviceInfoList := nil;
  SetupDiCreateDeviceInfoListExA := nil;
  SetupDiCreateDeviceInfoListExW := nil;
  SetupDiCreateDeviceInfoListEx := nil;
  SetupDiGetDeviceInfoListClass := nil;
  SetupDiGetDeviceInfoListDetailA := nil;
  SetupDiGetDeviceInfoListDetailW := nil;
  SetupDiGetDeviceInfoListDetail := nil;
  SetupDiCreateDeviceInfoA := nil;
  SetupDiCreateDeviceInfoW := nil;
  SetupDiCreateDeviceInfo := nil;
  SetupDiOpenDeviceInfoA := nil;
  SetupDiOpenDeviceInfoW := nil;
  SetupDiOpenDeviceInfo := nil;
  SetupDiGetDeviceInstanceIdA := nil;
  SetupDiGetDeviceInstanceIdW := nil;
  SetupDiGetDeviceInstanceId := nil;
  SetupDiDeleteDeviceInfo := nil;
  SetupDiEnumDeviceInfo := nil;
  SetupDiDestroyDeviceInfoList := nil;
  SetupDiEnumDeviceInterfaces := nil;
  SetupDiEnumInterfaceDevice := nil;
  SetupDiCreateDeviceInterfaceA := nil;
  SetupDiCreateInterfaceDeviceA := nil;
  SetupDiCreateDeviceInterfaceW := nil;
  SetupDiCreateInterfaceDeviceW := nil;
  SetupDiCreateDeviceInterface := nil;
  SetupDiOpenDeviceInterfaceA := nil;
  SetupDiOpenInterfaceDeviceA := nil;
  SetupDiOpenDeviceInterfaceW := nil;
  SetupDiOpenInterfaceDeviceW := nil;
  SetupDiOpenDeviceInterface := nil;
  SetupDiGetDeviceInterfaceAlias := nil;
  SetupDiGetInterfaceDeviceAlias := nil;
  SetupDiDeleteDeviceInterfaceData := nil;
  SetupDiDeleteInterfaceDeviceData := nil;
  SetupDiRemoveDeviceInterface := nil;
  SetupDiRemoveInterfaceDevice := nil;
  SetupDiGetDeviceInterfaceDetailA := nil;
  SetupDiGetInterfaceDeviceDetailA := nil;
  SetupDiGetDeviceInterfaceDetailW := nil;
  SetupDiGetInterfaceDeviceDetailW := nil;
  SetupDiGetDeviceInterfaceDetail := nil;
  SetupDiInstallDeviceInterfaces := nil;
  SetupDiInstallInterfaceDevices := nil;
  {$IFDEF WINXP}
  SetupDiSetDeviceInterfaceDefault := nil;
  {$ENDIF WINXP}
  SetupDiRegisterDeviceInfo := nil;
  SetupDiBuildDriverInfoList := nil;
  SetupDiCancelDriverInfoSearch := nil;
  SetupDiEnumDriverInfoA := nil;
  SetupDiEnumDriverInfoW := nil;
  SetupDiEnumDriverInfo := nil;
  SetupDiGetSelectedDriverA := nil;
  SetupDiGetSelectedDriverW := nil;
  SetupDiGetSelectedDriver := nil;
  SetupDiSetSelectedDriverA := nil;
  SetupDiSetSelectedDriverW := nil;
  SetupDiSetSelectedDriver := nil;
  SetupDiGetDriverInfoDetailA := nil;
  SetupDiGetDriverInfoDetailW := nil;
  SetupDiGetDriverInfoDetail := nil;
  SetupDiDestroyDriverInfoList := nil;
  SetupDiGetClassDevsA := nil;
  SetupDiGetClassDevsW := nil;
  SetupDiGetClassDevs := nil;
  SetupDiGetClassDevsExA := nil;
  SetupDiGetClassDevsExW := nil;
  SetupDiGetClassDevsEx := nil;
  SetupDiGetINFClassA := nil;
  SetupDiGetINFClassW := nil;
  SetupDiGetINFClass := nil;
  SetupDiBuildClassInfoList := nil;
  SetupDiBuildClassInfoListExA := nil;
  SetupDiBuildClassInfoListExW := nil;
  SetupDiBuildClassInfoListEx := nil;
  SetupDiGetClassDescriptionA := nil;
  SetupDiGetClassDescriptionW := nil;
  SetupDiGetClassDescription := nil;
  SetupDiGetClassDescriptionExA := nil;
  SetupDiGetClassDescriptionExW := nil;
  SetupDiGetClassDescriptionEx := nil;
  SetupDiCallClassInstaller := nil;
  SetupDiSelectDevice := nil;
  SetupDiSelectBestCompatDrv := nil;
  SetupDiInstallDevice := nil;
  SetupDiInstallDriverFiles := nil;
  SetupDiRegisterCoDeviceInstallers := nil;
  SetupDiRemoveDevice := nil;
  SetupDiUnremoveDevice := nil;
  SetupDiMoveDuplicateDevice := nil;
  SetupDiChangeState := nil;
  SetupDiInstallClassA := nil;
  SetupDiInstallClassW := nil;
  SetupDiInstallClass := nil;
  SetupDiInstallClassExA := nil;
  SetupDiInstallClassExW := nil;
  SetupDiInstallClassEx := nil;
  SetupDiOpenClassRegKey := nil;
  SetupDiOpenClassRegKeyExA := nil;
  SetupDiOpenClassRegKeyExW := nil;
  SetupDiOpenClassRegKeyEx := nil;
  SetupDiCreateDeviceInterfaceRegKeyA := nil;
  SetupDiCreateInterfaceDeviceRegKeyA := nil;
  SetupDiCreateDeviceInterfaceRegKeyW := nil;
  SetupDiCreateInterfaceDeviceRegKeyW := nil;
  SetupDiCreateDeviceInterfaceRegKey := nil;
  SetupDiOpenDeviceInterfaceRegKey := nil;
  SetupDiOpenInterfaceDeviceRegKey := nil;
  SetupDiDeleteDeviceInterfaceRegKey := nil;
  SetupDiDeleteInterfaceDeviceRegKey := nil;
  SetupDiCreateDevRegKeyA := nil;
  SetupDiCreateDevRegKeyW := nil;
  SetupDiCreateDevRegKey := nil;
  SetupDiOpenDevRegKey := nil;
  SetupDiDeleteDevRegKey := nil;
  SetupDiGetHwProfileList := nil;
  SetupDiGetHwProfileListExA := nil;
  SetupDiGetHwProfileListExW := nil;
  SetupDiGetHwProfileListEx := nil;
  SetupDiGetDeviceRegistryPropertyA := nil;
  SetupDiGetDeviceRegistryPropertyW := nil;
  SetupDiGetDeviceRegistryProperty := nil;
  {$IFDEF WIN2000}
  SetupDiGetClassRegistryPropertyA := nil;
  SetupDiGetClassRegistryPropertyW := nil;
  SetupDiGetClassRegistryProperty := nil;
  {$ENDIF WIN2000}
  SetupDiSetDeviceRegistryPropertyA := nil;
  SetupDiSetDeviceRegistryPropertyW := nil;
  SetupDiSetDeviceRegistryProperty := nil;
  {$IFDEF WIN2000}
  SetupDiSetClassRegistryPropertyA := nil;
  SetupDiSetClassRegistryPropertyW := nil;
  SetupDiSetClassRegistryProperty := nil;
  {$ENDIF WIN2000}
  SetupDiGetDeviceInstallParamsA := nil;
  SetupDiGetDeviceInstallParamsW := nil;
  SetupDiGetDeviceInstallParams := nil;
  SetupDiGetClassInstallParamsA := nil;
  SetupDiGetClassInstallParamsW := nil;
  SetupDiGetClassInstallParams := nil;
  SetupDiSetDeviceInstallParamsA := nil;
  SetupDiSetDeviceInstallParamsW := nil;
  SetupDiSetDeviceInstallParams := nil;
  SetupDiSetClassInstallParamsA := nil;
  SetupDiSetClassInstallParamsW := nil;
  SetupDiSetClassInstallParams := nil;
  SetupDiGetDriverInstallParamsA := nil;
  SetupDiGetDriverInstallParamsW := nil;
  SetupDiGetDriverInstallParams := nil;
  SetupDiSetDriverInstallParamsA := nil;
  SetupDiSetDriverInstallParamsW := nil;
  SetupDiSetDriverInstallParams := nil;
  SetupDiLoadClassIcon := nil;
  SetupDiDrawMiniIcon := nil;
  SetupDiGetClassBitmapIndex := nil;
  SetupDiGetClassImageList := nil;
  SetupDiGetClassImageListExA := nil;
  SetupDiGetClassImageListExW := nil;
  SetupDiGetClassImageListEx := nil;
  SetupDiGetClassImageIndex := nil;
  SetupDiDestroyClassImageList := nil;
  SetupDiGetClassDevPropertySheetsA := nil;
  SetupDiGetClassDevPropertySheetsW := nil;
  SetupDiGetClassDevPropertySheets := nil;
  SetupDiAskForOEMDisk := nil;
  SetupDiSelectOEMDrv := nil;
  SetupDiClassNameFromGuidA := nil;
  SetupDiClassNameFromGuidW := nil;
  SetupDiClassNameFromGuid := nil;
  SetupDiClassNameFromGuidExA := nil;
  SetupDiClassNameFromGuidExW := nil;
  SetupDiClassNameFromGuidEx := nil;
  SetupDiClassGuidsFromNameA := nil;
  SetupDiClassGuidsFromNameW := nil;
  SetupDiClassGuidsFromName := nil;
  SetupDiClassGuidsFromNameExA := nil;
  SetupDiClassGuidsFromNameExW := nil;
  SetupDiClassGuidsFromNameEx := nil;
  SetupDiGetHwProfileFriendlyNameA := nil;
  SetupDiGetHwProfileFriendlyNameW := nil;
  SetupDiGetHwProfileFriendlyName := nil;
  SetupDiGetHwProfileFriendlyNameExA := nil;
  SetupDiGetHwProfileFriendlyNameExW := nil;
  SetupDiGetHwProfileFriendlyNameEx := nil;
  SetupDiGetWizardPage := nil;
  SetupDiGetSelectedDevice := nil;
  SetupDiSetSelectedDevice := nil;
  SetupDiGetActualSectionToInstallA := nil;
  SetupDiGetActualSectionToInstallW := nil;
  SetupDiGetActualSectionToInstall := nil;
  {$IFDEF WINXP}
  SetupDiGetActualSectionToInstallExA := nil;
  SetupDiGetActualSectionToInstallExW := nil;
  SetupDiGetActualSectionToInstallEx := nil;
  SetupEnumInfSectionsA := nil;
  SetupEnumInfSectionsW := nil;
  SetupEnumInfSections := nil;
  SetupVerifyInfFileA := nil;
  SetupVerifyInfFileW := nil;
  SetupVerifyInfFile := nil;
  SetupDiGetCustomDevicePropertyA := nil;
  SetupDiGetCustomDevicePropertyW := nil;
  SetupDiGetCustomDeviceProperty := nil;
  {$ENDIF WINXP}
  {$ENDIF SETUPAPI_LINKONREQUEST}
end;

{$IFNDEF SETUPAPI_LINKONREQUEST}

{$IFDEF WINXP}
function SetupGetFileQueueCount; external SetupApiModuleName name 'SetupGetFileQueueCount';
function SetupGetFileQueueFlags; external SetupApiModuleName name 'SetupGetFileQueueFlags';
function SetupSetFileQueueFlags; external SetupApiModuleName name 'SetupSetFileQueueFlags';
{$ENDIF WINXP}
function SetupGetInfInformationA; external SetupApiModuleName name 'SetupGetInfInformationA';
function SetupGetInfInformationW; external SetupApiModuleName name 'SetupGetInfInformationW';
function SetupGetInfInformation; external SetupApiModuleName name 'SetupGetInfInformationA';
function SetupQueryInfFileInformationA; external SetupApiModuleName name 'SetupQueryInfFileInformationA';
function SetupQueryInfFileInformationW; external SetupApiModuleName name 'SetupQueryInfFileInformationW';
function SetupQueryInfFileInformation; external SetupApiModuleName name 'SetupQueryInfFileInformationA';
{$IFDEF WIN2000}
function SetupQueryInfOriginalFileInformationA; external SetupApiModuleName name 'SetupQueryInfOriginalFileInformationA';
function SetupQueryInfOriginalFileInformationW; external SetupApiModuleName name 'SetupQueryInfOriginalFileInformationW';
function SetupQueryInfOriginalFileInformation; external SetupApiModuleName name 'SetupQueryInfOriginalFileInformationA';
{$ENDIF WIN2000}
function SetupQueryInfVersionInformationA; external SetupApiModuleName name 'SetupQueryInfVersionInformationA';
function SetupQueryInfVersionInformationW; external SetupApiModuleName name 'SetupQueryInfVersionInformationW';
function SetupQueryInfVersionInformation; external SetupApiModuleName name 'SetupQueryInfVersionInformationA';
function SetupGetInfFileListA; external SetupApiModuleName name 'SetupGetInfFileListA';
function SetupGetInfFileListW; external SetupApiModuleName name 'SetupGetInfFileListW';
function SetupGetInfFileList; external SetupApiModuleName name 'SetupGetInfFileListA';
function SetupOpenInfFileA; external SetupApiModuleName name 'SetupOpenInfFileA';
function SetupOpenInfFileW; external SetupApiModuleName name 'SetupOpenInfFileW';
function SetupOpenInfFile; external SetupApiModuleName name 'SetupOpenInfFileA';
function SetupOpenMasterInf; external SetupApiModuleName name 'SetupOpenMasterInf';
function SetupOpenAppendInfFileA; external SetupApiModuleName name 'SetupOpenAppendInfFileA';
function SetupOpenAppendInfFileW; external SetupApiModuleName name 'SetupOpenAppendInfFileW';
function SetupOpenAppendInfFile; external SetupApiModuleName name 'SetupOpenAppendInfFileA';
procedure SetupCloseInfFile; external SetupApiModuleName name 'SetupCloseInfFile';
function SetupFindFirstLineA; external SetupApiModuleName name 'SetupFindFirstLineA';
function SetupFindFirstLineW; external SetupApiModuleName name 'SetupFindFirstLineW';
function SetupFindFirstLine; external SetupApiModuleName name 'SetupFindFirstLineA';
function SetupFindNextLine; external SetupApiModuleName name 'SetupFindNextLine';
function SetupFindNextMatchLineA; external SetupApiModuleName name 'SetupFindNextMatchLineA';
function SetupFindNextMatchLineW; external SetupApiModuleName name 'SetupFindNextMatchLineW';
function SetupFindNextMatchLine; external SetupApiModuleName name 'SetupFindNextMatchLineA';
function SetupGetLineByIndexA; external SetupApiModuleName name 'SetupGetLineByIndexA';
function SetupGetLineByIndexW; external SetupApiModuleName name 'SetupGetLineByIndexW';
function SetupGetLineByIndex; external SetupApiModuleName name 'SetupGetLineByIndexA';
function SetupGetLineCountA; external SetupApiModuleName name 'SetupGetLineCountA';
function SetupGetLineCountW; external SetupApiModuleName name 'SetupGetLineCountW';
function SetupGetLineCount; external SetupApiModuleName name 'SetupGetLineCountA';
function SetupGetLineTextA; external SetupApiModuleName name 'SetupGetLineTextA';
function SetupGetLineTextW; external SetupApiModuleName name 'SetupGetLineTextW';
function SetupGetLineText; external SetupApiModuleName name 'SetupGetLineTextA';
function SetupGetFieldCount; external SetupApiModuleName name 'SetupGetFieldCount';
function SetupGetStringFieldA; external SetupApiModuleName name 'SetupGetStringFieldA';
function SetupGetStringFieldW; external SetupApiModuleName name 'SetupGetStringFieldW';
function SetupGetStringField; external SetupApiModuleName name 'SetupGetStringFieldA';
function SetupGetIntField; external SetupApiModuleName name 'SetupGetIntField';
function SetupGetMultiSzFieldA; external SetupApiModuleName name 'SetupGetMultiSzFieldA';
function SetupGetMultiSzFieldW; external SetupApiModuleName name 'SetupGetMultiSzFieldW';
function SetupGetMultiSzField; external SetupApiModuleName name 'SetupGetMultiSzFieldA';
function SetupGetBinaryField; external SetupApiModuleName name 'SetupGetBinaryField';
function SetupGetFileCompressionInfoA; external SetupApiModuleName name 'SetupGetFileCompressionInfoA';
function SetupGetFileCompressionInfoW; external SetupApiModuleName name 'SetupGetFileCompressionInfoW';
function SetupGetFileCompressionInfo; external SetupApiModuleName name 'SetupGetFileCompressionInfoA';
{$IFDEF WINXP}
function SetupGetFileCompressionInfoExA; external SetupApiModuleName name 'SetupGetFileCompressionInfoExA';
function SetupGetFileCompressionInfoExW; external SetupApiModuleName name 'SetupGetFileCompressionInfoExW';
function SetupGetFileCompressionInfoEx; external SetupApiModuleName name 'SetupGetFileCompressionInfoExA';
{$ENDIF WINXP}
function SetupDecompressOrCopyFileA; external SetupApiModuleName name 'SetupDecompressOrCopyFileA';
function SetupDecompressOrCopyFileW; external SetupApiModuleName name 'SetupDecompressOrCopyFileW';
function SetupDecompressOrCopyFile; external SetupApiModuleName name 'SetupDecompressOrCopyFileA';
function SetupGetSourceFileLocationA; external SetupApiModuleName name 'SetupGetSourceFileLocationA';
function SetupGetSourceFileLocationW; external SetupApiModuleName name 'SetupGetSourceFileLocationW';
function SetupGetSourceFileLocation; external SetupApiModuleName name 'SetupGetSourceFileLocationA';
function SetupGetSourceFileSizeA; external SetupApiModuleName name 'SetupGetSourceFileSizeA';
function SetupGetSourceFileSizeW; external SetupApiModuleName name 'SetupGetSourceFileSizeW';
function SetupGetSourceFileSize; external SetupApiModuleName name 'SetupGetSourceFileSizeA';
function SetupGetTargetPathA; external SetupApiModuleName name 'SetupGetTargetPathA';
function SetupGetTargetPathW; external SetupApiModuleName name 'SetupGetTargetPathW';
function SetupGetTargetPath; external SetupApiModuleName name 'SetupGetTargetPathA';
function SetupSetSourceListA; external SetupApiModuleName name 'SetupSetSourceListA';
function SetupSetSourceListW; external SetupApiModuleName name 'SetupSetSourceListW';
function SetupSetSourceList; external SetupApiModuleName name 'SetupSetSourceListA';
function SetupCancelTemporarySourceList; external SetupApiModuleName name 'SetupCancelTemporarySourceList';
function SetupAddToSourceListA; external SetupApiModuleName name 'SetupAddToSourceListA';
function SetupAddToSourceListW; external SetupApiModuleName name 'SetupAddToSourceListW';
function SetupAddToSourceList; external SetupApiModuleName name 'SetupAddToSourceListA';
function SetupRemoveFromSourceListA; external SetupApiModuleName name 'SetupRemoveFromSourceListA';
function SetupRemoveFromSourceListW; external SetupApiModuleName name 'SetupRemoveFromSourceListW';
function SetupRemoveFromSourceList; external SetupApiModuleName name 'SetupRemoveFromSourceListA';
function SetupQuerySourceListA; external SetupApiModuleName name 'SetupQuerySourceListA';
function SetupQuerySourceListW; external SetupApiModuleName name 'SetupQuerySourceListW';
function SetupQuerySourceList; external SetupApiModuleName name 'SetupQuerySourceListA';
function SetupFreeSourceListA; external SetupApiModuleName name 'SetupFreeSourceListA';
function SetupFreeSourceListW; external SetupApiModuleName name 'SetupFreeSourceListW';
function SetupFreeSourceList; external SetupApiModuleName name 'SetupFreeSourceListA';
function SetupPromptForDiskA; external SetupApiModuleName name 'SetupPromptForDiskA';
function SetupPromptForDiskW; external SetupApiModuleName name 'SetupPromptForDiskW';
function SetupPromptForDisk; external SetupApiModuleName name 'SetupPromptForDiskA';
function SetupCopyErrorA; external SetupApiModuleName name 'SetupCopyErrorA';
function SetupCopyErrorW; external SetupApiModuleName name 'SetupCopyErrorW';
function SetupCopyError; external SetupApiModuleName name 'SetupCopyErrorA';
function SetupRenameErrorA; external SetupApiModuleName name 'SetupRenameErrorA';
function SetupRenameErrorW; external SetupApiModuleName name 'SetupRenameErrorW';
function SetupRenameError; external SetupApiModuleName name 'SetupRenameErrorA';
function SetupDeleteErrorA; external SetupApiModuleName name 'SetupDeleteErrorA';
function SetupDeleteErrorW; external SetupApiModuleName name 'SetupDeleteErrorW';
function SetupDeleteError; external SetupApiModuleName name 'SetupDeleteErrorA';
{$IFDEF WIN2000}
function SetupBackupErrorA; external SetupApiModuleName name 'SetupBackupErrorA';
function SetupBackupErrorW; external SetupApiModuleName name 'SetupBackupErrorW';
function SetupBackupError; external SetupApiModuleName name 'SetupBackupErrorA';
{$ENDIF WIN2000}
function SetupSetDirectoryIdA; external SetupApiModuleName name 'SetupSetDirectoryIdA';
function SetupSetDirectoryIdW; external SetupApiModuleName name 'SetupSetDirectoryIdW';
function SetupSetDirectoryId; external SetupApiModuleName name 'SetupSetDirectoryIdA';
function SetupSetDirectoryIdExA; external SetupApiModuleName name 'SetupSetDirectoryIdExA';
function SetupSetDirectoryIdExW; external SetupApiModuleName name 'SetupSetDirectoryIdExW';
function SetupSetDirectoryIdEx; external SetupApiModuleName name 'SetupSetDirectoryIdExA';
function SetupGetSourceInfoA; external SetupApiModuleName name 'SetupGetSourceInfoA';
function SetupGetSourceInfoW; external SetupApiModuleName name 'SetupGetSourceInfoW';
function SetupGetSourceInfo; external SetupApiModuleName name 'SetupGetSourceInfoA';
function SetupInstallFileA; external SetupApiModuleName name 'SetupInstallFileA';
function SetupInstallFileW; external SetupApiModuleName name 'SetupInstallFileW';
function SetupInstallFile; external SetupApiModuleName name 'SetupInstallFileA';
function SetupInstallFileExA; external SetupApiModuleName name 'SetupInstallFileExA';
function SetupInstallFileExW; external SetupApiModuleName name 'SetupInstallFileExW';
function SetupInstallFileEx; external SetupApiModuleName name 'SetupInstallFileExA';
function SetupOpenFileQueue; external SetupApiModuleName name 'SetupOpenFileQueue';
function SetupCloseFileQueue; external SetupApiModuleName name 'SetupCloseFileQueue';
{$IFDEF WIN2000}
function SetupSetFileQueueAlternatePlatformA; external SetupApiModuleName name 'SetupSetFileQueueAlternatePlatformA';
function SetupSetFileQueueAlternatePlatformW; external SetupApiModuleName name 'SetupSetFileQueueAlternatePlatformW';
function SetupSetFileQueueAlternatePlatform; external SetupApiModuleName name 'SetupSetFileQueueAlternatePlatformA';
{$ENDIF WIN2000}
function SetupSetPlatformPathOverrideA; external SetupApiModuleName name 'SetupSetPlatformPathOverrideA';
function SetupSetPlatformPathOverrideW; external SetupApiModuleName name 'SetupSetPlatformPathOverrideW';
function SetupSetPlatformPathOverride; external SetupApiModuleName name 'SetupSetPlatformPathOverrideA';
function SetupQueueCopyA; external SetupApiModuleName name 'SetupQueueCopyA';
function SetupQueueCopyW; external SetupApiModuleName name 'SetupQueueCopyW';
function SetupQueueCopy; external SetupApiModuleName name 'SetupQueueCopyA';
{$IFDEF WIN2000}
function SetupQueueCopyIndirectA; external SetupApiModuleName name 'SetupQueueCopyIndirectA';
function SetupQueueCopyIndirectW; external SetupApiModuleName name 'SetupQueueCopyIndirectW';
function SetupQueueCopyIndirect; external SetupApiModuleName name 'SetupQueueCopyIndirectA';
{$ENDIF WIN2000}
function SetupQueueDefaultCopyA; external SetupApiModuleName name 'SetupQueueDefaultCopyA';
function SetupQueueDefaultCopyW; external SetupApiModuleName name 'SetupQueueDefaultCopyW';
function SetupQueueDefaultCopy; external SetupApiModuleName name 'SetupQueueDefaultCopyA';
function SetupQueueCopySectionA; external SetupApiModuleName name 'SetupQueueCopySectionA';
function SetupQueueCopySectionW; external SetupApiModuleName name 'SetupQueueCopySectionW';
function SetupQueueCopySection; external SetupApiModuleName name 'SetupQueueCopySectionA';
function SetupQueueDeleteA; external SetupApiModuleName name 'SetupQueueDeleteA';
function SetupQueueDeleteW; external SetupApiModuleName name 'SetupQueueDeleteW';
function SetupQueueDelete; external SetupApiModuleName name 'SetupQueueDeleteA';
function SetupQueueDeleteSectionA; external SetupApiModuleName name 'SetupQueueDeleteSectionA';
function SetupQueueDeleteSectionW; external SetupApiModuleName name 'SetupQueueDeleteSectionW';
function SetupQueueDeleteSection; external SetupApiModuleName name 'SetupQueueDeleteSectionA';
function SetupQueueRenameA; external SetupApiModuleName name 'SetupQueueRenameA';
function SetupQueueRenameW; external SetupApiModuleName name 'SetupQueueRenameW';
function SetupQueueRename; external SetupApiModuleName name 'SetupQueueRenameA';
function SetupQueueRenameSectionA; external SetupApiModuleName name 'SetupQueueRenameSectionA';
function SetupQueueRenameSectionW; external SetupApiModuleName name 'SetupQueueRenameSectionW';
function SetupQueueRenameSection; external SetupApiModuleName name 'SetupQueueRenameSectionA';
function SetupCommitFileQueueA; external SetupApiModuleName name 'SetupCommitFileQueueA';
function SetupCommitFileQueueW; external SetupApiModuleName name 'SetupCommitFileQueueW';
function SetupCommitFileQueue; external SetupApiModuleName name 'SetupCommitFileQueueA';
function SetupScanFileQueueA; external SetupApiModuleName name 'SetupScanFileQueueA';
function SetupScanFileQueueW; external SetupApiModuleName name 'SetupScanFileQueueW';
function SetupScanFileQueue; external SetupApiModuleName name 'SetupScanFileQueueA';
function SetupCopyOEMInfA; external SetupApiModuleName name 'SetupCopyOEMInfA';
function SetupCopyOEMInfW; external SetupApiModuleName name 'SetupCopyOEMInfW';
function SetupCopyOEMInf; external SetupApiModuleName name 'SetupCopyOEMInfA';
{$IFDEF WINXP}
function SetupUninstallOEMInfA; external SetupApiModuleName name 'SetupUninstallOEMInfA';
function SetupUninstallOEMInfW; external SetupApiModuleName name 'SetupUninstallOEMInfW';
function SetupUninstallOEMInf; external SetupApiModuleName name 'SetupUninstallOEMInfA';
function SetupUninstallNewlyCopiedInfs; external SetupApiModuleName name 'SetupUninstallNewlyCopiedInfs';
{$ENDIF WINXP}
function SetupCreateDiskSpaceListA; external SetupApiModuleName name 'SetupCreateDiskSpaceListA';
function SetupCreateDiskSpaceListW; external SetupApiModuleName name 'SetupCreateDiskSpaceListW';
function SetupCreateDiskSpaceList; external SetupApiModuleName name 'SetupCreateDiskSpaceListA';
function SetupDuplicateDiskSpaceListA; external SetupApiModuleName name 'SetupDuplicateDiskSpaceListA';
function SetupDuplicateDiskSpaceListW; external SetupApiModuleName name 'SetupDuplicateDiskSpaceListW';
function SetupDuplicateDiskSpaceList; external SetupApiModuleName name 'SetupDuplicateDiskSpaceListA';
function SetupDestroyDiskSpaceList; external SetupApiModuleName name 'SetupDestroyDiskSpaceList';
function SetupQueryDrivesInDiskSpaceListA; external SetupApiModuleName name 'SetupQueryDrivesInDiskSpaceListA';
function SetupQueryDrivesInDiskSpaceListW; external SetupApiModuleName name 'SetupQueryDrivesInDiskSpaceListW';
function SetupQueryDrivesInDiskSpaceList; external SetupApiModuleName name 'SetupQueryDrivesInDiskSpaceListA';
function SetupQuerySpaceRequiredOnDriveA; external SetupApiModuleName name 'SetupQuerySpaceRequiredOnDriveA';
function SetupQuerySpaceRequiredOnDriveW; external SetupApiModuleName name 'SetupQuerySpaceRequiredOnDriveW';
function SetupQuerySpaceRequiredOnDrive; external SetupApiModuleName name 'SetupQuerySpaceRequiredOnDriveA';
function SetupAdjustDiskSpaceListA; external SetupApiModuleName name 'SetupAdjustDiskSpaceListA';
function SetupAdjustDiskSpaceListW; external SetupApiModuleName name 'SetupAdjustDiskSpaceListW';
function SetupAdjustDiskSpaceList; external SetupApiModuleName name 'SetupAdjustDiskSpaceListA';
function SetupAddToDiskSpaceListA; external SetupApiModuleName name 'SetupAddToDiskSpaceListA';
function SetupAddToDiskSpaceListW; external SetupApiModuleName name 'SetupAddToDiskSpaceListW';
function SetupAddToDiskSpaceList; external SetupApiModuleName name 'SetupAddToDiskSpaceListA';
function SetupAddSectionToDiskSpaceListA; external SetupApiModuleName name 'SetupAddSectionToDiskSpaceListA';
function SetupAddSectionToDiskSpaceListW; external SetupApiModuleName name 'SetupAddSectionToDiskSpaceListW';
function SetupAddSectionToDiskSpaceList; external SetupApiModuleName name 'SetupAddSectionToDiskSpaceListA';
function SetupAddInstallSectionToDiskSpaceListA; external SetupApiModuleName name 'SetupAddInstallSectionToDiskSpaceListA';
function SetupAddInstallSectionToDiskSpaceListW; external SetupApiModuleName name 'SetupAddInstallSectionToDiskSpaceListW';
function SetupAddInstallSectionToDiskSpaceList; external SetupApiModuleName name 'SetupAddInstallSectionToDiskSpaceListA';
function SetupRemoveFromDiskSpaceListA; external SetupApiModuleName name 'SetupRemoveFromDiskSpaceListA';
function SetupRemoveFromDiskSpaceListW; external SetupApiModuleName name 'SetupRemoveFromDiskSpaceListW';
function SetupRemoveFromDiskSpaceList; external SetupApiModuleName name 'SetupRemoveFromDiskSpaceListA';
function SetupRemoveSectionFromDiskSpaceListA; external SetupApiModuleName name 'SetupRemoveSectionFromDiskSpaceListA';
function SetupRemoveSectionFromDiskSpaceListW; external SetupApiModuleName name 'SetupRemoveSectionFromDiskSpaceListW';
function SetupRemoveSectionFromDiskSpaceList; external SetupApiModuleName name 'SetupRemoveSectionFromDiskSpaceListA';
function SetupRemoveInstallSectionFromDiskSpaceListA; external SetupApiModuleName name 'SetupRemoveInstallSectionFromDiskSpaceListA';
function SetupRemoveInstallSectionFromDiskSpaceListW; external SetupApiModuleName name 'SetupRemoveInstallSectionFromDiskSpaceListW';
function SetupRemoveInstallSectionFromDiskSpaceList; external SetupApiModuleName name 'SetupRemoveInstallSectionFromDiskSpaceListA';
function SetupIterateCabinetA; external SetupApiModuleName name 'SetupIterateCabinetA';
function SetupIterateCabinetW; external SetupApiModuleName name 'SetupIterateCabinetW';
function SetupIterateCabinet; external SetupApiModuleName name 'SetupIterateCabinetA';
function SetupPromptReboot; external SetupApiModuleName name 'SetupPromptReboot';
function SetupInitDefaultQueueCallback; external SetupApiModuleName name 'SetupInitDefaultQueueCallback';
function SetupInitDefaultQueueCallbackEx; external SetupApiModuleName name 'SetupInitDefaultQueueCallbackEx';
procedure SetupTermDefaultQueueCallback; external SetupApiModuleName name 'SetupTermDefaultQueueCallback';
function SetupDefaultQueueCallbackA; external SetupApiModuleName name 'SetupDefaultQueueCallbackA';
function SetupDefaultQueueCallbackW; external SetupApiModuleName name 'SetupDefaultQueueCallbackW';
function SetupDefaultQueueCallback; external SetupApiModuleName name 'SetupDefaultQueueCallbackA';
function SetupInstallFromInfSectionA; external SetupApiModuleName name 'SetupInstallFromInfSectionA';
function SetupInstallFromInfSectionW; external SetupApiModuleName name 'SetupInstallFromInfSectionW';
function SetupInstallFromInfSection; external SetupApiModuleName name 'SetupInstallFromInfSectionA';
function SetupInstallFilesFromInfSectionA; external SetupApiModuleName name 'SetupInstallFilesFromInfSectionA';
function SetupInstallFilesFromInfSectionW; external SetupApiModuleName name 'SetupInstallFilesFromInfSectionW';
function SetupInstallFilesFromInfSection; external SetupApiModuleName name 'SetupInstallFilesFromInfSectionA';
function SetupInstallServicesFromInfSectionA; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionA';
function SetupInstallServicesFromInfSectionW; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionW';
function SetupInstallServicesFromInfSection; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionA';
function SetupInstallServicesFromInfSectionExA; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionExA';
function SetupInstallServicesFromInfSectionExW; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionExW';
function SetupInstallServicesFromInfSectionEx; external SetupApiModuleName name 'SetupInstallServicesFromInfSectionExA';
{$IFDEF WINXP}
function InstallHinfSectionA; external SetupApiModuleName name 'InstallHinfSectionA';
function InstallHinfSectionW; external SetupApiModuleName name 'InstallHinfSectionW';
function InstallHinfSection; external SetupApiModuleName name 'InstallHinfSectionA';
{$ENDIF WINXP}
function SetupInitializeFileLogA; external SetupApiModuleName name 'SetupInitializeFileLogA';
function SetupInitializeFileLogW; external SetupApiModuleName name 'SetupInitializeFileLogW';
function SetupInitializeFileLog; external SetupApiModuleName name 'SetupInitializeFileLogA';
function SetupTerminateFileLog; external SetupApiModuleName name 'SetupTerminateFileLog';
function SetupLogFileA; external SetupApiModuleName name 'SetupLogFileA';
function SetupLogFileW; external SetupApiModuleName name 'SetupLogFileW';
function SetupLogFile; external SetupApiModuleName name 'SetupLogFileA';
function SetupRemoveFileLogEntryA; external SetupApiModuleName name 'SetupRemoveFileLogEntryA';
function SetupRemoveFileLogEntryW; external SetupApiModuleName name 'SetupRemoveFileLogEntryW';
function SetupRemoveFileLogEntry; external SetupApiModuleName name 'SetupRemoveFileLogEntryA';
function SetupQueryFileLogA; external SetupApiModuleName name 'SetupQueryFileLogA';
function SetupQueryFileLogW; external SetupApiModuleName name 'SetupQueryFileLogW';
function SetupQueryFileLog; external SetupApiModuleName name 'SetupQueryFileLogA';
function SetupOpenLog; external SetupApiModuleName name 'SetupOpenLog';
function SetupLogErrorA; external SetupApiModuleName name 'SetupLogErrorA';
function SetupLogErrorW; external SetupApiModuleName name 'SetupLogErrorW';
function SetupLogError; external SetupApiModuleName name 'SetupLogErrorA';
procedure SetupCloseLog; external SetupApiModuleName name 'SetupCloseLog';
{$IFDEF WIN2000}
function SetupGetBackupInformationA; external SetupApiModuleName name 'SetupGetBackupInformationA';
function SetupGetBackupInformationW; external SetupApiModuleName name 'SetupGetBackupInformationW';
function SetupGetBackupInformation; external SetupApiModuleName name 'SetupGetBackupInformationA';
{$ENDIF WIN2000}
{$IFDEF WINXP}
function SetupPrepareQueueForRestoreA; external SetupApiModuleName name 'SetupPrepareQueueForRestoreA';
function SetupPrepareQueueForRestoreW; external SetupApiModuleName name 'SetupPrepareQueueForRestoreW';
function SetupPrepareQueueForRestore; external SetupApiModuleName name 'SetupPrepareQueueForRestoreA';
function SetupSetNonInteractiveMode; external SetupApiModuleName name 'SetupSetNonInteractiveMode';
function SetupGetNonInteractiveMode; external SetupApiModuleName name 'SetupGetNonInteractiveMode';
{$ENDIF WINXP}
function SetupDiCreateDeviceInfoList; external SetupApiModuleName name 'SetupDiCreateDeviceInfoList';
function SetupDiCreateDeviceInfoListExA; external SetupApiModuleName name 'SetupDiCreateDeviceInfoListExA';
function SetupDiCreateDeviceInfoListExW; external SetupApiModuleName name 'SetupDiCreateDeviceInfoListExW';
function SetupDiCreateDeviceInfoListEx; external SetupApiModuleName name 'SetupDiCreateDeviceInfoListExA';
function SetupDiGetDeviceInfoListClass; external SetupApiModuleName name 'SetupDiGetDeviceInfoListClass';
function SetupDiGetDeviceInfoListDetailA; external SetupApiModuleName name 'SetupDiGetDeviceInfoListDetailA';
function SetupDiGetDeviceInfoListDetailW; external SetupApiModuleName name 'SetupDiGetDeviceInfoListDetailW';
function SetupDiGetDeviceInfoListDetail; external SetupApiModuleName name 'SetupDiGetDeviceInfoListDetailA';
function SetupDiCreateDeviceInfoA; external SetupApiModuleName name 'SetupDiCreateDeviceInfoA';
function SetupDiCreateDeviceInfoW; external SetupApiModuleName name 'SetupDiCreateDeviceInfoW';
function SetupDiCreateDeviceInfo; external SetupApiModuleName name 'SetupDiCreateDeviceInfoA';
function SetupDiOpenDeviceInfoA; external SetupApiModuleName name 'SetupDiOpenDeviceInfoA';
function SetupDiOpenDeviceInfoW; external SetupApiModuleName name 'SetupDiOpenDeviceInfoW';
function SetupDiOpenDeviceInfo; external SetupApiModuleName name 'SetupDiOpenDeviceInfoA';
function SetupDiGetDeviceInstanceIdA; external SetupApiModuleName name 'SetupDiGetDeviceInstanceIdA';
function SetupDiGetDeviceInstanceIdW; external SetupApiModuleName name 'SetupDiGetDeviceInstanceIdW';
function SetupDiGetDeviceInstanceId; external SetupApiModuleName name 'SetupDiGetDeviceInstanceIdA';
function SetupDiDeleteDeviceInfo; external SetupApiModuleName name 'SetupDiDeleteDeviceInfo';
function SetupDiEnumDeviceInfo; external SetupApiModuleName name 'SetupDiEnumDeviceInfo';
function SetupDiDestroyDeviceInfoList; external SetupApiModuleName name 'SetupDiDestroyDeviceInfoList';
function SetupDiEnumDeviceInterfaces; external SetupApiModuleName name 'SetupDiEnumDeviceInterfaces';
function SetupDiEnumInterfaceDevice; external SetupApiModuleName name 'SetupDiEnumDeviceInterfaces';
function SetupDiCreateDeviceInterfaceA; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceA';
function SetupDiCreateInterfaceDeviceA; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceA';
function SetupDiCreateDeviceInterfaceW; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceW';
function SetupDiCreateInterfaceDeviceW; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceW';
function SetupDiCreateDeviceInterface; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceA';
function SetupDiCreateInterfaceDevice; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceA';
function SetupDiOpenDeviceInterfaceA; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceA';
function SetupDiOpenInterfaceDeviceA; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceA';
function SetupDiOpenDeviceInterfaceW; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceW';
function SetupDiOpenInterfaceDeviceW; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceW';
function SetupDiOpenDeviceInterface; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceA';
function SetupDiOpenInterfaceDevice; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceA';
function SetupDiGetDeviceInterfaceAlias; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceAlias';
function SetupDiGetInterfaceDeviceAlias; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceAlias';
function SetupDiDeleteDeviceInterfaceData; external SetupApiModuleName name 'SetupDiDeleteDeviceInterfaceData';
function SetupDiDeleteInterfaceDeviceData; external SetupApiModuleName name 'SetupDiDeleteDeviceInterfaceData';
function SetupDiRemoveDeviceInterface; external SetupApiModuleName name 'SetupDiRemoveDeviceInterface';
function SetupDiRemoveInterfaceDevice; external SetupApiModuleName name 'SetupDiRemoveDeviceInterface';
function SetupDiGetDeviceInterfaceDetailA; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailA';
function SetupDiGetInterfaceDeviceDetailA; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailA';
function SetupDiGetDeviceInterfaceDetailW; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailW';
function SetupDiGetInterfaceDeviceDetailW; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailW';
function SetupDiGetDeviceInterfaceDetail; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailA';
function SetupDiGetInterfaceDeviceDetail; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailA';
function SetupDiInstallDeviceInterfaces; external SetupApiModuleName name 'SetupDiInstallDeviceInterfaces';
function SetupDiInstallInterfaceDevices; external SetupApiModuleName name 'SetupDiInstallDeviceInterfaces';
{$IFDEF WINXP}
function SetupDiSetDeviceInterfaceDefault; external SetupApiModuleName name 'SetupDiSetDeviceInterfaceDefault';
{$ENDIF WINXP}
function SetupDiRegisterDeviceInfo; external SetupApiModuleName name 'SetupDiRegisterDeviceInfo';
function SetupDiBuildDriverInfoList; external SetupApiModuleName name 'SetupDiBuildDriverInfoList';
function SetupDiCancelDriverInfoSearch; external SetupApiModuleName name 'SetupDiCancelDriverInfoSearch';
function SetupDiEnumDriverInfoA; external SetupApiModuleName name 'SetupDiEnumDriverInfoA';
function SetupDiEnumDriverInfoW; external SetupApiModuleName name 'SetupDiEnumDriverInfoW';
function SetupDiEnumDriverInfo; external SetupApiModuleName name 'SetupDiEnumDriverInfoA';
function SetupDiGetSelectedDriverA; external SetupApiModuleName name 'SetupDiGetSelectedDriverA';
function SetupDiGetSelectedDriverW; external SetupApiModuleName name 'SetupDiGetSelectedDriverW';
function SetupDiGetSelectedDriver; external SetupApiModuleName name 'SetupDiGetSelectedDriverA';
function SetupDiSetSelectedDriverA; external SetupApiModuleName name 'SetupDiSetSelectedDriverA';
function SetupDiSetSelectedDriverW; external SetupApiModuleName name 'SetupDiSetSelectedDriverW';
function SetupDiSetSelectedDriver; external SetupApiModuleName name 'SetupDiSetSelectedDriverA';
function SetupDiGetDriverInfoDetailA; external SetupApiModuleName name 'SetupDiGetDriverInfoDetailA';
function SetupDiGetDriverInfoDetailW; external SetupApiModuleName name 'SetupDiGetDriverInfoDetailW';
function SetupDiGetDriverInfoDetail; external SetupApiModuleName name 'SetupDiGetDriverInfoDetailA';
function SetupDiDestroyDriverInfoList; external SetupApiModuleName name 'SetupDiDestroyDriverInfoList';
function SetupDiGetClassDevsA; external SetupApiModuleName name 'SetupDiGetClassDevsA';
function SetupDiGetClassDevsW; external SetupApiModuleName name 'SetupDiGetClassDevsW';
function SetupDiGetClassDevs; external SetupApiModuleName name 'SetupDiGetClassDevsA';
function SetupDiGetClassDevsExA; external SetupApiModuleName name 'SetupDiGetClassDevsExA';
function SetupDiGetClassDevsExW; external SetupApiModuleName name 'SetupDiGetClassDevsExW';
function SetupDiGetClassDevsEx; external SetupApiModuleName name 'SetupDiGetClassDevsExA';
function SetupDiGetINFClassA; external SetupApiModuleName name 'SetupDiGetINFClassA';
function SetupDiGetINFClassW; external SetupApiModuleName name 'SetupDiGetINFClassW';
function SetupDiGetINFClass; external SetupApiModuleName name 'SetupDiGetINFClassA';
function SetupDiBuildClassInfoList; external SetupApiModuleName name 'SetupDiBuildClassInfoList';
function SetupDiBuildClassInfoListExA; external SetupApiModuleName name 'SetupDiBuildClassInfoListExA';
function SetupDiBuildClassInfoListExW; external SetupApiModuleName name 'SetupDiBuildClassInfoListExW';
function SetupDiBuildClassInfoListEx; external SetupApiModuleName name 'SetupDiBuildClassInfoListExA';
function SetupDiGetClassDescriptionA; external SetupApiModuleName name 'SetupDiGetClassDescriptionA';
function SetupDiGetClassDescriptionW; external SetupApiModuleName name 'SetupDiGetClassDescriptionW';
function SetupDiGetClassDescription; external SetupApiModuleName name 'SetupDiGetClassDescriptionA';
function SetupDiGetClassDescriptionExA; external SetupApiModuleName name 'SetupDiGetClassDescriptionExA';
function SetupDiGetClassDescriptionExW; external SetupApiModuleName name 'SetupDiGetClassDescriptionExW';
function SetupDiGetClassDescriptionEx; external SetupApiModuleName name 'SetupDiGetClassDescriptionExA';
function SetupDiCallClassInstaller; external SetupApiModuleName name 'SetupDiCallClassInstaller';
function SetupDiSelectDevice; external SetupApiModuleName name 'SetupDiSelectDevice';
function SetupDiSelectBestCompatDrv; external SetupApiModuleName name 'SetupDiSelectBestCompatDrv';
function SetupDiInstallDevice; external SetupApiModuleName name 'SetupDiInstallDevice';
function SetupDiInstallDriverFiles; external SetupApiModuleName name 'SetupDiInstallDriverFiles';
function SetupDiRegisterCoDeviceInstallers; external SetupApiModuleName name 'SetupDiRegisterCoDeviceInstallers';
function SetupDiRemoveDevice; external SetupApiModuleName name 'SetupDiRemoveDevice';
function SetupDiUnremoveDevice; external SetupApiModuleName name 'SetupDiUnremoveDevice';
function SetupDiMoveDuplicateDevice; external SetupApiModuleName name 'SetupDiMoveDuplicateDevice';
function SetupDiChangeState; external SetupApiModuleName name 'SetupDiChangeState';
function SetupDiInstallClassA; external SetupApiModuleName name 'SetupDiInstallClassA';
function SetupDiInstallClassW; external SetupApiModuleName name 'SetupDiInstallClassW';
function SetupDiInstallClass; external SetupApiModuleName name 'SetupDiInstallClassA';
function SetupDiInstallClassExA; external SetupApiModuleName name 'SetupDiInstallClassExA';
function SetupDiInstallClassExW; external SetupApiModuleName name 'SetupDiInstallClassExW';
function SetupDiInstallClassEx; external SetupApiModuleName name 'SetupDiInstallClassExA';
function SetupDiOpenClassRegKey; external SetupApiModuleName name 'SetupDiOpenClassRegKey';
function SetupDiOpenClassRegKeyExA; external SetupApiModuleName name 'SetupDiOpenClassRegKeyExA';
function SetupDiOpenClassRegKeyExW; external SetupApiModuleName name 'SetupDiOpenClassRegKeyExW';
function SetupDiOpenClassRegKeyEx; external SetupApiModuleName name 'SetupDiOpenClassRegKeyExA';
function SetupDiCreateDeviceInterfaceRegKeyA; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyA';
function SetupDiCreateInterfaceDeviceRegKeyA; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyA';
function SetupDiCreateDeviceInterfaceRegKeyW; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyW';
function SetupDiCreateInterfaceDeviceRegKeyW; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyW';
function SetupDiCreateDeviceInterfaceRegKey; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyA';
function SetupDiCreateInterfaceDeviceRegKey; external SetupApiModuleName name 'SetupDiCreateDeviceInterfaceRegKeyA';
function SetupDiOpenDeviceInterfaceRegKey; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceRegKey';
function SetupDiOpenInterfaceDeviceRegKey; external SetupApiModuleName name 'SetupDiOpenDeviceInterfaceRegKey';
function SetupDiDeleteDeviceInterfaceRegKey; external SetupApiModuleName name 'SetupDiDeleteDeviceInterfaceRegKey';
function SetupDiDeleteInterfaceDeviceRegKey; external SetupApiModuleName name 'SetupDiDeleteDeviceInterfaceRegKey';
function SetupDiCreateDevRegKeyA; external SetupApiModuleName name 'SetupDiCreateDevRegKeyA';
function SetupDiCreateDevRegKeyW; external SetupApiModuleName name 'SetupDiCreateDevRegKeyW';
function SetupDiCreateDevRegKey; external SetupApiModuleName name 'SetupDiCreateDevRegKeyA';
function SetupDiOpenDevRegKey; external SetupApiModuleName name 'SetupDiOpenDevRegKey';
function SetupDiDeleteDevRegKey; external SetupApiModuleName name 'SetupDiDeleteDevRegKey';
function SetupDiGetHwProfileList; external SetupApiModuleName name 'SetupDiGetHwProfileList';
function SetupDiGetHwProfileListExA; external SetupApiModuleName name 'SetupDiGetHwProfileListExA';
function SetupDiGetHwProfileListExW; external SetupApiModuleName name 'SetupDiGetHwProfileListExW';
function SetupDiGetHwProfileListEx; external SetupApiModuleName name 'SetupDiGetHwProfileListExA';
function SetupDiGetDeviceRegistryPropertyA; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyA';
function SetupDiGetDeviceRegistryPropertyW; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyW';
function SetupDiGetDeviceRegistryProperty; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyA';
{$IFDEF WIN2000}
function SetupDiGetClassRegistryPropertyA; external SetupApiModuleName name 'SetupDiGetClassRegistryPropertyA';
function SetupDiGetClassRegistryPropertyW; external SetupApiModuleName name 'SetupDiGetClassRegistryPropertyW';
function SetupDiGetClassRegistryProperty; external SetupApiModuleName name 'SetupDiGetClassRegistryPropertyA';
{$ENDIF WIN2000}
function SetupDiSetDeviceRegistryPropertyA; external SetupApiModuleName name 'SetupDiSetDeviceRegistryPropertyA';
function SetupDiSetDeviceRegistryPropertyW; external SetupApiModuleName name 'SetupDiSetDeviceRegistryPropertyW';
function SetupDiSetDeviceRegistryProperty; external SetupApiModuleName name 'SetupDiSetDeviceRegistryPropertyA';
{$IFDEF WIN2000}
function SetupDiSetClassRegistryPropertyA; external SetupApiModuleName name 'SetupDiSetClassRegistryPropertyA';
function SetupDiSetClassRegistryPropertyW; external SetupApiModuleName name 'SetupDiSetClassRegistryPropertyW';
function SetupDiSetClassRegistryProperty; external SetupApiModuleName name 'SetupDiSetClassRegistryPropertyA';
{$ENDIF WIN2000}
function SetupDiGetDeviceInstallParamsA; external SetupApiModuleName name 'SetupDiGetDeviceInstallParamsA';
function SetupDiGetDeviceInstallParamsW; external SetupApiModuleName name 'SetupDiGetDeviceInstallParamsW';
function SetupDiGetDeviceInstallParams; external SetupApiModuleName name 'SetupDiGetDeviceInstallParamsA';
function SetupDiGetClassInstallParamsA; external SetupApiModuleName name 'SetupDiGetClassInstallParamsA';
function SetupDiGetClassInstallParamsW; external SetupApiModuleName name 'SetupDiGetClassInstallParamsW';
function SetupDiGetClassInstallParams; external SetupApiModuleName name 'SetupDiGetClassInstallParamsA';
function SetupDiSetDeviceInstallParamsA; external SetupApiModuleName name 'SetupDiSetDeviceInstallParamsA';
function SetupDiSetDeviceInstallParamsW; external SetupApiModuleName name 'SetupDiSetDeviceInstallParamsW';
function SetupDiSetDeviceInstallParams; external SetupApiModuleName name 'SetupDiSetDeviceInstallParamsA';
function SetupDiSetClassInstallParamsA; external SetupApiModuleName name 'SetupDiSetClassInstallParamsA';
function SetupDiSetClassInstallParamsW; external SetupApiModuleName name 'SetupDiSetClassInstallParamsW';
function SetupDiSetClassInstallParams; external SetupApiModuleName name 'SetupDiSetClassInstallParamsA';
function SetupDiGetDriverInstallParamsA; external SetupApiModuleName name 'SetupDiGetDriverInstallParamsA';
function SetupDiGetDriverInstallParamsW; external SetupApiModuleName name 'SetupDiGetDriverInstallParamsW';
function SetupDiGetDriverInstallParams; external SetupApiModuleName name 'SetupDiGetDriverInstallParamsA';
function SetupDiSetDriverInstallParamsA; external SetupApiModuleName name 'SetupDiSetDriverInstallParamsA';
function SetupDiSetDriverInstallParamsW; external SetupApiModuleName name 'SetupDiSetDriverInstallParamsW';
function SetupDiSetDriverInstallParams; external SetupApiModuleName name 'SetupDiSetDriverInstallParamsA';
function SetupDiLoadClassIcon; external SetupApiModuleName name 'SetupDiLoadClassIcon';
function SetupDiDrawMiniIcon; external SetupApiModuleName name 'SetupDiDrawMiniIcon';
function SetupDiGetClassBitmapIndex; external SetupApiModuleName name 'SetupDiGetClassBitmapIndex';
function SetupDiGetClassImageList; external SetupApiModuleName name 'SetupDiGetClassImageList';
function SetupDiGetClassImageListExA; external SetupApiModuleName name 'SetupDiGetClassImageListExA';
function SetupDiGetClassImageListExW; external SetupApiModuleName name 'SetupDiGetClassImageListExW';
function SetupDiGetClassImageListEx; external SetupApiModuleName name 'SetupDiGetClassImageListExA';
function SetupDiGetClassImageIndex; external SetupApiModuleName name 'SetupDiGetClassImageIndex';
function SetupDiDestroyClassImageList; external SetupApiModuleName name 'SetupDiDestroyClassImageList';
function SetupDiGetClassDevPropertySheetsA; external SetupApiModuleName name 'SetupDiGetClassDevPropertySheetsA';
function SetupDiGetClassDevPropertySheetsW; external SetupApiModuleName name 'SetupDiGetClassDevPropertySheetsW';
function SetupDiGetClassDevPropertySheets; external SetupApiModuleName name 'SetupDiGetClassDevPropertySheetsA';
function SetupDiAskForOEMDisk; external SetupApiModuleName name 'SetupDiAskForOEMDisk';
function SetupDiSelectOEMDrv; external SetupApiModuleName name 'SetupDiSelectOEMDrv';
function SetupDiClassNameFromGuidA; external SetupApiModuleName name 'SetupDiClassNameFromGuidA';
function SetupDiClassNameFromGuidW; external SetupApiModuleName name 'SetupDiClassNameFromGuidW';
function SetupDiClassNameFromGuid; external SetupApiModuleName name 'SetupDiClassNameFromGuidA';
function SetupDiClassNameFromGuidExA; external SetupApiModuleName name 'SetupDiClassNameFromGuidExA';
function SetupDiClassNameFromGuidExW; external SetupApiModuleName name 'SetupDiClassNameFromGuidExW';
function SetupDiClassNameFromGuidEx; external SetupApiModuleName name 'SetupDiClassNameFromGuidExA';
function SetupDiClassGuidsFromNameA; external SetupApiModuleName name 'SetupDiClassGuidsFromNameA';
function SetupDiClassGuidsFromNameW; external SetupApiModuleName name 'SetupDiClassGuidsFromNameW';
function SetupDiClassGuidsFromName; external SetupApiModuleName name 'SetupDiClassGuidsFromNameA';
function SetupDiClassGuidsFromNameExA; external SetupApiModuleName name 'SetupDiClassGuidsFromNameExA';
function SetupDiClassGuidsFromNameExW; external SetupApiModuleName name 'SetupDiClassGuidsFromNameExW';
function SetupDiClassGuidsFromNameEx; external SetupApiModuleName name 'SetupDiClassGuidsFromNameExA';
function SetupDiGetHwProfileFriendlyNameA; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameA';
function SetupDiGetHwProfileFriendlyNameW; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameW';
function SetupDiGetHwProfileFriendlyName; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameA';
function SetupDiGetHwProfileFriendlyNameExA; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameExA';
function SetupDiGetHwProfileFriendlyNameExW; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameExW';
function SetupDiGetHwProfileFriendlyNameEx; external SetupApiModuleName name 'SetupDiGetHwProfileFriendlyNameExA';
function SetupDiGetWizardPage; external SetupApiModuleName name 'SetupDiGetWizardPage';
function SetupDiGetSelectedDevice; external SetupApiModuleName name 'SetupDiGetSelectedDevice';
function SetupDiSetSelectedDevice; external SetupApiModuleName name 'SetupDiSetSelectedDevice';
function SetupDiGetActualSectionToInstallA; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallA';
function SetupDiGetActualSectionToInstallW; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallW';
function SetupDiGetActualSectionToInstall; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallA';
{$IFDEF WINXP}
function SetupDiGetActualSectionToInstallExA; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallExA';
function SetupDiGetActualSectionToInstallExW; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallExW';
function SetupDiGetActualSectionToInstallEx; external SetupApiModuleName name 'SetupDiGetActualSectionToInstallExA';
function SetupEnumInfSectionsA; external SetupApiModuleName name 'SetupEnumInfSectionsA';
function SetupEnumInfSectionsW; external SetupApiModuleName name 'SetupEnumInfSectionsW';
function SetupEnumInfSections; external SetupApiModuleName name 'SetupEnumInfSectionsA';
function SetupVerifyInfFileA; external SetupApiModuleName name 'SetupVerifyInfFileA';
function SetupVerifyInfFileW; external SetupApiModuleName name 'SetupVerifyInfFileW';;
function SetupVerifyInfFile; external SetupApiModuleName name 'SetupVerifyInfFileA';
function SetupDiGetCustomDevicePropertyA; external SetupApiModuleName name 'SetupDiGetCustomDevicePropertyA';
function SetupDiGetCustomDevicePropertyW; external SetupApiModuleName name 'SetupDiGetCustomDevicePropertyW';
function SetupDiGetCustomDeviceProperty; external SetupApiModuleName name 'SetupDiGetCustomDevicePropertyA';
{$ENDIF WINXP}

{$ENDIF SETUPAPI_LINKONREQUEST}

end.
