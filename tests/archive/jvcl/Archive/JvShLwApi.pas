{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShLwApi.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


(******************************************************************************
*                                                                             *
*                                                                             *
* shlwapi.h - Interface for the Windows light-weight utility APIs             *
*                                                                             *
* Version 1.0                                                                 *
*                                                                             *
* Copyright 1991-1998, Microsoft Corp.      All rights reserved.              *
*                                                                             *
 ******************************************************************************)


//
//=============== String Routines ===================================
//
unit
  JvShLwApi;

interface
uses
  Windows;

function StrChr(lpStart:PChar;wMatch:word):PChar;stdcall;
function StrChrI(lpStart:PChar ;wMatch:WORD ):PChar;stdcall;
function StrCmpN(lpStr1:PChar ; lpStr2:PChar ; nChar:integer ):integer;stdcall;
function StrCSpn(lpStr:PChar ; lpSet:PChar ):integer;stdcall;
function StrNCmpI(lpStr1:PChar ; lpStr2:PChar ; nChar:integer ):integer;stdcall;
function StrCSpnI(lpStr:PChar ; lpSet:PChar ):integer;stdcall;
function StrDup(lpSrch:PChar ):PChar;stdcall;
function StrFormatByteSize(dw:DWORD ; szBuf:PChar ; uiBufSize:UINT ):PChar;stdcall;
function StrFromTimeInterval(pszOut:PChar ; cchMax:UINT ; dwTimeMS:DWORD ; digits:integer ):integer;stdcall;
function StrIsIntlEqual(fCaseSens:boolean ; PCharing1:PChar ; PCharing2:PChar ; nChar:integer ):boolean;stdcall;
function StrNCat(psz1:PChar ; psz2:PChar ; cchMax:integer ):PChar;stdcall;
function StrNCmp(lpStr1:PChar ; lpStr2:PChar ; nChar:integer ):integer;stdcall;
function StrCmpNI(lpStr1:PChar ; lpStr2:PChar ; nChar:integer ):integer;stdcall;
function StrCatN(psz1:PChar ; psz2:PChar ; cchMax:integer ):PChar;stdcall;
function StrPBrk(psz:PChar ; pszSet:PChar ):PChar;stdcall;
function StrRChr(lpStart:PChar ; lpEnd:PChar ; wMatch:WORD ):PChar;stdcall;
function StrRChrI(lpStart:PChar ; lpEnd:PChar ; wMatch:WORD ):PChar;stdcall;
function StrRStrI(lpSource:PChar ; lpLast:PChar ; lpSrch:PChar ):PChar;stdcall;
function StrSpn(psz:PChar ; pszSet:PChar ):integer;stdcall;
function StrStr(lpFirst:PChar ; lpSrch:PChar ):PChar;stdcall;
function StrStrI(lpFirst:PChar ; lpSrch:PChar ):PChar;stdcall;
function StrToInt(lpSrc:PChar ):integer;stdcall;
function StrToIntEx(pszString:PChar ; dwFlags:DWORD ; var piRet:integer):boolean;stdcall;
function StrTrim(psz:PChar ; pszTrimChars:PChar ):boolean;stdcall;
function ChrCmpI(w1:WORD ; w2:WORD ):boolean;stdcall;

{
#define StrIntlEqNA( s1; s2; nChar) StrIsIntlEqualA( TRUE; s1; s2; nChar)
#define StrIntlEqNIA(s1; s2; nChar) StrIsIntlEqualA(FALSE; s1; s2; nChar)
}
// Flags for StrToIntEx
const
  STIF_DEFAULT      =  $00000000;
  STIF_SUPPORT_HEX  =  $00000001;


{
  StrCatA                 lstrcatA;
  StrCmpA                 lstrcmpA;
  StrCmpIA                lstrcmpiA;
  StrCpyA                 lstrcpyA;
  StrCpyNA                lstrcpynA;
  StrToLong               StrToInt;
  StrNCmp                 StrCmpN;
  StrNCmpI                StrCmpNI;
  StrNCpy                 StrCpyN;
  StrCatN                 StrNCat;

  StrCat                  lstrcatA;
  StrCmp                  lstrcmpA;
  StrCmpI                 lstrcmpiA;
  StrCpy                  lstrcpyA;
  StrCpyN                 lstrcpynA;}


function StrCat(lpString1, lpString2: PChar): PAnsiChar; stdcall;
function StrToLong(lpSrc:PChar ):integer;stdcall;
function StrCmp(lpString1, lpString2: PChar): Integer; stdcall;
function StrCmpI(lpString1, lpString2: PChar): Integer; stdcall;
function StrCpyN(lpString1, lpString2: PChar; iMaxLength: Integer): PChar; stdcall;
function StrNCpy(lpString1, lpString2: PChar; iMaxLength: Integer): PChar; stdcall;
function StrCpy(lpString1, lpString2: PChar): PChar; stdcall;

//
//=============== Path Routines ===================================
//

function PathAddBackslash(pszPath:PChar):PChar;stdcall;
function PathAddExtension(pszPath:PChar; pszExt:PChar):boolean;stdcall;
function PathAppend(pszPath:PChar; pMore:PChar):boolean;stdcall;
function PathBuildRoot(szRoot:PChar; iDrive:integer):PChar;stdcall;
function PathCanonicalize(pszBuf:PChar; pszPath:PChar):boolean;stdcall;
function PathCombine(szDest:PChar; lpszDir:PChar; lpszFile:PChar):PChar;stdcall;
function PathCompactPath(hDC:HDC; pszPath:PChar; dx:UINT):boolean;stdcall;
function PathCompactPathEx(pszOut:PChar; pszSrc:PChar; cchMax:UINT; dwFlags:DWORD):boolean;stdcall;
function PathCommonPrefix(pszFile1:PChar; pszFile2:PChar; achPath:PChar):integer;stdcall;
function PathFileExists(pszPath:PChar):boolean;stdcall;
function PathFindExtension(pszPath:PChar):PChar;stdcall;
function PathFindFileName(pszPath:PChar):PChar;stdcall;
function PathFindNextComponent(pszPath:PChar):PChar;stdcall;
function PathFindOnPath(pszPath:PChar; ppszOtherDirs:PChar):boolean;stdcall;
function PathGetArgs(pszPath:PChar):PChar;stdcall;
function PathGetCharType(ch:UCHAR):UINT;stdcall;

// Return flags for PathGetCharType
const
  GCT_INVALID         =    $0000;
  GCT_LFNCHAR         =    $0001;
  GCT_SHORTCHAR       =    $0002;
  GCT_WILD            =    $0004;
  GCT_SEPARATOR       =    $0008;

function PathGetDriveNumber(pszPath:PChar):integer;stdcall;
function PathIsDirectory(pszPath:PChar):boolean;stdcall;
function PathIsFileSpec(pszPath:PChar):boolean;stdcall;
function PathIsPrefix(pszPrefix:PChar; pszPath:PChar):boolean;stdcall;
function PathIsRelative(pszPath:PChar):boolean;stdcall;
function PathIsRoot(pszPath:PChar):boolean;stdcall;
function PathIsSameRoot(pszPath1:PChar; pszPath2:PChar):boolean;stdcall;
function PathIsUNC(pszPath:PChar):boolean;stdcall;
function PathIsUNCServer(pszPath:PChar):boolean;stdcall;
function PathIsUNCServerShare(pszPath:PChar):boolean;stdcall;
function PathIsContentType(pszPath:PChar; pszContentType:PChar):boolean;stdcall;
function PathIsURL(pszPath:PChar):boolean;stdcall;
function PathMakePretty(pszPath:PChar):boolean;stdcall;
function PathMatchSpec(pszFile:PChar; pszSpec:PChar):boolean;stdcall;
function PathParseIconLocation(pszIconFile:PChar):integer;stdcall;
procedure PathQuoteSpaces(lpsz:PChar);stdcall;
function PathRelativePathTo(pszPath:PChar; pszFrom:PChar; dwAttrFrom:DWORD; pszTo:PChar; dwAttrTo:DWORD):boolean;stdcall;
procedure PathRemoveArgs(pszPath:PChar);stdcall;
function PathRemoveBackslash(pszPath:PChar):PChar;stdcall;
procedure PathRemoveBlanks(pszPath:PChar);stdcall;
procedure PathRemoveExtension(pszPath:PChar);stdcall;
function PathRemoveFileSpec(pszPath:PChar):boolean;stdcall;
function PathRenameExtension(pszPath:PChar; pszExt:PChar):boolean;stdcall;
function PathSearchAndQualify(pszPath:PChar; pszBuf:PChar; cchBuf:UINT):boolean;stdcall;
procedure PathSetDlgItemPath(hDlg:HWND; id:integer; pszPath:PChar);stdcall;
function PathSkipRoot(pszPath:PChar):PChar;stdcall;
procedure PathStripPath(pszPath:PChar);stdcall;
function PathStripToRoot(pszPath:PChar):boolean;stdcall;
procedure PathUnquoteSpaces(lpsz:PChar);stdcall;
function PathMakeSystemFolder(pszPath:PChar):boolean;stdcall;
function PathUnmakeSystemFolder(pszPath:PChar):boolean;stdcall;
function PathIsSystemFolder(pszPath:PChar; dwAttrb:DWORD):boolean;stdcall;

//
//=============== Registry Routines ===================================
//

// SHDeleteEmptyKey mimics RegDeleteKey as it behaves on NT.
// SHDeleteKey mimics RegDeleteKey as it behaves on Win95.

function SHDeleteEmptyKey(hkey:HKEY ; pszSubKey:PChar):DWORD;stdcall;
function SHDeleteKey(hkey:HKEY ; pszSubKey:PChar):DWORD;stdcall;
// These functions open the key; get/set/delete the value, then close the key.

function SHDeleteValue(hkey:HKEY; pszSubKey:PChar; pszValue:PChar):DWORD;stdcall;
function SHGetValue(hkey:HKEY; pszSubKey:PChar; pszValue:PChar; pdwType:LPDWORD; pvData:Pointer; pcbData:LPDWORD):DWORD;stdcall;
function SHSetValue(hkey:HKEY; pszSubKey:PChar; pszValue:PChar; dwType:DWORD; pvData:Pointer; cbData:DWORD):DWORD;stdcall;

// These functions work just like RegQueryValueEx; except if the
// data type is REG_EXPAND_SZ; then these will go ahead and expand
// out the string.  *pdwType will always be massaged to REG_SZ
// if this happens.  REG_SZ values are also guaranteed to be null
// terminated.
function SHQueryValueEx(hkey:HKEY; pszValue:PChar; pdwReserved:LPDWORD; pdwType:LPDWORD; pvData:Pointer; pcbData:LPDWORD):DWORD;stdcall;
// Enumeration functions support.
function SHEnumKeyEx(hkey:HKEY; dwIndex:DWORD; pszName:PChar; pcchName:LPDWORD):longint;stdcall;
function SHEnumValue(hkey:HKEY; dwIndex:DWORD; pszValueName:PChar; pcchValueName:LPDWORD; pdwType:LPDWORD; pvData:Pointer; pcbData:LPDWORD):longint;stdcall;
function SHQueryInfoKey(hkey:HKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD):longint;stdcall;

//////////////////////////////////////////////
// User Specific Registry Access Functions
//////////////////////////////////////////////

//
// Type definitions.
//

const
  // Delete's HKCU; or HKLM if HKCU is not found.
  SHREGDEL_DEFAULT = $00000000;
  // Delete HKCU only
  SHREGDEL_HKCU    = $00000001;
  // Delete HKLM only.
  SHREGDEL_HKLM    = $00000010;
  // Delete both HKCU and HKLM.
  SHREGDEL_BOTH    = $00000011;
  // Enumerates HKCU or HKLM if not found.
  SHREGENUM_DEFAULT = $00000000;
  // Enumerates HKCU only
  SHREGENUM_HKCU    = $00000001;
  // Enumerates HKLM only.
  SHREGENUM_HKLM    = $00000010;
  // Enumerates both HKCU and HKLM without duplicates.
  // This option is NYI.
  SHREGENUM_BOTH    = $00000011;



  // Write to HKCU if empty.
  SHREGSET_HKCU         =  $00000001;
  // Write to HKCU.
  SHREGSET_FORCE_HKCU   =  $00000002;
  // Write to HKLM if empty.
  SHREGSET_HKLM         =  $00000004;
  // Write to HKLM.
  SHREGSET_FORCE_HKLM   =  $00000008;
  // Default is SHREGSET_FORCE_HKCU | SHREGSET_HKLM.
  SHREGSET_DEFAULT      =  (SHREGSET_FORCE_HKCU or SHREGSET_HKLM);

type
// HUSKEY is a Handle to a User Specific KEY.
  HUSKey = THandle;
  PHusKey = ^THandle;
  SHREGDEL_FLAGS = integer;

function SHRegCreateUSKey(pszPath:PChar; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; dwFlags:DWORD):longint;stdcall;
function SHRegOpenUSKey(pszPath:PChar; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; fIgnoreHKCU:boolean):longint;stdcall;
function SHRegQueryUSValue(hUSKey:HUSKEY; pszValue:PChar; pdwType:LPDWORD; pvData:Pointer; pcbData:LPDWORD; fIgnoreHKCU:boolean; pvDefaultData:Pointer; dwDefaultDataSize:DWORD):longint;stdcall;
function SHRegWriteUSValue(hUSKey:HUSKEY; pszValue:PChar; dwType:DWORD; pvData:Pointer; cbData:DWORD; dwFlags:DWORD):longint;stdcall;
function SHRegDeleteUSValue(hUSKey:HUSKEY; pszValue:PChar; delRegFlags:integer):longint;stdcall;
function SHRegDeleteEmptyUSKey(hUSKey:HUSKEY; pszSubKey:PChar; delRegFlags:integer):longint;stdcall;
function SHRegEnumUSKey(hUSKey:HUSKEY; dwIndex:DWORD; pszName:PChar; pcchName:LPDWORD; RegFlags:integer):longint;stdcall;
function SHRegEnumUSValue(hUSkey:HUSKEY; dwIndex:DWORD; pszValueName:PChar; pcchValueName:LPDWORD; pdwType:LPDWORD; pvData:Pointer; pcbData:LPDWORD; enumRegFlags:integer):longint;stdcall;
function SHRegQueryInfoUSKey(hUSKey:HUSKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD; enumRegFlags:integer):longint;stdcall;
function SHRegCloseUSKey(hUSKey:HUSKEY):longint;stdcall;


// These calls are equal to an SHRegOpenUSKey; SHRegQueryUSValue; and then a SHRegCloseUSKey.
function SHRegGetUSValue(pszSubKey:PChar; pszValue:PChar; pdwType:LPDWORD; pvData:Pointer ; pcbData:LPDWORD; fIgnoreHKCU:boolean; pvDefaultData:Pointer; dwDefaultDataSize:DWORD):longint;stdcall;
function SHRegSetUSValue(pszSubKey:PChar; pszValue:PChar; dwType:DWORD; pvData:Pointer; cbData:DWORD; dwFlags:DWORD):longint;stdcall;
function SHRegGetbooleanUSValue(pszSubKey:PChar; pszValue:PChar; fIgnoreHKCU:boolean; fDefault:boolean):boolean;stdcall;
//
//=============== Stream Routines ===================================
//
{
#ifdef __IStream_INTERFACE_DEFINED__
IStream *)    SHOpenRegStreamA(HKEY hkey; PChar pszSubkey; PChar pszValue; DWORD grfMode);
IStream *)    SHOpenRegStreamW(HKEY hkey; LPCWSTR pszSubkey; LPCWSTR pszValue; DWORD grfMode);
#define SHOpenRegStream  SHOpenRegStreamA
}
//
//====== GDI helper functions  ================================================
//

function SHCreateShellPalette(hDC:HDC):hPalette;

// HPALETTE SHCreateShellPalette(HDC hdc);

//
//====== DllGetVersion  =======================================================
//

type
  TDLLVersionInfo = packed record
    cbSize:DWord;
    dwMajorVersion:DWord;
    dwMinorVersion:DWord;
    dwBuildNumber:DWord;
    dwPlatformID:DWord;
  end;

// Platform IDs for DLLVERSIONINFO
const
  DLLVER_PLATFORM_WINDOWS = $00000001;      // Windows 95
  DLLVER_PLATFORM_NT      = $00000002;      // Windows NT

//
// The caller should always GetProcAddress("DllGetVersion"); not
// implicitly link to it.
//
{
typedef HRESULT (CALLBACK* DLLGETVERSIONPROC)(DLLVERSIONINFO *);

// DllInstall (to be implemented by self-installing DLLs)
STDAPI DllInstall(boolean bInstall, LPCWSTR pszCmdLine);
}

type
  TDllGetVersionProc=function (var dvi:TDllVersionInfo):integer;

implementation

const
  ShLWApiDll = 'SHLWAPI.DLL';
  kernel32 = 'kernel32.dll';

function StrChr; external ShLwApiDll name 'StrChrA';
function StrChrI; external ShLwApiDll name 'StrChrIA';
function StrCmpN; external ShLwApiDll name 'StrCmpNA';
function StrCmpNI; external ShLwApiDll name 'StrCmpNIA';
function StrNCmpI; external ShLwApiDll name 'StrCmpNIA';
function StrCSpn; external ShLwApiDll name 'StrCSpnA';
function StrCSpnI; external ShLwApiDll name 'StrCSpnIA';
function StrDup; external ShLwApiDll name 'StrDupA';
function StrFormatByteSize; external ShLwApiDll name 'StrFormatByteSizeA';
function StrFromTimeInterval; external ShLwApiDll name 'StrFromTimeIntervalA';
function StrIsIntlEqual; external ShLwApiDll name 'StrIsIntlEqualA';
function StrNCat; external ShLwApiDll name 'StrNCatA';
function StrCatN; external ShLwApiDll name 'StrNCatA';
function StrNCmp; external ShLwApiDll name 'StrCmpNA';
function StrPBrk; external ShLwApiDll name 'StrPBrkA';
function StrRChr; external ShLwApiDll name 'StrRChrA';
function StrRChrI; external ShLwApiDll name 'StrRChrIA';
function StrRStrI; external ShLwApiDll name 'StrRStrIA';
function StrSpn; external ShLwApiDll name 'StrSpnA';
function StrStr; external ShLwApiDll name 'StrStrA';
function StrStrI; external ShLwApiDll name 'StrStrIA';
function StrToInt; external ShLwApiDll name 'StrToIntA';
function StrToIntEx; external ShLwApiDll name 'StrToIntExA';
function StrTrim; external ShLwApiDll name 'StrTrimA';

function StrCat;external kernel32 name 'lstrcatA';
function StrToLong;external kernel32 name 'StrToIntA';
function StrCmp;external kernel32 name 'lstrcmpA';
function StrCmpI;external kernel32 name 'lstrcmpIA';
function StrCpyN;external kernel32 name 'lstrcpynA';
function StrNCpy;external kernel32 name 'lstrcpynA';
function StrCpy;external kernel32 name 'lstrcpyA';


function ChrCmpI; external ShLwApiDll name 'ChrCmpIA';
function PathAddBackslash; external ShLwApiDll name 'PathAddBackslashA';
function PathAddExtension; external ShLwApiDll name 'PathAddExtensionA';
function PathAppend; external ShLwApiDll name 'PathAppendA';
function PathBuildRoot; external ShLwApiDll name 'PathBuildRootA';
function PathCanonicalize; external ShLwApiDll name 'PathCanonicalizeA';
function PathCombine; external ShLwApiDll name 'PathCombineA';
function PathCompactPath; external ShLwApiDll name 'PathCompactPathA';
function PathCompactPathEx; external ShLwApiDll name 'PathCompactPathExA';
function PathCommonPrefix; external ShLwApiDll name 'PathCommonPrefixA';
function PathFileExists; external ShLwApiDll name 'PathFileExistsA';
function PathFindExtension; external ShLwApiDll name 'PathFindExtensionA';
function PathFindFileName; external ShLwApiDll name 'PathFindFileNameA';
function PathFindNextComponent; external ShLwApiDll name 'PathFindNextComponentA';
function PathFindOnPath; external ShLwApiDll name 'PathFindOnPathA';
function PathGetArgs; external ShLwApiDll name 'PathGetArgsA';
function PathGetCharType; external ShLwApiDll name 'PathGetCharTypeA';
function PathGetDriveNumber; external ShLwApiDll name 'PathGetDriveNumberA';
function PathIsDirectory; external ShLwApiDll name 'PathIsDirectoryA';
function PathIsFileSpec; external ShLwApiDll name 'PathIsFileSpecA';
function PathIsPrefix; external ShLwApiDll name 'PathIsPrefixA';
function PathIsRelative; external ShLwApiDll name 'PathIsRelativeA';
function PathIsRoot; external ShLwApiDll name 'PathIsRootA';
function PathIsSameRoot; external ShLwApiDll name 'PathIsSameRootA';
function PathIsUNC; external ShLwApiDll name 'PathIsUNCA';
function PathIsUNCServer; external ShLwApiDll name 'PathIsUNCServerA';
function PathIsUNCServerShare; external ShLwApiDll name 'PathIsUNCServerShareA';
function PathIsContentType; external ShLwApiDll name 'PathIsContentTypeA';
function PathIsURL; external ShLwApiDll name 'PathIsURLA';
function PathMakePretty; external ShLwApiDll name 'PathMakePrettyA';
function PathMatchSpec; external ShLwApiDll name 'PathMatchSpecA';
function PathParseIconLocation; external ShLwApiDll name 'PathParseIconLocationA';
procedure PathQuoteSpaces; external ShLwApiDll name 'PathQuoteSpacesA';
function PathRelativePathTo; external ShLwApiDll name 'PathRelativePathToA';
procedure PathRemoveArgs; external ShLwApiDll name 'PathRemoveArgsA';
function PathRemoveBackslash; external ShLwApiDll name 'PathRemoveBackslashA';
procedure PathRemoveBlanks; external ShLwApiDll name 'PathRemoveBlanksA';
procedure PathRemoveExtension; external ShLwApiDll name 'PathRemoveExtensionA';
function PathRemoveFileSpec; external ShLwApiDll name 'PathRemoveFileSpecA';
function PathRenameExtension; external ShLwApiDll name 'PathRenameExtensionA';
function PathSearchAndQualify; external ShLwApiDll name 'PathSearchAndQualifyA';
procedure PathSetDlgItemPath; external ShLwApiDll name 'PathSetDlgItemPathA';
function PathSkipRoot; external ShLwApiDll name 'PathSkipRootA';
procedure PathStripPath; external ShLwApiDll name 'PathStripPathA';
function PathStripToRoot; external ShLwApiDll name 'PathStripToRootA';
procedure PathUnquoteSpaces; external ShLwApiDll name 'PathUnquoteSpacesA';
function PathMakeSystemFolder; external ShLwApiDll name 'PathMakeSystemFolderA';
function PathUnmakeSystemFolder; external ShLwApiDll name 'PathUnmakeSystemFolderA';
function PathIsSystemFolder; external ShLwApiDll name 'PathIsSystemFolderA';
function SHDeleteEmptyKey; external ShLwApiDll name 'SHDeleteEmptyKeyA';
function SHDeleteKey; external ShLwApiDll name 'SHDeleteKeyA';
function SHDeleteValue; external ShLwApiDll name 'SHDeleteValueA';
function SHGetValue; external ShLwApiDll name 'SHGetValueA';
function SHSetValue; external ShLwApiDll name 'SHSetValueA';
function SHQueryValueEx; external ShLwApiDll name 'SHQueryValueExA';
function SHEnumKeyEx; external ShLwApiDll name 'SHEnumKeyExA';
function SHEnumValue; external ShLwApiDll name 'SHEnumValueA';
function SHQueryInfoKey; external ShLwApiDll name 'SHQueryInfoKeyA';
function SHRegCreateUSKey; external ShLwApiDll name 'SHRegCreateUSKeyA';
function SHRegOpenUSKey; external ShLwApiDll name 'SHRegOpenUSKeyA';
function SHRegQueryUSValue; external ShLwApiDll name 'SHRegQueryUSValueA';
function SHRegWriteUSValue; external ShLwApiDll name 'SHRegWriteUSValueA';
function SHRegDeleteUSValue; external ShLwApiDll name 'SHRegDeleteUSValueA';
function SHRegDeleteEmptyUSKey; external ShLwApiDll name 'SHRegDeleteEmptyUSKeyA';
function SHRegEnumUSKey; external ShLwApiDll name 'SHRegEnumUSKeyA';
function SHRegEnumUSValue; external ShLwApiDll name 'SHRegEnumUSValueA';
function SHRegQueryInfoUSKey; external ShLwApiDll name 'SHRegQueryInfoUSKeyA';
function SHRegCloseUSKey; external ShLwApiDll name 'SHRegCloseUSKey';
function SHRegGetUSValue; external ShLwApiDll name 'SHRegGetUSValueA';
function SHRegSetUSValue; external ShLwApiDll name 'SHRegSetUSValueA';
function SHRegGetBooleanUSValue; external ShLwApiDll name 'SHRegGetbooleanUSValueA';
function SHCreateShellPalette; external ShLwApiDll name 'SHCreateShellPalette';

end.
