{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShDlg.PAS, released on 2002-05-26.

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

{ Various undocumented shell functions. }

unit JvShDlg;

interface
uses
  Windows, SysUtils, ShlObj;

const
  SHERB_NOCONFIRMATION = $00000001;
  SHERB_NOPROGRESSUI = $00000002;
  SHERB_NOSOUND = $00000004;
  // removes the browse button
  RFF_NOBROWSE = $01;
  // no default item selected
  RFF_NODEFAULT = $02;
  // extracts the working directory from the filename
  RFF_CALCDIRECTORY = $04;
  // removes the edit box label
  RFF_NOLABEL = $08;
  // removes the "Separate Memory Space" check box (WinNT only)
  RFF_NOSEPARATEMEM = $20;

  // Allow application to run
  RF_OK = $00;
  // Cancel operation and close dialog
  RF_CANCEL = $01;
  // Cancel but keep dialog open
  RF_RETRY = $02;

type
  _SHQUERYRBINFO = record
    cbSize: DWORD;
    i64Size: int64;
    i64NumItems: int64;

  end;

{$EXTERNALSYM _SHQUERYRBINFO}
  TSHQueryRBInfo = _SHQUERYRBINFO;
  PSHQueryRBInfo = ^TSHQueryRBInfo;

  NM_RUNFILEDLG = record
    hdr: NMHDR;
    lpFile: LPCSTR;
    lpDirectory: LPCSTR;
    nShow: integer;
  end;

{$EXTERNALSYM NM_RUNFILEDLG }

  TNMRunFileDlgA = NM_RUNFILEDLG;
  PNMRunFileDlgA = ^TNMRunFileDlgA;
  NM_RUNFILEDLGW = record
    hdr: NMHDR;
    lpFile: LPCWSTR;
    lpDirectory: LPCWSTR;
    nShow: integer;
  end;
  PNMRunFileDlgW = ^TNMRunFileDlgW;
  TNMRunFileDlgW = NM_RUNFILEDLGW;
  TNMRunFileDlg = TNMRunFileDlgA;
  PNMRunFileDlg = ^TNMRunFileDlg;

  {
  Retrieves the size of the Recycle Bin, and the number of items in it, on the specified drive.
<p>
  Returns S_OK if successful, or an OLE-defined error value otherwise.
<p>
pszRootPath -  Address of a NULL-terminated string to contain the path of the root drive on which
  the Recycle Bin is located. This parameter can contain the address of a string
  formatted with the drive, folder, and subfolder names (c:\windows\system . . .).
  It can also contain an empty string or NULL. If this value is an empty string or
  NULL, information is retrieved for all Recycle Bins on all drives.
<p>
  pSHQueryRBInfo - Address of a SHQUERYRBINFO structure that receives the Recycle Bin information.
  The cbSize member of the structure must be set to the size of the structure before
  calling this API.
  }
function SHQueryRecycleBin(pszRootPath: PChar; var ShQueryRBInfo: TSHQueryRBInfo): HRESULT; stdcall;
function SHQueryRecycleBinW(pszRootPath: PWideChar; var ShQueryRBInfo: TSHQueryRBInfo): HRESULT; stdcall;
{
Empties the Recycle Bin on the specified drive.
<p>
Returns S_OK if successful, or an OLE-defined error value otherwise.
<p>
hwnd - Handle to the parent window of any dialog boxes that might be displayed during
the operation. This parameter can be NULL.
<p>
pszRootPath - Address of a NULL-terminated string that contains the path of the root drive on
which the Recycle Bin is located. This parameter can contain the address of a
string formatted with the drive, folder, and subfolder names (c:\windows\system . . .).
It can also contain an empty string or NULL. If this value is an empty string or
NULL, all Recycle Bins on all drives will be emptied.
<p>
dwFlags - One or more of the following values:
<br>
SHERB_NOCONFIRMATION  No dialog confirming the deletion of the objects will be displayed.
<br>
SHERB_NOPROGRESSUI  No dialog indicating the progress will be displayed.
<br>
SHERB_NOSOUND  No sound will be played when the operation is complete.
}
function SHEmptyRecycleBin(hwnd: HWND; pszRootPath: PChar; dwFlags: DWORD): HRESULT; stdcall;
procedure SHUpdateRecycleBinIcon; stdcall;
{
Shows the Restart Dialog.
<p>
  hWnd - is a handle to the calling window
<p>
lpStrReason -is any text to prepend to the default text. Place a hash
  (#) first in the string to replace the default text
<p>uFlags - can be any of the following:
<br>EWX_LOGOFF,EWX_SHUTDOWN,EWX_REBOOT,EWX_RESTARTWINDOWS,EWX_REBOOTSYSTEM,EWX_EXITANDEXECAPP
<p>
  Returns IDYES if the user choose the Yes button or IDNO otherwise.
  Even if the restart fails, IDYES can be returned
}
function ShRestartDialog(hWnd: HWND; lpstrReason: PChar; uFlags: UINT): integer;
{ On Windows 95 the hwndOwner will receive a WM_QUIT message if the user choose to exit.
  On WinNT, hwndOwner is ignored }
procedure ShExitWinDlg(hWndOwner: HWND); stdcall;
{
Shows the Windows "Run" dialog
<p>
hwndOwner - is a handle to the dialogs owner
<p>
hIcon - is a handle to an icon to display, if 0, the default icon is displayed
<p>
lpStrDirectory - specifies the working directory
<p>
lpStrTitle - is the Title of the dialog
<p>
lpStrDescription - is a brief description to the user on what to do
<p>
uFlags - can be any of the following:
<br>RFF_NOBROWSE,RFF_NODEFAULT,RFF_CALCDIRECTORY,RFF_NOLABEL,RFF_NOSEPARATEMEM
<p>
After the user clicks the OK button,a WM_NOTIFY message is sent to hwndOwner
with the notification code RFN_VALIDATE ( = -510) and the lParam points to a
NM_RUNFILEDLG structure (see below).

<p>
The return value determines whether the application will run or not. Returned value can be any of the follwing:
<br>RF_OK,RF_CANCEL,RF_RETRY
}
procedure ShRunFileDlg(hwndOwner: HWND; hIcon: HICON; lpStrDirectory, lpStrTitle, lpStrDescription: LPCSTR; uFlags: UINT); stdcall;
function ShPickIconDlg(hwndOwner: HWND; var lpstrFile: PChar; nMaxFile: DWORD; var lpdwIconIndex: integer): bool; stdcall;
function SHFindFiles(pidlRoot, pidlSavedSearch: PItemIDList): bool; stdcall;
function SHFindComputer(pidlRoot, pidlSavedSearch: PItemIDList): bool stdcall;
{ lpStrParameters contains the *localized* caption of the tab to display... If nil, the first page is displayed }
function SHObjectProperties(hwndOwner: HWND; uFlags: UINT; lpStrName, lpStrParameters: PChar): bool;
{ EmptyBin empties the Recycle Bin on the drive specified by RootPath. If RootPath is empty, all
  RecycleBins on all drives are emptied }
function EmptyBin(RootPath: string; Confirm: boolean): integer;
{ RecycleBinSize returns the total size of all files the Recycle Bin on the drive specified by RootPath.
If RootPath is empty, the size is computed over all drives having a Recycle Bin }
function RecycleBinSize(RootPath: string): integer;
{ RecycleBinCount returns the total count of all files the Recycle Bin on the drive specified by RootPath.
  If RootPath is empty, the count is computed over all drives having a Recycle Bin }
function RecycleBinCount(RootPath: string): integer;
{ Runs the Control Panel application given by CPL. CPL can be the name of an existing
  CPL file (must include extension) or one of the following identifiers:
<br>
<table>
<tr>
<th>DESKTOP</th><td>(desk.cpl)</td>
</tr>
<tr>
<th>COLOR</th><td>(desk.cpl,,2)</td>
</tr>
<tr>
<th>DATE/TIME</th><td>(timedate.cpl)</td>
</tr>
<tr>
<th>PORTS</th><td>(sysdm.cpl,,1)</td>
</tr>
<tr>
<th>INTERNATIONAL</th><td>(intl.cpl)</td>
</tr>
<tr>
<th>MOUSE</th><td>(main.cpl)</td>
</tr>
<tr>
<th>KEYBOARD</th><td>(main.cpl @@1)</td>
</tr>
<tr>
<th>PRINTERS</th><td>(main.cpl @@2)</td>
</tr>
<tr>
<th>FONTS</th><td>(main.cpl @@3)</td>
</tr>
</table>

<p>
In addition any the following might work:
<table>
<tr>
<th>Main.cpl @@4</th><td>Power Management (Win95)</td>
</tr>
<tr>
<th>Main.cpl @@5</th><td>PC Cards (Win95) - (same as devapps.cpl in WinNT)</td>
</tr>
<tr>
<th>desk.cpl,,1</th><td>Screensaver page active</td>
</tr>
<tr>
<th>desk.cpl,,3</th><td>Settings page active</td>
</tr>
<tr>
<th>timedate.cpl,,1</th><td>Timezone page active</td>
</tr>
</table>
<p>
If CPL is empty, the main Control Panel window is opened. Most CPL files that
have multiple pages can be called with the following format:
<br>
<code>name.cpl,,index_of_page</code>
<br>
If a CPL file contains the code for more than one dialog (as main.cpl), the
different dialogs can be called using the following format:
<br>
<code>name.cpl @@index_of_dialog</code>
<br>
Note that there are differences between WinNT and Win95 and Win98: only
experimentation can determine if a certain CPL file has the same name and / or
is available on all systems.
}

procedure ControlPanel(const CPL: string);

implementation
uses
  ShellAPI;

const
  shell32 = 'shell32.dll';
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
function SHQueryRecycleBinW(pszRootPath: PWideChar; var ShQueryRBInfo: TSHQueryRBInfo): HRESULT; external shell32 name 'SHQueryRecycleBinW';
function SHQueryRecycleBin(pszRootPath: PChar; var ShQueryRBInfo: TSHQueryRBInfo): HRESULT; external shell32 name 'SHQueryRecycleBinA';
function SHEmptyRecycleBin(hwnd: HWND; pszRootPath: PChar; dwFlags: DWORD): HRESULT; external shell32 name 'SHEmptyRecycleBinA';
procedure SHUpdateRecycleBinIcon; external shell32 name 'SHUpdateRecycleBinIcon';
procedure ShExitWinDlg(hWndOwner: HWND); external shell32 index 60;
function ShRestartDialogA(hWnd: HWND; lpstrReason: PChar; uFlags: UINT): integer; external shell32 index 59;
function ShRestartDialogW(hWnd: HWND; lpstrReason: PWideChar; uFlags: UINT): integer; external shell32 index 59;
function ShPickIconDlgA(hwndOwner: HWND; var lpstrFile: PChar; nMaxFile: DWORD; var lpdwIconIndex: integer): bool; external shell32 index 62;
function ShPickIconDlgW(hwndOwner: HWND; var lpstrFile: PWideChar; nMaxFile: DWORD; var lpdwIconIndex: integer): bool; external shell32 index 62;
procedure ShRunFileDlgA(hwndOwner: HWND; hIcon: HICON; lpStrDirectory, lpStrTitle, lpStrDescription: LPCSTR; uFlags: UINT); external shell32 index 61
procedure ShRunFileDlgW(hwndOwner: HWND; hIcon: HICON; lpStrDirectory, lpStrTitle, lpStrDescription: LPCWSTR; uFlags: UINT); external shell32 index 61
function SHFindFiles(pidlRoot, pidlSavedSearch: PItemIDList): bool; external shell32 index 90;
function SHFindComputer(pidlRoot, pidlSavedSearch: PItemIDList): bool; external shell32 index 91;
function SHObjectPropertiesA(hwndOwner: HWND; uFlags: UINT; lpStrName, lpStrParameters: PChar): bool; external shell32 index 178;
function SHObjectPropertiesW(hwndOwner: HWND; uFlags: UINT; lpStrName, lpStrParameters: PWideChar): bool; external shell32 index 178;

function ShRestartDialog(hWnd: HWND; lpstrReason: PChar; uFlags: UINT): integer;
var P: array[0..MAX_PATH] of WideChar;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := ShRestartDialogW(hwnd, StringToWideChar(string(lpStrReason), P, MAX_PATH), uFlags)
  else
    Result := ShRestartDialogA(hwnd, lpStrReason, uFlags);
end;

procedure ShRunFileDlg(hwndOwner: HWND; hIcon: HICON; lpStrDirectory, lpStrTitle, lpStrDescription: LPCSTR; uFlags: UINT);
var P: array[0..MAX_PATH] of WideChar;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    ShRunFileDlgW(hwndOwner, hIcon, StringToWideChar(string(lpStrDirectory), P, MAX_PATH),
      StringToWideChar(string(lpStrTitle), P, MAX_PATH), StringToWideChar(string(lpStrDescription), P, MAX_PATH), uFlags)
  else
    ShRunFileDlgA(hwndOwner, hIcon, lpStrDirectory, lpStrTitle, lpStrDescription, uFlags)
end;

function ShPickIconDlg(hwndOwner: HWND; var lpstrFile: PChar; nMaxFile: DWORD; var lpdwIconIndex: integer): bool;
var P: PWideChar;
begin
  GetMem(P, sizeof(Widechar) * (nMaxFile + 1));
  P := StringToWideChar(string(lpStrFile), P, MAX_PATH);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Result := ShPickIconDlgW(hwndOwner, P, nMaxFile, lpdwIconIndex);
    lpStrFile := PChar(WideCharToString(P));
  end
  else
    Result := ShPickIconDlgA(hwndOwner, lpStrFile, nMaxFile, lpdwIconIndex);
  FreeMem(P);
end;

function SHObjectProperties(hwndOwner: HWND; uFlags: UINT; lpStrName, lpStrParameters: PChar): bool;
var P: array[0..MAX_PATH] of WideChar;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := SHObjectPropertiesW(hwndOwner, uFLags, StringToWideChar(string(lpStrName), P, MAX_PATH),
      StringToWideChar(string(lpStrParameters), P, MAX_PATH))
  else
    Result := SHObjectPropertiesA(hwndOwner, uFLags, lpStrName, lpStrParameters);
end;

function EmptyBin(RootPath: string; Confirm: boolean): integer;
var Flags: integer;
begin
  if Confirm then
    Flags := 0
  else
    Flags := SHERB_NOCONFIRMATION;
  Result := SHEmptyRecycleBin(0, PChar(RootPath), Flags);
end;

function RecycleBinSize(RootPath: string): integer;
var rb: TSHQueryRBInfo;
begin
  FillChar(rb, sizeof(rb), #0);
  rb.cbSize := sizeof(rb);
  SHQueryRecycleBin(PChar(RootPath), rb);
  Result := Trunc(rb.i64Size / 10);
end;

function RecycleBinCount(RootPath: string): integer;
var rb: TSHQueryRBInfo;
begin
  FillChar(rb, sizeof(rb), #0);
  rb.cbSize := sizeof(rb);
  SHQueryRecycleBin(PChar(RootPath), rb);
  Result := rb.i64NumItems;
end;

function AddPathBackslash(Path:string):string;
begin
  Result := Path;
  if (Length(Path) > 1) and (AnsiLastChar(Path) <> '\') then
    Result := Path + '\';
end;

function GetSystemDir:string;
var Buff:array[0..MAX_PATH] of char;
begin
  FillChar(Buff,MAX_PATH,#0);
  GetSystemDirectory(Buff,MAX_PATH);
  Result := Buff;
end;

function GetWinDir:string;
var Buff:array[0..MAX_PATH] of char;
begin
  FillChar(Buff,MAX_PATH,#0);
  GetWindowsDirectory(Buff,MAX_PATH);
  Result := Buff;
end;

procedure Run(AppName, Params: string; Hide: boolean);
var Flag: integer;
begin
  if Hide then Flag := SW_HIDE
  else Flag := SW_NORMAL;
  ShellExecute(0, PChar('open'), PChar(AppName), PChar(Params), PChar(''), Flag);
end;

procedure ControlPanel(const CPL: string);
var S: string;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    S := AddPathBackSlash(GetSystemDir) + 'Control.exe'
  else
    S := AddPathBackSlash(GetWinDir) + 'Control.exe';
  Run(S, CPL, false);
end;

{
Retrieves disk space information for a disk volume.

Returns non-zero if successful or zero otherwise.
pszVolume
A NULL-terminated string that specifies the volume for which size information will be retrieved. This can be a drive letter, UNC name or the path of a folder.
pqwFreeCaller
Address of a ULARGE_INTEGER value that receives the number of bytes available to the caller on the volume. If the operating system implements per-user quotas, this value may be less than the total number of free bytes on the volume.
pqwTot
Address of a ULARGE_INTEGER value that receives the total size of the volume, in bytes.
pqwFree
Address of a ULARGE_INTEGER value that receives the number of bytes of free space on the volume.
SHGetDiskFreeSpace is nearly identical to the GetDiskFreeSpaceEx API. SHGetDiskFreeSpace will load the proper module, obtain the address of GetDiskFreeSpaceEx and then call it. This prevents the caller from having to perform these operations themselves. See the operating system restrictions for GetDiskFreeSpaceEx for more information.

This function is implemented in shell versions 4.71 and later. In order to
maintain backward compatibility with previous shell versions, this function
should not be used explicitly. Instead, the LoadLibrary and GetProcAddress
functions should be used to obtain the function address.
}

function SHGetDiskFreeSpace(pszVolume: LPCSTR;
  var pqwFreeCaller, pqwTot, pqwFree: {$IFDEF D4_AND_UP}Int64{$ELSE}TLargeInteger{$ENDIF}): bool; stdcall; external shell32;
{
SHGetFolderPath
Retrieves the path of a folder and returns the path name.

Syntax

Return Values

S_OK Success.
S_FALSE The CSIDL in nFolder is valid, but the folder does not exist.
E_INVALIDARG The CSIDL in nFolder is not valid.

A standard OLE-defined error result may also be returned.

Parameters

hwndOwner
Handle to an owner window. This parameter is typically set to NULL. If it is not NULL, and a dial-up connection needs to be made to access the folder, a UI prompt will appear in this window.
nFolder
CSIDL value that identifies the folder whose path is to be retrieved. Only real folders are valid. If a virtual folder is specified, this function will fail. You can force creation of a folder with SHGetFolderLocation by combining the folder's CSIDL with CSIDL_FLAG_CREATE.
hToken
Token that can be used to represent a particular user. It is usually set to NULL, but it may be needed when there are multiple users for those folders that are treated as belonging to a single user. The caller is responsible for correct impersonation when hToken is non-NULL. It must have appropriate security privileges for the particular user, and the user's registry hive must be currently mounted.
dwReserved
Reserved. Must be set to zero.
pszPath
Buffer of length [MAX_PATH] to receive the path. If an error occurs or S_FALSE is returned, this string will be empty.
Example

The following code fragment uses SHGetFolderPath to find or create a folder and
then creates a file in it.

TCHAR szPath[MAX_PATH]
...
if(SUCCEEDED(SHGetFolderPath(NULL, CSIDL_PERSONAL|CSIDL_FLAG_CREATE, NULL, O, szPath))
(
 PathAppend(szPath, TEXT("New Doc.txt"));
 HANDLE hFile = CreateFile(szPath, ...);
)
Remarks

This function is a superset of SHGetSpecialFolderPath, included with earlier versions of the shell. It is also implemented in a redistributable DLL, SHFolder.dll, that simulates many of the new shell folders on older platforms such as Windows 95 and Windows NT 4.0. This DLL always calls the current platform's version of this function. If that fails, it will try to simulate the appropriate behavior. Only some CSIDLs are supported, including: CSIDL_PERSONAL My Documents
CSIDL_APPDATA c:\...\User\Application Data
CSIDL_MYPICTURES My Documents\My Pictures
CSIDL_LOCAL_APPDATA \...\User\Local Application Data (nonroaming)
  Version 5.00 and later of shell32.dll
}

function SHGetFolderPath(hwndOwner: HWND; nFolder: integer; hToken: THandle;
  dwReserved: DWORD; pszPath: LPCSTR): HResult; stdcall; external shell32;
{
SHGetNewLinkInfo

Creates the proper name for a new shortcut. This function does not actually create a shortcut.

Returns non-zero if successful or zero otherwise.
pszLinkTo
Specifies the path and file name of the target of the shortcut. If uFlags does not contain the SHGNLI_PIDL value, this parameter is the address of a NULL-terminated string that contains the target. If uFlags contains the SHGNLI_PIDL value, this parameter is a PIDL that represents the target.
pszDir
Address of a NULL-terminated string that contains the path of the folder that the shortcut would be created in.
pszName
Address of a string that receives the NULL-terminated path and file name for the shortcut. This buffer is assumed to be at least MAX_PATH characters in size.
pfMustCopy
Address of a BOOL value that receives a flag that indicates if the shortcut would be copied. When a shortcut to another shortcut is created, the shell simply copies the target shortcut and modifies that copied shortcut appropriately. This value receives a non-zero value if the target specified in pszLinkTo specifies a shortcut that will cause the target shortcut to be copied. This value receives zero if the target does not specify a shortcut that would be copied.
uFlags
Specifies the options for the function. This can be zero or a combination of the
following values:
SHGNLI_PIDL pszLinkTo is a PIDL that represents the target. If this flag is not
            included, pszLinkTo is the address of a string that contains the path and file
            name of the target.
SHGNLI_NOUNIQUE The function will not create a unique name within the destination
                folder. If this flag is not included, the function create the shortcut name and
                then determine if the name is unique in the destination folder. If a file with
                the same name exists within the destination folder, the shortcut name will be modified.
                This process is repeated until a unique name is found.
SHGNLI_PREFIXNAME The created name will be preceded by the string "Shortcut to ".

SHGetNewLinkInfo will determine the executable type of the target. If pszLinkTo specifies
a DOS application, the ".PIF" extension will be used, otherwise the ".LNK" extension
will be used for the shortcut.

SHGetNewLinkInfo will also determine if the destination file system supports
long file names. If the destination file system does support long file names,
then a long file name will be used for the shortcut name. If the destination
file system does not support long file names, then the shortcut name will be
returned in an 8.3 format.

This function is implemented in shell versions 4.71 and later. In order to maintain backward compatibility with previous shell versions, this function should not be used explicitly. Instead, the LoadLibrary and GetProcAddress functions should be used to obtain the function address.
}

function SHGetNewLinkInfo(pszLinkTo, pszDir, pszName: LPCSTR; var pfMustCopy: boolean; uFlags: UINT): boolean; stdcall; external shell32;

{
Retrieves the current shell option settings.

lpsfs
Address of a SHELLFLAGSTATE structure that receives the shell option settings.
dwMask
Set of flags that determine which members of lpsfs are being requested.
This can be one or more of the following values:
SSF_DESKTOPHTML  The fDesktopHTML member is being requested.
SSF_DONTPRETTYPATH  The fDontPrettyPath member is being requested.
SSF_DOUBLECLICKINWEBVIEW  The fDoubleClickInWebView member is being requested.
SSF_HIDEICONS  The fHideIcons member is being requested.
SSF_MAPNETDRVBUTTON  The fMapNetDrvBtn member is being requested.
SSF_NOCONFIRMRECYCLE  The fNoConfirmRecycle member is being requested.
SSF_NOWEBVIEW  The fNoWebView member is being requested.
SSF_SHOWALLOBJECTS  The fShowAllObjects member is being requested.
SSF_SHOWATTRIBCOL  The fShowAttribCol member is being requested.
SSF_SHOWCOMPCOLOR  The fShowCompColor member is being requested.
SSF_SHOWEXTENSIONS  The fShowExtensions member is being requested.
SSF_SHOWINFOTIP  The fShowInfoTip member is being requested.
SSF_SHOWSYSFILES  The fShowSysFiles member is being requested.
SSF_WIN95CLASSIC  The fWin95Classic member is being requested.

  Version 4.00 and later of shell32.dll
}
type
  LPSHELLFLAGSTATE = record // ???
  end;
  TShellFlagState = LPSHELLFLAGSTATE;

procedure SHGetSettings(var lpsfs: LPSHELLFLAGSTATE; dwMask: DWORD); stdcall; external shell32;
{
SHGetSpecialFolderPath

Retrieves the path of a special folder, identified by its CSIDL.

Syntax

Parameters

hwndOwner
Handle to the owner window the client should specify if it displays a dialog box or message box.
lpszPath
Address of a character buffer that receives the drive and path of the specified folder. This buffer must be at least MAX_PATH characters in size.
nFolder
Folder of interest. It must be one of the CSIDL_XXX values. This argument must specify a real folder. If a virtual folder is specified, this function will fail.
fCreate
Indicates if the folder should be created if it does not already exist. If this value is nonzero, the folder will be created. If this value is zero, the folder will not be created.
Return Values

Returns TRUE if successful, or FALSE otherwise.

Remarks

With Windows NT® 5.0, this function is superseded by ShGetFolderPath.

QuickInfo
  Version 4.71 and later of shell32.dll
}

function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: LPCSTR; nFolder: integer; fCreate: boolean): boolean; stdcall; external shell32;
{
Executes a command on a printer object.

Returns non-zero if successful or zero otherwise.
hwnd
Handle of the window that will be used as the parent of any windows or dialog boxes that are created during the operation.
uAction
A value that determines the type of printer operation that will be performed. This can be one of the following values: PRINTACTION_DOCUMENTDEFAULTS Windows NT® only. The default document properties for the printer specified by the name in lpBuf1 will be displayed. lpBuf2 is ignored.
PRINTACTION_NETINSTALL The network printer specified by the name in lpBuf1 will be installed. lpBuf2 is ignored.
PRINTACTION_NETINSTALLLINK A shortcut to the network printer specified by the name in lpBuf1 will be created. lpBuf2 specifies the drive and path of the folder in which the shortcut will be created. The network printer must have already been installed on the local machine.
PRINTACTION_OPEN The printer specified by the name in lpBuf1 will be opened. lpBuf2 is ignored.
PRINTACTION_OPENNETPRN The network printer specified by the name in lpBuf1 will be opened. lpBuf2 is ignored.
PRINTACTION_PROPERTIES The properties for the printer specified by the name in lpBuf1 will be displayed. lpBuf2 can either be NULL or specify.
PRINTACTION_SERVERPROPERTIES Windows NT® only. The properties for the server of the printer specified by the name in lpBuf1 will be displayed. lpBuf2 is ignored.
PRINTACTION_TESTPAGE A test page will be printed on the printer specified by the name in lpBuf1. lpBuf2 is ignored.

lpBuf1
Address of a string that contains additional information for the printer command.
The information contained in this parameter depends upon the value of uAction.
lpBuf2
Address of a string that contains additional information for the printer command.
The information contained in this parameter depends upon the value of uAction.
fModal
A value that determines whether SHInvokePrinterCommand should return after
initializing the command or wait until the command is completed. If this value
is non-zero, SHInvokePrinterCommand will not return until the command is completed.
If this value is zero, SHInvokePrinterCommand will return as soon as the command
is initialized.
When a printer name is specified by lpBuf1, the name can either be the name of a
local printer or the server and share name of a network printer.
When specifying a network printer name, the name must be specified in the
format of "\\<server>\<shared printer name>".

This function is implemented in shell versions 4.71 and later. In order to
maintain backward compatibility with previous shell versions, this function should
not be used explicitly. Instead, the LoadLibrary and GetProcAddress functions
should be used to obtain the function address.

QuickInfo
  Version 4.71 and later of shell32.dll
}

function SHInvokePrinterCommand(hwnd: HWND; uAction: UINT; lpBuf1, lpBuf2: LPCSTR; fModal: boolean): boolean; stdcall; external shell32;

(*

  Windows NT: Requires version 5.0 or later (or version 4.0 with Internet Explorer 4.0 or later).
  Windows: Requires Windows 98 (or Windows 95 with Internet Explorer 4.0 or later).
  Windows CE: Unsupported.
  Header: Declared in shlobj.h.
  Import Library: shell32.lib.

SoftwareUpdateMessageBox

SHDOCAPI_(DWORD) SoftwareUpdateMessageBox(
    HWND hWnd,
    LPCWSTR szDistUnit,
    DWORD dwFlags,
    LPSOFTDISTINFO psdi
);
Displays a standard message box that can be used to notify a user that an application
has been updated.

Returns one of the following values.
IDNO The user clicked the Don't Update button on the dialog box.
IDYES The user clicked the Update Now or About Update button. The application
should navigate to the HTML page referred to by the szHREF member of the structure
pointed to by psdi.
IDIGNORE There is no pending software update.
IDABORT An error occurred.

hWnd
Handle to the parent window.
szDistUnit
String value containing the identifier for the code distribution unit. For
ActiveX® controls, szDistUnit is typically a globally unique identifier (GUID).
dwFlags
Reserved. Must be set to zero.
psdi
Address of a SOFTDISTINFO structure in which to store the update information.
The cbSize member must be initialized to the sizeof(SOFTDISTINFO).
Remarks
The preferred way to handle updates is to author a Channel Definition Format
with an Open Software Description (OSD) vocabulary inside and make the shortcut
OSD-aware. Refer to the Channel Definition Format documentation for details
(Site Builder Network).

The SoftwareMessageBox function is intended to be used in the case where shell
shortcut hooks do not work. One example is an application that was not installed
on the start menu. If that application needs to do it's own software update check
it should use this function.

QuickInfo
  Version 4.71 and later of shell32.dll

  Windows NT: Requires version 5.0 or later (or version 4.0 with Internet Explorer 4.0 or later).
  Windows: Requires Windows 98 (or Windows 95 with Internet Explorer 4.0 or later).
  Windows CE: Unsupported.
  Header: Declared in shlobj.h.
*)
(*
TranslateURL
Applies common translations to a given URL string, creating a new URL string.

HRESULT TranslateURL(
  LPCSTR pcszURL,
  DWORD dwInFlags,
  LPSTR *ppszTranslatedURL,
);

Parameters
pcszURL
Address of the URL string to be translated.
dwInFlags
Bit flags that specify how the URL string is to be translated. This value can be
a combination of the following:
TRANSLATEURL_FL_GUESS_PROTOCOL
If the protocol scheme is not specified in the pcszURL parameter to TranslateURL,
the system automatically chooses a scheme and adds it to the URL.
TRANSLATEURL_FL_USE_DEFAULT_PROTOCOL
If the protocol scheme is not specified in the pcszURL parameter to TranslateURL,
the system adds the default protocol to the URL.
ppszTranslatedURL
Pointer variable that receives the pointer to the newly created, translated URL
string, if any. The *ppszTranslatedURL parameter is valid only if the function returns S_OK.
Return Value
Returns S_OK upon success, or S_FALSE if the URL did not require translation. If
an error occurs, the function returns one of the following values:

E_FLAGS
The flag combination passed in dwInFlags is invalid.
E_OUTOFMEMORY
There was insufficient memory to complete the operation.
E_POINTER
One of the input pointers is invalid.
Remarks
This function does not validate the input URL string. A successful return value
does not indicate that the URL strings are valid URLs.

QuickInfo
  Version 4.00 and later of shell32.dll

  Windows NT: Requires version 4.0 or later
  Windows: Requires Windows 95 or later
  Windows CE: Unsupported.
  Header: Declared in intshcut.h.
*)
(*
URLAssociationDialog
Invokes the unregistered URL protocol dialog box. This dialog box allows the user
to select an application to associate with a previously unknown protocol.

HRESULT URLAssociationDialog(
  HWND hwndParent,
  DWORD dwInFlags,
  LPCSTR pcszFile,
  LPCSTR pcszURL,
  LPSTR pszAppBuf,
  UINT ucAppBufLen,
);

Parameters
hwndParent
Handle to the parent window.
dwInFlags
Bit flags that specify the behavior of the function. This value can be a combination
of the following:
URLASSOCDLG_FL_USE_DEFAULT_NAME
Use the default file name (that is, "Internet Shortcut").
URLASSOCDLG_FL_REGISTER_ASSOC
Register the selected application as the handler for the protocol specified in pcszURL.
The application is registered only if this flag is set and the user indicates that
a persistent association is desired.
pcszFile
Address of a constant zero-terminated string that contains the file name to associate with the URL's protocol.
pcszURL
Address of a constant zero-terminated string that contains the URL with an unknown protocol.
pszAppBuf
Address of a buffer that receives the path of the application specified by the user.
ucAppBufLen
The size of pszAppBuf, in characters.
Return Value
Returns S_OK if the application is registered with the URL protocol, or S_FALSE
if nothing is registered. For example, the function returns S_FALSE when the user
elects to perform a one-time execution via the selected application.

QuickInfo
  Version 4.00 and later of shell32.dll

  Windows NT: Requires version 4.0 or later
  Windows: Requires Windows 95 or later
  Windows CE: Unsupported.
  Header: Declared in intshcut.h.
*)
(*
MIMEAssociationDialog
Runs the unregistered MIME content type dialog box.

HRESULT MIMEAssociationDialog(
  HWND     hwndParent,
  DWORD    dwInFlags,
  LPCSTR   pcszFile,
  LPCSTR   pcszMIMEContentType,
  LPSTR    pszAppBuf,
  UINT     ucAppBufLen,
);

Parameters
hwndParent
Handle to the parent window of any posted child windows.
dwInFlags
Bit flag value that specifies if an association is to be registered. The bit flag
is the value MIMEASSOCDLG_FL_REGISTER_ASSOC (0x0001). If this bit is set, the selected
application is registered as the handler for the given MIME type. If this bit is
clear, no association is registered.
An application is registered only if this flag is set and the user indicates that
a persistent association is to be made.

Registration is impossible if the string at pcszFile does not contain an extension.

pcszFile
Address of a null-terminated string that contains the name of the target file.
This file must conform to the content type described by the pcszMIMEContentType parameter.
pcszMIMEContentType
Address of a null-terminated string that contains the unregistered content type.
pszAppBuf
Address of a buffer that receives the path of the application specified by the user.
ucAppBufLen
Size of pszAppBuf, in characters.
Return Value
Returns one of the following values:

Returns S_OK if the content type was successfully associated with the extension.
In this case, the extension is associated as the default for the content type,
and pszAppBuf points to the string that contains the path of the specified application.
The function returns S_FALSE if nothing was registered. Otherwise, the return value
will be one of the following:

E_ABORT  The user canceled the operation.
E_FLAGS  The flag combination passed in dwInFlags is invalid.
E_OUTOFMEMORY  There was insufficient memory available to complete the operation.
E_POINTER  One of the input pointers is invalid.

Remarks
This function does not validate the syntax of the input content type string at
pcszMIMEContentType. A successful return value does not indicate that the specified
MIME content type is valid.

QuickInfo
  Version 4.00 and later of shell32.dll

  Windows NT: Requires version 4.0 or later
  Windows: Requires Windows 95 or later
  Windows CE: Unsupported.
  Header: Declared in intshcut.h.
*)
(*
InetIsOffline
Determines whether or not the system is connected to the Internet.

BOOL InetIsOffline(
  DWORD dwFlags,
);

Parameters
dwFlags
Input flags for the function. This must be set to zero.
Return Value
Returns TRUE if the local system in not currently connected to the Internet. Returns
FALSE if the local system is connected to the Internet or if no attempt has yet
been made to connect to the Internet.

QuickInfo
  Version 4.00 and later of shell32.dll

  Windows NT: Requires version 4.0 or later
  Windows: Requires Windows 95 or later
  Windows CE: Unsupported.
  Header: Declared in intshcut.h.
*)
(*
FindEnvironmentString
--------------------------------------------------------------------------------

Looks up the specified environment variable and returns a pointer to its value.

Syntax

LPCTSTR FindEnvironmentString(
    LPCTSTR pszEnvVar,
);
Parameters

pszEnvVar
Null-terminated string with the environment variable of interest. Case is ignored.
Because environment variables can be added by the user or applications, the complete
list is system-dependent. The following environment variables are standard with
Microsoft® Windows NT® and are available to both interactive applications and services.
COMPUTERNAME NUMBER_OF_PROCESSORS
OS PROCESSOR_ARCHITECTURE
PROCESSOR_IDENTIFIER PROCESSOR_LEVEL
PROCESSOR_REVISION PROGRAMFILES
SYSTEMDRIVE SYSTEMROOT
USERPROFILE WINDIR

The remainder are only available to interactive applications.
HOMEDRIVE HOMEPATH
LOGONSERVER USERDOMAIN
USERNAME

Only the WINDIR variable is available on Windows 95/98 systems.

Return Values

Returns the value of the environment variable, or returns NULL if the variable
isn't in the environment.

QuickInfo
  Version 4.71 and later of shell32.dll

  Windows NT: Requires version 5.0 or later (or version 4.0 with Internet Explorer 4.0 or later).
  Windows: Requires Windows 98 (or Windows 95 with Internet Explorer 4.0 or later).
  Windows CE: Unsupported.
  Header: Declared in shellapi.h.
*)
(*
SHELLFLAGSTATE

typedef struct {
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;
    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1;
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    UINT fRestFlags : 3;
} SHELLFLAGSTATE, * LPSHELLFLAGSTATE;
Contains a set of flags that indicate the current shell settings. This structure
is used with the SHGetSettings function.

fShowAllObjects
Nonzero if the Show All Files option is enabled or zero otherwise.
fShowExtensions
Nonzero if the Hide File Extensions for Known File Types option is disabled or zero otherwise.
fNoConfirmRecycle
Nonzero if the Display Delete Confirmation Dialog box in the Recycle Bin is
enabled or zero otherwise.
fShowSysFiles
Nonzero if the Do Not Show Hidden Files option is selected or zero otherwise.
fShowCompColor
Nonzero if the Display Compressed Files and Folders with Alternate Color option
is enabled or zero otherwise.
fDoubleClickInWebView
Nonzero if the Double-Click to Open an Item option is enabled or zero otherwise.
fDesktopHTML
Nonzero if the Active Desktop – View as Web Page option is enabled or zero otherwise.
fWin95Classic
Nonzero if the Classic Style option is enabled or zero otherwise.
fDontPrettyPath
Nonzero if the Allow All Uppercase Names option is enabled or zero otherwise.
fShowAttribCol
Nonzero if the Show File Attributes in Detail View option is enabled or zero otherwise.
fMapNetDrvBtn
Nonzero if the Show Map Network Drive Button in Toolbar option is enabled or zero otherwise.
fShowInfoTip
Nonzero if the Show Info Tips for Items in Folders & Desktop option is enabled or zero otherwise.
fHideIcons
Not used.
fRestFlags
Not used.
QuickInfo
  Version 4.71 and later of shell32.dll

  Windows NT: Requires version 5.0 or later (or version 4.0 with Internet Explorer 4.0 or later).
  Windows: Requires Windows 98 (or Windows 95 with Internet Explorer 4.0 or later).
  Windows CE: Unsupported.
  Header: Declared in shlobj.h.
*)
(*
SOFTDISTINFO

typedef struct _TagSOFTDISTINFO{
 ULONG	cbSize;
 DWORD	dwFlags;
 DWORD	dwAdState;
 LPWSTR	lpszTitle;
 LPWSTR	lpszAbstract;
 LPWSTR	lpszHREF;
 DWORD	dwInstalledVersionMS;
 DWORD	dwInstalledVersionLS;
 DWORD	dwUpdateVersionMS;
 DWORD	dwUpdateVersionLS;
 DWORD	dwAdvertisedVersionMS;
 DWORD	dwAdvertisedVersionLS;
 DWORD	dwReserved
} SOFTDISTINFO, *LPSOFTDISTINFO;
Contains information about a software update.

cbSize
Contains the size of the structure, in bytes.
dwFlags
This parameter can take one of the following values:
SOFTDIST_FLAG_USAGE_EMAIL
SOFTDIST_FLAG_USAGE_PRECACHE
SOFTDIST_FLAG_USAGE_AUTOINSTALL
SOFTDIST_FLAG_DELETE_SUBSCRIPTION
dwAdState
The advertised state. It can take one of the following values:
0: Update-available dialog box has not been presented to the user.
1: Files-downloaded dialog box has not been presented to the user.
2: Program-installed dialog box has not been presented to the user.
lpszTitle
String containing the contents of the TITLE flag from the associated CDF file.
lpszAbstract
String containing the contents of the ABSTRACT flag from the associated CDF file.
lpszHREF
String containing the URL of the Web page to advertise or install the update.
dwInstalledVersionMS
Contains the most-significant unsigned long integer value of the installed version number.
dwInstalledVersionLS
Contains the least-significant unsigned long integer value of the installed version number.
dwUpdateVersionMS
Contains the most-significant unsigned long integer value of the update version number.
dwUpdateVersionLS
Contains the Least-significant unsigned long integer value of the update version number.
dwAdvertisedVersionMS
Contains the most-significant unsigned long integer value of the advertised version number.
dwAdvertisedVersionLS
Contains the least-significant unsigned long integer value of the advertised version number.
cbReserved
Reserved. Must be set to zero.
The most-significant unsigned long integer of a version number contains the major
and minor version numbers. The least-significant unsigned long integer of the version
number contains the custom version and build numbers.

QuickInfo
  Version 4.71 and later of shell32.dll

  Windows NT: Requires version 5.0 or later (or version 4.0 with Internet Explorer 4.0 or later).
  Windows: Requires Windows 98 (or Windows 95 with Internet Explorer 4.0 or later).
  Windows CE: Unsupported.
  Header: Declared in shlobj.h.
*)

(*
var P:array[0..MAX_PATH] of WideChar
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    ShRunFileDlgW(hwndOwner,hIcon,StringToWideChar(string(lpStrDirectory),P,255),
    StringToWideChar(string(lpStrTitle),P,255) ,StringToWideChar(string(lpStrDescription),P,255),uFlags)
  else
    ShRunFileDlgA(hwndOwner,hIcon,lpStrDirectory,lpStrTitle,lpStrDescription,uFlags)

var srb:TSHQueryRBInfo;P:PChar;i:integer;
begin
//  SHQueryRecycleBin('',srb);
//  SHEmptyRecycleBin(Handle,PChar(''),0);
//  SHUpdateRecycleBinIcon;
  ShRestartDialog(Handle,PChar(''),EWX_LOGOFF);
  ShExitWinDlg(Handle);
  ShRunFileDlg(Handle,0,PChar(''),PChar(''),PChar(''),RFF_NOSEPARATEMEM);
  GetMem(P,sizeof(widechar) * MAX_PATH);
  P := '';
  ShPickIconDlg(Handle,P,MAX_PATH,i);
  Caption := P;
  FreeMem(P);
  SHFindFiles(nil,nil);
  SHFindComputer(nil,nil);
  SHObjectProperties(Handle,0,'C:\Winnt','Verktyg');
*)
end.

