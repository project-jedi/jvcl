{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHDialogs2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvHDialogs2;

{*******************************************************}
{   This unit is an interface to the MRU List (shell32) }
{   Informations from :                                 }
{      http://www.geocities.com/SiliconValley/4942      }
{*******************************************************}

interface

uses
  Windows, Messages, SHlObj,
  JvFunctions;

const
  //RunFileDlg Flags
  //Removes the browse button.
  RFF_NOBROWSE = $01;
  //No default item selected.
  RFF_NODEFAULT = $02;
  //Calculates the working directory from the file name.
  RFF_CALCDIRECTORY = $04;
  //Removes the edit box label.
  RFF_NOLABEL = $08;
  //Removes the Separate Memory Space check box (Windows NT only).
  RFF_NOSEPARATEMEM = $20;

  //Notification Return Values
  //Allow the Application to run.
  RF_OK = $00;
  //Cancel the operation and close the dialog.
  RF_CANCEL = $01;
  //Cancel the operation, but leave the dialog open.
  RF_RETRY = $02;

  //SHObjectProperties Flags
  OPF_PRINTERNAME = $01;
  OPF_PATHNAME = $02;

type
  NM_RUNFILEDLG = record
    hdr: NMHDR;
    lpFile: PChar;
    lpDirectory: PChar;
    nShow: Integer;
  end;

  {The first function I'll be discussing is PickIconDlg. Ob-
  viously enough, it provides an interface with which the user
  can select an icon. It's used by the file type editor when
  selecting the icon to associate with a particular file type. It's
  also used in the shortcut properties dialog when changing
  the icon.
        hwndOwner identifies the window that owns the dialog
  box. lpstrFile points to a buffer containing the initial file-
  name. When the function returns, this buffer will contain
  the new FileName. nMaxFile specifies the Size, in charac-
  ters, of the buffer. lpdwIconIndex points to a variable con-
  taining the zero based offset of the icon. When the function
  returns, the variable will be set to the new icon index.
        if the user selects an icon, the return Value is TRUE. It
  is FALSE if the user chooses the Cancel button, or the Close
  command on the System menu.}
  TSHPickIconDlg = function(hwndOwner: HWND; lpstrFile: LPWSTR;
    var pdwBufferSize: DWord; var lpdwIconIndex: DWord): Boolean; stdcall;

  {The next function, RunFileDlg, is probably not as useful, but
  still warants mentioning since it is amazingly flexible. It is
  the dialog that you see when launching Applications from
  the Start/Run menu.
        hwndOwner identifies the window that owns the dialog
  box. hIcon is the Handle of the icon that will be displayed in
  the dialog. if it is NULL, the default icon will be used. lpstrDirectory points
  to a string that specifies the working directory. lpstrTitle points to a
  string to be placed in the title bar of the dialog box. if it is NULL,
  the default title is used. lpstrDescription points to a stringthat is
  displayed in the dialog, briefly informing the user what to do.
  if it is NULL, the default description is used. uFlags is a set of bit
  flags that specify other properties of the dialog.
        A nice feature of this dialog is that it allows you to control
  which Applications the  user may run. When the user selects the OK
  button, your Parent window is sent a notification with details of the program
  that is about to be started. The notification is in the form of a WM_NOTIFY
  message with the notification code set to RFN_VALIDATE (-510) and
  the lParam pointing to an NM_RUNFILEDLG. The return Value determines whether
  the Application will be run or not.}
  TSHRunFileDlg = procedure(hwndOwner: HWND; hIcon: HICON;
    lpstrDirectory, lpstrTitle, lpstrDescription: PChar; Flags: longint); stdcall;

  {The next two functions, ExitWindowsDialog and RestartDialog, both deal
  with the problem of shutting down and restarting the operating system.
  They may seem out of place in this section, since they're not really much more
  than extensions of the ExitWindowsEx function, but they do
  both produce dialogs as part of the process.
        ExitWindowsDialog is probably the least useful of the
  two. It is the dialog that is displayed when you select ShutDown from the
  Start menu. The dialog never actually uses hwndOwner as a Parent.
  On Windows 95, hwndOwner will receive a WM_QUIT message if the operation is successful.
  On Windows NT, the window doesn't appear to be used at
  all. There is no return Value for the function, so you have no
  way of knowing what the user selected or whether the
  operation was canceled.
        RestartDialog is used when changes are made to the
  system that require a shutdown or restart before they can take effect.
  hwndOwner identifies the owner window. lpstrReason points to
  a string that is displayed in the dialog explaining the reason for the shutdown.
  uFlags specifies the type of shutdown - you can use a subset of the flags used
  by ExitWindowsEx. The return Value is IDYES if the user choose to perform the shutdown.
  It is IDNO if the operation was canceled.
        There are a couple of other points you should note.
  The reason displayed in the dialog always has some default text appended to
  it asking the user to confirm the operation. It is therefore advisable that
  you always end your reason with a space or a newline. The title of the
  dialog is always set to 'System Settings Change'. The return
  Value can not be used to determine the success of the operation. if the
  operation failed for some reason, the return Value will still be IDYES.}
  TSHRestartDlg = function(hwndOwner: HWND; Reason: PAnsiChar; flag: longint): LongInt; stdcall;
  TSHExitWindowsDlg = procedure(hwndOwner: HWND); stdcall;

  {The next two functions, SHFindFiles and SHFindComputer,
  give you access to the two system Find dialogs on the Start
  menu.
        Typically you would specify NULL for both parameters,
  which would start the dialogs as if the user had selected
  them from the Start menu. However, if you want to search
  for files rooted at a particular folder (as if the user had
  selected Find from the folder's context menu), you should
  set the pidlRoot parameter to the pidl of the folder. This
  only works for the SHFindFiles function though - the pa-
  rameter is ignored by SHFindComputer.
        You can also open a previously saved search by speci-
  fying the pidl of the file (a .fnd file) in the pidlSavedSearch
  parameter. Once again, this parameter is only supported
  with the SHFindFiles function - it appears to be ignored by
  SHFindComputer.
        For both functions, the dialog is started in a separate
  thread so the function call will return almost immediately.
  The return Value is TRUE if the thread was started suc-
  cessfully or FALSE if there was an error of some sort. If
  you have specified a Value for the pidlRoot parameter, it is
  your responsibility to Free it when the function returns. The
  pidlSavedSearch parameter, on the other hand, is usually
  freed by the system - you should only attempt to Free it if
  the function returns FALSE.
  }
  TSHFindComputer = function(pidlRoot, pidlSavedSearch: PItemIDList): Boolean; stdcall;
  TSHFindFiles = function(pidlRoot, pidlSavedSearch: PItemIDList): Boolean; stdcall;

  {The last function I'm going to deal with in this section is
  SHObjectProperties. This is what you would use to display
  the properties dialog for a file or folder. It can also be used
  to display the properties for a printer object.
        hwndOwner identifies the window that owns the dialog.
  lpstrName points to a string containing the path
  name or the printer name whose properties
  will be displayed. uFlags specifies the type of
  name contained in lpstrName.
  lpstrParameters points to a string containing
  the name of the page that will initially be selected. if lpstr-
  Parameters is NULL, the first page on the property sheet
  will be selected.
        if the function succeeds, the return Value is TRUE. If
  the function fails, the return Value is FALSE. To get exten-
  ded error information, call GetLastError. Note that this
  dialog is actually modeless, so when the function returns the
  dialog will probably still be open. There is no way of kno-
  wing when the user has closed the dialog.
        I should also mention that if you only need to display
  the properties for a file or folder, you can quite easily ac-
  complish the same thing with a call to the documented
  function ShellExecuteEx, specifying "properties" for the
  lpVerb parameter. This doesn't appear to work for printer
  names though.}
  TSHObjectProperties = function(hwndOwner: HWND; uFlags: Integer; lpstrName, lpstrParameters: LPWSTR): Boolean;
  stdcall;

  TSHOutOfMemoryMessageBox = function(Owner: HWND; Caption: Pointer; style: UINT): Integer; stdcall;
  TSHHandleDiskFull = procedure(Owner: HWND; Drive: UINT); stdcall;

var
  SHPickIconDlg: TSHPickIconDlg;
  SHHandleDiskFull: TSHHandleDiskFull;
  SHOutOfMemoryMessageBox: TSHOutOfMemoryMessageBox;
  SHObjectProperties: TSHObjectProperties;
  SHFindComputer: TSHFindComputer;
  SHFindFiles: TSHFindComputer;
  SHRunFileDlg: TSHRunFileDlg;
  SHRestartDlg: TSHRestartDlg;
  SHExitWindowsDlg: TSHExitWindowsDlg;

implementation

const
  DllName = 'Shell32.dll';

var
  hDll: THandle;

initialization
  hDll := LoadLibrary(DllName);
  if hDll <> 0 then
  begin
    // (rom) is load by ID really good?
    SHPickIconDlg := GetProcAddress(hDll, PChar(62));
    SHHandleDiskFull := GetProcAddress(hDll, PChar(185));
    SHOutOfMemoryMessageBox := GetProcAddress(hDll, PChar(126));
    SHObjectProperties := GetProcAddress(hDll, PChar(178));
    SHFindComputer := GetProcAddress(hDll, PChar(91));
    SHFindFiles := GetProcAddress(hDll, PChar(90));
    SHRunFileDlg := GetProcAddress(hDll, PChar(61));
    SHRestartDlg := GetProcAddress(hDll, PChar(59));
    SHExitWindowsDlg := GetProcAddress(hDll, PChar(60));
  end
  else
    PError('MRU');

finalization
  if hDll <> 0 then
    FreeLibrary(hDll);

end.
