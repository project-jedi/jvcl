{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinDialogs.PAS, released on 2002-05-13.

The Initial Developer of the Original Code is Serhiy Perevoznyk.
All Rights Reserved.

Contributor(s):
Michael Beck

Last Modified: 2002-05-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}

unit JvWinDialogs;

{$IFDEF COMPILER6}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  ShellAPI, Windows, Classes, Forms, SysUtils, Graphics, Dialogs,
  Controls, ShlOBJ, ComObj, CPL, ActiveX, CommDlg, JvComponent, JvBaseDlg;

type
  EShellOleError = class(Exception);
  EWinDialogError = class(Exception);

  TShellLinkInfo = record
    PathName: string;
    Arguments: string;
    Description: string;
    WorkingDirectory: string;
    IconLocation: string;
    IconIndex: integer;
    ShowCmd: integer;
    HotKey: word;
  end;

  TSpecialFolderInfo = record
    Name: string;
    ID: Integer;
  end;

const

  OFN_EX_NOPLACESBAR = 1; // for new style of standard Windows dialogs

  SpecialFolders: array[0..29] of TSpecialFolderInfo = (
    (Name: 'Alt Startup'; ID: CSIDL_ALTSTARTUP),
    (Name: 'Application Data'; ID: CSIDL_APPDATA),
    (Name: 'Recycle Bin'; ID: CSIDL_BITBUCKET),
    (Name: 'Common Alt Startup'; ID: CSIDL_COMMON_ALTSTARTUP),
    (Name: 'Common Desktop'; ID: CSIDL_COMMON_DESKTOPDIRECTORY),
    (Name: 'Common Favorites'; ID: CSIDL_COMMON_FAVORITES),
    (Name: 'Common Programs'; ID: CSIDL_COMMON_PROGRAMS),
    (Name: 'Common Start Menu'; ID: CSIDL_COMMON_STARTMENU),
    (Name: 'Common Startup'; ID: CSIDL_COMMON_STARTUP),
    (Name: 'Controls'; ID: CSIDL_CONTROLS),
    (Name: 'Cookies'; ID: CSIDL_COOKIES),
    (Name: 'Desktop'; ID: CSIDL_DESKTOP),
    (Name: 'Desktop Directory'; ID: CSIDL_DESKTOPDIRECTORY),
    (Name: 'Drives'; ID: CSIDL_DRIVES),
    (Name: 'Favorites'; ID: CSIDL_FAVORITES),
    (Name: 'Fonts'; ID: CSIDL_FONTS),
    (Name: 'History'; ID: CSIDL_HISTORY),
    (Name: 'Internet'; ID: CSIDL_INTERNET),
    (Name: 'Internet Cache'; ID: CSIDL_INTERNET_CACHE),
    (Name: 'Network Neighborhood'; ID: CSIDL_NETHOOD),
    (Name: 'Network Top'; ID: CSIDL_NETWORK),
    (Name: 'Personal'; ID: CSIDL_PERSONAL),
    (Name: 'Printers'; ID: CSIDL_PRINTERS),
    (Name: 'Printer Links'; ID: CSIDL_PRINTHOOD),
    (Name: 'Programs'; ID: CSIDL_PROGRAMS),
    (Name: 'Recent Documents'; ID: CSIDL_RECENT),
    (Name: 'Send To'; ID: CSIDL_SENDTO),
    (Name: 'Start Menu'; ID: CSIDL_STARTMENU),
    (Name: 'Startup'; ID: CSIDL_STARTUP),
    (Name: 'Templates'; ID: CSIDL_TEMPLATES));

  {SHObjectProperties Flags}
  OPF_PRINTERNAME = $01;
  OPF_PATHNAME = $02;

type

  TOpenFileNameEx = packed record
    // Size of the structure in bytes.
    lStructSize: DWORD;
    // Handle that is the parent of the dialog.
    hWndOwner: HWND;
    // Application instance handle.
    hInstance: HINST;
    // String containing filter information.
    lpstrFilter: PAnsiChar;
    // Will hold the filter chosen by the user.
    lpstrCustomFilter: PAnsiChar;
    // Size of lpstrCustomFilter, in bytes.
    nMaxCustFilter: DWORD;
    // Index of the filter to be shown.
    nFilterIndex: DWORD;
    // File name to start with (and retrieve).
    lpstrFile: PAnsiChar;
    // Size of lpstrFile, in bytes.
    nMaxFile: DWORD;
    // File name without path will be returned.
    lpstrFileTitle: PAnsiChar;
    // Size of lpstrFileTitle, in bytes.
    nMaxFileTitle: DWORD;
    // Starting directory.
    lpstrInitialDir: PansiChar;
    // Title of the open dialog.
    lpstrTitle: PAnsiChar;
    // Controls user selection options.
    Flags: DWORD;
    // Offset of file name in filepath=lpstrFile.
    nFileOffset: Word;
    // Offset of extension in filepath=lpstrFile.
    nFileExtension: Word;
    // Default extension if no extension typed.
    lpstrDefExt: PAnsiChar;
    // Custom data to be passed to hook.
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM;
      lParam: LPARAM): UINT stdcall; // Hook.
    // Template dialog, if applicable.
    lpTemplateName: PAnsiChar;
    // Extended structure starts here.
    pvReserved: Pointer; // Reserved, use nil.
    dwReserved: DWORD; // Reserved, use 0.
    FlagsEx: DWORD; // Extended Flags.
  end;

  TJvFormatType = (ftFull, ftQuick);
  TShellObjectType = (sdPathObject, sdPrinterObject);
  TShellObjectTypes = set of TShellObjectType;

type
  {Format drive dialog}
  TJvFormatDialog = class(TjvCommonDialogP)
  private
    FFormatType: TJvFormatType;
    FDrive: integer;
    FDriveChar: Char;
    procedure SetDriveChar(Value: Char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
  published
    property FormatType: TJvFormatType read FFormatType write FFormatType;
    property DriveChar: char read FDriveChar write SetDriveChar;
  end;

  {Organize Favorites dialog}
  TJvOrganizeFavoritesDialog = class(TjvCommonDialog)
  public
    function Execute: boolean; override;
  end;

  {Show control panel}
  TJvControlPanelDialog = class(TjvCommonDialogP)
  public
    procedure Execute; override;
  end;

  {execute Control Panel applet}
  TJvAppletDialog = class(TjvCommonDialog)
  private
    FAppletName: string;
  public
    function Execute: boolean; override;
  published
    property AppletName: string read FAppletName write FAppletName;
  end;

  {browse network}
  TJvComputerNameDialog = class(TjvCommonDialog)
  private
    FComputerName: string;
    FCaption: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; override;
    property ComputerName: string read FComputerName;
  published
    property Caption: string read FCaption write FCaption;
  end;

  {selection folder from tree}
  TJvBrowseFolderDialog = class(TjvCommonDialog)
  private
    FFolderName: string;
    FCaption: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; override;
    property FolderName: string read FFolderName;
  published
    property Caption: string read FCaption write FCaption;
  end;

  {"Out of memory" dialog}
  TJvOutOfMemoryDialog = class(TjvCommonDialog)
  private
    FCaption: string;
  public
    function Execute: boolean; override;
  published
    property Caption: string read FCaption write FCaption;
  end;

  TJvChangeIconDialog = class(TjvCommonDialogP)
  private
    FIconIndex: integer;
  public
    procedure Execute; override;
  published
    property IconIndex: integer read FIconIndex write FIconIndex;
  end;

  TJvShellAboutDialog = class(TjvCommonDialog)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FCaption: string;
    FIcon: TIcon;
    FOtherText: string;
    FProduct: string;
  private
    procedure SetIcon(NewValue: TIcon);
  private
    function StoreIcon: Boolean;
  public
    function Execute: Boolean; override;
  published
    property Caption: string read FCaption write FCaption;
    property Icon: TIcon read FIcon write SetIcon stored StoreIcon;
    property OtherText: string read FOtherText write FOtherText;
    property Product: string read FProduct write FProduct;
  end;

  TJvRunDialog = class(TjvCommonDialogP)
  private
    FCaption: string;
    FDescription: string;
    FIcon: TIcon;
    procedure SetIcon(const Value: TIcon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
    property Icon: TIcon read FIcon write SetIcon;
  end;

  TJvObjectPropertiesDialog = class(TjvCommonDialog)
  private
    FObjectName: TFileName;
    FObjectType: TShellObjectType;
    FInitialTab: string;
  public
    function Execute: boolean; override;
  published
    property ObjectName: TFileName read FObjectName write FObjectName;
    property ObjectType: TShellObjectType read FObjectType write FObjectType;
    property InitialTab: string read FInitialTab write FInitialTab;
  end;

  TJvNewLinkDialog = class(TjvCommonDialogP)
  private
    FDestinationFolder: string;
  public
    procedure Execute; override;
  published
    property DestinationFolder: string read FDestinationFolder write FDestinationFolder;
  end;

  TJvAddHardwareDialog = class(TjvCommonDialogP)
  public
    procedure Execute; override;
  end;

  TJvOpenWithDialog = class(TjvCommonDialogP)
  private
    FFileName: string;
  public
    procedure Execute; override;
  published
    property FileName: string read FFileName write FFileName;
  end;

  TJvDiskFullDialog = class(TjvCommonDialog)
  private
    FDriveChar: Char;
    procedure SetDriveChar(Value: Char);
    function GetDrive: UINT;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property DriveChar: char read FDriveChar write SetDriveChar;
  end;

  TJvExitWindowsDialog = class(TjvCommonDialogP)
  public
    procedure Execute; override;
  end;

  //Common dialogs with Windows 2000 style support
  TJvOpenDialog2000 = class(TOpenDialog)
  public
    function Execute: Boolean; override;
  end;

  TJvSaveDialog2000 = class(TSaveDialog)
  public
    function Execute: Boolean; override;
  end;

  // Tools routines
function GetSpecialFolderPath(FolderName: string; CanCreate: Boolean): string;
procedure AddToRecentDocs(const Filename: string);
procedure ClearRecentDocs;
function ExtractIconFromFile(FileName: string; Index: integer): HIcon;
function CreateShellLink(const AppName, Desc: string; Dest: string): string;
procedure GetShellLinkInfo(const LinkFile: WideString; var SLI: TShellLinkInfo);
procedure SetShellLinkInfo(const LinkFile: WideString; const SLI: TShellLinkInfo);
function RecycleFile(FileToRecycle: string): boolean;
function CopyFile(FromFile, ToDir: string): boolean;
procedure ExecuteApplet(AppletName: string);
function ShellObjectTypeEnumToConst(ShellObjectType: TShellObjectType): UINT;
function ShellObjectTypeConstToEnum(ShellObjectType: UINT): TShellObjectType;
function ShellMessageBox(Instance: THandle; Owner: HWND; Text: PChar; Caption: PChar;
  Style: UINT; Parameters: array of Pointer): Integer; cdecl;

type
  FreePIDLProc = procedure(PIDL: PItemIDList); stdcall;
  SHChangeIconProc = function(wnd: HWND; szFileName: PChar; reserved: integer; var lpIconIndex: integer): DWORD; stdcall;
  SHFormatDriveProc = function(wnd: HWND; drive: UINT; fmtID: UINT; options: UINT): DWORD; stdcall;
  SHShutDownDialogProc = procedure(wnd: HWND); stdcall;
  SHRunDialogProc = function(wnd: HWND; Unknown1: integer; Unknown2: Pointer; szTitle: PChar; szPrompt: PChar; uiFlages: integer): DWORD; stdcall;
  SHFindFilesProc = function(Root: PItemIDList; SavedSearchFile: PItemIDList): LongBool; stdcall;
  SHFindComputerProc = function(Reserved1: PItemIDList; Reserved2: PItemIDList): LongBool; stdcall;
  SHObjectPropertiesProc = function(Owner: HWND; Flags: UINT; ObjectName: Pointer; InitialTabName: Pointer): LongBool; stdcall;
  SHNetConnectionDialogProc = function(Owner: HWND; ResourceName: Pointer; ResourceType: DWORD): DWORD; stdcall;
  SHStartNetConnectionDialogProc = function(Owner: HWND; ResourceName: PWideChar; ResourceType: DWORD): DWORD; stdcall;
  SHOutOfMemoryMessageBoxProc = function(Owner: HWND; Caption: Pointer; Style: UINT): Integer; stdcall;
  SHHandleDiskFullProc = procedure(Owner: HWND; uDrive: UINT); stdcall;
  NewLinkHereProc = procedure(HWND: THandle; HInstance: THandle; CmdLine: Pchar; cmdShow: integer); stdcall;
  SHOpenWithProc = procedure(HWND: THandle; HInstance: THandle; cmdLine: PChar; cmdShow: integer); stdcall;
  GetOpenFileNameExProc = function(var OpenFile: TOpenFilenameEx): Bool; stdcall;
  GetSaveFileNameExProc = function(var SaveFile: TOpenFileNameEx): bool; stdcall;

var
  FreePIDL: FreePIDLProc = nil;
  GetOpenFileNameEx: GetOpenFileNameExProc = nil;
  GetSaveFileNameEx: GetSaveFileNameExProc = nil;
  SHFormatDrive: SHFormatDriveProc = nil;
  SHShutDownDialog: SHShutDownDialogProc = nil;
  SHRunDialog: SHRunDialogProc = nil;
  SHFindFiles: SHFindFilesProc = nil;
  SHFindComputer: SHFindComputerProc = nil;
  SHObjectProperties: SHObjectPropertiesProc = nil;
  SHNetConnectionDialog: SHNetConnectionDialogProc = nil;
  SHStartNetConnectionDialog: SHStartNetConnectionDialogProc = nil;
  SHOutOfMemoryMessageBox: SHOutOfMemoryMessageBoxProc = nil;
  SHHandleDiskFull: SHHandleDiskFullProc = nil;
  NewLinkHere: NewLinkHereProc = nil;
  SHOpenWith: SHOpenWithProc = nil;
  SHChangeIcon: SHChangeIconProc = nil;

resourcestring
  SDiskFullError = 'TJvDiskFullDialog does not support removable media or network drives.';
  SNotSupported = 'This function is not supported by your version of Windows';
  SInvalidDriveChar = 'Invalid drive (%s)';
  SUnsupportedDisk = 'Unsupported drive (%s): JvDiskFullDialog only supports fixed drives.';

implementation

const
  shell32 = 'shell32.dll';

var
  ShellHandle: THandle = 0;
  CommHandle: THandle = 0;
  AppWizHandle: THandle = 0;

procedure LoadJvDialogs;
begin
  ShellHandle := Windows.LoadLibrary(PChar(shell32));
  if ShellHandle <> 0 then
  begin
    SHChangeIcon := GetProcAddress(ShellHandle, PChar(62));
    SHFormatDrive := GetProcAddress(ShellHandle, PChar('SHFormatDrive'));
    FreePIDL := GetProcAddress(ShellHandle, PChar(155));
    SHShutDownDialog := GetProcAddress(ShellHandle, PChar(60));
    SHRunDialog := GetProcAddress(ShellHandle, PChar(61));
    SHFindFiles := GetProcAddress(ShellHandle, PChar(90));
    SHFindComputer := GetProcAddress(ShellHandle, PChar(91));
    SHObjectProperties := GetProcAddress(ShellHandle, PChar(178));
    SHNetConnectionDialog := GetProcAddress(ShellHandle, PChar(160));
    SHOutOfMemoryMessageBox := GetProcAddress(ShellHandle, PChar(126));
    SHHandleDiskFull := GetProcAddress(ShellHandle, PChar(185));
    SHStartNetConnectionDialog := GetProcAddress(ShellHandle, PChar(215));
    SHOpenWith := GetProcAddress(ShellHandle, PChar('OpenAs_RunDLLA'));

  end;

  CommHandle := Windows.LoadLibrary('comdlg32.dll');
  if CommHandle <> 0 then
  begin
    GetOpenFileNameEx := GetProcAddress(CommHandle, PChar('GetOpenFileNameA'));
    GetSaveFileNameEx := GetProcAddress(CommHandle, PChar('GetSaveFileNameA'));
  end;

  AppWizHandle := Windows.LoadLibrary('appwiz.cpl');
  if AppWizHandle <> 0 then
    NewLinkHere := GetProcAddress(AppWizHandle, PChar('NewLinkHereA'));
end;

procedure UnloadJvDialogs;
begin
  if Shellhandle <> 0 then
    FreeLibrary(ShellHandle);
  if CommHandle <> 0 then
    FreeLibrary(CommHandle);
  if AppWizHandle <> 0 then
    FreeLibrary(AppWizHandle);
end;

{  Although most Win32 applications do not need to be able
   to format disks, some do. Windows 95 and Windows NT provide
   an API function called SHFormatDrive, which presents the
   same dialog box as the Windows 95 and Windows NT shells,
   formats the specified diskette.

   The SHFormatDrive API provides access to the Shell's format
   dialog box. This allows applications that want to format disks to bring
   up the same dialog box that the Shell uses for disk formatting.

   PARAMETERS
      hwnd    = The window handle of the window that will own the
                dialog. NOTE that hwnd == NULL does not cause this
                dialog to come up as a "top level application"
                window. This parameter should always be non-null,
                this dialog box is only designed to be the child of
                another window, not a stand-alone application.

      drive   = The 0 based (A: == 0) drive number of the drive
                to format.

      fmtID   = Currently must be set to SHFMT_ID_DEFAULT.

      options = There are currently only two option bits defined.

                   SHFMT_OPT_FULL
                   SHFMT_OPT_SYSONLY

                SHFMT_OPT_FULL specifies that the "Quick Format"
                setting should be cleared by default. If the user
                leaves the "Quick Format" setting cleared, then a
                full format will be applied (this is useful for
                users that detect "unformatted" disks and want
                to bring up the format dialog box).

                If options is set to zero (0), then the "Quick Format"
                setting is set by default. In addition, if the user leaves
                it set, a quick format is performed. Under Windows NT 4.0,
                this flag is ignored and the "Quick Format" box is always
                checked when the dialog box first appears. The user can
                still change it. This is by design.

                The SHFMT_OPT_SYSONLY initializes the dialog to
                default to just sys the disk.

                All other bits are reserved for future expansion
                and must be 0.

                Please note that this is a bit field and not a
                value, treat it accordingly.

      RETURN
         The return is either one of the SHFMT_* values, or if
         the returned DWORD value is not == to one of these
         values, then the return is the physical format ID of the
         last successful format. The LOWORD of this value can be
         passed on subsequent calls as the fmtID parameter to
         "format the same type you did last time".
}

const
  SHFMT_ID_DEFAULT = $FFFF;
  SHFMT_OPT_FULL = $0001;
  SHFMT_OPT_SYSONLY = $0002;
  // Special return values. PLEASE NOTE that these are DWORD values.
  SHFMT_ERROR = $FFFFFFFF; // Error on last format
  // drive may be formatable
  SHFMT_CANCEL = $FFFFFFFE; // Last format wascanceled
  SHFMT_NOFORMAT = $FFFFFFFD; // Drive is not formatable

type
  LPFNORGFAV = function(Wnd: hWnd; Str: lptStr): integer; stdcall;

function ExtractIconFromFile(FileName: string; Index: integer): HIcon;
var
  iNumberOfIcons: integer;
begin
  Result := 0;
  if FileExists(FileName) then
  begin
    iNumberOfIcons := ExtractIcon(hInstance, PChar(FileName), Cardinal(-1));
    if ((Index > 0) and
      (Index < iNumberOfIcons) and
      (iNumberOfIcons > 0))
      then
      Result := ExtractIcon(hInstance, PChar(FileName), Index);
  end;
end;

{ TJvOrganizeFavoritesDialog }

function TJvOrganizeFavoritesDialog.Execute: boolean;
var
  SHModule: THandle;
  Path: string;
  lpfnDoOrganizeFavDlg: LPFNORGFAV;
begin
  lpfnDoOrganizeFavDlg := nil;
  ShModule := SafeLoadLibrary('shdocvw.dll');
  try
    if ShModule <= HINSTANCE_ERROR then
    begin
      Result := False;
      Exit;
    end;
    Path := GetSpecialFolderPath('Favorites', true) + #0#0;
    lpfnDoOrganizeFavDlg := LPFNORGFAV(GetProcAddress(SHModule, 'DoOrganizeFavDlg'));
    if not Assigned(lpfnDoOrganizeFavDlg) then
      raise EWinDialogError.Create(SNotSupported);
    lpfnDoOrganizeFavDlg(Application.Handle, PChar(Path));
  finally
    FreeLibrary(SHModule);
  end;
  Result := true;
end;

{ TJvControlPanelDialog }

procedure TJvControlPanelDialog.Execute;
begin
  ShellExecute(0, 'open',
    'Control.exe',
    nil,
    nil,
    SW_SHOWDEFAULT);
end;

{ TJvAppletDialog }

function TJvAppletDialog.Execute: boolean;
var APModule: THandle;
  Applet: Applet_PROC;
begin
  if FAppletName = EmptyStr then
  begin
    Result := False;
    Exit;
  end;
  APModule := LoadLibrary(Pchar(AppletName));
  if APModule <= HINSTANCE_ERROR then
  begin
    Result := False;
    Exit;
  end;
  Applet := Applet_proc(GetProcAddress(APModule, 'CPlApplet'));
  Applet(0, CPL_DBLCLK, 0, 0);
  FreeLibrary(ApModule);
  Result := true;
end;

{ TJvComputerNameDialog }

constructor TJvComputerNameDialog.Create(AOwner: TComponent);
begin
  inherited;
  FComputerName := EmptyStr;
end;

function TJvComputerNameDialog.Execute: boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  NameBuffer: array[0..MAX_PATH] of Char;
  WindowList: Pointer;
begin
  Result := False;

  if Failed(SHGetSpecialFolderLocation(Application.Handle, CSIDL_NETWORK, ItemIDList)) then
    Exit;

  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := Application.Handle;
  BrowseInfo.pidlRoot := ItemIDList;
  BrowseInfo.pszDisplayName := NameBuffer;
  BrowseInfo.lpszTitle := PChar(FCaption);
  BrowseInfo.ulFlags := BIF_BROWSEFORCOMPUTER;
  WindowList := DisableTaskWindows(0);
  try
    Result := SHBrowseForFolder(BrowseInfo) <> nil;
  finally
    EnableTaskWindows(WindowList);
    FreePidl(BrowseInfo.pidlRoot);
  end;
  if Result then
    FComputerName := NameBuffer;
end;

{ TJvBrowseFolderDialog }

constructor TJvBrowseFolderDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFolderName := EmptyStr;
end;

function TJvBrowseFolderDialog.Execute: boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  ItemSelected: PItemIDList;
  NameBuffer: array[0..MAX_PATH] of Char;
  WindowList: Pointer;
begin
  itemIDList := nil;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := Application.Handle;
  BrowseInfo.pidlRoot := ItemIDList;
  BrowseInfo.pszDisplayName := NameBuffer;
  BrowseInfo.lpszTitle := PChar(FCaption);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
  WindowList := DisableTaskWindows(0);
  try
    ItemSelected := SHBrowseForFolder(BrowseInfo);
    Result := ItemSelected <> nil;
  finally
    EnableTaskWindows(WindowList);
  end;

  if Result then
  begin
    SHGetPathFromIDList(ItemSelected, NameBuffer);
    FFolderName := NameBuffer;
  end;
  Freepidl(BrowseInfo.pidlRoot);
end;

{ TJvFormatDialog }

constructor TJvFormatDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDriveChar := 'A';
end;

procedure TJvFormatDialog.Execute;
var options: UINT;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    case FFormatType of
      ftFull: options := 0;
      ftQuick: options := SHFMT_OPT_FULL;
    else
      options := 0;
    end;
  end
  else
  begin
    case FFormatType of
      ftFull: options := SHFMT_OPT_FULL;
      ftQuick: options := 0;
    else
      options := 0;
    end;
  end;
  if not Assigned(SHFormatDrive) then
    raise EWinDialogError.Create(SNotSupported);
  SHFormatDrive(Application.Handle, FDrive, SHFMT_ID_DEFAULT, Options);
end;

procedure TJvFormatDialog.SetDriveChar(Value: Char);
begin
  if (Value in ['a'..'z']) then
    Value := Char(ord(Value) - $20);
  if (not (Value in ['A'..'Z'])) then
    raise EWinDialogError.CreateFmt(SInvalidDriveChar, [Value]);
  FDriveChar := Value;
  FDrive := ord(FDriveChar) - ord('A');
end;

function GetSpecialFolderPath(FolderName: string; CanCreate: Boolean): string;
var
  FilePath: array[0..MAX_PATH] of char;
  Folder: integer;
  Found: boolean;
  I: integer;
begin
  Found := False;
  Folder := 0;
  Result := EmptyStr;
  for i := 0 to 29 do
  begin
    if (UpperCase(FolderName) = UpperCase(SpecialFolders[i].Name)) then
    begin
      Folder := SpecialFolders[i].ID;
      Found := true;
      break;
    end;
  end;
  if not found then
    Exit;
  { Get path of selected location }
  SHGetSpecialFolderPath(0, FilePath, Folder, CanCreate);
  Result := FilePath;
end;

procedure AddToRecentDocs(const Filename: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(Filename));
end;

procedure ClearRecentDocs;
begin
  SHAddToRecentDocs(SHARD_PATH, nil);
end;

function ExecuteShellMessageBox(MethodPtr: Pointer; Instance: THandle; Owner: HWND; Text: Pointer; Caption: Pointer; Style: UINT; Parameters: array of Pointer): Integer;

type
  PPointer = ^Pointer;
var
  ParamCount: Integer;
  ParamBuffer: PChar;
  BufferIndex: Integer;
begin
  ParamCount := (High(Parameters) + 1);
  GetMem(ParamBuffer, ParamCount * SizeOf(Pointer));
  try
    for BufferIndex := 0 to High(Parameters) do
    begin
      PPointer(@ParamBuffer[BufferIndex * SizeOf(Pointer)])^ := Parameters[High(Parameters) - BufferIndex];
    end;
    asm
      mov ECX, ParamCount
      cmp ECX, 0
      je  @MethodCall
      mov EDX, ParamBuffer
      @StartLoop:
      push DWORD PTR[EDX]
      add  EDX, 4
      loop @StartLoop
      @MethodCall:
      push Style
      push Caption
      push Text
      push Owner
      push Instance

      call MethodPtr
      mov  Result, EAX
    end;
  finally
    FreeMem(ParamBuffer);
  end;
end;

function ShellMessageBox(Instance: THandle; Owner: HWND; Text: PChar; Caption: PChar;
  Style: UINT; Parameters: array of Pointer):
  Integer; cdecl;
var
  MethodPtr: Pointer;
  ShellDLL: HMODULE;
begin
  ShellDLL := LoadLibrary(PChar(shell32));
  MethodPtr := GetProcAddress(ShellDLL, PChar(183));
  if (MethodPtr <> nil) then
  begin
    Result := ExecuteShellMessageBox(MethodPtr, Instance, Owner, Text, Caption, Style, Parameters);
  end
  else
  begin
    Result := ID_CANCEL;
  end;
end;

{ TJvOutOfMemoryDialog }

function TJvOutOfMemoryDialog.Execute: boolean;
var
  CaptionBuffer: Pointer;

begin
  CaptionBuffer := nil;
  if (FCaption <> '') then
    GetMem(CaptionBuffer, (Length(FCaption) + 1) * SizeOf(WideChar));

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if (CaptionBuffer <> nil) then
      StringToWideChar(FCaption, PWideChar(CaptionBuffer), (Length(FCaption) + 1));
  end
  else
  begin
    if (CaptionBuffer <> nil) then
      StrPCopy(PChar(CaptionBuffer), FCaption);
  end;
  if Assigned(SHOutOfMemoryMessageBox) then
    Result := Boolean(SHOutOfMemoryMessageBox(Application.Handle, CaptionBuffer, MB_OK or MB_ICONHAND))
  else
    raise EWinDialogError.Create(sNotSupported);
end;

{ TJvShellAboutDialog }

constructor TJvShellAboutDialog.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TIcon.Create;
end;

destructor TJvShellAboutDialog.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TJvShellAboutDialog.SetIcon(NewValue: TIcon);
begin
  FIcon.Assign(NewValue);
end;

function TJvShellAboutDialog.StoreIcon: Boolean;
begin
  Result := (not FIcon.Empty);
end;

function TJvShellAboutDialog.Execute: Boolean;
const
  AboutText = 'JvDialogs 2.0';
  CaptionSeparator = '#';
var
  CaptionText: string;
begin
  if (Caption = EmptyStr) then
  begin
    CaptionText := AboutText;
  end
  else
  begin
    CaptionText := Caption;
  end;

  CaptionText := CaptionText + CaptionSeparator + Product;

  Win32Check(LongBool(ShellAbout(Application.MainForm.Handle,
    PChar(CaptionText),
    PChar(OtherText),
    FIcon.Handle)));
  Result := True;
end;

{ TJvRunDialog }

constructor TJvRunDialog.Create(AOwner: TComponent);
begin
  inherited;
  FCaption := '';
  FDescription := '';
  FIcon := TIcon.Create;
end;

destructor TJvRunDialog.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TJvRunDialog.Execute;
var
  CaptionBuffer: Pointer;
  DescriptionBuffer: Pointer;
begin
  CaptionBuffer := nil;
  DescriptionBuffer := nil;

  if (FCaption <> '') then
    GetMem(CaptionBuffer, (Length(FCaption) + 1) * SizeOf(WideChar));

  if (FDescription <> '') then
    GetMem(DescriptionBuffer, (Length(FDescription) + 1) * SizeOf(WideChar));

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if (CaptionBuffer <> nil) then
      StringToWideChar(FCaption, PWideChar(CaptionBuffer), (Length(FCaption) + 1));
    if (DescriptionBuffer <> nil) then
      StringToWideChar(FDescription, PWideChar(DescriptionBuffer), (Length(FDescription) + 1));
  end
  else
  begin
    if (CaptionBuffer <> nil) then
      StrPCopy(PChar(CaptionBuffer), FCaption);
    if (DescriptionBuffer <> nil) then
      StrPCopy(PChar(DescriptionBuffer), FDescription);
  end;

  if Assigned(SHRunDialog) then
    SHRunDialog(Application.Handle, FIcon.Handle, nil, CaptionBuffer, DescriptionBuffer, 0)
  else
    raise EWinDialogError.Create(sNotSupported);
end;

procedure TJvRunDialog.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
end;

{ TJvObjectPropertiesDialog }

function TJvObjectPropertiesDialog.Execute: boolean;
var
  ObjectNameBuffer: Pointer;
  TabNameBuffer: Pointer;
begin
  GetMem(ObjectNameBuffer, (Length(ObjectName) + 1) * SizeOf(WideChar));
  try
    if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
      StringToWideChar(ObjectName, PWideChar(ObjectNameBuffer), (Length(ObjectName) + 1));
    end
    else
    begin
      StrPCopy(PChar(ObjectNameBuffer), ObjectName);
    end;

    GetMem(TabNameBuffer, (Length(InitialTab) + 1) * SizeOf(WideChar));
    try
      if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        StringToWideChar(InitialTab, PWideChar(TabNameBuffer), (Length(InitialTab) + 1));
      end
      else
      begin
        StrPCopy(PChar(TabNameBuffer), InitialTab);
      end;
      Result := SHObjectProperties(Application.Handle, ShellObjectTypeEnumToConst(ObjectType), ObjectNameBuffer, TabNameBuffer);
    finally
      FreeMem(TabNameBuffer);
    end;
  finally
    FreeMem(ObjectNameBuffer);
  end;
end;

function ShellObjectTypeEnumToConst(ShellObjectType: TShellObjectType):
  UINT;
begin
  case (ShellObjectType) of
    sdPathObject: Result := OPF_PATHNAME;
    sdPrinterObject: Result := OPF_PRINTERNAME;
  else
    Result := 0;
  end;
end;

function ShellObjectTypeConstToEnum(ShellObjectType: UINT):
  TShellObjectType;
begin
  case (ShellObjectType) of
    OPF_PATHNAME: Result := sdPathObject;
    OPF_PRINTERNAME: Result := sdPrinterObject;
  else
    Result := sdPathObject;
  end;
end;

{ TNewLinkDialog }

procedure TJvNewLinkDialog.Execute;
begin
  NewLinkHere(0, 0, PChar(DestinationFolder), 0);
end;

{ TJvAddHardwareDialog }

procedure TJvAddHardwareDialog.Execute;
var APModule: THandle;
  Applet: Applet_PROC;
begin
  APModule := LoadLibrary('hdwwiz.cpl');
  if APModule <= HINSTANCE_ERROR then
    Exit;
  Applet := Applet_proc(GetProcAddress(APModule, 'CPlApplet'));
  Applet(0, CPL_DBLCLK, 0, 0);
  FreeLibrary(ApModule);
end;

function CreateShellLink(const AppName, Desc: string; Dest: string): string;
{ Creates a shell link for application or document specified in  }
{ AppName with description Desc.  Link will be located in folder }
{ specified by Dest, which is one of the string constants shown  }
{ at the top of this unit.  Returns the full path name of the    }
{ link file. }
var
  SL: IShellLink;
  PF: IPersistFile;
  LnkName: WideString;
begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IShellLink, SL));
  { The IShellLink implementer must also support the IPersistFile }
  { interface. Get an interface pointer to it. }
  PF := SL as IPersistFile;
  OleCheck(SL.SetPath(PChar(AppName))); // set link path to proper file
  if Desc <> '' then
    OleCheck(SL.SetDescription(PChar(Desc))); // set description
  { create a path location and filename for link file }
  LnkName := GetSpecialFolderPath(Dest, True) + '\' +
    ChangeFileExt(AppName, 'lnk');
  PF.Save(PWideChar(LnkName), True); // save link file
  Result := LnkName;
end;

procedure GetShellLinkInfo(const LinkFile: WideString; var SLI: TShellLinkInfo);
{ Retrieves information on an existing shell link }
var
  SL: IShellLink;
  PF: IPersistFile;
  FindData: TWin32FindData;
  AStr: array[0..MAX_PATH] of char;
begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IShellLink, SL));
  { The IShellLink implementer must also support the IPersistFile }
  { interface. Get an interface pointer to it. }
  PF := SL as IPersistFile;
  { Load file into IPersistFile object }
  OleCheck(PF.Load(PWideChar(LinkFile), STGM_READ));
  { Resolve the link by calling the Resolve interface function. }
  OleCheck(SL.Resolve(0, SLR_ANY_MATCH or SLR_NO_UI));
  { Get all the info! }
  with SLI do
  begin
    OleCheck(SL.GetPath(AStr, MAX_PATH, FindData, SLGP_SHORTPATH));
    PathName := AStr;
    OleCheck(SL.GetArguments(AStr, MAX_PATH));
    Arguments := AStr;
    OleCheck(SL.GetDescription(AStr, MAX_PATH));
    Description := AStr;
    OleCheck(SL.GetWorkingDirectory(AStr, MAX_PATH));
    WorkingDirectory := AStr;
    OleCheck(SL.GetIconLocation(AStr, MAX_PATH, IconIndex));
    IconLocation := AStr;
    OleCheck(SL.GetShowCmd(ShowCmd));
    OleCheck(SL.GetHotKey(HotKey));
  end;
end;

procedure SetShellLinkInfo(const LinkFile: WideString; const SLI: TShellLinkInfo);
{ Sets information for an existing shell link }
var
  SL: IShellLink;
  PF: IPersistFile;
begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IShellLink, SL));
  { The IShellLink implementer must also support the IPersistFile }
  { interface. Get an interface pointer to it. }
  PF := SL as IPersistFile;
  { Load file into IPersistFile object }
  OleCheck(PF.Load(PWideChar(LinkFile), STGM_SHARE_DENY_WRITE));
  { Resolve the link by calling the Resolve interface function. }
  OleCheck(SL.Resolve(0, SLR_ANY_MATCH or SLR_UPDATE or SLR_NO_UI));
  { Set all the info! }
  with SLI, SL do
  begin
    OleCheck(SetPath(PChar(PathName)));
    OleCheck(SetArguments(PChar(Arguments)));
    OleCheck(SetDescription(PChar(Description)));
    OleCheck(SetWorkingDirectory(PChar(WorkingDirectory)));
    OleCheck(SetIconLocation(PChar(IconLocation), IconIndex));
    OleCheck(SetShowCmd(ShowCmd));
    OleCheck(SetHotKey(HotKey));
  end;
  PF.Save(PWideChar(LinkFile), True); // save file
end;

function RecycleFile(FileToRecycle: string): boolean;
var Struct: TSHFileOpStruct;
  pFromc: PChar;
  Resultval: integer;
begin
  if not FileExists(FileToRecycle) then
  begin
    RecycleFile := False;
    exit;
  end
  else
  begin
    pfromc := PChar(ExpandFileName(FileToRecycle) + #0#0);
    Struct.wnd := 0;
    Struct.wFunc := FO_DELETE;
    Struct.pFrom := pFromC;
    Struct.pTo := nil;
    Struct.fFlags := FOF_ALLOWUNDO;
    Struct.fAnyOperationsAborted := false;
    Struct.hNameMappings := nil;
    Resultval := ShFileOperation(Struct);
    RecycleFile := (Resultval = 0);
  end;
end;

function CopyFile(FromFile, ToDir: string): boolean;
var F: TShFileOpStruct;
begin
  F.Wnd := 0;
  F.wFunc := FO_COPY;
  FromFile := FromFile + #0;
  F.pFrom := pchar(FromFile);
  ToDir := ToDir + #0;
  F.pTo := pchar(ToDir);
  F.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
  result := ShFileOperation(F) = 0;
end;

type CPLApplet = function(hwndCPL: HWND; uMsg: UINT;
    lParam1: LPARAM; lParam2: LPARAM): LongInt; stdcall;

procedure ExecuteApplet(AppletName: string);
var APModule: THandle;
  Applet: CPLApplet;
begin
  APModule := LoadLibrary(Pchar(AppletName));
  if APModule <= HINSTANCE_ERROR then
    Exit;
  Applet := CPLApplet(GetProcAddress(APModule, 'CPlApplet'));
  Applet(0, CPL_DBLCLK, 0, 0);
  FreeLibrary(ApModule);
end;

{ TJvOpenWithDialog }

procedure TJvOpenWithDialog.Execute;
begin
  SHOpenWith(0, 0, PChar(FFileName), SW_SHOW);
end;

{ TJvDiskFullDialog }

constructor TJvDiskFullDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DriveChar := 'C';
end;

function TJvDiskFullDialog.GetDrive: UINT;
begin
  Result := Ord(FDriveChar) - Ord('A');
end;

function TJvDiskFullDialog.Execute: boolean;
begin
  if not Assigned(SHHandleDiskFull) then
    raise EWinDialogError.Create(SNotSupported);
  Result := GetDriveType(PChar(DriveChar + ':\')) = 3;
  if Result then
    SHHandleDiskFull(GetFocus, GetDrive)
  else
    raise EWinDialogError.CreateFmt(SUnSupportedDisk, [DriveChar]);
end;

procedure TJvDiskFullDialog.SetDriveChar(Value: Char);
begin
  if (Value in ['a'..'z']) then
    Value := Char(ord(Value) - $20);
  if (not (Value in ['A'..'Z'])) then
    raise EWinDialogError.CreateFmt(SInvalidDriveChar, [Value]);
  FDriveChar := Value;
end;

{ TJvExitWindowsDialog }

procedure TJvExitWindowsDialog.Execute;
begin
  SHShutDownDialog(Application.Handle);
end;

{ TJvChangeIconDialog }

procedure TJvChangeIconDialog.Execute;
begin
  if Assigned(SHChangeIcon) then
    SHChangeIcon(Application.Handle, nil, 0, FIconIndex)
  else
    raise EWinDialogError.Create(SNotSupported);
end;

function OpenInterceptor(var DialogData: TOpenFileName):
  Bool; stdcall;
var
  DialogDataEx: TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  DialogDataEx.FlagsEx := 0;
  DialogDataEx.lStructSize := SizeOf(TOpenFileNameEx);
  Result := GetOpenFileNameEx(DialogDataEx);
end;

function SaveInterceptor(var DialogData: TOpenFileName):
  Bool; stdcall;
var
  DialogDataEx: TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  DialogDataEx.FlagsEx := 0;
  DialogDataEx.lStructSize := SizeOf(TOpenFileNameEx);
  Result := GetSaveFileNameEx(DialogDataEx);
end;

{ TJvOpenDialog2000 }

function TJvOpenDialog2000.Execute: Boolean;
begin
  if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    Result := DoExecute(@OpenInterceptor);
  end
  else
    Result := inherited Execute;
end;

{ TJvSaveDialog2000 }

function TJvSaveDialog2000.Execute: Boolean;
begin
  if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    Result := DoExecute(@SaveInterceptor);
  end
  else
    Result := inherited Execute;
end;

initialization
  LoadJvDialogs;
finalization
  UnloadJvDialogs;

end.

