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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWinDialogs;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, ShellAPI, ShlObj, ComObj, ActiveX, CommDlg, UrlMon,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvJCLUtils; // For OSCheck

{$HPPEMIT '#include "dbt.h"'}

type
  EShellOleError = class(Exception);
  EWinDialogError = class(Exception);

  TShellLinkInfo = record
    PathName: string;
    Arguments: string;
    Description: string;
    WorkingDirectory: string;
    IconLocation: string;
    IconIndex: Integer;
    ShowCmd: Integer;
    HotKey: Word;
  end;

  TSpecialFolderInfo = record
    Name: string;
    ID: Integer;
  end;

const
  OFN_EX_NOPLACESBAR = 1; // for new style of standard Windows dialogs
  {$EXTERNALSYM OFN_EX_NOPLACESBAR}

  SpecialFolders: array [0..29] of TSpecialFolderInfo = (
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
  TOpenFileNameExA = record
    lStructSize: DWORD; // Size of the structure in bytes.
    hWndOwner: HWND; // Handle that is the parent of the dialog.
    hInstance: HINST; // Application instance handle.
    lpstrFilter: PAnsiChar; // String containing filter information.
    lpstrCustomFilter: PAnsiChar; // Will hold the filter chosen by the user.
    nMaxCustFilter: DWORD; // Size of lpstrCustomFilter, in bytes.
    nFilterIndex: DWORD; // Index of the filter to be shown.
    lpstrFile: PAnsiChar; // File name to start with (and retrieve).
    nMaxFile: DWORD; // Size of lpstrFile, in bytes.
    lpstrFileTitle: PAnsiChar; // File name without path will be returned.
    nMaxFileTitle: DWORD; // Size of lpstrFileTitle, in bytes.
    lpstrInitialDir: PAnsiChar; // Starting directory.
    lpstrTitle: PAnsiChar; // Title of the open dialog.
    Flags: DWORD; // Controls user selection Options.
    nFileOffset: Word; // Offset of file name in filepath=lpstrFile.
    nFileExtension: Word; // Offset of extension in filepath=lpstrFile.
    lpstrDefExt: PAnsiChar; // Default extension if no extension typed.
    lCustData: LPARAM; // Custom data to be passed to hook.
    lpfnHook: function(Wnd: THandle; Msg: UINT; wParam: WPARAM;
      lParam: LPARAM): UINT stdcall; // Hook.
    lpTemplateName: PAnsiChar; // Template dialog, if applicable.
    // Extended structure starts here.
    pvReserved: Pointer; // Reserved, use nil.
    dwReserved: DWORD; // Reserved, use 0.
    FlagsEx: DWORD; // Extended Flags.
  end;

  TOpenFileNameExW = record
    lStructSize: DWORD; // Size of the structure in bytes.
    hWndOwner: HWND; // Handle that is the parent of the dialog.
    hInstance: HINST; // Application instance handle.
    lpstrFilter: PWideChar; // String containing filter information.
    lpstrCustomFilter: PWideChar; // Will hold the filter chosen by the user.
    nMaxCustFilter: DWORD; // Size of lpstrCustomFilter, in bytes.
    nFilterIndex: DWORD; // Index of the filter to be shown.
    lpstrFile: PWideChar; // File name to start with (and retrieve).
    nMaxFile: DWORD; // Size of lpstrFile, in bytes.
    lpstrFileTitle: PWideChar; // File name without path will be returned.
    nMaxFileTitle: DWORD; // Size of lpstrFileTitle, in bytes.
    lpstrInitialDir: PWideChar; // Starting directory.
    lpstrTitle: PWideChar; // Title of the open dialog.
    Flags: DWORD; // Controls user selection Options.
    nFileOffset: Word; // Offset of file name in filepath=lpstrFile.
    nFileExtension: Word; // Offset of extension in filepath=lpstrFile.
    lpstrDefExt: PWideChar; // Default extension if no extension typed.
    lCustData: LPARAM; // Custom data to be passed to hook.
    lpfnHook: function(Wnd: THandle; Msg: UINT; wParam: WPARAM;
      lParam: LPARAM): UINT stdcall; // Hook.
    lpTemplateName: PWideChar; // Template dialog, if applicable.
    // Extended structure starts here.
    pvReserved: Pointer; // Reserved, use nil.
    dwReserved: DWORD; // Reserved, use 0.
    FlagsEx: DWORD; // Extended Flags.
  end;

  {$IFDEF UNICODE}
  TOpenFileNameEx = TOpenFileNameExW;
  {$ELSE}
  TOpenFileNameEx = TOpenFileNameExA;
  {$ENDIF UNICODE}

  TShellObjectType = (sdPathObject, sdPrinterObject);
  TShellObjectTypes = set of TShellObjectType;

  TJvFormatDriveKind = (ftQuick, ftStandard, ftBootable);
  TJvDriveCapacity = (dcDefault, dcSize360kB, dcSize720kB);
  TJvFormatDriveError = (errParams, errSysError, errAborted, errCannotFormat, errOther);
  TJvFormatDriveErrorEvent = procedure(Sender: TObject; Error: TJvFormatDriveError) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFormatDriveDialog = class(TJvCommonDialog)
  private
    FDrive: Char;
    FFormatType: TJvFormatDriveKind;
    FCapacity: TJvDriveCapacity;
    FOnError: TJvFormatDriveErrorEvent;
    procedure SetDrive(Value: Char);
  protected
    procedure DoError(ErrValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; override;
  published
    property Drive: Char read FDrive write SetDrive default 'A';
    property FormatType: TJvFormatDriveKind read FFormatType write FFormatType;
    property Capacity: TJvDriveCapacity read FCapacity write FCapacity;
    property OnError: TJvFormatDriveErrorEvent read FOnError write FOnError;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOrganizeFavoritesDialog = class(TJvCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

  TJvCplInfo = record
    Icon: TIcon;
    Name: string;
    Info: string;
    lData: Longint;
  end;

  // the signature of procedures in CPL's that implements Control Panel functionality
  TCplApplet = function(hwndCPl: THandle; uMsg: UINT; lParam1, lParam2: LPARAM): Longint; stdcall;

  // (rom) largely reimplemented
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvAppletDialog = class(TJvCommonDialog)
  private
    FAppletName: string;
    FAppletIndex: Integer;
    FModule: HMODULE;
    FCount: Integer;
    FAppletFunc: TCplApplet;
    FAppletInfo: array of TJvCplInfo;
    function GetAppletInfo(Index: Integer): TJvCplInfo;
    procedure SetAppletName(const AAppletName: string);
    procedure Unload;
    procedure Load;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValidApplet: Boolean;
    // (p3) NOTE: if AppletName or AppletIndex is invalid, shows the control
    // panel explorer window instead and returns FALSE
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property Count: Integer read FCount;
    property AppletInfo[Index: Integer]: TJvCplInfo read GetAppletInfo;
  published
    property AppletName: string read FAppletName write SetAppletName;
    property AppletIndex: Integer read FAppletIndex write FAppletIndex default 0;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvComputerNameDialog = class(TJvCommonDialog)
  private
    FComputerName: string;
    FCaption: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property ComputerName: string read FComputerName;
  published
    property Caption: string read FCaption write FCaption;
  end;

  // (p3) could be removed - a more complete implementation is in JvBrowseFolder
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvBrowseFolderDialog = class(TJvCommonDialog)
  private
    FFolderName: string;
    FCaption: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property FolderName: string read FFolderName;
  published
    property Caption: string read FCaption write FCaption;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOutOfMemoryDialog = class(TJvCommonDialog)
  private
    FCaption: string;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property Caption: string read FCaption write FCaption;
  end;

  // (rom) changed to new TJvCommonDialog to get better Execute
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvChangeIconDialog = class(TJvCommonDialog)
  private
    FIconIndex: Integer;
    FFileName: TFileName;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property IconIndex: Integer read FIconIndex write FIconIndex;
    property FileName: TFileName read FFileName write FFileName;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvShellAboutDialog = class(TJvCommonDialog)
  private
    FCaption: string;
    FIcon: TIcon;
    FOtherText: string;
    FProduct: string;
    procedure SetIcon(NewValue: TIcon);
    function StoreIcon: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property Caption: string read FCaption write FCaption;
    property Icon: TIcon read FIcon write SetIcon stored StoreIcon;
    property OtherText: string read FOtherText write FOtherText;
    property Product: string read FProduct write FProduct;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvRunDialog = class(TJvCommonDialog)
  private
    FCaption: string;
    FDescription: string;
    FIcon: TIcon;
    procedure SetIcon(const Value: TIcon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
    property Icon: TIcon read FIcon write SetIcon;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvObjectPropertiesDialog = class(TJvCommonDialog)
  private
    FObjectName: TFileName;
    FObjectType: TShellObjectType;
    FInitialTab: string;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property ObjectName: TFileName read FObjectName write FObjectName;
    property ObjectType: TShellObjectType read FObjectType write FObjectType;
    property InitialTab: string read FInitialTab write FInitialTab;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvNewLinkDialog = class(TJvCommonDialog)
  private
    FDestinationFolder: string;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property DestinationFolder: string read FDestinationFolder write FDestinationFolder;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvAddHardwareDialog = class(TJvCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOpenWithDialog = class(TJvCommonDialog)
  private
    FFileName: TFileName;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property FileName: TFileName read FFileName write FFileName;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDiskFullDialog = class(TJvCommonDialog)
  private
    FDriveChar: Char;
    procedure SetDriveChar(Value: Char);
    function GetDrive: UINT;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property DriveChar: Char read FDriveChar write SetDriveChar default 'C';
  end;

  TJvExitWindowsKind = (
    ekXPDialog = 0,        // Show XP style shutdown dialog
    ekVistaLogoff = 1,     // Vista Logoff without dialog
    ekVistaShutdown = 2    // Vista Shutdown without dialog
  );

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvExitWindowsDialog = class(TJvCommonDialog)
  private
    FKind: TJvExitWindowsKind;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    property Kind: TJvExitWindowsKind read FKind write FKind default ekXPDialog;
  end;

  // (p3) this extension (PlacesBar) is already in TJvOpenDialog
  TJvOpenDialog2000 = class(TOpenDialog)
  public
    function Execute{$IFDEF RTL180_UP}(ParentWnd: HWND){$ENDIF RTL180_UP}: Boolean; overload; override;
  end;

  // (p3) this extension (PlacesBar) is already in TJvSaveDialog
  TJvSaveDialog2000 = class(TSaveDialog)
  public
    function Execute{$IFDEF RTL180_UP}(ParentWnd: HWND){$ENDIF RTL180_UP}: Boolean; overload; override;
  end;


  TJvURLAssociationDialogOption = (uaDefaultName, uaRegisterAssoc);
  TJvURLAssociationDialogOptions = set of TJvURLAssociationDialogOption;

  TJvURLAssociationDialog = class(TJvCommonDialog)
  private
    FURL: string;
    FAssociatedApp: string;
    FFileName: TFileName;
    FOptions: TJvURLAssociationDialogOptions;
    FDefaultProtocol: string;
    FReturnValue: HRESULT;
  public
    // Returns false if user cancelled or if the user
    // elected not to register the association. To find out if the user made
    // a one-time choice, check the AssociatedApp property: if it is empty,
    // the user cancelled
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    constructor Create(AOwner: TComponent); override;
    // After Execute, contains the path and filename to the associated application (if user didn't cancel)
    property AssociatedApp: string read FAssociatedApp;
    // Value returned by the function called by Execute.
    // Possible return values:
    // S_OK -  content type succesfully associated with the extnesion
    // S_FALSE - nothing was registered (f ex a one time registration)
    property ReturnValue: HRESULT read FReturnValue;
  published
    // The file (type) to associate with the Protocol
    // NB! FileName *must* contain an extension!
    property FileName: TFileName read FFileName write FFileName;
    // The URL with the protocol to assoiacte with FileName
    // NB! if URL has no protocol (i.e "http://", "mailto:", "home-made:", etc),
    // the function fails even before the dialog is displayed!
    property URL: string read FURL write FURL;
    // DefaultProtocol to prepend to URL if it doesn't have a protocol
    property DefaultProtocol: string read FDefaultProtocol write FDefaultProtocol;
    // Options for the dialog
    property Options: TJvURLAssociationDialogOptions read FOptions write FOptions default [];
  end;

  TJvMIMEAssociationOption = (maRegisterAssoc);
  TJvMIMEAssociationOptions = set of TJvMIMEAssociationOption;

  TJvMIMEAssociationDialog = class(TJvCommonDialog)
  private
    FContentType: string;
    FAssociatedApp: string;
    FFileName: TFileName;
    FOptions: TJvMIMEAssociationOptions;
    FReturnValue: HRESULT;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    // After Execute, contains the path and filename to the associated application (if user didn't cancel)
    property AssociatedApp: string read FAssociatedApp;
    // Value returned by the function called by Execute.
    // Possible return values:
    // S_OK -  content type succesfully associated with the extnesion
    // S_FALSE - nothing was registered
    // E_ABORT - user cancelled
    // E_FLAGS - invalid flag combination
    // E_OUTOFMEMORY - out of memory
    // E_POINTER - one of the input pointers are invalid
    property ReturnValue: HRESULT read FReturnValue;
  published
    // The file (type) to associate with the Protocol
    // NB! FileName *must* contain an extension!
    property FileName: TFileName read FFileName write FFileName;
    // The MIME contentype of FileName
    property ContentType: string read FContentType write FContentType;
    property Options: TJvMIMEAssociationOptions read FOptions write FOptions default [];
  end;

const
  SOFTDIST_FLAG_USAGE_EMAIL = $0001;
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_EMAIL}
  SOFTDIST_FLAG_USAGE_PRECACHE = $0002;
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_PRECACHE}
  SOFTDIST_FLAG_USAGE_AUTOINSTALL = $0003;
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_AUTOINSTALL}
  SOFTDIST_FLAG_DELETE_SUBSCRIPTION = $0004;
  {$EXTERNALSYM SOFTDIST_FLAG_DELETE_SUBSCRIPTION}

type
  _tagSOFTDISTINFO = packed record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwAdState: DWORD;
    lpszTitle: LPWSTR;
    lpszAbstract: LPWSTR;
    lpszHREF: LPWSTR;
    dwInstalledVersionMS: DWORD;
    dwInstalledVersionLS: DWORD;
    dwUpdateVersionMS: DWORD;
    dwUpdateVersionLS: DWORD;
    dwAdvertisedVersionMS: DWORD;
    dwAdvertisedVersionLS: DWORD;
    cbReserved: DWORD;
  end;
  {$EXTERNALSYM _tagSOFTDISTINFO}
  {$EXTERNALSYM SOFTDISTINFO}
  SOFTDISTINFO = _tagSOFTDISTINFO;
  {$EXTERNALSYM SOFTDISTINFO}
  LPSOFTDISTINFO = ^_tagSOFTDISTINFO;
  {$EXTERNALSYM LPSOFTDISTINFO}
  TSoftDistInfo = SOFTDISTINFO;

  TJvSoftwareUpdateAdState = (asNone, asAvailable, asDownloaded, asInstalled);
  TJvSoftwareUpdateFlags = (ufEmail, ufPreCache, ufAutoInstall, ufDeleteSubscription);

  TJvSoftwareUpdateInfo = class(TPersistent)
  private
    FInstalledVersionMS: DWORD;
    FUpdateVersionLS: DWORD;
    FUpdateVersionMS: DWORD;
    FAdvertisedVersionMS: DWORD;
    FAdvertisedVersionLS: DWORD;
    FInstalledVersionLS: DWORD;
    FDescription: string;
    FTitle: string;
    FHREF: string;
    FAdState: TJvSoftwareUpdateAdState;
    FFlags: TJvSoftwareUpdateFlags;
    function GetSoftDistInfo: TSoftDistInfo;
    procedure SetSoftDistInfo(const Value: TSoftDistInfo);
  public
    property SoftDistInfo: TSoftDistInfo read GetSoftDistInfo write SetSoftDistInfo;
  published
    property AdState: TJvSoftwareUpdateAdState read FAdState write FAdState;
    property Flags: TJvSoftwareUpdateFlags read FFlags write FFlags;
    property Title: string read FTitle write FTitle;
    property HREF: string read FHREF write FHREF;
    property Description: string read FDescription write FDescription;
    property InstalledVersionMS: DWORD read FInstalledVersionMS write FInstalledVersionMS;
    property InstalledVersionLS: DWORD read FInstalledVersionLS write FInstalledVersionLS;
    property UpdateVersionMS: DWORD read FUpdateVersionMS write FUpdateVersionMS;
    property UpdateVersionLS: DWORD read FUpdateVersionLS write FUpdateVersionLS;
    property AdvertisedVersionMS: DWORD read FAdvertisedVersionMS write FAdvertisedVersionMS;
    property AdvertisedVersionLS: DWORD read FAdvertisedVersionLS write FAdvertisedVersionLS;
  end;

  // (p3) encapsulation of the SoftwareUpdateMessageBox ( for CDF file updating)
  TJvSoftwareUpdateDialog = class(TJvCommonDialog)
  private
    FReturnValue: Cardinal;
    FDistributionUnit: string;
    FDistInfo: TJvSoftwareUpdateInfo;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property ReturnValue: Cardinal read FReturnValue;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DistributionUnit: string read FDistributionUnit write FDistributionUnit;
    property DistInfo: TJvSoftwareUpdateInfo read FDistInfo write FDistInfo;
  end;

// Tools routines
function GetSpecialFolderPath(const FolderName: string; CanCreate: Boolean): string;
procedure AddToRecentDocs(const FileName: string);
procedure ClearRecentDocs;
function ExtractIconFromFile(FileName: string; Index: Integer): HICON;
function CreateShellLink(const AppName, Desc: string; Dest: string): string;
procedure GetShellLinkInfo(const LinkFile: WideString; var SLI: TShellLinkInfo);
procedure SetShellLinkInfo(const LinkFile: WideString; const SLI: TShellLinkInfo);
function RecycleFile(FileToRecycle: string): Boolean;
function CopyFile(FromFile, ToDir: string): Boolean;
function ShellObjectTypeEnumToConst(ShellObjectType: TShellObjectType): UINT;
function ShellObjectTypeConstToEnum(ShellObjectType: UINT): TShellObjectType;

type
  FreePIDLProc = procedure(PIDL: PItemIDList); stdcall;
  SHChangeIconProc = function(Wnd: THandle; szFileName: PChar; Reserved: Integer;
    var lpIconIndex: Integer): DWORD; stdcall;
  SHChangeIconProcW = function(Wnd: THandle; szFileName: PWideChar;
    Reserved: Integer; var lpIconIndex: Integer): DWORD; stdcall;
  SHFormatDriveProc = function(Wnd: THandle; Drive: UINT; fmtID: UINT;
    Options: UINT): DWORD; stdcall;
  SHShutDownDialogProc = procedure(Wnd: THandle); stdcall;
  SHShutDownDialog6Proc = procedure(Wnd: THandle; Kind: Integer); stdcall; // Vista or newer
  SHRunDialogProc = function(Wnd: THandle; Unknown1: Integer; Unknown2: Pointer;
    szTitle: PChar; szPrompt: PChar; uiFlages: Integer): DWORD; stdcall;
  SHFindFilesProc = function(Root: PItemIDList; SavedSearchFile: PItemIDList): LongBool; stdcall;
  SHFindComputerProc = function(Reserved1: PItemIDList; Reserved2: PItemIDList): LongBool; stdcall;
  SHObjectPropertiesProc = function(Owner: THandle; Flags: UINT;
    ObjectName: Pointer; InitialTabName: Pointer): LongBool; stdcall;
  SHNetConnectionDialogProc = function(Owner: THandle; ResourceName: Pointer;
    ResourceType: DWORD): DWORD; stdcall;
  SHStartNetConnectionDialogProc = function(Owner: THandle;
    ResourceName: PWideChar; ResourceType: DWORD): DWORD; stdcall;
  SHOutOfMemoryMessageBoxProc = function(Owner: THandle; Caption: Pointer;
    Style: UINT): Integer; stdcall;
  SHHandleDiskFullProc = procedure(Owner: THandle; uDrive: UINT); stdcall;
  NewLinkHereProc = procedure(HWND: THandle; HInstance: THandle; CmdLine: PChar;
    CmdShow: Integer); stdcall;
  SHOpenWithProc = procedure(HWND: THandle; HInstance: THandle; CmdLine: PChar;
    CmdShow: Integer); stdcall;
  GetOpenFileNameExProc = function(var OpenFile: TOpenFileNameEx): BOOL; stdcall;
  GetSaveFileNameExProc = function(var SaveFile: TOpenFileNameEx): BOOL; stdcall;
  
  URLAssociationDialogProcA = function(hwndParent: THandle; dwInFlags: DWORD; const pcszFile: PAnsiChar; const pcszURL: PAnsiChar;
    pszBuff: PAnsiChar; ucAppBufLen: UINT): HRESULT; stdcall;
  URLAssociationDialogProcW = function(hwndParent: THandle; dwInFlags: DWORD; const pcszFile: PWideChar; const pcszURL:
    PWideChar; pszBuff: PWideChar; ucAppBufLen: UINT): HRESULT; stdcall;

  URLAssociationDialogProc = {$IFDEF UNICODE}URLAssociationDialogProcW{$ELSE}URLAssociationDialogProcA{$ENDIF UNICODE};

  MIMEAssociationDialogProcA = function(hwndParent: THandle; dwInFlags: DWORD;
    const pcszFile: PAnsiChar; const pcszMIMEContentType: PAnsiChar; pszAppBuf: PAnsiChar; ucAppBufLen: UINT): HRESULT; stdcall;
  MIMEAssociationDialogProcW = function(hwndParent: THandle; dwInFlags: DWORD;
    const pcszFile: PWideChar; const pcszMIMEContentType: PWideChar; pszAppBuf: PWideChar;
      ucAppBufLen: UINT): HRESULT; stdcall;

  MIMEAssociationDialogProc = {$IFDEF UNICODE}MIMEAssociationDialogProcW{$ELSE}MIMEAssociationDialogProcA{$ENDIF UNICODE};

  SoftwareUpdateMessageBoxProc = function(hWnd: THandle; szDistUnit: LPCWSTR; dwFlags: DWORD;
    var psdi: TSoftDistInfo): DWORD; stdcall;

var
  FreePIDL: FreePIDLProc = nil;
  GetOpenFileNameEx: GetOpenFileNameExProc = nil;
  GetSaveFileNameEx: GetSaveFileNameExProc = nil;
  SHFormatDrive: SHFormatDriveProc = nil;
  SHShutDownDialog: SHShutDownDialogProc = nil;
  SHShutDownDialog6: SHShutDownDialog6Proc = nil;
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
  SHChangeIconW: SHChangeIconProcW = nil;
  URLAssociationDialog: URLAssociationDialogProc = nil;
  MIMEAssociationDialog: MIMEAssociationDialogProc = nil;
  SoftwareUpdateMessageBox: SoftwareUpdateMessageBoxProc = nil;

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
  JvResources;

const
  Shell32 = 'shell32.dll';
  URLASSOCDLG_FL_USE_DEFAULT_NAME = $0001;
  URLASSOCDLG_FL_REGISTER_ASSOC = $0002;
  MIMEASSOCDLG_FL_REGISTER_ASSOC = $0001;

var
  ShellHandle: THandle = 0;
  CommHandle: THandle = 0;
  AppWizHandle: THandle = 0;
  URLHandle: THandle = 0;
  SHDocvwHandle: THandle = 0;

procedure LoadJvDialogs;
begin
  ShellHandle := SafeLoadLibrary(Shell32);
  if ShellHandle <> 0 then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      @SHChangeIconW := GetProcAddress(ShellHandle, PAnsiChar(62))
    else
      @SHChangeIcon := GetProcAddress(ShellHandle, PAnsiChar(62));
    @SHFormatDrive := GetProcAddress(ShellHandle, PAnsiChar('SHFormatDrive'));
    @FreePIDL := GetProcAddress(ShellHandle, PAnsiChar(155));
    if CheckWin32Version(6, 0) then
      @SHShutDownDialog6 := GetProcAddress(ShellHandle, PAnsiChar(60))
    else
      @SHShutDownDialog := GetProcAddress(ShellHandle, PAnsiChar(60));
    @SHRunDialog := GetProcAddress(ShellHandle, PAnsiChar(61));
    @SHFindFiles := GetProcAddress(ShellHandle, PAnsiChar(90));
    @SHFindComputer := GetProcAddress(ShellHandle, PAnsiChar(91));
    @SHObjectProperties := GetProcAddress(ShellHandle, PAnsiChar(178));
    @SHNetConnectionDialog := GetProcAddress(ShellHandle, PAnsiChar(160));
    @SHOutOfMemoryMessageBox := GetProcAddress(ShellHandle, PAnsiChar(126));
    @SHHandleDiskFull := GetProcAddress(ShellHandle, PAnsiChar(185));
    @SHStartNetConnectionDialog := GetProcAddress(ShellHandle, PAnsiChar(215));

    @SHOpenWith := GetProcAddress(ShellHandle, {$IFDEF UNICODE}'OpenAs_RunDLLW'{$ELSE}'OpenAs_RunDLLA'{$ENDIF UNICODE});
  end;

  CommHandle := SafeLoadLibrary('comdlg32.dll');
  if CommHandle <> 0 then
  begin
    @GetOpenFileNameEx := GetProcAddress(CommHandle, {$IFDEF UNICODE}'GetOpenFileNameW'{$ELSE}'GetOpenFileNameA'{$ENDIF UNICODE});
    @GetSaveFileNameEx := GetProcAddress(CommHandle, {$IFDEF UNICODE}'GetSaveFileNameW'{$ELSE}'GetSaveFileNameA'{$ENDIF UNICODE});
  end;

  AppWizHandle := SafeLoadLibrary('appwiz.cpl');
  if AppWizHandle <> 0 then
    @NewLinkHere := GetProcAddress(AppWizHandle, {$IFDEF UNICODE}'NewLinkHereW'{$ELSE}'NewLinkHereA'{$ENDIF UNICODE});
  URLHandle := SafeLoadLibrary('url.dll');
  if URLHandle <> 0 then
  begin
    @URLAssociationDialog := GetProcAddress(URLHandle, {$IFDEF UNICODE}'URLAssociationDialogW'{$ELSE}'URLAssociationDialogA'{$ENDIF UNICODE});
    @MIMEAssociationDialog := GetProcAddress(URLHandle, {$IFDEF UNICODE}'MIMEAssociationDialogW'{$ELSE}'MIMEAssociationDialogA'{$ENDIF UNICODE});
  end;
  SHDocvwHandle := SafeLoadLibrary('shdocvw.dll');
  if SHDocvwHandle <> 0 then
    @SoftwareUpdateMessageBox := GetProcAddress(SHDocvwHandle, 'SoftwareUpdateMessageBox');
end;

procedure UnloadJvDialogs;
begin
  if ShellHandle > 0 then
    FreeLibrary(ShellHandle);
  if CommHandle > 0 then
    FreeLibrary(CommHandle);
  if AppWizHandle > 0 then
    FreeLibrary(AppWizHandle);
  if URLHandle > 0 then
    FreeLibrary(URLHandle);
  if SHDocvwHandle > 0 then
    FreeLibrary(SHDocvwHandle);
  ShellHandle := 0;
  CommHandle := 0;
  AppWizHandle := 0;
  URLHandle := 0;
  SHDocvwHandle := 0;
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

      Drive   = The 0 based (A: == 0) Drive number of the Drive
                to format.

      fmtID   = Currently must be set to SHFMT_ID_DEFAULT.

      Options = There are currently only two option bits defined.

                   SHFMT_OPT_FULL
                   SHFMT_OPT_SYSONLY

                SHFMT_OPT_FULL specifies that the "Quick Format"
                setting should be cleared by default. If the user
                leaves the "Quick Format" setting cleared, then a
                full format will be applied (this is useful for
                users that detect "unformatted" disks and want
                to bring up the format dialog box).

                If Options is set to zero (0), then the "Quick Format"
                setting is set by default. In addition, if the user leaves
                it set, a quick format is performed. Under Windows NT 4.0,
                this flag is ignored and the "Quick Format" box is always
                checked when the dialog box first appears. The user can
                still change it. This is by design.

                The SHFMT_OPT_SYSONLY initializes the dialog to
                default to just sys the disk.

                All other bits are Reserved for future expansion
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
  LPFNORGFAV = function(Wnd: hWnd; Str: LPCSTR): Integer; stdcall;

function ExtractIconFromFile(FileName: string; Index: Integer): HICON;
var
  iNumberOfIcons: Integer;
begin
  Result := 0;
  if FileExists(FileName) then
  begin
    iNumberOfIcons := ExtractIcon(hInstance, PChar(FileName), Cardinal(-1));
    if (Index >= 0) and (Index < iNumberOfIcons) and (iNumberOfIcons > 0) then
      Result := ExtractIcon(hInstance, PChar(FileName), Index);
  end;
end;

//=== { TJvOrganizeFavoritesDialog } =========================================

function TJvOrganizeFavoritesDialog.Execute(ParentWnd: HWND): Boolean;
var
  Path: AnsiString;
  lpfnDoOrganizeFavDlg: LPFNORGFAV;
begin
  Result := False;
  if SHDocvwHandle <> 0 then
  begin
    Path := AnsiString(GetSpecialFolderPath('Favorites', True)) + #0#0;
    lpfnDoOrganizeFavDlg := LPFNORGFAV(GetProcAddress(SHDocvwHandle, 'DoOrganizeFavDlg'));
    if not Assigned(lpfnDoOrganizeFavDlg) then
      raise EWinDialogError.CreateRes(@RsEFunctionNotSupported);
    lpfnDoOrganizeFavDlg(ParentWnd, PAnsiChar(Path));
    Result := True;
  end;
end;

//=== { TJvAppletDialog } ====================================================

const
  CPL_INIT = 1;
  CPL_GETCOUNT = 2;
  CPL_INQUIRE = 3;
  CPL_SELECT = 4;
  CPL_DBLCLK = 5;
  CPL_STOP = 6;
  CPL_EXIT = 7;
  CPL_NEWINQUIRE = 8;

type
  PCPLInfo = ^TCplInfo;
  TCplInfo = record
    idIcon: Integer;
    idName: Integer;
    idInfo: Integer;
    lData: Longint;
  end;

constructor TJvAppletDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppletName := '';
  FAppletIndex := 0;
  FModule := HINSTANCE_ERROR;
  FCount := 0;
  FAppletFunc := nil;
  SetLength(FAppletInfo, 0);
end;

destructor TJvAppletDialog.Destroy;
begin
  Unload;
  inherited Destroy;
end;

procedure TJvAppletDialog.Unload;
var
  I: Integer;
begin
  if (FModule > HINSTANCE_ERROR) and Assigned(FAppletFunc) then
  begin
    FAppletFunc(GetForegroundWindow, CPL_EXIT, AppletIndex, AppletInfo[AppletIndex].lData);
    FreeLibrary(FModule);
  end;
  for I := 0 to Count - 1 do
  begin
    FAppletInfo[I].Icon.Free;
    FAppletInfo[I].Name := '';
    FAppletInfo[I].Info := '';
  end;
  FModule := HINSTANCE_ERROR;
  FCount := 0;
  FAppletFunc := nil;
  SetLength(FAppletInfo, 0);
end;

procedure TJvAppletDialog.Load;
var
  I: Integer;
  AplInfo: TCplInfo;
  Buffer: array [0..1023] of Char;
begin
  Unload;
  if AppletName <> '' then
  begin
    FModule := SafeLoadLibrary(AppletName);
    if FModule <> 0 then
    begin
      FAppletFunc := TCplApplet(GetProcAddress(FModule, 'CPlApplet'));
      if Assigned(FAppletFunc) and (FAppletFunc(GetForegroundWindow, CPL_INIT, 0, 0) <> 0) then
      begin
        FCount := FAppletFunc(GetForegroundWindow, CPL_GETCOUNT, 0, 0);
        SetLength(FAppletInfo, FCount);
        for I := 0 to Count - 1 do
        begin
          FAppletFunc(GetForegroundWindow, CPL_INQUIRE, I, LPARAM(@AplInfo));
          with FAppletInfo[I] do
          begin
            Icon := TIcon.Create;
            Icon.Handle := LoadIcon(FModule, MakeIntResource(AplInfo.idIcon));
            LoadString(FModule, AplInfo.idName, Buffer, SizeOf(Buffer));
            Name := Buffer;
            LoadString(FModule, AplInfo.idInfo, Buffer, SizeOf(Buffer));
            Info := Buffer;
          end;
        end;
      end
      else
      begin
        FreeLibrary(FModule);
        FModule := HINSTANCE_ERROR;
      end;
    end;
  end;
  if AppletIndex >= Count then
    AppletIndex := 0;
end;

function TJvAppletDialog.GetAppletInfo(Index: Integer): TJvCplInfo;
begin
  FillChar(Result, SizeOf(Result), #0);
  if (Index >= 0) and (Index < Count) then
    Result := FAppletInfo[Index];
end;

procedure TJvAppletDialog.SetAppletName(const AAppletName: string);
begin
  Unload;
  FAppletName := AAppletName;
  Load;
end;

function TJvAppletDialog.Execute(ParentWnd: HWND): Boolean;
begin
  Result := ValidApplet;
  if Result then
    FAppletFunc(ParentWnd, CPL_DBLCLK, AppletIndex, AppletInfo[AppletIndex].lData)
  else
    ShellExecute(ParentWnd, 'open', 'Control.exe', nil, nil, SW_SHOWDEFAULT);
end;

function TJvAppletDialog.ValidApplet: Boolean;
begin
  Result := Assigned(FAppletFunc) and (AppletIndex >= 0) and (AppletIndex < Count);
end;



//=== { TJvComputerNameDialog } ==============================================

constructor TJvComputerNameDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComputerName := '';
end;

function TJvComputerNameDialog.Execute(ParentWnd: HWND): Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  NameBuffer: array [0..MAX_PATH] of Char;
  WindowList: Pointer;
begin
  Result := False;

  if Failed(SHGetSpecialFolderLocation(ParentWnd, CSIDL_NETWORK,
    ItemIDList)) then
    Exit;

  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := ParentWnd;
  BrowseInfo.pidlRoot := ItemIDList;
  BrowseInfo.pszDisplayName := NameBuffer;
  BrowseInfo.lpszTitle := PChar(FCaption);
  BrowseInfo.ulFlags := BIF_BROWSEFORCOMPUTER;
  WindowList := DisableTaskWindows(0);
  try
    Result := SHBrowseForFolder(BrowseInfo) <> nil;
  finally
    EnableTaskWindows(WindowList);
    FreePIDL(BrowseInfo.pidlRoot);
  end;
  if Result then
    FComputerName := NameBuffer;
end;

//=== { TJvBrowseFolderDialog } ==============================================

constructor TJvBrowseFolderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFolderName := '';
end;

function TJvBrowseFolderDialog.Execute(ParentWnd: HWND): Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  ItemSelected: PItemIDList;
  NameBuffer: array [0..MAX_PATH] of Char;
  WindowList: Pointer;
begin
  ItemIDList := nil;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := ParentWnd;
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
  FreePIDL(BrowseInfo.pidlRoot);
end;

//=== { TJvFormatDialog } ====================================================

constructor TJvFormatDriveDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrive := 'A';
end;

function TJvFormatDriveDialog.Execute(ParentWnd: HWND): Boolean;
var
  iDrive, iCapacity, iFormatType, RetVal: Integer;
begin
  iDrive := Ord(FDrive) - Ord('A');
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    iCapacity := 0; // other styles not supported
    if FFormatType = ftQuick then
      iFormatType := 1
    else
      iFormatType := 0;
  end
  else
  begin
    case FCapacity of
      dcSize360kB:
        iCapacity := 3;
      dcSize720kB:
        iCapacity := 5;
    else
      iCapacity := 0;
    end;
    iFormatType := Ord(FFormatType);
  end;

  RetVal := SHFormatDrive(ParentWnd, iDrive, iCapacity, iFormatType);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RetVal = 0
  else
    Result := RetVal = 6;
  if not Result then
    DoError(RetVal);
end;

procedure TJvFormatDriveDialog.DoError(ErrValue: Integer);
var
  Err: TJvFormatDriveError;
begin
  if Assigned(FOnError) then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      Err := errOther
    else
      case ErrValue of
        0:
          Err := errParams;
        -1:
          Err := errSysError;
        -2:
          Err := errAborted;
        -3:
          Err := errCannotFormat;
      else
        Err := errOther;
      end;
    FOnError(Self, Err);
  end;
end;

procedure TJvFormatDriveDialog.SetDrive(Value: Char);
begin
  // (rom) secured
  Value := UpCase(Value);
  if CharInSet(Value, ['A'..'Z']) then
    FDrive := Value;
end;

function GetSpecialFolderPath(const FolderName: string; CanCreate: Boolean): string;
var
  Folder: Integer;
  Found: Boolean;
  I: Integer;
  PIDL: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
begin
  Found := False;
  Folder := 0;
  Result := '';
  for I := Low(SpecialFolders) to High(SpecialFolders) do
  begin
    if SameFileName(FolderName, SpecialFolders[I].Name) then
    begin
      Folder := SpecialFolders[I].ID;
      Found := True;
      Break;
    end;
  end;
  if not Found then
    Exit;
  { Get path of selected location }
  {JPR}
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, PIDL)) then
  begin
    if SHGetPathFromIDList(PIDL, Buf) then
      Result := Buf;
    CoTaskMemFree(PIDL);
  end;
  {JPR}
end;

procedure AddToRecentDocs(const FileName: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
end;

procedure ClearRecentDocs;
begin
  SHAddToRecentDocs(SHARD_PATH, nil);
end;

//=== { TJvOutOfMemoryDialog } ===============================================

function TJvOutOfMemoryDialog.Execute(ParentWnd: HWND): Boolean;
var
  CaptionBuffer: Pointer;
begin
  CaptionBuffer := nil;
  if FCaption <> '' then
    GetMem(CaptionBuffer, (Length(FCaption) + 1) * SizeOf(WideChar));

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if CaptionBuffer <> nil then
      StringToWideChar(FCaption, PWideChar(CaptionBuffer), Length(FCaption) + 1);
  end
  else
  begin
    if CaptionBuffer <> nil then
      StrPCopy(PChar(CaptionBuffer), FCaption);
  end;
  if Assigned(SHOutOfMemoryMessageBox) then
    Result := Boolean(SHOutOfMemoryMessageBox(ParentWnd, CaptionBuffer,
      MB_OK or MB_ICONHAND))
  else
    raise EWinDialogError.CreateRes(@RsENotSupported);
end;

//=== { TJvShellAboutDialog } ================================================

constructor TJvShellAboutDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

function TJvShellAboutDialog.Execute(ParentWnd: HWND): Boolean;
const
  AboutText = 'JvDialogs 2.0';
  CaptionSeparator = '#';
var
  CaptionText: string;
begin
  if Caption = '' then
    CaptionText := AboutText
  else
    CaptionText := Caption;

  CaptionText := CaptionText + CaptionSeparator + Product;

  OSCheck(LongBool(ShellAbout(ParentWnd,
    PChar(CaptionText), PChar(OtherText), FIcon.Handle)));
  Result := True;
end;

//=== { TJvRunDialog } =======================================================

constructor TJvRunDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := '';
  FDescription := '';
  FIcon := TIcon.Create;
end;

destructor TJvRunDialog.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

function TJvRunDialog.Execute(ParentWnd: HWND): Boolean;
var
  CaptionBuffer: Pointer;
  DescriptionBuffer: Pointer;
begin
  CaptionBuffer := nil;
  DescriptionBuffer := nil;

  if FCaption <> '' then
    GetMem(CaptionBuffer, (Length(FCaption) + 1) * SizeOf(WideChar));

  if FDescription <> '' then
    GetMem(DescriptionBuffer, (Length(FDescription) + 1) * SizeOf(WideChar));

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if CaptionBuffer <> nil then
      StringToWideChar(FCaption, PWideChar(CaptionBuffer), Length(FCaption) + 1);
    if DescriptionBuffer <> nil then
      StringToWideChar(FDescription, PWideChar(DescriptionBuffer),
        Length(FDescription) + 1);
  end
  else
  begin
    if CaptionBuffer <> nil then
      StrPCopy(PChar(CaptionBuffer), FCaption);
    if DescriptionBuffer <> nil then
      StrPCopy(PChar(DescriptionBuffer), FDescription);
  end;

  if Assigned(SHRunDialog) then
    Result := SHRunDialog(ParentWnd, FIcon.Handle, nil, CaptionBuffer,
      DescriptionBuffer, 0) = 0
  else
    raise EWinDialogError.CreateRes(@RsENotSupported);
end;

procedure TJvRunDialog.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
end;

//=== { TJvObjectPropertiesDialog } ==========================================

function TJvObjectPropertiesDialog.Execute(ParentWnd: HWND): Boolean;
var
  ObjectNameBuffer: Pointer;
  TabNameBuffer: Pointer;
begin
  GetMem(ObjectNameBuffer, (Length(ObjectName) + 1) * SizeOf(WideChar));
  try
    if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      StringToWideChar(ObjectName, PWideChar(ObjectNameBuffer),
        Length(ObjectName) + 1);
    end
    else
    begin
      StrPCopy(PChar(ObjectNameBuffer), ObjectName);
    end;

    GetMem(TabNameBuffer, (Length(InitialTab) + 1) * SizeOf(WideChar));
    try
      if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
      begin
        StringToWideChar(InitialTab, PWideChar(TabNameBuffer),
          Length(InitialTab) + 1);
      end
      else
      begin
        StrPCopy(PChar(TabNameBuffer), InitialTab);
      end;
      Result := SHObjectProperties(ParentWnd,
        ShellObjectTypeEnumToConst(ObjectType), ObjectNameBuffer,
        TabNameBuffer);
    finally
      FreeMem(TabNameBuffer);
    end;
  finally
    FreeMem(ObjectNameBuffer);
  end;
end;

function ShellObjectTypeEnumToConst(ShellObjectType: TShellObjectType): UINT;
begin
  case ShellObjectType of
    sdPathObject:
      Result := OPF_PATHNAME;
    sdPrinterObject:
      Result := OPF_PRINTERNAME;
  else
    Result := 0;
  end;
end;

function ShellObjectTypeConstToEnum(ShellObjectType: UINT): TShellObjectType;
begin
  case ShellObjectType of
    OPF_PATHNAME:
      Result := sdPathObject;
    OPF_PRINTERNAME:
      Result := sdPrinterObject;
  else
    Result := sdPathObject;
  end;
end;

//=== { TJvNewLinkDialog } ===================================================

function TJvNewLinkDialog.Execute(ParentWnd: HWND): Boolean;
begin
  NewLinkHere(0, 0, PChar(DestinationFolder), 0);
  Result := True; // No way to know it worked
end;

//=== { TJvAddHardwareDialog } ===============================================

function TJvAddHardwareDialog.Execute(ParentWnd: HWND): Boolean;
var
  APModule: THandle;
  Applet: TCplApplet;
begin
  Result := False;
  APModule := SafeLoadLibrary('hdwwiz.cpl');
  if APModule <= HINSTANCE_ERROR then
    Exit;
  Applet := TCplApplet(GetProcAddress(APModule, 'CPlApplet'));
  Applet(0, CPL_DBLCLK, 0, 0);
  FreeLibrary(APModule);
  Result := True;
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
  AStr: array [0..MAX_PATH] of Char;
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

procedure SetShellLinkInfo(const LinkFile: WideString;
  const SLI: TShellLinkInfo);
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

function RecycleFile(FileToRecycle: string): Boolean;
var
  OpStruct: TSHFileOpStruct;
  PFromC: PChar;
  ResultVal: Integer;
begin
  if not FileExists(FileToRecycle) then
  begin
    RecycleFile := False;
    Exit;
  end
  else
  begin
    PFromC := PChar(ExpandFileName(FileToRecycle) + #0#0);
    OpStruct.Wnd := 0;
    OpStruct.wFunc := FO_DELETE;
    OpStruct.pFrom := PFromC;
    OpStruct.pTo := nil;
    OpStruct.fFlags := FOF_ALLOWUNDO;
    OpStruct.fAnyOperationsAborted := False;
    OpStruct.hNameMappings := nil;
    ResultVal := ShFileOperation(OpStruct);
    RecycleFile := (ResultVal = 0);
  end;
end;

function CopyFile(FromFile, ToDir: string): Boolean;
var
  F: TSHFileOpStruct;
begin
  F.Wnd := 0;
  F.wFunc := FO_COPY;
  FromFile := FromFile + #0;
  F.pFrom := PChar(FromFile);
  ToDir := ToDir + #0;
  F.pTo := PChar(ToDir);
  F.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
  Result := ShFileOperation(F) = 0;
end;

// (rom) ExecuteApplet function removed

//=== { TJvOpenWithDialog } ==================================================

function TJvOpenWithDialog.Execute(ParentWnd: HWND): Boolean;
begin
  SHOpenWith(ParentWnd, 0, PChar(FileName), SW_SHOW);
  Result := True; // No way to know it worked
end;

//=== { TJvDiskFullDialog } ==================================================

constructor TJvDiskFullDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DriveChar := 'C';
end;

function TJvDiskFullDialog.GetDrive: UINT;
begin
  Result := Ord(FDriveChar) - Ord('A');
end;

function TJvDiskFullDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if not Assigned(SHHandleDiskFull) then
    raise EWinDialogError.CreateRes(@RsENotSupported);
  Result := GetDriveType(PChar(DriveChar + ':\')) = 3;
  if Result then
    SHHandleDiskFull(ParentWnd, GetDrive);
end;

procedure TJvDiskFullDialog.SetDriveChar(Value: Char);
begin
  Value := UpCase(Value);
  if not CharInSet(Value, ['A'..'Z']) then
    raise EWinDialogError.CreateResFmt(@RsEInvalidDriveChar, [Value]);
  FDriveChar := Value;
end;

//=== { TJvExitWindowsDialog } ===============================================

function TJvExitWindowsDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if not Assigned(SHShutDownDialog) and not Assigned(SHShutDownDialog6) then
    raise EWinDialogError.CreateRes(@RsENotSupported);
  if Assigned(SHShutDownDialog6) then
    SHShutDownDialog6(ParentWnd, Integer(Kind)) // Vista or newer
  else
    SHShutDownDialog(ParentWnd);
  Result := True;
end;

//=== { TJvChangeIconDialog } ================================================

function TJvChangeIconDialog.Execute(ParentWnd: HWND): Boolean;
var
  Buf: array [0..MAX_PATH] of Char;
  BufW: array [0..MAX_PATH] of WideChar;
begin
  if Assigned(SHChangeIconW) then
  begin
    StringToWideChar(FileName, BufW, SizeOf(BufW));
    Result := SHChangeIconW(ParentWnd, BufW, SizeOf(BufW), FIconIndex) = 1;
    if Result then
      FileName := BufW;
  end
  else
  if Assigned(SHChangeIcon) then
  begin
    StrPCopy(Buf, FileName);
    Result := SHChangeIcon(ParentWnd, Buf, SizeOf(Buf), FIconIndex) = 1;
    if Result then
      FileName := Buf;
  end
  else
    raise EWinDialogError.CreateRes(@RsENotSupported);
end;

function OpenInterceptor(var DialogData: TOpenFileName): BOOL; stdcall;
var
  DialogDataEx: TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  DialogDataEx.FlagsEx := 0;
  DialogDataEx.lStructSize := SizeOf(TOpenFileNameEx);
  Result := GetOpenFileNameEx(DialogDataEx);
end;

function SaveInterceptor(var DialogData: TOpenFileName): BOOL; stdcall;
var
  DialogDataEx: TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  DialogDataEx.FlagsEx := 0;
  DialogDataEx.lStructSize := SizeOf(TOpenFileNameEx);
  Result := GetSaveFileNameEx(DialogDataEx);
end;



//=== { TJvOpenDialog2000 } ==================================================

function TJvOpenDialog2000.Execute{$IFDEF RTL180_UP}(ParentWnd: HWND){$ENDIF RTL180_UP}: Boolean;
begin
  if CheckWin32Version(5, 0) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Result := DoExecute(@OpenInterceptor)
  else
    Result := inherited Execute{$IFDEF RTL180_UP}(ParentWnd){$ENDIF RTL180_UP};
end;

//=== { TJvSaveDialog2000 } ==================================================

function TJvSaveDialog2000.Execute{$IFDEF RTL180_UP}(ParentWnd: HWND){$ENDIF RTL180_UP}: Boolean;
begin
  if CheckWin32Version(5, 0) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Result := DoExecute(@SaveInterceptor)
  else
    Result := inherited Execute{$IFDEF RTL180_UP}(ParentWnd){$ENDIF RTL180_UP};
end;

//=== { TJvURLAssociationDialog } ============================================

constructor TJvURLAssociationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
  FDefaultProtocol := 'http://'; // the URL property needs a protocol or the function call fails
end;

function TJvURLAssociationDialog.Execute(ParentWnd: HWND): Boolean;
var
  dwFlags: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
  Result := False;
  FReturnValue := S_FALSE;
  FAssociatedApp := '';
  if Pos(':', URL) < 1 then
    FURL := FDefaultProtocol + FURL;
  if Assigned(URLAssociationDialog) then
  begin
    dwFlags := 0;
    FillChar(Buf[0], SizeOf(Buf), 0);
    if uaDefaultName in Options then
      dwFlags := dwFlags or URLASSOCDLG_FL_USE_DEFAULT_NAME;
    if uaRegisterAssoc in Options then
      dwFlags := dwFlags or URLASSOCDLG_FL_REGISTER_ASSOC;
    FReturnValue := URLAssociationDialog(ParentWnd, dwFlags,
      PChar(FileName), PChar(URL), Buf, SizeOf(Buf));
    Result := ReturnValue = S_OK;
    FAssociatedApp := Buf;
  end;
end;

//=== { TJvMIMEAssociationDialog } ===========================================

function TJvMIMEAssociationDialog.Execute(ParentWnd: HWND): Boolean;
var
  dwFlags: Cardinal;
  Buf: array [0..MAX_PATH] of Char;
begin
  Result := False;
  FReturnValue := S_FALSE;
  if Assigned(MIMEAssociationDialog) then
  begin
    FillChar(Buf[0], SizeOf(Buf), 0);
    FAssociatedApp := '';
    if maRegisterAssoc in Options then
      dwFlags := MIMEASSOCDLG_FL_REGISTER_ASSOC
    else
      dwFlags := 0;
    FReturnValue := MIMEAssociationDialog(ParentWnd, dwFlags,
      PChar(FileName), PChar(ContentType), Buf, SizeOf(Buf));
    Result := ReturnValue = 0;
    FAssociatedApp := Buf;
  end;
end;

//=== { TJvSoftwareUpdateDialog } ============================================

constructor TJvSoftwareUpdateDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDistInfo := TJvSoftwareUpdateInfo.Create;
end;

destructor TJvSoftwareUpdateDialog.Destroy;
begin
  FDistInfo.Free;
  inherited Destroy;
end;

function TJvSoftwareUpdateDialog.Execute(ParentWnd: HWND): Boolean;
var
  psdi: TSoftDistInfo;
begin
  Result := False;
  FReturnValue := IDCANCEL;
  if Assigned(SoftwareUpdateMessageBox) then
  begin
    psdi := FDistInfo.SoftDistInfo;
    FReturnValue := SoftwareUpdateMessageBox(ParentWnd, '', 0, psdi);
    Result := ReturnValue = IDYES;
    if ReturnValue <> IDABORT then
      FDistInfo.SoftDistInfo := psdi;
  end;
end;

//=== { TJvSoftwareUpdateInfo } ==============================================

function TJvSoftwareUpdateInfo.GetSoftDistInfo: TSoftDistInfo;
const
  cAdState: array [TJvSoftwareUpdateAdState] of DWORD =
   (SOFTDIST_ADSTATE_NONE, SOFTDIST_ADSTATE_AVAILABLE,
    SOFTDIST_ADSTATE_DOWNLOADED, SOFTDIST_ADSTATE_INSTALLED);
  cFlags: array [TJvSoftwareUpdateFlags] of DWORD =
   (SOFTDIST_FLAG_USAGE_EMAIL, SOFTDIST_FLAG_USAGE_PRECACHE,
    SOFTDIST_FLAG_USAGE_AUTOINSTALL, SOFTDIST_FLAG_DELETE_SUBSCRIPTION);
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  with Result do
  begin
    dwAdState := cAdState[AdState];
    dwFlags := cFlags[Flags];
    // (p3)_ does result from StringToOLEStr need to be freed?
    lpszTitle := StringToOleStr(Title);
    lpszAbstract := StringToOleStr(Description);
    lpszHREF := StringToOleStr(HREF);
    dwInstalledVersionMS := InstalledVersionMS;
    dwInstalledVersionLS := InstalledVersionLS;
    dwUpdateVersionMS := UpdateVersionMS;
    dwUpdateVersionLS := UpdateVersionLS;
    dwAdvertisedVersionMS := AdvertisedVersionMS;
    dwAdvertisedVersionLS := AdvertisedVersionLS;
  end;
end;

procedure TJvSoftwareUpdateInfo.SetSoftDistInfo(const Value: TSoftDistInfo);
begin
  with Value do
  begin
    case dwAdState of
      SOFTDIST_ADSTATE_NONE:
        AdState := asNone;
      SOFTDIST_ADSTATE_AVAILABLE:
        AdState := asAvailable;
      SOFTDIST_ADSTATE_DOWNLOADED:
        AdState := asDownloaded;
      SOFTDIST_ADSTATE_INSTALLED:
        AdState := asInstalled;
    end;
    case dwFlags of
      SOFTDIST_FLAG_USAGE_EMAIL:
        Flags := ufEmail;
      SOFTDIST_FLAG_USAGE_PRECACHE:
        Flags := ufPreCache;
      SOFTDIST_FLAG_USAGE_AUTOINSTALL:
        Flags := ufAutoInstall;
      SOFTDIST_FLAG_DELETE_SUBSCRIPTION:
        Flags := ufDeleteSubscription;
    end;

    Title := lpszTitle;
    Description := lpszAbstract;
    HREF := lpszHREF;
    InstalledVersionMS := dwInstalledVersionMS;
    InstalledVersionLS := dwInstalledVersionLS;
    UpdateVersionMS := dwUpdateVersionMS;
    UpdateVersionLS := dwUpdateVersionLS;
    AdvertisedVersionMS := dwAdvertisedVersionMS;
    AdvertisedVersionLS := dwAdvertisedVersionLS;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  LoadJvDialogs;

finalization
  UnloadJvDialogs;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
