{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBrowseFolder.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com]
  Roman Kovbasiouk [roko att users dott sourceforge dott net]
  Remko Bonte [remkobonte att myrealbox dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvBrowseFolder;

interface

{$IFDEF BCB6}
{$HPPEMIT '#include <shtypes.h>'}
{$ELSE}
// BCB5 doesn't have the shtypes.h file, so we have to cope with it
(*$HPPEMIT 'namespace shlobj_h'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '#include <shlobj.h>'*)
(*$HPPEMIT '}'*)
(*$HPPEMIT 'using namespace shlobj_h;'*)
(*$HPPEMIT '#define _ITEMIDLIST shlobj_h::_ITEMIDLIST'*)
{$ENDIF BCB6}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ShellAPI, ShlObj, ActiveX,
  JvBaseDlg, JvJCLUtils;

const
  { Interfaces from ShObjIdl.h }
  IID_IFolderFilterSite: TGUID = (
    D1: $C0A651F5; D2: $B48B; D3: $11D2; D4: ($B5, $ED, $00, $60, $97, $C6, $86, $F6));
  SID_IFolderFilterSite = '{C0A651F5-B48B-11d2-B5ED-006097C686F6}';

type
  IFolderFilterSite = interface(IUnknown)
    [SID_IfolderFilterSite]
    function SetFilter(punk: IUnknown): HResult; stdcall;
  end;

const
  IID_IFolderFilter: TGUID = (
    D1: $9CC22886; D2: $DC8E; D3: $11D2; D4: ($B1, $D0, $00, $C0, $4F, $8E, $EB, $3E));
  SID_IFolderFilter = '{9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E}';

type
  IFolderFilter = interface(IUnknown)
    [SID_IFolderFilter]
    function ShouldShow(psf: IShellFolder; pidlFolder, pidlItem: PItemIDList): HResult; stdcall;
    function GetEnumFlags(psf: IShellFolder; pidlFolder: PItemIDList; const phWnd: HWND;
      var pgrfFlags: DWORD): HResult; stdcall;
  end;

type
  { (rb) Stupid name, feel free to change :) }
  TJvBrowsableObjectClass = (
    ocFolders, //SHCONTF_FOLDERS,
    ocNonFolders, //SHCONTF_NONFOLDERS,
    ocIncludeHidden, //SHCONTF_INCLUDEHIDDEN,
    ocInitOnFirstNext, //SHCONTF_INIT_ON_FIRST_NEXT,
    ocNetPrinterSrch, //SHCONTF_NETPRINTERSRCH,
    ocSharable, //SHCONTF_SHAREABLE,
    ocStorage //SHCONTF_STORAGE
    );
  TJvBrowsableObjectClasses = set of TJvBrowsableObjectClass;

  TJvBrowseAcceptChange = procedure(Sender: TObject; const NewFolder: string;
    var Accept: Boolean) of object;
  TJvShouldShowEvent = procedure(Sender: TObject; const Item: string;
    var DoShow: Boolean) of object;
  TJvGetEnumFlagsEvent = procedure(Sender: TObject; const AFolder: string;
    var Flags: TJvBrowsableObjectClasses) of object;
  TJvDirChange = procedure(Sender: TObject; const Directory: string) of object;
  TJvValidateFailedEvent = procedure(Sender: TObject; const AEditText: string;
    var CanCloseDialog: Boolean) of object;

  TFromDirectory = (
    fdNoSpecialFolder, { 0 }
    fdRootFolder, { 0 }
    fdRecycleBin, { CSIDL_BITBUCKET }
    fdControlPanel, { CSIDL_CONTROLS }
    fdDesktop, { CSIDL_DESKTOP }
    fdDesktopDirectory, { CSIDL_DESKTOPDIRECTORY }
    fdMyComputer, { CSIDL_DRIVES }
    fdFonts, { CSIDL_FONTS }
    fdNetHood, { CSIDL_NETHOOD }
    fdNetwork, { CSIDL_NETWORK }
    fdPersonal, { CSIDL_PERSONAL }
    fdPrinters, { CSIDL_PRINTERS }
    fdPrograms, { CSIDL_PROGRAMS }
    fdRecent, { CSIDL_RECENT }
    fdSendTo, { CSIDL_SENDTO }
    fdStartMenu, { CSIDL_STARTMENU }
    fdStartup, { CSIDL_STARTUP }
    fdTemplates, { CSIDL_TEMPLATES }
    fdStartUpNonLocalized, { CSIDL_ALTSTARTUP }
    fdCommonStartUpNonLocalized, { CSIDL_COMMON_ALTSTARTUP }
    fdCommonDocuments, { CSIDL_COMMON_DOCUMENTS }
    fdCommonFavorites, { CSIDL_COMMON_FAVORITES }
    fdCommonPrograms, { CSIDL_COMMON_PROGRAMS }
    fdCommonStartUp, { CSIDL_COMMON_STARTUP }
    fdCommonTemplates, { CSIDL_COMMON_TEMPLATES }
    fdCookies, { CSIDL_COOKIES }
    fdFavorites, { CSIDL_FAVORITES }
    fdHistory, { CSIDL_HISTORY }
    fdInternet, { CSIDL_INTERNET }
    fdMyMusic, { CSIDL_MYMUSIC }
    fdPrinthood, { CSIDL_PRINTHOOD }
    fdConnections, { CSIDL_CONNECTIONS }

    { Version 4.71 }
    fdAppData, { CSIDL_APPDATA }

    { Version 4.72 }
    fdInternetCache, { CSIDL_INTERNET_CACHE }

    { Version 5.00 }
    fdAdminTools, { CSIDL_ADMINTOOLS }
    fdCommonAdminTools, { CSIDL_COMMON_ADMINTOOLS }
    fdCommonAppData, { CSIDL_COMMON_APPDATA }
    fdLocalAppData, { CSIDL_LOCAL_APPDATA }
    fdMyPictures, { CSIDL_MYPICTURES }
    fdProfile, { CSIDL_PROFILE }
    fdProgramFiles, { CSIDL_PROGRAM_FILES }
    fdProgramFilesCommon, { CSIDL_PROGRAM_FILES_COMMON }
    fdSystem, { CSIDL_SYSTEM }
    fdWindows, { CSIDL_WINDOWS }

    {  Version 6.00 }
    fdCDBurnArea, { CSIDL_CDBURN_AREA }
    fdCommonMusic, { CSIDL_COMMON_MUSIC }
    fdCommonPictures, { CSIDL_COMMON_PICTURES }
    fdCommonVideo, { CSIDL_COMMON_VIDEO }
    fdMyDocuments, { CSIDL_MYDOCUMENTS }
    fdMyVideo, { CSIDL_MYVIDEO }
    fdProfiles, { CSIDL_PROFILES }

    { Unknown version }
    fdResources, { CSIDL_RESOURCES }
    fdResourcesLocalized,
    fdCommonOEMLinks, { CSIDL_COMMON_OEM_LINKS }
    fdComputersNearMe { CSIDL_COMPUTERSNEARME }
    );

  TJvFolderPos = (fpDefault, fpScreenCenter, fpFormCenter, fpTopLeft,
    fpTopRight, fpBottomLeft, fpBottomRight);
  TOptionsDirectory = (odBrowseForComputer, odOnlyDirectory, odOnlyPrinters,
    odNoBelowDomain, odSystemAncestorsOnly, odFileSystemDirectoryOnly,
    odStatusAvailable, odIncludeFiles, odIncludeUrls, odEditBox,
    odNewDialogStyle, odShareable, odUsageHint, odNoNewButtonFolder, odValidate);
  // (p3) shouldn't TOptionsDir be changed to T(Jv)OptionsDirectories?
  TOptionsDir = set of TOptionsDirectory;

  // (rom) changed name
  {$IFDEF COMPILER6_UP}
  TJvBrowseForFolderDialog = class(TJvCommonDialogF, IFolderFilter)
  {$ELSE}
  TJvBrowseForFolderDialog = class(TJvCommonDialogF, IFolderFilter, IUnknown)
  {$ENDIF COMPILER6_UP}
  private
    // (rom) changed names to Window and type to HWND
    { Handle to the owner form of the dialog, used if Position = fpFormCenter }
    FOwnerWindow: HWND;
    { Handle to the MS "Browse for folder" dialog }
    FDialogWindow: HWND;
    FHelpContext: THelpContext;
    FTitle: string;
    FOptions: TOptionsDir;
    FUsedOptions: TOptionsDir;
    FDisplayName: string;
    FRootDirectory: TFromDirectory;
    FRootDirectoryPath: string;
    FDirectory: string;
    FPosition: TJvFolderPos;
    FPidl: TItemIDList;
    FStatusText: string;

    FHelpButtonHandle: THandle;
    FHelpButtonHeightDelta: Integer;

    FOnInit: TNotifyEvent;
    FOnChange: TJvDirChange;
    FOnAcceptChange: TJvBrowseAcceptChange;
    FOnShouldShow: TJvShouldShowEvent;
    FOnGetEnumFlags: TJvGetEnumFlagsEvent;
    FOnValidateFailed: TJvValidateFailedEvent;

    { For hooking the control }
    FDefWndProc: Pointer;
    FObjectInstance: Pointer;
    FPositionSet: Boolean;

    // (p3) updates the status text. NOTE: doesn't work if odNewDialogStyle is true (MS limitation)!!!
    procedure UpdateStatusText(AText: string);
    procedure WMShowWindow(var Msg: TMessage); message WM_SHOWWINDOW;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    function GetRootDirectoryPath: string;
    function IsRootDirectoryPathStored: Boolean;
    procedure SetRootDirectory(const Value: TFromDirectory);
    procedure SetRootDirectoryPath(const Value: string);
    procedure SetOptions(const Value: TOptionsDir);
  protected
    { Messages from the browser }
    procedure DoInitialized;
    procedure DoIUnknown(const Unknown: IUnknown);
    procedure DoSelChanged(IDList: PItemIDList);
    function DoValidateFailed(AEditText: PChar): Integer;
    function DoValidateFailedW(AEditText: PWideChar): Integer;
    function DoShouldShow(const AItem: string): Boolean;
    function DoGetEnumFlags(const AFolder: string; var Flags: TJvBrowsableObjectClasses): Boolean;

    function GetOwnerWindow: HWND;
    procedure MainWndProc(var Msg: TMessage);
    procedure HookDialog;

    { IFolderFilter }
    function ShouldShow(psf: IShellFolder; pidlFolder, pidlItem: PItemIDList): HResult; stdcall;
    function GetEnumFlags(psf: IShellFolder; pidlFolder: PItemIDList; const phWnd: HWND;
      var pgrfFlags: DWORD): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var Msg); override;
    { Messages to the browser }
    procedure SetSelection(const APath: string); overload;
    procedure SetSelection(IDList: PItemIDList); overload;
    procedure SetStatusText(const AText: string);
    procedure SetStatusTextW(const AText: WideString);
    procedure SetOKEnabled(const Value: Boolean);
    procedure SetOKText(const AText: string);
    procedure SetOKTextW(const AText: WideString);
    procedure SetExpanded(const APath: string); overload;
    procedure SetExpandedW(const APath: WideString);
    procedure SetExpanded(IDList: PItemIDList); overload;

    property LastPidl: TItemIDList read FPidl write FPidl;
    property Handle: HWND read FDialogWindow;

    function Execute: Boolean; override;
  published
    property Directory: string read FDirectory write FDirectory;
    property DisplayName: string read FDisplayName write FDisplayName stored False;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Options: TOptionsDir read FOptions write SetOptions default
      [odStatusAvailable, odNewDialogStyle];
    property Position: TJvFolderPos read FPosition write FPosition default fpDefault;
    property RootDirectory: TFromDirectory read FRootDirectory write SetRootDirectory default fdNoSpecialFolder;
    property RootDirectoryPath: string read GetRootDirectoryPath write SetRootDirectoryPath
      stored IsRootDirectoryPathStored;
    property Title: string read FTitle write FTitle;
    property StatusText: string read FStatusText write FStatusText;
    property OnAcceptChange: TJvBrowseAcceptChange read FOnAcceptChange write FOnAcceptChange;
    property OnChange: TJvDirChange read FOnChange write FOnChange;
    property OnGetEnumFlags: TJvGetEnumFlagsEvent read FOnGetEnumFlags write FOnGetEnumFlags;
    property OnInitialized: TNotifyEvent read FOnInit write FOnInit;
    property OnShouldShow: TJvShouldShowEvent read FOnShouldShow write FOnShouldShow;
    property OnValidateFailed: TJvValidateFailedEvent read FOnValidateFailed write FOnValidateFailed;
  end;

function BrowseForFolder(const ATitle: string; AllowCreate: Boolean;
  var ADirectory: string; AHelpContext: THelpContext = 0): Boolean;
function BrowseForComputer(const ATitle: string; AllowCreate: Boolean;
  var ADirectory: string; AHelpContext: THelpContext = 0): Boolean;
// (p3) moved from JvFileUtil, deprecated removed
function BrowseDirectory(var AFolderName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
// (p3) moved from JvFileUtil, deprecated removed
function BrowseComputer(var AComputerName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;   

implementation

uses
  Consts, FileCtrl,
  JvJVCLUtils, JvConsts, JvResources, JvTypes;

type
  TSHGetFolderPathProc = function(hWnd: HWND; csidl: Integer; hToken: THandle;
    dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;

var
  SHGetFolderPathProc: TSHGetFolderPathProc = nil;

const
  { Taken from ShlObj.h & ShObjIdl.h }
  BIF_RETURNFSANCESTORS      = $0008;
  BIF_EDITBOX                = $0010; // Add an editbox to the dialog
  BIF_VALIDATE               = $0020; // insist on valid result (or CANCEL)
  BIF_NEWDIALOGSTYLE         = $0040; // Use the new dialog layout with the ability to resize
                                      // Caller needs to call OleInitialize() before using this API
  BIF_BROWSEINCLUDEURLS      = $0080; // Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)
  BIF_UAHINT                 = $0100; // Add a UA hint to the dialog, in place of the edit box.
                                      // May not be combined with BIF_EDITBOX
  BIF_NONEWFOLDERBUTTON      = $0200; // Do not add the "New Folder" button to the dialog.
                                      // Only applicable with BIF_NEWDIALOGSTYLE.
  BIF_BROWSEINCLUDEFILES     = $4000; // Browsing for Everything
  BIF_SHAREABLE              = $8000; // sharable resources displayed (remote shares, requires BIF_USENEWUI)

  SHCONTF_INIT_ON_FIRST_NEXT = $0100; // allow EnumObject() to return before validating enum
  SHCONTF_NETPRINTERSRCH     = $0200; // hint that client is looking for printers
  SHCONTF_SHAREABLE          = $0400; // hint that client is looking sharable resources (remote shares)
  SHCONTF_STORAGE            = $0800; // include all items with accessible storage and their ancestors

  CSIDL_MYDOCUMENTS          = $000C; // logical "My Documents" desktop icon
  CSIDL_MYMUSIC              = $000D; // "My Music" folder
  CSIDL_MYVIDEO              = $000E; // "My Videos" folder
  CSIDL_LOCAL_APPDATA        = $001C; // <user name>\Local Settings\Applicaiton Data (non roaming)
  CSIDL_COMMON_APPDATA       = $0023; // All Users\Application Data
  CSIDL_WINDOWS              = $0024; // GetWindowsDirectory()
  CSIDL_SYSTEM               = $0025; // GetSystemDirectory()
  CSIDL_PROGRAM_FILES        = $0026; // C:\Program Files
  CSIDL_MYPICTURES           = $0027; // C:\Program Files\My Pictures
  CSIDL_PROFILE              = $0028; // USERPROFILE
  CSIDL_PROGRAM_FILES_COMMON = $002B; // C:\Program Files\Common
  CSIDL_COMMON_TEMPLATES     = $002D; // All Users\Templates
  CSIDL_COMMON_DOCUMENTS     = $002E; // All Users\Documents
  CSIDL_COMMON_ADMINTOOLS    = $002F; // All Users\Start Menu\Programs\Administrative Tools
  CSIDL_ADMINTOOLS           = $0030; // <user name>\Start Menu\Programs\Administrative Tools
  CSIDL_CONNECTIONS          = $0031; // Network and Dial-up Connections
  CSIDL_COMMON_MUSIC         = $0035; // All Users\My Music
  CSIDL_COMMON_PICTURES      = $0036; // All Users\My Pictures
  CSIDL_COMMON_VIDEO         = $0037; // All Users\My Video
  CSIDL_RESOURCES            = $0038; // Resource Direcotry
  CSIDL_RESOURCES_LOCALIZED  = $0039; // Localized Resource Direcotry
  CSIDL_COMMON_OEM_LINKS     = $003A; // Links to All Users OEM specific apps
  CSIDL_CDBURN_AREA          = $003B; // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
  CSIDL_COMPUTERSNEARME      = $003D; // Computers Near Me (computered from Workgroup membership)
  CSIDL_PROFILES             = $003E; // ??

  BFFM_SETOKTEXT             = WM_USER + 105; // Unicode only
  BFFM_SETEXPANDED           = WM_USER + 106; // Unicode only
  BFFM_IUNKNOWN              = 5;             // provides IUnknown to client. lParam: IUnknown*

  { TOptionsDirectory = (odBrowseForComputer, odOnlyDirectory, odOnlyPrinters,
      odNoBelowDomain, odSystemAncestorsOnly, odFileSystemDirectoryOnly,
      odStatusAvailable, odIncludeFiles, odIncludeUrls, odEditBox,
      odNewDialogStyle, odShareable, odUsageHint, odNoNewButtonFolder, odValidate);
  }

  { (rb) No idea why odOnlyDirectory is used? }

  COptionsDirectory: array [TOptionsDirectory] of Cardinal = (
    BIF_BROWSEFORCOMPUTER, 0, BIF_BROWSEFORPRINTER, BIF_DONTGOBELOWDOMAIN,
    BIF_RETURNFSANCESTORS, BIF_RETURNONLYFSDIRS, BIF_STATUSTEXT,
    BIF_BROWSEINCLUDEFILES, BIF_BROWSEINCLUDEURLS, BIF_EDITBOX,
    BIF_NEWDIALOGSTYLE, BIF_SHAREABLE, BIF_UAHINT, BIF_NONEWFOLDERBUTTON,
    BIF_VALIDATE);

  { TJvBrowseObjectClass = (ocFolders, ocNonFolders, ocIncludeHidden,
      ocInitOnFirstNext, ocNetPrinterSrch, ocSharable, ocStorage)
  }

  CBrowseObjectClasses: array [TJvBrowsableObjectClass] of Cardinal = (
    SHCONTF_FOLDERS, SHCONTF_NONFOLDERS, SHCONTF_INCLUDEHIDDEN,
    SHCONTF_INIT_ON_FIRST_NEXT, SHCONTF_NETPRINTERSRCH,
    SHCONTF_SHAREABLE, SHCONTF_STORAGE);

function BrowseForFolder(const ATitle: string; AllowCreate: Boolean;
  var ADirectory: string; AHelpContext: THelpContext): Boolean;
begin
  with TJvBrowseForFolderDialog.Create(nil) do
  try
    Position := fpScreenCenter;
    Directory := ADirectory;
    Title := ATitle;
    HelpContext := AHelpContext;
    if AllowCreate then
      Options := Options + [odNewDialogStyle]
    else
      Options := Options - [odNewDialogStyle];
    Result := Execute;
    if Result then
      ADirectory := Directory;
  finally
    Free;
  end;
end;

function BrowseForComputer(const ATitle: string; AllowCreate: Boolean;
  var ADirectory: string; AHelpContext: THelpContext): Boolean;
begin
  with TJvBrowseForFolderDialog.Create(nil) do
  try
    Position := fpScreenCenter;
    Directory := ADirectory;
    Title := ATitle;
    HelpContext := AHelpContext;
    if AllowCreate then
      Options := Options + [odNewDialogStyle]
    else
      Options := Options - [odNewDialogStyle];
    Options := Options + [odBrowseForComputer];
    RootDirectory := fdNetWork;
    Result := Execute;
    if Result then
      ADirectory := Directory;
  finally
    Free;
  end;
end;

function BrowseDirectory(var AFolderName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
begin
  Result := BrowseForFolder(DlgText, True, AFolderName, AHelpContext);
end;

function BrowseComputer(var AComputerName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
begin
  Result := BrowseForComputer(DlgText, True, AComputerName, AHelpContext);
end;

{ From QDialogs.pas }

function StrRetToString(PIDL: PItemIDList; StrRet: TStrRet): string;
var
  P: PChar;
begin
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;

type
  TFromDirectoryData = record
    CSIDL: Cardinal;
    MinVersion: Cardinal;
    OnlyNT: Boolean;
    CanSimulate: Boolean;
    Alternative: TFromDirectory;
  end;

const
  CSIDLLocations: array [TFromDirectory] of TFromDirectoryData = (
    { fdNoSpecialFolder }
    (CSIDL: 0; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdRootFolder }
    (CSIDL: 0; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdRecycleBin }
    (CSIDL: CSIDL_BITBUCKET; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdControlPanel }
    (CSIDL: CSIDL_CONTROLS; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdDesktop }
    (CSIDL: CSIDL_DESKTOP; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdDesktopDirectory }
    (CSIDL: CSIDL_DESKTOPDIRECTORY; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdMyComputer }
    (CSIDL: CSIDL_DRIVES; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdFonts }
    (CSIDL: CSIDL_FONTS; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdNetHood }
    (CSIDL: CSIDL_NETHOOD; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdNetWork }
    (CSIDL: CSIDL_NETWORK; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdPersonal }
    (CSIDL: CSIDL_PERSONAL; MinVersion: 0; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdPrinters }
    (CSIDL: CSIDL_PRINTERS; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdPrograms }
    (CSIDL: CSIDL_PROGRAMS; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdRecent }
    (CSIDL: CSIDL_RECENT; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdSendTo }
    (CSIDL: CSIDL_SENDTO; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdStartMenu }
    (CSIDL: CSIDL_STARTMENU; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdStartup }
    (CSIDL: CSIDL_STARTUP; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdTemplates }
    (CSIDL: CSIDL_TEMPLATES; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdStartUpNonLocalized }
    (CSIDL: CSIDL_ALTSTARTUP; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonStartUpNonLocalized }
    (CSIDL: CSIDL_COMMON_ALTSTARTUP; MinVersion: 0; OnlyNT: True;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonDocuments }
    (CSIDL: CSIDL_COMMON_DOCUMENTS; MinVersion: 0; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdCommonFavorites }
    (CSIDL: CSIDL_COMMON_FAVORITES; MinVersion: 0; OnlyNT: True;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonPrograms }
    (CSIDL: CSIDL_COMMON_PROGRAMS; MinVersion: 0; OnlyNT: True;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonStartUp }
    (CSIDL: CSIDL_COMMON_STARTUP; MinVersion: 0; OnlyNT: True;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonTemplates }
    (CSIDL: CSIDL_COMMON_TEMPLATES; MinVersion: 0; OnlyNT: True;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCookies }
    (CSIDL: CSIDL_COOKIES; MinVersion: 0; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdFavorites }
    (CSIDL: CSIDL_FAVORITES; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdHistory }
    (CSIDL: CSIDL_HISTORY; MinVersion: 0; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdInternet }
    (CSIDL: CSIDL_INTERNET; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdMyMusic }
    (CSIDL: CSIDL_MYMUSIC; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdPrinthood }
    (CSIDL: CSIDL_PRINTHOOD; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdConnections }
    (CSIDL: CSIDL_CONNECTIONS; MinVersion: 0; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdAppData }
    (CSIDL: CSIDL_APPDATA; MinVersion: $00040071; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdInternetCache }
    (CSIDL: CSIDL_INTERNET_CACHE; MinVersion: $00040072; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdAdminTools }
    (CSIDL: CSIDL_ADMINTOOLS; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdCommonAdminTools }
    (CSIDL: CSIDL_COMMON_ADMINTOOLS; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdCommonAppData }
    (CSIDL: CSIDL_COMMON_APPDATA; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdLocalAppData }
    (CSIDL: CSIDL_LOCAL_APPDATA; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdMyPictures }
    (CSIDL: CSIDL_MYPICTURES; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdPersonal),
    { fdProfile }
    (CSIDL: CSIDL_PROFILE; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdProgramFiles }
    (CSIDL: CSIDL_PROGRAM_FILES; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdProgramFilesCommon }
    (CSIDL: CSIDL_PROGRAM_FILES_COMMON; MinVersion: $00050000; OnlyNT: True;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdSystem }
    (CSIDL: CSIDL_SYSTEM; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdWindows }
    (CSIDL: CSIDL_WINDOWS; MinVersion: $00050000; OnlyNT: False;
    CanSimulate: True; Alternative: fdNoSpecialFolder),
    { fdCDBurnArea }
    (CSIDL: CSIDL_CDBURN_AREA; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonMusic }
    (CSIDL: CSIDL_COMMON_MUSIC; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdCommonDocuments),
    { fdCommonPictures }
    (CSIDL: CSIDL_COMMON_PICTURES; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdCommonDocuments),
    { fdCommonVideo }
    (CSIDL: CSIDL_COMMON_VIDEO; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdCommonDocuments),
    { fdMyDocuments }
    (CSIDL: CSIDL_MYDOCUMENTS; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdPersonal),
    { fdMyVideo }
    (CSIDL: CSIDL_MYVIDEO; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdPersonal),
    { fdProfiles }
    (CSIDL: CSIDL_PROFILES; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdResources }
    (CSIDL: CSIDL_RESOURCES; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdResourcesLocalized }
    (CSIDL: CSIDL_RESOURCES_LOCALIZED; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdCommonOEMLinks }
    (CSIDL: CSIDL_COMMON_OEM_LINKS; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder),
    { fdComputersNearMe }
    (CSIDL: CSIDL_COMPUTERSNEARME; MinVersion: $00060000; OnlyNT: False;
    CanSimulate: False; Alternative: fdNoSpecialFolder)
    );

procedure InitSHFolder;
const
  SHFolderDll = 'SHFolder.dll';
var
  SHFolderHandle: HMODULE;
begin
  { You never know, maybe someone does not have SHFolder.dll, thus load on request }
  SHFolderHandle := GetModuleHandle(SHFolderDll);
  if SHFolderHandle <> 0 then
    @SHGetFolderPathProc := GetProcAddress(SHFolderHandle, 'SHGetFolderPathA');
end;

procedure GetCSIDLLocation(const ASpecialDirectory: TFromDirectory;
  var CSIDL: Cardinal; var APath: string);
{ This function is a bit overkill }
var
  LSpecialDirectory: TFromDirectory;

  function IsOk: Boolean;
  begin
    with CSIDLLocations[LSpecialDirectory] do
      Result := (MinVersion <= GetShellVersion) and
        (not OnlyNT or (Win32Platform = VER_PLATFORM_WIN32_NT));
  end;

var
  Buffer: PChar;
begin
  LSpecialDirectory := ASpecialDirectory;
  while (LSpecialDirectory <> fdNoSpecialFolder) and not CSIDLLocations[LSpecialDirectory].CanSimulate
    and not IsOk do
    LSpecialDirectory := CSIDLLocations[LSpecialDirectory].Alternative;

  if (LSpecialDirectory = fdNoSpecialFolder) or IsOk then
  begin
    CSIDL := CSIDLLocations[LSpecialDirectory].CSIDL;
    Exit;
  end;

  CSIDL := 0;
  GetMem(Buffer, MAX_PATH);
  try
    if not Assigned(SHGetFolderPathProc) then
      InitSHFolder;
    if Assigned(SHGetFolderPathProc) and
      Succeeded(SHGetFolderPathProc(0, CSIDLLocations[LSpecialDirectory].CSIDL, 0, 0, Buffer)) then

      APath := Buffer
    else
      APath := '';
  finally
    FreeMem(Buffer);
  end;
end;

function CreateIDListFromPath(const APath: string): PItemIDList;
var
  WS: WideString;
  Eaten, Flags: LongWord;
  IDesktopFolder: IShellFolder;
begin
  { Returned value must be freed }

  Result := nil;

  if APath = '' then
    Exit;

  WS := APath;
  { MSDN : Since Flags is an in/out parameter, it should always be initialized }
  Flags := 0;

  if Failed(SHGetDesktopFolder(IDesktopFolder)) or
    Failed(IDesktopFolder.ParseDisplayName(0, nil, POleStr(WS), Eaten, Result, Flags)) then
    Result := nil;
end;

function CreateIDListFromCSIDL(const ASpecialDirectory: TFromDirectory): PItemIDList;
var
  CSIDL: Cardinal;
  Path: string;
begin
  { Returned value must be freed }

  Result := nil;

  if ASpecialDirectory = fdNoSpecialFolder then
    Exit;

  GetCSIDLLocation(ASpecialDirectory, CSIDL, Path);

  if CSIDL <> 0 then
  begin
    { MSDN: The calling application is responsible for freeing this pointer }
    { SHGetSpecialFolderLocation is shell v4.7 or later}
    if Failed(SHGetSpecialFolderLocation(0, CSIDL, Result)) then
      Result := nil;
  end
  else
    Result := CreateIDListFromPath(Path);
end;

function IDListToPath(IDList: PItemIDList): string;
var
  IDesktopFolder: IShellFolder;
  StrRet: TStrRet;
begin
  { Similar to SHGetPathFromIDList }
  if Succeeded(SHGetDesktopFolder(IDesktopFolder)) and
    Succeeded(IDesktopFolder.GetDisplayNameOf(IDList, SHGDN_NORMAL or SHGDN_FORPARSING, StrRet)) then

    { Result may be a GUID; Don't know whether these GUIDs are portable. Microsoft
      does recommend to return strings 'that are as close to the display names
      as possible'. But in this case display names aren't usable }
    Result := StrRetToString(IDList, StrRet)
  else
    Result := '';

  (* These GUID's seem pretty portable, you can enter them at RootDirectoryPath
     or Directory, ie the "::{GUID}" part (only tested on Windows XP).

    ::{00020D75-0000-0000-C000-000000000046} - Inbox
    ::{20D04FE0-3AEA-1069-A2D8-08002B30309D} - CSIDL_DRIVES
    ::{208D2C60-3AEA-1069-A2D7-08002B30309D} - CSIDL_NETWORK, CSIDL_NETHOOD
    ::{21EC2020-3AEA-1069-A2DD-08002B30309D} - CSIDL_CONTROLS
    ::{2227A280-3AEA-1069-A2DE-08002B30309D} - CSIDL_PRINTERS, CSIDL_PRINTHOOD
    ::{450D8FBA-AD25-11D0-98A8-0800361B1103} - CSIDL_PERSONAL
    ::{645FF040-5081-101B-9F08-00AA002F954E} - CSIDL_BITBUCKET
    ::{7007ACC7-3202-11D1-AAD2-00805FC1270E} - CSIDL_CONNECTIONS
    ::{871C5380-42A0-1069-A2EA-08002B30309D} - CSIDL_INTERNET
    ::{D6277990-4C6A-11CF-8D87-00AA0060F5BF} - Scheduled Tasks
  *)
end;

function CSIDLToPath(const ASpecialDirectory: TFromDirectory): string;
var
  CSIDL: Cardinal;
  IDList: PItemIDList;
  ShellMalloc: IMalloc;
begin
  if ASpecialDirectory = fdNoSpecialFolder then
  begin
    Result := '';
    Exit;
  end;

  GetCSIDLLocation(ASpecialDirectory, CSIDL, Result);

  if CSIDL = 0 then
    Exit;

  { SHGetSpecialFolderLocation is shell v4.7 or later}
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, IDList)) then
  try
    Result := IDListToPath(IDList);
  finally
    if Succeeded(SHGetMalloc(ShellMalloc)) then
      ShellMalloc.Free(IDList);
  end
  else
    Result := '';
end;

procedure SetDialogPos(AParentHandle, AWndHandle: THandle;
  Position: TJvFolderPos);
var
  R, SR: TRect;
begin
  if GetClientRect(AWndHandle, R) then
  begin
    //R.Right := R.Left + AWidth;
    //R.Bottom := R.Top + AHeight;
    SystemParametersInfo(SPI_GETWORKAREA, 0, @SR, 0);
    case Position of
      fpScreenCenter:
        begin
          R.Left := ((SR.Right - SR.Left - (R.Right - R.Left)) div 2);
          R.Top := (SR.Bottom - SR.Top - (R.Bottom - R.Top)) div 2;
        end;
      fpFormCenter:
        begin
          GetWindowRect(AParentHandle, SR);
          R.Left := SR.Left + ((SR.Right - SR.Left - (R.Right - R.Left)) div 2);
          R.Top := SR.Top + (SR.Bottom - SR.Top - (R.Bottom - R.Top)) div 2;
        end;
      fpTopLeft:
        begin
          R.Left := SR.Left;
          R.Top := SR.Top;
        end;
      fpTopRight:
        begin
          R.Top := SR.Top;
          R.Left := SR.Right - (R.Right - R.Left) -
            GetSystemMetrics(SM_CXFIXEDFRAME);
        end;
      fpBottomLeft:
        begin
          R.Top := SR.Bottom - (R.Bottom - R.Top) -
            GetSystemMetrics(SM_CYCAPTION) -
            -GetSystemMetrics(SM_CYFIXEDFRAME);
          R.Left := SR.Left;
        end;
      fpBottomRight:
        begin
          R.Top := SR.Bottom - (R.Bottom - R.Top) -
            GetSystemMetrics(SM_CYCAPTION) -
            GetSystemMetrics(SM_CYFIXEDFRAME);
          R.Left := SR.Right - (R.Right - R.Left) -
            GetSystemMetrics(SM_CXFIXEDFRAME);
        end;
      fpDefault:
        Exit;
    end;
    SetWindowPos(AWndHandle, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE
      or SWP_NOZORDER);
  end;
end;

function lpfnBrowseProc(Wnd: HWND; uMsg: UINT; lParam, lpData: LParam): Integer; stdcall;
begin
  Result := 0;

  with TJvBrowseForFolderDialog(lpData) do
  begin
    FDialogWindow := Wnd;
    case uMsg of
      BFFM_INITIALIZED:
        DoInitialized;
      BFFM_SELCHANGED:
        DoSelChanged(PItemIDList(lParam));
      BFFM_IUNKNOWN:
        DoIUnknown(IUnknown(lParam));
      BFFM_VALIDATEFAILEDA:
        Result := DoValidateFailed(PChar(lParam));
      BFFM_VALIDATEFAILEDW:
        Result := DoValidateFailedW(PWideChar(lParam));
    end;
  end;
end;

{ TJvBrowseForFolderDialog }

constructor TJvBrowseForFolderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [odStatusAvailable, odNewDialogStyle];
  FPosition := fpDefault;
  FRootDirectory := fdNoSpecialFolder;
  FObjectInstance := JvMakeObjectInstance(MainWndProc);
end;

destructor TJvBrowseForFolderDialog.Destroy;
begin
  JvFreeObjectInstance(FObjectInstance);
  inherited Destroy;
end;

procedure TJvBrowseForFolderDialog.DefaultHandler(var Msg);
begin
  if FDialogWindow <> 0 then
    with TMessage(Msg) do
      Result := CallWindowProc(FDefWndProc, FDialogWindow, Msg, WParam, LParam)
  else
    inherited DefaultHandler(Msg);
end;

function TJvBrowseForFolderDialog.DoGetEnumFlags(const AFolder: string;
  var Flags: TJvBrowsableObjectClasses): Boolean;
begin
  { (rb) Always return True? }
  Result := True;
  if Assigned(FOnGetEnumFlags) then
    FOnGetEnumFlags(Self, AFolder, Flags);
end;

procedure TJvBrowseForFolderDialog.DoInitialized;
const
  SBtn = 'BUTTON';
  HelpButtonId = $FFFF;
var
  BtnHandle, BtnFont: THandle;
  BtnSize, WindowSize: TRect;
begin
  { We can now change the position of the dialog - if it's not NewDialogStyle.. }
  FPositionSet := not (odNewDialogStyle in FUsedOptions);
  if FPositionSet then
    SetDialogPos(FOwnerWindow, FDialogWindow, Position);

  { ..Otherwise we have to delay the change until receive of WM_SHOWWINDOW,
    thus we need to hook the dialog; we also need to hook the dialog if there
    is a new help button on the dialog and the dialog is resizeable - ie
    NewDialogStyle }
  if not FPositionSet or ((FHelpContext <> 0) and (odNewDialogStyle in FUsedOptions)) then
    HookDialog;

  // [roko] Rx's code to insert Help button
  if FHelpContext <> 0 then
  begin
    { SomeBtnHandle is some button on the window; we need it to determine a
      useable height & width for the new help button }
    BtnHandle := FindWindowEx(FDialogWindow, 0, SBtn, nil);
    if BtnHandle <> 0 then
    begin
      GetWindowRect(BtnHandle, BtnSize);
      GetWindowRect(FDialogWindow, WindowSize);
      ScreenToClient(FDialogWindow, BtnSize.TopLeft);
      ScreenToClient(FDialogWindow, BtnSize.BottomRight);
      BtnFont := SendMessage(FDialogWindow, WM_GETFONT, 0, 0);
      { Note: BtnSize.Top = "Window.Height" - FHelpButtonHeightDelta, used in
              WM_SIZE }
      FHelpButtonHeightDelta := WindowSize.Bottom - WindowSize.Top - BtnSize.Top;
      { Remember the new buttons handle, because we need it, when the dialog
        is resized }
      FHelpButtonHandle := CreateWindow(SBtn, PChar(SHelpButton),
        WS_CHILD or WS_CLIPSIBLINGS or WS_VISIBLE or BS_PUSHBUTTON or WS_TABSTOP,
        12, BtnSize.Top, BtnSize.Right - BtnSize.Left, BtnSize.Bottom - BtnSize.Top,
        FDialogWindow, HelpButtonId, HInstance, nil);
      if BtnFont <> 0 then
        SendMessage(FHelpButtonHandle, WM_SETFONT, BtnFont, MakeLParam(1, 0));
      UpdateWindow(FDialogWindow);
    end;
  end;

  { Change directory (if possible) }
  if FDirectory <> '' then
    SetSelection(FDirectory);
  UpdateStatusText(FDirectory);

  if Assigned(FOnInit) then
    FOnInit(Self);
end;

procedure TJvBrowseForFolderDialog.DoIUnknown(const Unknown: IUnknown);
var
  FolderFilterSite: IFolderFilterSite;
begin
  if (Assigned(FOnGetEnumFlags) or Assigned(FOnShouldShow)) and
    Supports(Unknown, IID_IFolderFilterSite, FolderFilterSite) then
  begin
    FolderFilterSite.SetFilter(Self);
    FolderFilterSite := nil;
  end;
end;

procedure TJvBrowseForFolderDialog.DoSelChanged(IDList: PItemIDList);
var
  // (p3) use buff array instead of string as this works better
  Buffer: array [0..MAX_PATH] of Char;
  Path: string;
  Accept: Boolean;
begin
  { Note :
    * If the location specified by the pidl parameter is not part of the file
      system, this function will fail.
    * If the pidl parameter specifies a shortcut, the pszPath will contain the
      path to the shortcut, not to the shortcut's target. (if not win XP )

    Could also use IDListToPath
  }

  if SHGetPathFromIDList(IDList, Buffer) then
    Path := Buffer
  else
    Path := '';

  if Assigned(FOnAcceptChange) then
  begin
    Accept := True;
    FOnAcceptChange(Self, Path, Accept);
    SetOKEnabled(Accept);
  end;

  UpdateStatusText(Path);

  if Assigned(FOnChange) then
    FOnChange(Self, Path);
end;

function TJvBrowseForFolderDialog.DoShouldShow(
  const AItem: string): Boolean;
begin
  if Assigned(FOnShouldShow) then
    FOnShouldShow(Self, AItem, Result)
  else
    Result := True;
end;

function TJvBrowseForFolderDialog.DoValidateFailed(
  AEditText: PChar): Integer;
var
  CanClose: Boolean;
begin
  { Return zero to allow the dialog to be dismissed or nonzero to keep
    the dialog displayed. }
  if Assigned(FOnValidateFailed) then
  begin
    CanClose := True;
    FOnValidateFailed(Self, AEditText, CanClose);
    Result := Integer(not CanClose);
  end
  else
    Result := 0; // = Integer(False)
end;

function TJvBrowseForFolderDialog.DoValidateFailedW(
  AEditText: PWideChar): Integer;
begin
  { Explicit conversion }
  Result := DoValidateFailed(PChar(string(AEditText)));
end;

function TJvBrowseForFolderDialog.Execute: Boolean;
var
  dspName: array [0..MAX_PATH] of Char;
  BrowseInfo: TBrowseInfo;
  pidl: PItemIDList;
  ShellVersion: Cardinal;
  ActiveWindow: HWND;
  WindowList: Pointer;
  Option: TOptionsDirectory;
begin
  ShellVersion := GetShellVersion;
  if ShellVersion < $00040000 then
    raise EJVCLException.CreateRes(@RsEShellNotCompatible);

  FDialogWindow := 0;
  FOwnerWindow := GetOwnerWindow;
  FPositionSet := False;
  FHelpButtonHandle := 0;
  FHelpButtonHeightDelta := 0;

  Result := False;

  FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);

  { FUsedOptions is a subset of FOptions; the options that actually can be
    used because of shell version limitations }
  FUsedOptions := FOptions;
  if ShellVersion < $00060000 then
    FUsedOptions := FUsedOptions - [odNoNewButtonFolder, odUsageHint];
  if ShellVersion < $00050000 then
    FUsedOptions := FUsedOptions - [odIncludeUrls, odNewDialogStyle, odShareable];
  if ShellVersion < $00040071 then
    FUsedOptions := FUsedOptions - [odIncludeFiles, odEditBox, odValidate];

  for Option := Low(TOptionsDirectory) to High(TOptionsDirectory) do
    if Option in FUsedOptions then
      Inc(BrowseInfo.ulFlags, COptionsDirectory[Option]);

  BrowseInfo.hwndOwner := FOwnerWindow;
  BrowseInfo.pszDisplayName := dspName;
  BrowseInfo.lpfn := TFNBFFCallBack(@lpfnBrowseProc);
  BrowseInfo.lParam := Longint(Self);

  if (FStatusText = '') or not (odNewDialogStyle in FUsedOptions) then
    BrowseInfo.lpszTitle := Pointer(FTitle)
  else
  if FTitle = '' then
    BrowseInfo.lpszTitle := PChar(FStatusText)
  else
    BrowseInfo.lpszTitle := PChar(FTitle + Cr + FStatusText);

  if FRootDirectory = fdNoSpecialFolder then
    BrowseInfo.pidlRoot := CreateIDListFromPath(FRootDirectoryPath)
  else
    BrowseInfo.pidlRoot := CreateIDListFromCSIDL(FRootDirectory);

  try
    if odNewDialogStyle in FUsedOptions then
      CoInitialize(nil);
    try
      ActiveWindow := GetActiveWindow;
      WindowList := DisableTaskWindows(0);
      try
        pidl := SHBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
      end;

      Result := pidl <> nil;
      if Result then
      begin
        { (rb) This does not work; pidl^ has variable length }
        FPidl := pidl^;
        FDisplayName := BrowseInfo.pszDisplayName;
        FDirectory := IDListToPath(pidl);
        CoTaskMemFree(pidl);
      end;

      CoTaskMemFree(BrowseInfo.pidlRoot);
    finally
      FDialogWindow := 0;
      FOwnerWindow := 0;
      if odNewDialogStyle in FUsedOptions then
        CoUninitialize;
    end;
  except
  end;
end;

function TJvBrowseForFolderDialog.GetEnumFlags(psf: IShellFolder;
  pidlFolder: PItemIDList; const phWnd: HWND;
  var pgrfFlags: DWORD): HResult;
var
  Flags: TJvBrowsableObjectClasses;
  Obj: TJvBrowsableObjectClass;
begin
  { (rb) Don't know for sure if pgrfFlags is initialized }
  Flags := [];
  for Obj := Low(TJvBrowsableObjectClass) to High(TJvBrowsableObjectClass) do
    if pgrfFlags and CBrowseObjectClasses[Obj] = CBrowseObjectClasses[Obj] then
      Include(Flags, Obj);

  { This seems not to work ?? : }
  //if psf.GetDisplayNameOf(pidlFolder, SHGDN_NORMAL or SHGDN_FORPARSING, StrRet) <> S_OK then
  //  Exit;

  if DoGetEnumFlags(IDListToPath(pidlFolder), Flags) then
    Result := S_OK
  else
    Result := S_FALSE;

  pgrfFlags := 0;
  for Obj := Low(TJvBrowsableObjectClass) to High(TJvBrowsableObjectClass) do
    if Obj in Flags then
      Inc(pgrfFlags, CBrowseObjectClasses[Obj]);
end;

function TJvBrowseForFolderDialog.GetOwnerWindow: HWND;
var
  F: TCustomForm;
begin
  // (Ralf Kaiser) Owner maybe a TDataModule
  if Owner is TControl then
    F := GetParentForm(TControl(Owner))
  else
    F := nil;
  if F <> nil then
    Result := F.Handle
  else
  if Owner is TWinControl then
    Result := (Owner as TWinControl).Handle
  else
  if (Screen <> nil) and (Screen.ActiveCustomForm <> nil) then
    Result := Screen.ActiveCustomForm.Handle
  else
    Result := GetForegroundWindow;
end;

function TJvBrowseForFolderDialog.GetRootDirectoryPath: string;
begin
  if FRootDirectory = fdNoSpecialFolder then
    Result := FRootDirectoryPath
  else
    Result := CSIDLToPath(FRootDirectory);
end;

procedure TJvBrowseForFolderDialog.HookDialog;
begin
  if FDialogWindow <> 0 then
    FDefWndProc := Pointer(SetWindowLong(FDialogWindow, GWL_WNDPROC,
      Longint(FObjectInstance)));
end;

function TJvBrowseForFolderDialog.IsRootDirectoryPathStored: Boolean;
begin
  Result := (RootDirectory = fdNoSpecialFolder) and (FRootDirectoryPath > '');
end;

procedure TJvBrowseForFolderDialog.MainWndProc(var Msg: TMessage);
begin
  try
    Dispatch(Msg);
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvBrowseForFolderDialog.SetExpanded(const APath: string);
begin
  if FDialogWindow <> 0 then
    { Implicit conversion }
    SetExpandedW(APath);
end;

procedure TJvBrowseForFolderDialog.SetExpanded(IDList: PItemIDList);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETEXPANDED, WParam(False), LParam(IDList));
end;

procedure TJvBrowseForFolderDialog.SetExpandedW(const APath: WideString);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETEXPANDED, WParam(True), LParam(PWideChar(APath)));
end;

procedure TJvBrowseForFolderDialog.SetOKEnabled(const Value: Boolean);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_ENABLEOK, 0, LParam(Value));
end;

procedure TJvBrowseForFolderDialog.SetOKText(const AText: string);
begin
  if FDialogWindow <> 0 then
    { Implicit conversion }
    SetOKTextW(AText);
end;

procedure TJvBrowseForFolderDialog.SetOKTextW(const AText: WideString);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETOKTEXT, 0, LParam(PWideChar(AText)));
end;

procedure TJvBrowseForFolderDialog.SetOptions(const Value: TOptionsDir);
var
  AddedOptions, RemovedOptions: TOptionsDir;
begin
  if FOptions = Value then
    Exit;

  AddedOptions := Value - (FOptions * Value);
  RemovedOptions := FOptions - (FOptions * Value);

  FOptions := Value;

  { Force correct options }
  if odIncludeUrls in AddedOptions then
    FOptions := FOptions + [odEditBox, odNewDialogStyle, odIncludeFiles];
  if odShareable in AddedOptions then
    FOptions := FOptions + [odNewDialogStyle];
  if odUsageHint in AddedOptions then
    FOptions := FOptions + [odNewDialogStyle] - [odEditBox];
  if odValidate in AddedOptions then
    FOptions := FOptions + [odEditBox];
  if odEditBox in AddedOptions then
    FOptions := FOptions - [odUsageHint];

  if odEditBox in RemovedOptions then
    FOptions := FOptions - [odIncludeUrls, odValidate];
  if odNewDialogstyle in RemovedOptions then
    FOptions := FOptions - [odIncludeUrls, odShareable, odUsageHint];
  if odIncludeFiles in RemovedOptions then
    FOptions := FOptions - [odIncludeUrls];

  { Last check }
  if odEditBox in FOptions then
    FOptions := FOptions - [odUsageHint]
  else
    FOptions := FOptions - [odIncludeUrls, odValidate];
  if odUsageHint in FOptions then
    FOptions := FOptions - [odValidate, odEditBox];
end;

procedure TJvBrowseForFolderDialog.SetRootDirectory(
  const Value: TFromDirectory);
begin
  if (Value = fdNoSpecialFolder) and (FRootDirectory <> fdNoSpecialFolder) then
    FRootDirectoryPath := GetRootDirectoryPath;

  FRootDirectory := Value;
end;

procedure TJvBrowseForFolderDialog.SetRootDirectoryPath(
  const Value: string);
begin
  FRootDirectory := fdNoSpecialFolder;
  FRootDirectoryPath := Value;
end;

procedure TJvBrowseForFolderDialog.SetSelection(const APath: string);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETSELECTION, WParam(True), LParam(Pointer(APath)));
end;

procedure TJvBrowseForFolderDialog.SetSelection(IDList: PItemIDList);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETSELECTION, WParam(False), LParam(IDList));
end;

procedure TJvBrowseForFolderDialog.SetStatusText(const AText: string);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETSTATUSTEXT, 0, LParam(Pointer(AText)));
end;

procedure TJvBrowseForFolderDialog.SetStatusTextW(const AText: WideString);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETSTATUSTEXTW, 0, LParam(PWideChar(AText)));
end;

function TJvBrowseForFolderDialog.ShouldShow(psf: IShellFolder; pidlFolder,
  pidlItem: PItemIDList): HResult;
var
  StrRet: TStrRet;
begin
  psf.GetDisplayNameOf(pidlItem, SHGDN_NORMAL or SHGDN_FORPARSING, StrRet);

  if DoShouldShow(StrRetToString(pidlItem, StrRet)) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

procedure TJvBrowseForFolderDialog.UpdateStatusText(AText: string);
const
  cStatusLabel = $3743;
var
  WindowRect, ItemRect: TRect;
  ItemHandle: THandle;
begin
  if [odStatusAvailable, odNewDialogStyle] * FUsedOptions <> [odStatusAvailable] then
    Exit;

  if StatusText <> '' then
    AText := StatusText
  else
  begin
    ItemHandle := GetDlgItem(FDialogWindow, cStatusLabel);

    if ItemHandle <> 0 then
    begin
      GetWindowRect(FDialogWindow, WindowRect);
      GetWindowRect(ItemHandle, ItemRect);
      // (rom) MinimizeName from FileCtrl not JvJCLUtils
      AText := MinimizeName(AText, Application.MainForm.Canvas,
        (WindowRect.Right - WindowRect.Left) - (ItemRect.Left - WindowRect.Left) * 2 - 8);
    end;
  end;

  SetStatusText(AText);
end;

procedure TJvBrowseForFolderDialog.WMShowWindow(var Msg: TMessage);
begin
  { If the dialog isn't resized, we won't get a WM_SIZE message. Thus we
    respond to the WM_SHOWWINDOW message }

  if not FPositionSet then
    SetDialogPos(FOwnerWindow, FDialogWindow, Position);
  FPositionSet := True;

  inherited;
end;

procedure TJvBrowseForFolderDialog.WMSize(var Msg: TWMSize);
var
  BtnSize: TRect;
  WindowSize: TRect;
begin
  inherited;

  if FHelpButtonHandle <> 0 then
  begin
    GetWindowRect(FHelpButtonHandle, BtnSize);
    GetWindowRect(FDialogWindow, WindowSize);
    ScreenToClient(FDialogWindow, BtnSize.TopLeft);
    ScreenToClient(FDialogWindow, BtnSize.BottomRight);

    SetWindowPos(FHelpButtonHandle, 0, BtnSize.Left,
      WindowSize.Bottom - WindowSize.Top - FHelpButtonHeightDelta,
      BtnSize.Right - BtnSize.Left, BtnSize.Bottom - BtnSize.Top,
      SWP_NOZORDER + SWP_NOACTIVATE);
  end;
end;

end.

