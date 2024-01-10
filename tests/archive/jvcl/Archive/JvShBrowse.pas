{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShBrowse.PAS, released on 2002-05-26.

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

{ A wrapper for the SHBrowseForFolder API call. }

unit JvShBrowse;

interface

uses
  Windows, Messages, Classes, Forms, Dialogs, SysUtils, ActiveX, Shlobj, Controls,
  Graphics, ShellAPI, JvComponent;

const
  // For finding a folder to start document searching
  BIF_RETURNONLYFSDIRS = $0001;
  // For starting the Find Computer
  BIF_DONTGOBELOWDOMAIN = $0002;
  // Top of the dialog has 2 lines of text for BROWSEINFO.lpszTitle and one line if
  // this flag is set.  Passing the message BFFM_SETSTATUSTEXTA to the hwnd can set the
  // rest of the text.  This is not used with BIF_USENEWUI and BROWSEINFO.lpszTitle gets
  // all three lines of text.
  BIF_STATUSTEXT = $0004;
  BIF_RETURNFSANCESTORS = $0008;
  // Add an editbox to the dialog
  BIF_EDITBOX = $0010;
  // insist on valid result (or CANCEL)
  BIF_VALIDATE = $0020;

  // Use the new dialog layout with the ability to resize
  // Caller needs to call OleInitialize() before using this API
  BIF_NEWDIALOGSTYLE = $0040;
  BIF_USENEWUI = (BIF_NEWDIALOGSTYLE or BIF_EDITBOX);
  // Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)
  BIF_BROWSEINCLUDEURLS = $0080;
  // Add a UA hint to the dialog, in place of the edit box. May not be combined with BIF_EDITBOX
  BIF_UAHINT = $0100;
  // Do not add the "New Folder" button to the dialog.  Only applicable with BIF_NEWDIALOGSTYLE.
  BIF_NONEWFOLDERBUTTON = $0200;
  // don't traverse target as shortcut
  BIF_NOTRANSLATETARGETS = $0400;
  // Browsing for Computers.
  BIF_BROWSEFORCOMPUTER = $1000;
  // Browsing for Printers
  BIF_BROWSEFORPRINTER = $2000;
  // Browsing for Everything
  BIF_BROWSEINCLUDEFILES = $4000;
  // sharable resources displayed (remote shares, requires BIF_USENEWUI)
  BIF_SHAREABLE = $8000;

type
  TJvBrowseFlag = (bfFileSysDirsOnly, bfDontGoBelowDomain, bfStatusText, bfFileSysAncestors,
    bfEditBox, bfValidate, bfNewDialogStyle, bfBrowseIncludeURLs,
    bfUAHint, bfNoNewFolderButton, bfNoTranslateTargets, bfBrowseForComputer, bfBrowseForPrinter,
    bfIncludeFiles, bfShareable);
  TJvBrowseFlags = set of TJvBrowseFlag;

  TJvBrowseSelChanged = procedure(Sender: TObject; const NewFolder: string; var Accept: boolean) of object;
  TJvSpecialFolder = (
    sfDesktop, sfDesktopExpanded, sfInternet, sfPrograms, sfControlPanel, sfPrinters, sfPersonal, sfFavorites,
    sfStartUp, sfRecent, sfSendTo, sfBitBucket, stStartMenu, sfDesktopDirectory,
    sfDrives, sfNetwork, sfNethood, sfFonts, sfTemplates, sfCommonStartMenu, sfCommonPrograms, sfCommonStartUp,
    sfCommonDesktopDirectory, sfAppData, sfPrintHood, sfAltStartUp, sfCommonAltStartup,
    sfCommonFavorites, sfInternetCache, sfCookies, sfHistory);

const
  cShellFolder: array[TJvSpecialFolder] of uInt = (CSIDL_DESKTOP, 0, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS,
    CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET,
    CSIDL_STARTMENU, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD, CSIDL_FONTS,
    CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD, CSIDL_ALTSTARTUP,
    CSIDL_COMMON_ALTSTARTUP, CSIDL_COMMON_FAVORITES, CSIDL_INTERNET_CACHE, CSIDL_COOKIES, CSIDL_HISTORY);
type
  {TJvShellBrowser}
  TJvShellBrowser = class(TJvComponent)
  private
    FHandle: THandle;
    FTitle: string;
    FImageIndex: Integer;
    FDirectory: string;
    FOptions: TJvBrowseFlags;
    FRootFolder: TJvSpecialFolder;
    FOnShow: TNotifyEvent;
    FOnSelectionChanged: TJvBrowseSelChanged;
    FStatusText: string;
    FPosition: TPoint;
    FCaption: string;
    function GetOwnerHandle: THandle;
    procedure PlaceWindow(hWnd: THandle);
    procedure SetPosition(const Value: TPoint);
    procedure SetStatusText(const hWnd: THandle; const Text: string);
    procedure SetPath(const hWnd: THandle; const Path: string);
    procedure EnableOK(const hWnd: THandle; const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    { Displays the Browse For Folder dialog and returns true if the user clicked the OK button to close the dialog. }
    function Execute: Boolean;
    { Returns the imageindex in the system imagelist for the selected folder }
    property ImageIndex: Integer read FImageIndex;
    { Contains the handle to the dialog component. This is only valid while the dialog is
        showing on the screen }
    property Handle: THandle read FHandle;
    { Sets the top / left position of the dialog. If Position.X = -1 and Position.Y = -1, the
        dialog is centered on the screen.  X and Y are relative to the screen. }
    property Position: TPoint read FPosition write SetPosition;
  published
    { Set this before calling Execute to specify the starting folder. When returning successfully
        from the @link(Execute) call, this property contains the name of the selected directory. }
    property Directory: string read FDirectory write FDirectory;
    { Sets the caption of the dialog. If left empty, the default caption is used
        ("Browse for Folder") }
    property Caption: string read FCaption write FCaption;
    { Set this to dislay a title on the dialog as a label just below the caption }
    property Title: string read FTitle write FTitle;
    { The options available for the dialog. Not all options are available on all versions of Windows. }
    property Options: TJvBrowseFlags read FOptions write FOptions default [bfFileSysDirsOnly];
    { if bfStatusText in @link(Options) is true and this string is empty, the status text displays the path to
        the currently selected folder, otherwise the text in StatusText is displayed. }
    property StatusText: string read FStatusText write FStatusText;
    { Set this to the folder that should be set as the browse root }
    property RootFolder: TJvSpecialFolder read FRootFolder write FRootFolder default sfDrives;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    { Called when the user changes the selection in the dialog. NewFolder contains the name of the
        newly selected directory, set Accept to false to disable the OK button. From this event, you can
        change the @link(StatusText), @link(Caption) and @link(Title) properties and the changes will
        be reflected in the dialog. You can also change the bfStatusText flag in @link(Options) from here }
    property OnSelectionChanged: TJvBrowseSelChanged read FOnSelectionChanged write FOnSelectionChanged;
  end;

  { Utility function that creates the TJvShellBrowser class with default options and displays it. Returns true if
    the user clicked the OK button. }
function BrowseForFolder(const ACaption: string; var ADirectory: string): boolean;

implementation
uses
  JvComponentFunctions;

function BrowseForFolder(const ACaption: string; var ADirectory: string): boolean;
begin
  with TJvShellBrowser.Create(nil) do
  try
    Directory := ADirectory;
    Caption := ACaption;
    Result := Execute;
    if Result then
      ADirectory := Directory;
  finally
    Free;
  end;
end;

procedure BrowserCallbackProc(hWnd: THandle; uMsg: Integer; lParam: LPARAM; lpData: LPARAM); stdcall;
var
  FBrowser: TJvShellBrowser; // absolute lpData; //(p3) use of absolute deprecated
  Path: string;
  Accept: Boolean;
  FBuff: array[0..MAX_PATH] of Char;
begin
  FBrowser := TJvShellBrowser(lpData);
  with FBrowser do
    case uMsg of
      BFFM_INITIALIZED:
        begin
          FHandle := hWnd;
          PlaceWindow(hWnd);

          if FCaption <> '' then
            SetWindowText(hWnd, PChar(FCaption));
          if FDirectory <> '' then
            SetPath(hWnd, FDirectory);
          Path := FDirectory;
          if Path = '' then
            Path := StatusText;
          SetStatusText(hWnd, Path);
          if Assigned(FOnShow) then
            FOnShow(FBrowser);
        end;
      BFFM_SELCHANGED:
        begin
          Accept := true;

          SHGetPathFromIDList(PItemIDList(lParam), FBuff);

          Path := StrPas(FBuff);
          if Assigned(FOnSelectionChanged) then
            FOnSelectionChanged(FBrowser, Path, Accept);
          if StatusText <> '' then
            Path := StatusText;
          SetStatusText(hWnd, Path);
          EnableOK(hWnd, Accept);
        end;
    end;
end;

{ TJvShellBrowser }

constructor TJvShellBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FDirectory, MAX_PATH);
  FRootFolder := sfDrives;
  FOptions := [bfFileSysDirsOnly];
  FDirectory := '';
  FPosition := Point(-1, -1); // center by default
end;

procedure TJvShellBrowser.SetStatusText(const hWnd: THandle; const Text: string);
const
  cStatusLabel = $3743;
var R, R2: TRect; S: string; Hnd: THandle;
begin
  if (bfStatusText in FOptions) then
  begin
    if StatusText <> '' then
      S := StatusText
    else
      S := Text;
    hnd := GetDlgItem(hWnd, cStatusLabel);
    if (hnd <> 0) then
    begin
      if StatusText = '' then
      begin
        GetWindowRect(hwnd, R);
        GetWindowRect(hnd, R2);
        S := MinimizeName(S, Application.MainForm.Canvas, (R.Right - R.Left) - (R2.Left - R.Left) * 2 - 8);
      end;
      SendMessage(hWnd, BFFM_SETSTATUSTEXT, 0, integer(PChar(S)));
    end;
  end;
end;

procedure TJvShellBrowser.SetPath(const hWnd: THandle; const Path: string);
begin
  SendMessage(hWnd, BFFM_SETSELECTION, Ord(TRUE), integer(PChar(Path)));
end;

procedure TJvShellBrowser.EnableOK(const hWnd: THandle; const Value: Boolean);
begin
  SendMessage(hWnd, BFFM_ENABLEOK, 0, Ord(Value));
end;

procedure TJvShellBrowser.PlaceWindow(hWnd: THandle);
var
  R: TRect;
begin
  if (FPosition.X = -1) and (FPosition.Y = -1) then
  begin
    GetWindowRect(hWnd, R);
    SetWindowPos(hWnd, 0,
      (Screen.Width - R.Right + R.Left) div 2,
      (Screen.Height - R.Bottom + R.Top) div 2,
      0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
  end
  else
    SetWindowPos(HWnd, 0, FPosition.X, FPosition.Y, 0, 0,
      SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

function TJvShellBrowser.Execute: Boolean;
const
  cFlags: array[TJvBrowseFlag] of DWORD =
  (BIF_RETURNONLYFSDIRS,
    BIF_DONTGOBELOWDOMAIN,
    BIF_STATUSTEXT,
    BIF_RETURNFSANCESTORS,
    BIF_EDITBOX,
    BIF_VALIDATE,
    BIF_NEWDIALOGSTYLE,
    BIF_BROWSEINCLUDEURLS,
    BIF_UAHINT,
    BIF_NONEWFOLDERBUTTON,
    BIF_NOTRANSLATETARGETS,
    BIF_BROWSEFORCOMPUTER,
    BIF_BROWSEFORPRINTER,
    BIF_BROWSEINCLUDEFILES,
    BIF_SHAREABLE);
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  i: TJvBrowseFlag;
  FBuff: array[0..MAX_PATH] of Char;
begin
  ItemIDList := nil;
  try
    FillChar(BrowseInfo, sizeof(TBrowseInfo), 30);
    with BrowseInfo do
    begin
      hwndOwner := GetOwnerHandle;
      pszDisplayName := FBuff;
      lpszTitle := PChar(FTitle);

      if FRootFolder = sfDesktopExpanded then
        pidlRoot := nil
      else
        SHGetSpecialFolderLocation(Application.Handle, cShellFolder[FRootFolder], pidlRoot);
      ulFlags := 0;
      for i := Low(TJvBrowseFlag) to High(TJvBrowseFlag) do
        if TJvBrowseFlag(i) in FOptions then
          ulFlags := ulFlags or cFlags[i];

      lpfn := @BrowserCallbackProc;
      lParam := Longint(Self);
      iImage := 0;
    end;

    ItemIDList := SHBrowseForFolder(BrowseInfo);
    Result := ItemIDList <> nil;
    if Result then
    begin
      FDirectory := FBuff;
      SHGetPathFromIDList(ItemIDList, FBuff);
      FDirectory := StrPas(FBuff);
      FImageIndex := BrowseInfo.iImage;
    end;

  finally
    CoTaskMemFree(ItemIDList);
    CoTaskMemFree(BrowseInfo.pidlRoot);
  end;
end;

procedure TJvShellBrowser.SetPosition(const Value: TPoint);
begin
  FPosition := Value;
end;

function TJvShellBrowser.GetOwnerHandle: THandle;
begin
  if Assigned(Owner) and (Owner is TWinControl) then
    Result := (Owner as TWinControl).Handle
  else if (Screen <> nil) and (Screen.ActiveCustomForm <> nil) then
    Result := Screen.ActiveCustomForm.Handle
  else
    Result := GetFocus;
end;

end.

