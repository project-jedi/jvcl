{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBrowseFolder.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBrowseFolder;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ShellAPI, ShlObj, ActiveX,
  JvBaseDlg, JvFunctions;

type
  TJvBrowseAcceptChange = procedure(Sender: TObject; const NewFolder: string; var
    Accept: Boolean) of object;
  TFromDirectory = (fdRootFolder, fdRecycleBin, fdControlPanel, fdDesktop,
    fdDesktopDirectory, fdMyComputer, fdFonts, fdNetHood, fdNetWork, fdPersonal,
    fdPrinters, fdPrograms, fdRecent, fdSendTo, fdStartMenu, fdStartup,
    fdTemplates);
  TJvFolderPos = (fpDefault, fpScreenCenter, fpFormCenter, fpTopLeft,
    fpTopRight, fpBottomLeft, fpBottomRight);
  TJvDirChange = procedure(Sender: TObject; Directory: string) of object;
  TOptionsDirectory = (odBrowseForComputer, odOnlyDirectory, odOnlyPrinters,
    odNoBelowDomain, odSystemAncestorsOnly, odFileSystemDirectoryOnly,
    odStatusAvailable, odIncludeFiles, odIncludeUrls, odEditBox,
    odNewDialogStyle, odShareable);
  TOptionsDir = set of TOptionsDirectory;

  // (rom) changed name
  TJvBrowseForFolderDialog = class(TJvCommonDialog)
  private
    // (rom) changed names to Window and type to HWND
    { Handle to the owner form of the dialog, used if Position = fpFormCenter }
    FOwnerWindow: HWND;
    { Handle to the MS "Browse for folder" dialog }
    FDialogWindow: HWND;
    FTitle: string;
    FOptions: TOptionsDir;
    FDisplayName: string;
    FFromDirectory: TFromDirectory;
    FDirectory: string;
    FOnInit: TNotifyEvent;
    FPosition: TJvFolderPos;
    FOnChange: TJvDirChange;
    FPidl: TItemIDList;
    FStatusText: string;
    FOnAcceptChange: TJvBrowseAcceptChange;

    { For hooking the control }
    FDefWndProc: Pointer;
    FObjectInstance: Pointer;
    FPositionSet: Boolean;

    { For hooking the control we need some vars (FDefWndProc, FObjectInstance)
      and some methods (MainWndProc, DefaultHandler) that are already defined
      in TCommonDialog. Unfortunately these vars aren't accessible for
      ancestors. Thus I think it's a better idea to derive the common dialogs
      from a new base class, that enables to hook the dialogs, instead of
      deriving it from TCommonDialog. (remko)
    }

    // (p3) updates the status text. NOTE: doesn't work if odNewDialogStyle is true (MS limitation)!!!
    procedure UpdateStatusText(const HWND: THandle; const Text: string);
    procedure SetPath(const HWND: THandle; const Path: string);
    procedure WMShowWindow(var Msg: TMessage); message WM_SHOWWINDOW;
  protected
    function GetOwnerWindow: HWND;
    procedure MainWndProc(var Msg: TMessage);
    procedure HookDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var Msg); override;
    procedure SetStatusText(Value: string);
    procedure SetOkEnabled(Value: Boolean);
    property LastPidl: TItemIDList read FPidl write FPidl;
    property Handle: HWND read FDialogWindow;

    function Execute: Boolean; override;
  published
    property Directory: string read FDirectory write FDirectory;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Options: TOptionsDir read FOptions write FOptions default
      [odStatusAvailable, odNewDialogStyle];
    property Position: TJvFolderPos read FPosition write FPosition default
      fpDefault;
    property RootDirectory: TFromDirectory read FFromDirectory write
      FFromDirectory;
    property Title: string read FTitle write FTitle;
    property StatusText: string read FStatusText write FStatusText;
    property OnAcceptChange: TJvBrowseAcceptChange read FOnAcceptChange write
      FOnAcceptChange;
    property OnInitialized: TNotifyEvent read FOnInit write FOnInit;
    property OnChange: TJvDirChange read FOnChange write FOnChange;
  end;

function BrowseForFolder(const ATitle: string; AllowCreate: Boolean; var ADirectory: string): Boolean;

implementation

uses
  JvTypes;

const
  BIF_BROWSEINCLUDEURLS  = $0080;
  BIF_BROWSEINCLUDEFILES = $4000;
  BIF_SHAREABLE          = $8000;
  BIF_RETURNFSANCESTORS  = $0008;
  BIF_EDITBOX            = $0010;
  BIF_VALIDATE           = $0020;
  BIF_NEWDIALOGSTYLE     = $0040;

function BrowseForFolder(const ATitle: string; AllowCreate: Boolean; var ADirectory: string): Boolean;
begin
  with TJvBrowseForFolderDialog.Create(nil) do
  try
    Position := fpScreenCenter;
    Directory := ADirectory;
    Title := ATitle;
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

constructor TJvBrowseForFolderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [odStatusAvailable, odNewDialogStyle];
  { (rb) delayed until execute }
  //FOwnerWindow := GetOwnerWindow;
  {$IFDEF COMPILER6_UP}
  FObjectInstance := Classes.MakeObjectInstance(MainWndProc);
  {$ELSE}
  FObjectInstance := MakeObjectInstance(MainWndProc);
  {$ENDIF}
end;

destructor TJvBrowseForFolderDialog.Destroy;
begin
  if FObjectInstance <> nil then
    {$IFDEF COMPILER6_UP}
    Classes.FreeObjectInstance(FObjectInstance);
    {$ELSE}
    FreeObjectInstance(FObjectInstance);
    {$ENDIF}
  inherited Destroy;
end;

procedure SetDialogPos(AParentHandle, AWndHandle: THandle;
  Position: TJvFolderPos);
var
  R, sr: TRect;
begin
  if GetClientRect(AWndHandle, R) then
  begin
    //R.Right := R.Left + AWidth;
    //R.Bottom := R.Top + AHeight;
    SystemParametersInfo(SPI_GETWORKAREA, 0, @sr, 0);
    case Position of
      fpScreenCenter:
        begin
          R.Left := ((sr.Right - sr.Left - (R.Right - R.Left)) div 2);
          R.Top := (sr.Bottom - sr.Top - (R.Bottom - R.Top)) div 2;
        end;
      fpFormCenter:
        begin
          GetWindowRect(AParentHandle, sr);
          R.Left := sr.Left + ((sr.Right - sr.Left - (R.Right - R.Left)) div 2);
          R.Top := sr.Top + (sr.Bottom - sr.Top - (R.Bottom - R.Top)) div 2;
        end;
      fpTopLeft:
        begin
          R.Left := sr.Left;
          R.Top := sr.Top;
        end;
      fpTopRight:
        begin
          R.Top := sr.Top;
          R.Left := sr.Right - (R.Right - R.Left) -
            GetSystemMetrics(SM_CXFIXEDFRAME);
        end;
      fpBottomLeft:
        begin
          R.Top := sr.Bottom - (R.Bottom - R.Top) -
            GetSystemMetrics(SM_CYCAPTION) -
            -GetSystemMetrics(SM_CYFIXEDFRAME);
          R.Left := sr.Left;
        end;
      fpBottomRight:
        begin
          R.Top := sr.Bottom - (R.Bottom - R.Top) -
            GetSystemMetrics(SM_CYCAPTION) -
            GetSystemMetrics(SM_CYFIXEDFRAME);
          R.Left := sr.Right - (R.Right - R.Left) -
            GetSystemMetrics(SM_CXFIXEDFRAME);
        end;
    end;
    SetWindowPos(AWndHandle, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE
      or SWP_NOZORDER);
  end;
end;

procedure TJvBrowseForFolderDialog.UpdateStatusText(const HWND: THandle;
  const Text: string);
const
  cStatusLabel = $3743;
var
  R, R2: TRect;
  S: string;
  Hnd: THandle;
begin
  if odStatusAvailable in FOptions then
  begin
    if StatusText <> '' then
      S := StatusText
    else
      S := Text;
    Hnd := GetDlgItem(HWND, cStatusLabel);
    if Hnd <> 0 then
    begin
      if StatusText = '' then
      begin
        GetWindowRect(HWND, R);
        GetWindowRect(Hnd, R2);
        S := MinimizeName(S, Application.MainForm.Canvas, (R.Right - R.Left) -
          (R2.Left - R.Left) * 2 - 8);
      end;
      SendMessage(HWND, BFFM_SETSTATUSTEXT, 0, Integer(PChar(S)));
    end;
  end;
end;

// (p3) incorrectly declared as a function before

procedure lpfnBrowseProc(hWnd: THandle; uMsg: Integer; lParam: LPARAM;
  lpData: LPARAM); stdcall;
var
  Accept: Boolean;
  FBrowser: TJvBrowseForFolderDialog;
  FBuff: array [0..MAX_PATH] of Char;
  APath: string;
begin
  { TODO : Need to react on BFFM_IUNKNOWN (for custom filtering) and
           BFFM_VALIDATEFAILED }

  FBrowser := TJvBrowseForFolderDialog(lpData);
  with FBrowser do
  begin
    FDialogWindow := hWnd;
    case uMsg of
      BFFM_INITIALIZED:
        begin
          // Change the position of the dialog
          FPositionSet := not (odNewDialogStyle in Options);
          if not FPositionSet then
            // Delay the change until receive of WM_SHOWWINDOW
            HookDialog
          else
            SetDialogPos(FOwnerWindow, FDialogWindow, Position);

          //Change directory (if possible)
          if FDirectory <> '' then
            SetPath(FDialogWindow, FDIrectory);
          //            SendMessage(FDialogWindow, BFFM_SETSELECTION, Integer(True), Integer(PChar(FDirectory)));
          UpdateStatusText(HWND, FDirectory);
          //Call init event
          if Assigned(FOnInit) then
            FOnInit(TObject(lpData) as TJvBrowseForFolderDialog);
        end;
      BFFM_SELCHANGED:
        begin
          Accept := True;
          // (p3) use buff array instead of string as this works better
          SHGetPathFromIDList(PItemIDList(lParam), FBuff);
          APath := StrPas(FBuff);
          if Assigned(FOnAcceptChange) then
            FOnAcceptChange(FBrowser, APath, Accept);
          SetOKEnabled(Accept);

          UpdateStatusText(HWND, APath);
          if Assigned(FOnChange) then
            FOnChange(FBrowser, APath);
        end;
    end;
  end;
end;

procedure TJvBrowseForFolderDialog.SetOkEnabled(Value: Boolean);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_ENABLEOK, 0, LPARAM(Value));
end;

procedure TJvBrowseForFolderDialog.SetStatusText(Value: string);
begin
  if FDialogWindow <> 0 then
    SendMessage(FDialogWindow, BFFM_SETSTATUSTEXT, 0, LPARAM(PChar(Value)));
end;

function TJvBrowseForFolderDialog.Execute: Boolean;
const
  CSIDLLocations: array [TFromDirectory] of Cardinal =
    (0, CSIDL_BITBUCKET, CSIDL_CONTROLS, CSIDL_DESKTOP,
     CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_FONTS, CSIDL_NETHOOD,
     CSIDL_NETWORK, CSIDL_PERSONAL,
     CSIDL_PRINTERS, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU,
     CSIDL_STARTUP, CSIDL_TEMPLATES);
var
  Path: array [0..MAX_PATH] of Char;
  dspName: array [0..MAX_PATH] of Char;
  BrowseInfo: TBrowseInfo;
  pidl: PItemIDList;
  ShellVersion: Cardinal;
begin
  ShellVersion := GetShellVersion;
  if ShellVersion < $00040000 then
    raise EJVCLException.Create('Shell not compatible with BrowseForFolder');

  FDialogWindow := 0;
  FOwnerWindow := GetOwnerWindow;
  FPositionSet := False;
  Result := False;

  FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);

  if odBrowseForComputer in FOptions then
    BrowseInfo.ulFlags := BIF_BROWSEFORCOMPUTER;
  if odOnlyPrinters in FOptions then
    BrowseInfo.ulFlags := BIF_BROWSEFORPRINTER;
  if odNoBelowDomain in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_DONTGOBELOWDOMAIN;
  if odSystemAncestorsOnly in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_RETURNFSANCESTORS;
  if odFileSystemDirectoryOnly in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_RETURNONLYFSDIRS;
  if odStatusAvailable in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_STATUSTEXT;
  if (odIncludeFiles in FOptions) and (ShellVersion >= $00040071) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_BROWSEINCLUDEFILES;
  if (odIncludeUrls in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_BROWSEINCLUDEURLS;
  if (odEditBox in FOptions) and (GetShellVersion >= $00040071) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_EDITBOX;
  if (odNewDialogStyle in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_NEWDIALOGSTYLE;
  if (odShareable in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_SHAREABLE;

  BrowseInfo.hwndOwner := FOwnerWindow;
  
  if CSIDLLocations[FFromDirectory] <> 0 then
    SHGetSpecialFolderLocation(Handle, CSIDLLocations[FFromDirectory],
      BrowseInfo.pidlRoot);

  BrowseInfo.pszDisplayName := dspName;
  if FTitle = '' then
    BrowseInfo.lpszTitle := nil
  else
    BrowseInfo.lpszTitle := PChar(FTitle);
  BrowseInfo.lpfn := TFNBFFCallBack(@lpfnBrowseProc);
  BrowseInfo.lParam := Longint(Self);

  try
    CoInitialize(nil);
    pidl := SHBrowseForFolder(BrowseInfo);
    if pidl <> nil then
    begin
      try
        FPidl := pidl^;
        if SHGetPathFromIDList(pidl, Path) then
        begin
          if Path <> nil then
            FDirectory := StrPas(Path);
          Result := True;
        end;
        FDisplayName := BrowseInfo.pszDisplayName;
        CoTaskMemFree(pidl);
        CoTaskMemFree(BrowseInfo.pidlRoot);
      except
      end;
    end;
    CoUninitialize;
  except
  end;
  FDialogWindow := 0;
  FOwnerWindow := 0;
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
    Result := GetFocus;
end;

procedure TJvBrowseForFolderDialog.SetPath(const HWND: THandle; const Path: string);
begin
  SendMessage(HWND, BFFM_SETSELECTION, Ord(True), Integer(PChar(Path)));
end;

procedure TJvBrowseForFolderDialog.MainWndProc(var Msg: TMessage);
begin
  try
    Dispatch(Msg);
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvBrowseForFolderDialog.DefaultHandler(var Msg);
begin
  if FDialogWindow <> 0 then
    with TMessage(Msg) do
      Result := CallWindowProc(FDefWndProc, FDialogWindow, Msg, WParam, LParam)
  else
    inherited DefaultHandler(Msg);
end;

procedure TJvBrowseForFolderDialog.HookDialog;
begin
  if FDialogWindow <> 0 then
    FDefWndProc := Pointer(SetWindowLong(FDialogWindow, GWL_WNDPROC,
      Longint(FObjectInstance)));
end;

procedure TJvBrowseForFolderDialog.WMShowWindow(var Msg: TMessage);
begin
  { If the dialog isn't resized, we won't get a WM_SIZE message. Thus we
    respond to the WM_SHOWWINDOW message

    Maybe good idea to only respond to WM_SHOWWINDOW <g> }

  if not FPositionSet then
    SetDialogPos(FOwnerWindow, FDialogWindow, Position);
  FPositionSet := True;

  inherited;
end;

end.

