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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvBrowseFolder;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ShellApi, Shlobj, Activex,
  JvBaseDlg, JvFunctions;

type
  TFromDirectory = (fdRootFolder, fdRecycleBin, fdControlPanel, fdDesktop,
    fdDesktopDirectory, fdMyComputer, fdFonts, fdNetHood, fdNetWork, fdPersonal,
    fdPrinters, fdPrograms, fdRecent, fdSendTo, fdStartMenu, fdStartup, fdTemplates);
  TJvFolderPos = (fpDefault, fpScreenCenter, fpFormCenter, fpTopLeft,
    fpTopRight, fpBottomLeft, fpBottomRight);
  TJvDirChange = procedure(Sender: TObject; Directory: string) of object;
  TOptionsDirectory = (odBrowseForComputer, odOnlyDirectory, odOnlyPrinters, odNoBelowDomain,
    odSystemAncestorsOnly, odFileSystemDirectoryOnly, odStatusAvailable, odIncludeFiles,
    odIncludeUrls, odEditBox, odNewDialogStyle, odShareable);
  TOptionsDir = set of TOptionsDirectory;

  TJvBrowseFolder = class(TJvCommonDialog)
  private
    FDialogHandle: Integer;
    FTitle: string;
    FOptions: TOptionsDir;
    FDisplayName: string;
    FFromDirectory: TFromDirectory;
    FDirectory: string;
    FDummy: string;
    FOnInit: TNotifyEvent;
    FPosition: TJvFolderPos;
    FOnChange: TJvDirChange;
    FPidl: TItemIDList;
  protected
    function getParentHandle:THandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetStatusText(Value: string);
    procedure SetOkEnabled(Value: Boolean);
    property LastPidl: TItemIDList read FPidl write FPidl;

    function Execute: Boolean; override;
  published
    property Directory: string read FDirectory write FDummy;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Options: TOptionsDir read FOptions write FOptions default
      [odStatusAvailable, odNewDialogStyle];
    property Position: TJvFolderPos read FPosition write FPosition default fpDefault;
    property RootDirectory: TFromDirectory read FFromDirectory write FFromDirectory;
    property Title: string read FTitle write FTitle;

    property OnInitialized: TNotifyEvent read FOnInit write FOnInit;
    property OnChange: TJvDirChange read FOnChange write FOnChange;
  end;

implementation

const
  BIF_BROWSEINCLUDEURLS = $0080;
  BIF_BROWSEINCLUDEFILES = $4000;
  BIF_SHAREABLE = $8000;
  BIF_RETURNFSANCESTORS = $0008;
  BIF_EDITBOX = $0010;
  BIF_VALIDATE = $0020;
  BIF_NEWDIALOGSTYLE = $0040;

  {**************************************************}

constructor TJvBrowseFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [odStatusAvailable, odNewDialogStyle];
end;

{**************************************************}

function lpfnBrowseProc(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
var
  r, sr: TRect;
  st: string;
  FHandle:THandle;
begin
  Result := 0;
  with TObject(lpData) as TJvBrowseFolder do
  begin
    FHandle := getParentHandle;
    case uMsg of
      BFFM_INITIALIZED:
        begin
          //Change the position of the dialog
          FDialogHandle := Wnd;
          if GetClientRect(Wnd, r) then
          begin
            SystemParametersInfo(SPI_GETWORKAREA, 0, @sr, 0);
            case FPosition of
              fpScreenCenter:
                begin
                  r.Left := ((sr.Right - sr.Left - (r.Right - r.Left)) div 2);
                  r.Top := (sr.Bottom - sr.Top - (r.Bottom - r.Top)) div 2;
                end;
              fpFormCenter:
                begin
                  GetWindowRect(FHandle, sr);
                  r.Left := sr.Left + ((sr.Right - sr.Left - (r.Right - r.Left)) div 2);
                  r.Top := sr.Top + (sr.Bottom - sr.Top - (r.Bottom - r.Top)) div 2;
                end;
              fpTopLeft:
                begin
                  r.Left := sr.Left;
                  r.Top := sr.Top;
                end;
              fpTopRight:
                begin
                  r.Top := sr.Top;
                  r.Left := sr.Right - (r.Right - r.Left) - GetSystemMetrics(SM_CXFIXEDFRAME);
                end;
              fpBottomLeft:
                begin
                  r.Top := sr.Bottom - (r.Bottom - r.Top) - GetSystemMetrics(SM_CYCAPTION) -
                    -GetSystemMetrics(SM_CYFIXEDFRAME);
                  r.Left := sr.Left;
                end;
              fpBottomRight:
                begin
                  r.Top := sr.Bottom - (r.Bottom - r.Top) - GetSystemMetrics(SM_CYCAPTION) -
                    GetSystemMetrics(SM_CYFIXEDFRAME);
                  r.Left := sr.Right - (r.Right - r.Left) - GetSystemMetrics(SM_CXFIXEDFRAME);
                end;
            end;
            SetWindowPos(Wnd, 0, r.Left, r.Top, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
          end;

          //Change directory (if possible)
          if FDirectory <> '' then
          begin
            st := FDirectory;
            SendMessage(FDialogHandle, BFFM_SETSELECTION, Integer(True), Integer(st));
          end;
          //Call init event
          if Assigned(FOnInit) then
            FOnInit(TObject(lpData) as TJvBrowseFolder);
        end;
      BFFM_SELCHANGED:
        begin
          st := '';
          try
            if Pointer(lParam) <> nil then
            begin
              SetLength(st, MAX_PATH);
              if SHGetPathFromIDList(PItemIDList(lParam), PChar(st)) then
                SetLength(st, StrLen(PChar(st)));
            end;
          except
          end;
          if Assigned(FOnChange) then
            FOnChange(TObject(lpData) as TJvBrowseFolder, st);
        end;
    end;
  end;
end;

{**************************************************}

procedure TJvBrowseFolder.SetOkEnabled(Value: Boolean);
begin
  if FDialogHandle <> 0 then
    SendMessage(FDialogHandle, BFFM_ENABLEOK, 0, LPARAM(Value));
end;

{**************************************************}

procedure TJvBrowseFolder.SetStatusText(Value: string);
begin
  if FDialogHandle <> 0 then
    SendMessage(FDialogHandle, BFFM_SETSTATUSTEXT, 0, LPARAM(PChar(Value)));
end;

{**************************************************}

function TJvBrowseFolder.Execute: Boolean;
const
  CSIDLLocations: array[TFromDirectory] of Cardinal =
  (0, CSIDL_BITBUCKET, CSIDL_CONTROLS, CSIDL_DESKTOP,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_FONTS, CSIDL_NETHOOD, CSIDL_NETWORK, CSIDL_PERSONAL,
    CSIDL_PRINTERS, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU, CSIDL_STARTUP, CSIDL_TEMPLATES);
var
  path: array[0..MAX_PATH] of Char;
  dspName: array[0..MAX_PATH] of Char;
  BrowseInfo: TBrowseInfo;
  pidl: PItemIDList;
  ShellVersion: Integer;
begin
  FDialogHandle := 0;
  Result := False;

  ShellVersion := GetShellVersion;
  if ShellVersion < $00040000 then
    raise Exception.Create('Shell not compatible with BrowseForFolder');

  ZeroMemory(@BrowseInfo, SizeOf(BrowseInfo));

  if odBrowseForComputer in FOptions then
    BrowseInfo.ulFlags := BIF_BROWSEFORCOMPUTER;
  if odOnlyPrinters in FOptions then
    BrowseInfo.ulFlags := BIF_BROWSEFORPRINTER;
  if odNoBelowDomain in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_DONTGOBELOWDOMAIN;
  if odSystemAncestorsOnly in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_RETURNFSANCESTORS;
  if odFileSystemDirectoryOnly in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_RETURNONLYFSDIRS;
  if odStatusAvailable in FOptions then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_STATUSTEXT;
  if (odIncludeFiles in FOptions) and (ShellVersion >= $00040071) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_BROWSEINCLUDEFILES;
  if (odIncludeUrls in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_BROWSEINCLUDEURLS;
  if (odEditBox in FOptions) and (GetShellVersion >= $00040071) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_EDITBOX;
  if (odNewDialogStyle in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_NEWDIALOGSTYLE;
  if (odShareable in FOptions) and (ShellVersion >= $00050000) then
    BrowseInfo.ulFlags := BrowseInfo.ulFlags + BIF_SHAREABLE;

  BrowseInfo.hwndOwner := getParentHandle;
  if CSIDLLocations[FFromDirectory] <> 0 then
    SHGetSpecialFolderLocation(getParentHandle, CSIDLLocations[FFromDirectory], BrowseInfo.pidlRoot);

  BrowseInfo.pszDisplayName := dspName;
  if FTitle = '' then
    BrowseInfo.lpszTitle := nil
  else
    BrowseInfo.lpszTitle := PChar(FTitle);
  BrowseInfo.lpfn := @lpfnBrowseProc;
  BrowseInfo.lParam := LongInt(Self);
  BrowseInfo.iImage := 0;

  try
    CoInitialize(nil);
    pidl := SHBrowseForFolder(BrowseInfo);
    if pidl <> nil then
    begin
      try
        FPidl := pidl^;
        if SHGetPathFromIDList(pidl, path) then
        begin
          if path <> nil then
            FDirectory := StrPas(path);
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
  FDialogHandle := 0;
end;

function TJvBrowseFolder.getParentHandle: THandle;
var F:TCustomForm;
begin
  F := GetParentForm(TControl(Owner));
  if F <> nil then
    Result := F.Handle
  else if (Screen <> nil) and (Screen.ActiveCustomForm <> nil) then
    Result := Screen.ActiveCustomForm.Handle
  else
    Result := GetFocus;
end;

end.
