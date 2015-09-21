{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: AHCompBrowseFolderDlg.pas, released on 2003-12-07.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit AHCompBrowseFolderDlg;

{$I jvcl.inc}
{$I windowsonly.inc}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Classes, Dialogs,
  ActiveX, ComObj, ShlObj, FileCtrl, JclBase, JclWin32;

type
{ TBrowseFolderDialog }

  TBrowseKind = (bfFolders, bfComputers);
  TDialogPosition = (dpDefault, dpScreenCenter);

  TCustomizeEvent = procedure(Sender: TObject; Handle: HWND) of object;
  TWndProcEvent = procedure(Sender: TObject; var Msg: TMessage; var Handled: Boolean) of object;
  TCommandEvent = procedure(Sender: TObject; var Msg: TWMCommand; var Handled: Boolean) of object;
  TEnableOKBtnEvent = procedure(Sender: TObject; var Enable: Boolean) of object;

  TBrowseFolderDialog = class(TCommonDialog)
  private
    FDefWndProc: Pointer;
    FHelpContext: THelpContext;
    FHandle: HWND;
    FObjectInstance: Pointer;
    FDesktopRoot: Boolean;
    FBrowseKind: TBrowseKind;
    FPosition: TDialogPosition;
    FText: string;
    FDisplayName: string;
    FSelectedName: string;
    FFolderName: string;
    FImageIndex: Integer;
    FOnInitialized: TNotifyEvent;
    FOnSelChanged: TNotifyEvent;
    FOnCustomize: TCustomizeEvent;
    FOnWndProc: TWndProcEvent;
    FOnCommand: TCommandEvent;
    FOnEnableOKBtn: TEnableOKBtnEvent;
    procedure SetSelPath(const Path: string);
    procedure SetOkEnable(Value: Boolean);
    procedure DoInitialized;
    procedure DoSelChanged(Param: PItemIDList);
    procedure DoCustomize;
    function DoWndProc(var Msg: TMessage): Boolean;
    function DoCommand(var Msg: TMessage): Boolean;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMCommand(var Message: TMessage); message WM_COMMAND;
  protected
    function TaskModalDialog2(var Info: TBrowseInfo): PItemIDList;
  public
    procedure DefaultHandler(var Message); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

    function GetOKBtnID: Integer;
    function GetOKBtn: THandle;
    property Handle: HWND read FHandle;
    property DisplayName: string read FDisplayName;
    property SelectedName: string read FSelectedName write FSelectedName;
    property ImageIndex: Integer read FImageIndex;
  published
    property BrowseKind: TBrowseKind read FBrowseKind write FBrowseKind default bfFolders;
    property DesktopRoot: Boolean read FDesktopRoot write FDesktopRoot default True;
    property DialogText: string read FText write FText;
    property FolderName: string read FFolderName write FFolderName;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Position: TDialogPosition read FPosition write FPosition default dpScreenCenter;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
    property OnSelChanged: TNotifyEvent read FOnSelChanged write FOnSelChanged;
    property OnCustomize: TCustomizeEvent read FOnCustomize write FOnCustomize;
    property OnWndProc: TWndProcEvent read FOnWndProc write FOnWndProc;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property OnEnableOKBtn: TEnableOKBtnEvent read FOnEnableOKBtn write FOnEnableOKBtn;
  end;

function BrowseDirectory(var AFolderName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
function BrowseComputer(var ComputerName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;

procedure CenterWindow(wnd: HWND);

implementation

resourcestring
  STR_HELPBTN = '&Help';

procedure CenterWindow(wnd: HWND);
var r: TRect;
begin
  GetWindowRect(wnd, r);
  r := Rect((GetSystemMetrics(SM_CXSCREEN) - r.Right + r.Left) div 2,
            (GetSystemMetrics(SM_CYSCREEN) - r.Bottom + r.Top) div 2,
            r.Right - r.Left, r.Bottom - r.Top);
  SetWindowPos(wnd, 0, r.Left, r.Top, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOZORDER);
end;

function ExplorerHook(wnd: HWnd; Msg: UINT; LParam: LPARAM; Data: LPARAM): Integer; stdcall;
begin
  Result := 0;
  if Msg = BFFM_INITIALIZED then
  begin
    if TBrowseFolderDialog(Data).Position = dpScreenCenter then CenterWindow(wnd);
    TBrowseFolderDialog(Data).FHandle := wnd;
    TBrowseFolderDialog(Data).FDefWndProc := Pointer(SetWindowLongPtr(wnd, GWLP_WNDPROC,
     LONG_PTR(TBrowseFolderDialog(Data).FObjectInstance)));
    TBrowseFolderDialog(Data).DoInitialized;
   end
   else if Msg = BFFM_SELCHANGED then
   begin
     TBrowseFolderDialog(Data).FHandle := wnd;
     TBrowseFolderDialog(Data).DoSelChanged(PItemIDList(LParam));
  end;
end;

const
  HelpButtonId = $FFFF;

function RemoveBackSlash(const s: String): String;
begin
  Result := s;
  if (Result <> '') and (Result[length(Result)] = '\') then
     Delete(Result, length(Result), 1);
end;

function DirExists(const dir: String): Boolean;
var
  Attr: DWord;
begin
  Attr := GetFileAttributes(PChar(dir));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;


// *****************************************************************************
constructor TBrowseFolderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnCustomize := nil;
  FOnWndProc := nil;
  FOnCommand := nil;
  FOnEnableOKBtn := nil;

  FObjectInstance := MakeObjectInstance(WndProc);
  FDesktopRoot := True;
  FBrowseKind := bfFolders;
  FPosition := dpScreenCenter;
  SetLength(FDisplayName, MAX_PATH);
end;

destructor TBrowseFolderDialog.Destroy;
begin
  if FObjectInstance <> nil then FreeObjectInstance(FObjectInstance);
  inherited Destroy;
end;

function TBrowseFolderDialog.GetOKBtnID: Integer;
begin
  Result := GetDlgCtrlID(GetOKBtn);
end;

function TBrowseFolderDialog.GetOKBtn: THandle;
begin
  Result := 0;
  if FHandle = 0 then Exit;
  Result := FindWindowEx(FHandle, 0, 'BUTTON', 'OK');
end;

procedure TBrowseFolderDialog.DoInitialized;
const
  SBtn = 'BUTTON';
var
  BtnHandle, HelpBtn, BtnFont: THandle;
  BtnSize: TRect;
begin
  if (FBrowseKind = bfComputers) or DirExists(FFolderName) then
     SetSelPath(FFolderName);
  if FHelpContext <> 0 then
  begin
    BtnHandle := FindWindowEx(FHandle, 0, SBtn, nil);
    if (BtnHandle <> 0) then
    begin
      GetWindowRect(BtnHandle, BtnSize);
      ScreenToClient(FHandle, BtnSize.TopLeft);
      ScreenToClient(FHandle, BtnSize.BottomRight);
      BtnFont := SendMessage(FHandle, WM_GETFONT, 0, 0);
      HelpBtn := CreateWindow(SBtn, PChar(STR_HELPBTN),
        WS_CHILD or WS_CLIPSIBLINGS or WS_VISIBLE or BS_PUSHBUTTON or WS_TABSTOP,
        12, BtnSize.Top, BtnSize.Right - BtnSize.Left, BtnSize.Bottom - BtnSize.Top,
        FHandle, HelpButtonId, HInstance, nil);
      if BtnFont <> 0 then
        SendMessage(HelpBtn, WM_SETFONT, BtnFont, MakeLParam(1, 0));
      UpdateWindow(FHandle);
    end;
  end;
  if Assigned(FOnInitialized) then FOnInitialized(Self);
  DoCustomize;
end;

procedure TBrowseFolderDialog.DoSelChanged(Param: PItemIDList);
var Temp: Array[0..MAX_PATH] of Char;
begin
  if (FBrowseKind = bfComputers) then
    FSelectedName := DisplayName
  else
  begin
    if SHGetPathFromIDList(Param, Temp) then
    begin
      FSelectedName := StrPas(Temp);
      SetOkEnable(DirExists(FSelectedName));
    end
    else
    begin
      FSelectedName := '';
      SetOkEnable(False);
    end;
  end;
  if Assigned(FOnSelChanged) then FOnSelChanged(Self);
end;

procedure TBrowseFolderDialog.DoCustomize;
begin
  if Assigned(FOnCustomize) then FOnCustomize(Self, Handle);
end;
function TBrowseFolderDialog.DoWndProc(var Msg: TMessage): Boolean;
begin
  Result := False;
  if Assigned(FOnWndProc) then FOnWndProc(Self, Msg, Result);
end;
function TBrowseFolderDialog.DoCommand(var Msg: TMessage): Boolean;
begin
  Result := False;
  if Assigned(FOnCommand) then FOnCommand(Self, TWMCommand(Msg), Result);
end;

procedure TBrowseFolderDialog.SetSelPath(const Path: string);
begin
  if FHandle <> 0 then
    SendMessage(FHandle, BFFM_SETSELECTION, 1, Longint(PChar(Path)));
end;

procedure TBrowseFolderDialog.SetOkEnable(Value: Boolean);
begin
  if Assigned(FOnEnableOKBtn) then FOnEnableOKBtn(Self, Value);
  if FHandle <> 0 then SendMessage(FHandle, BFFM_ENABLEOK, 0, Ord(Value));
end;

procedure TBrowseFolderDialog.DefaultHandler(var Message);
begin
  if FHandle <> 0 then
    if not DoWndProc(TMessage(Message)) then
    begin
      with TMessage(Message) do
        Result := CallWindowProc(FDefWndProc, FHandle, Msg, WParam, LParam)
    end
  else
    inherited DefaultHandler(Message);
end;

procedure TBrowseFolderDialog.WMCommand(var Message: TMessage);
begin
  if (Message.wParam = HelpButtonId) and
     (LongRec(Message.lParam).Hi = BN_CLICKED) and (FHelpContext <> 0) then
    Application.HelpContext(FHelpContext)
  else
    if not DoCommand(Message) then inherited;
end;

procedure TBrowseFolderDialog.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
  FHandle := 0;
end;

function TBrowseFolderDialog.Execute: Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  Temp: Array[0..MAX_PATH] of Char;
begin
  if FDesktopRoot and (FBrowseKind = bfFolders) then
    BrowseInfo.pidlRoot := nil
  else
  begin
    if FBrowseKind = bfComputers then { root - Network }
      OleCheck(SHGetSpecialFolderLocation(0, CSIDL_NETWORK, BrowseInfo.pidlRoot))
    else { root - MyComputer }
      OleCheck(SHGetSpecialFolderLocation(0, CSIDL_DRIVES, BrowseInfo.pidlRoot));
  end;
  try
    SetLength(FDisplayName, MAX_PATH);
    with BrowseInfo do
    begin
      pszDisplayName := PChar(DisplayName);
      if DialogText <> '' then lpszTitle := PChar(DialogText) else lpszTitle := nil;
      if FBrowseKind = bfComputers then
        ulFlags := BIF_BROWSEFORCOMPUTER
      else
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_RETURNFSANCESTORS;
      lpfn := ExplorerHook;
      lParam := Longint(Self);
      hWndOwner := Application.Handle;
      iImage := 0;
    end;
    ItemIDList := TaskModalDialog2(BrowseInfo);
    Result := ItemIDList <> nil;
    if Result then
    try
      if FBrowseKind = bfFolders then
      begin
        Win32Check(SHGetPathFromIDList(ItemIDList, Temp));
        FFolderName := RemoveBackSlash(StrPas(Temp));
      end
      else
        FFolderName := DisplayName;
      FSelectedName := FFolderName;
      FImageIndex := BrowseInfo.iImage;
    finally
      CoTaskMemFree(ItemIDList);
    end;
  finally
    if BrowseInfo.pidlRoot <> nil then CoTaskMemFree(BrowseInfo.pidlRoot);
  end;
end;

function TBrowseFolderDialog.TaskModalDialog2(var Info: TBrowseInfo): PItemIDList;
var
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    try
      Result := SHBrowseForFolder(Info);
    finally
      FHandle := 0;
      FDefWndProc := nil;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function BrowseDirectory(var AFolderName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
begin
  if NewStyleControls then
  begin
    with TBrowseFolderDialog.Create(Application) do
    try
      DialogText := DlgText;
      FolderName := AFolderName;
      HelpContext := AHelpContext;
      Result := Execute;
      if Result then AFolderName := FolderName;
    finally
      Free;
    end;
  end
  else
    Result := SelectDirectory(AFolderName, [], AHelpContext);
end;

function BrowseComputer(var ComputerName: string; const DlgText: string;
  AHelpContext: THelpContext): Boolean;
begin
  with TBrowseFolderDialog.Create(Application) do
  try
    BrowseKind := bfComputers;
    DialogText := DlgText;
    FolderName := ComputerName;
    HelpContext := AHelpContext;
    Result := Execute;
    if Result then ComputerName := FolderName;
  finally
    Free;
  end;
end;

end.