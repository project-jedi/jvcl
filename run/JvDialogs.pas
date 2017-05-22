{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDialogs.PAS, released Oct 10, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): Debbie Gregory <Debbie.Gregory att cmsis dott com>
                Marcel van Brakel <brakelm att bart dott nl>.

Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDialogs;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  JVCLVer;

type
  TJvOpenDialogAC = (acEdit, acListView);
  TJvOpenDialogAS = (asSmallIcon, asReport);
  TDialogErrorEvent = procedure(Sender: TObject; ErrorCode:Cardinal) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOpenDialog = class(TOpenDialog)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FActiveControl: TJvOpenDialogAC;
    FActiveStyle: TJvOpenDialogAS;
    FActiveSettingDone: Boolean;
    FAutoSize: Boolean;
    FDefBtnCaption: string;
    FFilterLabelCaption: string;
    FInitialSize: TSize;
    FMakeResizeable: Boolean;
    FOriginalRect: TRect;
    FParentWndInstance, FOldParentWndInstance: Pointer;
    FParentWnd: THandle;
    FOnShareViolation: TCloseQueryEvent;
    FHeight: Integer;
    FWidth: Integer;
    FUseUserSize: Boolean;
    FOnError: TDialogErrorEvent;
    procedure CenterAndSize;
    function DoActiveSetting: Boolean;
    procedure WMNCDestroy(var Msg: TWMNCDestroy); message WM_NCDESTROY;
    procedure SetDefBtnCaption(const Value: string);
    procedure SetFilterLabelCaption(const Value: string);
  protected
    procedure DoFolderChange; override;
    function DoShareViolation: Boolean; dynamic;
    procedure DoShow; override;
    function GetLocalizedSizeCommand: string;
    procedure ParentResize; dynamic;
    procedure ParentWndProc(var Msg: TMessage); virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
    procedure UpdateCaptions;
    procedure UpdateControlPos; dynamic;
    procedure WndProc(var Msg: TMessage); override;
  protected
    procedure DoError(ErrorCode:Cardinal);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ParentWnd: THandle read FParentWnd;
    procedure SelectFolder(const FolderName: string);
    property Template;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ActiveControl: TJvOpenDialogAC read FActiveControl write FActiveControl default acEdit;
    property ActiveStyle: TJvOpenDialogAS read FActiveStyle write FActiveStyle default asSmallIcon;
    property AutoSize: Boolean read FAutoSize write FAutoSize default False;
    property DefBtnCaption: string read FDefBtnCaption write SetDefBtnCaption;
    property FilterLabelCaption: string read FFilterLabelCaption write SetFilterLabelCaption;
    property Height: Integer read FHeight write FHeight;
    property UseUserSize: Boolean read FUseUserSize write FUseUserSize default False;
    property Width: Integer read FWidth write FWidth;
    property OnError: TDialogErrorEvent read FOnError write FOnError;
    property OnShareViolation: TCloseQueryEvent read FOnShareViolation write FOnShareViolation;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSaveDialog = class(TJvOpenDialog)
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  end;

  TJvCDQueryEvent = procedure(Sender: TObject; SelectedColor: TColor; var Accept: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvColorDialog = class(TColorDialog)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FColorOkMessage: DWORD;
    FSetRBGMessage: DWORD;
    FOnQueryColor: TJvCDQueryEvent;
    procedure WMNCDestroy(var Msg: TWMNCDestroy); message WM_NCDESTROY;
  protected
    procedure DoClose; override;
    procedure DoShow; override;
    function DoQueryColor(Color: TColor): Boolean; dynamic;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectColor(Color: TColor);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property OnQueryColor: TJvCDQueryEvent read FOnQueryColor write FOnQueryColor;
  end;

var
  JvDialogsUseFixW2k: Boolean = True;

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
  CommDlg, CommCtrl, Dlgs,
  Types, SysUtils, Math,
  JclSysInfo,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils, // SetWindowLongPtr
  {$ENDIF ~COMPILER12_UP}
  JvJVCLUtils;

const
  btnOk = 1;
  btnCancel = 2;

var
  W2kFixMsAcmLibrary: THandle = 0;

function IsWin2kOrAbove: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and  CheckWin32Version(5, 0);
end;

procedure UninstallW2kFix;
begin
  if W2kFixMsAcmLibrary > 0 then
  begin
    FreeLibrary(W2kFixMsAcmLibrary);
    W2kFixMsAcmLibrary := 0;
  end;
end;

procedure InstallW2kFix;
begin
  if JvDialogsUseFixW2k and IsWin2K and (W2kFixMsAcmLibrary = 0) then
    W2kFixMsAcmLibrary := SafeLoadLibrary('msacm32.dll');
end;

//=== { TJvOpenDialog } ======================================================

constructor TJvOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveControl := acEdit;
  FActiveStyle := asSmallIcon;
  FMakeResizeable := GetWindowsVersion in [wvWin95, wvWin95OSR2, wvWinNT4];
  FParentWndInstance := JvMakeObjectInstance(ParentWndProc);
  FParentWndInstance := JvMakeObjectInstance(ParentWndProc);
end;

destructor TJvOpenDialog.Destroy;
begin
  JvFreeObjectInstance(FParentWndInstance);
  inherited Destroy;
end;

procedure TJvOpenDialog.CenterAndSize;
var
  Monitor: TMonitor;
begin
  if UseUserSize then
  begin
    if Application.MainForm <> nil then
      Monitor := Application.MainForm.Monitor
    else
      Monitor := Screen.Monitors[0];
    SetWindowPos(FParentWnd, 0,
      Monitor.Left + ((Monitor.Width - Width) div 2),
      Monitor.Top + ((Monitor.Height - Height) div 3),
      Width, Height,
      SWP_NOACTIVATE or SWP_NOZORDER);
  end;
end;

function TJvOpenDialog.DoActiveSetting: Boolean;
var
  DefViewWnd, ListViewWnd: HWND;
begin
  Result := False;
  if not FActiveSettingDone then
  begin
    DefViewWnd := FindWindowEx(FParentWnd, 0, PChar('SHELLDLL_DefView'), nil);
    ListViewWnd := FindWindowEx(DefViewWnd, 0, PChar('SysListView32'), nil);
    if (DefViewWnd <> 0) and (ListViewWnd <> 0) then
    begin
      if FActiveStyle = asReport then
        SendMessage(DefViewWnd, WM_COMMAND, $702C, 0);
      if FActiveControl = acListView then
      begin
        SetFocus(ListViewWnd);
        PostMessage(ListViewWnd, WM_KEYDOWN, VK_SPACE, 0);
      end;
      FActiveSettingDone := True;
      CenterAndSize;
      Result := True;
    end;
  end;
end;

procedure TJvOpenDialog.DoFolderChange;
begin
  DoActiveSetting;
  inherited DoFolderChange;
end;

function TJvOpenDialog.DoShareViolation: Boolean;
begin
  Result := True;
  if Assigned(FOnShareViolation) then
    FOnShareViolation(Self, Result);
end;

procedure TJvOpenDialog.DoShow;
var
  SysMenu: HMENU;
  R: TRect;
begin
  FParentWnd := GetParent(Handle);
  GetClientRect(FParentWnd, FOriginalRect);
  GetWindowRect(FParentWnd, R);
  FInitialSize.cx := R.Right - R.Left;
  FInitialSize.cy := R.Bottom - R.Top;
  Width := Max(Width, FInitialSize.cx);
  Height := Max(Height, FInitialSize.cy);
  if FMakeResizeable and (ofEnableSizing in Options) then
  begin
    SetWindowLong(ParentWnd, GWL_STYLE, GetWindowLong(ParentWnd, GWL_STYLE) or WS_THICKFRAME);
    SetWindowPos(ParentWnd, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
      SWP_FRAMECHANGED or SWP_DRAWFRAME or SWP_NOCOPYBITS);
    SysMenu := GetSystemMenu(ParentWnd, False);
    InsertMenu(SysMenu, SC_CLOSE, MF_BYCOMMAND, SC_SIZE, PChar(GetLocalizedSizeCommand));
    FOldParentWndInstance := Pointer(SetWindowLongPtr(FParentWnd, GWL_WNDPROC, LONG_PTR(FParentWndInstance)));
    UpdateControlPos;
  end;
  UpdateCaptions;
  inherited DoShow;
end;

function TJvOpenDialog.GetLocalizedSizeCommand: string;
var
  SysMenu: HMENU;
begin
  if not (csDesigning in ComponentState) and Assigned(Application.MainForm) then
  begin
    SysMenu := GetSystemMenu(Application.MainForm.Handle, False);
    SetString(Result, nil, 50);
    GetMenuString(SysMenu, SC_SIZE, PChar(Result), 50, MF_BYCOMMAND);
    Result := PChar(Result);
  end
  else
    Result := '';
  if Result = '' then
    Result := 'Size'; // do not localize
end;

procedure TJvOpenDialog.ParentResize;
begin
  InvalidateRect(ParentWnd, nil, False);
  UpdateControlPos;
end;

procedure TJvOpenDialog.ParentWndProc(var Msg: TMessage);
const
  SizeGripRectSize = 15;

  function SizeGripRect: TRect;
  begin
    GetClientRect(ParentWnd, Result);
    SetRect(Result, Result.Right - SizeGripRectSize, Result.Bottom - SizeGripRectSize,
      Result.Right, Result.Bottom);
  end;

  procedure PaintSizeGrip;
  var
    PS: TPaintStruct;
    DC: HDC;
    R: TRect;
    Pen, SavePen: HPen;
    I: Integer;
  begin
    DC := BeginPaint(ParentWnd, PS);
    R := SizeGripRect;
    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(clBtnShadow));
    SavePen := SelectObject(DC, Pen);
    for I := 0 to (SizeGripRectSize - 2) div 4 do
    begin
      MoveToEx(DC, R.Right, R.Bottom - (I * 4), nil);
      LineTo(DC, R.Right - (I * 4), R.Bottom);
      MoveToEx(DC, R.Right, R.Bottom - (I * 4) - 1, nil);
      LineTo(DC, R.Right - (I * 4) - 1, R.Bottom);
    end;
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(clWindow));
    SavePen := SelectObject(DC, Pen);
    for I := 0 to (SizeGripRectSize - 2) div 4 do
    begin
      MoveToEx(DC, R.Right, R.Bottom - (I * 4) - 2, nil);
      LineTo(DC, R.Right - (I * 4) - 2, R.Bottom);
    end;
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
    EndPaint(ParentWnd, PS);
  end;

begin
  with Msg do
  begin
    case Msg of
      {      WM_SIZE:
              ParentResize;}
      WM_GETMINMAXINFO:
        with PMinMaxInfo(LParam)^ do
        begin
          ptMinTrackSize.X := FInitialSize.cx;
          ptMinTrackSize.Y := FInitialSize.cy;
        end;
      WM_PAINT:
        PaintSizeGrip;
    end;
    Result := CallWindowProc(FOldParentWndInstance, FParentWnd, Msg, WParam, LParam);
    if Msg = WM_SIZE then
      ParentResize;
  end;
end;

procedure TJvOpenDialog.SetDefBtnCaption(const Value: string);
begin
  if FDefBtnCaption <> Value then
  begin
    FDefBtnCaption := Value;
    if FParentWnd <> 0 then
      UpdateCaptions;
  end;
end;

procedure TJvOpenDialog.SetFilterLabelCaption(const Value: string);
begin
  if FFilterLabelCaption <> Value then
  begin
    FFilterLabelCaption := Value;
    if FParentWnd <> 0 then
      UpdateCaptions;
  end;
end;

procedure TJvOpenDialog.SelectFolder(const FolderName: string);
var
  LastFocus: HWND;
begin
  if ParentWnd = 0 then
    Exit;
  LastFocus := GetFocus;
  SendMessage(ParentWnd, CDM_SETCONTROLTEXT, edt1, LPARAM(PChar(FolderName)));
  SendMessage(GetDlgItem(ParentWnd, btnOk), BM_CLICK, 0, 0);
  SendMessage(ParentWnd, CDM_SETCONTROLTEXT, edt1, 0);
  SetFocus(LastFocus);
end;

function TJvOpenDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
begin
  TOpenFileName(DialogData).hInstance := FindClassHInstance(Self.ClassType);
  FActiveSettingDone := False;
  if IsWin2kOrAbove then
  begin
    if ActiveStyle = asReport then
      InstallW2kFix;
    Result := inherited TaskModalDialog(DialogFunc, DialogData);
  end
  else
    Result := inherited TaskModalDialog(DialogFunc, DialogData);
  if not Result then
    DoError(CommDlgExtendedError);
end;

procedure TJvOpenDialog.UpdateCaptions;
begin
  if Length(FDefBtnCaption) > 0 then
    SendMessage(ParentWnd, CDM_SETCONTROLTEXT, btnOk, LPARAM(PChar(DefBtnCaption)));
  if Length(FFilterLabelCaption) > 0 then
    SendMessage(ParentWnd, CDM_SETCONTROLTEXT, stc2, LPARAM(PChar(FilterLabelCaption)));
end;

procedure TJvOpenDialog.UpdateControlPos;
var
  WRect: TRect;
  CtrlWnd: HWND;
  OfsSize: TPoint;
  CLeft, CTop, CWidth, CHeight: Integer;
  DeferHandle: HDWP;

  function GetDlgWndInfo(Wnd: HWND): Boolean;
  var
    Rect: TRect;
  begin
    Result := Wnd <> 0;
    if not Result then
      Exit;
    CtrlWnd := Wnd;
    GetWindowRect(CtrlWnd, Rect);
    MapWindowPoints(0, ParentWnd, Rect, 2);
    CLeft := Rect.Left;
    CTop := Rect.Top;
    CWidth := Rect.Right - Rect.Left;
    CHeight := Rect.Bottom - Rect.Top;
  end;

  function GetDlgItemInfo(ItemNum: Integer): Boolean;
  begin
    Result := GetDlgWndInfo(GetDlgItem(ParentWnd, ItemNum));
  end;

begin
  GetClientRect(ParentWnd, WRect);
  OfsSize.X := (WRect.Right - WRect.Left) - (FOriginalRect.Right - FOriginalRect.Left);
  OfsSize.Y := (WRect.Bottom - WRect.Top) - (FOriginalRect.Bottom - FOriginalRect.Top);
  FOriginalRect := WRect;

  DeferHandle := BeginDeferWindowPos(12);

  GetDlgItemInfo(btnOk); // Default Button
  Inc(CLeft, OfsSize.X);
  Inc(CTop, OfsSize.Y);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(btnCancel); // Cancel Button
  Inc(CLeft, OfsSize.X);
  Inc(CTop, OfsSize.Y);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(pshHelp); // Help Button
  Inc(CLeft, OfsSize.X);
  Inc(CTop, OfsSize.Y);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(edt1); // Filename
  Inc(CTop, OfsSize.Y);
  Inc(CWidth, OfsSize.X);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, CWidth, CHeight,
    SWP_NOACTIVATE or SWP_NOZORDER);

  GetDlgItemInfo(cmb1); // File Type
  Inc(CTop, OfsSize.Y);
  Inc(CWidth, OfsSize.X);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, CWidth, CHeight,
    SWP_NOACTIVATE or SWP_NOZORDER);

  GetDlgItemInfo(chx1); // Read-only Checkbox
  Inc(CTop, OfsSize.Y);
  Inc(CWidth, OfsSize.X);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, CWidth, CHeight,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(stc2); // File Type Label
  Inc(CTop, OfsSize.Y);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(stc3); // Filename Label
  Inc(CTop, OfsSize.Y);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);

  GetDlgItemInfo(cmb2); // Folder combobox
  Inc(CWidth, OfsSize.X);
  DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, 0, 0, CWidth, CHeight,
    SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE);

  if GetDlgItemInfo(lst2) then // ListView run
  begin
    Inc(CHeight, OfsSize.Y);
    Inc(CWidth, OfsSize.X);
    DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, 0, 0, CWidth, CHeight,
      SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE);
  end;
  if GetDlgItemInfo(lst1) then // ListView init
  begin
    Inc(CHeight, OfsSize.Y);
    Inc(CWidth, OfsSize.X);
    DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, 0, 0, CWidth, CHeight,
      SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE);
  end;

  if GetDlgWndInfo(FindWindowEx(FParentWnd, 0, TOOLBARCLASSNAME, nil)) then
  begin
    Inc(CLeft, OfsSize.X);
    DeferHandle := DeferWindowPos(DeferHandle, CtrlWnd, 0, CLeft, CTop, 0, 0,
      SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSIZE);
  end;

  EndDeferWindowPos(DeferHandle);
end;

procedure TJvOpenDialog.WMNCDestroy(var Msg: TWMNCDestroy);
begin
  FParentWnd := 0;
  inherited;
end;

procedure TJvOpenDialog.WndProc(var Msg: TMessage);
const
  ShareViolResult: array [Boolean] of DWORD = (OFN_SHARENOWARN, OFN_SHAREFALLTHROUGH);
begin
  with Msg do
    case Msg of
      WM_ENTERIDLE:
        DoActiveSetting;
      WM_NOTIFY:
        case POFNotify(LParam)^.hdr.code of
          CDN_SHAREVIOLATION:
            if Assigned(FOnShareViolation) then
            begin
              Result := ShareViolResult[DoShareViolation];
              SetWindowLong(Handle, DWL_MSGRESULT, Result);
              Exit;
            end;
        end;
    end;
  inherited;
end;

procedure TJvOpenDialog.DoError(ErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorCode);
end;

//=== { TJvSaveDialog } ======================================================

function TJvSaveDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
begin
  DialogFunc := @GetSaveFileName;
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

//=== { TJvColorDialog } =====================================================

var
  GlobalColorDialog: TJvColorDialog = nil;
  OldColorDialogHookProc: Pointer = nil;

function ColorDialogHook(Wnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): {$IFDEF RTL230_UP}UINT_PTR{$ELSE}UINT{$ENDIF RTL230_UP}; stdcall;
begin
  if Assigned(GlobalColorDialog) and (Msg = GlobalColorDialog.FColorOkMessage) then
    Result := Integer(not GlobalColorDialog.DoQueryColor(TColor(PChooseColor(LParam)^.rgbResult)))
  else
    Result := CallWindowProc(OldColorDialogHookProc, Wnd, Msg, WParam, LParam);
end;

constructor TJvColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOkMessage := RegisterWindowMessage(COLOROKSTRING);
  FSetRBGMessage := RegisterWindowMessage(SETRGBSTRING);
end;

procedure TJvColorDialog.DoClose;
begin
  GlobalColorDialog := nil;
  inherited DoClose;
end;

function TJvColorDialog.DoQueryColor(Color: TColor): Boolean;
begin
  Result := True;
  if Assigned(FOnQueryColor) then
    FOnQueryColor(Self, Color, Result);
end;

procedure TJvColorDialog.DoShow;
begin
  GlobalColorDialog := Self;
  inherited DoShow;
end;

procedure TJvColorDialog.SelectColor(Color: TColor);
begin
  if Handle <> 0 then
    SendMessage(Handle, FSetRBGMessage, 0, ColorToRGB(Color));
end;

function TJvColorDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
begin
  with TChooseColor(DialogData) do
  begin
    OldColorDialogHookProc := @lpfnHook;
    lpfnHook := ColorDialogHook;
  end;
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

procedure TJvColorDialog.WMNCDestroy(var Msg: TWMNCDestroy);
begin
  inherited;
  OldColorDialogHookProc := nil;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  UninstallW2kFix;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.