{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageSetup.PAS, released on 2000-07-25.

The Initial Developer of the Original Code is Pasha Sivtsov [psivtsov@mail.ru]
Portions created by Pasha Sivtsov are Copyright (C) 2000 Pasha Sivtsov.
All Rights Reserved.

Last Modified: 2002-02-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvPageSetup;

interface

uses
  Windows, Classes, Dialogs, Messages, Graphics, CommDlg, JVCLVer, JvComponent;

const
  // Internal events
  CM_PAINTINIT = WM_USER + 10;
  CM_PAINTPAGE = WM_USER + 11;

  // masks for separation of parameters from TJvPSPaintEvent.aFlags
  PRINTER_MASK = $00000002;
  ORIENT_MASK = $00000004;
  PAPER_MASK = $00000008;

type
  // Available options
  TJvPageSetupFlags = (
    poDefaultMinMargins, poMargins, poMinMargins, poDisableMargins,
    poDisableOrientation, poDisablePagePainting, poDisablePaper, poDisablePrinter,
    poHundredthsOfMillimeters, poThousandthsOfInches, poNoWarning);
  TJvPageOptions = set of TJvPageSetupFlags;

  // Areas of drawing
  TJvPSPaintWhat = (
    pwFullPage, pwMinimumMargins,
    pwMargins, pwGreekText,
    pwEnvStamp, pwYAFullPage);

  TJvMarginSize = class(TPersistent)
  private
    FMargin: TRect;
    procedure AssignError;
    function GetValue(aIndex: Integer): Integer;
    procedure SetValue(aIndex: Integer; aValue: Integer);
    procedure SetRect(aValue: TRect);
  protected
    procedure AssignTo(aDest: TPersistent); override;
  public
    function IsNull: Boolean;
    function MarginsEqu(aMargin: TJvMarginSize): Boolean;
    property AsRect: TRect read FMargin write SetRect;
  published
    property Left: Integer index 0 read GetValue write SetValue stored False;
    property Top: Integer index 1 read GetValue write SetValue stored False;
    property Right: Integer index 2 read GetValue write SetValue stored False;
    property Bottom: Integer index 3 read GetValue write SetValue stored False;
  end;

  TJvPageSetupDialog = class;

  TJvPSPaintEvent = procedure(Sender: TJvPageSetupDialog; aPaper, aFlags: Integer;
    PageSetupRec: TPageSetupDlg; PaintWhat: TJvPSPaintWhat; Canvas: TCanvas;
    Rect: TRect; var NoDefaultPaint: Boolean) of object;

  TJvPageSetupDialog = class(TCommonDialog)
  private
    FOptions: TJvPageOptions;
    FFlags: DWORD;
    FMargin, FMinMargin: TJvMarginSize;
    FPaperSize: TPoint;
    FOnPrinter: TNotifyEvent;
    FOnPaint: TJvPSPaintEvent;
    FInitPaper, FInitFlags: Integer;
    FPageSetupRec: TPageSetupDlg;
    FPaintWhat: TJvPSPaintWhat;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetOptions(aValue: TJvPageOptions);
    function DoExecute(aShow: Boolean): Boolean;

    procedure ReadMargin(aMargin: TJvMarginSize; Reader: TReader);
    procedure WriteMargin(aMargin: TJvMarginSize; Writer: TWriter);
    procedure ReadValues(aReader: TReader);
    procedure WriteValues(aWriter: TWriter);
    procedure ReadMinValues(aReader: TReader);
    procedure WriteMinValues(aWriter: TWriter);

    procedure WMHelp(var aMessage: TWMHelp); message WM_HELP;
    procedure WMCommand(var aMessage: TMessage); message WM_COMMAND;
    procedure WMPaintInit(var aMessage: TMessage); message CM_PAINTINIT;
    procedure WMPaintPage(var aMessage: TMessage); message CM_PAINTPAGE;
  protected
    procedure DefineProperties(aFiler: TFiler); override;
    function DoPrinter: Boolean; virtual;
    function DoPaint(aInitPaper, aInitFlags: Integer; aPageSetupRec: TPageSetupDlg;
      aPaintWhat: TJvPSPaintWhat; aCanvas: TCanvas; aRect: TRect): Boolean; virtual;
    function TaskModalDialog(aDialogFunc: Pointer; var aDialogData): Bool; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    procedure GetDefaults; virtual;
    property PaperSize: TPoint read FPaperSize;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Margin: TJvMarginSize read FMargin;
    property MinMargin: TJvMarginSize read FMinMargin;
    property Options: TJvPageOptions read FOptions write SetOptions
      default [poDefaultMinMargins, poHundredthsOfMillimeters];
    property OnPaint: TJvPSPaintEvent read FOnPaint write FOnPaint;
    property OnPrinter: TNotifyEvent read FOnPrinter write FOnPrinter;
  end;

  // Internal routines
procedure CenterWindow(Wnd: HWnd);
procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
procedure SetPrinter(DeviceMode, DeviceNames: THandle);
function CopyData(Handle: THandle): THandle;
function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
function PageDrawHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;

///////////////////////////////////////////////////////////////////////////////
implementation
///////////////////////////////////////////////////////////////////////////////

uses
  SysUtils, Controls, Forms, Printers,
  JvFunctions;

resourcestring
  SInvalidValue = 'Value must be greater then zero';

  { TJvMarginSize }

  //-----------------------------------------------------------------------------

procedure TJvMarginSize.AssignError;
begin
  raise ERangeError.Create(SInvalidValue);
end;

//-----------------------------------------------------------------------------

procedure TJvMarginSize.AssignTo(aDest: TPersistent);
begin
  if aDest is TJvMarginSize then
    with aDest as TJvMarginSize do
      FMargin := Self.FMargin
  else
    inherited AssignTo(aDest);
end;

//-----------------------------------------------------------------------------

function TJvMarginSize.IsNull: Boolean;
begin
  with FMargin do
    Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

//-----------------------------------------------------------------------------

function TJvMarginSize.MarginsEqu(aMargin: TJvMarginSize): Boolean;
begin
  Result := (FMargin.Left = aMargin.Left) and (FMargin.Top = aMargin.Top)
    and (FMargin.Right = aMargin.Right) and (FMargin.Bottom = aMargin.Bottom);
end;

//-----------------------------------------------------------------------------

function TJvMarginSize.GetValue(aIndex: Integer): Integer;
begin
  case aIndex of
    0: Result := FMargin.Left;
    1: Result := FMargin.Top;
    2: Result := FMargin.Right;
  else
    Result := FMargin.Bottom;
  end;
end;

//-----------------------------------------------------------------------------

procedure TJvMarginSize.SetValue(aIndex: Integer; aValue: Integer);
begin
  if aValue < 0 then
    AssignError;
  case aIndex of
    0: FMargin.Left := aValue;
    1: FMargin.Top := aValue;
    2: FMargin.Right := aValue;
  else
    FMargin.Bottom := aValue;
  end;
end;

//-----------------------------------------------------------------------------

procedure TJvMarginSize.SetRect(aValue: TRect);
begin
  with aValue do
    if (Left < 0) or (Top < 0) or (Right < 0) or (Bottom < 0) then
      AssignError;
  FMargin := aValue;
end;

{ Private globals - some routines copied from dialogs.pas }

//-----------------------------------------------------------------------------
type
  THackCommonDialog = class(TComponent)
  private
{$HINTS OFF}FCtl3D: Boolean;
{$HINTS ON}
    FDefWndProc: Pointer;
{$HINTS OFF}FHelpContext: THelpContext;
{$HINTS ON}
    FHandle: HWnd;
    FObjectInstance: Pointer;
  end;

var
  CreationControl: TCommonDialog = nil;
  PageSetupControl: TJvPageSetupDialog = nil;

  // Center the given window on the screen - D3/D4/D5
  //-----------------------------------------------------------------------------

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
{$IFDEF COMPILER4_UP}
  Monitor: TMonitor;
{$ENDIF}
begin
  GetWindowRect(Wnd, Rect);
{$IFNDEF COMPILER4_UP}
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
{$ELSE}
  if Application.MainForm <> nil then
    Monitor := Application.MainForm.Monitor
  else
    Monitor := Screen.Monitors[0];
  SetWindowPos(Wnd, 0,
    Monitor.Left + ((Monitor.Width - Rect.Right + Rect.Left) div 2),
    Monitor.Top + ((Monitor.Height - Rect.Bottom + Rect.Top) div 3),
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
{$ENDIF}
end;

// Generic dialog hook. Centers the dialog on the screen in response to
// the WM_INITDIALOG message
//-----------------------------------------------------------------------------

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT;
begin
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    CenterWindow(Wnd);
    THackCommonDialog(CreationControl).FHandle := Wnd;
    THackCommonDialog(CreationControl).FDefWndProc :=
      Pointer(SetWindowLong(Wnd, GWL_WNDPROC,
      Longint(THackCommonDialog(CreationControl).FObjectInstance)));
    CallWindowProc(THackCommonDialog(CreationControl).FObjectInstance, Wnd,
      Msg, WParam, LParam);
    CreationControl := nil;
  end;
end;

//-----------------------------------------------------------------------------

function PageDrawHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT;
const
  PagePaintWhat: array[WM_PSD_FULLPAGERECT..WM_PSD_YAFULLPAGERECT] of TJvPSPaintWhat = (
    pwFullPage, pwMinimumMargins, pwMargins,
    pwGreekText, pwEnvStamp, pwYAFullPage);
begin
  case Msg of
    WM_PSD_PAGESETUPDLG:
      Result := SendMessage(PageSetupControl.Handle, CM_PAINTINIT, WParam, LParam);
    WM_PSD_FULLPAGERECT,
      WM_PSD_MINMARGINRECT,
      WM_PSD_MARGINRECT,
      WM_PSD_GREEKTEXTRECT,
      WM_PSD_ENVSTAMPRECT,
      WM_PSD_YAFULLPAGERECT:
      begin
        PageSetupControl.FPaintWhat := PagePaintWhat[Msg];
        Result := SendMessage(PageSetupControl.Handle, CM_PAINTPAGE, WParam, LParam);
      end;
  else
    Result := 0;
  end;
end;

{ Printer dialog routines }

//-----------------------------------------------------------------------------

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array[0..79] of Char;
  DevNames: PDevNames;
  Offset: PChar;
begin
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  if DeviceMode <> 0 then
  begin
    DeviceNames := GlobalAlloc(GHND, SizeOf(TDevNames) +
      StrLen(Device) + StrLen(Driver) + StrLen(Port) + 3);
    DevNames := PDevNames(GlobalLock(DeviceNames));
    try
      Offset := PChar(DevNames) + SizeOf(TDevNames);
      with DevNames^ do
      begin
        wDriverOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Driver) + 1;
        wDeviceOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Device) + 1;
        wOutputOffset := Longint(Offset) - Longint(DevNames);
        StrCopy(Offset, Port);
      end;
    finally
      GlobalUnlock(DeviceNames);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure SetPrinter(DeviceMode, DeviceNames: THandle);
var
  DevNames: PDevNames;
begin
  if DeviceNames = 0 then
    Exit;

  DevNames := PDevNames(GlobalLock(DeviceNames));
  try
    with DevNames^ do
{$IFDEF COMPILER4_UP}
      Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
        PChar(DevNames) + wDriverOffset,
        PChar(DevNames) + wOutputOffset, DeviceMode);
{$ELSE} // D3+NT bugfix
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
          PChar(DevNames) + wDriverOffset, '', DeviceMode)
      else
        Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
          PChar(DevNames) + wDriverOffset,
          PChar(DevNames) + wOutputOffset, DeviceMode);
{$ENDIF}
  finally
    GlobalUnlock(DeviceNames);
    GlobalFree(DeviceNames);
  end;
end;

//-----------------------------------------------------------------------------

function CopyData(Handle: THandle): THandle;
var
  Src, Dest: PChar;
  Size: Integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
    try
      Src := GlobalLock(Handle);
      Dest := GlobalLock(Result);
      if (Src <> nil) and (Dest <> nil) then
        Move(Src^, Dest^, Size);
    finally
      GlobalUnlock(Handle);
      GlobalUnlock(Result);
    end
  end
  else
    Result := 0;
end;

{ TJvPageSetupDialog }

//-----------------------------------------------------------------------------

constructor TJvPageSetupDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMargin := TJvMarginSize.Create;
  FMinMargin := TJvMarginSize.Create;
  Options := [poDefaultMinMargins, poHundredthsOfMillimeters];
end;

//-----------------------------------------------------------------------------

destructor TJvPageSetupDialog.Destroy;
begin
  FMargin.Free;
  FMinMargin.Free;
  inherited;
end;

// Determination of streamed properties
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.DefineProperties(aFiler: TFiler);

// Rule 1
//---------------------------------------------------------------------------
  function DoWriteMargin1: Boolean;
  begin
    if aFiler.Ancestor <> nil then
      Result := not TJvPageSetupDialog(aFiler.Ancestor).FMargin.MarginsEqu(FMargin)
    else
      Result := (FMargin <> nil) and (not FMargin.IsNull);
  end;

  // Rule 2
  //---------------------------------------------------------------------------
  function DoWriteMargin2: Boolean;
  begin
    if aFiler.Ancestor <> nil then
      Result := not TJvPageSetupDialog(aFiler.Ancestor).FMinMargin.MarginsEqu(FMinMargin)
    else
      Result := (FMinMargin <> nil) and (not FMinMargin.IsNull);
  end;

begin
  inherited DefineProperties(aFiler);
  with aFiler do
  begin
    DefineProperty('MarginData', ReadValues, WriteValues, DoWriteMargin1);
    DefineProperty('MinMarginData', ReadMinValues, WriteMinValues, DoWriteMargin2);
  end;
end;

// Reading from stream
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.ReadMargin(aMargin: TJvMarginSize; Reader: TReader);
begin
  with aMargin, Reader do
  begin
    ReadListBegin;
    Left := ReadInteger;
    Top := ReadInteger;
    Right := ReadInteger;
    Bottom := ReadInteger;
    ReadListEnd;
  end;
end;

// Writing to stream
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WriteMargin(aMargin: TJvMarginSize; Writer: TWriter);
begin
  with aMargin, Writer do
  begin
    WriteListBegin;
    WriteInteger(Left);
    WriteInteger(Top);
    WriteInteger(Right);
    WriteInteger(Bottom);
    WriteListEnd;
  end;
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.ReadValues(aReader: TReader);
begin
  ReadMargin(FMargin, aReader);
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WriteValues(aWriter: TWriter);
begin
  WriteMargin(FMargin, aWriter);
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.ReadMinValues(aReader: TReader);
begin
  ReadMargin(FMinMargin, aReader);
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WriteMinValues(aWriter: TWriter);
begin
  WriteMargin(FMinMargin, aWriter);
end;

// Processing Help commands
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WMHelp(var aMessage: TWMHelp);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext)
  else
    inherited;
end;

// Processing <Printer> button
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WMCommand(var aMessage: TMessage);
const
  IDPRINTERBTN = $0402;
begin
  if not ((LongRec(aMessage.WParam).Lo = IDPRINTERBTN) and
    (LongRec(aMessage.WParam).Hi = BN_CLICKED) and DoPrinter) then
    inherited;
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WMPaintInit(var aMessage: TMessage);
begin
  FInitPaper := LoWord(aMessage.WParam);
  FInitFlags := HiWord(aMessage.WParam);
  FPageSetupRec := PPageSetupDlg(aMessage.LParam)^;
  aMessage.Result := Ord(not Assigned(FOnPaint));
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.WMPaintPage(var aMessage: TMessage);
var
  ePaintRect: TRect;
  ePaintCanvas: TCanvas;
begin
  if aMessage.LParam <> 0 then
    ePaintRect := PRect(aMessage.LParam)^
  else
    ePaintRect := Rect(0, 0, 0, 0);

  ePaintCanvas := TCanvas.Create;
  ePaintCanvas.Handle := HDC(aMessage.WParam);
  try
    aMessage.Result := Ord(DoPaint(FInitPaper, FInitFlags, FPageSetupRec,
      FPaintWhat, ePaintCanvas, ePaintRect));
  finally
    ePaintCanvas.Free;
  end;
end;

//-----------------------------------------------------------------------------

function TJvPageSetupDialog.DoPrinter: Boolean;
begin
  Result := Assigned(FOnPrinter);
  if Result then
    FOnPrinter(Self);
end;

//-----------------------------------------------------------------------------

function TJvPageSetupDialog.DoPaint(aInitPaper, aInitFlags: Integer;
  aPageSetupRec: TPageSetupDlg; aPaintWhat: TJvPSPaintWhat; aCanvas: TCanvas;
  aRect: TRect): Boolean;
begin
  Result := False;
  if Assigned(FOnPaint) then
    FOnPaint(Self, aInitPaper, aInitFlags, aPageSetupRec, aPaintWhat, aCanvas,
      aRect, Result);
end;

// Show modal dialog
//-----------------------------------------------------------------------------

function TJvPageSetupDialog.TaskModalDialog(aDialogFunc: Pointer; var aDialogData): Bool;
type
  TDialogFunc = function(var aDialogData): Bool; stdcall;
var
  eActiveWindow: HWnd;
  eWindowList: Pointer;
  eFPUControlWord: Word;
begin
  eActiveWindow := GetActiveWindow;
  eWindowList := DisableTaskWindows(0);
  try
    Application.HookMainWindow(MessageHook);
    asm
      // Avoid FPU control word change in NETRAP.dll, NETAPI32.dll, etc
      FNSTCW  eFPUControlWord
    end;
    try
      CreationControl := Self;
      PageSetupControl := Self;
      Result := TDialogFunc(aDialogFunc)(aDialogData);
    finally
      PageSetupControl := nil;
      asm
        FNCLEX
        FLDCW eFPUControlWord
      end;
      Application.UnhookMainWindow(MessageHook);
    end;
  finally
    EnableTaskWindows(eWindowList);
    SetActiveWindow(eActiveWindow);
  end;
end;

//-----------------------------------------------------------------------------

function TJvPageSetupDialog.DoExecute(aShow: Boolean): Boolean;
var
  ePageDlgRec: TPageSetupDlg;
  eDevHandle: THandle;
  eErr: Integer;
begin
  // fill record
  FillChar(ePageDlgRec, SizeOf(ePageDlgRec), 0);
  with ePageDlgRec do
  begin
    lStructSize := SizeOf(ePageDlgRec);
    hWndOwner := Application.Handle;
    Flags := FFlags;
    rtMinMargin := Rect(FMinMargin.Left, FMinMargin.Top, FMinMargin.Right,
      FMinMargin.Bottom);
    rtMargin := Rect(FMargin.Left, FMargin.Top, FMargin.Right, FMargin.Bottom);
    hInstance := SysInit.HInstance;
    if aShow then
    begin
      lpfnPageSetupHook := DialogHook;
      Flags := FFlags or PSD_ENABLEPAGESETUPHOOK;
      GetPrinter(eDevHandle, hDevNames);
      hDevMode := CopyData(eDevHandle);
    end
    else
    begin
      Flags := Flags or PSD_RETURNDEFAULT;
    end;
    if Template <> nil then
    begin
      Flags := Flags or PSD_ENABLEPAGESETUPTEMPLATE;
      lpPageSetupTemplateName := Template;
    end;
    if Assigned(FOnPaint) then
    begin
      Flags := Flags or PSD_ENABLEPAGEPAINTHOOK;
      lpfnPagePaintHook := PageDrawHook;
    end;

    if aShow then
      Result := TaskModalDialog(@PageSetupDlg, ePageDlgRec)
    else
      Result := PageSetupDlg(ePageDlgRec);
    eErr := CommDlgExtendedError;

    if Result then
      SetPrinter(hDevMode, hDevNames)
    else
    begin
      if hDevMode <> 0 then
        GlobalFree(hDevMode);
      if hDevNames <> 0 then
        GlobalFree(hDevNames);
    end;
    OSCheck(eErr = 0);

    FMargin.AsRect := rtMargin;
    FPaperSize := ptPaperSize;
  end;
end;

//-----------------------------------------------------------------------------

function TJvPageSetupDialog.Execute: Boolean;
begin
  Result := DoExecute(True);
end;

// Get default margin values
//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.GetDefaults;
begin
  DoExecute(False);
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupDialog.SetOptions(aValue: TJvPageOptions);
const
  WinFlags: array[TJvPageSetupFlags] of DWORD = (
    PSD_DEFAULTMINMARGINS, PSD_MARGINS, PSD_MINMARGINS,
    PSD_DISABLEMARGINS, PSD_DISABLEORIENTATION,
    PSD_DISABLEPAGEPAINTING, PSD_DISABLEPAPER, PSD_DISABLEPRINTER,
    PSD_INHUNDREDTHSOFMILLIMETERS, PSD_INTHOUSANDTHSOFINCHES,
    PSD_NOWARNING);
var
  i: TJvPageSetupFlags;
begin
  if (poDefaultMinMargins in aValue) and not (poDefaultMinMargins in FOptions) then
    aValue := aValue - [poMinMargins];
  if (poMinMargins in aValue) and not (poMinMargins in FOptions) then
    aValue := aValue - [poDefaultMinMargins];
  if (poHundredthsOfMillimeters in aValue) and not (poHundredthsOfMillimeters in FOptions) then
    aValue := aValue - [poThousandthsOfInches];
  if (poThousandthsOfInches in aValue) and not (poThousandthsOfInches in FOptions) then
    aValue := aValue - [poHundredthsOfMillimeters];
  FOptions := aValue;

  // set flags
  FFlags := 0;
  for i := Low(TJvPageSetupFlags) to High(TJvPageSetupFlags) do
    if i in FOptions then
      FFlags := FFlags or WinFlags[i];
end;

end.
