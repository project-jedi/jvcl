{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageSetup.PAS, released on 2000-07-25.

The Initial Developer of the Original Code is Pasha Sivtsov [psivtsov att mail dott ru]
Portions created by Pasha Sivtsov are Copyright (C) 2000 Pasha Sivtsov.
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I vclonly.inc}

unit JvPageSetup;

interface

uses
  Windows, Classes, Messages, Graphics, CommDlg, Dialogs,
  JvBaseDlg;

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
  TJvPageSetupFlags =
    (poDefaultMinMargins, poMargins, poMinMargins, poDisableMargins,
     poDisableOrientation, poDisablePagePainting, poDisablePaper, poDisablePrinter,
     poHundredthsOfMillimeters, poThousandthsOfInches, poNoWarning);
  TJvPageOptions = set of TJvPageSetupFlags;

  // Areas of drawing
  TJvPSPaintWhat =
   (pwFullPage, pwMinimumMargins,
    pwMargins, pwGreekText,
    pwEnvStamp, pwYAFullPage);

  TJvMarginSize = class(TPersistent)
  private
    FMargin: TRect;
    procedure AssignError;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; Value: Integer);
    procedure SetRect(Value: TRect);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function IsNull: Boolean;
    function MarginsEqu(AMargin: TJvMarginSize): Boolean;
    property AsRect: TRect read FMargin write SetRect;
  published
    property Left: Integer index 0 read GetValue write SetValue stored False;
    property Top: Integer index 1 read GetValue write SetValue stored False;
    property Right: Integer index 2 read GetValue write SetValue stored False;
    property Bottom: Integer index 3 read GetValue write SetValue stored False;
  end;

  TJvPageSetupDialog = class;

  TJvPSPaintEvent = procedure(Sender: TJvPageSetupDialog; Paper, Flags: Integer;
    PageSetupRec: TPageSetupDlg; PaintWhat: TJvPSPaintWhat; Canvas: TCanvas;
    Rect: TRect; var NoDefaultPaint: Boolean) of object;

  TJvPageSetupDialog = class(TJvCommonDialog)
  private
    FOptions: TJvPageOptions;
    FFlags: DWORD;
    FMargin: TJvMarginSize;
    FMinMargin: TJvMarginSize;
    FPaperSize: TPoint;
    FOnPrinter: TNotifyEvent;
    FOnPaint: TJvPSPaintEvent;
    FInitPaper: Integer;
    FInitFlags: Integer;
    FPageSetupRec: TPageSetupDlg;
    FPaintWhat: TJvPSPaintWhat;
    procedure SetOptions(Value: TJvPageOptions);
    function DoExecute(Show: Boolean): Boolean;
    procedure ReadMargin(AMargin: TJvMarginSize; Reader: TReader);
    procedure WriteMargin(AMargin: TJvMarginSize; Writer: TWriter);
    procedure ReadValues(AReader: TReader);
    procedure WriteValues(AWriter: TWriter);
    procedure ReadMinValues(AReader: TReader);
    procedure WriteMinValues(AWriter: TWriter);
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMPaintInit(var Msg: TMessage); message CM_PAINTINIT;
    procedure WMPaintPage(var Msg: TMessage); message CM_PAINTPAGE;
  protected
    procedure DefineProperties(AFiler: TFiler); override;
    function DoPrinter: Boolean; virtual;
    function DoPaint(InitPaper, InitFlags: Integer; PageSetupRec: TPageSetupDlg;
      PaintWhat: TJvPSPaintWhat; Canvas: TCanvas; Rect: TRect): Boolean; virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    procedure GetDefaults; virtual;
    property PaperSize: TPoint read FPaperSize;
  published
    property Margin: TJvMarginSize read FMargin;
    property MinMargin: TJvMarginSize read FMinMargin;
    property Options: TJvPageOptions read FOptions write SetOptions
      default [poDefaultMinMargins, poHundredthsOfMillimeters];
    property OnPaint: TJvPSPaintEvent read FOnPaint write FOnPaint;
    property OnPrinter: TNotifyEvent read FOnPrinter write FOnPrinter;
  end;

implementation

uses
  SysUtils, Controls, Forms, Printers,
  JvJCLUtils, JvResources;

//=== { TJvMarginSize } ======================================================

procedure TJvMarginSize.AssignError;
begin
  raise ERangeError.CreateRes(@RsEInvalidValue);
end;

procedure TJvMarginSize.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvMarginSize then
    with Dest as TJvMarginSize do
      FMargin := Self.FMargin
  else
    inherited AssignTo(Dest);
end;

function TJvMarginSize.IsNull: Boolean;
begin
  with FMargin do
    Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

function TJvMarginSize.MarginsEqu(AMargin: TJvMarginSize): Boolean;
begin
  Result := (FMargin.Left = AMargin.Left) and (FMargin.Top = AMargin.Top)
    and (FMargin.Right = AMargin.Right) and (FMargin.Bottom = AMargin.Bottom);
end;

function TJvMarginSize.GetValue(Index: Integer): Integer;
begin
  case Index of
    0:
      Result := FMargin.Left;
    1:
      Result := FMargin.Top;
    2:
      Result := FMargin.Right;
  else
    Result := FMargin.Bottom;
  end;
end;

procedure TJvMarginSize.SetValue(Index: Integer; Value: Integer);
begin
  if Value < 0 then
    AssignError;
  case Index of
    0:
      FMargin.Left := Value;
    1:
      FMargin.Top := Value;
    2:
      FMargin.Right := Value;
  else
    FMargin.Bottom := Value;
  end;
end;

procedure TJvMarginSize.SetRect(Value: TRect);
begin
  with Value do
    if (Left < 0) or (Top < 0) or (Right < 0) or (Bottom < 0) then
      AssignError;
  FMargin := Value;
end;

{ Private globals - some routines copied from dialogs.pas }

type
  THackCommonDialog = class(TComponent)
  private
    {$HINTS OFF}
    FCtl3D: Boolean;
    {$HINTS ON}
    FDefWndProc: Pointer;
    {$HINTS OFF}
    FHelpContext: THelpContext;
    {$HINTS ON}
    FHandle: HWND;
    FObjectInstance: Pointer;
  end;

var
  CreationControl: TCommonDialog = nil;
  PageSetupControl: TJvPageSetupDialog = nil;

// Center the given window on the screen - D3/D4/D5

procedure CenterWindow(Wnd: HWND);
var
  Rect: TRect;
  Monitor: TMonitor;
begin
  GetWindowRect(Wnd, Rect);
  if Application.MainForm <> nil then
    Monitor := Application.MainForm.Monitor
  else
    Monitor := Screen.Monitors[0];
  SetWindowPos(Wnd, 0,
    Monitor.Left + ((Monitor.Width - Rect.Right + Rect.Left) div 2),
    Monitor.Top + ((Monitor.Height - Rect.Bottom + Rect.Top) div 3),
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

// Generic dialog hook. Centers the dialog on the screen in response to
// the WM_INITDIALOG message

function DialogHook(Wnd: HWND; Msg: UINT; AWParam: WPARAM; ALParam: LPARAM): UINT; stdcall;
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
      Msg, AWParam, ALParam);
    CreationControl := nil;
  end;
end;

function PageDrawHook(Wnd: HWND; Msg: UINT; AWParam: WPARAM; ALParam: LPARAM): UINT; stdcall;
const
  PagePaintWhat: array [WM_PSD_FULLPAGERECT..WM_PSD_YAFULLPAGERECT] of TJvPSPaintWhat =
   (pwFullPage, pwMinimumMargins, pwMargins,
    pwGreekText, pwEnvStamp, pwYAFullPage);
begin
  case Msg of
    WM_PSD_PAGESETUPDLG:
      Result := SendMessage(PageSetupControl.Handle, CM_PAINTINIT, AWParam, ALParam);
    WM_PSD_FULLPAGERECT, WM_PSD_MINMARGINRECT, WM_PSD_MARGINRECT,
    WM_PSD_GREEKTEXTRECT, WM_PSD_ENVSTAMPRECT, WM_PSD_YAFULLPAGERECT:
      begin
        PageSetupControl.FPaintWhat := PagePaintWhat[Msg];
        Result := SendMessage(PageSetupControl.Handle, CM_PAINTPAGE, AWParam, ALParam);
      end;
  else
    Result := 0;
  end;
end;

{ Printer dialog routines }

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array [0..79] of Char;
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

procedure SetPrinter(DeviceMode, DeviceNames: THandle);
var
  DevNames: PDevNames;
begin
  if DeviceNames = 0 then
    Exit;

  DevNames := PDevNames(GlobalLock(DeviceNames));
  try
    with DevNames^ do
      Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
        PChar(DevNames) + wDriverOffset,
        PChar(DevNames) + wOutputOffset, DeviceMode);
  finally
    GlobalUnlock(DeviceNames);
    GlobalFree(DeviceNames);
  end;
end;

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

//=== { TJvPageSetupDialog } =================================================

constructor TJvPageSetupDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMargin := TJvMarginSize.Create;
  FMinMargin := TJvMarginSize.Create;
  Options := [poDefaultMinMargins, poHundredthsOfMillimeters];
end;

destructor TJvPageSetupDialog.Destroy;
begin
  FMargin.Free;
  FMinMargin.Free;
  inherited Destroy;
end;

// Determination of streamed properties

procedure TJvPageSetupDialog.DefineProperties(AFiler: TFiler);

  // Rule 1
  function DoWriteMargin1: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := not TJvPageSetupDialog(AFiler.Ancestor).FMargin.MarginsEqu(FMargin)
    else
      Result := (FMargin <> nil) and (not FMargin.IsNull);
  end;

  // Rule 2
  function DoWriteMargin2: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := not TJvPageSetupDialog(AFiler.Ancestor).FMinMargin.MarginsEqu(FMinMargin)
    else
      Result := (FMinMargin <> nil) and (not FMinMargin.IsNull);
  end;

begin
  inherited DefineProperties(AFiler);
  with AFiler do
  begin
    DefineProperty('MarginData', ReadValues, WriteValues, DoWriteMargin1);
    DefineProperty('MinMarginData', ReadMinValues, WriteMinValues, DoWriteMargin2);
  end;
end;

// Reading from stream

procedure TJvPageSetupDialog.ReadMargin(AMargin: TJvMarginSize; Reader: TReader);
begin
  with AMargin, Reader do
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

procedure TJvPageSetupDialog.WriteMargin(AMargin: TJvMarginSize; Writer: TWriter);
begin
  with AMargin, Writer do
  begin
    WriteListBegin;
    WriteInteger(Left);
    WriteInteger(Top);
    WriteInteger(Right);
    WriteInteger(Bottom);
    WriteListEnd;
  end;
end;

procedure TJvPageSetupDialog.ReadValues(AReader: TReader);
begin
  ReadMargin(FMargin, AReader);
end;

procedure TJvPageSetupDialog.WriteValues(AWriter: TWriter);
begin
  WriteMargin(FMargin, AWriter);
end;

procedure TJvPageSetupDialog.ReadMinValues(AReader: TReader);
begin
  ReadMargin(FMinMargin, AReader);
end;

procedure TJvPageSetupDialog.WriteMinValues(AWriter: TWriter);
begin
  WriteMargin(FMinMargin, AWriter);
end;

// Processing Help commands

procedure TJvPageSetupDialog.WMHelp(var Msg: TWMHelp);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext)
  else
    inherited;
end;

// Processing <Printer> button

procedure TJvPageSetupDialog.WMCommand(var Msg: TWMCommand);
const
  IDPRINTERBTN = $0402;
begin
  if not ((Msg.ItemID = IDPRINTERBTN) and
    (Msg.NotifyCode = BN_CLICKED) and DoPrinter) then
    inherited;
end;

procedure TJvPageSetupDialog.WMPaintInit(var Msg: TMessage);
begin
  FInitPaper := LoWord(Msg.WParam);
  FInitFlags := HiWord(Msg.WParam);
  FPageSetupRec := PPageSetupDlg(Msg.LParam)^;
  Msg.Result := Ord(not Assigned(FOnPaint));
end;

procedure TJvPageSetupDialog.WMPaintPage(var Msg: TMessage);
var
  PaintRect: TRect;
  Canvas: TCanvas;
begin
  if Msg.LParam <> 0 then
    PaintRect := PRect(Msg.LParam)^
  else
    PaintRect := Rect(0, 0, 0, 0);

  Canvas := TCanvas.Create;
  Canvas.Handle := HDC(Msg.WParam);
  try
    Msg.Result := Ord(DoPaint(FInitPaper, FInitFlags, FPageSetupRec,
      FPaintWhat, Canvas, PaintRect));
  finally
    Canvas.Free;
  end;
end;

function TJvPageSetupDialog.DoPrinter: Boolean;
begin
  Result := Assigned(FOnPrinter);
  if Result then
    FOnPrinter(Self);
end;

function TJvPageSetupDialog.DoPaint(InitPaper, InitFlags: Integer;
  PageSetupRec: TPageSetupDlg; PaintWhat: TJvPSPaintWhat; Canvas: TCanvas;
  Rect: TRect): Boolean;
begin
  Result := False;
  if Assigned(FOnPaint) then
    FOnPaint(Self, InitPaper, InitFlags, PageSetupRec, PaintWhat, Canvas, Rect, Result);
end;

// Show modal dialog

function TJvPageSetupDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
type
  TDialogFunc = function(var ADialogData): Bool; stdcall;
var
  ActiveWindow: HWND;
  WindowList: Pointer;
  FPUControlWord: Word;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    Application.HookMainWindow(MessageHook);
    asm
      // Avoid FPU control word change in NETRAP.dll, NETAPI32.dll, etc
      FNSTCW  FPUControlWord
    end;
    try
      CreationControl := Self;
      PageSetupControl := Self;
      Result := TDialogFunc(DialogFunc)(DialogData);
    finally
      PageSetupControl := nil;
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
      Application.UnhookMainWindow(MessageHook);
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function TJvPageSetupDialog.DoExecute(Show: Boolean): Boolean;
var
  PageDlgRec: TPageSetupDlg;
  DevHandle: THandle;
  Err: Integer;
begin
  // fill record
  FillChar(PageDlgRec, SizeOf(PageDlgRec), 0);
  with PageDlgRec do
  begin
    lStructSize := SizeOf(PageDlgRec);
    hwndOwner := Application.Handle;
    Flags := FFlags;
    rtMinMargin := Rect(FMinMargin.Left, FMinMargin.Top, FMinMargin.Right,
      FMinMargin.Bottom);
    rtMargin := Rect(FMargin.Left, FMargin.Top, FMargin.Right, FMargin.Bottom);
    hInstance := SysInit.HInstance;
    if Show then
    begin
      lpfnPageSetupHook := DialogHook;
      Flags := FFlags or PSD_ENABLEPAGESETUPHOOK;
      GetPrinter(DevHandle, hDevNames);
      hDevMode := CopyData(DevHandle);
    end
    else
      Flags := Flags or PSD_RETURNDEFAULT;
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

    if Show then
      Result := TaskModalDialog(@PageSetupDlg, PageDlgRec)
    else
      Result := PageSetupDlg(PageDlgRec);
    Err := CommDlgExtendedError;

    if Result then
      SetPrinter(hDevMode, hDevNames)
    else
    begin
      if hDevMode <> 0 then
        GlobalFree(hDevMode);
      if hDevNames <> 0 then
        GlobalFree(hDevNames);
    end;
    OSCheck(Err = 0);

    FMargin.AsRect := rtMargin;
    FPaperSize := ptPaperSize;
  end;
end;

function TJvPageSetupDialog.Execute: Boolean;
begin
  Result := DoExecute(True);
end;

// Get default margin values

procedure TJvPageSetupDialog.GetDefaults;
begin
  DoExecute(False);
end;

procedure TJvPageSetupDialog.SetOptions(Value: TJvPageOptions);
const
  WinFlags: array [TJvPageSetupFlags] of DWORD =
    (PSD_DEFAULTMINMARGINS, PSD_MARGINS, PSD_MINMARGINS,
    PSD_DISABLEMARGINS, PSD_DISABLEORIENTATION,
    PSD_DISABLEPAGEPAINTING, PSD_DISABLEPAPER, PSD_DISABLEPRINTER,
    PSD_INHUNDREDTHSOFMILLIMETERS, PSD_INTHOUSANDTHSOFINCHES,
    PSD_NOWARNING);
var
  I: TJvPageSetupFlags;
begin
  if (poDefaultMinMargins in Value) and not (poDefaultMinMargins in FOptions) then
    Value := Value - [poMinMargins];
  if (poMinMargins in Value) and not (poMinMargins in FOptions) then
    Value := Value - [poDefaultMinMargins];
  if (poHundredthsOfMillimeters in Value) and not (poHundredthsOfMillimeters in FOptions) then
    Value := Value - [poThousandthsOfInches];
  if (poThousandthsOfInches in Value) and not (poThousandthsOfInches in FOptions) then
    Value := Value - [poHundredthsOfMillimeters];
  FOptions := Value;

  // set flags
  FFlags := 0;
  for I := Low(TJvPageSetupFlags) to High(TJvPageSetupFlags) do
    if I in FOptions then
      FFlags := FFlags or WinFlags[I];
end;

end.

