{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExControls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExControls;

{$I jvcl.inc}

WARNINGHEADER

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Messages, Graphics, Controls, Forms,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer;

 {$DEFINE NeedMouseEnterLeave}

type
  TDlgCode = (
    dcWantAllKeys, dcWantArrows, dcWantTab, dcWantChars,
    dcButton,
    dcNative // if dcNative is in the set the native functions are used and DoGetDlgCode is ignored
  );
  TDlgCodes = set of TDlgCode;


const
  dcWantMessage = dcWantAllKeys;

type
  IPerformControl = interface
    ['{B11AA73D-D7C2-43E5-BED8-8F82DE6152AB}']
    function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint;
  end;

  IJvControlEvents = interface(IPerformControl)
    ['{61FC57FF-D4DA-4840-B871-63DE804E9921}']
    procedure VisibleChanged;
    procedure EnabledChanged;
    procedure TextChanged;
    procedure FontChanged;
    procedure ColorChanged;
    procedure ParentFontChanged;
    procedure ParentColorChanged;
    procedure ParentShowHintChanged;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; // CM_DIALOGCHAR
    function HintShow(var HintInfo: THintInfo): Boolean;
    function HitTest(X, Y: Integer): Boolean; // CM_HITTEST
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    procedure DoFocusChanged(Control: TWinControl);
    procedure SetAutoSize(Value: Boolean);
  end;

  IJvWinControlEvents = interface(
    {$IFDEF BCB}
    IPerformControl
    {$ELSE}
    IJvControlEvents
    {$ENDIF BCB}
    )
    ['{B5F7FB62-78F0-481D-AFF4-7A24ED6776A0}']
    procedure DoBoundsChanged;
    procedure CursorChanged;
    procedure ShowingChanged;
    procedure ShowHintChanged;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean);
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean);
    procedure DoGetDlgCode(var Code: TDlgCodes); // WM_GETDLGCODE
    procedure DoSetFocus(FocusedWnd: HWND);  // WM_SETFOCUS
    procedure DoKillFocus(FocusedWnd: HWND); // WM_KILLFOCUS
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; // WM_ERASEBKGND
  end;

  IJvEditControlEvents = interface(IPerformControl)
    ['{C1AE5EF8-F6C4-4BD4-879E-17946FD0FBAB}']
    procedure DoClipboardPaste;
    procedure DoClipboardCopy;
    procedure DoClipboardCut;
    procedure DoUndo;
    procedure DoClearText;
  end;

const
  CM_DENYSUBCLASSING = JvThemes.CM_DENYSUBCLASSING;

type
  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;


type
  JV_CONTROL_EVENTS(Control)
  JV_WINCONTROL_EVENTS(WinControl)
  JV_CONTROL_EVENTS(GraphicControl)
  JV_WINCONTROL_EVENTS(CustomControl)
  JV_WINCONTROL_EVENTS(HintWindow)

function ShiftStateToKeyData(Shift: TShiftState): Longint;

function InheritMsgEx(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer; overload;
function InheritMsg(Instance: TControl; Msg: Integer): Integer; overload;
procedure InheritMessage(Instance: TControl; var Msg: TMessage); overload;
procedure DispatchMsg(Instance: TControl; var Msg);

// jump targets:

procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);

{$IFDEF COMPILER5}
procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
{$ENDIF COMPILER5}

procedure Control_MouseEnter(Instance, Control: TControl; var FMouseOver: Boolean;
  var FSavedHintColor: TColor; FHintColor: TColor; var Event: TNotifyEvent);

procedure Control_MouseLeave(Instance, Control: TControl; var FMouseOver: Boolean;
  var FSavedHintColor: TColor; var Event: TNotifyEvent);

function DefaultDoPaintBackground(Instance: TWinControl; Canvas: TCanvas; Param: Integer): Boolean;

procedure TCustomEdit_Undo(Instance: TWinControl);
procedure TCustomEdit_Copy(Instance: TWinControl);
procedure TCustomEdit_Paste(Instance: TWinControl);
procedure TCustomEdit_Cut(Instance: TWinControl);

implementation

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
end;

type
  PInterface = ^IInterface;

  TFreeNotificationHelper = class(TComponent)
  private
    FInstance: TComponent;
    FIntfPtr: PInterface;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AInstance: TComponent; AIntfPtr: PInterface); reintroduce;
    destructor Destroy; override;
    function IsValid: Boolean;
  end;

constructor TFreeNotificationHelper.Create(AInstance: TComponent; AIntfPtr: PInterface);
begin
  inherited Create(nil);
  FIntfPtr := AIntfPtr;
  if csDestroying in AInstance.ComponentState then
    FInstance := nil
  else
  begin
    FInstance := AInstance;
    FInstance.FreeNotification(Self);
  end;
end;

destructor TFreeNotificationHelper.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TFreeNotificationHelper.IsValid: Boolean;
begin
  Result := FIntfPtr <> nil;
end;

procedure TFreeNotificationHelper.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (Component = FInstance) then
  begin
   // (ahuser) The component destroys the whole list so the following line could
   //          be removed (but who knowns what the Delphi IDE will do without
   //          this line.
    FInstance.RemoveFreeNotification(Self);
    FInstance := nil;
    FIntfPtr^ := nil;
    FIntfPtr := nil;
  end;
end;

function InheritMsgEx(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
var
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
  InheritMessage(Instance, Mesg);
  Result := Mesg.Result;
end;

function InheritMsg(Instance: TControl; Msg: Integer): Integer;
begin
  Result := InheritMsgEx(Instance, Msg, 0, 0);
end;

procedure InheritMessage(Instance: TControl; var Msg: TMessage);
type
  TDispatchMethod = procedure(Self: TObject; var Msg: TMessage);
var
  Proc: TDispatchMethod;
begin
  Proc := @TObject.Dispatch;
  Proc(Instance, Msg);
end;

procedure DispatchMsg(Instance: TControl; var Msg);
var
  Temp: IJvDenySubClassing;
  IntfControl: IJvControlEvents;
  IntfWinControl: IJvWinControlEvents;
  IntfEditControl: IJvEditControlEvents;
  PMsg: PMessage;
  CallInherited: Boolean;
  Canvas: TCanvas;
  DlgCodes: TDlgCodes;
  IdSaveDC: Integer;
  Helper: TFreeNotificationHelper;
begin
  CallInherited := True;
  PMsg := @Msg;

  if PMsg^.Msg = CM_DENYSUBCLASSING then
  begin
    PMsg^.Result := Ord(Instance.GetInterface(IJvDenySubClassing, Temp));
    Temp := nil; // does not destroy the control because it is derived from TComponent
   // Let the control handle CM_DENYSUBCLASSING the old way, too.
  end;

  { GetInterface is no problem because Instance is a TComponent derived class that
    is not released by an interface "Release". }
  if Instance.GetInterface(IJvControlEvents, IntfControl) then
  begin
    CallInherited := False;
    try
      with IntfControl do
        case PMsg^.Msg of
          CM_VISIBLECHANGED:
            VisibleChanged;
          CM_ENABLEDCHANGED:
            EnabledChanged;
          CM_FONTCHANGED:
            FontChanged;
          CM_COLORCHANGED:
            ColorChanged;
          CM_PARENTFONTCHANGED:
            ParentFontChanged;
          CM_PARENTCOLORCHANGED:
            ParentColorChanged;
          CM_PARENTSHOWHINTCHANGED:
            ParentShowHintChanged;
          CM_TEXTCHANGED:
            TextChanged;
          CM_HINTSHOW:
            PMsg^.Result := Integer(HintShow(TCMHintShow(PMsg^).HintInfo^));
          CM_HITTEST:
            with TCMHitTest(PMsg^) do
              Result := Integer(HitTest(XPos, YPos));
          CM_MOUSEENTER:
              MouseEnter(TControl(PMsg^.LParam));
          CM_MOUSELEAVE:
              MouseLeave(TControl(PMsg^.LParam));
          CM_DIALOGCHAR:
            with TCMDialogChar(PMsg^) do
              Result := Ord(WantKey(CharCode, KeyDataToShiftState(KeyData), WideChar(CharCode)));
          // CM_FOCUSCHANGED: handled by a message handler in the JvExVCL classes
        else
          CallInherited := True;
        end;
    finally
      IntfControl := nil;
    end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvWinControlEvents, IntfWinControl) then
    begin
      CallInherited := False;
      try
        with IntfWinControl do
          case PMsg^.Msg of
            CM_CURSORCHANGED:
              CursorChanged;
            CM_SHOWINGCHANGED:
              ShowingChanged;
            CM_SHOWHINTCHANGED:
              ShowHintChanged;
            CM_CONTROLLISTCHANGE:
              if PMsg^.LParam <> 0 then
                ControlsListChanging(TControl(PMsg^.WParam), True)
              else
                ControlsListChanged(TControl(PMsg^.WParam), False);
            CM_CONTROLCHANGE:
              if PMsg^.LParam = 0 then
                ControlsListChanging(TControl(PMsg^.WParam), False)
              else
                ControlsListChanged(TControl(PMsg^.WParam), True);
            WM_GETDLGCODE:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  InheritMessage(Instance, PMsg^);

                  DlgCodes := [dcNative];
                  if PMsg^.Result and DLGC_WANTARROWS <> 0 then
                    Include(DlgCodes, dcWantArrows);
                  if PMsg^.Result and DLGC_WANTTAB <> 0 then
                    Include(DlgCodes, dcWantTab);
                  if PMsg^.Result and DLGC_WANTALLKEYS <> 0 then
                    Include(DlgCodes, dcWantAllKeys);
                  if PMsg^.Result and DLGC_WANTCHARS <> 0 then
                    Include(DlgCodes, dcWantChars);
                  if PMsg^.Result and DLGC_BUTTON <> 0 then
                    Include(DlgCodes, dcButton);

                  if Helper.IsValid then
                  begin
                    DoGetDlgCode(DlgCodes);

                    if not (dcNative in DlgCodes) then
                    begin
                      PMsg^.Result := 0;
                      if dcWantAllKeys in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTALLKEYS;
                      if dcWantArrows in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTARROWS;
                      if dcWantTab in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTTAB;
                      if dcWantChars in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTCHARS;
                      if dcButton in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_BUTTON;
                    end;
                  end;
                finally
                  Helper.Free;
                end;
              end;
            WM_SETFOCUS:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  InheritMessage(Instance, PMsg^);
                  if Helper.IsValid then
                    DoSetFocus(HWND(PMsg^.WParam));
                finally
                  Helper.Free;
                end;
              end;
            WM_KILLFOCUS:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  InheritMessage(Instance, PMsg^);
                  if Helper.IsValid then
                    DoKillFocus(HWND(PMsg^.WParam));
                finally
                  Helper.Free;
                end;
              end;
            WM_SIZE:
              begin
                DoBoundsChanged;
                IntfWinControl := nil;
                InheritMessage(Instance, PMsg^);
              end;
            WM_ERASEBKGND:
              begin
                IdSaveDC := SaveDC(HDC(PMsg^.WParam)); // protect DC against Stock-Objects from Canvas
                Canvas := TCanvas.Create;
                try
                  Canvas.Handle := HDC(PMsg^.WParam);
                  PMsg^.Result := Ord(DoPaintBackground(Canvas, PMsg^.LParam));
                finally
                  Canvas.Handle := 0;
                  Canvas.Free;
                  RestoreDC(HDC(PMsg^.WParam), IdSaveDC);
                end;
              end;
            WM_PRINTCLIENT,
            WM_PRINT:
              begin
                IdSaveDC := SaveDC(HDC(PMsg^.WParam)); // protect DC against changes
                try
                  InheritMessage(Instance, PMsg^);
                finally
                  RestoreDC(HDC(PMsg^.WParam), IdSaveDC);
                end;
              end;
          else
            CallInherited := True;
        end;
      finally
        IntfWinControl := nil;
      end;
    end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvEditControlEvents, IntfEditControl) then
    begin
      CallInherited := False;
      try
        with IntfEditControl do
          case PMsg^.Msg of
            WM_PASTE:
              begin
                DoClipboardPaste;
                PMsg^.Result := 1;
              end;
            WM_COPY:
              begin
                // The PSDK documentation says that WM_COPY does not has a result
                // value. This is wrong. WM_COPY returns the number of chars that
                // were copied to the clipboard. Unfortunatelly does the CLX methods
                // have no return value and so return 1. If we do not do this
                // WM_CUT will not work.
                DoClipboardCopy;
                PMsg^.Result := 1;
              end;
            WM_CUT:
              begin
                DoClipboardCut;
                PMsg^.Result := 1;
              end;
            WM_UNDO, EM_UNDO:
              begin
                DoUndo;
                PMsg^.Result := 1;
              end;
            WM_CLEAR:
              begin
                DoClearText;
                PMsg^.Result := 1;
              end;
          else
            CallInherited := True;
          end;
      finally
        IntfEditControl := nil;
      end;
    end;
  end;

  if CallInherited then
    InheritMessage(Instance, PMsg^);
end;

{ VCL sends CM_CONTROLLISTCHANGE and CM_CONTROLCHANGE in an other order than
  the CLX methods are used. So we must correct it by evaluating "Inserting". }
procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if Inserting then
    InheritMsgEx(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsgEx(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if not Inserting then
    InheritMsgEx(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsgEx(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure Control_MouseEnter(Instance, Control: TControl; var FMouseOver: Boolean;
  var FSavedHintColor: TColor; FHintColor: TColor; var Event: TNotifyEvent);
begin
  // (HEG) VCL: Control is nil iff Instance is the control that the mouse has left.
  // Otherwise this is just a notification that the mouse entered
  // one of its child controls
  if (Control = nil) and not FMouseOver and not (csDesigning in Instance.ComponentState) then
  begin
    FMouseOver := True;
    FSavedHintColor := Application.HintColor;
    if FHintColor <> clNone then
      Application.HintColor := FHintColor;
    if Assigned(Event) then
      Event(Instance);
  end;
  InheritMsgEx(Instance, CM_MOUSEENTER, 0, Integer(Control));
end;

procedure Control_MouseLeave(Instance, Control: TControl; var FMouseOver: Boolean;
  var FSavedHintColor: TColor; var Event: TNotifyEvent);
begin
  // (HEG) Control is nil iff Instance is the control that the mouse has left.
  // Otherwise this is just a notification that the mouse left
  // one of its child controls
  if (Control = nil) and FMouseOver and not (csDesigning in Instance.ComponentState) then
  begin
    FMouseOver := False;
    Application.HintColor := FSavedHintColor;
    if Assigned(Event) then
      Event(Instance);
  end;
  InheritMsgEx(Instance, CM_MOUSELEAVE, 0, Integer(Control));
end;

function DefaultDoPaintBackground(Instance: TWinControl; Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := InheritMsgEx(Instance, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
end;

type
  TControlAccessProtected = class(TControl);

procedure TCustomEdit_Undo(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_UNDO);
end;

procedure TCustomEdit_Copy(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_COPY);
end;

procedure TCustomEdit_Cut(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_CUT);
end;

procedure TCustomEdit_Paste(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_PASTE);
end;

// *****************************************************************************

JV_CONTROL_EVENTS_IMPL(Control)
JV_WINCONTROL_EVENTS_IMPL(WinControl)
JV_CONTROL_EVENTS_IMPL(GraphicControl)
JV_WINCONTROL_EVENTS_IMPL(CustomControl)
JV_WINCONTROL_EVENTS_IMPL(HintWindow)


// *****************************************************************************

type
  PBoolean = ^Boolean;
  PPointer = ^Pointer;

function ReadProtectedMemory(Address: Pointer; var Buffer; Count: Cardinal): Boolean;
var
  n: Cardinal;
begin
  Result := ReadProcessMemory(GetCurrentProcess, Address, @Buffer, Count, n);
  Result := Result and (n = Count);
end;

function WriteProtectedMemory(Address: Pointer; const Buffer; Count: Cardinal): Boolean;
var
  n: Cardinal;
begin
  Result := WriteProcessMemory(GetCurrentProcess, Address, @Buffer, Count, n);
  Result := Result and (n = Count);
end;

type
  TJumpCode = packed record
    Pop: Byte; // pop xxx
    Jmp: Byte; // jmp Offset
    Offset: Integer;
  end;

  TOrgCallCode = packed record
    Push: Byte; // push ebx/ebp
    InjectedCode: TJumpCode;
    Jmp: Byte; // jmp Offset
    Offset: Integer;
    Address: Pointer;
  end;

function GetRelocAddress(ProcAddress: Pointer): Pointer;
type
  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;
var
  Relocation: TRelocationRec;
  Data: Byte;
begin
  Result := ProcAddress;
 // the relocation table meight be protected
  if ReadProtectedMemory(ProcAddress, Data, SizeOf(Data)) then
  begin
    if Data = $FF then // ProcAddress is in a dll or package
      if ReadProtectedMemory(ProcAddress, Relocation, SizeOf(Relocation)) then
        Result := Relocation.Address^;
  end;
end;

function InstallProcHook(ProcAddress, HookProc, OrgCallProc: Pointer): Boolean;
var
  Code: TJumpCode;
  OrgCallCode: TOrgCallCode;
begin
  ProcAddress := GetRelocAddress(ProcAddress);
  Result := False;
  if Assigned(ProcAddress) and Assigned(HookProc) then
  begin
    if OrgCallProc <> nil then
    begin
      if ReadProtectedMemory(ProcAddress, OrgCallCode, SizeOf(OrgCallCode) - (1 + SizeOf(Integer))) then
      begin
        OrgCallCode.Jmp := $E9;
        OrgCallCode.Offset := (Integer(ProcAddress) + 1 + SizeOf(Code)) -
          Integer(OrgCallProc) -
          (SizeOf(OrgCallCode) - SizeOf(OrgCallCode.Address));
        OrgCallCode.Address := ProcAddress;

        WriteProtectedMemory(OrgCallProc, OrgCallCode, SizeOf(OrgCallCode));
        FlushInstructionCache(GetCurrentProcess, OrgCallProc, SizeOf(OrgCallCode));
      end;
    end;

    if PByte(ProcAddress)^ = $53 then // push ebx
      Code.Pop := $5B // pop ebx
    else
    if PByte(ProcAddress)^ = $55 then // push ebp
      Code.Pop := $5D // pop ebp
    else
      Exit;
    Code.Jmp := $E9;
    Code.Offset := Integer(HookProc) - (Integer(ProcAddress) + 1) - SizeOf(Code);

    { The strange thing is that something overwrites the $e9 with a "PUSH xxx" }
    if WriteProtectedMemory(Pointer(Cardinal(ProcAddress) + 1), Code, SizeOf(Code)) then
    begin
      FlushInstructionCache(GetCurrentProcess, ProcAddress, SizeOf(Code));
      Result := True;
    end;
  end;
end;

function UninstallProcHook(OrgCallProc: Pointer): Boolean;
var
  OrgCallCode: TOrgCallCode;
  ProcAddress: Pointer;
begin
  Result := False;
  if Assigned(OrgCallProc) then
  begin
    if OrgCallProc <> nil then
    begin
      if ReadProtectedMemory(OrgCallProc, OrgCallCode, SizeOf(OrgCallCode)) then
      begin
        ProcAddress := OrgCallCode.Address;

        Result := WriteProtectedMemory(ProcAddress, OrgCallCode, 1 + SizeOf(TJumpCode));
        FlushInstructionCache(GetCurrentProcess, ProcAddress, SizeOf(OrgCallCode));
      end;
    end;
  end;
end;

{$IFDEF COMPILER5}
var
  AutoSizeOffset: Cardinal;
  TControl_SetAutoSize: Pointer;

procedure OrgSetAutoSize(Instance: TControl; Value: Boolean);
asm
        DD    0, 0, 0, 0  // 16 Bytes
end;

procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
begin
  with TControlAccessProtected(Instance) do
  begin
    if AutoSize <> Value then
    begin
      PBoolean(Cardinal(Instance) + AutoSizeOffset)^ := Value;
      if Value then
        AdjustSize;
    end;
  end;
  // same as OrgSetAutoSize(Instance, Value); but secure
end;

procedure SetAutoSizeHook(Instance: TControl; Value: Boolean);
var
  IntfControl: IJvControlEvents;
begin
  if Instance.GetInterface(IJvControlEvents, IntfControl) then
    IntfControl.SetAutoSize(Value)
  else
    TOpenControl_SetAutoSize(Instance, Value);
end;

{$OPTIMIZATION ON} // be sure to have optimization activated
function GetCode(Instance: TControlAccessProtected): Boolean; register;
begin
  { generated code:
      8A40xx       mov al,[eax+Byte(Offset)]
  }
  Result := Instance.AutoSize;
end;

procedure SetCode(Instance: TControlAccessProtected); register;
begin
  { generated code:
      B201         mov dl,$01
      E8xxxxxxxx   call TControl.SetAutoSize
  }
  Instance.AutoSize := True;
end;

type
  PGetCodeRec = ^TGetCodeRec;
  TGetCodeRec = packed record
    Sign: Word; // $408a   bytes swapped
    Offset: Byte;
  end;

type
  PSetCodeRec = ^TSetCodeRec;
  TSetCodeRec = packed record
    Sign1: Word; // $01b2  bytes swapped
    Sign2: Byte; // $e8
    Offset: Integer;
  end;

const
  GetCodeSign = $408a;
  SetCodeSign1 = $01b2;
  SetCodeSign2 = $e8;

procedure InitHookVars;
var
  PGetCode: PGetCodeRec;
  PSetCode: PSetCodeRec;
begin
  TControl_SetAutoSize := nil;
  AutoSizeOffset := 0;

  PGetCode := @GetCode;
  PSetCode := @SetCode;

  if (PGetCode^.Sign = GetCodeSign) and
    (PSetCode^.Sign1 = SetCodeSign1) and
    (PSetCode^.Sign2 = SetCodeSign2) then
  begin
    AutoSizeOffset := PGetCode^.Offset;
    TControl_SetAutoSize := GetRelocAddress(
      Pointer(Integer(@SetCode) + SizeOf(TSetCodeRec) + PSetCode^.Offset));
  end;
end;

{$ENDIF COMPILER5}



initialization
  {$IFDEF COMPILER5}
  InitHookVars;
  InstallProcHook(TControl_SetAutoSize, @SetAutoSizeHook, @OrgSetAutoSize);
  {$ENDIF COMPILER5}


finalization
  {$IFDEF COMPILER5}
  UninstallProcHook(@OrgSetAutoSize);
  {$ENDIF COMPILER5}

end.

