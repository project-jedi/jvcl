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
  Windows, Messages,
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer;

type
  TDlgCode = (
    dcWantAllKeys, dcWantArrows, dcWantChars, dcButton, dcHasSetSel, dcWantTab,
    dcNative // if dcNative is in the set the native allowed keys are used and GetDlgCode is ignored
  );
  TDlgCodes = set of TDlgCode;

const
  dcWantMessage = dcWantAllKeys;

const
  CM_DENYSUBCLASSING = JvThemes.CM_DENYSUBCLASSING;
  {$IFDEF VisualCLX}
  CM_PERFORM = CM_BASE + $500 + 0; // LParam: "Msg: ^TMessage"
  {$ENDIF VisualCLX}
  CM_SETAUTOSIZE = CM_BASE + $500 + 1; // WParam: "Value: Boolean"

type
  { IJvExControl is used for the identification of an JvExXxx control. }
  IJvExControl = interface
    ['{8E6579C3-D683-4562-AFAB-D23C8526E386}']
  end;

  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;

function ShiftStateToKeyData(Shift: TShiftState): Longint;
function GetFocusedControl(AControl: TControl): TWinControl;
function DlgcToDlgCodes(Value: Longint): TDlgCodes;
function DlgCodesToDlgc(Value: TDlgCodes): Longint;
procedure GetHintColor(var HintInfo: THintInfo; AControl: TControl; HintColor: TColor);
{$IFDEF COMPILER5}
procedure TOpenControl_SetAutoSize(AControl: TControl; Value: Boolean);
{$ENDIF COMPILER5}

{$IFDEF VisualCLX}
function Perform(AControl: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
{$ENDIF VisualCLX}

{******************************************************************************}

type
  CONTROL_DECL_DEFAULT(Control)
  WINCONTROL_DECL_DEFAULT(WinControl)
  WINCONTROL_DECL_DEFAULT(CustomControl)
  CONTROL_DECL_DEFAULT(GraphicControl)
  WINCONTROL_DECL_DEFAULT(HintWindow)

  TJvExPubGraphicControl = class(TJvExGraphicControl)
  COMMON_PUBLISHED
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
  CtrlMask = $10000000;
  ShiftMask = $08000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
  if ssCtrl in Shift then
    Result := Result or CtrlMask;
  if ssShift in Shift then
    Result := Result or ShiftMask;
end;

function GetFocusedControl(AControl: TControl): TWinControl;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetParentForm(AControl);
  if Assigned(Form) then
    {$IFDEF VCL}
    Result := Form.ActiveControl;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Result := Form.FocusedControl;
    {$ENDIF VisualCLX}
end;

function DlgcToDlgCodes(Value: Longint): TDlgCodes;
begin
  Result := [];
  if Value and DLGC_WANTARROWS <> 0 then
    Include(Result, dcWantArrows);
  if Value and DLGC_WANTTAB <> 0 then
    Include(Result, dcWantTab);
  if Value and DLGC_WANTALLKEYS <> 0 then
    Include(Result, dcWantAllKeys);
  if Value and DLGC_WANTCHARS <> 0 then
    Include(Result, dcWantChars);
  if Value and DLGC_BUTTON <> 0 then
    Include(Result, dcButton);
  if Value and DLGC_HASSETSEL <> 0 then
    Include(Result, dcHasSetSel);
end;

function DlgCodesToDlgc(Value: TDlgCodes): Longint;
begin
  Result := 0;
  if dcWantAllKeys in Value then
    Result := Result or DLGC_WANTALLKEYS;
  if dcWantArrows in Value then
    Result := Result or DLGC_WANTARROWS;
  if dcWantTab in Value then
    Result := Result or DLGC_WANTTAB;
  if dcWantChars in Value then
    Result := Result or DLGC_WANTCHARS;
  if dcButton in Value then
    Result := Result or DLGC_BUTTON;
  if dcHasSetSel in Value then
    Result := Result or DLGC_HASSETSEL;
end;

procedure GetHintColor(var HintInfo: THintInfo; AControl: TControl; HintColor: TColor);
var
  AHintInfo: THintInfo;
begin
  case HintColor of
    clNone:
      HintInfo.HintColor := Application.HintColor;
    clDefault:
      begin
        if Assigned(AControl) and Assigned(AControl.Parent) then
        begin
          AHintInfo := HintInfo;
          {$IFDEF VCL}
          AControl.Parent.Perform(CM_HINTSHOW, 0, Integer(@AHintInfo));
          {$ENDIF VCL}
          {$IFDEF VisualCLX}
          Perform(AControl.Parent, CM_HINTSHOW, 0, Integer(@AHintInfo));
          {$ENDIF VisualCLX}
          HintInfo.HintColor := AHintInfo.HintColor;
        end;
      end;
  else
    HintInfo.HintColor := HintColor;
  end;
end;

{$IFDEF VisualCLX}
function Perform(AControl: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
var
  PerformMsg, Mesg: TMessage;
begin
  if AControl.GetInterfaceEntry(IJvExControl) <> nil then
  begin
    Mesg.Msg := Msg;
    Mesg.WParam := WParam;
    Mesg.LParam := LParam;
    Mesg.Result := 0;

    PerformMsg.Msg := CM_PERFORM;
    PerformMsg.WParam := 0;
    PerformMsg.LParam := @Mesg;
    PerformMsg.Result := 0;
    AControl.Dispatch(PerformMsg);
  end;
end;
{$ENDIF VisualCLX}

// *****************************************************************************

{$IFDEF COMPILER5}

{ Delphi 5's SetAutoSize is private and not virtual. This code installs a
  JUMP-Hook into SetAutoSize that jumps to our function. }
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

var
  AutoSizeOffset: Cardinal;
  TControl_SetAutoSize: Pointer;

type
  TControlAccessProtected = class(TControl);

procedure OrgSetAutoSize(AControl: TControl; Value: Boolean);
asm
        DD    0, 0, 0, 0  // 16 Bytes
end;

procedure TOpenControl_SetAutoSize(AControl: TControl; Value: Boolean);
begin
  // same as OrgSetAutoSize(AControl, Value); but secure
  with TControlAccessProtected(AControl) do
  begin
    if AutoSize <> Value then
    begin
      PBoolean(Cardinal(AControl) + AutoSizeOffset)^ := Value;
      if Value then
        AdjustSize;
    end;
  end;
end;

procedure SetAutoSizeHook(AControl: TControl; Value: Boolean);
var
  Msg: TMessage;
begin
  if AControl.GetInterfaceEntry(IJvExControl) <> nil then
  begin
    Msg.Msg := CM_SETAUTOSIZE;
    Msg.WParam := Ord(Value);
    AControl.Dispatch(Msg);
  end
  else
    TOpenControl_SetAutoSize(AControl, Value);
end;

{$OPTIMIZATION ON} // be sure to have optimization activated
function GetCode(AControl: TControlAccessProtected): Boolean; register;
begin
  { generated code:
      8A40xx       mov al,[eax+Byte(Offset)]
  }
  Result := AControl.AutoSize;
end;

procedure SetCode(AControl: TControlAccessProtected); register;
begin
  { generated code:
      B201         mov dl,$01
      E8xxxxxxxx   call TControl.SetAutoSize
  }
  AControl.AutoSize := True;
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

// *****************************************************************************

CONTROL_IMPL_DEFAULT(Control)
WINCONTROL_IMPL_DEFAULT(WinControl)
CONTROL_IMPL_DEFAULT(GraphicControl)
WINCONTROL_IMPL_DEFAULT(CustomControl)
WINCONTROL_IMPL_DEFAULT(HintWindow)


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF COMPILER5}
  InitHookVars;
  InstallProcHook(TControl_SetAutoSize, @SetAutoSizeHook, @OrgSetAutoSize);
  {$ENDIF COMPILER5}

finalization
  {$IFDEF COMPILER5}
  UninstallProcHook(@OrgSetAutoSize);
  {$ENDIF COMPILER5}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

