{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExControls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExControls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QWindows, QForms,  // order: QControls, QWindows
  {$ENDIF VisualCLX}
  Classes, SysUtils;

type
  IJvControlEvents = interface
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
      const KeyText: WideString): Boolean;
    function HintShow(var HintInfo : THintInfo): Boolean;
    function HitTest(X, Y: Integer): Boolean;
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    {$IFDEF VCL}
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF VCL}
  end;

  IJvWinControlEvents = interface(IJvControlEvents)
    ['{B5F7FB62-78F0-481D-AFF4-7A24ED6776A0}']
    procedure CursorChanged;
    procedure ShowingChanged;
    procedure ShowHintChanged;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean);
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean);
  end;

const
  CM_DENYSUBCLASSING = CM_BASE + 2000; // from JvThemes.pas

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
  JV_CUSTOMCONTROL_EVENTS(CustomControl)
  JV_CUSTOMCONTROL_EVENTS(HintWindow)

{$IFDEF VCL}

function ShiftStateToKeyData(Shift: TShiftState): Longint;

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer; overload;
function InheritMsg(Instance: TControl; Msg: Integer): Integer; overload;
procedure DispatchMsg(Instance: TControl; var Msg);

{$IFNDEF COMPILER6_UP}
procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
{$ENDIF !COMPILER6_UP}
{$ENDIF VCL}

implementation

{$IFDEF VCL}

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
end;

type
  TDisptachMethod = procedure(Self: TObject; var Msg: TMessage);

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
var
  Proc: TDisptachMethod;
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
  Proc := @TObject.Dispatch;
  Proc(Instance, Mesg);
  Result := Mesg.Result;
end;

function InheritMsg(Instance: TControl; Msg: Integer): Integer;
begin
  Result := InheritMsg(Instance, Msg, 0, 0);
end;

procedure DispatchMsg(Instance: TControl; var Msg);
var
  Temp: IJvDenySubClassing;
  IntfControl: IJvControlEvents;
  IntfWinControl: IJvWinControlEvents;
  PMsg: PMessage;
  CallInherited: Boolean;
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
      else
        CallInherited := True;
      end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvWinControlEvents, IntfWinControl) then
    begin
      CallInherited := False;
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
        else
          CallInherited := True;
        end;
    end;
  end;

  if CallInherited then
    PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);
end;

{$ENDIF VCL}

// *****************************************************************************

JV_CONTROL_EVENTS_IMPL(Control)
JV_WINCONTROL_EVENTS_IMPL(WinControl)

JV_CONTROL_EVENTS_IMPL(GraphicControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(HintWindow)

{$IFNDEF COMPILER6_UP}
var
  AutoSizeOffset: Cardinal;
  TControl_SetAutoSize: Pointer;

type
  TOpenControl = class(TControl);
  PBoolean = ^Boolean;
  PPointer = ^Pointer;

procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
begin
  with TOpenControl(Instance) do
  begin
    if AutoSize <> Value then
    begin
      PBoolean(Cardinal(Instance) + AutoSizeOffset)^ := Value;
      if Value then
        AdjustSize;
    end;
  end;
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

type
  TJumpCode = packed record
    Pop: Byte; // pop xxx
    Jmp: Byte; // jmp Offset
    Offset: Integer;
  end;

  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;

var
  SavedControlCode: TJumpCode;

{$O-}
procedure InitHookVars;
label
  Field, Proc, Leave;
var
  c: TControl;
  b: Boolean;
  Data: Byte;
  Relocation: TRelocationRec;
  n: Cardinal;
begin
  asm
        MOV     EAX, OFFSET Field
        ADD     EAX, 3
        ADD     EAX, 2
        XOR     EDX, EDX
        MOV     DL, BYTE PTR [EAX]
        MOV     [AutoSizeOffset], EDX

        MOV     EAX, OFFSET Proc
        ADD     EAX, 2
        ADD     EAX, 3
        ADD     EAX, 1
        MOV     EDX, [EAX]
        MOV     [TControl_SetAutoSize], EDX

        JMP     Leave
  end;

  c := nil;
Field:
  b := TOpenControl(c).AutoSize;
  if b then ;
Proc:
  TOpenControl(c).AutoSize := True;

Leave:
  if ReadProcessMemory(GetCurrentProcess, Pointer(Cardinal(TControl_SetAutoSize)),
     @Data, SizeOf(Data), n) then
  begin
    if Data = $FF then // Proc is in a dll or package
    begin
      if not ReadProcessMemory(GetCurrentProcess, Pointer(Cardinal(TControl_SetAutoSize)),
        @Relocation, SizeOf(Relocation), n) then
      TControl_SetAutoSize := Relocation.Address^;
    end;
  end;
end;
{$O+}

procedure InstallSetAutoSizeHook;
var
  Code: TJumpCode;
  P: procedure;
  n: Cardinal;
begin
  InitHookVars;
  P := TControl_SetAutoSize;
  if Assigned(P) then
  begin
    if PByte(@P)^ = $53 then // push ebx
      Code.Pop := $5B // pop ebx
    else
    if PByte(@P)^ = $55 then // push ebp
      Code.Pop := $5D // pop ebp
    else
      Exit;
    Code.Jmp := $E9;
    Code.Offset := Integer(@SetAutoSizeHook) - (Integer(@P) + 1) - SizeOf(Code);

    if ReadProcessMemory(GetCurrentProcess, Pointer(Cardinal(@P) + 1),
         @SavedControlCode, SizeOf(SavedControlCode), n) then
    begin
     { The strange thing is that WriteProcessMemory does not want @P or something
       overrides the $e9 with a "PUSH xxx"}
      if WriteProcessMemory(GetCurrentProcess, Pointer(Cardinal(@P) + 1), @Code,
           SizeOf(Code), n) then
        FlushInstructionCache(GetCurrentProcess, @P, SizeOf(Code));
    end;
  end;
end;

procedure UninstallSetAutoSizeHook;
var
  P: procedure;
  n: Cardinal;
begin
  P := TControl_SetAutoSize;
  if Assigned(P) then
  begin
    if WriteProcessMemory(GetCurrentProcess, Pointer(Cardinal(@P) + 1),
         @SavedControlCode, SizeOf(SavedControlCode), n) then
      FlushInstructionCache(GetCurrentProcess, @P, SizeOf(SavedControlCode));
  end;
end;

initialization
  InstallSetAutoSizeHook;

finalization
  UninstallSetAutoSizeHook;

{$ENDIF !COMPILER6_UP}

end.
