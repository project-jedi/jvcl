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

Last Modified: 2004-01-13

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
  Classes, SysUtils,
  JvThemes;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ELSE}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VCL}

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
    function HintShow(var HintInfo: THintInfo): Boolean;
    function HitTest(X, Y: Integer): Boolean;
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
    {$IFDEF VCL}
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF VCL}
  end;

  IJvWinControlEvents = interface
    ['{B5F7FB62-78F0-481D-AFF4-7A24ED6776A0}']
    procedure CursorChanged;
    procedure ShowingChanged;
    procedure ShowHintChanged;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean);
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean);
    procedure DoGetDlgCode(var Code: TDlgCodes);
  end;

  IJvCustomControlEvents = interface
    ['{7804BD3A-D7A5-4314-9259-6DE08A0DC38A}']
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

procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);

{$IFNDEF COMPILER6_UP}
procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
{$ENDIF !COMPILER6_UP}
{$ENDIF VCL}

{$IFDEF VisualCLX}
function WidgetControl_Painting(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH): IInterface;
  // - returns NIL if the Instance is in csDestroying.
  // - enters the painting and returns an interface that leaves the painting when
  //   is is released.
procedure WidgetControl_DefaultPaint(Instance: TWidgetControl; Canvas: TCanvas);

function TWidgetControl_NeedKey(Instance: TWidgetControl; Key: Integer;
  Shift: TShiftState; const KeyText: WideString; InheritedValue: Boolean): Boolean;
{$ENDIF VisualCLX}

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
  Canvas: TCanvas;
  DlgCodes: TDlgCodes;
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

        WM_ERASEBKGND:
          begin
            Canvas := TCanvas.Create;
            try
              Canvas.Handle := HDC(PMsg^.WParam);
              PMsg^.Result := Ord(DoPaintBackground(Canvas, PMsg^.LParam));
            finally
              Canvas.Handle := 0;
              Canvas.Free;
            end;
          end
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

          WM_GETDLGCODE:
            begin
              PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);

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
        else
          CallInherited := True;
        end;
    end;
  end;

  if CallInherited then
    PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);
end;

{ VCL sends CM_CONTROLLISTCHANGE and CM_CONTROLCHANGE in an other order that
  the CLX methods are used. So we must correct it by evaluating "Inserting". } 
procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}

type
  TOpenWidgetControl = class(TWidgetControl);

  TWidgetControlPainting = class(TInterfacedObject)
  private
    FCanvas: TCanvas;
    FInstance: TWidgetControl;
  public
    constructor Create(Instance: TWidgetControl; Canvas: TCanvas;
      EventRegion: QRegionH);
    destructor Destroy; override;
  end;

constructor TWidgetControlPainting.Create(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH);
begin
  inherited Create;
  FCanvas := Canvas;
  FInstance := Instance;

  TControlCanvas(FCanvas).StartPaint;
  QPainter_setClipRegion(FCanvas.Handle, EventRegion);
end;

destructor TWidgetControlPainting.Destroy;
begin
  TControlCanvas(FCanvas).StopPaint;
  inherited Destroy;
end;

function WidgetControl_Painting(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH): IInterface;
begin
  if csDestroying in Instance.ComponentState then
    Result := nil
  else
    Result := TWidgetControlPainting.Create(Instance, Canvas, EventRegion);
end;

procedure WidgetControl_DefaultPaint(Instance: TWidgetControl; Canvas: TCanvas);
var
  Event: QPaintEventH;
  PaintDevice: QPaintDeviceH;
  IsActive: Boolean;
begin
  if not (csDestroying in Instance.ComponentState) and
     (not Supports(Instance, IJvCustomControlEvents)) then
       { TCustomControls do not have a default paint method. }
  begin
   // Canvas.StopPaint uses a counter, but we must garantee the Stop.
    PaintDevice := nil;
    IsActive := QPainter_isActive(Canvas.Handle);
    if IsActive then
    begin
      PaintDevice := QPainter_device(Canvas.Handle);
      QPainter_end(Canvas.Handle);
    end;
    try
      Event := QPaintEvent_create(QPainter_clipRegion(Canvas.Handle), False);
      try
        Instance.ControlState := Instance.ControlState + [csWidgetPainting];
        try
          QObject_event(Instance.Handle, Event);
        finally
          Instance.ControlState := Instance.ControlState - [csWidgetPainting];
        end;
      finally
        QPaintEvent_destroy(Event);
      end;
    finally
      // restore
      if IsActive then
        QPainter_begin(Canvas.Handle, PaintDevice); // restart
    end;
  end;
end;

function TWidgetControl_NeedKey(Instance: TWidgetControl; Key: Integer;
  Shift: TShiftState; const KeyText: WideString; InheritedValue: Boolean): Boolean;

  function IsTabKey: Boolean;
  begin
    Result := (Key = Key_Tab) or (Key = Key_BackTab);
  end;

  function IsArrowKey: Boolean;
  begin
    Result := (Key = Key_Left) or (Key = Key_Right) or
              (Key = Key_Down) or (Key = Key_Up);
  end;

var
  DlgCodes: TDlgCodes;
  Value: TInputKeys;
begin
  Result := InheritedValue;
  Value := TOpenWidgetControl(Instance).InputKeys;

  DlgCodes := [dcNative];
  if ikAll in Value then
    Include(DlgCodes, dcWantAllKeys);
  if ikArrows in Value then
    Include(DlgCodes, dcWantArrows);
  if ikTabs in Value then
    Include(DlgCodes, dcWantTab);
  if ikChars in Value then
    Include(DlgCodes, dcWantChars);

  (Instance as IJvWinControlEvents).DoGetDlgCode(DlgCodes);

  if not (dcNative in DlgCodes) then
  begin
    Result := False;
    if dcWantAllKeys in DlgCodes then
      Result := True;
    if (not Result) and (dcWantTab in DlgCodes) then
      Result := IsTabKey;
    if (not Result) and (dcWantArrows in DlgCodes) then
      Result := IsArrowKey;
    if (not Result) and (dcWantChars in DlgCodes) then
      Result := ((Shift * [ssCtrl, ssAlt] = []) and
                ((Hi(Word(Key)) = 0) or (Length(KeyText) > 0))) and
                not (IsTabKey or IsArrowKey);
  end;
end;

{$ENDIF VisualCLX}

// *****************************************************************************

JV_CONTROL_EVENTS_IMPL(Control)
JV_WINCONTROL_EVENTS_IMPL(WinControl)

JV_CONTROL_EVENTS_IMPL(GraphicControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(HintWindow)

// *****************************************************************************

{$IFNDEF COMPILER6_UP}
type
  TOpenControl = class(TControl);
  PBoolean = ^Boolean;
  PPointer = ^Pointer;

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
  AutoSizeOffset: Cardinal;
  TControl_SetAutoSize: Pointer;
  SavedControlCode: TJumpCode;

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

{$OPTIMIZATION ON} // be sure to have optimization activated
function GetCode(Instance: TOpenControl): Boolean;
begin
  { generated code:
      8A40xx       mov al,[eax+Byte(Offset)]
  }
  Result := Instance.AutoSize;
end;

procedure SetCode(Instance: TOpenControl);
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
  Relocation: TRelocationRec;
  PGetCode: PGetCodeRec;
  PSetCode: PSetCodeRec;
  Data: Byte;
begin
  TControl_SetAutoSize := nil;
  AutoSizeOffset := 0;

  PGetCode := @GetCode;
  PSetCode := @SetCode;

  if (PGetCode^.Sign = GetCodeSign) and
     (PSetCode^.Sign1 = SetCodeSign1) and (PSetCode^.Sign2 = SetCodeSign2) then
  begin
    AutoSizeOffset := PGetCode^.Offset;
    TControl_SetAutoSize := Pointer(Integer(@SetCode) + SizeOf(TSetCodeRec) +
      PSetCode^.Offset);

   // the relocation table meight be protected
    if ReadProtectedMemory(TControl_SetAutoSize, Data, SizeOf(Data)) then
    begin
      if Data = $FF then // TControl.SetAutoSize is in a dll or package
        if ReadProtectedMemory(TControl_SetAutoSize, Relocation, SizeOf(Relocation)) then
          TControl_SetAutoSize := Relocation.Address^;
    end;
  end;
end;

procedure InstallSetAutoSizeHook;
var
  Code: TJumpCode;
begin
  InitHookVars;
  if Assigned(TControl_SetAutoSize) then
  begin
    if PByte(TControl_SetAutoSize)^ = $53 then // push ebx
      Code.Pop := $5B // pop ebx
    else
    if PByte(TControl_SetAutoSize)^ = $55 then // push ebp
      Code.Pop := $5D // pop ebp
    else
      Exit;
    Code.Jmp := $E9;
    Code.Offset := Integer(@SetAutoSizeHook) -
      (Integer(TControl_SetAutoSize) + 1) - SizeOf(Code);

    if ReadProtectedMemory(Pointer(Cardinal(TControl_SetAutoSize) + 1),
      SavedControlCode, SizeOf(SavedControlCode)) then
    begin
     { The strange thing is that something overwrites the $e9 with a "PUSH xxx" }
      if WriteProtectedMemory(Pointer(Cardinal(TControl_SetAutoSize) + 1), Code,
           SizeOf(Code)) then
        FlushInstructionCache(GetCurrentProcess, TControl_SetAutoSize, SizeOf(Code));
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
