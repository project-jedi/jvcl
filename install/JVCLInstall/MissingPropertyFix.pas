{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Compiler5MissingPropertyFix.pas, released on 2004-03-31.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit MissingPropertyFix;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Buttons, Graphics, Forms;

procedure RedirectFunction(OldP, DestP: Pointer);

implementation

uses
  JclSysUtils;

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: ^Pointer;
  end;

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean;
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure RedirectFunction(OldP, DestP: Pointer);
type
  TJump = packed record
    Jmp: Byte; // $E9;
    Offset: Integer;
  end;
var
  Jump: TJump;
  WrittenBytes: Cardinal;
begin
  if IsLibrary then
    raise Exception.Create('Not allowed in a DLL');
  OldP := GetActualAddr(OldP);
  DestP := GetActualAddr(DestP);
  Jump.Jmp := $E9;
  Jump.Offset := Integer(DestP) - Integer(OldP) - SizeOf(TJump);
  WriteProtectedMemory(OldP, @Jump, SizeOf(TJump), WrittenBytes);
end;

{$IFNDEF COMPILER7_UP}

type
  TNativeBitBtn = class(TBitBtn)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

type
  TOpenButton = class(TButton);
  TOpenBitBtn = class(TBitBtn);

procedure TNativeBitBtn.CreateParams(var Params: TCreateParams);
var
  p: procedure(Instance: TObject; var Params: TCreateParams);
begin
  p := @TOpenButton.CreateParams;
  p(Self, Params);
end;

procedure HookBitBtn;
begin
  RedirectFunction(@TOpenBitBtn.CreateParams, @TNativeBitBtn.CreateParams);
end;

{$ENDIF ~COMPILER7_UP}

{$IFNDEF COMPILER10_UP}
type                          
  TOpenWinControl = class(TWinControl);

  TWinControlFix = class(TWinControl)
  private
    {$IFDEF COMPILER7_UP}
    procedure WMPrintClient(var Message: TWMPrintClient);
    {$ENDIF COMPILER7_UP}
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure MainWndProc(var Message: TMessage);
  end;

{$IFDEF COMPILER7_UP}
procedure TWinControlFix.WMPrintClient(var Message: TWMPrintClient);
var
  SaveIndex: Integer;
begin
  with Message do
    if Result <> 1 then
      if ((Flags and PRF_CHECKVISIBLE) = 0) or Visible then
      begin
        SaveIndex := SaveDC(DC);
        try
          PaintHandler(TWMPaint(Message));
        finally
          RestoreDC(DC, SaveIndex);
        end;
      end
      else
        DefaultHandler(Message)
    else
      DefaultHandler(Message);
end;
{$ENDIF COMPILER7_UP}

procedure TWinControlFix.PaintWindow(DC: HDC);
var
  Message: TMessage;
begin
  if not (TWinControl(Self) is TCustomFrame) then
  begin
    Message.Msg := WM_PAINT;
    Message.WParam := DC;
    Message.LParam := 0;
    Message.Result := 0;
    DefaultHandler(Message);
  end;
end;

procedure TWinControlFix.MainWndProc(var Message: TMessage);
begin
  try
    try
      WindowProc(Message);
      if Message.Msg = WM_UPDATEUISTATE then
        Invalidate; // Ensure control is repainted
    finally
      //FreeDeviceContexts;  The installer doesn't use that many controls
      FreeMemoryContexts;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure HookWinControl;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
  begin
    { Vista Workaround }
    RedirectFunction(@TOpenWinControl.MainWndProc, @TWinControlFix.MainWndProc);
  end;
  {$IFDEF COMPILER7_UP}
  RedirectFunction(GetDynamicMethod(TWinControl, WM_PRINTCLIENT), @TWinControlFix.WMPrintClient);
  {$ENDIF COMPILER7_UP}
  RedirectFunction(@TOpenWinControl.PaintWindow, @TWinControlFix.PaintWindow);
end;

{$ENDIF ~COMPILER10_UP}

{$IFNDEF COMPILER10_UP}

type
  TMissingPropertyFix = class(TReader)
  private
    FPropDefined: Boolean;
  protected
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
  protected
    {$IFNDEF COMPILER10_UP}
    procedure ReadControlExplicitProp(Reader: TReader);
    {$ENDIF ~COMPILER10_UP}
    procedure DefineProperties(Filer: TFiler);
  end;

{$IFNDEF COMPILER10_UP}
procedure TMissingPropertyFix.ReadControlExplicitProp(Reader: TReader);
begin
  Reader.ReadInteger;
end;
{$ENDIF ~COMPILER10_UP}

procedure TMissingPropertyFix.DefineProperties(Filer: TFiler);
begin
  {$IFNDEF COMPILER10_UP}
  if Root is TControl then
  begin
    Filer.DefineProperty('ExplicitLeft', ReadControlExplicitProp, nil, False);
    Filer.DefineProperty('ExplicitTop', ReadControlExplicitProp, nil, False);
    Filer.DefineProperty('ExplicitWidth', ReadControlExplicitProp, nil, False);
    Filer.DefineProperty('ExplicitHeight', ReadControlExplicitProp, nil, False);
  end;
  {$ENDIF ~COMPILER10_UP}
end;

procedure TMissingPropertyFix.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc;
  HasData: Boolean);
begin
  if not FPropDefined then
  begin
    FPropDefined := True;
    try
      DefineProperties(Self);
    finally
      FPropDefined := False;
    end;
  end;
  inherited DefineProperty(Name, ReadData, WriteData, HasData);
end;


function NewInstanceHook(ReaderClass: TClass): TObject;
begin
  Result := TMissingPropertyFix.NewInstance;
end;

type
  PVmt = ^TVmt;
  TVmt = array[0..MaxWord - 1] of Pointer;

{$R-}
procedure ReplaceVmtField(Vmt: PVmt; VmtOffset: Integer; Value: Pointer);
var
  Index: Integer;
  OldProt, Dummy: Cardinal;
begin
  Index := VmtOffset div SizeOf(Pointer);
  if VirtualProtect(@vmt[Index], SizeOf(Pointer), PAGE_EXECUTE_READWRITE, @OldProt) then
  begin
    Vmt[Index] := Value;
    VirtualProtect(@Vmt[Index], SizeOf(Pointer), OldProt, Dummy);
  end;
end;
{$R+}

procedure ReplaceDefineProperty;
begin
  {$WARNINGS OFF}
  ReplaceVmtField(PVmt(TReader), vmtNewInstance, @NewInstanceHook);
  {$WARNINGS ON}
end;

{$ENDIF ~COMPILER10_UP}

initialization
  {$IFNDEF COMPILER10_UP}
  ReplaceDefineProperty;
  {$ENDIF ~COMPILER10_UP}

  {$IFNDEF COMPILER10_UP}
  HookWinControl;
  {$ENDIF ~COMPILER10_UP}

  {$IFNDEF COMPILER7_UP}
  HookBitBtn;
  {$ENDIF !COMPILER7_UP}

end.