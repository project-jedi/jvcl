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
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit Compiler5MissingPropertyFix;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Buttons;

implementation

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
type
  TJump = packed record
    Jmp: Byte; // $E9;
    Offset: Integer;
  end;
var
  Jump: TJump;
  DestP, OldP: Pointer;
  OldProt: Cardinal;
begin
  if IsLibrary then
    raise Exception.Create('Not allowed in a DLL');
  Jump.Jmp := $E9;

  DestP := @TNativeBitBtn.CreateParams;
  OldP := @TOpenBitBtn.CreateParams;
  Jump.Offset := Integer(DestP) - Integer(OldP) - SizeOf(TJump);
  if VirtualProtect(OldP, SizeOf(TJump), PAGE_READWRITE, @OldProt) then
  begin
    Move(Jump, OldP^, SizeOf(TJump));
    VirtualProtect(OldP, SizeOf(TJump), OldProt, nil);
  end;
end;

{$ENDIF !COMPILER7_UP}

{$IFDEF COMPILER5}

type
  TCompiler5MissingPropertyFix = class(TReader)
  private
    FPropDefined: Boolean;
  protected
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
  protected
    procedure ReadWinControlDesignSize(Reader: TReader);
    procedure DefineProperties(Filer: TFiler);
  end;

procedure TCompiler5MissingPropertyFix.ReadWinControlDesignSize(Reader: TReader);
begin
  Reader.ReadListBegin;
  Reader.ReadInteger;
  Reader.ReadInteger;
  Reader.ReadListEnd;
end;

procedure TCompiler5MissingPropertyFix.DefineProperties(Filer: TFiler);
begin
  if Root is TWinControl then
    Filer.DefineProperty('DesignSize', ReadWinControlDesignSize, nil, False);
end;

procedure TCompiler5MissingPropertyFix.DefineProperty(const Name: string;
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
  Result := TCompiler5MissingPropertyFix.NewInstance;
end;

type
  PVmt = ^TVmt;
  TVmt = array[0..MaxWord - 1] of Pointer;

{$R-}
procedure ReplaceVmtField(Vmt: PVmt; VmtOffset: Integer; Value: Pointer);
var
  Index: Integer;
  OldProt: Cardinal;
begin
  Index := VmtOffset div SizeOf(Pointer);
  if VirtualProtect(@vmt[Index], SizeOf(Pointer), PAGE_READWRITE, @OldProt) then
  begin
    Vmt[Index] := Value;
    VirtualProtect(@Vmt[Index], SizeOf(Pointer), OldProt, nil);
  end;
end;
{$R+}

procedure ReplaceDefineProperty;
begin
  ReplaceVmtField(PVmt(TReader), vmtNewInstance, @NewInstanceHook);
end;

{$ENDIF COMPILER5}

initialization
{$IFDEF COMPILER5}
  ReplaceDefineProperty;
{$ENDIF COMPILER5}

{$IFNDEF COMPILER7_UP}
  HookBitBtn;
{$ENDIF !COMPILER7_UP}

end.
