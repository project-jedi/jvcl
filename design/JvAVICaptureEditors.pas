{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAVICaptureEditors.Pas, released on 2003-08-04.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvAVICaptureEditors;

interface

uses
  Windows, Classes, SysUtils, VFW, 
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvAVICapture, JvDsgnTypes;

type
  TJvDriverIndexEditor = class(TIntegerProperty)
  protected
    FDrivers: TStringList;
    procedure EnumDrivers;
  public
    constructor Create(const ADesigner: IJvFormDesigner; APropCount: Integer);
      override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TJvVirtualKeyEditor = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

implementation

uses
  Controls,
  JvVirtualKeyEditorForm, JvDsgnConsts;

//=== { TJvDriverIndexEditor } ===============================================

constructor TJvDriverIndexEditor.Create(const ADesigner: IJvFormDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FDrivers := TStringList.Create;
  EnumDrivers;
end;

destructor TJvDriverIndexEditor.Destroy;
begin
  FDrivers.Free;
  inherited Destroy;
end;

procedure TJvDriverIndexEditor.EnumDrivers;
var
  I: Integer;
  DeviceName: array [0..MAX_PATH] of Char;
  DeviceVersion: array [0..MAX_PATH] of Char;
begin
  // no more than 10 drivers in the system (cf Win32 API)
  for I := 0 to 9 do
    if capGetDriverDescription(I, DeviceName, SizeOf(DeviceName), DeviceVersion, SizeOf(DeviceVersion)) then
      FDrivers.Add(DeviceName);
end;

function TJvDriverIndexEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

function TJvDriverIndexEditor.GetValue: string;
var
  Index: TJvDriverIndex;
begin
  Index := GetOrdValue;
  if Index = -1 then
    Result := Format(RsGetValueFmt, [GetOrdValue, RsDisconnected])
  else
    Result := Format(RsGetValueFmt, [GetOrdValue, FDrivers[GetOrdValue]]);
end;

procedure TJvDriverIndexEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(Format(RsGetValueFmt, [-1, RsDisconnected]));
  for I := 0 to FDrivers.Count - 1 do
    Proc(Format(RsGetValueFmt, [I, FDrivers[I]]));
end;

procedure TJvDriverIndexEditor.SetValue(const Value: string);
var
  NewIndex: Integer;
begin
  // only consider string until the first space
  // then convert it into a integer
  NewIndex := StrToInt(Copy(Value, 1, Pos(' ', Value) - 1));

  // check its validity
  if (NewIndex >= -1) and (NewIndex < FDrivers.Count) then
    SetOrdValue(NewIndex)
  else
    raise ERangeError.CreateResFmt(@RsEdIsNotWithinTheValidRangeOfdd,
      [NewIndex, -1, FDrivers.Count - 1]);
end;

//=== { TJvVirtualKeyEditor } ================================================

procedure TJvVirtualKeyEditor.Edit;
begin
  with TfrmJvVirtualKeyEditor.Create(nil) do
  begin
    EditingFrame.CombinedKeyCode := GetOrdValue;
    if ShowModal = mrOk then
      SetOrdValue(EditingFrame.CombinedKeyCode);
    Free;
  end;
end;

function TJvVirtualKeyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TJvVirtualKeyEditor.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TJvVirtualKeyEditor.SetValue(const Value: string);
begin
  SetOrdValue(StrToInt(value));
end;

end.

