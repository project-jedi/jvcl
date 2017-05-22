{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvInterpreter_JvRegAuto;

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes;

{ TJvRegAuto }

{ constructor Create(AOwner: TComponent) }

procedure TRegAuto_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TJvRegAuto.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Save; }

procedure TRegAuto_Save(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).Save;
end;

{ procedure Load; }

procedure TRegAuto_Load(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).Load;
end;

{$IFDEF VCL}

{ function ReadRootString(const Section, Ident, Default: string): string; }

procedure TRegAuto_ReadRootString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadRootString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ReadRootInteger(const Section, Ident: string; Default: Longint): Longint; }

procedure TRegAuto_ReadRootInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadRootInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteRootString(const Section, Ident, Value: string); }

procedure TRegAuto_WriteRootString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteRootString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteRootInteger(const Section, Ident: string; Value: Longint); }

procedure TRegAuto_WriteRootInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteRootInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{$ENDIF VCL}

{ function ReadString(const Section, Ident, Default: string): string; }

procedure TRegAuto_ReadString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteString(const Section, Ident, Value: String); }

procedure TRegAuto_WriteString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ReadInteger(const Section, Ident: string; Default: Longint): Longint; }

procedure TRegAuto_ReadInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteInteger(const Section, Ident: string; Value: Longint); }

procedure TRegAuto_WriteInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; }

procedure TRegAuto_ReadBool(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadBool(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteBool(const Section, Ident: string; Value: Boolean); }

procedure TRegAuto_WriteBool(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteBool(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ReadFloat(const Section, Ident: string; Default: Double): Double; }

procedure TRegAuto_ReadFloat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).ReadFloat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure WriteFloat(const Section, Ident: string; Value: Double); }

procedure TRegAuto_WriteFloat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteFloat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure ReadStrings(const Section, Ident: string; Strings: TStrings); }

procedure TRegAuto_ReadStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).ReadStrings(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ procedure WriteStrings(const Section, Ident: string; Value: TStrings); }

procedure TRegAuto_WriteStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).WriteStrings(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ procedure ReadSection(const Section: string; Ss: TStrings); }

procedure TRegAuto_ReadSection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).ReadSection(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ procedure ReadSections(Ss: TStrings); }

procedure TRegAuto_ReadSections(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).ReadSections(V2O(Args.Values[0]) as TStrings);
end;

{ procedure EraseSection(const Section: string); }

procedure TRegAuto_EraseSection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).EraseSection(Args.Values[0]);
end;

{ procedure DeleteKey(const Section, Ident: string); }

procedure TRegAuto_DeleteKey(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).DeleteKey(Args.Values[0], Args.Values[1]);
end;

{ procedure ReadWholeSection(const Section: string; Ss: TStrings); }

procedure TRegAuto_ReadWholeSection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).ReadWholeSection(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ property Read UseReg: Boolean }

procedure TRegAuto_Read_UseReg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).UseReg;
end;

{ property Write UseReg(Value: Boolean) }

procedure TRegAuto_Write_UseReg(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).UseReg := Value;
end;

{ property Read UseIni: Boolean }

procedure TRegAuto_Read_UseIni(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).UseIni;
end;

{ property Write UseIni(Value: Boolean) }

procedure TRegAuto_Write_UseIni(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).UseIni := Value;
end;

{ property Read UseStr: Boolean }

procedure TRegAuto_Read_UseStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvRegAuto(Args.Obj).UseStr;
end;

{ property Write UseStr(Value: Boolean) }

procedure TRegAuto_Write_UseStr(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvRegAuto(Args.Obj).UseStr := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cJvRegAuto = 'JvRegAuto';
begin
  with JvInterpreterAdapter do
  begin
    { TJvRegAuto }
    AddClass(cJvRegAuto, TJvRegAuto, 'TJvRegAuto');
    AddGet(TJvRegAuto, 'Create', TRegAuto_Create, 1, [varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'Save', TRegAuto_Save, 0, [0], varEmpty);
    AddGet(TJvRegAuto, 'Load', TRegAuto_Load, 0, [0], varEmpty);
    {$IFDEF VCL}
    AddGet(TJvRegAuto, 'ReadRootString', TRegAuto_ReadRootString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadRootInteger', TRegAuto_ReadRootInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteRootString', TRegAuto_WriteRootString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteRootInteger', TRegAuto_WriteRootInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TJvRegAuto, 'ReadString', TRegAuto_ReadString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteString', TRegAuto_WriteString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadInteger', TRegAuto_ReadInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteInteger', TRegAuto_WriteInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadBool', TRegAuto_ReadBool, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteBool', TRegAuto_WriteBool, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadFloat', TRegAuto_ReadFloat, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteFloat', TRegAuto_WriteFloat, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadStrings', TRegAuto_ReadStrings, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'WriteStrings', TRegAuto_WriteStrings, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadSection', TRegAuto_ReadSection, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadSections', TRegAuto_ReadSections, 1, [varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'EraseSection', TRegAuto_EraseSection, 1, [varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'DeleteKey', TRegAuto_DeleteKey, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'ReadWholeSection', TRegAuto_ReadWholeSection, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvRegAuto, 'UseReg', TRegAuto_Read_UseReg, 0, [0], varBoolean);
    AddSet(TJvRegAuto, 'UseReg', TRegAuto_Write_UseReg, 0, [varBoolean]);
    AddGet(TJvRegAuto, 'UseIni', TRegAuto_Read_UseIni, 0, [0], varBoolean);
    AddSet(TJvRegAuto, 'UseIni', TRegAuto_Write_UseIni, 0, [varBoolean]);
    AddGet(TJvRegAuto, 'UseStr', TRegAuto_Read_UseStr, 0, [0], varBoolean);
    AddSet(TJvRegAuto, 'UseStr', TRegAuto_Write_UseStr, 0, [varBoolean]);
    { EJvRegAutoError  }
    AddClass(cJvRegAuto, EJvRegAutoError, 'EJvRegAutoError ');
  end;
  RegisterClasses([TJvRegAuto]);
end;

end.

