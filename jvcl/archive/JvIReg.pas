{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Register JvInterpreter components

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

// Enable new property editor for integer types
{$DEFINE JvInterpreter_INTEGERPROPERTY}
// enables dblclick feature in new property editor
{.$DEFINE JvInterpreter_INTEGERPROPERTY_DBLCLICK}

unit JvIReg;

interface

procedure Register;

implementation

uses
  SysUtils, Classes, TypInfo,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  JvInterpreterFm,
  {$ENDIF VCL}
  JvInterpreter, JvxDConst;

{$R ..\resources\rai.dcr}

{$IFDEF JvInterpreter_INTEGERPROPERTY}

type
  TJvIntegerProperty = class(TIntegerProperty)
  private
    Component: TPersistent;
    JvInterpreterP: TJvInterpreterProgram;
    procedure JvInterpreterPGetValue(Sender: TObject; Identifier: string;
      var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
  public
    procedure SetValue(const Value: string); override;
    {$IFDEF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
    procedure Edit; override;
    {$ENDIF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
  end;

type
  THackJvInterpreterProgram = class(TJvInterpreterProgram);

procedure TJvIntegerProperty.JvInterpreterPGetValue(Sender: TObject; Identifier: string;
  var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
var
  Com: TComponent;
begin
  if (Component is TComponent) and (Args.Obj = nil) then
  begin
    Com := (Component as TComponent).Owner.FindComponent(Identifier);
    if Com <> nil then
    begin
      Value := O2V(Com);
      Done := True;
    end
    else
    begin
      Args.Obj := Component;
      Args.ObjTyp := varObject;
      try
        Done := THackJvInterpreterProgram(JvInterpreterP).GetValue(Identifier, Value, Args);
      finally
        Args.Obj := nil;
        Args.ObjTyp := 0;
      end;
    end;
  end;
end;

procedure TJvIntegerProperty.SetValue(const Value: string);

  function Calc: Longint;
  begin
    if JvInterpreterP = nil then
    begin
      JvInterpreterP := TJvInterpreterProgram.Create(nil);
      JvInterpreterP.OnGetValue := JvInterpreterPGetValue;
    end;
    JvInterpreterP.Source := Value;
    JvInterpreterP.Run;
    Result := JvInterpreterP.VResult;
  end;

var
  Int: Longint;
  PropInf: PPropInfo;
  I: Integer;
begin
  try
    { if Value is a simple integer we do not need to run JvInterpreter }
    Int := StrToInt(Value);
    SetOrdValue(Int);
  except
    { and now we really need to do this }
    for I := 0 to PropCount - 1 do
    begin
      Component := GetComponent(I) as TPersistent;
      Int := Calc;
      PropInf := TypInfo.GetPropInfo(Component.ClassInfo, GetName);
      if PropInf <> nil then
        SetOrdProp(Component, PropInf, Int);
      Modified;
    end;
  end;
end;

{$IFDEF JvInterpreter_INTEGERPROPERTY_DBLCLICK}

procedure TJvIntegerProperty.Edit;
var
  Int, P: Longint;
  PropInf: PPropInfo;
  I: Integer;
  Component: TPersistent;
begin
  for I := 0 to PropCount - 1 do
  begin
    Component := GetComponent(I) as TPersistent;
    PropInf := TypInfo.GetPropInfo(Component.ClassInfo, GetName);
    if PropInf <> nil then
    begin
      Int := GetOrdProp(Component, PropInf);
      {$R-}
      if KeyPressed(VK_CONTROL) then
        P := 10
      else
        P := 1;
      if KeyPressed(VK_SHIFT) then
        Dec(Int, P)
      else
        Inc(Int, P);
      {$R+}
      SetOrdProp(Component, PropInf, Int);
      Modified;
    end;
  end;
end;

{$ENDIF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
{$ENDIF JvInterpreter_INTEGERPROPERTY}

procedure Register;
begin
  {JvInterpreter unit}
  RegisterComponents(srJvInterpreterPalette, [TJvInterpreterProgram]);
  {$IFDEF VCL}
  RegisterComponents(srJvInterpreterPalette, [TJvInterpreterFm]);
  {$ENDIF VCL}
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  {$IFDEF JvInterpreter_INTEGERPROPERTY}
  RegisterPropertyEditor(TypeInfo(Integer), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Smallint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Shortint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), TObject, '', TJvIntegerProperty);
  {$ENDIF JvInterpreter_INTEGERPROPERTY}
  {$ENDIF}
end;

end.

