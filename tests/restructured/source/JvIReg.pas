{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
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

uses SysUtils,
  Classes,
  TypInfo,
  JvInterpreter,
  {$IFDEF COMPLIB_VCL}
  JvInterpreterFm,
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF COMPILER6_UP}
  ;

{$R ..\resources\rai.dcr}


  {$IFDEF JvInterpreter_INTEGERPROPERTY}
type

  TJvIntegerProperty = class(TIntegerProperty)
  private
    Component: TPersistent;
    JvInterpreterP: TJvInterpreterProgram;
    procedure JvInterpreterPGetValue(Sender: TObject; Identifer: string;
      var Value: Variant; Args: TArgs; var Done: Boolean);
  public
    procedure SetValue(const Value: string); override;
    {$IFDEF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
    procedure Edit; override;
    {$ENDIF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
  end;

  {$ENDIF JvInterpreter_INTEGERPROPERTY}

procedure Register;
begin
  {JvInterpreter unit}
  RegisterComponents('Jv Interpreter', [TJvInterpreterProgram]);
  {$IFDEF COMPLIB_VCL}
  RegisterComponents('Jv Interpreter', [TJvInterpreterFm]);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF JvInterpreter_INTEGERPROPERTY}
  RegisterPropertyEditor(TypeInfo(integer), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(cardinal), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(longint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(smallint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(shortint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(word), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(byte), TObject, '', TJvIntegerProperty);
  {$ENDIF JvInterpreter_INTEGERPROPERTY}
end;

{$IFDEF JvInterpreter_INTEGERPROPERTY}

{ TJvIntegerProperty  }

type
  THackJvInterpreterProgram = class(TJvInterpreterProgram);

procedure TJvIntegerProperty.JvInterpreterPGetValue(Sender: TObject; Identifer: string;
  var Value: Variant; Args: TArgs; var Done: Boolean);
var
  Com: TComponent;
begin
  if (Component is TComponent) and (Args.Obj = nil) then
    begin
      Com := (Component as TComponent).Owner.FindComponent(Identifer);
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
            Done := THackJvInterpreterProgram(JvInterpreterP).GetValue(Identifer, Value, Args);
          finally
            Args.Obj := nil;
            Args.ObjTyp := 0;
          end;
        end;
    end;
end; { GetValue }

procedure TJvIntegerProperty.SetValue(const Value: string);

  function Calc: longint;
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
  Int: longint;
  PropInf: PPropInfo;
  i: integer;
begin
  try
    { if Value is a simple integer we do not need to run JvInterpreter }
    Int := StrToInt(Value);
    SetOrdValue(Int);
  except
    { and now we really need do this }
    for i := 0 to PropCount - 1 do
      begin
        Component := GetComponent(i) as TPersistent;
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
  Int, P: longint;
  PropInf: PPropInfo;
  i: integer;
  Component: TPersistent;
begin
  for i := 0 to PropCount - 1 do
    begin
      Component := GetComponent(i) as TPersistent;
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
            dec(Int, P)
          else
            inc(Int, P);
          {$R+}
          SetOrdProp(Component, PropInf, Int);
          Modified;
        end;
    end;
end;
{$ENDIF JvInterpreter_INTEGERPROPERTY_DBLCLICK}
{$ENDIF JvInterpreter_INTEGERPROPERTY}

end.

