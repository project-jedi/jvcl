{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvInterpreter_MyLabel;

interface

uses Classes, JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses MyLabel;


  { TMyLabel }

{  procedure DoSomething; }
procedure TMyLabel_DoSomething(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TMyLabel(Args.Obj).DoSomething;
end;

{ property Write SomeProperty(Value: String) }
procedure TMyLabel_Write_SomeProperty(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TMyLabel(Args.Obj).SomeProperty := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
   { TMyLabel }
    AddClass('MyLabel', TMyLabel, 'TMyLabel');
    AddGet(TMyLabel, 'DoSomething', TMyLabel_DoSomething, 0, [0], varEmpty);
    AddSet(TMyLabel, 'SomeProperty', TMyLabel_Write_SomeProperty, 0, [varString]);
    RegisterClasses([TMyLabel]);
  end;    { with }
end;    { RegisterJvInterpreterAdapter }

initialization
  RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
end.
