unit JvInterpreter_MyLabel;

interface

uses Classes, JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses MyLabel;


  { TMyLabel }

{  procedure DoSomething; }
procedure TMyLabel_DoSomething(var Value: Variant; Args: TArgs);
begin
  TMyLabel(Args.Obj).DoSomething;
end;

{ property Write SomeProperty(Value: String) }
procedure TMyLabel_Write_SomeProperty(const Value: Variant; Args: TArgs);
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
