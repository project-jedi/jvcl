unit JvInterpreter_iMTracer;

interface

uses JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses iMTracer;


  { TTracerSocket }

{  procedure Writeln(const S: string); }
procedure TTracerSocket_Writeln(var Value: Variant; Args: TArgs);
begin
  TTracerSocket(Args.Obj).Writeln(Args.Values[0]);
end;

{  procedure Clear; }
procedure TTracerSocket_Clear(var Value: Variant; Args: TArgs);
begin
  TTracerSocket(Args.Obj).Clear;
end;

{  procedure TimerStart(Timer: integer); }
procedure TTracerSocket_TimerStart(var Value: Variant; Args: TArgs);
begin
  TTracerSocket(Args.Obj).TimerStart(Args.Values[0]);
end;

{  procedure TimerStop(Timer: integer); }
procedure TTracerSocket_TimerStop(var Value: Variant; Args: TArgs);
begin
  TTracerSocket(Args.Obj).TimerStop(Args.Values[0]);
end;

{ property Read Enabled: boolean }
procedure TTracerSocket_Read_Enabled(var Value: Variant; Args: TArgs);
begin
  Value := TTracerSocket(Args.Obj).Enabled;
end;

{ function Tracer: TTracerSocket; }
procedure JvInterpreter_Tracer(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Tracer);
end;

{ procedure ODS(S: string); }
procedure JvInterpreter_ODS(var Value: Variant; Args: TArgs);
begin
  ODS(Args.Values[0]);
end;


procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
   { TTracerSocket }
    AddClass('iMTracer', TTracerSocket, 'TTracerSocket');
    AddGet(TTracerSocket, 'Writeln', TTracerSocket_Writeln, 1, [varString], varEmpty);
    AddGet(TTracerSocket, 'Clear', TTracerSocket_Clear, 0, [0], varEmpty);
    AddGet(TTracerSocket, 'TimerStart', TTracerSocket_TimerStart, 1, [varInteger], varEmpty);
    AddGet(TTracerSocket, 'TimerStop', TTracerSocket_TimerStop, 1, [varInteger], varEmpty);
    AddGet(TTracerSocket, 'Enabled', TTracerSocket_Read_Enabled, 0, [0], varBoolean);
    AddFun('iMTracer', 'Tracer', JvInterpreter_Tracer, 0, [0], varObject);
    AddFun('iMTracer', 'ODS', JvInterpreter_ODS, 1, [varString], varObject);
  end;    { with }
end;    { RegisterJvInterpreterAdapter }

end.
