unit JvInterpreter_iMTracer;

interface

uses JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses iJvMTracer;


  { TJvTracerSocket  }

{  procedure Writeln(const S: string); }
procedure TTracerSocket_Writeln(var Value: Variant; Args: TArgs);
begin
  TJvTracerSocket (Args.Obj).Writeln(Args.Values[0]);
end;

{  procedure Clear; }
procedure TTracerSocket_Clear(var Value: Variant; Args: TArgs);
begin
  TJvTracerSocket (Args.Obj).Clear;
end;

{  procedure TimerStart(Timer: integer); }
procedure TTracerSocket_TimerStart(var Value: Variant; Args: TArgs);
begin
  TJvTracerSocket (Args.Obj).TimerStart(Args.Values[0]);
end;

{  procedure TimerStop(Timer: integer); }
procedure TTracerSocket_TimerStop(var Value: Variant; Args: TArgs);
begin
  TJvTracerSocket (Args.Obj).TimerStop(Args.Values[0]);
end;

{ property Read Enabled: boolean }
procedure TTracerSocket_Read_Enabled(var Value: Variant; Args: TArgs);
begin
  Value := TJvTracerSocket (Args.Obj).Enabled;
end;

{ function Tracer: TJvTracerSocket ; }
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
   { TJvTracerSocket  }
    AddClass('iJvMTracer', TJvTracerSocket , 'TJvTracerSocket ');
    AddGet(TJvTracerSocket , 'Writeln', TTracerSocket_Writeln, 1, [varString], varEmpty);
    AddGet(TJvTracerSocket , 'Clear', TTracerSocket_Clear, 0, [0], varEmpty);
    AddGet(TJvTracerSocket , 'TimerStart', TTracerSocket_TimerStart, 1, [varInteger], varEmpty);
    AddGet(TJvTracerSocket , 'TimerStop', TTracerSocket_TimerStop, 1, [varInteger], varEmpty);
    AddGet(TJvTracerSocket , 'Enabled', TTracerSocket_Read_Enabled, 0, [0], varBoolean);
    AddFun('iJvMTracer', 'Tracer', JvInterpreter_Tracer, 0, [0], varObject);
    AddFun('iJvMTracer', 'ODS', JvInterpreter_ODS, 1, [varString], varObject);
  end;    { with }
end;    { RegisterJvInterpreterAdapter }

end.
