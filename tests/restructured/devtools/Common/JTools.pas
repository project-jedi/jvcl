unit JTools;

interface
uses
  Classes, SysUtils;


function ParseCmdLine(ACmdLine: PChar; List: TStrings; QuoteChar: char = '"'): boolean;
function GetCmdSwitchValue(const Switch: string; SwitchChars: TSysCharSet; var
  Value: string; IgnoreCase: boolean): boolean;
procedure ShowError(const S:string;const Args:array of const);

implementation


function ParseCmdLine(ACmdLine: PChar; List: TStrings; QuoteChar: char = '"'): boolean;
var 
  P: PChar;
  InQuote: boolean;

  procedure AddItem(StartAt, EndAt: PChar);
  var ch: char;
  begin
    ch := EndAt^;
    EndAt^ := #0;
//    if trim(StartAt) <> '' then
      List.Add(StartAt);
    EndAt^ := ch;
  end;
begin
  Result := false;
  if (ACmdLine = nil) or (ACmdLine^ = #0) then
    ACmdLine := System.CmdLine;
  if (ACmdLine = nil) or (ACmdLine^ = #0) then
    Exit;
  P := ACmdLine;
  while true do
  begin
    if ACmdLine^ = QuoteChar then
    begin
      InQuote := true;
      while InQuote do
      begin
        Inc(ACmdLine);
        if ACmdLine^ = #0 then
          Exit;                         // error in quoting
        InQuote := ACmdLine^ <> QuoteChar;
      end;
      Inc(ACmdLine);
      AddItem(P, ACmdLine);
      P := ACmdLine;
      Inc(P);
    end
    else if ACmdLine^ = #0 then
    begin
      if P^ <> #0 then
        AddItem(P, ACmdLine);
      Result := true;
      Exit;
    end
    else if ACmdLine^ <= #32 then
    begin
      AddItem(P,ACmdLine);
      P := ACmdLine;
      Inc(P);
    end;
    Inc(ACmdLine);
  end;
end;

function InRange(Value,Min,Max:integer):boolean;
begin
  Result := (Value >= Min) and (Value <= Max);
end;  

function GetCmdSwitchValue(const Switch: string; SwitchChars: TSysCharSet; var
  Value: string; IgnoreCase: boolean): boolean;
var 
  i: integer;
  S: string;
begin
  Result := false;
  for i := 1 to ParamCount do
  begin
    S := ParamStr(i);
    if (SwitchChars = []) or (S[1] in SwitchChars) then
    begin
      if IgnoreCase then
        Result := InRange(Pos(AnsiUpperCase(Switch), AnsiUpperCase(S)),1,2)
      else
        Result := InRange(Pos(Switch, S),1,2);
      if Result then
      begin
        Value := trim(Copy(S, Length(Switch) + Ord(SwitchChars <> []) + 1, MaxInt));
        Exit;
      end;
    end;
  end;
end;

procedure ShowError(const S:string;const Args:array of const);
begin
  writeln(ErrOutput,Format(S,Args));
end;

end.

