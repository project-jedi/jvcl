{$I JVCL.INC}
unit JTools;

interface
uses
  Classes, SysUtils;

// ParseCmdLine reads ACmdLine and splits it into separate tokens, adding them to List
// List must be allocated by caller, Items withing quotes (as defined by QuoteChar) are not
// split on space boundary. If ACmdLine is empty, reads system CmdLine instead
function ParseCmdLine(ACmdLine: PChar; List: TStrings; QuoteChar: char = '"'): boolean;

// GetCmdSwitchValue searches the command-line parameters for a command switch and returns
// the value associated with the switch (if found). Set IgnoreCase to true if Switch should be searched for
// without case-sensitivity.
// Switch is the character(s) making up the "switch ID", SwitchChars are the characters that can
// precede Switch, Value is filled with any value remaining after the switch character ( f ex in the command-line
//  -f"C:\Program Files\find.txt", 'f' is the Switch, '-' is a SwitchChars and Value will be filled with
// "C:\Program Files\find.txt"
function GetCmdSwitchValue(const Switch: string; SwitchChars: TSysCharSet; var
  Value: string; IgnoreCase: boolean): boolean;

// ShowError is a simple utility procecdure to show S on the default error output device
// in a console program the error is written to the console window, in a gui program an Exception is raised (and shown in a dialog)
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
  if IsConsole then
    writeln({$IFDEF COMPILER6_UP}ErrOutput,{$ENDIF}Format(S,Args))
  else
    raise Exception.CreateFmt(S,Args);
end;

end.

