var
  S: string;
begin
  S := '1234567890';
  S[4] := '!';
  Result := S;
end;
