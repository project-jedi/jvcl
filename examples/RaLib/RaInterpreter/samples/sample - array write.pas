var
  A: array[1..1000] of integer;
  i: Integer;
begin
  for i := 1 to 1000 do
    A[i] := i;
  Result := 1;
  for i := 1 to 1000 do
    Result := Result + A[i];
end;
