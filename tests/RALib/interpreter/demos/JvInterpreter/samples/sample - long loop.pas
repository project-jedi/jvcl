unit U;

function main: Integer;
var
  I: integer;
begin
  Result := 0;
  for i := 1 to 1000 do
    Result := square(i);
end;

function square(P: Integer): Integer;
begin
  Result := P * P;
end;

end.
