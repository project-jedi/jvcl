unit Unit1;

procedure main;
var
  i: Integer;
begin
  for i := 1 to 100000 do
  begin
    func(i);
  end;
end;

procedure func(i: Integer);
var
  v1: Integer;
  v2: Integer;
  v3: Integer;
  v4: Integer;
begin
  v1 := i + 1;
  v2 := i + 2;
  v3 := i + 3;
  v4 := i + 4;
end;

end.

