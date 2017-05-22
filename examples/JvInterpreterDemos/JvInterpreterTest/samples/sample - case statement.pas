var
  C: Integer;
begin
  Result := 0;
  C := 2;
  case C of
    1:
      begin
        Result := 1;
      end;
    1 + 1:
      Result := 2;
    else
      Result := 9;
  end;
  Result := Result + 100;
end;
