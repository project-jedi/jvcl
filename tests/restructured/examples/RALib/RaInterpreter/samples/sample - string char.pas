var
  S: string;
begin
  //S := '1234567890';
  S := '';
  try
    Result := S[11];
  except
    on E: ERangeError do
      Result := 'error';
  end;
end;

