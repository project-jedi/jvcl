var
  SS: TStringList;
begin
  SS := TStringList.Create;
  SS.Add('Line 0');
  SS.Add('Line 1');
  Result := '! ' + SS[1] + ' !';
  SS.Free;
end;
