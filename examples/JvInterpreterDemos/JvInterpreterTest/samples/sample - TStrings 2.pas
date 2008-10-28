unit U;

function main: string;
var
  SS: TStringList;
begin
  SS := TStringList.Create;
  SS.Add('Line 0');
  Result := SS[0];
  SS.Free;
end;

end.
