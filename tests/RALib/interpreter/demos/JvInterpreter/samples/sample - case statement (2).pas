unit Unit1;

const
  V = 1;

function caseme(Param: integer): string;
begin
  case Param of
    V:
      Result := '1 selected';
    1 + 1:
      Result := '2 selected';
    else
      Result := '"else" selected';
  end;
end;

function main: string;
begin
  Result := caseme(1);
end;

end.
