unit UUnit1;

uses UUnit2, UUnit3;

{const
  UConst = 'UUnit1.UConst';}
var
  UVar: TStringList;

function main: string;
begin
  //Result := UConst;
  //Result := UUnit2.UConst;
  Result := UUnit2.UFunction;
end;

end.
