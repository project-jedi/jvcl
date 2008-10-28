// This script not working !
// Arrays as parameters not supported now....
// Sorry....

unit cray;

function main: string;
var
  A: array[1..10] of integer;
begin
  InitArray(A);
  //Result := A[4];
end;

procedure InitArray(var A: _array);
begin
  //A[4] := '!';
end;

end.
