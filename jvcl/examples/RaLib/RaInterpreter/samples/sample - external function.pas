unit UserName;

function main: string;
var
  S: string;
  Sz: Integer;
begin
  S := '';
  Sz := 255;
  SetLength(S, Sz);
  if not GetUserName(S, Sz) then
    RaiseLastWin32Error;
  SetLength(S, Sz);
  Result := S;
end;

function GetUserName(lpBuffer: PChar; var nSize: DWORD): BOOL;
  external 'advapi32.dll' name 'GetUserNameA';

end.
