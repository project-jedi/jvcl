unit u1;

function main: String;
var
  Fl: Integer;
  St: string;
  I: Integer;
  Tmp: Integer;
begin
  Fl := FileOpen('.\samples\file.txt', fmOpenRead);
  try
    St := '';
    Tmp := 0;
    for i := 1 to 14 do
    begin
      FileRead(Fl, Tmp, 1);
      St := St + Chr(Tmp);
    end;
    ShowMessage(St);
  finally
    FileClose(Fl);
  end;
end;

end.
