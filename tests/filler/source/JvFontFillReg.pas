unit JvFontFillReg;

interface

procedure Register;

implementation
uses
  Classes, JvFillListBox, JvFillFontList;

procedure Register;
begin
  RegisterComponents('Jv Filler',[TJvFillListBox, TJvFontFiller]);
end;

end.
