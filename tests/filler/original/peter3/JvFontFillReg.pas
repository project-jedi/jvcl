unit JvFontFillReg;

interface

procedure Register;

implementation
uses
  Classes, JvFillCtrls, JvFillFontList;

procedure Register;
begin
  RegisterComponents('Jv Filler',[TJvFillListBox, TJvFillLabel,TJvFontFiller]);
end;

end.
