unit JvTestReg;

interface

procedure Register;

implementation
uses
  Classes, JvFillerControls, JvFillImpl;

procedure Register;
begin
  RegisterComponents('Jv Fillers',[TJvFillListBox, TJvTextFiller, TJvImageFiller, TFillLabel]);
end;

end.
