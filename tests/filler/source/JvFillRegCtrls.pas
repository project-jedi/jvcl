unit JvFillRegCtrls;

{$I JVCL.INC}

interface

procedure Register;

implementation

uses
  Classes,
  JvFillerControls, JvFillPropEdits;

procedure Register;
begin
  RegisterComponents('Jv Filler Controls', [TJvFillListBox, TJvFillLabel]);
  RegFillerPropEdits;
end;

end.
