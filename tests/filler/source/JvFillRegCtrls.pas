unit JvFillRegCtrls;

{$I JVCL.INC}

interface

procedure Register;

implementation

uses
  Classes,
  JvFillerControls{$IFNDEF COMPILER6_UP}, JvFillPropEdits{$ENDIF COMPILER6_UP};

procedure Register;
begin
  RegisterComponents('Jv Filler Controls', [TJvFillListBox, TJvFillLabel]);
  {$IFNDEF COMPILER6_UP}
  RegFillerPropEdits;
  {$ENDIF COMPILER6_UP}
end;

end.
