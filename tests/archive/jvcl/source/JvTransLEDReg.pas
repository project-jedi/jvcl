// (rom) no Jedi header because thi file will die soon

unit JvTransLedReg;

interface

procedure Register;

implementation

uses
  Classes,
  JvTransLED;
  
{$R ..\resources\JvTransLed.dcr}

procedure Register;
begin
  RegisterComponents('Jv JFreeVCS', [TJvTransLED]);
end;


end.
