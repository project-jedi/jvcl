unit JvTransLedReg;

interface

procedure Register;

implementation

uses
  Classes, JvTransLed;
  
{$R ..\resources\JvTransLed.dcr}

procedure Register;
begin
  RegisterComponents('Jv JFreeVCS', [TJvTransLED]);
end;


end.
