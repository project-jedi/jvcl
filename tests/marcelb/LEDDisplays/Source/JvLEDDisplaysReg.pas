unit JvLEDDisplaysReg;

interface

{$R ..\Resources\JVLEDDisplaysreg.dcr}

procedure Register;

implementation

uses
  Classes,
  JvLEDDisplays;

procedure Register;
begin
  RegisterComponents('Jv Custom', [TJvSegmentLEDDisplay]);
end;

end.
