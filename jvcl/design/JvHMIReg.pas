{$I JVCL.INC}

unit JvHMIReg;

interface

procedure Register;

implementation

{$R ..\..\Resources\JvHMIReg.dcr}

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvSegmentedLEDDisplay, JvLED, JvRadioControl, 
  JvSegmentedLEDDisplayEditors;

procedure Register;
begin
  RegisterComponents('HMI Indicators', [
    TJvSegmentedLEDDisplay,
    TJvLED
    ]);

  RegisterComponents('HMI Non Visual', [
    TJv7SegmentedLEDCharacterMapper
    ]);

  RegisterComponents('HMI Controls', [
    TJvRadioControl]);

  RegisterPropertyEditor(TypeInfo(TJvSegmentedLEDDigitClassName), TPersistent, '', TJvSegmentedLEDDigitClassProperty);
  RegisterPropertyEditor(TypeInfo(TUnlitColor), TPersistent, '', TUnlitColorProperty);

  RegisterComponentEditor(TJvCustomSegmentedLEDDisplay, TJvSegmentedLEDDisplayEditor);
end;

end.
