{$I JVCL.INC}

unit JvHMIReg;

interface

procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvSegmentedLEDDisplay, JvSegmentedLEDDisplayEditors;

procedure Register;
begin
  RegisterComponents('HMI Indicators',[
    TJvSegmentedLEDDisplay
    ]);

  RegisterComponents('HMI Non Visual',[
    TJv7SegmentedLEDCharacterMapper
    ]);

  RegisterPropertyEditor(TypeInfo(TJvSegmentedLEDDigitClassName), TPersistent, '', TJvSegmentedLEDDigitClassProperty);
  RegisterPropertyEditor(TypeInfo(TUnlitColor), TPersistent, '', TUnlitColorProperty);

  RegisterComponentEditor(TJvCustomSegmentedLEDDisplay, TJvSegmentedLEDDisplayEditor);
end;

end.
