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
  JvConsts, JvSegmentedLEDDisplay, JvLED, JvDialButton,
  JvSegmentedLEDDisplayEditors;

procedure Register;
begin
  RegisterComponents(SPaletteHMIIndicator, [
    TJvSegmentedLEDDisplay,
    TJvLED
    ]);

  RegisterComponents(SPaletteHMIControls, [
    TJvDialButton]);

  RegisterComponents(SPaletteHMINonVisual, [
    TJv7SegmentedLEDCharacterMapper
    ]);

  RegisterPropertyEditor(TypeInfo(TJvSegmentedLEDDigitClassName), TPersistent, '', TJvSegmentedLEDDigitClassProperty);
  RegisterPropertyEditor(TypeInfo(TUnlitColor), TPersistent, '', TUnlitColorProperty);

  RegisterComponentEditor(TJvCustomSegmentedLEDDisplay, TJvSegmentedLEDDisplayEditor);
end;

end.
