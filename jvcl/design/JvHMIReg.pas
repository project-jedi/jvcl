{$I JVCL.INC}

unit JvHMIReg;

interface

procedure Register;

implementation

{$R ..\..\Resources\JvHMIReg.dcr}

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, ToolsApi,
  {$ELSE}
  DsgnIntf, ToolsApi,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvSegmentedLEDDisplay, JvLED, JvDialButton,
  JvSegmentedLEDDisplayEditors, JvSegmentedLEDDisplayMapperFrame;

procedure Register;
begin
  RegisterComponents(SPaletteHMIIndicator, [
    TJvSegmentedLEDDisplay,
    TJvLED
    ]);

  RegisterComponents(SPaletteHMIControls, [
    TJvDialButton
    ]);

  RegisterPropertyEditor(TypeInfo(TJvSegmentedLEDDigitClassName), TPersistent, '', TJvSegmentedLEDDigitClassProperty);
  RegisterPropertyEditor(TypeInfo(TUnlitColor), TPersistent, '', TUnlitColorProperty);

  RegisterComponentEditor(TJvCustomSegmentedLEDDisplay, TJvSegmentedLEDDisplayEditor);

  RegisterCustomModule(TfmeJvSegmentedLEDDisplayMapper, TCustomModule);
end;

end.
