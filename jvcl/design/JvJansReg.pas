// No licence info because this should be merged with other Registration Unit
{$I JEDI.INC}
unit JvJansReg;

interface
uses Classes, JvMarkupLabel, JvMarkupViewer, JvSAL, JvSticker, JvSALCORE, JvSALMath,
  JvYearGrid, JvTracker, JvAirBrush, JvGridFilter, JvGridPrinter,
  JvJanTreeView, JvArrayButton, JvForth, JvTurtle, JvPaintFX, JvDrawImage,
  JvBitmapButton, JvSimScope, JvSimIndicator, JvSimPID, JvSIMPIDLinker,
  JvSimLogic, JvSpellerForm, JvShapedButton, JvCSVBaseControls, JvCSVBaseEditor,
    {$IFDEF COMPILER6_UP}
   DesignIntf, DesignEditors, VCLEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF COMPILER6_UP}
  ;

procedure Register;
implementation
{$R ..\Resources\JvJansReg.dcr}

procedure Register;
begin
  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents('Jv Jans', [
    TJvMarkupLabel, TJvMarkupViewer, TJvSAL, TJvSticker, TJvSALCore, TJvSALMath, TJvYearGrid,
    TJvAirBrush, TJvTracker, TJvGridFilter, TJvGridPrinter, TJvJanTreeview, TJvPaintFX,
    TJvDrawImage, TJvArrayButton, TJvForthScript, TJvTurtle, TJvBitmapButton, TJvSpeller, TJvShapedButton]);

  // Simluator Components
  RegisterComponents('Jv Jans SIM', [
    TJvSimScope, TJvSimIndicator, TJvSimPID, TJvSimPIDLinker,
    TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight, TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents('Jv Jans CSV', [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox, TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, 'CSVFileName', TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, 'CSVField', TCSVFieldProperty);

end;

end.
