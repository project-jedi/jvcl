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
  RegisterComponents('Jv Jans', [TJvMarkupLabel]);
  RegisterComponents('Jv Jans', [TJvMarkupViewer]);
  RegisterComponents('Jv Jans', [TJvSAL]);
  RegisterComponents('Jv Jans', [TJvSticker]);
  RegisterComponents('Jv Jans', [TJvSALCore]);
  RegisterComponents('Jv Jans', [TJvSALMath]);
  RegisterComponents('Jv Jans', [TJvYearGrid]);
  RegisterComponents('Jv Jans', [TJvAirBrush]);
  RegisterComponents('Jv Jans', [TJvTracker]);
  RegisterComponents('Jv Jans', [TJvGridFilter]);
  RegisterComponents('Jv Jans', [TJvGridPrinter]);
  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents('Jv Jans', [TJvJanTreeview]);
  RegisterComponents('Jv Jans', [TJvPaintFX]);
  RegisterComponents('Jv Jans', [TJvDrawImage]);
  RegisterComponents('Jv Jans', [TJvArrayButton]);

  RegisterComponents('Jv Jans', [TJvForthScript]);
  RegisterComponents('Jv Jans', [TJvTurtle]);
  RegisterComponents('Jv Jans', [TJvBitmapButton]);
  RegisterComponents('Jv Jans', [TJvSpeller]);

  RegisterComponents('Jv Jans', [TJvShapedButton]);

  // Simluator Components
  RegisterComponents('Jv Jans SIM', [TJvSimScope]);
  RegisterComponents('Jv Jans SIM', [TJvSimIndicator]);
  RegisterComponents('Jv Jans SIM', [TJvSimPID]);
  RegisterComponents('Jv Jans SIM', [TJvSimPIDLinker]);
  RegisterComponents('Jv Jans SIM', [TJvSIMConnector, TJvLogic, TJvSimButton, TJvSimLight, TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents('Jv Jans CSV', [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox, TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, 'CSVFileName', TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, 'CSVField', TCSVFieldProperty);

end;

end.
