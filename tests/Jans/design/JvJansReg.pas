// No licence info because this should be merged with other Registration Unit
{$I JEDI.INC}
unit JvJansReg;

interface
uses Classes, JvMarkupLabel, JvMarkupViewer, JvSAL, JvSticker, JvSALCORE, JvSALMath,
  JvYearGrid, JvTracker, JvAirBrush, JvGridFilter, JvGridPrinter,
  JvJanTreeView, JvArrayButton, JvForth, JvTurtle, JvPaintFX, JvDrawImage,
  JvBitmapButton, JvSimScope, JvSimIndicator, JvSimPID, JvSIMPIDLinker,
  JvSimLogic, JvSpeller, JvShapedButton, JvCSVBaseControls, JvCSVBaseEditor,
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
  RegisterComponents('JvJans', [TJvMarkupLabel]);
  RegisterComponents('JvJans', [TJvMarkupViewer]);
  RegisterComponents('JvJans', [TJvSAL]);
  RegisterComponents('JvJans', [TJvSticker]);
  RegisterComponents('JvJans', [TJvSALCore]);
  RegisterComponents('JvJans', [TJvSALMath]);
  RegisterComponents('JvJans', [TJvYearGrid]);
  RegisterComponents('JvJans', [TJvAirBrush]);
  RegisterComponents('JvJans', [TJvTracker]);
  RegisterComponents('JvJans', [TJvGridFilter]);
  RegisterComponents('JvJans', [TJvGridPrinter]);
  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents('JvJans', [TJvJanTreeview]);
  RegisterComponents('JvJans', [TJvPaintFX]);
  RegisterComponents('JvJans', [TJvDrawImage]);
  RegisterComponents('JvJans', [TJvArrayButton]);

  RegisterComponents('JvJans', [TJvForthScript]);
  RegisterComponents('JvJans', [TJvTurtle]);
  RegisterComponents('JvJans', [TJvBitmapButton]);
  RegisterComponents('JvJans', [TJvSpeller]);

  RegisterComponents('JvJans', [TJvShapedButton]);

  // Simluator Components
  RegisterComponents('JvJans SIM', [TJvSimScope]);
  RegisterComponents('JvJans SIM', [TJvSimIndicator]);
  RegisterComponents('JvJans SIM', [TJvSimPID]);
  RegisterComponents('JvJans SIM', [TJvSimPIDLinker]);
  RegisterComponents('JvJans SIM', [TJvSIMConnector, TJvLogic, TJvSimButton, TJvSimLight, TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents('JvJans CSV', [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox, TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, 'CSVFileName', TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, 'CSVField', TCSVFieldProperty);

end;

end.
