// No licence info because this should be merged with other Registration Unit
{$I JEDI.INC}
unit JvJansReg;

interface
uses Classes, JvMarkupLabel, JvMarkupViewer, JvSAL, JvSticker, JvSALCORE, JvSALMath,
  JvYearGrid, JvTracker, JvAirBrush, JvGridFilter, JvGridPrinter,
  JvJanTreeView, JvArrayButton, JvForth, JvTurtle, JvPaintFX, JvDrawImage,
  JvBitmapButton, JvSimScope, JvSimIndicator, JvSimPID, JvSIMPIDLinker,
  JvSimLogic, JvSpeller, JvShapedButton, JvCSVBase, JvCSVBaseProp,
    {$IFDEF COMPILER6_UP}
   DesignIntf, DesignEditors, VCLEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF COMPILER6_UP}

  ;

procedure Register;
implementation
{$R ..\Resources\jvJansReg.dcr}

procedure Register;
begin
  RegisterComponents('JvJANS', [TJvMarkupLabel]);
  RegisterComponents('JvJANS', [TJvMarkupViewer]);
  RegisterComponents('JvJANS', [TJvSAL]);
  RegisterComponents('JvJANS', [TJvSticker]);
  RegisterComponents('JvJANS', [TJvSALCore]);
  RegisterComponents('JvJANS', [TJvSALMath]);
  RegisterComponents('JvJANS', [TJvYearGrid]);
  RegisterComponents('JvJANS', [TJvAirBrush]);
  RegisterComponents('JvJANS', [TJvTracker]);
  RegisterComponents('JvJANS', [TJvGridFilter]);
  RegisterComponents('JvJANS', [TJvGridPrinter]);
  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents('JvJANS', [TJvJanTreeview]);
  RegisterComponents('JvJANS', [TJvPaintFX]);
  RegisterComponents('JvJANS', [TJvDrawImage]);
  RegisterComponents('JvJANS', [TJvArrayButton]);

  RegisterComponents('JvJANS', [TJvForthScript]);
  RegisterComponents('JvJANS', [TJvTurtle]);
  RegisterComponents('JvJANS', [TJvBitmapButton]);
  RegisterComponents('JvJANS', [TJvSpeller]);

  RegisterComponents('JvJANS', [TJvShapedButton]);

  // Simluator Components
  RegisterComponents('JvJANS SIM', [TJvSimScope]);
  RegisterComponents('JvJANS SIM', [TJvSimIndicator]);
  RegisterComponents('JvJANS SIM', [TJvSimPID]);
  RegisterComponents('JvJANS SIM', [TJvSimPIDLinker]);
  RegisterComponents('JvJANS SIM', [TJvSIMConnector, TJvLogic, TJvSimButton, TJvSimLight, TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents('JvJANS CSV', [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox, TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, 'CSVFileName', TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, 'CSVField', TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, 'CSVField', TCSVFieldProperty);

end;

end.
