// No licence info because this should be merged with other Registration Unit
{$I JEDI.INC}
unit jvJansReg;

interface
uses Classes, DesignIntf,jvMarkupLabel,jvMarkupViewer, jvSAL, jvSticker, jvSALCORE, jvSALMath,
     jvYearGrid, jvTracker, jvAirBrush, jvGridFilter, jvGridPrinter,
     jvJanTreeView,jvArrayButton,jvForth,jvTurtle, jvPaintFX,jvDrawImage,
     jvBitmapButton,jvSimScope,jvSimIndicator,jvSimPID, jvSIMPIDLinker,
     jvSimLogic,jvSpeller,jvShappedButton,jvCSVBase,jvCSVBaseProp;

Procedure Register;
implementation

Procedure Register;
begin
    RegisterComponents('jv JANS', [TJvMarkupLabel]);
    RegisterComponents('jv JANS', [TJvMarkupViewer]);
    RegisterComponents('jv JANS', [TJvSAL]);
    RegisterComponents('jv JANS', [TjvSticker]);
    RegisterComponents('jv JANS', [TJvSALCore]);
    RegisterComponents('jv JANS', [TJvSALMath]);
    RegisterComponents('jv JANS', [TjvYearGrid]);
    RegisterComponents('jv JANS', [TjvAirBrush]);
    RegisterComponents('jv JANS', [TjvTracker]);
    RegisterComponents('jv JANS', [TjvGridFilter]);
    RegisterComponents('jv JANS', [TjvGridPrinter]);
//TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TjvJanTreeView
    RegisterComponents('jv JANS', [TjvJanTreeview]);
    RegisterComponents('jv JANS', [TjvPaintFX]);
    RegisterComponents('jv JANS', [TjvDrawImage]);     
    RegisterComponents('jv JANS', [TjvArrayButton]);

    RegisterComponents('jv JANS', [TJvForthScript]);
    RegisterComponents('jv JANS', [TjvTurtle]);
    RegisterComponents('jv JANS', [TjvBitmapButton]);
    RegisterComponents('jv JANS', [TJvSpeller]);

    RegisterComponents('jv JANS', [TjvShappedButton]);    

// Simluator Components
    RegisterComponents('jv JANS SIM', [TjvSimScope]);
    RegisterComponents('jv JANS SIM', [TjvSimIndicator]);
    RegisterComponents('jv JANS SIM', [TjvSimPID]);
    RegisterComponents('jv JANS SIM', [TjvSimPIDLinker]);
    RegisterComponents('jv JANS SIM', [TjvSIMConnector,TjvLogic,TjvSimButton,TjvSimLight, TjvSimLogicBox, TjvSimReverse]);
// CSV Components
    RegisterComponents('jv JANS CSV', [TjvCSVBase, TjvCSVEdit, TjvCSVComboBox, TjvCSVCheckBox, TjvCSVNavigator]);
    RegisterPropertyEditor(TypeInfo(string), TjvCSVBase, 'CSVFileName',  TCSVFileNameProperty);
    RegisterPropertyEditor(TypeInfo(string), TjvCSVEdit, 'CSVField',  TCSVFieldProperty);
    RegisterPropertyEditor(TypeInfo(string), TjvCSVComboBox, 'CSVField',  TCSVFieldProperty);
    RegisterPropertyEditor(TypeInfo(string), TjvCSVCheckBox, 'CSVField',  TCSVFieldProperty);

end;






end.
