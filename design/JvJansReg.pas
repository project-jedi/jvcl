// No licence info because this should be merged with other Registration Unit

{$I jvcl.inc}

unit JvJansReg;

interface

uses
  Classes, Controls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvMarkupLabel, JvMarkupViewer, JvSAL, JvSticker, JvSALCore, JvSALMath,
  JvYearGrid, JvTracker, JvAirBrush, JvGridFilter, JvGridPrinter,
  JvJanTreeView, JvArrayButton, JvForth, JvTurtle, JvPaintFX, JvDrawImage,
  JvBitmapButton, JvSimScope, JvSimIndicator, JvSimPID, JvSIMPIDLinker,
  JvSimLogic, JvSpellerForm, JvShapedButton, JvCSVBaseControls, JvCSVBaseEditor;

procedure Register;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvJansReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvJansReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  cCSVField = 'CSVField';
  cCSVFieldName = 'CSVFieldName';
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvSALCore, TControl);
  GroupDescendentsWith(TJvSALMath, TControl);
  GroupDescendentsWith(TJvGridFilter, TControl);
  GroupDescendentsWith(TJvGridPrinter, TControl);
  GroupDescendentsWith(TJvPaintFX, TControl);
  GroupDescendentsWith(TJvForthScript, TControl);
  GroupDescendentsWith(TJvTurtle, TControl);
  GroupDescendentsWith(TJvSimPIDLinker, TControl);
  {$ENDIF COMPILER7_UP}

  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents('Jv Jans', [TJvMarkupLabel, TJvMarkupViewer, TJvSAL,
    TJvSticker, TJvSALCore, TJvSALMath, TJvYearGrid, TJvAirBrush, TJvTracker,
    TJvGridFilter, TJvGridPrinter, TJvJanTreeview, TJvPaintFX, TJvDrawImage,
    TJvArrayButton, TJvForthScript, TJvTurtle, TJvBitmapButton, TJvSpeller, TJvShapedButton]);
  // Simulator Components
  RegisterComponents('Jv Jans SIM', [TJvSimScope, TJvSimIndicator, TJvSimPID,
    TJvSimPIDLinker, TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight,
    TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents('Jv Jans CSV', [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox,
    TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, cCSVFieldName, TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, cCSVField, TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, cCSVField, TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, cCSVField, TCSVFieldProperty);

end;

end.
