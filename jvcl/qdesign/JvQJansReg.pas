{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

// No licence info because this should be merged with other Registration Unit

{$I jvcl.inc}

unit JvQJansReg;

interface

uses
  Classes, QControls, 
  DesignIntf, DesignEditors,
  CLXEditors, 
  JvQDsgnConsts,
  JvQJanTreeView,
  JvQMarkupLabel, JvQMarkupViewer, JvQSAL, JvQSALCore, JvQSALMath,
  JvQYearGrid, JvQTracker, JvQAirBrush, JvQGridFilter, JvQGridPrinter,
  JvQArrayButton, JvQForth, JvQTurtle, JvQPaintFX, JvQDrawImage,
  JvQBitmapButton, JvQSimScope, JvQSimIndicator, JvQSimPID, JvQSIMPIDLinker,
  JvQSimLogic, JvQSpellerForm, JvQCSVBaseControls, JvQCsvBaseEditor;

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
  GroupDescendentsWith(TJvSALCore, TControl);
  GroupDescendentsWith(TJvSALMath, TControl);
  GroupDescendentsWith(TJvGridFilter, TControl);
  GroupDescendentsWith(TJvGridPrinter, TControl);
  GroupDescendentsWith(TJvPaintFX, TControl);
  GroupDescendentsWith(TJvForthScript, TControl);
  GroupDescendentsWith(TJvTurtle, TControl);
  GroupDescendentsWith(TJvSimPIDLinker, TControl);
  GroupDescendentsWith(TJvSpeller, TControl); 

  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents(RsPaletteJans, [TJvMarkupLabel, TJvMarkupViewer, TJvSAL, 
    TJvSALCore, TJvSALMath, TJvYearGrid, TJvAirBrush, TJvTracker,
    TJvGridFilter, TJvGridPrinter,
    TJvJanTreeview,
    TJvPaintFX, TJvDrawImage,
    TJvArrayButton, TJvForthScript, TJvTurtle, TJvBitmapButton, TJvSpeller ]);
  // Simulator Components
  RegisterComponents(RsPaletteJansSim, [TJvSimScope, TJvSimIndicator, TJvSimPID,
    TJvSimPIDLinker, TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight,
    TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents(RsPaletteJansCsv, [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox,
    TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, cCSVFieldName, TCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, cCSVField, TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, cCSVField, TCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, cCSVField, TCSVFieldProperty);

end;

end.
