{$I JVCL.INC}

unit JvDBReg;

interface

procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, 
  JvMemoryDataset, JvDBDatePickerEdit, JvDBDateTimePicker, JvDBLookupTreeView, JvDBProgressBar, JvDBRichEdit,
  JvDBSpinEdit, JvDBTreeView, JvDBLookup, JvCsvData, JvDBCombobox, JvDBControls, JvDBRadioPanel,
  JvDBEditors, JvDBMemDatasetEditor

  {$IFDEF JV_MIDAS},JvDBRemoteLogin {$ENDIF};

{$R ..\resources\JvDBReg.dcr}

procedure Register;
const
  cKeyField = 'KeyField';
  cListField = 'ListField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
  cStartMasterValue = 'StartMasterValue';
begin
  RegisterComponents(SPaletteDBNonVisual,[TJvMemoryData,
    TJvCSVDataSet {$IFDEF JV_MIDAS},TJvDBRemoteLogin{$ENDIF}
    ]);
  RegisterComponents(SPaletteDBVisual,[
    TJvDBDatePickerEdit, TJvDBProgressBar, TJvDBRichEdit, TJvDBSpinEdit,
    TJvDBLookupList, TJvDBLookupCombo, TJvDBLookupEdit, TJvDBRadioPanel,
    TJvDBCombobox, TJvDBTreeView, TJvDBLookupTreeViewCombo, TJvDBLookupTreeView,
    TJvDBGrid, TJvDBComboEdit, TJvDBDateEdit, TJvDBCalcEdit, TJvDBStatusLabel
    ]);

  RegisterPropertyEditor(TypeInfo(Integer), TJvDBGrid, 'RowsHeight', nil);
  RegisterPropertyEditor(TypeInfo(string), TJvLookupControl, 'LookupField',
    TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupEdit, 'LookupField',
    TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cDetailField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cIconField, TJvDataFieldProperty);

  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cIconField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cIconField, TJvListFieldProperty);

  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);
end;

end.

