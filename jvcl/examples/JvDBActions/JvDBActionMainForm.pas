{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [Ralf dot Grenzing@gmx.de]

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}
unit JvDBActionMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, JvCsvData,
  JvExExtCtrls, JvComponent, JvPanel, ComCtrls, JvExComCtrls, JvDBActions,
  JvDateTimePicker, JvDBDateTimePicker, StdCtrls, ActnList, Buttons, DBActns,
  JvExDBGrids, JvDBGrid, ImgList, JvActionsEngine, JvControlActions,
  JvDBActionsEngine, JvDBActionsEngineControlCxTreeList,JvControlActionsEngineCxTreeList;

type
  TJvDBActionMainFrm = class(TForm)
    JvCsvDataSet2: TJvCsvDataSet;
    JvCsvDataSet1NAME: TStringField;
    JvCsvDataSet1ADDRESS: TStringField;
    JvCsvDataSet1ADDRESS2: TStringField;
    JvCsvDataSet1TELEPHONE: TStringField;
    JvCsvDataSet1AGE: TIntegerField;
    JvCsvDataSet1LASTPHONECALL: TDateTimeField;
    JvCsvDataSet1PRIVATENUMBER: TBooleanField;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    JvDatabaseActionList1: TJvDatabaseActionList;
    BitBtn6: TBitBtn;
    JvDBGrid1: TJvDBGrid;
    JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    JvDatabaseFirstAction1: TJvDatabaseFirstAction;
    JvDatabaseLastAction1: TJvDatabaseLastAction;
    JvDatabaseNextAction1: TJvDatabaseNextAction;
    JvDatabasePriorAction1: TJvDatabasePriorAction;
    JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction;
    JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction;
    JvDatabasePositionAction1: TJvDatabasePositionAction;
    JvDatabaseRefreshAction1: TJvDatabaseRefreshAction;
    JvDatabaseInsertAction1: TJvDatabaseInsertAction;
    JvDatabaseCopyAction1: TJvDatabaseCopyAction;
    JvDatabaseEditAction1: TJvDatabaseEditAction;
    JvDatabaseDeleteAction1: TJvDatabaseDeleteAction;
    JvDatabasePostAction1: TJvDatabasePostAction;
    JvDatabaseCancelAction1: TJvDatabaseCancelAction;
    JvDatabaseSimpleAction1: TJvDatabaseSimpleAction;
    JvDatabaseOpenAction1: TJvDatabaseOpenAction;
    JvDatabaseCloseAction1: TJvDatabaseCloseAction;
    JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction;
    JvDatabaseSimpleAction2: TJvDatabaseSimpleAction;
    Button1: TButton;
    BitBtn11: TBitBtn;
    JvDatabaseEditAction2: TJvDatabaseEditAction;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    JvControlCollapseAction1: TJvControlCollapseAction;
    JvControlExpandAction1: TJvControlExpandAction;
    JvControlExportAction1: TJvControlExportAction;
    JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction;
    JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction;
    JvControlPrintAction1: TJvControlPrintAction;
    JvControlCustomizeAction1: TJvControlCustomizeAction;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    procedure cxDBTreeList1Enter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1Enter(Sender: TObject);
    procedure JvDatabaseFirstAction1AfterExecute(Sender: TObject; ControlEngine:
        TJvDatabaseActionBaseControlEngine; DataComponent: TComponent);
    procedure JvDatabaseFirstAction1Execute(Sender: TObject; ControlEngine:
        TJvDatabaseActionBaseControlEngine; DataComponent: TComponent);
    procedure JvDatabaseSimpleAction2CheckEnabled(aDataset: TDataSet;
        aDataComponent: TComponent; aDatabaseControlEngine:
        TJvDatabaseActionBaseControlEngine; var aEnabled: Boolean);
    procedure JvDBGrid1Enter(Sender: TObject);
  end;

var
  JvDBActionMainFrm: TJvDBActionMainFrm;

implementation

{$R *.dfm}

uses JvDynControlEngine,
  JvDynControlEngineDB,
  JvDynControlEngineDBToolscxVGrid,
  JvDynControlEngineDevExpcx,

  JvDynControlEngineDevExpcxDb,
//  JvDynControlEngineVCLDB,
  JvDynControlEngineJVCLDB,
  JvDynControlEngineTools,
  JvDynControlEngineDBTools,
  JvDBActionsEngineControlCxGrid,
  JvDBActionsEngineDatasetAdo,
  JvDBActionsEngineDatasetDevart,
  JvDBActionsEngineDatasetdoa,
  JvDBActionsEngineDatasetDBExpress,
  JvDBActionsEngineDatasetCSVDataset;

procedure TJvDBActionMainFrm.FormCreate(Sender: TObject);
begin
  SetDefaultDynControlEngineDB(DynControlEngineJVCLDB);
  //SetDefaultDynControlEngineDB(DynControlEngineCxDB);
//  JvDatabaseSingleRecordWindowAction1.OnCreateDataControlsEvent := DefaultDataSourceEditDialogCreateDataControlscxVGridEventClass.CreateDataControls;
end;

procedure TJvDBActionMainFrm.DBGrid1Enter(Sender: TObject);
begin
  JvDatabaseActionList1.DataComponent := DBgrid1;
end;


function StateName( MyState: TDataSetState):string;
begin
   case MyState of
      dsInactive     : Result := 'sInactive';
      dsBrowse       : Result := 'dsBrowse';
      dsEdit         : Result := 'dsEdit';
      dsInsert       : Result := 'dsInsert';
      dsSetKey       : Result := 'dsSetKey';
      dsCalcFields   : Result := 'dsCalcFields';
      dsFilter       : Result := 'dsFilter';
      dsNewValue     : Result := 'dsNewValue';
      dsOldValue     : Result := 'dsOldValue';
      dsCurValue     : Result := 'dsCurValue';
      dsBlockRead    : Result := 'dsBlockRead';
      dsInternalCalc : Result := 'dsInternalCalc';
      dsOpening      : Result := 'dsOpening';
   end;
end;

procedure TJvDBActionMainFrm.cxDBTreeList1Enter(Sender: TObject);
begin
//  JvDatabaseActionList1.DataComponent := cxDBTreeList1;
end;

procedure TJvDBActionMainFrm.JvDatabaseFirstAction1AfterExecute(Sender:
    TObject; ControlEngine: TJvDatabaseActionBaseControlEngine; DataComponent:
    TComponent);
begin
//   showmessage(TComponent(Sender).Name+' AfterExecute: '+StateName(JvCsvDataSet2.State));
end;

procedure TJvDBActionMainFrm.JvDatabaseFirstAction1Execute(Sender: TObject;
    ControlEngine: TJvDatabaseActionBaseControlEngine; DataComponent:
    TComponent);
begin
//   showmessage(TComponent(Sender).Name+' OnExecute: '+StateName(JvCsvDataSet2.State));
end;

procedure TJvDBActionMainFrm.JvDatabaseSimpleAction2CheckEnabled(aDataset:
    TDataSet; aDataComponent: TComponent; aDatabaseControlEngine:
    TJvDatabaseActionBaseControlEngine; var aEnabled: Boolean);
begin
  if Enabled and Assigned(aDataset) and Assigned(aDataset.FindField('AGE')) then
    aEnabled := aDataset.FieldByName('AGE').AsFloat >= 50
  else
    aEnabled := False;
end;

procedure TJvDBActionMainFrm.JvDBGrid1Enter(Sender: TObject);
begin
  JvDatabaseActionList1.DataComponent := JvDBGrid1;
end;

end.