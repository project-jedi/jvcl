unit JvDBActionMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, JvCsvData,
  JvExExtCtrls, JvComponent, JvPanel, ComCtrls, JvExComCtrls, JvDBActions,
  JvDateTimePicker, JvDBDateTimePicker, StdCtrls, ActnList, Buttons, DBActns,
  JvExDBGrids, JvDBGrid, ImgList;

type
  TForm1 = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1Enter(Sender: TObject);
    procedure JvDBGrid1Enter(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses JvDynControlEngine, JvDynControlEngineDB, JvDynControlEngineVCLDB, JvDynControlEngineJVCLDB,
  JvDynControlEngineTools, JvDynControlEngineDBTools;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetDefaultDynControlEngineDB(DynControlEngineJVCLDB);
end;

procedure TForm1.DBGrid1Enter(Sender: TObject);
begin
  JvDatabaseActionList1.DataComponent := DBgrid1;
end;

procedure TForm1.JvDBGrid1Enter(Sender: TObject);
begin
  JvDatabaseActionList1.DataComponent := JvDBGrid1;
end;

end.
