unit JvDynControlDatabaseMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, JvCsvData,
  JvExExtCtrls, JvComponent, JvPanel, ComCtrls, JvExComCtrls,
  JvDateTimePicker, JvDBDateTimePicker, StdCtrls;

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
    JvPanel1: TJvPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  DefaultDynControlEngineDB.CreateControlsFromDatasourceOnControl (Datasource1, JvPanel1, False, False, 50, 0,250);
  DefaultDynControlEngineDB.CreateDBGridControl(self, JvPanel1, '', Datasource1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowDatasourceEditDialog (Datasource1,'', '&Post', '&Cancel', True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowDatasourceEditDialog (Datasource1,'', '&Post', '&Cancel', True, DynControlEngineVCLDB);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowDatasourceEditDialog (Datasource1,'', '&Post', '&Cancel', True, DynControlEngineJVCLDB);
end;

end.
