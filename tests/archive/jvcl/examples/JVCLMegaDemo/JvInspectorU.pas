{ NOTE: this demo requires that the Borland sample data is installed and that the
  DBDEMOS folder is available }
unit JvInspectorU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, Grids, DBGrids, JvInspector, JvInspDB, StdCtrls,
  JvComponent;

type
  TJvInspectorDBForm = class(TForm)
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspector1: TJvInspector;
    dbInspector: TDatabase;
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    btnOpenCloseDS: TButton;
    Table1EmpNo: TIntegerField;
    Table1LastName: TStringField;
    Table1FirstName: TStringField;
    Table1PhoneExt: TStringField;
    Table1HireDate: TDateTimeField;
    Table1Salary: TFloatField;
    procedure FormShow(Sender: TObject);
    procedure btnOpenCloseDSClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TJvInspectorDBForm.FormShow(Sender: TObject);
begin
  TJvInspectorDBData.New(JvInspector1.Root, DataSource1, ['LastName', 'FirstName', 'Salary']);
end;

procedure TJvInspectorDBForm.btnOpenCloseDSClick(Sender: TObject);
begin
  Table1.Active := not Table1.Active;
  if not Table1.Active then
    btnOpenCloseDS.Caption := 'Open table'
  else
    btnOpenCloseDS.Caption := 'Close table'
end;

end.
