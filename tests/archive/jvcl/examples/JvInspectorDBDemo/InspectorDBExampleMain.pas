{ NOTE: this demo requires that the Borland sample data is installed and that the
  DBDEMOS folder is available }
unit InspectorDBExampleMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, Grids, DBGrids, JvInspector, JvInspDB, StdCtrls,
  JvComponent;

type
  TForm1 = class(TForm)
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspector1: TJvInspector;
    dbInspector: TDatabase;
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    btnOpenCloseDS: TButton;
    btnClose: TButton;
    Table1EmpNo: TIntegerField;
    Table1LastName: TStringField;
    Table1FirstName: TStringField;
    Table1PhoneExt: TStringField;
    Table1HireDate: TDateTimeField;
    Table1Salary: TFloatField;
    procedure FormShow(Sender: TObject);
    procedure btnOpenCloseDSClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  TJvInspectorDBData.New(JvInspector1.Root, DataSource1, ['LastName', 'FirstName', 'Salary']);
end;

procedure TForm1.btnOpenCloseDSClick(Sender: TObject);
begin
  Table1.Active := not Table1.Active;
  if not Table1.Active then
    btnOpenCloseDS.Caption := 'Open table'
  else
    btnOpenCloseDS.Caption := 'Close table'
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
