unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvRegAuto, Grids, DBGrids, Db, DBTables;

type
  TForm2 = class(TForm)
    RegAuto1: TJvRegAuto;
    GroupBox2: TGroupBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    DBGrid1: TDBGrid;
    pnlStatus: TPanel;
    Button3: TButton;
    Label3: TLabel;
    Table1: TTable;
    procedure FormCreate(Sender: TObject);
    procedure Table1ActiveChanged(DataSet: TDataSet);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit3, Unit4;

{$R *.DFM}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := Name;
end;

procedure TForm2.Table1ActiveChanged(DataSet: TDataSet);
begin
  if Table1.Active then
    pnlStatus.Caption := 'Open'
  else
    pnlStatus.Caption := 'Close'
end;

procedure TForm2.CheckBox2Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
//  if Sender.Checked then
    DataModule1.Table1.Open
  else
    DataModule1.Table1.Close
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  DataModule1.Table1.AppendRecord([1000, 'Hello']);
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  Form3: TForm;
begin
  Form3 := TForm3.Create(Application);
  Form3.ShowModal;
  Form3.Free;
end;


end.
