unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, DB, StdCtrls, JvExStdCtrls,
  JvEdit, JvHTControls, JvDBHTLabel, JvExForms, JvCsvData, JvComponent,
  JvDragDrop, JvFormPlacement;

type
  TForm1 = class(TForm)
    JvDBHTLabel1: TJvDBHTLabel;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    memFormat: TMemo;
    Button1: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    Edit1: TJvEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvDBHTLabel1.Mask := StringReplace(memFormat.Lines.Text,#13#10,' ',[rfReplaceAll]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FileExists(JvCsvDataSet1.Filename) then
    ShowMessage('Data file not found!')
  else
    JvCsvDataSet1.Open;
  memFormat.Lines.Text := JvDBHTLabel1.Mask;
  Edit1.AutoHint := true;
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  Edit1.Text := JvCsvDataSet1.FieldByName('Filename').AsString;
end;

end.
