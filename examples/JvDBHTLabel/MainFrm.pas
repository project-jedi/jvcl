unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, DB, StdCtrls, JvExStdCtrls,
  JvHTControls, JvCsvData, JvDBHTLabel;

type
  TForm1 = class(TForm)
    JvDBHTLabel1: TJvDBHTLabel;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    memFormat: TMemo;
    Button1: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
end;

end.
