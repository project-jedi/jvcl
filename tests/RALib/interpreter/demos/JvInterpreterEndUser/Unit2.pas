unit Unit2;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, Forms, DBCtrls, DB, DBGrids, Grids, ExtCtrls, JvRegAuto,
  Buttons;

type
  TForm2 = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator: TDBNavigator;
    Panel1: TPanel;
    Panel2: TPanel;
    RegAuto1: TJvRegAuto;
    GradButton1: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    procedure GradButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

uses Unit1, fReports;

procedure TForm2.GradButton1Click(Sender: TObject);
begin
  fReports.Show;
end;

end.
