unit RaInterpreterEndUserMainFormU;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, Forms, DBCtrls, DB, DBGrids, Grids, ExtCtrls, JvRegAuto,
  Buttons, JvComponent;

type
  TRaInterpreterEndUserMainForm = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator: TDBNavigator;
    Panel1: TPanel;
    Panel2: TPanel;
    RegAuto1: TJvRegAuto;
    GradButton1: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    procedure GradButton1Click(Sender: TObject);
  end;

var
  RaInterpreterEndUserMainForm: TRaInterpreterEndUserMainForm;

implementation

{$R *.DFM}

uses fReports;

procedure TRaInterpreterEndUserMainForm.GradButton1Click(Sender: TObject);
begin
  fReports.Show;
end;

end.
