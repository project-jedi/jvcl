unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, JvExStdCtrls, JvEdit, JvCombobox,  ExtCtrls,
  DB, DBGrids, JvExDBGrids, JvDBGrid, DBTables, JvDBSearchEdit,
  JvDBSearchComboBox;

type
  TForm1 = class(TForm)
    tbl1: TTable;
    JvDBGrid1: TJvDBGrid;
    dts1: TDataSource;
    pnl1: TPanel;
    JvDBSearchComboBox1: TJvDBSearchComboBox;
    JvDBSearchEdit1: TJvDBSearchEdit;
    btn1: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    CheckBox1: TCheckBox;
    procedure btn1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  tbl1.Active := true;
  btn1.Enabled := false;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  JvDBSearchEdit1.ClearOnEnter := CheckBox1.Checked;
end;

end.
