unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, ExtCtrls, DBCtrls, DB, JvDBFindEdit,
  DBTables, Mask, JvExMask, JvMaskEdit;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Reset: TButton;
    yulFindEdit1: TJvDBFindEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  yulFindEdit1.IgnoreCase := not (CheckBox1.Checked);
  yulFindEdit1.ResetFilter;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    yulFindEdit1.FindMode := fmAnyPos
  else
    yulFindEdit1.FindMode := fmFirstPos;
  yulFindEdit1.ResetFilter;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
begin
  Table1.Open;
  for i := 0 to table1.FieldDefs.Count - 1 do
    ComboBox1.Items.Add(table1.FieldDefs.Items[i].Name);

  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := 0;

  if length(combobox1.Items[ComboBox1.ItemIndex]) > 0 then
    yulFindEdit1.DataField := combobox1.Items[ComboBox1.ItemIndex]
  else
    Showmessage('No field selected');

end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if length(combobox1.Items[ComboBox1.ItemIndex]) > 0 then
  begin
    yulFindEdit1.DataField := combobox1.Items[ComboBox1.ItemIndex];
    if table1.FieldList.FieldByName(yulFindEdit1.DataField) is TDateField then
      yulFindEdit1.EditMask := '!99/99/9999;1;_'
    else
      yulFindEdit1.EditMask := '';

  end
  else
    Showmessage('No field selected');
  yulFindEdit1.Text := '';
end;

procedure TForm1.ResetClick(Sender: TObject);
begin
  yulFindEdit1.ResetFilter;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Table1.Close;
end;

end.

