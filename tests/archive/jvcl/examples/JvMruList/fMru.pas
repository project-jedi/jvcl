unit fMru;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvMru, Spin, JvComponent;

type
  TForm1 = class(TForm)
    JvMruList1: TJvMruList;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    btnOpen: TButton;
    ListBox1: TListBox;
    btnRefresh: TButton;
    btnFirst: TButton;
    btnDeleteFirst: TButton;
    btnAdd: TButton;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    procedure btnOpenClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure JvMruList1EnumText(Sender: TObject; Value: string;
      Index: Integer);
    procedure btnFirstClick(Sender: TObject);
    procedure btnDeleteFirstClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  JvMruList1.SubKey := Edit1.text;
  JvMruList1.MaxItems := SpinEdit1.Value;
  btnRefresh.enabled := true;
  btnFirst.enabled := true;
  btnDeleteFirst.enabled := true;
  btnAdd.enabled := true;
  btnRefresh.Click;
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  ListBox1.Clear;
  JvMruList1.EnumItems;
end;

procedure TForm1.JvMruList1EnumText(Sender: TObject; Value: string;
  Index: Integer);
begin
  ListBox1.Items.Add(Value);
end;

procedure TForm1.btnFirstClick(Sender: TObject);
begin
  ListBox1.clear;
  JvMruList1.GetMostRecentItem;
end;

procedure TForm1.btnDeleteFirstClick(Sender: TObject);
begin
  JvMruList1.DeleteItem(0);
  btnRefresh.Click;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var
  st: string;
begin
  st := InputBox('', 'Enter text', '');
  if st <> '' then
  begin
    JvMruList1.AddString(st);
    btnRefresh.Click;
  end;
end;

end.

