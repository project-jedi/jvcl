unit JvMruListMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvMru, Spin, JvComponent;

type
  TJvMruListMainForm = class(TForm)
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
  end;

var
  JvMruListMainForm: TJvMruListMainForm;

implementation

{$R *.DFM}

procedure TJvMruListMainForm.btnOpenClick(Sender: TObject);
begin
  JvMruList1.SubKey := Edit1.text;
  JvMruList1.MaxItems := SpinEdit1.Value;
  btnRefresh.enabled := true;
  btnFirst.enabled := true;
  btnDeleteFirst.enabled := true;
  btnAdd.enabled := true;
  btnRefresh.Click;
end;

procedure TJvMruListMainForm.btnRefreshClick(Sender: TObject);
begin
  ListBox1.Clear;
  JvMruList1.EnumItems;
end;

procedure TJvMruListMainForm.JvMruList1EnumText(Sender: TObject; Value: string;
  Index: Integer);
begin
  ListBox1.Items.Add(Value);
end;

procedure TJvMruListMainForm.btnFirstClick(Sender: TObject);
begin
  ListBox1.clear;
  JvMruList1.GetMostRecentItem;
end;

procedure TJvMruListMainForm.btnDeleteFirstClick(Sender: TObject);
begin
  JvMruList1.DeleteItem(0);
  btnRefresh.Click;
end;

procedure TJvMruListMainForm.btnAddClick(Sender: TObject);
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

