unit JvMruListU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvMru, StdCtrls, Spin, ExtCtrls, JvCaptionPanel;

type
  TJvMruListForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    btnOpen: TButton;
    SpinEdit1: TSpinEdit;
    ListBox1: TListBox;
    btnRefresh: TButton;
    btnFirst: TButton;
    btnDeleteFirst: TButton;
    btnAdd: TButton;
    JvMruList1: TJvMruList;
    procedure btnOpenClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnDeleteFirstClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure JvMruList1EnumText(Sender: TObject; Value: String; Index: Integer);
  end;


implementation

{$R *.dfm}

procedure TJvMruListForm.btnOpenClick(Sender: TObject);
begin
  JvMruList1.SubKey := Edit1.text;
  JvMruList1.MaxItems := SpinEdit1.Value;
  btnRefresh.enabled := true;
  btnFirst.enabled := true;
  btnDeleteFirst.enabled := true;
  btnAdd.enabled := true;
  btnRefresh.Click;

end;

procedure TJvMruListForm.btnRefreshClick(Sender: TObject);
begin
  ListBox1.Clear;
  JvMruList1.EnumItems;
end;

procedure TJvMruListForm.btnFirstClick(Sender: TObject);
begin
  ListBox1.clear;
  JvMruList1.GetMostRecentItem;
end;

procedure TJvMruListForm.btnDeleteFirstClick(Sender: TObject);
begin
  JvMruList1.DeleteItem(0);
  btnRefresh.Click;
end;

procedure TJvMruListForm.btnAddClick(Sender: TObject);
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

procedure TJvMruListForm.JvMruList1EnumText(Sender: TObject; Value: String;
  Index: Integer);
begin
  ListBox1.Items.Add(Value);
end;

end.
