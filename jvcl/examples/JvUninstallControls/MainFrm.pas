unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvUninstallControls;

type
  TForm1 = class(TForm)
    JvUninstallComboBox1: TJvUninstallComboBox;
    JvUninstallListBox1: TJvUninstallListBox;
    chkListShowAll: TCheckBox;
    chkComboShowAll: TCheckBox;
    memListInfo: TMemo;
    memComboInfo: TMemo;
    chkListSorted: TCheckBox;
    chkComboSorted: TCheckBox;
    procedure chkListShowAllClick(Sender: TObject);
    procedure chkComboShowAllClick(Sender: TObject);
    procedure JvUninstallListBox1Click(Sender: TObject);
    procedure JvUninstallComboBox1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chkListSortedClick(Sender: TObject);
    procedure chkComboSortedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chkListShowAllClick(Sender: TObject);
begin
  JvUninstallListBox1.ShowAll := chkListShowAll.Checked;
end;

procedure TForm1.chkComboShowAllClick(Sender: TObject);
begin
  JvUninstallComboBox1.ShowAll := chkComboShowAll.Checked;
end;

procedure TForm1.JvUninstallListBox1Click(Sender: TObject);
begin
  with JvUninstallListBox1 do
    memListInfo.Lines := Properties;
end;

procedure TForm1.JvUninstallComboBox1Click(Sender: TObject);
begin
  with JvUninstallComboBox1 do
    memComboInfo.Lines := Properties;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  JvUninstallListBox1.Width := ClientWidth div 2 - 14;
  memListInfo.Width := JvUninstallListBox1.Width;
  chkListShowAll.Top := JvUninstallListBox1.Top + JvUninstallListBox1.height + 7;
  chkListSorted.Top := chkListShowAll.Top;
  chkListSorted.Left := chkListShowAll.Left + chkListShowAll.Width + 24;

  JvUninstallComboBox1.Left := JvUninstallListBox1.Left + JvUninstallListBox1.Width + 14;
  JvUninstallComboBox1.Width := ClientWidth div 2 - 14;

  chkComboShowAll.Left := JvUninstallComboBox1.Left;
  chkComboSorted.Left := chkComboShowAll.Left + chkComboShowAll.Width + 24;
  memComboInfo.Left := JvUninstallComboBox1.Left;
  memComboInfo.Width := JvUninstallComboBox1.Width;


end;

procedure TForm1.chkListSortedClick(Sender: TObject);
begin
  JvUninstallListBox1.Sorted := chkListSorted.Checked;
end;

procedure TForm1.chkComboSortedClick(Sender: TObject);
begin
  JvUninstallComboBox1.Sorted := chkComboSorted.Checked;
end;

end.
