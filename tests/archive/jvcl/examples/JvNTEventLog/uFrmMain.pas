unit uFrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, JvNTEventLog;

type
  TFrmMain = class(TForm)
    btnRefresh: TButton;
    ListBox1: TListBox;
    ButtonsPanel: TPanel;
    Splitter1: TSplitter;
    ListView1: TListView;
    JvNTEventLog1: TJvNTEventLog;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ReadEvents;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  JvNTEventLog1.ReadEventLogs(ListBox1.Items);
  JvNTEventLog1.Active := True;
end;

procedure TFrmMain.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    JvNTEventLog1.Log := ListBox1.Items[ListBox1.ItemIndex];
    JvNTEventLog1.Source := ListBox1.Items[ListBox1.ItemIndex];
    JvNTEventLog1.Active := True;

    ReadEvents;
  end;
end;

procedure TFrmMain.ReadEvents;
var
  item: TListItem;

begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    JvNTEventLog1.First;
    while not JvNTEventLog1.Eof do
    begin
      item := ListView1.Items.Add;

      item.Caption := JvNTEventLog1.EventRecord.EventType;
      item.SubItems.Add(DateToStr(JvNTEventLog1.EventRecord.DateTime));
      item.SubItems.Add(TimeToStr(JvNTEventLog1.EventRecord.DateTime));
      item.SubItems.Add(JvNTEventLog1.EventRecord.Source);
      item.SubItems.Add(IntToStr(JvNTEventLog1.EventRecord.Category));
      item.SubItems.Add(IntToStr(JvNTEventLog1.EventRecord.ID and $0FFFFFFF));
      item.SubItems.Add(JvNTEventLog1.EventRecord.Username);
      item.SubItems.Add(JvNTEventLog1.EventRecord.Computer);

      JvNTEventLog1.Next;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

end.

