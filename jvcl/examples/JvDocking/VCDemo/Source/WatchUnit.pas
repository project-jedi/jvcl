unit WatchUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, Tabs, JvDockControlForm, Menus,
  ComCtrls;

type
  TWatchForm = class(TForm)
    Panel2: TPanel;
    TabSet1: TTabSet;
    lbDockClient1: TJvDockClient;
    Shape1: TShape;
    ListView1: TListView;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure ListView1Resize(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WatchForm: TWatchForm;

implementation

uses Main;

{$R *.dfm}

procedure TWatchForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Watch_ToolButton.Down := False;
  MainForm.Watch1.Checked := False;
  MainForm.Watch_PopupItem.Checked := False;
end;

procedure TWatchForm.ListView1Resize(Sender: TObject);
begin
  ListView1.Columns[1].Width := ListView1.Width - ListView1.Columns[0].Width;
end;

procedure TWatchForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Watch_ToolButton.Down := True;
  MainForm.Watch1.Checked := True;
  MainForm.Watch_PopupItem.Checked := True;
end;

end.
