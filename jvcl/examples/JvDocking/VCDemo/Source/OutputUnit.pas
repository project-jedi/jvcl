unit OutputUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, Tabs, StdCtrls, ComCtrls, ExtCtrls;

type
  TOutputForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TabSet1: TTabSet;
    ScrollBar1: TScrollBar;
    Memo1: TMemo;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure Panel2CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OutputForm: TOutputForm;

implementation

uses Main;

{$R *.dfm}

procedure TOutputForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Output_ToolButton.Down := False;
  MainForm.Output1.Checked := False;
  MainForm.Output_PopupItem.Checked := False;
end;

procedure TOutputForm.Panel2CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if Panel2.Width = NewWidth then Exit;
  if Panel2.Width > 0 then
  begin
    if (TabSet1.Width > 0) and (TabSet1.Width < Panel2.Width) then
      TabSet1.Width := Round(TabSet1.Width * NewWidth / Panel2.Width)
    else
      TabSet1.Width := Panel2.Width div 2;
  end;
  Splitter1.Left := TabSet1.Width;
end;

procedure TOutputForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Output_ToolButton.Down := True;
  MainForm.Output1.Checked := True;
  MainForm.Output_PopupItem.Checked := True;
end;

end.
