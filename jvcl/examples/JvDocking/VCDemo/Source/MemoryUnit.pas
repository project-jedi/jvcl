unit MemoryUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, JvDockControlForm;

type
  TMemoryForm = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure Panel1Resize(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoryForm: TMemoryForm;

implementation

uses Main;

{$R *.dfm}

procedure TMemoryForm.Panel1Resize(Sender: TObject);
begin
  Edit1.Width := Panel1.Width - Edit1.Left;
end;

procedure TMemoryForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Memory_ToolButton.Down := False;
  MainForm.Memory1.Checked := False;
  MainForm.Memory_PopupItem.Checked := False;
end;

procedure TMemoryForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Memory_ToolButton.Down := True;
  MainForm.Memory1.Checked := True;
  MainForm.Memory_PopupItem.Checked := True;
end;

end.
