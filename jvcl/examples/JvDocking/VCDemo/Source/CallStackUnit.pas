unit CallStackUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvDockControlForm;

type
  TCallStackForm = class(TForm)
    Memo1: TMemo;
    lbDockClient1: TJvDockClient;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CallStackForm: TCallStackForm;

implementation

uses Main;

{$R *.dfm}

procedure TCallStackForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.CallStack_ToolButton.Down := False;
  MainForm.CallStack1.Checked := False;
  MainForm.CallStack_PopupItem.Checked := False;
end;

procedure TCallStackForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.CallStack_ToolButton.Down := True;
  MainForm.CallStack1.Checked := True;
  MainForm.CallStack_PopupItem.Checked := True;
end;

end.
