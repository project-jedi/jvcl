unit RegistersUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvDockControlForm, ExtCtrls;

type
  TRegistersForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegistersForm: TRegistersForm;

implementation

uses Main;

{$R *.dfm}

procedure TRegistersForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Registers_ToolButton.Down := False;
  MainForm.Register1.Checked := False;
  MainForm.Registers_PopupItem.Checked := False;
end;

procedure TRegistersForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Registers_ToolButton.Down := True;
  MainForm.Register1.Checked := True;
  MainForm.Registers_PopupItem.Checked := True;
end;

end.
