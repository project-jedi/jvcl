unit GenerationMessagesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmGenMessages = class(TForm)
    bbtOk: TBitBtn;
    memMessages: TMemo;
    procedure FormShow(Sender: TObject);
    procedure bbtOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure AddMessage(Msg : string);

var
  frmGenMessages: TfrmGenMessages;

implementation

{$R *.dfm}
procedure AddMessage(Msg : string);
begin
  frmGenMessages.memMessages.Lines.Add(msg);
  SendMessage(frmGenMessages.memMessages.Handle, EM_LINESCROLL, frmGenMessages.memMessages.Lines.Count, 0);
end;

procedure TfrmGenMessages.FormShow(Sender: TObject);
begin
  memMessages.Clear;
end;

procedure TfrmGenMessages.bbtOkClick(Sender: TObject);
begin
  Hide;
end;

end.
