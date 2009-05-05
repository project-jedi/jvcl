unit FrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvMailslots, StdCtrls, JvComponentBase, ExtCtrls;

type
  TFormMain = class(TForm)
    msServer: TJvMailSlotServer;
    msClient: TJvMailSlotClient;
    grpClient: TGroupBox;
    edtClientText: TEdit;
    btnClientSendText: TButton;
    pnlBottom: TPanel;
    bvlSplitter: TBevel;
    btnQuit: TButton;
    grpServer: TGroupBox;
    edtServerText: TEdit;
    lblInfo: TLabel;
    procedure btnQuitClick(Sender: TObject);
    procedure btnClientSendTextClick(Sender: TObject);
    procedure msServerNewMessage(Sender: TObject; MessageText: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.btnClientSendTextClick(Sender: TObject);
begin
  msClient.Send(edtClientText.Text);
end;

procedure TFormMain.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  msServer.Open;
end;

procedure TFormMain.msServerNewMessage(Sender: TObject; MessageText: string);
begin
  edtServerText.Text := MessageText;
end;

end.
