unit DSAExamplesProgressDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvProgressBar;

type
  TMsgProgressCallback = function(const Position, Max: Integer): Integer of object;

  TfrmDSAExamplesProgressDlg = class(TForm)
    lblItemCount: TLabel;
    edItems: TEdit;
    lblNote: TLabel;
    btnStart: TButton;
    btnCancel: TButton;
    pbProgress: TJvProgressBar;
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    DoMessage: TMsgProgressCallback;
  end;

procedure DoProgress(const MessageCallback: TMsgProgressCallback);

implementation

uses
  JvDSADialogs;

{$R *.DFM}

procedure DoProgress(const MessageCallback: TMsgProgressCallback);
begin
  with TfrmDSAExamplesProgressDlg.Create(Screen.ActiveForm) do
  try
    DoMessage := MessageCallback;
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmDSAExamplesProgressDlg }

procedure TfrmDSAExamplesProgressDlg.FormResize(Sender: TObject);
var
  TotCtrlW: Integer;
  Distance: Integer;
begin
  TotCtrlW := btnCancel.Left + btnCancel.Width - btnStart.Left;
  Distance := btnCancel.Left - btnStart.Left;
  btnStart.Left := (ClientWidth - TotCtrlW) div 2;
  btnCancel.Left := btnStart.Left + Distance;
end;

procedure TfrmDSAExamplesProgressDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := btnStart.Enabled;
end;

procedure TfrmDSAExamplesProgressDlg.btnStartClick(Sender: TObject);
var
  I: Integer;
begin
  btnStart.Enabled := False;
  pbProgress.Position := 0;
  pbProgress.Max := StrToInt(edItems.Text);
  Application.ProcessMessages;
  try
    I := 1;
    while I <= pbProgress.Max do
    begin
      pbProgress.Position := I;
      Application.ProcessMessages;
      if DoMessage(I, pbProgress.Max) in [mrNo, mrCancel] then
      begin
        ShowMessage('Canceled.', dckActiveForm);
        Break;
      end;
      Inc(I);
    end;
  finally
    pbProgress.Position := 0;
    btnStart.Enabled := True;
    DSAQueueStore.Clear;
  end;
end;

end.
