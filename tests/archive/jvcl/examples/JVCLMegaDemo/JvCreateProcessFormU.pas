unit JvCreateProcessFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, JvSysComp, JvComponent, JvCaptionPanel;

type
  TJvCreateProcessForm = class(TForm)
    JvCreateProcess1: TJvCreateProcess;
    JvCaptionPanel1: TJvCaptionPanel;
    Panel1: TPanel;
    RunBtn: TButton;
    QuitBtn: TButton;
    CloseBtn: TButton;
    TerminateBtn: TButton;
    StopBtn: TButton;
    RichEdit1: TRichEdit;
    procedure RunBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure TerminateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvCreateProcess1Terminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure StopBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AddLogMsg(const Text: String);
    procedure UpdateButtons;
  end;

var
  JvCreateProcessForm: TJvCreateProcessForm;

implementation

{$R *.DFM}

resourcestring
  sProcessStarted = 'Process "%s" started...';
  sProcessTerminated = 'Process "%s" terminated, ExitCode: %.8x';

procedure TJvCreateProcessForm.AddLogMsg(const Text: String);
begin
  RichEdit1.Lines.Add(FormatDateTime('DD.MM.YYYY HH:NN:SS  ', Now) + Text);
end;

procedure TJvCreateProcessForm.RunBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Run;
  UpdateButtons;
  AddLogMsg(Format(sProcessStarted, [JvCreateProcess1.CommandLine]));
end;

procedure TJvCreateProcessForm.CloseBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(False);
end;

procedure TJvCreateProcessForm.QuitBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(True);
end;

procedure TJvCreateProcessForm.TerminateBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Terminate;
end;

procedure TJvCreateProcessForm.UpdateButtons;
begin
  RunBtn.Enabled := JvCreateProcess1.State = psReady;
  CloseBtn.Enabled := JvCreateProcess1.State <> psReady;
  QuitBtn.Enabled := JvCreateProcess1.State <> psReady;
  TerminateBtn.Enabled := JvCreateProcess1.State <> psReady;
  StopBtn.Enabled := JvCreateProcess1.State <> psReady;
end;

procedure TJvCreateProcessForm.FormCreate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TJvCreateProcessForm.JvCreateProcess1Terminate(Sender: TObject; ExitCode: Cardinal);
begin
  UpdateButtons;
  AddLogMsg(Format(sProcessTerminated, [JvCreateProcess1.CommandLine, ExitCode]));
end;

procedure TJvCreateProcessForm.StopBtnClick(Sender: TObject);
begin
  JvCreateProcess1.StopWaiting;
  UpdateButtons;
end;

end.
