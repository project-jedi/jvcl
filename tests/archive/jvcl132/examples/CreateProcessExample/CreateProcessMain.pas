unit CreateProcessMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, JvSysComp, JvComponent;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    RichEdit1: TRichEdit;
    RunBtn: TButton;
    QuitBtn: TButton;
    CloseBtn: TButton;
    TerminateBtn: TButton;
    JvCreateProcess1: TJvCreateProcess;
    StopBtn: TButton;
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
  MainForm: TMainForm;

implementation

{$R *.DFM}

resourcestring
  sProcessStarted = 'Process "%s" started...';
  sProcessTerminated = 'Process "%s" terminated, ExitCode: %.8x';

procedure TMainForm.AddLogMsg(const Text: String);
begin
  RichEdit1.Lines.Add(FormatDateTime('DD.MM.YYYY HH:NN:SS  ', Now) + Text);
end;

procedure TMainForm.RunBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Run;
  UpdateButtons;
  AddLogMsg(Format(sProcessStarted, [JvCreateProcess1.CommandLine]));
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(False);
end;

procedure TMainForm.QuitBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(True);
end;

procedure TMainForm.TerminateBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Terminate;
end;

procedure TMainForm.UpdateButtons;
begin
  RunBtn.Enabled := JvCreateProcess1.State = psReady;
  CloseBtn.Enabled := JvCreateProcess1.State <> psReady;
  QuitBtn.Enabled := JvCreateProcess1.State <> psReady;
  TerminateBtn.Enabled := JvCreateProcess1.State <> psReady;
  StopBtn.Enabled := JvCreateProcess1.State <> psReady;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMainForm.JvCreateProcess1Terminate(Sender: TObject; ExitCode: Cardinal);
begin
  UpdateButtons;
  AddLogMsg(Format(sProcessTerminated, [JvCreateProcess1.CommandLine, ExitCode]));
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  JvCreateProcess1.StopWaiting;
  UpdateButtons;
end;

end.
