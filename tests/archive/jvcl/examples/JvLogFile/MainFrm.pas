unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvLogFile, StdCtrls;

type
  TfrmMain = class(TForm)
    JvLogFile1: TJvLogFile;
    btnStart: TButton;
    btnShow: TButton;
    lblActive: TLabel;
    btnReset: TButton;
    lblInactive: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
    FLogFileName:string;
    procedure StartLogging;
    procedure StopLogging;
    procedure ResetLogging;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLogFileName := ChangeFileExt(Application.ExeName,'.log');
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  if btnStart.Tag = 0 then
    StartLogging
  else
    StopLogging;
end;

procedure TfrmMain.btnShowClick(Sender: TObject);
begin
  if btnStart.Tag = 1 then
    btnStart.Click; // stop logging
  JvLogFile1.ShowLog('Mouse Move Log');
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if btnStart.Tag = 1 then
    JvLogFile1.Add(DateTimeToStr(Now),'Mouse Move',Format('X:%d, Y:%d',[X,Y]));
  Caption := Format('JvLogFile Demo - X:%d, Y:%d',[X,Y]);
end;

procedure TfrmMain.ResetLogging;
begin
  StopLogging;
  DeleteFile(FLogFileName);
  JvLogFile1.Clear;
end;

procedure TfrmMain.StartLogging;
begin
  if FileExists(FLogFileName) then
    JvLogFile1.LoadFromFile(FLogFileName);
  btnStart.Caption := '&Stop';
  btnStart.Tag := 1;
  lblActive.Visible := true;
  lblInactive.Visible := false;
end;

procedure TfrmMain.StopLogging;
begin
  btnStart.Tag := 0;
  lblActive.Visible := false;
  lblInactive.Visible := true;
  btnStart.Caption := '&Start';
  JvLogFile1.SaveToFile(FLogFileName);
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  ResetLogging;
end;

end.
