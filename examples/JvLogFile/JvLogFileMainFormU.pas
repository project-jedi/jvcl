unit JvLogFileMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvLogFile, StdCtrls;

type
  TJvLogFileMainForm = class(TForm)
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
  end;

var
  JvLogFileMainForm: TJvLogFileMainForm;

implementation

{$R *.dfm}

procedure TJvLogFileMainForm.FormCreate(Sender: TObject);
begin
  FLogFileName := ChangeFileExt(Application.ExeName,'.log');
end;

procedure TJvLogFileMainForm.btnStartClick(Sender: TObject);
begin
  if btnStart.Tag = 0 then
    StartLogging
  else
    StopLogging;
end;

procedure TJvLogFileMainForm.btnShowClick(Sender: TObject);
begin
  if btnStart.Tag = 1 then
    btnStart.Click; // stop logging
  JvLogFile1.ShowLog('Mouse Move Log');
end;

procedure TJvLogFileMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if btnStart.Tag = 1 then
    JvLogFile1.Add(DateTimeToStr(Now),'Mouse Move',Format('X:%d, Y:%d',[X,Y]));
  Caption := Format('JvLogFile Demo - X:%d, Y:%d',[X,Y]);
end;

procedure TJvLogFileMainForm.ResetLogging;
begin
  StopLogging;
  DeleteFile(FLogFileName);
  JvLogFile1.Clear;
end;

procedure TJvLogFileMainForm.StartLogging;
begin
  if FileExists(FLogFileName) then
    JvLogFile1.LoadFromFile(FLogFileName);
  btnStart.Caption := '&Stop';
  btnStart.Tag := 1;
  lblActive.Visible := true;
  lblInactive.Visible := false;
end;

procedure TJvLogFileMainForm.StopLogging;
begin
  btnStart.Tag := 0;
  lblActive.Visible := false;
  lblInactive.Visible := true;
  btnStart.Caption := '&Start';
  JvLogFile1.SaveToFile(FLogFileName);
end;

procedure TJvLogFileMainForm.btnResetClick(Sender: TObject);
begin
  ResetLogging;
end;

end.
