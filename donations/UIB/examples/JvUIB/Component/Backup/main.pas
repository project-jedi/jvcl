unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
   SysUtils, Classes, JvUIB;


type
  TMainForm = class(TForm)
    Backup: TJvUIBBackup;
    Log: TMemo;
    Go: TButton;
    procedure BackupVerbose(Sender: TObject; Message: String);
    procedure GoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.BackupVerbose(Sender: TObject; Message: String);
begin
  Log.Lines.Add(Message);
end;

procedure TMainForm.GoClick(Sender: TObject);
begin
  Log.Clear;
  Backup.Run;
end;

end.
