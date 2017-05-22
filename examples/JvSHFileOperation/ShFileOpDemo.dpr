program ShFileOpDemo;

uses
  Forms,
  JvShFileOperationMainFormU in 'JvShFileOperationMainFormU.pas' {JvShFileOperationMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvShFileOperationMainForm, JvShFileOperationMainForm);
  Application.Run;
end.
