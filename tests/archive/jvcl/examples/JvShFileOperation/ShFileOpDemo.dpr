program ShFileOpDemo;

uses
  Forms, JvShFileOperationMainFormU;  

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvShFileOperationMainForm, JvShFileOperationMainForm);
  Application.Run;
end.
