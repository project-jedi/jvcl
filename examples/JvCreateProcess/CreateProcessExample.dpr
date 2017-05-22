program CreateProcessExample;

uses
  Forms,
  CreateProcessExampleMainFormU in 'CreateProcessExampleMainFormU.pas' {CreateProcessExampleMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCreateProcessExampleMainForm, CreateProcessExampleMainForm);
  Application.Run;
end.
