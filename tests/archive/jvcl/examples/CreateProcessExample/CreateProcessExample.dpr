program CreateProcessExample;

uses
  Forms, CreateProcessExampleMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCreateProcessExampleMainForm, CreateProcessExampleMainForm);
  Application.Run;
end.
