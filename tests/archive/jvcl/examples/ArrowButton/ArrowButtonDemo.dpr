program ArrowButtonDemo;

uses
  Forms,
  ArrowButtonMainFormU in 'ArrowButtonMainFormU.pas' {ArrowButtonMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TArrowButtonMainForm, ArrowButtonMainForm);
  Application.Run;
end.
