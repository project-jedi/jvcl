program ArrowButtonDemo;

uses
  Forms,
  ArrowButtonMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TArrowButtonMainForm, ArrowButtonMainForm);
  Application.Run;
end.
