program FindReplaceDemo;

uses
  Forms, FindReplaceMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFindReplaceMainForm, FindReplaceMainForm);
  Application.Run;
end.
