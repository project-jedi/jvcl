program FindReplaceDemo;

uses
  Forms,
  FindReplaceMainFormU in 'FindReplaceMainFormU.pas' {FindReplaceMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFindReplaceMainForm, FindReplaceMainForm);
  Application.Run;
end.
