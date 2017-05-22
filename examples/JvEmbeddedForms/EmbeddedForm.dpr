program EmbeddedForm;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  EmbeddedFormUnit in 'EmbeddedFormUnit.pas' {FirstEmbeddedForm},
  DeepEmbeddedFormUnit in 'DeepEmbeddedFormUnit.pas' {DeepEmbeddedForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'JvEmbedded Forms Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDeepEmbeddedForm, DeepEmbeddedForm);
  Application.CreateForm(TFirstEmbeddedForm, FirstEmbeddedForm);
  Application.Run;
end.

