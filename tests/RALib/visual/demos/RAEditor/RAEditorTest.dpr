program RAEditorTest;

uses
  Forms,
  fRAEditorTest in 'fRAEditorTest.pas' {Editor};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TEditor, Editor);
  Application.Run;
end.
