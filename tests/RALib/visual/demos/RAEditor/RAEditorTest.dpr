program RAEditorTest;

uses
  Forms,
  fJvEditorTest in 'fJvEditorTest.pas' {frmEditor};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.Run;
end.
