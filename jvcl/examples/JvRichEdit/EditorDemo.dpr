program EditorDemo;

{$R 'EditorDemo.res' 'EditorDemo.rc'}

uses
  Forms,
  JvExceptionForm,
  EditorMainFormU in 'EditorMainFormU.pas' {EditorMainForm},
  ParagraphFormatFormU in 'ParagraphFormatFormU.pas' {ParagraphFormatForm};

begin
  Application.Initialize;
  JvErrorIntercept;
  Application.Title := 'RX RichEdit Demo';
  Application.CreateForm(TEditorMainForm, EditorMainForm);
  Application.Run;
end.
