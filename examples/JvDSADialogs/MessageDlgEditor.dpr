program MessageDlgEditor;

uses
  Forms,
  MessageDlgEditorMain in 'MessageDlgEditorMain.pas' {frmMessageDlgEditor},
  MessageDlgEditorSelectIcon in 'MessageDlgEditorSelectIcon.pas' {frmMessageDlgEditorSelectIcon};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(DSA)MessageDlg(Ex) editor';
  Application.CreateForm(TfrmMessageDlgEditor, frmMessageDlgEditor);
  Application.Run;
end.
