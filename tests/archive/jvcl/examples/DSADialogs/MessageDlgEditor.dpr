program MessageDlgEditor;

uses
  Forms, DSADialogsMainFormU, 
  MessageDlgEditorMain in 'MessageDlgEditorMain.pas' {frmMessageDlgEditor},
  MessageDlgEditorSelectIcon in 'MessageDlgEditorSelectIcon.pas' {frmMessageDlgEditorSelectIcon};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(DSA)MessageDlg(Ex) editor';
  Application.CreateForm(TDSADialogsMainForm, DSADialogsMainForm);
  Application.Run;
end.
