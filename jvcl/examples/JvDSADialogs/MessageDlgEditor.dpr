program MessageDlgEditor;

uses
  Forms,
  MessageDlgEditorMain in 'MessageDlgEditorMain.pas' {frmMessageDlgEditor},
  MessageDlgEditorSelectIcon in 'MessageDlgEditorSelectIcon.pas' {frmMessageDlgEditorSelectIcon},
  DSADialogsMainFormU in 'DSADialogsMainFormU.pas' {DSADialogsMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(DSA)MessageDlg(Ex) editor';
  Application.CreateForm(TDSADialogsMainForm, DSADialogsMainForm);
  Application.Run;
end.
