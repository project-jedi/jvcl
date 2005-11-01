{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program MessageDlgEditor;

uses
  QForms,
  MessageDlgEditorMain in 'MessageDlgEditorMain.pas' {frmMessageDlgEditor},
  MessageDlgEditorSelectIcon in 'MessageDlgEditorSelectIcon.pas' {frmMessageDlgEditorSelectIcon},
  JvObjectInspector in '..\JvXPControls\Simple\JvObjectInspector.pas' {ObjectInspector};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(DSA)MessageDlg(Ex) editor';
  Application.CreateForm(TfrmMessageDlgEditor, frmMessageDlgEditor);
  Application.CreateForm(TObjectInspector, ObjectInspector);
  Application.Run;
end.
