{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQProgressDialogDemo;

uses
  QForms,
  JvQProgressDialogMain in 'JvQProgressDialogMain.pas' {frmProgressDialogDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmProgressDialogDemo, frmProgressDialogDemo);
  Application.Run;
end.
