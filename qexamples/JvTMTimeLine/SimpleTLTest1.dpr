{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program SimpleTLTest1;

uses
  QForms,
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm},
  TMTimeLineMainFormU in 'TMTimeLineMainFormU.pas' {TMTimeLineMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTMTimeLineMainForm, TMTimeLineMainForm);
  Application.Run;
end.
