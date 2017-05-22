program SimpleTLTest1;

uses
  Forms,
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm},
  TMTimeLineMainFormU in 'TMTimeLineMainFormU.pas' {TMTimeLineMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTMTimeLineMainForm, TMTimeLineMainForm);
  Application.Run;
end.
