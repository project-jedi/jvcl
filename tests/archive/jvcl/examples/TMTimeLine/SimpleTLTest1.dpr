program SimpleTLTest1;

uses
  Forms, TMTimeLineMainFormU,  
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTMTimeLineMainForm, TMTimeLineMainForm);
  Application.Run;
end.
