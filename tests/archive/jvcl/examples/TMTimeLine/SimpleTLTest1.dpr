program SimpleTLTest1;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
