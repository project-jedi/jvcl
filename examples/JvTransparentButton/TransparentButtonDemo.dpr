program TransparentButtonDemo;

uses
  Forms,
  TransBtnFormMainU in 'TransBtnFormMainU.pas' {TransBtnFormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTransBtnFormMain, TransBtnFormMain);
  Application.Run;
end.
