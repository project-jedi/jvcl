program TransparentButtonDemo;

uses
  Forms, TransBtnFormMainU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTransBtnFormMain, TransBtnFormMain);
  Application.Run;
end.
