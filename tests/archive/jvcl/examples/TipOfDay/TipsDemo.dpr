program TipsDemo;

uses
  Forms, TipOfDayMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTipOfDayMainForm, TipOfDayMainForm);
  Application.Run;
end.
