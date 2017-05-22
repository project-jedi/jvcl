program TipsDemo;

uses
  Forms,
  TipOfDayMainFormU in 'TipOfDayMainFormU.pas' {TipOfDayMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTipOfDayMainForm, TipOfDayMainForm);
  Application.Run;
end.
