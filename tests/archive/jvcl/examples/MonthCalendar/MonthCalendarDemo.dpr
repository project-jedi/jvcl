program MonthCalendarDemo;

uses
  Forms,
  MonthCalendarMainFormU in 'MonthCalendarMainFormU.pas' {MonthCalendarMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMonthCalendarMainForm, MonthCalendarMainForm);
  Application.Run;
end.
