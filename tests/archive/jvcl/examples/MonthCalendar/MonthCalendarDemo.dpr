program MonthCalendarDemo;

uses
  Forms, MonthCalendarMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMonthCalendarMainForm, MonthCalendarMainForm);
  Application.Run;
end.
