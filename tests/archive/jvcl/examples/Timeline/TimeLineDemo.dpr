program TimeLineDemo;

uses
  Forms, TimelineMainFormU, TimelineNotesFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTimelineMainForm, TimelineMainForm);
  Application.CreateForm(TTimelineNotesForm, TimelineNotesForm);
  Application.Run;
end.
