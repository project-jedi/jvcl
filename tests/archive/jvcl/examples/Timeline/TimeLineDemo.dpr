program TimeLineDemo;

uses
  Forms,
  TimelineNotesFormU in 'TimelineNotesFormU.pas' {TimelineNotesForm},
  TimelineMainFormU in 'TimelineMainFormU.pas' {TimelineMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTimelineMainForm, TimelineMainForm);
  Application.Run;
end.
