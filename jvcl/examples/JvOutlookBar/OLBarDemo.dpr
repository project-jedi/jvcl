program OLBarDemo;

uses
  Forms,
  OLBarMainFormU in 'OLBarMainFormU.pas' {OLBarMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOLBarMainForm, OLBarMainForm);
  Application.Run;
end.
