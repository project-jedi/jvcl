program OLBarDemo;

uses
  Forms, OLBarMainFormU;  

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOLBarMainForm, OLBarMainForm);
  Application.Run;
end.
