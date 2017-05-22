program JvWindowsTitleProj;

uses
  Forms,
  JvWindowsTitleMainFomU in 'JvWindowsTitleMainFomU.pas' {JvWindowsTitleMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvWindowsTitleMainForm, JvWindowsTitleMainForm);
  Application.Run;
end.
