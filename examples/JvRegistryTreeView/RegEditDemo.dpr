program RegEditDemo;

uses
  Forms,
  RegTVMainFormU in 'RegTVMainFormU.pas' {RegTVMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRegTVMainForm, RegTVMainForm);
  Application.Run;
end.
