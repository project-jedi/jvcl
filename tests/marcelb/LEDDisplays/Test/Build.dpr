program Build;

uses
  Forms,
  MainBuild in 'MainBuild.pas' {Form4},
  JvLEDDisplays in '..\Source\JvLEDDisplays.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
