program RunDLL32Demo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form2},
  InfoFrm in 'InfoFrm.pas' {frmInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
