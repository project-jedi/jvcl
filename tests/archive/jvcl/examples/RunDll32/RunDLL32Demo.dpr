program RunDLL32Demo;

uses
  Forms, RunDll32MainFormU,
  InfoFrm in 'InfoFrm.pas' {frmInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRunDll32MainForm, RunDll32MainForm);
  Application.Run;
end.
