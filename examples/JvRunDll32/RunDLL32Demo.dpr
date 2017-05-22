program RunDLL32Demo;

uses
  Forms,
  InfoFrm in 'InfoFrm.pas' {frmInfo},
  RunDll32MainFormU in 'RunDll32MainFormU.pas' {RunDll32MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRunDll32MainForm, RunDll32MainForm);
  Application.Run;
end.
