program VCDemoPro;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  WorkSpaceUnit in 'WorkSpaceUnit.pas' {WorkSpaceForm},
  OutputUnit in 'OutputUnit.pas' {OutputForm},
  SourceEditUnit in 'SourceEditUnit.pas' {SourceEditForm},
  WatchUnit in 'WatchUnit.pas' {WatchForm},
  RegistersUnit in 'RegistersUnit.pas' {RegistersForm},
  MemoryUnit in 'MemoryUnit.pas' {MemoryForm},
  VariablesUnit in 'VariablesUnit.pas' {VariablesForm},
  CallStackUnit in 'CallStackUnit.pas' {CallStackForm},
  Splash in 'Splash.pas' {Splashs};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Macrosoft Visual C++';
  Application.CreateForm(TMainForm, MainForm);
  MainForm.Visible := False;
  Application.CreateForm(TSplashs, Splashs);
  Splashs.Show;
  repeat Application.ProcessMessages until Splashs.SplashClosed;
  MainForm.LoadDockInfo;
  Splashs.Update;
  Splashs.Free;
  Splashs := nil;
  Application.Run;
end.
