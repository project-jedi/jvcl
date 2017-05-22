program VIDDemoPro;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  ProjectExplorerUnit in 'ProjectExplorerUnit.pas' {ProjectExplorerForm},
  PropertiesUnit in 'PropertiesUnit.pas' {PropertiesForm},
  ToolboxUnit in 'ToolboxUnit.pas' {ToolboxForm},
  ImmediateUnit in 'ImmediateUnit.pas' {ImmediateForm},
  AutosUnit in 'AutosUnit.pas' {AutosForm},
  LocalsUnit in 'LocalsUnit.pas' {LocalsForm},
  WatchUnit in 'WatchUnit.pas' {WatchForm},
  ThreadsUnit in 'ThreadsUnit.pas' {ThreadsForm},
  CallStackUnit in 'CallStackUnit.pas' {CallStackForm},
  RunningDocumentsUnit in 'RunningDocumentsUnit.pas' {RunningDocumentsForm},
  TaskListUnit in 'TaskListUnit.pas' {TaskListForm},
  DocumentOutlineUnit in 'DocumentOutlineUnit.pas' {DocumentOutlineForm},
  OutputUnit in 'OutputUnit.pas' {OutputForm},
  ScriptOutlineUnit in 'ScriptOutlineUnit.pas' {ScriptOutlineForm},
  DefineWindowLayoutUnit in 'DefineWindowLayoutUnit.pas' {DefineWindowLayoutForm},
  FindAndReplaceUnit in 'FindAndReplaceUnit.pas' {FindAndReplaceForm},
  SplashUnit in 'SplashUnit.pas' {SplashForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Microsoft Development Environment [design]';
  Application.CreateForm(TMainForm, MainForm);
  MainForm.Visible := False;
  Application.CreateForm(TSplashForm, SplashForm);
  SplashForm.Show;
  repeat Application.ProcessMessages until SplashForm.SplashClosed;
  MainForm.LoadDockInfo;
  SplashForm.Update;
  SplashForm.Free;
  SplashForm := nil;
  Application.Run;
end.
