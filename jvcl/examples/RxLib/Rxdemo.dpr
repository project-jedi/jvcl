program RxDemo;

uses
  Forms,
  JvExceptionForm,
  JvBDEExceptionForm,
  Main in 'MAIN.PAS' {MainForm},
  Dbaware in 'DBAWARE.PAS' {DBAwareForm},
  Tools in 'TOOLS.PAS' {ToolsForm},
  CTRLS in 'CTRLS.pas' {ControlsForm},
  About in 'ABOUT.PAS' {AboutForm},
  PageDemo in 'PAGEDEMO.PAS' {ClientAssistant};

{$R *.R32}

begin
  Application.Initialize;
  Application.Title := 'RX Demo';
  JvErrorIntercept;
  DBErrorIntercept;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
