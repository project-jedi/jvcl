program RxDemo;

uses
  Forms,
  JvExcptDlg,
  JvDBExcpt,
  Main in 'MAIN.PAS' {MainForm},
  Dbaware in 'DBAWARE.PAS' {DBAwareForm},
  Tools in 'TOOLS.PAS' {ToolsForm},
  CTRLS in 'CTRLS.pas' {ControlsForm},
  About in 'ABOUT.PAS' {AboutForm},
  PageDemo in 'PAGEDEMO.PAS' {ClientAssistant};

{$IFDEF WIN32}
{$R *.R32}
{$ELSE}
{$R *.RES}
{$ENDIF}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.Title := 'RX Demo';
  JvErrorIntercept;
  DBErrorIntercept;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
