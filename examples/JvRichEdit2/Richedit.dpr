program RichEdit;

{$R 'RICHEDIT.res' 'RICHEDIT.RC'}

uses
  Forms,
  JvExceptionForm,
  REMain in 'REMain.pas' {MainForm},
  ParaFmt in 'ParaFmt.pas' {ParaFormatDlg};

{.$R *.RES}

begin
  Application.Initialize;
  JvErrorIntercept;
  Application.Title := 'RX RichEdit Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
