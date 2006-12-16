program JvEdits;

{$I jvcl.inc}

uses
  {$IFDEF VCL}
  Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms,
  {$ENDIF VisualCLX}
  fEdits in 'fEdits.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
