program JvDbMaskEditDemo;

uses
  Forms,
  JvDbMaskEditDemoFm in 'JvDbMaskEditDemoFm.pas' {Form1},
  StdCtrls in '..\..\..\..\program files\borland\delphi7\source\vcl\StdCtrls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
