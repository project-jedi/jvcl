program AppDdeCmdExample;

uses
  Forms,
  AppDdeCmdMain in 'AppDdeCmdMain.pas' {MainForm},
  AppDdeCmdModal in 'AppDdeCmdModal.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MyApp';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
