program Hospital;

uses
  Forms,
  FormMain in 'FormMain.pas' {foMain},
  Strings in 'Strings.pas',
  ClassUtils in 'ClassUtils.pas',
  Declarations in 'Declarations.pas',
  ClassRequest in 'ClassRequest.pas',
  ClassHospital in 'ClassHospital.pas',
  ClassScript in 'ClassScript.pas',
  ClassQueryDisplay in 'ClassQueryDisplay.pas',
  ClassUrlParser in 'ClassUrlParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
