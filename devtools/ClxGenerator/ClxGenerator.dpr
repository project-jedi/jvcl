program ClxGenerator;

uses
  Forms,
  GenerationX in 'GenerationX.pas' {FormMain},
  Generator in 'Generator.pas',
  dpp_PascalParser in '..\common\dpp_PascalParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
