program ColorComboDemo;

uses
  Forms,
  JvColorComboDemoMainFormU in 'JvColorComboDemoMainFormU.pas' {JvColorComboDemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvColorComboDemoMainForm, JvColorComboDemoMainForm);
  Application.Run;
end.
