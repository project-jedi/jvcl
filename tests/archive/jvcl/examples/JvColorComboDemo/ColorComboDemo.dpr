program ColorComboDemo;

uses
  Forms, JvColorComboDemoMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvColorComboDemoMainForm, JvColorComboDemoMainForm);
  Application.Run;
end.
