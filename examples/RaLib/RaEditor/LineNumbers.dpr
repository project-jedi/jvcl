program LineNumbers;

uses
  Forms,
  fJvLineNumbersMain in 'fJvLineNumbersMain.pas' {LineNumbersMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvLineNumbersMain , LineNumbersMain);
  Application.Run;
end.
