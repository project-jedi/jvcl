program LineNumbers;

uses
  Forms,
  fLineNumbersMain in 'fLineNumbersMain.pas' {LineNumbersMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TLineNumbersMain, LineNumbersMain);
  Application.Run;
end.
