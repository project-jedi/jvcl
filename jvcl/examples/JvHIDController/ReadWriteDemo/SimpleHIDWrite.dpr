program SimpleHIDWrite;

uses
  Forms,
  DevReader in 'DevReader.pas' {Form1},
  Info in 'Info.pas' {InfoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
