program MDISample;

uses
  ShareMem,
  Forms,
  MDISampleU in 'MDISampleU.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
