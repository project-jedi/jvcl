program JvThumbnailDemo;

uses
  Forms,
  main in 'main.pas' {Form1},
  sample2 in 'sample2.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
