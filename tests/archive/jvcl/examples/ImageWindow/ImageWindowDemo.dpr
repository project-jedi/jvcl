program ImageWindowDemo;

uses
  Forms,
  Unit3 in 'Unit3.pas' {Form3},
  Unit2 in 'Unit2.pas' {Form2},
  ImageWindowMainFormU in 'ImageWindowMainFormU.pas' {ImageWindowMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TImageWindowMainForm, ImageWindowMainForm);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TImageWindowMainForm, ImageWindowMainForm);
  Application.Run;
end.
