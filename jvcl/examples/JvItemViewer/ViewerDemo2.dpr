program ViewerDemo2;

uses
  Forms,
  MainFrm2 in 'MainFrm2.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
