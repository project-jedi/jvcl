program CollectionDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Hid in '..\..\Hid.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
