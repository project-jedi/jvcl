program _glXMLSerializer_demo;

uses
  Forms,
  main in 'main.pas' {fglXMLSerializerDemo},
  testClasses in 'testClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfglXMLSerializerDemo, fglXMLSerializerDemo);
  Application.Run;
end.
