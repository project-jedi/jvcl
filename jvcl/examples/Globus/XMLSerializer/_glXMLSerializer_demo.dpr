program _glXMLSerializer_demo;

uses
  Forms,
  testClasses in 'testClasses.pas',
  XMLSerializerMainFormU in 'XMLSerializerMainFormU.pas' {fglXMLSerializerDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfglXMLSerializerDemo, fglXMLSerializerDemo);
  Application.CreateForm(TfglXMLSerializerDemo, fglXMLSerializerDemo);
  Application.Run;
end.
