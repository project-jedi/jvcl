program SimpleXmlTest;

uses
  Forms,
  GuiTestRunner,
  TestFramework,
  JvSimpleXmlTestCases in 'JvSimpleXmlTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GuiTestRunner.RunRegisteredTests;
end.
