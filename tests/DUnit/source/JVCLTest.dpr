program JVCLTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  SimpleFormU in 'SimpleFormU.pas' {SimpleFrm},
  JvOLBar_Test in 'JvOLBar_Test.pas',
  JvWndProcHook_Test in 'JvWndProcHook_Test.pas',
  DUnit_TestSpell in 'DUnit_TestSpell.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
