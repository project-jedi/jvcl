program JVCLTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  SimpleFormU in 'SimpleFormU.pas' {SimpleFrm},
  JvWndProcHook_Test in 'JvWndProcHook_Test.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
