program JvParameterListDemo;

uses
  Forms,
  JvParameterListMainForm in 'JvParameterListMainForm.pas' {Form1},
  JvParameterListParameter in '..\..\JvParameterListParameter.pas',
  JvAppStoreSelectList in '..\..\JvAppStoreSelectList.pas',
  JvDynControlEngine in '..\..\JvDynControlEngine.pas',
  JvDynControlEngineIntf in '..\..\JvDynControlEngineIntf.pas',
  JvDynControlEngineJVCL in '..\..\JvDynControlEngineJVCL.pas',
  JvDynControlEngineVCLRed in 'JvDynControlEngineVCLRed.pas',
  JvFormPlacementSelectList in '..\..\JvFormPlacementSelectList.pas',
  JvParameterList in '..\..\JvParameterList.pas',
  JvDynControlEngineDevExpCx in 'JvDynControlEngineDevExpCx.pas',
  JvDSADialogs in '..\..\JvDSADialogs.pas',
  JvDynControlEngineVCL in '..\..\JvDynControlEngineVCL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
