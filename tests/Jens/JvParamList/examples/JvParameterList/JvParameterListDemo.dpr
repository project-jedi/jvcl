program JvParameterListDemo;

uses
  Forms,
  JvParameterListMainForm in 'JvParameterListMainForm.pas' {Form1},
  JvParameterList_Parameter in '..\..\JvParameterList_Parameter.pas',
  JvAppStoreSelectList in '..\..\JvAppStoreSelectList.pas',
  JvDynControlEngine in '..\..\JvDynControlEngine.pas',
  JvDynControlEngine_Interface in '..\..\JvDynControlEngine_Interface.pas',
  JvDynControlEngine_JVCL in '..\..\JvDynControlEngine_JVCL.pas',
  JvDynControlEngine_VCLRed in 'JvDynControlEngine_VCLRed.pas',
  JvFormPlacementSelectList in '..\..\JvFormPlacementSelectList.pas',
  JvParameterList in '..\..\JvParameterList.pas',
  JvDynControlEngine_DevExpCx in 'JvDynControlEngine_DevExpCx.pas',
  JvDSADialogs in '..\..\JvDSADialogs.pas',
  JvDynControlEngine_VCL in '..\..\JvDynControlEngine_VCL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
