{$I JVCL.INC}
unit JvBandsReg;

interface

procedure Register;
implementation
uses
  Classes, ImgList,
  {$IFDEF COMPILER6_UP} DesignEditors, DesignIntf, {$ELSE} DsgnIntf, {$ENDIF COMPILER6_UP}
  {$IFNDEF COMPILER7_UP}ExptIntf, {$ENDIF}ToolsApi,
  JclSchedule,

  JvBandForms, JvBandObject, JvBandObjectDLLWizard, JvBandObjectDLLWizardForm;
procedure Register;
begin
  RegisterCustomModule(TJvBandForm, TCustomModule);
  RegisterPackageWizard(TJvBandObjectDLLWizard.Create);
end;


end.
