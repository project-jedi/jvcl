{$I JVCL.INC}
unit JvPluginReg;

interface

procedure Register;

implementation
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ToolsApi,
  JvPlugin, JvPluginManager, JvPluginWizard, JvPluginParamsForm;

{$R ..\resources\JvPluginReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Custom',[
    TJvPluginManager
    ]);
  RegisterPackageWizard( TJvPluginWizard.Create );
//  RegisterLibraryExpert(TJvPluginWizard.Create)
end;
  



end.
