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
  ExptIntf, ToolsApi,
  JvPlugin, JvPluginManager, JvPluginWizard, JvPluginParamsForm;

{$R ..\resources\JvPluginReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Custom',[
    TJvPluginManager
    ]);
  RegisterLibraryExpert(TJvPluginWizard.Create)
end;
  



end.
