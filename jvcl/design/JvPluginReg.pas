{$I JVCL.INC}
unit JvPluginReg;

interface

procedure Register;

implementation
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ToolsApi,
  JvPlugin, JvPluginManager, JvPluginWizard, JvPluginParamsForm;

{$R ..\resources\JvPluginReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Custom',[TJvPluginManager]);

// Bianconi
  RegisterPropertyEditor(TypeInfo(TShortCut), TJvPluginCommand, 'ShortCut', TShortCutProperty);
// End of Bianconi 

  RegisterPackageWizard( TJvPluginWizard.Create );
//  RegisterLibraryExpert(TJvPluginWizard.Create)
end;
  



end.
