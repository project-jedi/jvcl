unit JvCoreReg;

interface

procedure Register;

implementation
uses
  // About JVCL
  Classes, ActnList, DesignIntf,
  JvxDConst, JVCLVer,
  JvActions, JvActnRes, JvJVCLAbout, 
  JvJVCLAboutEditor;

{.$R ..\resources\JvCoreReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Standard',[TJvJVCLAboutComponent]);
  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);
  RegisterActions(srJVCLActions, [TJvSendMail, TJvWebAction], TJvStandardActions);
end;

end.
