unit JvCoreReg;

interface

procedure Register;

implementation
uses
  // About JVCL
  Classes, ActnList, DesignIntf,
  JvxDConst,
  JvActions, JvActnRes, JvJVCLAbout, JvJVCLAboutProperty, JVCLVer;
  // JvWndProcHook

{.$R ..\resources\JvCoreReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Core',[TJvJVCLAboutComponent {,TJvWindowHook}]);
  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);
  RegisterActions(srJVCLActions, [TJvSendMail, TJvWebAction], TJvStandardActions);
end;

end.
