unit JvCoreReg;

interface

procedure Register;

implementation
uses
  Classes, ActnList, JvJVCLAbout, JvActions, JvActnRes, JvxDConst;

{.$R ..\resources\JvCoreReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Standard',[TJvJVCLAboutComponent]);
  RegisterActions('JVCL',[TJvSendMail, TJvWebAction],TJvStandardActions);
end;

end.
