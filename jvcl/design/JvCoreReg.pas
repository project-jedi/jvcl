{$I JVCL.INC}

unit JvCoreReg;

interface

procedure Register;

implementation
uses
  // About JVCL
  Classes, Controls, ActnList, DesignIntf,
  JVCLVer, JvCOmponent, JvActions, JvActnRes, JvJVCLAbout,
  JvJVCLAboutEditor, JvDsgnEditors;

{.$R ..\resources\JvCoreReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Standard',[TJvJVCLAboutComponent]);

  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
  RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);

  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TShortCut), TJvComponent, '', TJvShortCutProperty);

  RegisterActions('JVCL', [TJvSendMail, TJvWebAction], TJvStandardActions);
end;

end.
