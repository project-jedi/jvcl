program prjControls;

uses
  Forms,
  JvFooterAndGroupHeaderDemoForm in 'JvFooterAndGroupHeaderDemoForm.pas' {JvFooterAndGroupHeaderDemoFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvFooterAndGroupHeaderDemoFrm, JvFooterAndGroupHeaderDemoFrm);
  Application.Run;
end.

