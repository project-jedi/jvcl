program JvFormatEditDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmValidateEditDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmValidateEditDemo, frmValidateEditDemo);
  Application.Run;
end.

