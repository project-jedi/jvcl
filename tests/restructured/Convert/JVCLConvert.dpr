program JVCLConvert;

uses
  Forms,
  fJvclConverterMain in 'fJvclConverterMain.pas' {frmMain},
  FastTime in 'FastTime.pas',
  fAboutMe in 'fAboutMe.pas' {frmAboutMe};

{$R *.RES}

begin
  Application.Title := 'JVCL Converter';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
