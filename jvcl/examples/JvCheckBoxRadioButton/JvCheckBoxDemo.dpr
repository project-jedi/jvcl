program JvCheckBoxDemo;

uses
  Forms,
  JvCheckBoxRadioBtnFrmU in 'JvCheckBoxRadioBtnFrmU.pas' {JvCheckBoxRadioBtnFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvCheckBoxRadioBtnFrm, JvCheckBoxRadioBtnFrm);
  Application.Run;
end.
