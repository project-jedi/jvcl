program SpecialProgressTestPrj;

uses
  Forms,
  JvSpecialProgressMainFormU in 'JvSpecialProgressMainFormU.pas' {JvSpecialProgressMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvSpecialProgressMainForm, JvSpecialProgressMainForm);
  Application.Run;
end.
