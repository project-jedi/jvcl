program SpecialProgressTestPrj;

uses
  Forms, JvSpecialProgressMainFormU;  

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvSpecialProgressMainForm, JvSpecialProgressMainForm);
  Application.Run;
end.
