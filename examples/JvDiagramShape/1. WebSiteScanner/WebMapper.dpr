program WebMapper;

uses
  Forms,
  WebMapperDemoMainForm in 'WebMapperDemoMainForm.pas' {WebMapperDemoMainFrm},
  JimParse;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWebMapperDemoMainFrm, WebMapperDemoMainFrm);
  Application.Run;
end.
