program JvParameterListDemo;

uses
  Forms,
  JvParameterListMainForm in 'JvParameterListMainForm.pas' {JvParameterListDemoMainFrm},
  JvJclUnitVersioningBrowser in '..\..\run\JvJclUnitVersioningBrowser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvParameterListDemoMainFrm, JvParameterListDemoMainFrm);
  Application.Run;
end.
