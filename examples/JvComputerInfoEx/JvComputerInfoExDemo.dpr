program JvComputerInfoExDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  MSHTML_TLB in '..\..\..\..\..\Delphi6\Imports\MSHTML_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
