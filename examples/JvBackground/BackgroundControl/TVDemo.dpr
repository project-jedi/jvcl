program TVDemo;

uses
  Forms,
  TVDemoMain in 'TVDemoMain.pas' {JvBackgroundDemoFrm},
  JvBackgroundTreeview in 'JvBackgroundTreeview.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvBackgroundDemoFrm, JvBackgroundDemoFrm);
  Application.Run;
end.
