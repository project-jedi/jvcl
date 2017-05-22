program JvDBAction;

uses
  Forms,
  JvDBActionMainForm in 'JvDBActionMainForm.pas' {JvDBActionMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBActionMainFrm, JvDBActionMainFrm);
  Application.Run;
end.
