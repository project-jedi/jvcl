program JvSystemPopupProj;

uses
  Forms,
  JvSystemPopupMainFormU in 'JvSystemPopupMainFormU.pas' {JvSystemPopupMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvSystemPopupMainForm, JvSystemPopupMainForm);
  Application.Run;
end.
