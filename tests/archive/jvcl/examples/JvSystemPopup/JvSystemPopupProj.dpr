program JvSystemPopupProj;

uses
  Forms, JvSystemPopupMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvSystemPopupMainForm, JvSystemPopupMainForm);
  Application.Run;
end.
