program ContentScrollerDemo;

uses
  Forms,
  ContentScrollerMainFormU in 'ContentScrollerMainFormU.pas' {ContentScrollerMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TContentScrollerMainForm, ContentScrollerMainForm);
  Application.Run;
end.
