program ImageWindowDemo;

uses
  Forms,
  ImageWindowMainFormU in 'ImageWindowMainFormU.pas' {ImageWindowMainForm},
  ImageWindowChild2U in 'ImageWindowChild2U.pas' {ImageWindowChild2},
  ImageWindowChild1U in 'ImageWindowChild1U.pas' {ImageWindowChild1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TImageWindowMainForm, ImageWindowMainForm);
  Application.Run;
end.
