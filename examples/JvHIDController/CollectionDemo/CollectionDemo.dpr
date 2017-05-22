program CollectionDemo;

uses
  Forms,
  CollectionMain in 'CollectionMain.pas' {CollectionDemoForm},
  UsagesInfo in 'UsagesInfo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCollectionDemoForm, CollectionDemoForm);
  Application.Run;
end.
